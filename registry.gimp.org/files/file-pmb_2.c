/* file-pmb.c
 * Gimp plug-in for loading/saving .pmb files (Polar Monitor Bitmap).
 * Based on the BMP plug-in included in the Gimp source tree.
 * Markus Heidelberg <markus.heidelberg@web.de>
 */

/*
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------
 */

/*
 * The Polar Monitor Bitmap is a simple text-based 1-bit image file format. It
 * is used for uploading logos to Polar watches (polar.fi) using e.g.
 * - Polar UpLink Tool
 *   http://www.polar.fi/en/support/Polar_UpLink_Tool_Software?product_id=3131&category=downloads
 * - Polar ProTrainer
 *   http://www.polar.fi/en/support/Polar_ProTrainer_5_FREE_30_day_trial?product_id=446&category=downloads
 *
 * External description of the file format:
 *   http://www.martinet.nl/articles/20050524
 *   http://www.runrev.com/newsletter/september/issue33/newsletter2.php
 *
 * Build and install:
 * $ gimptool-2.0 --install file-pmb.c
 *
 * Tested with Polar UpLink Tool v1.61 and Gimp 2.6.11.
 * Compatibility with the Polar UpLink Tool:
 * - DOS style EOL (CRLF) is written
 * - Unix style EOL (LF) can be read
 * - If the .pmb file is too short, the missing pixels are white (unset).
 * - If the .pmb file is too long, the remaining lines are ignored.
 * - Empty or garbage lines of pixel data in the .pmb file are treated as white (unset).
 *   This was possible by using fgets() instead of fscanf().
 *
 * TODO:
 * - Use 1-bit INDEXED image instead of 8-bit GRAY
 * - Add an export dialog (see bmp plug-in)
 *   - merging layers
 *   - handling of unsupported color depths
 * - Support a height of >32px
 * - Compatibility with Polar UpLink Tool:
 *   - handling of Mac style EOL (not important)
 *     correct: all pixels white (unset)
 *     currently: image dimensions wrong, garbage content
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <glib/gstdio.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>


#define  _(String) (String)
#define N_(String) (String)

#define LOAD_PROC      "file-pmb-load"
#define SAVE_PROC      "file-pmb-save"
#define PLUG_IN_BINARY "file-pmb"

#define IDENTIFIER  "[BitmapInfo2]"
#define MAX_HEIGHT  32
/*
 * Dimension limits for calculating the required buffer sizes.
 * Max width: 262144px = 2^18px (GIMP_MAX_IMAGE_SIZE)
 *   Text string of 6 digits for storing the width is less than for the IDENTIFIER.
 * Max height: 32px (guint32 coldata)
 *   Text string of 2 digits for storing the height is less than for the IDENTIFIER.
 *   The highest possible 32-bit number is 2^32-1 = 4294967295
 *   and requires 10 bytes as a string in the .pmb file.
 */


/* Declare some local functions.
 */
static void   query (void);
static void   run   (const gchar      *name,
                     gint              nparams,
                     const GimpParam  *param,
                     gint             *nreturn_vals,
                     GimpParam       **return_vals);

static gint32   load_pmb (const gchar      *filename,
                          GError          **error);
static gint32   read_image (FILE         *fd,
                            const gchar  *name,
                            gint          width,
                            gint          height);

static GimpPDBStatusType  save_pmb (const gchar  *filename,
                                    gint32        image,
                                    gint32        drawable_ID,
                                    GError      **error);
static void               write_image  (FILE   *f,
                                        guchar *src,
                                        gint    width,
                                        gint    height);


const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,  /* init_proc  */
  NULL,  /* quit_proc  */
  query, /* query_proc */
  run,   /* run_proc   */
};

MAIN ()

static void
query (void)
{
  static const GimpParamDef load_args[] =
  {
    { GIMP_PDB_INT32,    "run-mode",     "Non-interactive" },
    { GIMP_PDB_STRING,   "filename",     "The name of the file to load" },
    { GIMP_PDB_STRING,   "raw-filename", "The name entered" },
  };
  static const GimpParamDef load_return_vals[] =
  {
    { GIMP_PDB_IMAGE, "image", "Output image" },
  };

  static const GimpParamDef save_args[] =
  {
    { GIMP_PDB_INT32,    "run-mode",     "Non-interactive" },
    { GIMP_PDB_IMAGE,    "image",        "Input image" },
    { GIMP_PDB_DRAWABLE, "drawable",     "Drawable to save" },
    { GIMP_PDB_STRING,   "filename",     "The name of the file to save the image in" },
    { GIMP_PDB_STRING,   "raw-filename", "The name entered" },
  };

  gimp_install_procedure (LOAD_PROC,
                          "Loads files in Polar Monitor Bitmap file format",
                          "Loads files in Polar Monitor Bitmap file format",
                          "Markus Heidelberg",
                          "Markus Heidelberg",
                          "2010",
                          N_("Polar logo"),
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (load_args),
                          G_N_ELEMENTS (load_return_vals),
                          load_args, load_return_vals);

  gimp_register_file_handler_mime (LOAD_PROC, "image/pmb");
  gimp_register_magic_load_handler (LOAD_PROC,
                                    "pmb",
                                    "",
                                    "0,string," IDENTIFIER);

  gimp_install_procedure (SAVE_PROC,
                          "Saves files in Polar Monitor Bitmap file format",
                          "Dimensions for the watches supported by the Polar UpLink Tool:\n"
                          "  AXN300: 47x15\n"
                          "  AXN500: 47x15\n"
                          "  AXN700: 47x15\n"
                          "  CS100: 47x8\n"
                          "  CS200: 47x8\n"
                          "  CS300: 47x16\n"
                          "  E200: 47x8\n"
                          "  E600: 47x8\n"
                          "  F6, F7: 47x16\n"
                          "  F11: 47x16\n"
                          "  F55: 47x16\n"
                          "  RS100: 47x8\n"
                          "  RS200: 64x22\n"
                          "  S120: 44x8\n"
                          "  S150: 44x8\n"
                          "  S610i, S610: 47x8\n"
                          "  S720i, S710i, S710: 47x8\n"
                          "  S725: 47x8\n"
                          "  S725X, S625X: 47x8\n"
                          "  S810i, S810: 47x8\n"
                          "There are more available.",
                          "Markus Heidelberg",
                          "Markus Heidelberg",
                          "2010",
                          N_("Polar logo"),
                          "GRAY",
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (save_args), 0,
                          save_args, NULL);

  gimp_register_file_handler_mime (SAVE_PROC, "image/pmb");
  gimp_register_save_handler (SAVE_PROC, "pmb", "");
}

static void
run (const gchar      *name,
     gint             nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam   values[2];
  GimpRunMode        run_mode;
  GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
  gint32             image_ID;
  gint32             drawable_ID;
  GimpExportReturn   export = GIMP_EXPORT_CANCEL;
  GError            *error  = NULL;

  run_mode = param[0].data.d_int32;

  *nreturn_vals = 1;
  *return_vals  = values;
  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;

  if (strcmp (name, LOAD_PROC) == 0)
    {
       switch (run_mode)
        {
        case GIMP_RUN_INTERACTIVE:
          break;

        case GIMP_RUN_NONINTERACTIVE:
          /*  Make sure all the arguments are there!  */
          if (nparams != 3)
            status = GIMP_PDB_CALLING_ERROR;
          break;

        default:
          break;
        }

       if (status == GIMP_PDB_SUCCESS)
         {
           image_ID = load_pmb (param[1].data.d_string, &error);

           if (image_ID != -1)
             {
               *nreturn_vals = 2;
               values[1].type         = GIMP_PDB_IMAGE;
               values[1].data.d_image = image_ID;
             }
           else
             {
               status = GIMP_PDB_EXECUTION_ERROR;
             }
         }
    }
  else if (strcmp (name, SAVE_PROC) == 0)
    {
      image_ID    = param[1].data.d_int32;
      drawable_ID = param[2].data.d_int32;

      /*  eventually export the image */
      switch (run_mode)
        {
        case GIMP_RUN_INTERACTIVE:
          /* fallthrough */

        case GIMP_RUN_WITH_LAST_VALS:
          gimp_ui_init (PLUG_IN_BINARY, FALSE);
          export = gimp_export_image (&image_ID, &drawable_ID, NULL,
                                       GIMP_EXPORT_CAN_HANDLE_GRAY);
          if (export == GIMP_EXPORT_CANCEL)
            {
              values[0].data.d_status = GIMP_PDB_CANCEL;
              return;
            }
          break;

        case GIMP_RUN_NONINTERACTIVE:
          /*  Make sure all the arguments are there!  */
          if (nparams != 5)
            status = GIMP_PDB_CALLING_ERROR;
          break;

        default:
          break;
        }

      if (status == GIMP_PDB_SUCCESS)
        status = save_pmb (param[3].data.d_string, image_ID, drawable_ID,
                           &error);

      if (export == GIMP_EXPORT_EXPORT)
        gimp_image_delete (image_ID);
    }
  else
    {
      status = GIMP_PDB_CALLING_ERROR;
    }

  if (status != GIMP_PDB_SUCCESS && error)
    {
      *nreturn_vals = 2;
      values[1].type          = GIMP_PDB_STRING;
      values[1].data.d_string = error->message;
    }

  values[0].data.d_status = status;
}

static gint32
load_pmb (const gchar  *name,
         GError      **error)
{
  FILE     *fd;
  gint32    image_ID;
  gchar     buffer[sizeof(IDENTIFIER)+2]; /* max IDENTIFIER\r\n\0 */
  gint      width, height;

  fd = g_fopen (name, "rb");

  if (!fd)
    {
      g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                   _("Could not open '%s' for reading: %s"),
                   gimp_filename_to_utf8 (name), g_strerror (errno));
      return -1;
    }

  gimp_progress_init_printf (_("Opening '%s'"),
                             gimp_filename_to_utf8 (name));

  /* Verify the identifier (1st line) */
  if (fgets (buffer, sizeof(buffer), fd) == NULL ||
      strncmp (buffer, IDENTIFIER, sizeof(IDENTIFIER)-1))
    {
      g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_FAILED,
                   _("'%s' is not a valid PMB file"),
                   gimp_filename_to_utf8 (name));
      return -1;
    }

  /* Read the width (2nd line) */
  if (fgets (buffer, sizeof(buffer), fd) == NULL)
    {
      g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_FAILED,
                   _("'%s' is not a valid PMB file"),
                   gimp_filename_to_utf8 (name));
      return -1;
    }
  width = atoi (buffer);

  /* Read the height (3rd line) */
  if (fgets (buffer, sizeof(buffer), fd) == NULL)
    {
      g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_FAILED,
                   _("'%s' is not a valid PMB file"),
                   gimp_filename_to_utf8 (name));
      return -1;
    }
  height = atoi (buffer);

  image_ID = read_image (fd, name, width, height);

  if (image_ID < 0)
    return -1;

  return image_ID;
}

static gint32
read_image (FILE         *fd,
            const gchar  *name,
            gint          width,
            gint          height)
{
  GimpPixelRgn       pixel_rgn;
  gint               xpos;
  gint               ypos;
  gint32             image;
  gint32             layer;
  GimpDrawable      *drawable;
  guchar            *dest;

  if ((width < 1) || (width > GIMP_MAX_IMAGE_SIZE))
    {
      g_message (_("Unsupported or invalid image width: %d"), width);
      return -1;
    }

  if ((height < 1) || (height > GIMP_MAX_IMAGE_SIZE) || (height > MAX_HEIGHT))
    {
      g_message (_("Unsupported or invalid image height (max %d): %d"), MAX_HEIGHT, height);
      return -1;
    }

  image = gimp_image_new (width, height, GIMP_GRAY);
  layer = gimp_layer_new (image, _("Background"),
                          width, height,
                          GIMP_GRAY_IMAGE, 100, GIMP_NORMAL_MODE);

  gimp_image_set_filename (image, name);

  /* gimp_image_insert_layer is not available in public header */
  /* gimp_image_insert_layer (image, layer, -1, 0); */
  gimp_image_add_layer (image, layer, 0);
  drawable = gimp_drawable_get (layer);

  dest = g_malloc (width * height * 1);

  /* Walk through the file.
   * If it is too long, the extra lines are ignored.
   * Behavior is compatible with the Polar UpLink Tool.
   */
  for (xpos = 0; xpos < width; xpos++)
    {
      /* Initialize to 0 (pixel unset) for the case that the file is too short.
       * Behavior is compatible with the Polar UpLink Tool.
       */
      guint32 coldata = 0;
      /* Buffer for one image column (from 4th line), max 2^MAX_HEIGHT-1 (10 digits) */
      gchar   buffer[10+3]; /* 10 digits, 3 \r\n\0 */

      if (fgets (buffer, sizeof(buffer), fd) != NULL)
        coldata = atoi (buffer);

      for (ypos = 0; ypos < height; ypos++)
        {
          /* PBM starts counting at the bottom, Gimp at the top,
           * so we have to do "height - 1 - ypos".
           * 1-bit depth: 0 (black) if the pixel is set, else 255 (white) */
          dest[ypos * width + xpos] = (coldata & (1 << (height - 1 - ypos))) > 0 ? 0 : 255;
        }
      gimp_progress_update ((gdouble) xpos / (gdouble) width);
    }

  fclose (fd);

  gimp_progress_update (1.0);

  gimp_pixel_rgn_init (&pixel_rgn, drawable,
                       0, 0, drawable->width, drawable->height, TRUE, FALSE);
  gimp_pixel_rgn_set_rect (&pixel_rgn, dest,
                           0, 0, drawable->width, drawable->height);

  gimp_drawable_flush (drawable);
  gimp_drawable_detach (drawable);
  g_free (dest);

  return image;
}

static GimpPDBStatusType
save_pmb (const gchar  *filename,
          gint32        image,
          gint32        drawable_ID,
          GError      **error)
{
  FILE          *outfile;
  guchar        *pixels;
  GimpPixelRgn   pixel_rgn;
  GimpDrawable  *drawable;
  GimpImageType  drawable_type;

  drawable = gimp_drawable_get (drawable_ID);
  drawable_type = gimp_drawable_type (drawable_ID);

  if (drawable->height > MAX_HEIGHT)
    {
      g_message (_("Unsupported image height (max %d): %d"), MAX_HEIGHT, drawable->height);
      return -1;
    }

  gimp_pixel_rgn_init (&pixel_rgn, drawable,
                       0, 0, drawable->width, drawable->height, FALSE, FALSE);

  if (drawable_type != GIMP_GRAY_IMAGE)
    g_assert_not_reached ();

  outfile = g_fopen (filename, "wb");

  /* fetch the image */
  pixels = g_new (guchar, drawable->width * drawable->height * 1);
  gimp_pixel_rgn_get_rect (&pixel_rgn, pixels,
                           0, 0, drawable->width, drawable->height);

  /* And let's begin the progress */
  gimp_progress_init_printf (_("Saving '%s'"),
                             gimp_filename_to_utf8 (filename));

  /* Write the header: identifier, width, height */
  fwrite (IDENTIFIER "\r\n", sizeof(IDENTIFIER)+1, 1, outfile);
  fprintf (outfile, "%u\r\n", drawable->width);
  fprintf (outfile, "%u\r\n", drawable->height);

  write_image (outfile, pixels, drawable->width, drawable->height);

  fclose (outfile);
  gimp_drawable_detach (drawable);
  g_free (pixels);

  return GIMP_PDB_SUCCESS;
}

static void
write_image (FILE   *f,
             guchar *src,
             gint    width,
             gint    height)
{
  gint               xpos;
  gint               ypos;

  for (xpos = 0; xpos < width; xpos++)
    {
      guint32 coldata = 0;
      for (ypos = 0; ypos < height; ypos++)
        {
          /* PBM starts counting at the bottom, Gimp at the top,
           * so we have to do "height - 1 - ypos".
           * 1-bit depth: set the pixel, if it is more black than white (gray <= 127) */
          coldata |= (src[ypos * width + xpos] > 127 ? 0 : 1) << (height - 1 - ypos);
        }
      fprintf (f, "%u\r\n", coldata);
      gimp_progress_update ((gdouble) xpos / (gdouble) width);
    }

  gimp_progress_update (1.0);
}
