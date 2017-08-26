/*
 * ABR Plugin version 1.0
 * This GIMP plug-in is designed to load Adobe Photoshop(tm) brushes (.ABR)
 *
 * Initial code from Marco Lamberto abr2gbr (http://the.sunnyspot.org/gimp/)
 *
 * Eric Lamarque <eric.lamarque@free.fr>
 *
 *          Copyright (C) 2007 Eric Lamarque
 *          Copyright (C) 2001 Marco Lamberto <lm@sunnyspot.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * Adobe and Adobe Photoshop are trademarks of Adobe Systems
 * Incorporated that may be registered in certain jurisdictions.
 */

/*
 * Revision history:
 *
 *  2007-05-07 / v1.0 / Eric Lamarque
 *       Initial release :
 *         load v1 and v2 ABR brushes.
 *         load v6.1 and v6.2 ABR brushes.
 */

/* *** USER DEFINES *** */

/* *** END OF USER DEFINES *** */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>

#include <glib.h>

#include <libgimp/gimp.h>


#ifndef _O_BINARY
#define _O_BINARY 0
#endif


/* Local types etc
 */
typedef struct _AbrInfo AbrInfo;

struct _AbrInfo
{
  gshort version;
  gshort subversion;
  gshort count;
};

/* Declare some local functions.
 */
static void   query      (void);
static void   run        (const gchar      *name,
                          gint              nparams,
                          const GimpParam  *param,
                          gint             *nreturn_vals,
                          GimpParam       **return_vals);

static gint32   load_image         (const gchar *filename);

static gchar    abr_read_char      (FILE *abr);
static gshort   abr_read_short     (FILE *abr);
static gint32   abr_read_long      (FILE *abr);
static gchar *  abr_read_ucs2_text (FILE *abr);
static gboolean abr_read_content   (FILE *abr, AbrInfo *abr_hdr);
static gint32   abr_brush_load     (FILE *abr, AbrInfo *abr_hdr,
				    const gchar *filename, gint32 image_ID,
				    gint32 id);
static gint32   rle_decode         (FILE *abr, gchar *buffer, gint32 height);
static gboolean abr_reach_8BIM_section (FILE *abr, const gchar *name);
static gboolean abr_supported_content  (AbrInfo *abr_hdr);
static gint32   abr_load           (const gchar *filename);
static gchar *  abr_v1_brush_name  (const gchar *filename, gint32 id);




GimpPlugInInfo PLUG_IN_INFO =
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
  static GimpParamDef load_args[] =
  {
    { GIMP_PDB_INT32, "run_mode", "Interactive, non-interactive" },
    { GIMP_PDB_STRING, "filename", "The name of the file to load" },
    { GIMP_PDB_STRING, "raw_filename", "The name of the file to load" }
  };
  static GimpParamDef load_return_vals[] =
  {
    { GIMP_PDB_IMAGE, "image", "Output image" }
  };

  gimp_install_procedure ("file_abr_load",
                          "loads brushes of the Photoshop(tm) ABR file format",
                          "This filter loads brushes of Adobe Photoshop(tm)"
			  " ABR format.",
                          "Eric Lamarque",
                          "Eric Lamarque",
                          "2007",
                          "Photoshop Brush",
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (load_args),
                          G_N_ELEMENTS (load_return_vals),
                          load_args, load_return_vals);

  gimp_register_file_handler_mime ("file_abr_load", "image/x-photoshop-abr");
  gimp_register_magic_load_handler ("file_abr_load",
				    "abr",
				    "",
				    "");
}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam values[2];
  GimpRunMode run_mode;
  gint32 image_ID;

  run_mode = param[0].data.d_int32;

  *nreturn_vals = 1;
  *return_vals = values;
  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_CALLING_ERROR;

  if (strcmp (name, "file_abr_load") == 0)
    {
      image_ID = load_image (param[1].data.d_string);

      if (image_ID != -1)
	{
	  *nreturn_vals = 2;
	  values[0].data.d_status = GIMP_PDB_SUCCESS;
	  values[1].type = GIMP_PDB_IMAGE;
	  values[1].data.d_image = image_ID;
	}
      else
	{
	  values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;
	}
    }
}

static gint32
load_image (const gchar *filename)
{
  gchar *temp;
  gint   fd;

  fd = open (filename, O_RDONLY | _O_BINARY);
 
  if (fd == -1)
    {
      g_message ("Could not open '%s' for reading: %s",
                 gimp_filename_to_utf8 (filename), g_strerror (errno));
      return -1;
    }

  temp = g_strdup_printf ("Opening '%s'...",
                          gimp_filename_to_utf8 (filename));
  gimp_progress_init (temp);
  g_free (temp);

  return abr_load (filename);
}

static gchar
abr_read_char (FILE *abr)
{
  g_return_val_if_fail ( abr != NULL, 0 );
        
  return fgetc (abr);
}

static gshort
abr_read_short (FILE *abr)
{
  gshort val;

  g_return_val_if_fail (abr != NULL, 0 );

  fread (&val, sizeof (val), 1, abr);
  
  return GUINT16_FROM_BE (val);
}

static gint32
abr_read_long (FILE *abr)
{
  gint32 val;

  g_return_val_if_fail ( abr != NULL, 0 );
  
  fread (&val, sizeof (val), 1, abr);

  return GUINT32_FROM_BE (val);
}

static gchar*
abr_read_ucs2_text (FILE *abr)
{
  gulong  name_size;
  gulong  buf_size;
  gchar  *name_ucs2;
  gchar  *name_utf8;
  guint   i;

  /* two-bytes characters encoded (UCS-2)
   *  format:
   *   long : size - number of characters in string
   *   data : zero terminated UCS-2 string
   */

  name_size = abr_read_long (abr);
  if (name_size == 0)
    return (gchar*) NULL;

  buf_size = name_size * 2;

  name_ucs2 = (gchar*) g_malloc (buf_size * sizeof (gchar));
  for (i = 0; i < buf_size ; i++)
    {
         name_ucs2[i] =  abr_read_char (abr);
    }

  name_utf8 = g_convert (name_ucs2, buf_size,
                         "UTF-8", "UCS-2BE",
                         NULL, NULL, NULL);

  g_free (name_ucs2);

  return name_utf8;
}

static gchar*
abr_v1_brush_name (const gchar *filename, gint32 id)
{
  gchar *name;
  gchar *tmp;
  gchar *p;

  tmp = g_path_get_basename (filename);
  p = g_strrstr (tmp, ".abr");
  if (p != NULL)
    /* discard .abr extension */
    *p = '\0';

  name = g_strdup_printf ("%s %03d", gimp_filename_to_utf8 (tmp), id);
  g_free (tmp);

  return name;
}

static gint32
abr_brush_load_v12 (FILE *abr, AbrInfo *abr_hdr,
		    const gchar *filename, gint32 image_ID, gint32 id)
{
  gint16 brush_type;
  gint32 brush_size;
  gint32 next_brush;

  gint32 top, left, bottom, right;
  gint16 depth;
  gchar  compression;
  gchar *name = NULL;

  gint32 width, height;
  gint32 size;

  gint32            layer_ID = -1;
  GimpDrawable     *drawable;
  GimpPixelRgn      pixel_rgn;
  guchar           *buffer;

  g_return_val_if_fail (abr != NULL, -1 );

  brush_type = abr_read_short (abr);
  brush_size = abr_read_long (abr);
  next_brush = ftell (abr) + brush_size;

  switch (brush_type) {
  case 1: /* computed brush */
    /* FIXME: support it! */
    /* g_message("WARNING: computed brush unsupported, skipping.\n"); */
    fseek (abr, next_brush, SEEK_CUR);
    break;
  case 2: /* sampled brush */
    /* discard 4 misc bytes and 2 spacing bytes */
    fseek (abr, 6, SEEK_CUR);
    
    if (abr_hdr->version == 2)
      name = abr_read_ucs2_text (abr);
    if (name == NULL)
      name = abr_v1_brush_name (filename, id);
    
    /* discard 1 byte for antialiasing and 4 x short for short bounds */
    fseek (abr, 9, SEEK_CUR);

    top = abr_read_long (abr);
    left = abr_read_long (abr);
    bottom = abr_read_long (abr);
    right = abr_read_long (abr);
    depth = abr_read_short (abr);
    compression = abr_read_char (abr);

    width = right - left;
    height = bottom - top;
    size = width * (depth >> 3) * height;

    /* FIXME: support wide brushes */
    if (height > 16384)
      {
	g_message ("WARNING: wide brushes not supported\n");
	fseek (abr, next_brush, SEEK_SET);
	break;
      }

    layer_ID = gimp_layer_new (image_ID, name, width, height,
                               GIMP_GRAY_IMAGE, 100, GIMP_NORMAL_MODE);
    gimp_image_add_layer (image_ID, layer_ID, -1);
    
    g_free (name);

    drawable = gimp_drawable_get (layer_ID);
    gimp_pixel_rgn_init (&pixel_rgn, drawable,
                         0, 0, drawable->width, drawable->height,
                         TRUE, FALSE);

    buffer = (guchar*) g_malloc (size);

    if (!compression)
      /* not compressed - read raw bytes as brush data */
      fread (buffer, size, 1, abr);
    else
      rle_decode (abr, (gchar*) buffer, height);

    gimp_pixel_rgn_set_rect (&pixel_rgn, buffer,
			     0, 0, width, height);

    gimp_invert (layer_ID);

    gimp_drawable_flush (drawable);

    g_free (buffer);
    break;
  default:
    g_message ("WARNING: unknown brush type, skipping.\n");
    fseek (abr, next_brush, SEEK_SET);
  }

  return layer_ID;
}

static gint32
abr_brush_load_v6 (FILE *abr, AbrInfo *abr_hdr,
		   const gchar *filename, gint32 image_ID, gint32 id)
{
  gint32 brush_size;
  gint32 brush_end;
  gint32 complement_to_4;
  gint32 next_brush;

  gint32 top, left, bottom, right;
  gint16 depth;
  gchar  compression;

  gint32 width, height;
  gint32 size;

  gint32 layer_ID = -1;
  gchar *name;
  GimpDrawable     *drawable;
  GimpPixelRgn      pixel_rgn;
  guchar           *buffer;

  brush_size = abr_read_long (abr);
  brush_end = brush_size;
  /* complement to 4 */
  while (brush_end % 4 != 0) brush_end++;
  complement_to_4 = brush_end - brush_size;
  next_brush = ftell (abr) + brush_end;

  fseek (abr, 37, SEEK_CUR); /* discard key */
  if (abr_hdr->subversion == 1)
    /* discard short coordinates and unknown short */
    fseek (abr, 10, SEEK_CUR);
  else
    /* discard unknown bytes */
    fseek (abr, 264, SEEK_CUR);

  top = abr_read_long (abr);
  left = abr_read_long (abr);
  bottom = abr_read_long (abr);
  right = abr_read_long (abr);
  depth = abr_read_short (abr);
  compression = abr_read_char (abr);

  width = right - left;
  height = bottom - top;
  size = width * (depth >> 3) * height;

  name = abr_v1_brush_name (filename, id);

  layer_ID = gimp_layer_new (image_ID, name, width, height,
                             GIMP_GRAY_IMAGE, 100, GIMP_NORMAL_MODE);

  g_free (name);

  gimp_image_add_layer (image_ID, layer_ID, -1);

  drawable = gimp_drawable_get (layer_ID);
  gimp_pixel_rgn_init (&pixel_rgn, drawable,
                       0, 0, drawable->width, drawable->height,
                       TRUE, FALSE);

  buffer = (guchar*) g_new0 (guchar, size);

  /* data decoding */
  if (!compression) {
    /* not compressed - read raw bytes as brush data */
    fread (buffer, size, 1, abr);
  } else {
    rle_decode (abr, (gchar*) buffer, height);
  }

  gimp_pixel_rgn_set_rect (&pixel_rgn, buffer,
			   0, 0, width, height);
  
  gimp_invert (layer_ID);
  
  gimp_drawable_flush (drawable);
  
  g_free (buffer);

  fseek (abr, next_brush, SEEK_SET);

  return layer_ID;
}
static gint32
abr_brush_load (FILE *abr, AbrInfo *abr_hdr,
                const gchar *filename, gint32 image_ID, gint32 id)
{
  gint32 layer_ID = -1;
  switch (abr_hdr->version)
    {
    case 1:
    case 2:
      layer_ID = abr_brush_load_v12 (abr, abr_hdr, filename, image_ID, id);
      break;
    case 6:
      layer_ID = abr_brush_load_v6 (abr, abr_hdr, filename, image_ID, id);
      break;
    }

  return layer_ID;
}

static gint32
find_sample_count_v6(FILE *abr, AbrInfo *abr_info)
{
  gint32 origin;
  gint32 sample_section_size;
  gint32 sample_section_end;
  gint32 samples=0;
  gint32 data_start;
  
  gint32 brush_size;
  gint32 brush_end;

  if (!abr_supported_content(abr_info))
    return 0;

  origin = ftell (abr);

  if (!abr_reach_8BIM_section (abr, "samp")) {
    fseek (abr, origin, SEEK_SET);
    return 0;
  }

  sample_section_size = abr_read_long (abr);
  sample_section_end = sample_section_size + ftell (abr);

  data_start = ftell (abr);

  while (!feof (abr) && ftell (abr) < sample_section_end) {
    brush_size = abr_read_long (abr);
    brush_end = brush_size;
    /* complement to 4 */
    while (brush_end % 4 != 0) brush_end++;
    
    fseek (abr, brush_end, SEEK_CUR);
    samples++;
  }

  /* set stream to samples data */
  fseek (abr, data_start, SEEK_SET);
  
  return samples;
}

static gboolean
abr_read_content(FILE *abr, AbrInfo *abr_hdr)
{
  abr_hdr->version = abr_read_short(abr);
  abr_hdr->subversion = 0;
  abr_hdr->count = 0;

  switch (abr_hdr->version)
    {
    case 1:
    case 2:
      abr_hdr->count = abr_read_short(abr);
      break;
    case 6:
      abr_hdr->subversion = abr_read_short(abr);
      abr_hdr->count = find_sample_count_v6(abr, abr_hdr);
      break;
    default:
      /* unknown versions */
      break;
    }
  /* next bytes in abr are samples data */
  
  return TRUE;
}

static gboolean
abr_supported_content(AbrInfo *abr_hdr)
{
  switch(abr_hdr->version)
    {
    case 1:
    case 2:
      return TRUE;
      break;
    case 6:
      if (abr_hdr->subversion == 1 || abr_hdr->subversion == 2)
	return TRUE;
      break;
    }
  return FALSE;
}

static gint32
abr_load (const gchar *filename)
{
  FILE *abr;
  AbrInfo abr_hdr;
  gint32 image_ID;
  gint i;
  gint32 layer_ID;

  if (!(abr = fopen (filename, "rb")))
    {
      g_message("Could not open '%s' for reading: %s\n",
                gimp_filename_to_utf8 (filename), g_strerror (errno));
      return -1;
    }

  if (!abr_read_content (abr, &abr_hdr))
    {
      g_message("Error: cannot parse ABR file: %s\n",
                gimp_filename_to_utf8 (filename));
      return -1;
    }

  if (!abr_supported_content (&abr_hdr))
    {
      g_message ("ERROR: unable to decode abr format version %d (subver %d)\n",
                 abr_hdr.version, abr_hdr.subversion);
      return -1;
    }

  if (abr_hdr.count == 0)
    {
      g_message ("ERROR: no sample brush fond in %s\n",
                 gimp_filename_to_utf8 (filename));
      return -1;
    }

  image_ID = gimp_image_new (100, 100, GIMP_GRAY);
  gimp_image_set_filename (image_ID, filename);

  for (i = 0; i < abr_hdr.count; i++)
    {
      gchar * text;
      layer_ID = abr_brush_load(abr, &abr_hdr, filename, image_ID, i);
      if (layer_ID == -1)
        g_message ("Warning: problem loading brush #%d in %s\n",
                   i, filename);
      gimp_progress_update ((gdouble) i + 1 / (gdouble) abr_hdr.count);
      text = g_strdup_printf ("%d / %d", i + 1, abr_hdr.count);
      gimp_progress_set_text (text);
      g_free (text);
    }
  
  fclose (abr);
  
  gimp_image_resize_to_layers (image_ID);
  
  return image_ID;

}

static gboolean
abr_reach_8BIM_section(FILE* abr, const gchar *name)
{
  gchar tag[4];
  gchar tagname[5];
  gint32 section_size;
  gint r;

  /* find 8BIMname section */
  while(!feof(abr)) {
    r = fread(&tag, 1, 4, abr);
    if (r != 4) {
      g_print("Error: Cannot read 8BIM tag %s\n", g_strerror(errno));
      return FALSE;
    }
    if (strncmp(tag, "8BIM", 4)) {
      g_print("Error: Start tag not 8BIM\n");
      return FALSE;
    }
    r = fread(&tagname, 1, 4, abr);
    if (r != 4) {
      g_print("Error: Cannot read 8BIM tag name %s\n",
          g_strerror(errno));
      return FALSE;
    }
    tagname[4] = '\0';

    if (!strncmp(tagname, name, 4))
      return TRUE;
    
    section_size = abr_read_long(abr);
    fseek(abr, section_size, SEEK_CUR);
  }
  return FALSE;
}

static gint32
rle_decode(FILE *abr, gchar *buffer, gint32 height)
{
  gint32 n;
  gchar ch;
  gint i, j, c;
  gshort *cscanline_len;
  gchar *data = buffer;

  /* read compressed size foreach scanline */
  cscanline_len = g_new0(gshort, height);
  for (i = 0; i < height; i++)
    cscanline_len[i] = abr_read_short(abr);
  
  /* unpack each scanline data */
  for (i = 0; i < height; i++) {
    for (j = 0; j < cscanline_len[i];) {
      n = abr_read_char(abr);
      j++;
      if (n >= 128)     /* force sign */
        n -= 256;
      if (n < 0) {      /* copy the following char -n + 1 times */
        if (n == -128)  /* it's a nop */
          continue;
        n = -n + 1;
        ch = abr_read_char(abr);
        j++;
        for (c = 0; c < n; c++, data++)
          *data = ch;
      } else {          /* read the following n + 1 chars (no compr) */
        for (c = 0; c < n + 1; c++, j++, data++)
          *data = abr_read_char(abr);
      }
    }
  }
  g_free(cscanline_len);

  return 0;
}
