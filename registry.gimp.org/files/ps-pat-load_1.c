/*
 * Photoshop Pattern Loader Plugin version 1.2
 *
 * This GIMP plug-in is designed to load Adobe Photoshop(tm) pattern set (.pat)
 * The complete set is loaded as a new RGB image, one pattern per layer.
 * Only grayscale, indexed and RGB pattern are currently handled.
 *
 * Eric Lamarque <eric.lamarque@free.fr>
 *
 *          Copyright (C) 2008 Eric Lamarque
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
 *  2008-10-26 / v1.0 / Eric Lamarque
 *       Initial release :
 *       - load grayscale patterns
 *       - load indexed patterns
 *       - load rgb patterns
 *  2008-11-01 / v1.1 / Eric Lamarque
 *       Load 16 bits samples and convert them to 8 bits
 *  2008-11-04 / v1.2 / Eric Lamarque
 *       Handle 8 bits alpha channel.
 */

/* *** USER DEFINES *** */
//#define DEBUG_PAT
/* *** END OF USER DEFINES *** */

#include <errno.h>
//#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>

#include <glib.h>
#include <glib/gstdio.h>

#include <libgimp/gimp.h>

/* Local types etc
 */


/* Declare some local functions.
 */
static void   query      (void);
static void   run        (const gchar      *name,
                          gint              nparams,
                          const GimpParam  *param,
                          gint             *nreturn_vals,
                          GimpParam       **return_vals);

static gint32   load_image         (const gchar *filename);

static gchar *  pspat_read_ucs2_text (FILE *f);
static gint32   pspat_load           (FILE *f, const gchar *filename);
static gboolean pspat_is_8BPT        (FILE *f);

static gboolean pspat_read_char       (FILE *f, gchar *c);
static gboolean pspat_read_ushort     (FILE *f, guint16 *s);
static gboolean pspat_read_ulong      (FILE *f, guint32 *s);
static gint32   rle_decode            (FILE *abr, gchar *buffer, gint32 height);
static gint32   pat_8bitchannel_copy  (gchar *raw_data , gchar *buffer,
                                       gint32 size, gint step);
static gint32   pat_16bitchannel_copy (gchar *raw_data , gchar *buffer,
                                       gint32 size, gint step);

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

  gimp_install_procedure ("file_pspat_load",
                          "loads patterns of the Photoshop(tm) PAT file format",
                          "This filter loads patterns of Adobe Photoshop(tm) PAT format.",
                          "Eric Lamarque",
                          "Eric Lamarque",
                          "v1.2 - 04 Nov. 2008",
                          "Photoshop Pattern",
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (load_args),
                          G_N_ELEMENTS (load_return_vals),
                          load_args, load_return_vals);

  gimp_register_file_handler_mime ("file_pspat_load", "image/x-photoshop-pat");

  gimp_register_magic_load_handler ("file_pspat_load",
                                    "pat",
                                    "",
                                    "0,string,8BPT");
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

  image_ID = load_image (param[1].data.d_string);

  if (image_ID != -1)
  {
    *nreturn_vals = 2;
    values[0].data.d_status = GIMP_PDB_SUCCESS;
    values[1].type = GIMP_PDB_IMAGE;
    values[1].data.d_image = image_ID;
  }
}

static gint32
load_image (const gchar *filename)
{
  gchar *temp;
  FILE  *f;

  f = g_fopen (filename, "rb");
 
  if (f == NULL)
    {
      g_message ("Could not open '%s' for reading: %s",
                 gimp_filename_to_utf8 (filename), g_strerror (errno));
      return -1;
    }

  temp = g_strdup_printf ("Opening '%s'...",
                          gimp_filename_to_utf8 (filename));
  gimp_progress_init (temp);
  g_free (temp);

  return pspat_load (f, filename);
}


static gboolean
pspat_read_char (FILE *f, gchar *c)
{
  gint r;

  g_return_val_if_fail ( f != NULL, FALSE );
  g_return_val_if_fail ( c != NULL, FALSE );

  r = fread (c, 1, 1, f);
  if (r != 1)
    return FALSE;

  return TRUE;
}

static gboolean
pspat_read_ushort (FILE *f, guint16 *s)
{
  gint r;

  g_return_val_if_fail ( f != NULL, FALSE );
  g_return_val_if_fail ( s != NULL, FALSE );

  r = fread (s, 2, 1, f);
  if (r != 1)
    return FALSE;

  *s = GUINT16_FROM_BE (*s);

  return TRUE;
}

static gboolean
pspat_read_ulong (FILE *f, guint32 *l)
{
  gint r;

  g_return_val_if_fail ( f != NULL, FALSE );
  g_return_val_if_fail ( l != NULL, FALSE );

  r = fread (l, 4, 1, f);
  if (r != 1)
    return FALSE;

  *l = GUINT32_FROM_BE (*l);

  return TRUE;
}

static gchar*
pspat_read_ucs2_text (FILE *f)
{
  guint32  name_size;
  guint32  buf_size;
  gchar  *name_ucs2;
  gchar  *name_utf8;
  gint    r;

  /* two-bytes characters encoded (UCS-2)
   *  format:
   *   long : size - number of characters in string
   *   data : zero terminated UCS-2 string
   */
  if (!pspat_read_ulong (f, &name_size))
    {
      return (gchar*) NULL;
    }
  if (name_size == 0)
    return (gchar*) NULL;

  buf_size = name_size * 2;

  name_ucs2 = (gchar*) g_malloc (buf_size * sizeof (gchar));

  r = fread (name_ucs2, buf_size, 1, f);
  if (r != 1)
    {
      g_free (name_ucs2);
      return (gchar*) NULL;
    }

  name_utf8 = g_convert (name_ucs2, buf_size,
                         "UTF-8", "UCS-2BE",
                         NULL, NULL, NULL);

  g_free (name_ucs2);

  return name_utf8;
}

static gboolean
pspat_is_8BPT (FILE* f)
{
  gchar tag[4];
  gint r;

  /* find 8BPT header */
  r = fread (&tag, 1, 4, f);
  if (r != 4) 
    {
      g_message ("Error: Cannot read 8BPT tag %s\n", g_strerror (errno));
      return FALSE;
    }
  if (strncmp (tag, "8BPT", 4))
    {
      g_message ("Error: Start tag not 8BPT\n");
      return FALSE;
    }
  return TRUE;
}

static gint32
pat_sample_parse (FILE *f, guchar *buffer, gint step)
{
  guint32 version;
  guint32 depth_unused;
  guint32 top, left, bottom, right;
  guint16 depth;
  gchar   compression;

  guint32 width, height;
  guint32 size;

  guint32 sample_size;
  guint32 sample_start;

  gchar  *raw_data;
  gint32  r = 0;

  if (!pspat_read_ulong (f, &version))
    return -1;
  if (!pspat_read_ulong (f, &sample_size))
    return -1;
  sample_start = ftell (f);

  if (!pspat_read_ulong (f, &depth_unused)) /* twice */
    return -1;

  if (!pspat_read_ulong (f, &top))
    return -1;
  if (!pspat_read_ulong (f, &left))
    return -1;
  if (!pspat_read_ulong (f, &bottom))
    return -1;
  if (!pspat_read_ulong (f, &right))
    return -1;
  if (!pspat_read_ushort (f, &depth))
    return -1;
  if (!pspat_read_char (f, &compression))
    return -1;

  if ((depth != 8) && (depth != 16))
    return -1;

  height = bottom - top;
  width = right - left;
  size = width * (depth >> 3) * height;

  raw_data = g_malloc (size);

  /* data decoding */
  if (!compression) {
    /* not compressed - read raw bytes */
    r = (fread(raw_data, size, 1, f) == 1) ? 0 : -1;
  } else {
    r = rle_decode (f, raw_data, height * (depth >> 3));
  }

  if (r == 0) {
    /* convert/copy data from raw_data to buffer */
    switch (depth) {
    case 8:
      pat_8bitchannel_copy (raw_data , (gchar*)buffer, size, step);
      break;
    case 16:
      pat_16bitchannel_copy (raw_data , (gchar*)buffer, size, step);
      break;
    default:
      r = -1;
      break;
    }
  }

  g_free (raw_data);
  
  return r;
}

static gint32
pspat_sample_load (FILE *f, gint32 image_ID, gint32 id)
{
  guint32 pat_version;
  guint32 image_type;
  guint32 nb_channels;
  gushort sheight;
  gushort swidth;
  guint32 pattern_size;
  guint32 pattern_start;
  guint32 next_pattern;
  guint32 color_model;

  guint32 top, left, bottom, right;
  guint32 depth;

  guint32 width, height;
  guint32 size, ch_size;

  gint i = 0;
  gchar *name = NULL;
  guchar *cmap;
  guchar colidx;
  gushort pal_size = 0;

  gint32            layer_ID = -1;
  GimpDrawable     *drawable;
  GimpPixelRgn      pixel_rgn;
  guchar           *buffer, *dst;

  g_return_val_if_fail ( f != NULL, -1 );

  if (!pspat_read_ulong (f, &pat_version))
    return -1;
  if (!pspat_read_ulong (f, &image_type))
    return -1;
  if (!pspat_read_ushort (f, &sheight))
    return -1;
  if (!pspat_read_ushort (f, &swidth))
    return -1;

  name = pspat_read_ucs2_text (f);

  fseek(f, 37, SEEK_CUR); /* discard id/tag */

  if (image_type == 2) {
    cmap = (guchar*) g_malloc (768);
    if (fread (cmap, 768, 1, f) != 1)
      return -1;
    color_model = 2;
    if (!pspat_read_ushort (f, &pal_size))
      return -1;
    fseek(f , 2, SEEK_CUR); /* unknown short = -1 */
  }

  if (!pspat_read_ulong (f, &color_model))
    return -1;

  switch (image_type) {
  case 3: /* RGB */
    nb_channels = 3;
    break;
  case 2: /* indexed */
    nb_channels = 1;
    break;
  case 1: /* GRAY */
    nb_channels = 1;
    break;
  default:
    g_message("Unsupported color model %d\n", color_model);
    return -1;
    break;
  }

  if (!pspat_read_ulong (f, &pattern_size))
    return -1;
  pattern_start = ftell(f);
  next_pattern = pattern_size + pattern_start;

  if (!pspat_read_ulong (f, &top))
    return -1;
  if (!pspat_read_ulong (f, &left))
    return -1;
  if (!pspat_read_ulong (f, &bottom))
    return -1;
  if (!pspat_read_ulong (f, &right))
    return -1;
  if (!pspat_read_ulong (f, &depth))
    return -1;
#ifdef DEBUG_PAT
  g_print("top %d left %d bottom %d right %d depth %d channels %d(type=%d)"
	  " name %s\n",
	  top, left, bottom, right, depth, nb_channels, image_type, name);
#endif

  height = bottom - top;
  width = right - left;
  //size = width * (depth >> 3) * height;
  /* for now always provide RGB on 24bits */
  size = width * 3 * height;

  layer_ID = gimp_layer_new (image_ID, name, width, height,
			     GIMP_RGB_IMAGE, 100, GIMP_NORMAL_MODE);
  gimp_image_add_layer (image_ID, layer_ID, -1);

  gimp_layer_set_offsets(layer_ID, left, top);

  g_free (name);

  drawable = gimp_drawable_get (layer_ID);
  gimp_pixel_rgn_init (&pixel_rgn, drawable,
		       0, 0, drawable->width, drawable->height,
		       TRUE, FALSE);

  buffer = (guchar*) g_malloc (size);
  dst = buffer;

  for (i=0 ; i < nb_channels; i++) {
    pat_sample_parse(f, dst + i, nb_channels);
  }

  if (nb_channels < 3) {
    ch_size = width * height;
    switch (image_type) {
    case 1:
      /* duplicate bitmap from red channel to blue and green */
      for (i = ch_size - 1; i >=0; i--) {
	buffer [i*3+2] = buffer[i*3+1] = buffer[i*3] = buffer [i];
      }
      break;
    case 2:
      /* fill rgb image with colormap data */
      for (i = ch_size - 1; i >=0; i--) {
	colidx = buffer[i];
	memcpy(&dst[i*3], &cmap[colidx*3], 3);
      }
      break;
    }
  }
  
  gimp_pixel_rgn_set_rect(&pixel_rgn, buffer,
                              0, 0, width, height);

  gimp_drawable_flush (drawable);

  g_free (buffer);

  /* if there is room for 88 bytes + 31 bytes of sample header */
  if (ftell(f) < next_pattern - (88 + 31)) {
    /* there is an alpha channel */
    gint32            mask_id = -1;
    GimpDrawable     *mask_drawable;
    GimpPixelRgn      mask_rgn;
    guchar           *mask_buffer;

    fseek(f, 88, SEEK_CUR);
    
    /* for now only provide a 8bits alpha channel */
    mask_buffer = (guchar*) g_malloc (width * height);

    pat_sample_parse(f, mask_buffer, 1);

    gimp_layer_add_alpha (layer_ID);
    mask_id = gimp_layer_create_mask (layer_ID, GIMP_ADD_WHITE_MASK);
    gimp_layer_add_mask (layer_ID, mask_id);
    mask_drawable = gimp_drawable_get (mask_id);
    gimp_pixel_rgn_init (&mask_rgn, mask_drawable,
			 0, 0, width, height,
			 TRUE, FALSE);
  
    gimp_pixel_rgn_set_rect(&mask_rgn, mask_buffer,
			    0, 0, width, height);
    gimp_drawable_flush (mask_drawable);
    g_free (mask_buffer);

    gimp_layer_remove_mask (layer_ID, GIMP_MASK_APPLY);

  }


  fseek(f, pattern_start, SEEK_SET);
  fseek(f, pattern_size, SEEK_CUR);

  return layer_ID;
}

static gint32
pspat_load (FILE *f, const gchar *filename)
{
  guint16 version;
  guint32 pat_count;
  gint32 image_ID;
  gint i;
  gint32 layer_ID;

  if (!pspat_is_8BPT (f))
    {
      g_message("Not a photoshop pattern file\n");
      return -1;
    }

  if (!pspat_read_ushort (f, &version))
    return -1;
  
  if (version != 1)
    {
      g_message ("ERROR: unable to decode pat format version %d\n",
                 version);
      return -1;
    }

  if (!pspat_read_ulong (f, &pat_count))
    return -1;

  if (pat_count == 0)
    {
      g_message ("ERROR: no patterns found in %s\n",
                 gimp_filename_to_utf8 (filename));
      return -1;
    }

  image_ID = gimp_image_new (100, 100, GIMP_RGB);
  gimp_image_set_filename (image_ID, filename);

  for (i = 0; i < pat_count; i++)
    {
      layer_ID = pspat_sample_load(f, image_ID, i);
      if (layer_ID == -1)
        g_message ("Warning: problem loading pattern #%d in %s\n",
                   i, filename);
      gimp_progress_update ((gdouble) i+1 / (gdouble) pat_count);
    }

  fclose (f);

  gimp_image_resize_to_layers (image_ID);

  return image_ID;

}

static gint32
rle_decode (FILE *f, gchar *buffer, gint32 height)
{
  gint32 n;
  gchar ch, nc;
  gint i, j, c;
  gushort *cscanline_len;
  gchar *data = buffer;

  /* read compressed size foreach scanline */
  cscanline_len = g_new0(gushort, height);
  for (i = 0; i < height; i++)
    if (!pspat_read_ushort (f, &cscanline_len[i]))
      return -1;

  /* unpack each scanline data */
  for (i = 0; i < height ; i++) {
    for (j = 0; j < cscanline_len[i];) {
      if (!pspat_read_char (f, &nc))
	return -1;
      n = (gint32) nc;
      j++;
      if (n >= 128)     /* force sign */
        n -= 256;
      if (n < 0) {      /* copy the following char -n + 1 times */
        if (n == -128)  /* it's a nop */
          continue;
        n = -n + 1;

	if (!pspat_read_char (f, &ch))
	  return -1;
        j++;
        for (c = 0; c < n; c++, data++)
          *data = ch;
      } else {          /* read the following n + 1 chars (no compr) */
        for (c = 0; c < n + 1; c++, j++, data++)
	  if (!pspat_read_char (f, data))
	    return -1;
      }
    }
  }

  g_free(cscanline_len);

  return 0;
}

static gint32
pat_8bitchannel_copy (gchar *raw_data , gchar *buffer,
                      gint32 size, gint step)
{
  guint32 i;
  gchar *dest;

  for (i=0, dest=buffer; i<size; i++, dest+=step) {
    *dest = raw_data[i];
  }

  return 0;
}

static gint32
pat_16bitchannel_copy (gchar *raw_data , gchar *buffer,
                       gint32 size, gint step)
{
  guint32 i;
  gchar *dest;

  for (i=0, dest=buffer; i < size; i+=2, dest+=step) {
    *dest = raw_data[i];
  }

  return 0;
}
