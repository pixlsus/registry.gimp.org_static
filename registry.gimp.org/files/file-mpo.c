/* 3D MPO loader for Gimp - Version 0.1 
 *
 * This plug-in is based on mposplit.c 
 *    A small tool for splitting MPO files into their JPG components.
 *    $Id: mposplit.c,v 1.5 2012/06/24 01:16:33 chris Exp $
 *    Copyright (C) 2009-2012, Christian Steinruecken. All rights reserved.
 * 
 *    This code is released under the Revised BSD Licence.
 * 
 *    Redistribution and use in source and binary forms, with or without
 *    modification, are permitted provided that the following conditions
 *    are met:
 *    
 *      - Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      - Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      - The names of the author(s) and contributors may not be used to
 *        endorse or promote products derived from this software without
 *        specific prior written permission.
 *
 *    DISCLAIMER:
 *    This software is provided by the copyright holders and contributors
 *    "as is" and any express or implied warranties, including, but not
 *    limited to, the implied warranties of merchantability and fitness for
 *    a particular purpose are disclaimed.  In no event shall the copyright
 *    holders be liable for any direct, indirect, incidental, special,
 *    exemplary, or consequential damages (including, but not limited to,
 *    procurement of substitute goods or services; loss of use, data, or
 *    profits; or business interruption) however caused and on any theory of
 *    liability, whether in contract, strict liability, or tort (including
 *    negligence or otherwise) arising in any way out of the use of this
 *    software, even if advised of the possibility of such damage.
 *
 *
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
 */



#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <glib/gstdio.h>

#ifdef G_OS_WIN32
#include <io.h>
#endif

#include "libgimp/gimp.h"
#include "libgimp/gimpui.h"


#define LOAD_PROC        "file-mpo-load"
#define SAVE_PROC        "file-mpo-save"
#define PLUG_IN_BINARY   "file-mpo"
#define PLUG_IN_ROLE     "gimp-file-mpo"

static void              query               (void);
static void              run                 (const gchar      *name,
                                              gint              nparams,
                                              const GimpParam  *param,
                                              gint             *nreturn_vals,
                                              GimpParam       **return_vals);


static gboolean          load_image          (const gchar  *filename,
                                              GError      **error);
static gboolean          split_mpo           (const gchar  *filename);
static void              delete_layers       (void);

static gint           num_images = 0; /* Number of images in MPO file */
static gchar        **image_name;
static FILE          *fp;
static gint32         image_id;
static gint32         layer_id[8]; /* Number of images in MPO cannot exceed 8*/
const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,   /* init_proc  */
  NULL,   /* quit_proc  */
  query,  /* query_proc */
  run,    /* run_proc   */
};

MAIN()

static void
query (void)
{
  static const GimpParamDef load_args[] =
  {
    { GIMP_PDB_INT32,  "run-mode",     "The run mode { RUN-NONINTERACTIVE (1) }" },
    { GIMP_PDB_STRING, "filename",     "The name of the file to load" },
    { GIMP_PDB_STRING, "raw-filename", "The name entered"             }
  };

  static const GimpParamDef load_return_vals[] =
  {
    { GIMP_PDB_IMAGE, "image", "Output image" }
  };

  gimp_install_procedure (LOAD_PROC,
                          "Load MPO files",
                          "No help till now",
                          "Sashi Kumar <ksashikumark93@gmail.com>",
                          "Sashi Kumar <ksashikumark93@gmail.com>",
                          "24th April 2013",
                          "3D MPO Data",
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (load_args),
                          G_N_ELEMENTS (load_return_vals),
                          load_args, load_return_vals);

  gimp_register_load_handler (LOAD_PROC, "mpo", "");


}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam   values[2];
  GimpRunMode        run_mode;
  GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
  GError            *error  = NULL;


  run_mode = param[0].data.d_int32;

  *nreturn_vals = 1;
  *return_vals  = values;

  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;

  if (strcmp (name, LOAD_PROC) == 0)
    {
      
      if (load_image (param[1].data.d_string, &error))
        {
          *nreturn_vals = 2;
           values[1].type         = GIMP_PDB_IMAGE;
           values[1].data.d_image = image_id;
        }
      else
        {
          status = GIMP_PDB_EXECUTION_ERROR;
        }
    }
  else
    {
      status = GIMP_PDB_CALLING_ERROR;
    }


  if (status != GIMP_PDB_SUCCESS && error)
    {
      *nreturn_vals = 2;
      values[1].type           = GIMP_PDB_STRING;
      values[1].data.d_string  = error->message;
    }

  values[0].data.d_status = status;
}

static gboolean
load_image (const gchar  *filename,
            GError      **error)
{
  gint32  size;
  gint    i;
  GdkPixbuf* pixbuf;
  gboolean status;  

  fp = g_fopen (filename, "rb");
  
  if (!fp)
    {
      g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                   "Could not open '%s' for reading: %s",
                   gimp_filename_to_utf8 (filename), g_strerror (errno));
      return FALSE;
    }

 
  if (split_mpo (filename))
    {
      for (i = num_images - 1; i >= 0; i--)
        {
          GError *gerror = NULL;
          gchar  *layer_name = g_new0 (gchar, 100); /* Bad Assumption */
          sprintf (layer_name, "image#%d", i+1);
          pixbuf = gdk_pixbuf_new_from_file(image_name[i], &gerror);
          
          remove (image_name[i]); /* FIXME: Make IO operations more efficient, blind delete is dangerous */         
          
          if (pixbuf)
            {
            
              if (i == num_images - 1)
                {
                  image_id = gimp_image_new (gdk_pixbuf_get_width (pixbuf),
                                             gdk_pixbuf_get_height (pixbuf),
                                             GIMP_RGB);
                  gimp_image_set_filename (image_id, filename);
                }

              layer_id[i] = gimp_layer_new_from_pixbuf (image_id, layer_name,
                                                        pixbuf,
                                                        100.,
                                                        GIMP_NORMAL_MODE, 0, 0);
              
              gimp_image_insert_layer (image_id, layer_id[i], -1, -1);
              
              status = TRUE;
              g_object_unref (pixbuf);
        
           }
          else
            { 
              g_printf ("\nError message: %s\n", gerror->message); 
              exit (1);
            }

          free (layer_name);
          free (image_name[i]);
        }

      gimp_image_resize_to_layers (image_id); /* Resize the image to the maximum layer size */
  
      if (num_images > 2)
        delete_layers();
    }
  else
    status = FALSE;

  return status;

}

/* mposplit - Split MPO file into their JPG components. */
static gboolean
split_mpo (const gchar  *filename)
{
  size_t length;  /* Total length of file */
  size_t amount;  /* Amount read */
  gint   i = 0;
  gchar* buffer;
  gchar* fnmbase;
  gchar* ext;
  gchar *temp;

  fnmbase = strdup(filename);

  ext = strstr(fnmbase,".MPO");

  if (ext != NULL) 
      ext[0] = '\0';

  ext = strstr(fnmbase,".mpo");

  if (ext != NULL) 
      ext[0] = '\0';

  /* Obtain file size: */
  fseek(fp, 0, SEEK_END);
  length = ftell(fp);
  rewind(fp);

  /* Allocate memory to contain the whole file: */
  buffer = g_new0 (gchar ,length);

  amount = fread(buffer,1,length,fp);
  if (amount != length) 
      return FALSE;
  fclose(fp);

  /* Now find the individual images */

  gchar* view = buffer;
  gchar* last = NULL;
  image_name  = g_new (gchar *, 128); /* Assuming a maximum of 128  layers */

  while (view < buffer+length-4) 
    {
      if (((char) view[0] % 255) == (char) 0xff) 
        {
          if (((char) view[1] % 255) == (char) 0xd8) 
            {
              if (((char) view[2] % 255) == (char) 0xff) 
                {
                  if (((char) view[3] % 255) == (char) 0xe1)/* FIXME: Make generalized check
                                                                      for JFIF tag 0xff 0xe0 */ 
                    {
                      num_images++;
                      if (last != NULL) 
                        {
                          /* copy out the previous view */                   
                          image_name[i] = malloc (sizeof(gchar) * 200); /* Bad Assumption */
                          sprintf(image_name[i], "%s.image#%d", fnmbase, num_images-1);
                          FILE* w = fopen(image_name[i], "wb");
                          fwrite(last, 1, view-last, w);
                          fclose(w);
                          i++;
                        }
                      last = view;
                      view+=4;
                    } 
                  else 
                    view+=2;
                } 
              else 
                view+=3;
            } 
          else 
            view+=1;
        } 
      else 
        view+=1;
    }

  if (num_images > 1) 
    {
      
      image_name[i] = malloc (sizeof(gchar) * 200);;
      sprintf(image_name[i], "%s.image#%d", fnmbase, num_images);
      FILE* w = fopen(image_name[i], "wb");
      fwrite(last, 1, buffer+length-last, w);
      fclose(w);
    } 

  g_free(buffer);

  return TRUE;

}

/* delete_layers()
 *              The MPO file has only two views. But the file may contain duplicate images.
 * Hence they must be removed from the buffer to reduce the memory occupied by the file
 */

static void
delete_layers (void)
{
  gint width, height;
  gint i;

  width = gimp_image_width(image_id);
  height = gimp_image_height(image_id);

  for (i = 0; i < num_images; i++)
    {
      if ((gimp_drawable_width (layer_id[i]) < width) && (gimp_drawable_height(layer_id[i]) < height))
        gimp_image_remove_layer (image_id, layer_id[i]);
      else
        gimp_drawable_set_name (
    }
}
