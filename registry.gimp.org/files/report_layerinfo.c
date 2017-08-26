/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * report brightness (for quality measure purpose)
 * Copyright (C) 2008 Wolfgang Hofer
 * hof@gimp.org
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* INSTALL: gimptool-2.0 --install report_layerinfo.c
 *
 * developer utility useful to print information about layers
 * of an image (or animation frame image)
 *
 *
 * The report can be performed automatically on animation frames.
 *
 *   - use the "Frames Modify" feature of gimp-gap on the  XCF frames
 *     and apply this layer information report as filter on the processed frames.
 */

//#include "config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <gtk/gtk.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

//#include "libgimp/stdplugins-intl.h"


#define PLUG_IN_PROC        "plug_in_report_layerinfo"
#define PLUG_IN_ITER_NAME   "plug_in_report_layerinfo_Iterator"
#define KEY_ITER_FROM       "plug_in_report_layerinfo_ITER_FROM"
#define KEY_ITER_TO         "plug_in_report_layerinfo_ITER_TO"
#define PLUG_IN_BINARY      "report_brightness"
#define PLUG_IN_VERSION     "Jan 2008, 0.1"

/* The gettext translation domain. */
#define GETTEXT_PACKAGE "gimp20-report-layerinfo"
#ifndef LOCALEDIR
#define LOCALEDIR "/usr/local/share/locale"
#endif

#include <libintl.h>

#define _(String) gettext (String)

#ifdef gettext_noop
#    define N_(String) gettext_noop (String)
#else
#    define N_(String) (String)
#endif

#ifndef HAVE_BIND_TEXTDOMAIN_CODESET
#    define bind_textdomain_codeset(Domain, Codeset) (Domain)
#endif

#define INIT_I18N()	G_STMT_START{                  \
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);         \
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");  \
  textdomain (GETTEXT_PACKAGE);                        \
}G_STMT_END

#ifndef N_
/* for older gimp releases use dummys nls-macros */
#define N_(x)  x
#define _(x)   x
#endif


/***** Prototypes *****/

static void query (void);
static void run   (const gchar      *name,
                   gint              nparams,
                   const GimpParam  *param,
                   gint             *nreturn_vals,
                   GimpParam       **return_vals);


static void      p_do_reporting (gint32 drawable_id, const char *imagename);


/***** Variables *****/

const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,   /* init_proc  */
  NULL,   /* quit_proc  */
  query,  /* query_proc */
  run     /* run_proc   */
};




MAIN()

/* ------------------
 * query
 * ------------------
 */
static void
query (void)
{
  static const GimpParamDef args[] =
  {
    { GIMP_PDB_INT32,    "run-mode",        "Interactive, non-interactive" },
    { GIMP_PDB_IMAGE,    "image",           "Input image"                  },
    { GIMP_PDB_DRAWABLE, "drawable",        "Input drawable"               }
  };

  static GimpParamDef args_iter[] =
  {
        { GIMP_PDB_INT32, "run_mode", "non-interactive" },
        { GIMP_PDB_INT32, "total_steps", "total number of steps (# of layers-1 to apply the related plug-in)" },
        { GIMP_PDB_FLOAT, "current_step", "current (for linear iterations this is the layerstack position, otherwise some value inbetween)" },
        { GIMP_PDB_INT32, "len_struct", "length of stored data structure with id is equal to the plug_in  proc_name" },
  };

  gimp_install_procedure (PLUG_IN_PROC,
                          N_("Reports layer information of an image."),
                          "Reports imagename, layername and offset to stdout "
                          "TODO ##########################." ,
                          "Wolfgang Hofer",
                          "Wolfgang Hofer",
                          PLUG_IN_VERSION,
                          N_("Report Layerinfo..."),
                          "RGB*, GRAY*",
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (args), 0,
                          args, NULL);

  gimp_plugin_menu_register (PLUG_IN_PROC, "<Image>/Filters");



   /* the installation of the Iterator procedure for
    * animated apply with varying values.
    * (useful if GIMP_GAP is installed)
    */
  gimp_install_procedure (PLUG_IN_ITER_NAME,
                          "This procedure calculates the modified values "
                          "for one iterationstep for the call of "
                          "plug_in_report_layerinfo",
                          "",
                          "Wolfgang Hofer",
                          "Wolfgang Hofer",
                          PLUG_IN_VERSION,
                          NULL,    /* do not appear in menus */
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (args_iter), 0,
                          args_iter, NULL);

}  /* end query */

/* ------------------
 * run
 * ------------------
 */
static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam values[1];
  GimpRunMode        run_mode;
  GimpPDBStatusType  status;
  gint32             image_id;
  gint32             drawable_id;

  status   = GIMP_PDB_SUCCESS;
  run_mode = param[0].data.d_int32;

  INIT_I18N ();

  *nreturn_vals = 1;
  *return_vals  = values;

  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;



  /* check if caller wants to run the iterator
   */
  if (strcmp (name, PLUG_IN_ITER_NAME) == 0)
  {
      /* dummy iterator does nothing useful
       * (but presence of the itrator makes the plug in
       *  callable in GAP animated filter calls)
       */
      status = GIMP_PDB_CALLING_ERROR;
      values[0].data.d_status = status;
      return;
  }


  /* code for the case where caller requested
   * to run the reporting features
   */
  image_id = param[1].data.d_image;
  drawable_id = param[2].data.d_drawable;

  /* reporting of the layer information */
  p_do_reporting (drawable_id, gimp_image_get_name(image_id));

  if (run_mode == GIMP_RUN_INTERACTIVE)
  {
    gint32 dummy;
    dummy = 1;

    /* set dummy data (required to fulfill the API for automated
     * apply via GIMP-GAP Frames Modify feature)
     */
    gimp_set_data (PLUG_IN_PROC, &dummy, sizeof (gint32));
  }


  values[0].data.d_status = status;

}  /* end run */

/* --------------------------------
 * p_do_reporting
 * --------------------------------
 * print imagename, layername and offset
 */
static void
p_do_reporting (gint32 drawable_id, const char *imagename)
{
  gint     l_offsetx, l_offsety;

  gimp_drawable_offsets(drawable_id, &l_offsetx, &l_offsety);

  printf("image:%s; drawable:%s; x=%d; y=%d; width=%d, height=%d\n"
     , imagename
     , gimp_drawable_get_name(drawable_id)
     , l_offsetx
     , l_offsety
     , gimp_drawable_width(drawable_id)
     , gimp_drawable_height(drawable_id)
     );


}  /* end p_do_reporting */
