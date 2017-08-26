/* INSTALL: gimptool-2.0 --install isle.c
 */

/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * Selection remove isles filter
 * Copyright (C) 2006, 2011 Wolfgang Hofer
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

/* 2011.01.22 replaced recursive calls by list based processing. recursive calls leaded
 *            to stack overflow crash when processing large areas.
 *            added progress handling.
 */


//#include "config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gtk/gtk.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

//#include "libgimp/stdplugins-intl.h"


#define PLUG_IN_PROC        "plug_in_selection_subtract_isles"
#define PLUG_IN_BINARY      "isles"
#define PLUG_IN_VERSION     "2011.01.22, 1.1"

/* The gettext translation domain. */
#define GETTEXT_PACKAGE "gimp20-selection-isles"
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


/***** Macro definitions  *****/

/* the algorithm makes a copy of the current selection channel
 * that is refered as isle_drawablbe.
 * at begin the isle_drawable is converted where all values
 * are reduced to SEL_NONE (for unselected pixels) and SEL_UNCHECKED
 * (for selected pixels)
 * Next continous areas of pixels with value SEL_UNCHECKED are processed
 * and marked with value SEL_VISITED while processing of an area is in progress.
 * In case the area is identified as small area, it is marked
 * with value SEL_FULL.
 * -- see procedure p_clear_areas_by_size --
 * after all areas were checked, the big areas are cleared 
 * (from SEL_VISITED) to SEL_NONE in the isle_drawable.
 * -- see p_clear_all_not_full_selected_pixels --
 * finally the isle_drawable, that now contains only the values
 * SEL_NONE and SEL_FULL (for the small isles) is subtracted from
 * the original selection.
 */
 

#define SEL_NONE           0
#define SEL_FULL         255
#define SEL_UNCHECKED    250
#define SEL_VISITED        1


/***** Magic numbers *****/

#define SCALE_WIDTH  200
#define SPIN_BUTTON_WIDTH 60
#define ISLE_RESPONSE_RESET 1

/***** Types *****/

typedef struct
{
  gdouble  pixel_diagonal;
  gint     pixel_limit;
  gint     threshold;
  gboolean connect_by_corner;
  gboolean affect_selected;
  gboolean affect_unselected;
  gboolean clear_below_threshold;
  gboolean keep_workchannels;
} isle_vals_t;


typedef struct
{
  gboolean          run_flag;
  isle_vals_t      *isleval_ptr;

  GtkWidget *dialog;
  GtkWidget *check_affect_selected;
  GtkWidget *check_affect_unselected;
  GtkWidget *check_clear_below_threshold;
  GtkWidget *check_connect_by_corner;
  GtkWidget *check_keep_workchannels;
  GtkObject *adj_pixel_diagonal;
  GtkObject *adj_pixel_limit;
  GtkObject *adj_threshold;
  
} dialog_widgets_t;

typedef struct AreaCoordPoint {
     gint            x;
     gint            y;
     void           *next;
} AreaCoordPoint;

typedef struct AreaContext_t
{
  gint32 max_ix;
  gint32 max_iy;
  gint32 min_ix;
  gint32 min_iy;
  gint32 seed_ix;
  gint32 seed_iy;
  gint32 pixel_count;
  guchar marker;

  isle_vals_t      *isleval_ptr;
  GimpDrawable     *isle_drawable;
  GimpPixelFetcher *pft;
  
  /* the boundaries of the original  selection */
  gint              sel_x1;
  gint              sel_y1;
  gint              sel_x2;
  gint              sel_y2;
  gint              sel_width;
  gint              sel_height;
  
  AreaCoordPoint   *pointList;
  gint              pointCount;
  
  gint32            stepsProcessed;   /*  a step is implemented as processing one tile or one row */
  gint32            stepsToProcess;
  gboolean          doProgress;
  
} AreaContext_t;


/***** Prototypes *****/

static void query (void);
static void run   (const gchar      *name,
                   gint              nparams,
                   const GimpParam  *param,
                   gint             *nreturn_vals,
                   GimpParam       **return_vals);

static void      p_handle_progress_step(AreaContext_t *acontext_ptr, const char *name);
static void      p_init_default_values (isle_vals_t *isleval_ptr);
static void      p_init_islePr(AreaContext_t *acontext_ptr, GimpPixelRgn *islePR_ptr);

static void      p_exec_selection_processing  (gint32 image_id
                         , AreaContext_t    *acontext_ptr
                         );
static void      p_subtract_selected_isles  (gint32 image_id
                         , AreaContext_t *acontext_ptr
                         );

static void      p_clear_selection_areas_by_size(AreaContext_t *acontext_ptr);
static void      p_reset_area_context(AreaContext_t *acontext_ptr
                         , gint32 ix, gint32 iy);


static inline void  p_add_point(AreaContext_t *acontext_ptr, gint32 ix, gint32 iy);
static inline void  p_remove_first_point(AreaContext_t *acontext_ptr);

static gboolean  p_mark_area(AreaContext_t *acontext_ptr);
static void      p_check_and_mark_seed_pixel_and_neighbours(AreaContext_t *acontext_ptr);

static void      p_check_and_mark_selected_pixel(AreaContext_t *acontext_ptr
                         , gint32 ix, gint32 iy);
static void      p_mark_pixel(AreaContext_t *acontext_ptr
                         , gint32 ix, gint32 iy);

static void      p_set_all_selected_pixels_unchecked(AreaContext_t *acontext_ptr);
static void      p_clear_all_not_full_selected_pixels(AreaContext_t *acontext_ptr);
static void      p_clear_selection_below_threshold(AreaContext_t *acontext_ptr
                         , GimpDrawable *selection_drawable);

static gdouble   p_compute_diagonal(AreaContext_t *acontext_ptr);
static void      p_debug_print_vals(AreaContext_t *acontext_ptr);

static void      p_set_default_values(dialog_widgets_t *gui_ptr);
static void      p_response_callback (GtkWidget *widget
                   ,gint       response_id
                   ,dialog_widgets_t *gui_ptr);
static gboolean  p_isle_dialog(GimpDrawable *drawable, isle_vals_t *isleval_ptr);


/***** Variables *****/

const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,   /* init_proc  */
  NULL,   /* quit_proc  */
  query,  /* query_proc */
  run     /* run_proc   */
};

gboolean gs_debug = FALSE;

/***** Functions *****/

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
    { GIMP_PDB_INT32,    "run-mode",             "Interactive, non-interactive" },
    { GIMP_PDB_IMAGE,    "image",                "Input image"                  },
    { GIMP_PDB_DRAWABLE, "drawable",             "(ignored)"               },
    { GIMP_PDB_FLOAT,    "pixel_diagonal",       "maximum diagonal in pixels to consider as small isle"     },
    { GIMP_PDB_INT32,    "pixel_limit",          "maximum limit of pixels to consider as small isle"     },
    { GIMP_PDB_INT32,    "threshold",            "0..255 selection values below this threshold are treated as not selected"     },
    { GIMP_PDB_INT32,    "connect_by_corner",    "0: area touching only at corner are treated as seperate areas, !=0 are treated a one area" },
    { GIMP_PDB_INT32,    "affect_selected",      "0: dont affect selected isles, !=0 subtract selected isles from selection" },
    { GIMP_PDB_INT32,    "affect_unselected",    "0: dont affect unselected isles, !=0 add unselected isles to selection" },
    { GIMP_PDB_INT32,    "clear_below_threshold","0: clear selection below threshold (no matter if part of isle or not)" },
    { GIMP_PDB_INT32,    "keep_workchannels",    "0: remove work channles when finished, !=0 keep workchannels when finished" }
    
  };


  gimp_install_procedure (PLUG_IN_PROC,
                          N_("Subtract small isles from selection."),
                          "Subtract small isles from the current selection "
                          "and/or add small unselected isles to the current selection.  "
                          "The parameters pixel_diagonal and pixel_limit are used to "
                          "define area size that is considered as small isle. ",
                          "Wolfgang Hofer",
                          "Wolfgang Hofer",
                          PLUG_IN_VERSION,
                          N_("Subtract Small Selection Isles..."),
                          "RGB*, GRAY*",
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (args), 0,
                          args, NULL);

  gimp_plugin_menu_register (PLUG_IN_PROC, "<Image>/Select");

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
  static isle_vals_t gs_isle_vals;  


  GimpRunMode        run_mode;
  GimpPDBStatusType  status;
  GimpDrawable      *drawable;
  gint32             image_id;
  const gchar *l_env;
  AreaContext_t      acontext;
  AreaContext_t     *acontext_ptr;

  status   = GIMP_PDB_SUCCESS;
  run_mode = param[0].data.d_int32;
  acontext_ptr = &acontext;
  
  INIT_I18N ();

  l_env = g_getenv("ISLE_DEBUG");
  if(l_env != NULL)
    {
      if((*l_env != 'n') && (*l_env != 'N')) gs_debug = TRUE;
    }

  *nreturn_vals = 1;
  *return_vals  = values;

  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  acontext_ptr->doProgress = FALSE;

  image_id = param[1].data.d_image;
  /* Get the active drawable info */
  drawable = gimp_drawable_get (param[2].data.d_drawable);

  /* Set the tile cache size */
  gimp_tile_cache_ntiles (2 * drawable->ntile_cols);
  
  /* start with harcoded default values
   * (maybe replaced by last values of the same session)
   */
  p_init_default_values(&gs_isle_vals);

  /* See how we will run */
  switch (run_mode)
    {
    case GIMP_RUN_INTERACTIVE:
      /* Possibly retrieve data */
      gimp_get_data (PLUG_IN_PROC, &gs_isle_vals);
      acontext_ptr->doProgress = TRUE;

      /* Get information from the dialog */
      if (!p_isle_dialog (drawable, &gs_isle_vals))
        return;

      break;

    case GIMP_RUN_NONINTERACTIVE:
      /* Make sure all the arguments are present */
      acontext_ptr->doProgress = FALSE;
      if (nparams != 11)
        {
          status = GIMP_PDB_CALLING_ERROR;
        }
      else
        {
          gs_isle_vals.pixel_diagonal        = param[3].data.d_float;
          gs_isle_vals.pixel_limit           = param[4].data.d_int32;
          gs_isle_vals.threshold             = param[5].data.d_int32;
          gs_isle_vals.connect_by_corner     = param[6].data.d_int32;
          gs_isle_vals.affect_selected       = param[7].data.d_int32;
          gs_isle_vals.affect_unselected     = param[8].data.d_int32;
          gs_isle_vals.clear_below_threshold = param[9].data.d_int32;
          gs_isle_vals.keep_workchannels     = param[10].data.d_int32;
        }

      break;

    case GIMP_RUN_WITH_LAST_VALS:
      /* Possibly retrieve data */
      acontext_ptr->doProgress = TRUE;
      gimp_get_data (PLUG_IN_PROC, &gs_isle_vals);
      break;

    default:
      break;
    }

  /* selection processing */
  if ((status == GIMP_PDB_SUCCESS)
  &&  (image_id >= 0)
  &&  (gimp_selection_is_empty(image_id) != TRUE))
    {

      /* Run! */
      gimp_image_undo_group_start (image_id);
      acontext_ptr->isleval_ptr = &gs_isle_vals;
      p_exec_selection_processing (image_id, acontext_ptr);
      gimp_image_undo_group_end (image_id);

      /* If run mode is interactive, flush displays */
      if (run_mode != GIMP_RUN_NONINTERACTIVE)
        {
          gimp_displays_flush ();
        }

      /* Store data */

      if (run_mode == GIMP_RUN_INTERACTIVE)
        {
          gimp_set_data (PLUG_IN_PROC, &gs_isle_vals, sizeof (isle_vals_t));
        }
    }
  else if (status == GIMP_PDB_SUCCESS)
    {
      status = GIMP_PDB_EXECUTION_ERROR;
    }

  values[0].data.d_status = status;

  gimp_drawable_detach (drawable);
}  /* end run */


/* --------------------------------
 * p_handle_progress_step
 * --------------------------------
 */
static void
p_handle_progress_step(AreaContext_t *acontext_ptr, const char *name)
{
  gdouble progress;

  if (acontext_ptr->doProgress)
    {
      acontext_ptr->stepsProcessed++;
      progress = (gdouble)acontext_ptr->stepsProcessed / (gdouble)acontext_ptr->stepsToProcess;
      gimp_progress_update(progress);
      
//       printf("PROGRESS: %s step:%d total:%d  progress:%f\n"
//          , name
//          , acontext_ptr->stepsProcessed
//          , acontext_ptr->stepsToProcess
//          , progress
//          );
    }

  

}  /* end p_handle_progress_step */


/* --------------------------------
 * p_init_default_values
 * --------------------------------
 * init specified isle_vals with hardcoded default settings.
 */
static void
p_init_default_values (isle_vals_t *isleval_ptr)
{
  isleval_ptr->pixel_diagonal           = 5.0;
  isleval_ptr->pixel_limit              = 20;
  isleval_ptr->threshold                = 128;
  isleval_ptr->connect_by_corner        = FALSE;
  isleval_ptr->affect_selected          = TRUE;
  isleval_ptr->affect_unselected        = FALSE;
  isleval_ptr->clear_below_threshold    = TRUE;
  isleval_ptr->keep_workchannels        = FALSE;
}  /* end p_init_default_values */


/* --------------------------------
 * p_init_islePr
 * --------------------------------
 * initialize pixleregion of the isle drawable
 * (that is a working copy of the selection channel)
 */
static void
p_init_islePr(AreaContext_t *acontext_ptr, GimpPixelRgn *islePR_ptr)
{
  gimp_pixel_rgn_init (islePR_ptr, acontext_ptr->isle_drawable
                      , acontext_ptr->sel_x1
                      , acontext_ptr->sel_y1
                      , acontext_ptr->sel_width
                      , acontext_ptr->sel_height
		      , TRUE      /* dirty */
		      , FALSE     /* shadow */
		       );
}  /* end p_init_islePr */


/* --------------------------------
 * p_exec_selection_processing
 * --------------------------------
 * this procedure performs the processing on the selection channel
 * according to paramters (isleval_ptr)
 */
static void
p_exec_selection_processing (gint32 image_id
   , AreaContext_t    *acontext_ptr)
{
  gboolean          non_empty;
  gint32            bck_sel_height;
  gint32            sel_tiles;
  gint32            bck_sel_tiles;

  gimp_selection_bounds (image_id
				     , &non_empty
				     , &acontext_ptr->sel_x1
				     , &acontext_ptr->sel_y1
				     , &acontext_ptr->sel_x2
				     , &acontext_ptr->sel_y2
				     );

  acontext_ptr->sel_width = acontext_ptr->sel_x2 - acontext_ptr->sel_x1;
  acontext_ptr->sel_height = acontext_ptr->sel_y2 - acontext_ptr->sel_y1;
  sel_tiles = (1 + (acontext_ptr->sel_width / gimp_tile_width()))
            * (1 + (acontext_ptr->sel_height / gimp_tile_height()));

  /* init stuff for progress processing */
  acontext_ptr->stepsProcessed = 0;
  acontext_ptr->stepsToProcess = 0;
  bck_sel_height = acontext_ptr->sel_height;
  bck_sel_tiles = sel_tiles;
  if (acontext_ptr->doProgress)
    {
      gimp_progress_init( _("removing isles from selection..."));
    }
  if (acontext_ptr->isleval_ptr->clear_below_threshold)
    {
      acontext_ptr->stepsToProcess += sel_tiles;
    }
  if (acontext_ptr->isleval_ptr->affect_selected)
    {
      acontext_ptr->stepsToProcess += (sel_tiles + sel_tiles + acontext_ptr->sel_height);
    }
  if (acontext_ptr->isleval_ptr->affect_unselected)
    {
      acontext_ptr->stepsToProcess += (sel_tiles + sel_tiles + acontext_ptr->sel_height);
    }

  
  /* start processing */  
  if (acontext_ptr->isleval_ptr->clear_below_threshold)
    {
      GimpDrawable *selection_drawable;
      selection_drawable = gimp_drawable_get(gimp_image_get_selection(image_id));
      p_clear_selection_below_threshold(acontext_ptr, selection_drawable);
      gimp_drawable_detach(selection_drawable);
    }

  if (acontext_ptr->isleval_ptr->affect_selected)
    {
      p_subtract_selected_isles(image_id, acontext_ptr);
    }

  if (acontext_ptr->isleval_ptr->affect_unselected)
    {
      gimp_selection_invert(image_id);
      gimp_selection_bounds (image_id
				     , &non_empty
				     , &acontext_ptr->sel_x1
				     , &acontext_ptr->sel_y1
				     , &acontext_ptr->sel_x2
				     , &acontext_ptr->sel_y2
				     );
      acontext_ptr->sel_width = acontext_ptr->sel_x2 - acontext_ptr->sel_x1;
      acontext_ptr->sel_height = acontext_ptr->sel_y2 - acontext_ptr->sel_y1;
      sel_tiles = (1 + (acontext_ptr->sel_width / gimp_tile_width()))
            * (1 + (acontext_ptr->sel_height / gimp_tile_height()));
      
      /* recalculate rows to process after invert of the selection */
      acontext_ptr->stepsToProcess -= (bck_sel_height + bck_sel_height + bck_sel_height);
      acontext_ptr->stepsToProcess += (sel_tiles + sel_tiles + acontext_ptr->sel_height);
      p_subtract_selected_isles(image_id, acontext_ptr);
      gimp_selection_invert(image_id);
    }

  if (gs_debug)
    {
      p_debug_print_vals(acontext_ptr);
    }
  if (acontext_ptr->doProgress)
    {
      gimp_progress_update(1.0);
    }
}  /* end p_exec_selection_processing */


/* --------------------------------
 * p_subtract_selected_isles
 * --------------------------------
 */
static void
p_subtract_selected_isles(gint32 image_id, AreaContext_t *acontext_ptr)
{
  gint32            isle_id;

  /* save original selection to a channel (working copy for the isle detection) */
  isle_id = gimp_selection_save(image_id);

  if (gimp_drawable_bpp(isle_id) != 1)
    {
      printf("selection with bpp %d not supported (bpp must be 1)\n"
        , (int)gimp_drawable_bpp(isle_id)
        );
      return;
    }
  acontext_ptr->isle_drawable = gimp_drawable_get(isle_id);

  p_set_all_selected_pixels_unchecked(acontext_ptr);

  p_clear_selection_areas_by_size(acontext_ptr);
  
  gimp_drawable_flush (acontext_ptr->isle_drawable);

  gimp_selection_combine(isle_id, GIMP_CHANNEL_OP_SUBTRACT);
  
  gimp_drawable_detach(acontext_ptr->isle_drawable);
  
  if (!acontext_ptr->isleval_ptr->keep_workchannels)
    {
      gimp_image_remove_channel(image_id, isle_id);
    }
}  /* end p_subtract_selected_isles */


/* --------------------------------
 * p_clear_selection_areas_by_size
 * --------------------------------
 */
static void
p_clear_selection_areas_by_size(AreaContext_t *acontext_ptr)
{
  guchar          pixel[4];
  gboolean        small_area;
  gint            row;
  gint            col;
  gint32          bck_selection_id;


  acontext_ptr->pft = gimp_pixel_fetcher_new(acontext_ptr->isle_drawable
                                             , FALSE   /* shadow */
                                             );
  gimp_pixel_fetcher_set_edge_mode(acontext_ptr->pft, GIMP_PIXEL_FETCHER_EDGE_BLACK);

  for (row = acontext_ptr->sel_y1; row <= ((acontext_ptr->sel_y1 + acontext_ptr->sel_y2)); row++)
    {
      for (col = acontext_ptr->sel_x1; col < acontext_ptr->sel_x2; col++)
        {
          gimp_pixel_fetcher_get_pixel (acontext_ptr->pft, col, row, &pixel[0]);
          
          if ((gs_debug) && (pixel[0] != SEL_NONE))
            {
              printf("SEL_ANY  x:%4d y:%4d value:%d", col, row, (int)pixel[0]);
            }
       
          
          if (pixel[0] == SEL_UNCHECKED)
            {
              if (gs_debug)
                {
                  printf(" UNCHECKED");
                }
              p_reset_area_context(acontext_ptr, col, row);
              small_area = p_mark_area(acontext_ptr);
              if (small_area)
                {
                  acontext_ptr->marker = SEL_FULL;
                  p_mark_area(acontext_ptr);
                  if (gs_debug)
                    {
                      printf(" SMALL");
                    }
                }
            }

          if ((gs_debug) && (pixel[0] != SEL_NONE))
            {
              printf("\n");
            }
            
            
            
        }

      p_handle_progress_step(acontext_ptr, "p_clear_selection_areas_by_size");
    
    }


  /* the isle channel now contains small area isles marked as SEL_FULL
   * and big areas marked as SEL_VISTED.
   * (and unselected areas as SEL_NONE)
   *
   * clear all pixels that are not part of a small area isle
   * alternative option:
   * clear all pixels that are not part of a big area.
   *
   * Note that the isle channel will be subtracted from the original
   * selection.
   */


  p_clear_all_not_full_selected_pixels(acontext_ptr);

  
}  /* p_clear_selection_areas_by_size */



/* --------------------------------
 * p_add_point
 * --------------------------------
 * add specified coords as new element to the begin of the pointList
 */
static void
p_add_point(AreaContext_t *acontext_ptr, gint32 ix, gint32 iy)
{
  AreaCoordPoint *point;

  point = g_new ( AreaCoordPoint, 1 );

  point->x = ix;
  point->y = iy;

  point->next = acontext_ptr->pointList;

  acontext_ptr->pointList = point;

  acontext_ptr->pointCount++;


}  /* end p_add_point */


/* --------------------------------
 * p_remove_first_point
 * --------------------------------
 * remove first element from the pointList
 */
static inline void
p_remove_first_point(AreaContext_t *acontext_ptr)
{
  AreaCoordPoint *point;

  point = acontext_ptr->pointList;

  if (point != NULL)
  {
    acontext_ptr->pointCount--;
    acontext_ptr->pointList = (AreaCoordPoint *)point->next;
    g_free(point);
  }

}  /* end p_remove_first_point */


/* --------------------------------
 * p_mark_area
 * --------------------------------
 * find and mark all selected pixels
 * belonging to the same area as the seed pixel of the area context.
 * return TRUE if the marked are is considered as small isle 
 *                (according to current isle param value settings)
 *        FALSE if the marked area is greater or the seed pixel
 *                is not marked at all.
 */
static gboolean
p_mark_area(AreaContext_t *acontext_ptr)
{
  p_check_and_mark_seed_pixel_and_neighbours(acontext_ptr);
  if (acontext_ptr->pixel_count <= acontext_ptr->isleval_ptr->pixel_limit)
    {
      gdouble diagonal;
      diagonal = p_compute_diagonal(acontext_ptr);
      if (diagonal <= acontext_ptr->isleval_ptr->pixel_diagonal)
        {
          return (TRUE);
        }
    }
  return (FALSE);
}  /* p_mark_area */


/* ------------------------------------------
 * p_check_and_mark_seed_pixel_and_neighbours
 * ------------------------------------------
 * mark a selected but not yet marked pixel
 * with current area context marker value.
 * Also do this for all neighnour pixels.
 *
 * Note: the marker value must be different to SEL_UNCHECKED.
 *
 * Further update the number of pixels and min/max boundaries
 * in the area context.
 *
 * This procedure is typically called with the coords of a seed pixel
 * and returns if the full area of selected pixels surrounding the seed pixel 
 * is marked and analyzed.
 */
static void
p_check_and_mark_seed_pixel_and_neighbours(AreaContext_t *acontext_ptr)
{
  gint32 ix;
  gint32 iy;
  
  ix = acontext_ptr->seed_ix;
  iy = acontext_ptr->seed_iy;
  acontext_ptr->pointList = NULL;
  acontext_ptr->pointCount = 0;

  if(gs_debug)
    {
      printf("\n START check_and_mark seed_ix:%d seed_iy:%d\n"
	   ,(int)acontext_ptr->seed_ix
	   ,(int)acontext_ptr->seed_iy
	   );
      
    }

  while(TRUE)
    {
      p_check_and_mark_selected_pixel(acontext_ptr, ix, iy);
      if(acontext_ptr->pointList == NULL)
        {
          return;
        }
      else
        {
          /* continue check on the remaining neighbour pixels */
          ix = acontext_ptr->pointList->x;
          iy = acontext_ptr->pointList->y;
          p_remove_first_point(acontext_ptr);

        }
    }


}  /* end p_check_and_mark_seed_pixel_and_neighbours */


/* --------------------------------
 * p_check_and_mark_selected_pixel
 * --------------------------------
 * mark a selected but not yet marked pixel
 * with current area context marker value.
 * Also do this for all neighnour pixels.
 *
 * Note: the marker value must be different to SEL_UNCHECKED.
 *
 * Further update the number of pixels and min/max boundaries
 * in the area context.
 *
 * This procedure is typically called with the coords of a seed pixel
 * and returns if the full area of selected pixels surrounding the seed pixel 
 * is marked and analyzed.
 */
static void
p_check_and_mark_selected_pixel(AreaContext_t *acontext_ptr, gint32 ix, gint32 iy)
{
  guchar          pixel[4];


  gimp_pixel_fetcher_get_pixel (acontext_ptr->pft, ix, iy, &pixel[0]);

  if(gs_debug)
    {
      printf("CHK&MARK ix:%d iy:%d marker:%d pixel[0]:%d\n"
        ,(int)ix
        ,(int)iy
        ,(int)acontext_ptr->marker
        ,(int)pixel[0]
	);
    }


  if ((pixel[0] == SEL_NONE) || (pixel[0] == acontext_ptr->marker))
    {
       return;
    }

  p_mark_pixel(acontext_ptr, ix, iy);
  acontext_ptr->pixel_count++;
  acontext_ptr->max_ix = MAX(acontext_ptr->max_ix, ix);
  acontext_ptr->max_iy = MAX(acontext_ptr->max_iy, iy);
  acontext_ptr->min_ix = MIN(acontext_ptr->min_ix, ix);
  acontext_ptr->min_iy = MIN(acontext_ptr->min_iy, iy);
  
  /* trigger check of neighbours */
  /* Note that the pixel fetcher seems to do clipping
   * based on the current selection. pixels on coords sel_x2 sel_y2 are
   * treated as outside (e.g. put attempts on those coords are ignored)
   * therefore we clip at sel_x2 -1 / sel_y2 -1.
   * (otherwise we will run into an endless loop because the border pixels
   * on the sel_x2 sel_y2 border gets never marked when pixel_fetcher_put
   * calls have no effect on those coordinates.)
   */
  if (ix > acontext_ptr->sel_x1)
    {
      p_add_point(acontext_ptr, ix-1, iy);
    }

  if (ix < acontext_ptr->sel_x2 -1)
    {
      p_add_point(acontext_ptr, ix+1, iy);
    }

  if (iy > acontext_ptr->sel_y1)
    {
      p_add_point(acontext_ptr, ix, iy-1);
    }

  if (iy < acontext_ptr->sel_y2 -1)
    {
      p_add_point(acontext_ptr, ix, iy+1);
    }



  if (acontext_ptr->isleval_ptr->connect_by_corner != TRUE)
    {
       return;
    }

  /* check diagonal neighbours */
  if ((ix > acontext_ptr->sel_x1) && (iy > acontext_ptr->sel_y1))
    {
      p_add_point(acontext_ptr, ix-1, iy-1);
    }

  if ((ix < acontext_ptr->sel_x2 -1) && (iy < acontext_ptr->sel_y2 -1))
    {
      p_add_point(acontext_ptr, ix+1, iy+1);
    }

  if ((iy > acontext_ptr->sel_y1) && (ix < acontext_ptr->sel_x2 -1))
    {
      p_add_point(acontext_ptr, ix+1, iy-1);
    }

  if ((iy < acontext_ptr->sel_y2 -1) && (ix > acontext_ptr->sel_x1))
    {
      p_add_point(acontext_ptr, ix-1, iy+1);
    }
  
}  /* end p_check_and_mark_selected_pixel */



/* --------------------------------
 * p_mark_pixel
 * --------------------------------
 * mark the pixel at position ix/iy
 * with the current marker byte of the area context.
 */
static void
p_mark_pixel(AreaContext_t *acontext_ptr, gint32 ix, gint32 iy)
{
  guchar          pixel[4];
  
  pixel[0] = acontext_ptr->marker;
  gimp_pixel_fetcher_put_pixel(acontext_ptr->pft, ix, iy, &pixel[0]);
  
  if (gs_debug)
    {
      printf("  mark x:%4d y:%4d [%d]\n"
       ,ix
       ,iy
       ,pixel[0]
       );                                           
    }
}  /* end p_mark_pixel */


/* --------------------------------
 * p_reset_area_context
 * --------------------------------
 */
static void
p_reset_area_context(AreaContext_t *acontext_ptr, gint32 ix, gint32 iy)
{
  acontext_ptr->seed_ix = ix;
  acontext_ptr->seed_iy = iy;
  acontext_ptr->max_ix = ix;
  acontext_ptr->min_ix = ix;
  acontext_ptr->max_iy = iy;
  acontext_ptr->min_iy = iy;
  acontext_ptr->pixel_count = 0;
  acontext_ptr->marker = SEL_VISITED;
  
}  /* end p_reset_area_context */


/* -----------------------------------
 * p_set_all_selected_pixels_unchecked
 * -----------------------------------
 * set all selected pixles in the isle_drawable to value SEL_UNCHECKED
 * all other pixels to SEL_NONE.
 * (the isle_drawable is a working copy of the selection channel)
 */
static void
p_set_all_selected_pixels_unchecked(AreaContext_t *acontext_ptr)
{
  GimpPixelRgn      islePR;
  gpointer  pr;

  p_init_islePr(acontext_ptr, &islePR);

  for (pr = gimp_pixel_rgns_register (1, &islePR);
       pr != NULL;
       pr = gimp_pixel_rgns_process (pr))
    {
        guint    row;
        guint    col;
        guchar  *dest;
        
        p_handle_progress_step(acontext_ptr, "p_set_all_selected_pixels_unchecked");

        dest = islePR.data;
        for (row = 0; row < islePR.h; row++)
          {
            guchar* dest_ptr = dest;

            for(col = 0; col < islePR.w; col++)
              {
                if((*dest_ptr != SEL_NONE)
                && (*dest_ptr >= acontext_ptr->isleval_ptr->threshold))
                  {
                    *dest_ptr = SEL_UNCHECKED;
                  }
                else
                  {
                    *dest_ptr = SEL_NONE;
                  }
                dest_ptr += islePR.bpp;
              }

            dest += islePR.rowstride;
          }
    }

  
}  /* end p_set_all_selected_pixels_unchecked */


/* ------------------------------------
 * p_clear_all_not_full_selected_pixels
 * ------------------------------------
 */
static void
p_clear_all_not_full_selected_pixels(AreaContext_t *acontext_ptr)
{
  GimpPixelRgn      islePR;
  gpointer  pr;

  p_init_islePr(acontext_ptr, &islePR);

  for (pr = gimp_pixel_rgns_register (1, &islePR);
       pr != NULL;
       pr = gimp_pixel_rgns_process (pr))
    {
        guint    row;
        guint    col;
        guchar  *dest;
          
        p_handle_progress_step(acontext_ptr, "p_clear_all_not_full_selected_pixels");

        dest = islePR.data;
        for (row = 0; row < islePR.h; row++)
          {
            guchar* dest_ptr = dest;

            for(col = 0; col < islePR.w; col++)
              {
                if (*dest_ptr != SEL_FULL)
                  {
                    *dest_ptr = SEL_NONE;
                  }
                dest_ptr += islePR.bpp;
              }

            dest += islePR.rowstride;
          }
    }

  
}  /* end p_clear_all_not_full_selected_pixels */

/* --------------------------------
 * p_clear_selection_below_threshold
 * --------------------------------
 */
static void
p_clear_selection_below_threshold(AreaContext_t *acontext_ptr, GimpDrawable *selection_drawable)
{
  GimpPixelRgn      selectionPR;
  gpointer  pr;

  gimp_pixel_rgn_init (&selectionPR, selection_drawable
                      , acontext_ptr->sel_x1
                      , acontext_ptr->sel_y1
                      , acontext_ptr->sel_width
                      , acontext_ptr->sel_height
		      , TRUE      /* dirty */
		      , FALSE     /* shadow */
		       );

  for (pr = gimp_pixel_rgns_register (1, &selectionPR);
       pr != NULL;
       pr = gimp_pixel_rgns_process (pr))
    {
        guint    row;
        guint    col;
        guchar  *dest;
          
        p_handle_progress_step(acontext_ptr, "p_clear_selection_below_threshold");
          
        dest = selectionPR.data;
        for (row = 0; row < selectionPR.h; row++)
          {
            guchar* dest_ptr = dest;

            for(col = 0; col < selectionPR.w; col++)
              {
                if (*dest_ptr <  acontext_ptr->isleval_ptr->threshold)
                  {
                    *dest_ptr = SEL_NONE;
                  }
                dest_ptr += selectionPR.bpp;
              }

            dest += selectionPR.rowstride;
          }
    }

}  /* end p_clear_selection_below_threshold */


/* --------------------------------
 * p_compute_diagonal
 * --------------------------------
 */
static gdouble
p_compute_diagonal(AreaContext_t *acontext_ptr)
{
  gdouble dx;
  gdouble dy;
  gdouble diagonal;
  
  dx = acontext_ptr->max_ix - acontext_ptr->min_ix;
  dy = acontext_ptr->max_iy - acontext_ptr->min_iy;
  
  diagonal = sqrt((dx * dx) + (dy * dy));
  return (diagonal);
  
}  /* end p_compute_diagonal */


static void
p_debug_print_vals(AreaContext_t *acontext_ptr)
{
  printf("\nVALS:\n");
  printf("  pixel_diagonal:%.4f\n", (float)acontext_ptr->isleval_ptr->pixel_diagonal);
  printf("  pixel_limit:%d\n", (int)acontext_ptr->isleval_ptr->pixel_limit);
  printf("  threshold:%d\n", (int)acontext_ptr->isleval_ptr->threshold);
  printf("  connect_by_corner:%d\n", (int)acontext_ptr->isleval_ptr->connect_by_corner);
  printf("  affect_selected:%d\n", (int)acontext_ptr->isleval_ptr->affect_selected);
  printf("  affect_unselected:%d\n", (int)acontext_ptr->isleval_ptr->affect_unselected);
  printf("  clear_below_threshold:%d\n", (int)acontext_ptr->isleval_ptr->clear_below_threshold);
  printf("  keep_workchannels:%d\n", (int)acontext_ptr->isleval_ptr->keep_workchannels);
  printf("\nSELCTION:\n");
  printf("  sel_x1:%d\n", (int)acontext_ptr->sel_x1);
  printf("  sel_y1:%d\n", (int)acontext_ptr->sel_y1);
  printf("  sel_x2:%d\n", (int)acontext_ptr->sel_x2);
  printf("  sel_y2:%d\n", (int)acontext_ptr->sel_y2);
  printf("  sel_width:%d\n", (int)acontext_ptr->sel_width);
  printf("  sel_height:%d\n", (int)acontext_ptr->sel_height);
}

/* -----------------------------------------------------------  */
/* ------- D I A L O G --- GUI Procedures --------------------  */
/* -----------------------------------------------------------  */

/* --------------------------------
 * p_set_default_values
 * --------------------------------
 * reset all values to default (by entering default values in
 * the correspnding widgets via program control)
 */
static void
p_set_default_values(dialog_widgets_t *gui_ptr)
{
  isle_vals_t default_isle_vals;
  isle_vals_t *isleval_ptr;
  
  if(gui_ptr==NULL)
    {
      return;
    }

  isleval_ptr = &default_isle_vals;
  p_init_default_values(isleval_ptr);


  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gui_ptr->check_affect_selected)
                              , isleval_ptr->affect_selected);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gui_ptr->check_affect_unselected)
                              , isleval_ptr->affect_unselected);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gui_ptr->check_clear_below_threshold)
                              , isleval_ptr->clear_below_threshold);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gui_ptr->check_connect_by_corner)
                              , isleval_ptr->connect_by_corner);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gui_ptr->check_keep_workchannels)
                              , isleval_ptr->keep_workchannels);

  gtk_adjustment_set_value (GTK_ADJUSTMENT (gui_ptr->adj_pixel_diagonal)
                           , (gfloat)isleval_ptr->pixel_diagonal);
  gtk_adjustment_set_value (GTK_ADJUSTMENT (gui_ptr->adj_pixel_limit)
                           , (gfloat)isleval_ptr->pixel_limit);
  gtk_adjustment_set_value (GTK_ADJUSTMENT (gui_ptr->adj_threshold)
                           , (gfloat)isleval_ptr->threshold);
  
}  /* end p_set_default_values */


/* ---------------------------------
 * p_response_callback
 * ---------------------------------
 * handle response.
 */
static void
p_response_callback (GtkWidget *widget
                 ,gint       response_id
                 ,dialog_widgets_t *gui_ptr)
{
  GtkWidget *dialog;
  
  switch (response_id)
    {
    case ISLE_RESPONSE_RESET:
      p_set_default_values(gui_ptr);
      break;

    case GTK_RESPONSE_OK:
      gui_ptr->run_flag = TRUE;
      /* fall through */

    default:
      dialog = gui_ptr->dialog;
      gui_ptr->dialog = NULL;
      if (dialog != NULL)
        {
          gtk_widget_hide (dialog);
          gtk_widget_destroy (dialog);
          gtk_main_quit();
        }
      break;
    }
}  /* end p_response_callback */


/* --------------------------------
 * p_isle_dialog
 * --------------------------------
 * gtk dialog windows with controls to adjust parmeters
 */
static gboolean
p_isle_dialog (GimpDrawable *drawable, isle_vals_t *isleval_ptr)
{
  GtkWidget *main_vbox;
  GtkWidget *table;
  GtkWidget *check;
  GtkObject *adj;
  gint       row;
  gint       col;

  dialog_widgets_t  dialog_widgets;
  dialog_widgets_t *gui_ptr;

  
  gui_ptr = &dialog_widgets;

  gui_ptr->run_flag = FALSE;

  gimp_ui_init (PLUG_IN_BINARY, TRUE);

  gui_ptr->dialog = gimp_dialog_new (_("Remove Selection Isles"), PLUG_IN_BINARY,
                            NULL, 0,
                            gimp_standard_help_func, PLUG_IN_PROC,

			    GIMP_STOCK_RESET, ISLE_RESPONSE_RESET,
                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            GTK_STOCK_OK,     GTK_RESPONSE_OK,
                            NULL);

  gtk_dialog_set_alternative_button_order (GTK_DIALOG (gui_ptr->dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);
  g_signal_connect (G_OBJECT (gui_ptr->dialog), "response",
                    G_CALLBACK (p_response_callback),
                    gui_ptr);


  main_vbox = gtk_vbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (gui_ptr->dialog)->vbox), main_vbox);
  gtk_widget_show (main_vbox);


  /* Controls */
  table = gtk_table_new (9, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 5);
  gtk_table_set_row_spacings (GTK_TABLE (table), 5);
  gtk_box_pack_start (GTK_BOX (main_vbox), table, FALSE, FALSE, 0);
  gtk_widget_show (table);

  /* check button affect_selected */
  row=0;
  check = gtk_check_button_new_with_label (_("Selected"));
  gui_ptr->check_affect_selected = check;
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), isleval_ptr->affect_selected);
  gtk_table_attach (GTK_TABLE (table), check, 1, 3, row, row + 1,
                    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gimp_help_set_help_data (GTK_WIDGET (check),
			   _("Remove small selection isles from selection"), NULL);
  g_signal_connect (check, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &isleval_ptr->affect_selected);
  gtk_widget_show (check);

  /* check button affect_unselected */
  row++;
  check = gtk_check_button_new_with_label (_("Unselected"));
  gui_ptr->check_affect_unselected = check;
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), isleval_ptr->affect_unselected);
  gtk_table_attach (GTK_TABLE (table), check, 1, 3, row, row + 1,
                    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gimp_help_set_help_data (GTK_WIDGET (check),
			   _("Add small unselected isles to selection"), NULL);
  g_signal_connect (check, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &isleval_ptr->affect_unselected);
  gtk_widget_show (check);

  /* check button clear_below_threshold */
  row++;
  check = gtk_check_button_new_with_label (_("Clear Weak"));
  gui_ptr->check_clear_below_threshold = check;
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), isleval_ptr->clear_below_threshold);
  gtk_table_attach (GTK_TABLE (table), check, 1, 3, row, row + 1,
                    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gimp_help_set_help_data (GTK_WIDGET (check),
			   _("Clear weak selected pixels below threshold "
			   "(no matter if part of small isle or not)"), NULL);
  g_signal_connect (check, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &isleval_ptr->clear_below_threshold);
  gtk_widget_show (check);

  /* check button connect_by_corner */
  row++;
  check = gtk_check_button_new_with_label (_("Connect By Corner"));
  gui_ptr->check_connect_by_corner = check;
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), isleval_ptr->connect_by_corner);
  gtk_table_attach (GTK_TABLE (table), check, 1, 3, row, row + 1,
                    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gimp_help_set_help_data (GTK_WIDGET (check),
			   _("ON: selected pixels touching only at corners are"
                             "treated as part of the same area"), NULL);
  g_signal_connect (check, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &isleval_ptr->connect_by_corner);
  gtk_widget_show (check);

  /* check button keep_workchannels */
  row++;
  check = gtk_check_button_new_with_label (_("KeepWorkchannels"));
  gui_ptr->check_keep_workchannels = check;
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), isleval_ptr->keep_workchannels);
  gtk_table_attach (GTK_TABLE (table), check, 1, 3, row, row + 1,
                    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gimp_help_set_help_data (GTK_WIDGET (check),
			   _("Says whether or not keep working channels "), NULL);
  g_signal_connect (check, "toggled",
                    G_CALLBACK (gimp_toggle_button_update),
                    &isleval_ptr->keep_workchannels);
//  if (gs_debug)
    {
      gtk_widget_show (check);
    }

  /* the pixel_diagonal */
  row = 0;
  col = 3;
  adj = gimp_scale_entry_new (GTK_TABLE (table), col, row,
                  _("Area Diagonal"), SCALE_WIDTH, SPIN_BUTTON_WIDTH,
                  isleval_ptr->pixel_diagonal,
                  1.0, 1000.0, 1.0, 10.0, 2,
                  TRUE,
                  0, 0,
                  _("area diagonal in pixels. "
                    "selection areas with greater diagonal are NOT considered as small isles"),
                  NULL);
  gui_ptr->adj_pixel_diagonal = adj;
  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_double_adjustment_update),
                    &isleval_ptr->pixel_diagonal);


  /* the  pixel_limit */
  row++;
  adj = gimp_scale_entry_new(GTK_TABLE(table), col, row, 
			     _("Pixel Limit"), 
			     SCALE_WIDTH, SPIN_BUTTON_WIDTH, 
			     isleval_ptr->pixel_limit,
			     1, 20000, 1,10,0, 
			     TRUE,
			     0, 0,
			     ("selection areas having more pixels are NOT considered as small isles"),
			     NULL);
  gui_ptr->adj_pixel_limit = adj;
  g_signal_connect (adj, "value_changed",
		     G_CALLBACK (gimp_int_adjustment_update),
		     &isleval_ptr->pixel_limit);


  /* the selection threshold */
  row++;
  adj = gimp_scale_entry_new(GTK_TABLE(table), col, row, 
			     _("Selection Threshold"), 
			     SCALE_WIDTH, SPIN_BUTTON_WIDTH, 
			     isleval_ptr->threshold,
			     0, 255, 1,10,0, 
			     TRUE,
			     0, 0,
			     _("selection threshold (weak selected pixels below threshold are "
			       "treated as unselected pixels at isle detection)"),
			     NULL);
  gui_ptr->adj_threshold = adj;
  g_signal_connect (adj, "value_changed",
		     G_CALLBACK (gimp_int_adjustment_update),
		     &isleval_ptr->threshold);



  /* Done */

  gtk_widget_show (gui_ptr->dialog);

  gtk_main ();
  gdk_flush ();


  return gui_ptr->run_flag;
}  /* end p_isle_dialog */

