/* edit gimp comment
 *
 * This is a plug-in for the GIMP 2.0
 *
 * Copyright (C) Reinhard Geisler
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License (version 2) as 
 * published by the Free Software Foundation.
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

/* This plug-in changes the gimp comment.
 */

/*
 * Installation:
 * gimptool --install ecomment.c
 *
 * Enjoy!
 */


#include "stdio.h"
#include "string.h"
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "ecomment"
#define PLUG_IN_VERSION "Ecomment 0.2"
#define PLUG_IN_DATE    "5.2003-6.2004"


/* type of lextract parameters */
typedef struct
{
  gchar *text;
} tparameter;


static tparameter parameter =
{
  NULL
};

/* status of parameter input: if TRUE: use plug-in; if FALSE: cancel */
gint parameter_ok=FALSE;



/********************************************************************/
/* Prototypes                                                       */
/********************************************************************/

/* communication to the GIMP */
static void query(void);

static void run(const gchar      *name,
                gint              nparams,
                const GimpParam  *param,
                gint             *nreturn_vals,
                GimpParam       **return_vals);

/* user interface */
static gint get_parameters(void);


/********************************************************************/
/* Variables                                                        */
/********************************************************************/

/* PLUG_IN_INFO *****************************************************/
GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,   /* init_proc  */
  NULL,   /* quit_proc  */
  query,  /* query_proc */
  run     /* run_proc   */
}; 

/********************************************************************/
/* procedures                                                       */
/********************************************************************/
MAIN()

/* communication to the GIMP */
static void query(void)
{
  static GimpParamDef params[] =
  {
    { GIMP_PDB_INT32,    "run_mode",    "Interactive,non-interactive"},
    { GIMP_PDB_IMAGE,    "image_id",    "Input image" },
    { GIMP_PDB_DRAWABLE, "drawable_id", "Input drawable" },
    { GIMP_PDB_STRING,   "text",        "new gimp comment"}
  };

  static GimpParamDef *return_vals  = NULL;
  static int           nparams      = sizeof(params)/sizeof(params[0]);
  static int           nreturn_vals = 0;
  

  gimp_install_procedure("plug_in_"PLUG_IN_NAME,
                         "edits the gimp comment",
                         "This plug-in edits the gimp-comment parasite.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Image/Edit Comment",
                         "RGB*,GRAY*,INDEXED*",
                         GIMP_PLUGIN,
                         nparams,
                         nreturn_vals,
                         params,
                         return_vals);
}


/********************************************************************/
static void run(const gchar      *name,
                gint              nparams,
                const GimpParam  *param,
                gint             *nreturn_vals,
                GimpParam       **return_vals)
{
  static GimpParam  values[1];
  GimpPDBStatusType status;
  GimpRunMode       run_mode;
  GimpParasite     *parasite;

  status   = GIMP_PDB_SUCCESS;
  run_mode = param[0].data.d_int32;
  *nreturn_vals = 1;
  *return_vals  = values;
  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  switch(run_mode)
  {
  case GIMP_RUN_INTERACTIVE:
  case GIMP_RUN_WITH_LAST_VALS:
    parasite=gimp_image_parasite_find(param[1].data.d_int32,"gimp-comment");
    if(parasite!=NULL)
    {
      parameter.text=g_strdup(parasite->data);
      gimp_parasite_free(parasite);
    }
    if(!get_parameters()) return;
    break;
  case GIMP_RUN_NONINTERACTIVE:
    if (nparams!=4)
      status=GIMP_PDB_CALLING_ERROR;
    else
    {
      parameter.text    =param[3].data.d_string;
    }
    break;
  default:
    status=GIMP_PDB_CALLING_ERROR;
    break;
  }
  values[0].data.d_status = status;
  if(status!=GIMP_PDB_SUCCESS) return;
  if(parameter.text!=NULL)
  {
    gimp_image_parasite_detach(param[1].data.d_int32,"gimp-comment");
    gimp_image_attach_new_parasite(param[1].data.d_int32,
      "gimp-comment",0,strlen(parameter.text)+1,parameter.text);
  }
}



/***************************************************************************/
/* user interface                                                          */
/***************************************************************************/

/* exit dialog */
static void dialog_callback(GtkWidget *widget,
                            gint response_id, GtkTextBuffer *textbuffer)
{
  GtkTextIter  start;
  GtkTextIter  stop;

  if(response_id==GTK_RESPONSE_OK)
  {
    /* get image comment */
    if(parameter.text!=NULL) g_free(parameter.text);
    gtk_text_buffer_get_bounds(textbuffer,&start,&stop);
    parameter.text=gtk_text_buffer_get_text(textbuffer,&start,&stop,FALSE);
    parameter_ok=TRUE;
  }
  gtk_widget_destroy(widget);
}


/****************************************************************************/
/* edit comment                                                             */
/****************************************************************************/
static gint get_parameters(void)
{
  GtkWidget *dlg;
  GtkWidget *frame;
  GtkWidget *window;
  GtkWidget *view;
  GtkTextBuffer *textbuffer;
  
  gchar **argv;
  gint argc;
  
  argc   =1;
  argv   =g_new(gchar*,1);
  argv[0]=g_strdup(PLUG_IN_NAME);
  
  gtk_init(&argc,&argv);
  gtk_rc_parse(gimp_gtkrc());

  /* open new dialog */
  dlg=gimp_dialog_new(PLUG_IN_VERSION,PLUG_IN_NAME,
                       NULL,0,
                       gimp_standard_help_func,"plug-in-"PLUG_IN_NAME,
                       GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,
                       GTK_STOCK_OK,    GTK_RESPONSE_OK,
                       NULL);
  g_signal_connect(dlg,"destroy", G_CALLBACK(gtk_main_quit),  NULL);

  /* image comment */
  frame=gtk_frame_new("Image Comment (Stream Name)");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  gtk_widget_show(frame);

  window=gtk_scrolled_window_new(NULL,NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_container_set_border_width(GTK_CONTAINER(window),4);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(window),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_container_add(GTK_CONTAINER(frame),window);

  textbuffer=gtk_text_buffer_new(NULL);
  view=gtk_text_view_new_with_buffer(textbuffer);
  gtk_container_add(GTK_CONTAINER(window),view);
  if(parameter.text!=NULL)
    gtk_text_buffer_set_text(textbuffer,parameter.text,-1);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(view),GTK_WRAP_WORD);
  g_object_unref(textbuffer);

  g_signal_connect(dlg,"response",G_CALLBACK(dialog_callback),textbuffer);

  gtk_widget_show(view);
  gtk_widget_show(window);

  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}
