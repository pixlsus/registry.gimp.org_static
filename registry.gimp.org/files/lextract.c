/* extract layers
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

/* This plug-in creates a new image/film by extracting layers from another.
 */

/*
 * Installation:
 * gimptool --install lextract.c
 *
 * Enjoy!
 */


#include "stdio.h"
#include "string.h"
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "lextract"
#define PLUG_IN_VERSION "Lextract 0.4"
#define PLUG_IN_DATE    "5.2003-6.2004"


/* type of lextract parameters */
typedef struct
{
  gint32 first;
  gint32 interval;
  gint32 period;
  gint32 number;
} tparameter;


static tparameter parameter =
{
  0,
  1,
  10,
  0
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

/* extract layers... */
static gint32 lextract(gint32 src_image_id);

/* user interface */
static gint get_parameters(gint32 image_id);


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

/* sliders */
static GtkAdjustment *first_slider;
static GtkAdjustment *interval_slider;
static GtkAdjustment *period_slider;
static GtkAdjustment *number_slider;


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
    { GIMP_PDB_INT32,    "first",       "number of first layer to extract"},
    { GIMP_PDB_INT32,    "interval",    "interval length of layers to extract"},
    { GIMP_PDB_INT32,    "period",      "length of period"},
    { GIMP_PDB_INT32,    "number",      "number of periods"}};

  static GimpParamDef *return_vals  = NULL;
  static int           nparams      = sizeof(params)/sizeof(params[0]);
  static int           nreturn_vals = 0;
  

  gimp_install_procedure("plug_in_"PLUG_IN_NAME,
                         "extracts layers of an image/film",
                         "This plug-in creates a new image/film by extracting "
			 "layers from another. It takes a selectable number "
			 "of sequent layers every period of selectable "
			 "length.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Filters/Animation/Extract Layers",
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
  gint32 new_image_id=0;
  static GimpParam  values[1];
  GimpPDBStatusType status;
  GimpRunMode       run_mode;
  GimpParasite *parasite;
  gint          parasite_num;
  gchar       **parasite_name;

  status   = GIMP_PDB_SUCCESS;
  run_mode = param[0].data.d_int32;
  *nreturn_vals = 1;
  *return_vals  = values;
  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  switch(run_mode)
  {
  case GIMP_RUN_INTERACTIVE:
    gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
    if(!get_parameters(param[1].data.d_image)) return;
    break;
  case GIMP_RUN_NONINTERACTIVE:
    if (nparams!=7)
      status=GIMP_PDB_CALLING_ERROR;
    else
    {
      parameter.first   =param[3].data.d_int32;
      parameter.interval=param[4].data.d_int32;
      parameter.period  =param[5].data.d_int32;
      parameter.number  =param[6].data.d_int32;
    }
    break;
  case GIMP_RUN_WITH_LAST_VALS:
    gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
    break;
  default:
    status=GIMP_PDB_CALLING_ERROR;
    break;
  }
  values[0].data.d_status = status;
  if(status!=GIMP_PDB_SUCCESS) return;
  gimp_set_data("plug_in_"PLUG_IN_NAME,&parameter,sizeof(tparameter));

  new_image_id=lextract(param[1].data.d_image);
  if(new_image_id<0)
  {
    values[0].data.d_status=GIMP_PDB_EXECUTION_ERROR;
    return;
  }

  gimp_display_new(new_image_id);

  /* copy parasites */
  if(gimp_image_parasite_list(param[1].data.d_image,
			   &parasite_num,&parasite_name))
    for(;parasite_num>0;parasite_num--)
    {
      parasite=gimp_image_parasite_find(param[1].data.d_image,
					parasite_name[parasite_num-1]);
      if(parasite!=NULL)
	gimp_image_attach_new_parasite(new_image_id,
				       parasite_name[parasite_num-1],0,
				       gimp_parasite_data_size(parasite),
				       gimp_parasite_data(parasite));
      gimp_parasite_free(parasite);
    }
}


/********************************************************************/
/* extract layers                                                   */
/********************************************************************/
static gint32 lextract(gint32 src_image_id)
{
  GimpDrawable *src_drawable, *new_drawable;
  GimpPixelRgn  src_region,    new_region;
  gint32 new_image_id;
  gint32 new_layer_id;
  gint32 *layers, layers_number;

  gint32 width, height;
  gint32 interval, period, number, layer;
  gint   offset_x, offset_y;
  guchar *buffer;

  /* create new image */
  new_image_id=gimp_image_new(gimp_image_width    (src_image_id),
			      gimp_image_height   (src_image_id),
			      gimp_image_base_type(src_image_id));

  /*  copy the colormap, if necessary  */
  if(gimp_image_base_type(src_image_id)==GIMP_INDEXED)
  {
    int ncols;
    guchar *cmap;
    
    cmap=gimp_image_get_cmap(src_image_id, &ncols);
    gimp_image_set_cmap(new_image_id, cmap, ncols);
    g_free(cmap);
  }

  /* get layers & number of layers */
  layers=gimp_image_get_layers(src_image_id,&layers_number);

  /* loop through layers */
  interval=parameter.interval;
  period =parameter.period;
  number =parameter.number?parameter.number:layers_number;
  gimp_progress_init("extracting layers...");
  for(layer=layers_number-1-parameter.first; layer>=0; layer--)
  {
    /* show progress */
    gimp_progress_update((gdouble)(layers_number-layer)/layers_number);

    /* check if layer has to be extracted */
    if(period==0)
    {
      interval =parameter.interval;
      period=parameter.period;
      if(--number==0) break;
    }
    period--;
    if(interval==0) continue;
    interval--;

    /* get src layer, buffer */
    src_drawable=gimp_drawable_get(layers[layer]);
    width =src_drawable->width;
    height=src_drawable->height;
    gimp_pixel_rgn_init(&src_region,src_drawable,0,0,width,height,FALSE,FALSE);
    if((buffer=g_new(guchar,width*height*src_drawable->bpp))==NULL) return -1;
    
    /* create new layer */
    new_layer_id=gimp_layer_new(new_image_id,
				gimp_drawable_get_name(layers[layer]),
                                width, height,
                                gimp_drawable_type(layers[layer]),
                                100,
                                GIMP_NORMAL_MODE);
    gimp_image_add_layer(new_image_id, new_layer_id,0);
    new_drawable=gimp_drawable_get(new_layer_id);
    if(gimp_drawable_offsets(layers[layer],&offset_x,&offset_y))
      gimp_layer_set_offsets(new_layer_id,offset_x,offset_y);
    gimp_pixel_rgn_init(&new_region,new_drawable,0,0,width,height,TRUE,TRUE);

    /* copy layer */
    gimp_pixel_rgn_get_rect(&src_region,buffer,0,0,width,height);
    gimp_pixel_rgn_set_rect(&new_region,buffer,0,0,width,height);

    /* Tidy up dirty drawable */
    gimp_drawable_flush(new_drawable);
    gimp_drawable_merge_shadow(new_drawable->drawable_id, TRUE);
    gimp_drawable_update(new_drawable->drawable_id,0,0,width,height);
    gimp_drawable_detach(new_drawable); 

    /* free buffer */
    g_free(buffer);
  }

  gimp_displays_flush();
  return new_image_id;
}


/********************************************************************/
/* user interface                                                   */
/********************************************************************/

/* exit dialog */
static void dialog_callback(GtkWidget *widget,
                            gint response_id, gpointer data)
{
  if(response_id==GTK_RESPONSE_OK) parameter_ok=TRUE;
  gtk_widget_destroy(widget);
}


/* check the possible number of periods... */
void check_number_slider(void)
{
  gint32 max;

  max=(first_slider->upper-parameter.first+parameter.period)/parameter.period;
  if(max!=number_slider->upper)
  {
    number_slider->upper=max;
    if(parameter.number==0) gtk_adjustment_set_value(number_slider,max);
    gtk_adjustment_changed(number_slider);
  }
}

/* first slider update */
void first_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  parameter.first=first_slider->value;
  check_number_slider();
}

/* interval slider update */
void interval_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  parameter.interval=interval_slider->value;
  if(parameter.interval>period_slider->value)
  {
    gtk_adjustment_set_value(period_slider,parameter.interval);
    gtk_adjustment_changed(period_slider);
  }
}

/* period slider update */
void period_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  parameter.period=period_slider->value;
  if(parameter.period<interval_slider->value)
  {
    gtk_adjustment_set_value(interval_slider,parameter.period);
    gtk_adjustment_changed(interval_slider);
  }
  check_number_slider();
}

void number_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  parameter.number=number_slider->value<number_slider->upper?
    number_slider->value:0;
}


/*  main dialog *****************************************************/
static gint get_parameters(gint32 image_id)
{
  GtkWidget *dlg;
  GtkWidget *table;
  GtkWidget *frame;
  gint32    layers_number;

  gchar **argv;
  gint    argc;

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
  g_signal_connect(dlg,"response",G_CALLBACK(dialog_callback),NULL);
  g_signal_connect(dlg,"destroy", G_CALLBACK(gtk_main_quit),  NULL);

  /*  parameter settings  */
  frame=gtk_frame_new("Extract an Interval of Layers per Period");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start(GTK_BOX (GTK_DIALOG(dlg)->vbox),frame,TRUE,TRUE,0);

  table=gtk_table_new(4,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_container_add(GTK_CONTAINER(frame),table);

  /* parameter sliders */
  gimp_image_get_layers(image_id,&layers_number);
  if(parameter.first>layers_number-1) parameter.first=layers_number-1;
  first_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,0,"First Layer:",150,0,
                         parameter.first,0,layers_number-1,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  if(parameter.interval>layers_number) parameter.interval=layers_number;
  interval_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,1,"Interval Length:",150,0,
                         parameter.interval,1,layers_number,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  if(parameter.period>layers_number) parameter.period=layers_number;
  period_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,2,"Period Length:",150,0,
                         parameter.period,1,layers_number,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  if(parameter.number>layers_number) parameter.number=layers_number;
  number_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,3,"Number of Periods:",150,0,
                         parameter.number,1,layers_number,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  check_number_slider();

  gtk_signal_connect(GTK_OBJECT(first_slider),"value_changed",
                     GTK_SIGNAL_FUNC(first_slider_update),NULL);
  gtk_signal_connect(GTK_OBJECT(interval_slider),"value_changed",
                     GTK_SIGNAL_FUNC(interval_slider_update),NULL);
  gtk_signal_connect(GTK_OBJECT(period_slider),"value_changed",
                     GTK_SIGNAL_FUNC(period_slider_update),NULL);
  gtk_signal_connect(GTK_OBJECT(number_slider),"value_changed",
                     GTK_SIGNAL_FUNC(number_slider_update),NULL);
 
  gtk_widget_show (table);
  gtk_widget_show (frame);
  gtk_widget_show (dlg);

  gtk_main();

  return parameter_ok;
}
