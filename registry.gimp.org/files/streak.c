/* Streak
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

/* This plug-in simulates a streak camera.
 *
 * A streak camera images an object through a slit - thus getting a 
 * "one dimensional image". This image is propagated along the
 * second dimension of the image plane at a constant speed.
 * The result is a picture of the time dependency of the object.
 *
 * The plug-in takes a film (multilayer image), cuts a slice of
 * selectable width and position out of each layer and puts the
 * slices together to the streak image.
 */

/*
 * Installation:
 * gimptool --install streak.c
 *
 * Enjoy!
 */


#include <stdlib.h>
#include <stdio.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "streak"
#define PLUG_IN_VERSION "Streak 0.6"
#define PLUG_IN_DATE    "5.2000-6.2004"


#define GUCHAR_MAX 255
#define SCALE_WIDTH 125
#define ENTRY_WIDTH  60


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

/* streak creation */
static gboolean streak(gint32 image_id);

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

/* type of streak parameters */
typedef struct
{
  gint32 slit_position;
  gint32 slit_width;
  gint32 vertical;
} tparameter;

static tparameter parameter =
{
  0,
  1,
  TRUE
};

/* image dimensions */
static guint32 image_width, image_height;

/* input sliders */
static GtkAdjustment *position_slider;
static GtkAdjustment *width_slider;

/* status of parameter input: if TRUE: use plug-in; if FALSE: cancel */
gint parameter_ok=FALSE;


/********************************************************************/
/* procedures                                                       */
/********************************************************************/
MAIN()

/* communication to the GIMP */
static void query(void)
{
  static GimpParamDef params[] =
  {
    { GIMP_PDB_INT32,    "run_mode",      "Interactive, non-interactive"},
    { GIMP_PDB_IMAGE,    "image_id",      "Input image" },
    { GIMP_PDB_DRAWABLE, "drawable_id",   "Input drawable" },
    { GIMP_PDB_INT32,    "slit_position", "Slit position"},
    { GIMP_PDB_INT32,    "slit_width",    "Slit Width"},
    { GIMP_PDB_INT32,    "vertical",      "vertical slit? (0, 1)"}
  };

  static GimpParamDef *return_vals  = NULL;
  static int           nparams      = sizeof(params)/sizeof(params[0]);
  static int           nreturn_vals = 0;
  

  gimp_install_procedure("plug_in_"PLUG_IN_NAME,
                         "Streak image creation",
                         "This plug-in creates a streak-image from a film. "
			 "Width and position of the slit are selectable. "
			 "Position default is the center of the image.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Filters/Animation/Streak",
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
  static GimpParam values[1];
  GimpPDBStatusType status;
  GimpRunMode      run_mode;
  GimpDrawable     *drawable;
 
  status   = GIMP_PDB_SUCCESS;
  run_mode = param[0].data.d_int32;
  *nreturn_vals = 1;
  *return_vals  = values;
  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  drawable=gimp_drawable_get(param[2].data.d_drawable);
  image_width =drawable->width;
  image_height=drawable->height;
  switch(run_mode)
    {
    case GIMP_RUN_INTERACTIVE:
      gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
      if(!get_parameters())
	return;
      gimp_set_data("plug_in_"PLUG_IN_NAME,&parameter,sizeof(tparameter));
      break;
    case GIMP_RUN_NONINTERACTIVE:
      if (nparams!=5) status=GIMP_PDB_CALLING_ERROR;
      else
      {
	parameter.slit_position=param[3].data.d_int32;
	parameter.slit_width   =param[4].data.d_int32;
	parameter.vertical     =param[5].data.d_int32;
      }
      break;
    case GIMP_RUN_WITH_LAST_VALS:
      gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
      break;
    default:
      status=GIMP_PDB_CALLING_ERROR;
      break;
    }
  if(status==GIMP_PDB_SUCCESS) 
    if(!streak(param[1].data.d_image)) status=GIMP_PDB_EXECUTION_ERROR;

  values[0].data.d_status = status;
}


/********************************************************************/
/* streak procedure                                                 */
/********************************************************************/
static gboolean streak(gint32 image_id)
{
  gint32     new_image_id=0;
  gint32     new_layer_id=0;

  GimpImageBaseType image_type=GIMP_RGB;
  GimpImageType     drawable_type=GIMP_RGB_IMAGE;
  GimpDrawable     *drawable, *new_drawable;
  GimpPixelRgn      region,    new_region;

  int colors;
  guchar *colormap;

  gint32 layers_number, number;
  gint32* layers;

  gint slit_position, slit_width, width, height, new_position;

  guchar *buffer, *source, *destination;
  guint bpp_diff, bpp_num;

  GimpParasite *parasite;
  gint          parasite_num;
  gchar       **parasite_name;


  /* get layers & number of layers */
  layers=gimp_image_get_layers (image_id, &layers_number);

  /* check & calculate slit parameters */
  slit_position=parameter.slit_position-parameter.slit_width/2;
  slit_width=parameter.slit_width;
  if(slit_position<0) slit_position=0;
  width=parameter.vertical?image_width:image_height;
  if(slit_position+slit_width>width)
    slit_position=width-slit_width;
  
  /* determine size of new image */
  width =parameter.vertical?slit_width*layers_number:image_width;
  height=parameter.vertical?image_height:slit_width*layers_number;

  /* determine type of new imange */
  switch(gimp_image_base_type(image_id))
    {
    case GIMP_RGB:
      image_type=GIMP_RGB;
      drawable_type=GIMP_RGB_IMAGE;
      break;
    case GIMP_GRAY:
      image_type=GIMP_GRAY;
      drawable_type=GIMP_GRAY_IMAGE;
      break;
    case GIMP_INDEXED:
      image_type=GIMP_INDEXED;
      drawable_type=GIMP_INDEXED_IMAGE;
      break;
    }
  
  /* create new image with one layer */
  new_image_id=gimp_image_new(width,height,image_type);
  new_layer_id=gimp_layer_new(new_image_id,
			      "Background",
			      width, height,
			      drawable_type,
			      100,
			      GIMP_NORMAL_MODE);
  gimp_image_add_layer(new_image_id,new_layer_id,0);
  new_drawable=gimp_drawable_get(new_layer_id);


  /*  copy the colormap, if necessary  */
  if(image_type==GIMP_INDEXED)
  {
    colormap=gimp_image_get_cmap(image_id,&colors);
    gimp_image_set_cmap(new_image_id,colormap,colors);
    g_free (colormap);
  }

  /* calculate streak... */
  
  /* get copy buffer */
  if((buffer=malloc((parameter.vertical?height:width)*slit_width*
		    4*sizeof(*buffer)))==NULL)
    return FALSE;

  /* initialize destination region: complete image */
  gimp_pixel_rgn_init(&new_region,new_drawable,
		      0,0,width,height,
		      TRUE,TRUE);

  /* loop through layers */
  gimp_progress_init("creating streak image...");
  new_position=0;
  for(number=layers_number-1; number>=0; number--)
  {
    /* show progress */
    gimp_progress_update((gdouble)(layers_number-number)/layers_number);

    /* select layer */
    drawable=gimp_drawable_get(layers[number]);

    /* initialize source region, get slit */
    if(parameter.vertical)
    {
      gimp_pixel_rgn_init(&region,drawable,
			  slit_position,0,slit_width,height,
			  FALSE,FALSE);
      gimp_pixel_rgn_get_rect(&region,buffer,
			      slit_position,0,slit_width,height);
    }    
    else
    {
      gimp_pixel_rgn_init(&region,drawable,
			  0,slit_position,width,slit_width,
			  FALSE,FALSE);
      gimp_pixel_rgn_get_rect(&region,buffer,
			      0,slit_position,width,slit_width);
    }

    /* copy a layer with alpha in an image without alpha */
    bpp_diff=drawable->bpp-new_drawable->bpp;
    if(bpp_diff>0)
    {
      source=buffer;
      for(destination=buffer;
	  destination<buffer+new_drawable->bpp*slit_width*
	    (parameter.vertical?height:width);)
      {
	for(bpp_num=1; bpp_num<=new_drawable->bpp; bpp_num++)
	  *destination++=*source++;
	source+=bpp_diff;
      }
    }
    
    /* copy slit into new image */
    if(parameter.vertical)
      gimp_pixel_rgn_set_rect(&new_region,buffer,
			      new_position,0,slit_width,height);
    else
      gimp_pixel_rgn_set_rect(&new_region,buffer,
			      0,new_position,width,slit_width);

    new_position+=slit_width;
  }

  /* copy parasites */
  if(gimp_image_parasite_list(image_id,&parasite_num,&parasite_name))
    for(;parasite_num>0;parasite_num--)
    {
      parasite=gimp_image_parasite_find(image_id,parasite_name[parasite_num-1]);
      if(parasite!=NULL)
	gimp_image_attach_new_parasite(new_image_id,
				       parasite_name[parasite_num-1],0,
				       gimp_parasite_data_size(parasite),
				       gimp_parasite_data(parasite));
      gimp_parasite_free(parasite);
    }

  /* Tidy up dirty drawable */
  gimp_drawable_flush(new_drawable);
  gimp_drawable_merge_shadow(new_drawable->drawable_id, TRUE);
  gimp_drawable_update(new_drawable->drawable_id,0,0,width,height);
  gimp_drawable_detach(new_drawable);
  gimp_displays_flush();
  gimp_display_new(new_image_id);

  return TRUE;
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


/* updates */
void position_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  gint dist;

  dist=(parameter.vertical?image_width:image_height)-
       position_slider->value-1;
  if(dist>position_slider->value) dist=position_slider->value;
  parameter.slit_position=position_slider->value;

  width_slider->upper=2*dist+1;
  if(width_slider->value>width_slider->upper)
    gtk_adjustment_set_value(width_slider,width_slider->upper);
  gtk_adjustment_changed(width_slider);
}


void width_slider_update(GtkAdjustment *adjustment, gpointer data)
{
  parameter.slit_width=width_slider->value;
}


void direction_update(GtkWidget *widget, gint32 *value)
{
  gimp_menu_item_update(widget,value);
  parameter.vertical=*value;
  position_slider->upper=parameter.vertical?image_width-1:image_height-1;
  if(position_slider->value>position_slider->upper)
    gtk_adjustment_set_value(position_slider,position_slider->upper); 
  gtk_adjustment_changed(position_slider);
}


/* create menu */
GtkWidget *create_menu(GtkWidget *table, gint x, gint y,
                       const guchar *title,
                       const guchar *name[],
                       gint32 *result,
                       GtkSignalFunc update_function)
{
  GtkWidget *label;
  GtkWidget *selection;

  if(title!=NULL)
  {
    label=gtk_label_new(title);
    gtk_table_attach(GTK_TABLE(table),label,x,x+1,y,y+1,0,0,0,0);
    gtk_misc_set_alignment(GTK_MISC(label),1.0,0.5);
    gtk_widget_show(label);
    x++;
  }
  selection=
    gimp_option_menu_new2(FALSE,GTK_SIGNAL_FUNC(update_function),
                          result,
                          (gpointer) *result,
                          name[0],(gpointer) 0, NULL,
                          name[1],(gpointer) 1, NULL,
                          name[2],(gpointer) 2, NULL,
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,x,x+1,y,y+1,
                   GTK_FILL|GTK_EXPAND,0,0,0);
  gtk_widget_show(selection);

  return selection;
}


/* main dialog ******************************************************/
static gint get_parameters()
{
  const  guchar *name_direction[]={"horizontal","vertical", NULL};

  GtkWidget *dlg;
  GtkWidget *table;
  GtkWidget *frame;

  gchar **argv;
  gint argc;

 
  argc   =1;
  argv   =g_new (gchar *, 1);
  argv[0]=g_strdup (PLUG_IN_NAME);

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
  frame=gtk_frame_new("Create Streak Image");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  
  table=gtk_table_new(3,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_container_add(GTK_CONTAINER(frame),table);

  /* direction menue */
  create_menu(table,0,0,"Slit Direction:",
	      name_direction,&parameter.vertical,
	      (GtkSignalFunc)direction_update);
  gtk_table_set_row_spacing(GTK_TABLE(table),0,15);

  /* slit parameter sliders */
  position_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,1,"Slit Position:",150,0,
			 0,0,parameter.vertical?image_width-1:image_height-1,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  width_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,2,"Slit Width:",150,0,
                         parameter.slit_width,1,parameter.slit_width,
                         1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(position_slider),"value_changed",
                     GTK_SIGNAL_FUNC(position_slider_update),NULL);
  gtk_signal_connect(GTK_OBJECT(width_slider),"value_changed",
                     GTK_SIGNAL_FUNC(width_slider_update),NULL);

  gtk_adjustment_set_value(position_slider,parameter.slit_position);
 
  gtk_widget_show(table);
  gtk_widget_show(frame);
  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}
