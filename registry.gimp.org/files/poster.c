/* Poster
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

/* This plug-in combines image layers by attaching them in
 * rows and columns. This is useful especially for films.
 */

/*
 * Installation:
 * gimptool --install poster.c 
 *
 * Enjoy!
 */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "poster"
#define PLUG_IN_VERSION "Poster 0.6"
#define PLUG_IN_DATE    "7.2000-6.2004"


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

/* poster creation */
static gint32 poster(gint32 image_id,gint32 drawable_id);

/* user interface */
static gint get_parameters(GimpDrawable *drawable, gint32 layers_number);


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

/* poster parameters*/
typedef struct 
{
  gdouble columns;
  gdouble rows;
  gdouble separation;
  gint    row_by_row;
  gint    left_to_right;
  gint    top_to_bottom;
} tparameter;

static tparameter parameter =
{
  1,
  1,
  0,
  1,
  1,
  1,
};


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
    { GIMP_PDB_INT32,    "run_mode",      "Interactive"},
    { GIMP_PDB_IMAGE,    "image_id",      "Input image" },
    { GIMP_PDB_DRAWABLE, "drawable_id",   "Input drawable" }
  };

  static GimpParamDef *return_vals =NULL;
  static int           nparams     =sizeof(params)/sizeof(params[0]);
  static int           nreturn_vals=0;
  

  gimp_install_procedure("plug_in_"PLUG_IN_NAME,
                         "makes a film poster",
                         "This plug-in attaches layers in rows/columns. "
			 "The number of rows/columns and the width of "
			 "separation lines can be selected. Foreground "
			 "color is used for the separation lines, "
			 "background color for redundant fields.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Filters/Animation/Poster",
                         "RGB*,GRAY*",
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
  gint32 layers_number;
  static GimpParam values[1];
  GimpParasite *parasite;
  gint          parasite_num;
  gchar       **parasite_name;

  *nreturn_vals=1;
  *return_vals=values;
  values[0].type=GIMP_PDB_STATUS;
  values[0].data.d_status=GIMP_PDB_SUCCESS;

  gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
  gimp_image_get_layers(param[1].data.d_image, &layers_number);

  if(!get_parameters(gimp_drawable_get(param[2].data.d_drawable),
		     layers_number))
    return;

  new_image_id=poster(param[1].data.d_image,param[2].data.d_drawable);
  gimp_display_new(new_image_id);

  /* copy parasites */
  if(new_image_id>=0 && 
     gimp_image_parasite_list(param[1].data.d_image,
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

  gimp_set_data("plug_in_"PLUG_IN_NAME,&parameter,sizeof(tparameter));
}


/********************************************************************/
/* poster procedure                                                 */
/********************************************************************/
static gint32 poster(gint32 image_id,
		     gint32 drawable_id)
{
  gint32     new_image_id=0;
  gint32     new_layer_id=0;

  GimpImageBaseType image_type=GIMP_RGB;
  GimpImageType     drawable_type=GIMP_RGB_IMAGE;
  GimpDrawable     *drawable, *new_drawable;
  GimpPixelRgn      region,    new_region;
  GimpRGB           rgb;

  gint32 layers_number, number, sep_length;
  gint32* layers;

  gint width, height, poster_width, poster_height;
  gint row, rows, column, columns, separation;

  guchar *sep_buffer, *buffer, *source, *destination;
  guint bpp_diff, bpp_num;
  guchar red, green, blue, gray;

  columns=parameter.columns;
  rows   =parameter.rows;
  separation=parameter.separation;

  /* get layers & number of layers */
  layers=gimp_image_get_layers(image_id,&layers_number);

  drawable=gimp_drawable_get(drawable_id);

  /* determine size of new image */
  width =drawable->width;
  height=drawable->height;
  poster_width =(width +separation)*columns+separation;
  poster_height=(height+separation)*rows+separation;

  /* determine type of new imange */
  switch(gimp_drawable_type(drawable_id))
    {
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      image_type=GIMP_RGB;
      drawable_type=GIMP_RGB_IMAGE;
      break;
    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:
      image_type=GIMP_GRAY;
      drawable_type=GIMP_GRAY_IMAGE;
      break;
    case GIMP_INDEXED_IMAGE:
    case GIMP_INDEXEDA_IMAGE:
      image_type=GIMP_INDEXED;
      drawable_type=GIMP_INDEXED_IMAGE;
      break;      
    }
  
  /* create new image with one layer */
  new_image_id=gimp_image_new(poster_width,poster_height,image_type);
  new_layer_id=gimp_layer_new(new_image_id,
			      "Background",
			      poster_width, poster_height,
			      drawable_type,
			      100,
			      GIMP_NORMAL_MODE);
  gimp_image_add_layer(new_image_id, new_layer_id, 0);
  new_drawable=gimp_drawable_get(new_layer_id);

  /* calculate poster... */
  gimp_progress_init("creating poster...");
    
  /* initialize destination region: complete image */
  gimp_pixel_rgn_init(&new_region,new_drawable,
		      0,0,poster_width,poster_height,
		      TRUE,TRUE);

  /* get copy buffer */
  if((buffer=malloc(width*height*drawable->bpp*sizeof(*buffer)))==NULL)
    exit(EXIT_FAILURE);

  /* prepare separation buffer */
  sep_length=poster_width>poster_height?poster_width:poster_height;
  if((sep_buffer=malloc(sep_length*new_drawable->bpp*sizeof(*buffer)))==NULL)
    exit(EXIT_FAILURE);
  gimp_palette_get_foreground(&rgb);
  gimp_rgb_get_uchar(&rgb,&red,&green,&blue);

  if(new_drawable->bpp==1)
  {
    /* gray value */
    gray=((gint32)299*red+
          (gint32)587*green+
	  (gint32)114*blue)/1000;  
    for(number=0; number<sep_length*new_drawable->bpp;)
      sep_buffer[number++]=gray;
  }
  else
    for(number=0; number<sep_length*new_drawable->bpp;)
    {
      /* R, G, B values */
      sep_buffer[number++]=red;
      sep_buffer[number++]=green;
      sep_buffer[number++]=blue;
    }
  
  /* draw separation lines */  
  for(row=0; row<poster_height;row+=height)
    for(number=1; number<=separation; number++)
    {
      /* show progress */
      gimp_progress_update((gdouble)(row)/poster_height);
      gimp_pixel_rgn_set_rect(&new_region,sep_buffer,
			      0,row++,poster_width,1);
      }
  for(column=0; column<poster_width;column+=width)
    for(number=1; number<=separation; number++)
    {
      /* show progress */
      gimp_progress_update((gdouble)(column)/poster_width);
      gimp_pixel_rgn_set_rect(&new_region,sep_buffer,
			      column++,0,1,poster_height);
    }
  
  /* clear copy buffer */
  gimp_palette_get_background(&rgb);
  gimp_rgb_get_uchar(&rgb,&red,&green,&blue);
  if(new_drawable->bpp==1)
  {
    /* gray value */
    gray=((gint32)299*red+
          (gint32)587*green+
	  (gint32)114*blue)/1000;  
    for(number=0; number<width*height*new_drawable->bpp;)
      buffer[number++]=gray;
  }
  else
    for(number=0; number<width*height*new_drawable->bpp;)
    {
      /* R, G, B values */
      buffer[number++]=red;
      buffer[number++]=green;
      buffer[number++]=blue;
    }

  /* loop through layers (backwards!) */
  column=parameter.left_to_right?columns-1:0;
  row   =parameter.top_to_bottom?rows   -1:0;
  
  for(number=rows*columns; number>0; number--)
  {
    /* show progress */
    gimp_progress_update(1.0-(gdouble)number/rows/columns);

    if(number<=layers_number)
    {
      /* select layer */
      drawable=gimp_drawable_get(layers[layers_number-number]);
    
      /* initialize source region */
      gimp_pixel_rgn_init(&region,drawable,0,0,width,height,FALSE,FALSE);
    
      /* get layer */
      gimp_pixel_rgn_get_rect(&region,buffer,0,0,width,height);
    
      /* copy a layer with alpha into an image without alpha */
      bpp_diff=drawable->bpp-new_drawable->bpp;
      if(bpp_diff>0)
      {
	source=buffer;
	for(destination=buffer;
	    destination<buffer+new_drawable->bpp*width*height;)
	{
	  for(bpp_num=1; bpp_num<=new_drawable->bpp; bpp_num++)
	    *destination++=*source++;
	  source+=bpp_diff;
	}
      }
    }

    /* copy layer into new image */
    gimp_pixel_rgn_set_rect(&new_region,buffer,
			    column*(width +separation)+separation,
			    row   *(height+separation)+separation,
			    width,height);
    if(parameter.row_by_row)
    {
      if(parameter.left_to_right) column--; else column++;
      if(column<0 || column>=columns)
      {
	column=column<0?columns-1:0;
	if(parameter.top_to_bottom) row--; else row++;
      }
    }
    else
    {
      if(parameter.top_to_bottom) row--; else row++;
      if(row<0 || row>=rows)
      {
	row=row<0?rows-1:0;
	if(parameter.left_to_right) column--; else column++;
      }
    }
  }
  
  /* Tidy up dirty drawable */
  gimp_drawable_flush(new_drawable);
  gimp_drawable_merge_shadow(new_drawable->drawable_id,TRUE);
  gimp_drawable_update(new_drawable->drawable_id,0,0,poster_width,poster_height);
  gimp_drawable_detach(new_drawable);
  gimp_displays_flush();

  free(buffer);
  free(sep_buffer);
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


/* menu updates */
void row_by_row_update(GtkWidget *widget, gint32 *value)
{
  gimp_menu_item_update(widget,value);
  parameter.row_by_row=*value;
}

void left_to_right_update(GtkWidget *widget, gint32 *value)
{
  gimp_menu_item_update(widget,value);
  parameter.left_to_right=*value;
}

void top_to_bottom_update(GtkWidget *widget, gint32 *value)
{
  gimp_menu_item_update(widget,value);
  parameter.top_to_bottom=*value;
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
    gtk_table_attach_defaults(GTK_TABLE(table),label,x,x+1,y,y+1);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    gtk_widget_show(label);
    x++;
  }
  selection=
    gimp_option_menu_new2(FALSE,GTK_SIGNAL_FUNC(update_function),
                          result,
                          (gpointer) *result,
                          name[0],(gpointer) 0, NULL,
                          name[1],(gpointer) 1, NULL,
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,x,x+1,y,y+1,
                   GTK_EXPAND|GTK_FILL,0,0,0);
  gtk_widget_show(selection);

  return selection;
}


/********************************************************************/
/* main dialog ******************************************************/
/********************************************************************/
static gint get_parameters(GimpDrawable *drawable, gint32 layers_number)
{
  const guchar *name_order[]     ={"Column by Column","Row by Row",   NULL};    
  const guchar *name_horizontal[]={"Right to Left",   "Left to Right",NULL};   
  const guchar *name_vertical[]  ={"Bottom to Top",   "Top to Bottom",NULL};    
  
  GtkWidget *dlg;
  GtkWidget *label;
  GtkObject *slider;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *ovbox;

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
  g_signal_connect(dlg,"response",G_CALLBACK(dialog_callback),NULL);
  g_signal_connect(dlg,"destroy", G_CALLBACK(gtk_main_quit),  NULL);

  /*  parameter settings  */
  frame=gtk_frame_new("Join Layers to one Poster Image");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame,TRUE,TRUE,0);
  
  ovbox=gtk_vbox_new(FALSE,5);
  gtk_container_set_border_width(GTK_CONTAINER(ovbox),4);
  gtk_container_add(GTK_CONTAINER(frame),ovbox);

  table=gtk_table_new(9,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_box_pack_start(GTK_BOX(ovbox),table,TRUE,TRUE,0);

  /* geometry input sliders */
  label=gtk_label_new("Geometry");
  gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,0,1);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);
  if(parameter.rows>layers_number) parameter.rows=layers_number;
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,1,"Rows:",150,0,
                              parameter.rows,1,layers_number,
                              1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(gimp_double_adjustment_update),
                     &parameter.rows);
  if(parameter.columns>layers_number) parameter.columns=layers_number;
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,2,"Columns:",150,0,
                              parameter.columns,1,layers_number,
                              1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(gimp_double_adjustment_update),
                     &parameter.columns);
  gtk_table_set_row_spacing(GTK_TABLE(table),2,15);

  /* separation slider */
  label=gtk_label_new("Separation Lines");
  gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,3,4);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);
  if(parameter.separation>drawable->width)
    parameter.separation=drawable->width;
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,4,"Width:",150,0,
                              parameter.separation,0,drawable->width,
                              1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(gimp_double_adjustment_update),
                     &parameter.separation);
  gtk_table_set_row_spacing(GTK_TABLE(table),4,15);

  /* propagation menues */
  label=gtk_label_new("Propagation");
  gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,5,6);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);  
  gtk_table_set_row_spacing(GTK_TABLE(table),5,3);
  create_menu(table,0,6,"Order:",
	      name_order,&parameter.row_by_row,
	      (GtkSignalFunc)row_by_row_update);
  create_menu(table,0,7,"Horizontal:",
	      name_horizontal,&parameter.left_to_right,
	      (GtkSignalFunc)left_to_right_update);
  create_menu(table,0,8,"Vertical:",
	      name_vertical,&parameter.top_to_bottom,
	      (GtkSignalFunc)top_to_bottom_update);
  
  gtk_widget_show(table);
  gtk_widget_show(frame);
  gtk_widget_show(ovbox);
  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}
