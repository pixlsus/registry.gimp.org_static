/* Split
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

/* This plug-in splits an image into layers.
 * It's like the reverse of the poster plug-in.
 */

/*
 * Installation:
 * gimptool --install split.c
 *
 * Enjoy!
 */


#include <stdlib.h>
#include <stdio.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "split"
#define PLUG_IN_VERSION "Split 0.4"
#define PLUG_IN_DATE    "8.2000-6.2004"


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

/* split image */
static gint32 split(gint32 image_id,
		    gint32 drawable_id);

/* user interface */
static gint get_parameters(GimpDrawable *drawable);


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

/* split parameters*/
typedef struct 
{
  gdouble columns;
  gdouble rows;
  gint    row_by_row;
  gint    left_to_right;
  gint    top_to_bottom;
} tparameter;

static tparameter parameter =
{
  1,
  1,
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
                         "Splitting an image",
                         "This plug-in splits an image into layers. "
			 "The number of rows/columns and the direction of"
			 "propagation can be selected.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Filters/Animation/Split into Layers",
                         "RGB*,GRAY*",
                         GIMP_PLUGIN,
                         nparams,
                         nreturn_vals,
                         params,
                         return_vals);
}


/********************************************************************/
static void run(const gchar     *name,
                gint             nparams,
                const GimpParam *param,
                gint            *nreturn_vals,
                GimpParam      **return_vals)
{
  gint32     new_image_id=0;
  static GimpParam values[1];
  GimpParasite *parasite;
  gint          parasite_num;
  gchar       **parasite_name;
  
  *nreturn_vals=1;
  *return_vals=values;
  values[0].type=GIMP_PDB_STATUS;
  values[0].data.d_status=GIMP_PDB_SUCCESS;
  
  gimp_get_data("plug_in_"PLUG_IN_NAME,&parameter);
  if(!get_parameters(gimp_drawable_get(param[2].data.d_drawable)))
    return;
  
  new_image_id=split(param[1].data.d_image,param[2].data.d_drawable);
  gimp_display_new(new_image_id);
  
  gimp_set_data("plug_in_"PLUG_IN_NAME,&parameter,sizeof(tparameter));

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
/* split procedure                                                 */
/********************************************************************/
static gint32 split(gint32 image_id,
		    gint32 drawable_id)
{
  char       text[255];
  gint32     new_image_id=0;
  gint32     new_layer_id=0;
  
  GimpImageBaseType image_type=GIMP_RGB;
  GimpDrawable *drawable, *new_drawable;
  GimpPixelRgn  region,    new_region;
  
  gint layer;
  gint row, column, width, height;
  gdouble d_width, d_height;
  
  guchar *buffer, *source, *destination;
  guint bpp_diff, bpp_num;
  
  
  drawable=gimp_drawable_get(drawable_id);
  
  /* determine type of new imange */
  switch(gimp_drawable_type(drawable_id))
  {
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      image_type=GIMP_RGB;
      break;
    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:
      image_type=GIMP_GRAY;
      break;
    case GIMP_INDEXED_IMAGE:
    case GIMP_INDEXEDA_IMAGE:
      image_type=GIMP_INDEXED;
      break;
  }
  
  /* calculate split parameters */
  d_width =drawable->width /parameter.columns;
  d_height=drawable->height/parameter.rows;
  width=d_width;
  height=d_height;
  
  /* create new image */
  new_image_id=gimp_image_new(width,height,image_type);
  
  /*  copy the colormap, if necessary  */
  if(image_type==GIMP_INDEXED)
  {
    int ncols;
    guchar *cmap;
    
    cmap=gimp_image_get_cmap(image_id, &ncols);
    gimp_image_set_cmap(new_image_id, cmap, ncols);
    g_free(cmap);
  }

  /* get copy buffer */
  if((buffer=malloc(height*width*4*sizeof(*buffer)))==NULL)
    exit(EXIT_FAILURE);

  /* loop through new layers */
  gimp_progress_init("splitting image...");
  row   =parameter.top_to_bottom?0:parameter.rows   -1;
  column=parameter.left_to_right?0:parameter.columns-1;
  
  for(layer=0; layer<parameter.rows*parameter.columns; layer++)
  {
    /* show progress */
    gimp_progress_update((gdouble)layer/
			 (parameter.rows*parameter.columns));
    
    /* initialize destination layer */
    sprintf(text,"frame %d",layer);
    new_layer_id=gimp_layer_new(new_image_id,
				text,
				width, height,
				gimp_drawable_type(drawable_id),
				100,
				GIMP_NORMAL_MODE);
    gimp_image_add_layer(new_image_id, new_layer_id, 0);
    new_drawable=gimp_drawable_get(new_layer_id);
    gimp_pixel_rgn_init(&new_region,new_drawable,
			0,0,width,height,
			TRUE,TRUE);
    
    /* initialize source region & get frame */
    gimp_pixel_rgn_init(&region,drawable,
			column*d_width,row*d_height,width,height,
			FALSE,FALSE);
    gimp_pixel_rgn_get_rect(&region,buffer,
			    column*d_width,row*d_height,width,height);
    
    /* copy a layer without alpha in an image with alpha */
    bpp_diff=new_drawable->bpp-drawable->bpp;
    if(bpp_diff>0)
    {
      destination=buffer+new_drawable->bpp*width*height-1;
      
      for(source=buffer+drawable->bpp*width*height-1;source>=buffer;)
      {
	for(bpp_num=1; bpp_num<=bpp_diff; bpp_num++)
	  *destination--=GUCHAR_MAX;
	for(bpp_num=1; bpp_num<=drawable->bpp; bpp_num++)
	  *destination--=*source--;
      }
    }
    /* is there a possibility for bpp_diff<0??? */
    
    /* copy frame into new layer */
    gimp_pixel_rgn_set_rect(&new_region,buffer,0,0,width,height);	  
    
    /* Tidy up dirty drawable */
    gimp_drawable_flush(new_drawable);
    gimp_drawable_merge_shadow(new_drawable->drawable_id, TRUE);
    gimp_drawable_update(new_drawable->drawable_id,0,0,width,height);
    gimp_drawable_detach(new_drawable);	
    
    /* select next source */      
    if(parameter.row_by_row)
    {
      column+=parameter.left_to_right?1:-1;
      if(column<0 || column>=parameter.columns)
      {
	column =parameter.left_to_right?0:parameter.columns-1; 
	row   +=parameter.top_to_bottom?1:-1;
      }
    }
    else
    {
      row+=parameter.top_to_bottom?1:-1;
      if(row<0 || row>=parameter.rows)
      {
	row    =parameter.top_to_bottom?0:parameter.rows   -1;
	column+=parameter.left_to_right?1:-1;
      }
    }
  }
  
  gimp_displays_flush();
  
  free(buffer);
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
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,x,x+1,y,y+1,
                   GTK_EXPAND|GTK_FILL,0,0,0);
  gtk_widget_show(selection);

  return selection;
}


/********************************************************************/
/* main dialog ******************************************************/
/********************************************************************/
static gint get_parameters(GimpDrawable *drawable)
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
  frame=gtk_frame_new("Split Image into Layers");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame,TRUE,TRUE,0);
  
  ovbox=gtk_vbox_new(FALSE,5);
  gtk_container_set_border_width(GTK_CONTAINER(ovbox),4);
  gtk_container_add(GTK_CONTAINER(frame),ovbox);

  table=gtk_table_new(7,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_box_pack_start(GTK_BOX(ovbox),table,TRUE,TRUE,0);

  /* geometry sliders */
  label=gtk_label_new("Geometry");
  gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,0,1);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);
  if(parameter.rows>drawable->height) parameter.rows=drawable->height;
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,1,"Rows:",150,0,
                              parameter.rows,1,drawable->height,
                              1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(gimp_double_adjustment_update),
                     &parameter.rows);
  if(parameter.columns>drawable->width) parameter.columns=drawable->width;
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,2,"Columns:",150,0,
                              parameter.columns,1,drawable->width,
                              1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(gimp_double_adjustment_update),
                     &parameter.columns);
  gtk_table_set_row_spacing(GTK_TABLE(table),2,15);

  /* propagation menues */
  label=gtk_label_new("Propagation");
  gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,3,4);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);  
  gtk_table_set_row_spacing(GTK_TABLE(table),3,3);
  create_menu(table,0,4,"Order:",
	      name_order,&parameter.row_by_row,
	      (GtkSignalFunc)row_by_row_update);
  create_menu(table,0,5,"Horizontal:",
	      name_horizontal,&parameter.left_to_right,
	      (GtkSignalFunc)left_to_right_update);
  create_menu(table,0,6,"Vertical:",
	      name_vertical,&parameter.top_to_bottom,
	      (GtkSignalFunc)top_to_bottom_update);
  
  gtk_widget_show(table);
  gtk_widget_show(frame);
  gtk_widget_show(ovbox);
  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}

