/* interlace tool
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

/* This plug-in (de)interlaces all layers of a film. 
 */

/*
 * Installation:
 * gimptool --install intoolace.c
 *
 * Enjoy!
 */


#include "stdio.h"
#include "string.h"
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <gtk/gtk.h>


#define PLUG_IN_NAME    "intoolace"
#define PLUG_IN_VERSION "Intoolace 0.2a"
#define PLUG_IN_DATE    "12.2002-9.2004"


/* type of deinterlace parameters */
typedef struct
{
  gint32 even_first;
  gint32 keep_aspect;
  gint32 deinterlace;
} tparameter;


static tparameter parameter =
{
  0,
  0,
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

/* deinterlace film... */
static void intoolace(gint32 image_id);

/* user interface */
static gint get_parameters();


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

/* access to levels */
guchar    *level_buffer;
GtkWidget *level_preview;
double     level_log_max;


/********************************************************************/
/* procedures                                                       */
/********************************************************************/
MAIN()

/* communication to the GIMP */
static void query(void)
{
  static GimpParamDef params[] =
  {
    { GIMP_PDB_INT32,    "run_mode",      "Interactive,non-interactive"},
    { GIMP_PDB_IMAGE,    "image_id",      "Input image" },
    { GIMP_PDB_DRAWABLE, "drawable_id",   "Input drawable" },
    { GIMP_PDB_INT32,    "even_first",    "Take even lines first (TRUE,FALSE)"},
    { GIMP_PDB_INT32,    "keep_aspect",   "Keep aspect ratio (TRUE,FALSE)"},
    { GIMP_PDB_INT32,    "deinterlace",   "deinterlace (TRUE),"
      "interlace (FALSE)"}
  };

  static GimpParamDef *return_vals  = NULL;
  static int           nparams      = sizeof(params)/sizeof(params[0]);
  static int           nreturn_vals = 0;
  

  gimp_install_procedure("plug_in_"PLUG_IN_NAME,
                         "deinterlaces a film",
                         "This plug-in (de)interlaces all layers of a film. ",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Image>/Filters/Animation/Interlace Tool",
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
  static GimpParam  values[1];
  GimpPDBStatusType status;
  GimpRunMode       run_mode;

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
    if(!get_parameters()) return;
    gimp_set_data("plug_in_"PLUG_IN_NAME,&parameter,sizeof(tparameter));
    break;
  case GIMP_RUN_NONINTERACTIVE:
    if (nparams!=6)
      status=GIMP_PDB_CALLING_ERROR;
    else
    {
      parameter.even_first =param[3].data.d_int32;
      parameter.keep_aspect=param[4].data.d_int32;
      parameter.deinterlace=param[5].data.d_int32;
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
    intoolace(param[1].data.d_image);
  values[0].data.d_status = status;
}


/********************************************************************/
/* deinterlace layer                                                */
/********************************************************************/
static void deinterlace(gint32 layer_id, gboolean even)
{
  GimpPixelRgn src_region, dest_region;
  GimpDrawable *drawable;

  gint width, height;
  gint32 buffer_length, line_length, x;
  guchar *buffer, *src_1, *src_2, *dest;

  /* determine size of layer */
  drawable   = gimp_drawable_get(layer_id);
  width      = drawable->width;
  height     = drawable->height;
  line_length= drawable->bpp*width;
  buffer_length=line_length*height;

  /* get buffer */
  buffer=g_new(guchar,buffer_length);
    
  /* initialize regions */
  gimp_pixel_rgn_init(&src_region ,drawable,0,0,width,height,FALSE,FALSE);
  gimp_pixel_rgn_init(&dest_region,drawable,0,0,width,height,TRUE,TRUE);

  /* get layer */
  gimp_pixel_rgn_get_rect(&src_region,buffer,0,0,width,height);

  /* interpolate/skip lines */
  src_1=buffer;
  if(!even)
  {
    memcpy(buffer,buffer+line_length,line_length);
    src_1+=line_length;
  }
  dest =src_1+line_length;

  if(parameter.keep_aspect)
  {
    src_2=dest +line_length;
    while(src_2<buffer+buffer_length)
    {
      for(x=0; x<line_length; x++) *dest++=((gint)*src_1+++*src_2++)/2;
      src_1+=line_length;
      src_2+=line_length;
      dest +=line_length;
    }
    if(dest<buffer+buffer_length) memcpy(dest,src_1,line_length);
  }
  else
  {
    dest=buffer;
    while(src_1<buffer+buffer_length)
    {
      memcpy(dest,src_1,line_length);
      src_1+=2*line_length;
      dest +=  line_length;
    }
  }

  /* set region */
  gimp_pixel_rgn_set_rect(&dest_region,buffer,0,0,width,height);   

  /* Tidy up dirty drawable */
  gimp_drawable_flush(drawable);
  gimp_drawable_merge_shadow(drawable->drawable_id, TRUE);
  gimp_drawable_update(drawable->drawable_id,0,0,width,height);
  gimp_drawable_detach(drawable);

  if(!parameter.keep_aspect)
    gimp_layer_resize(layer_id,width,height/2,0,0);

  g_free(buffer);
}



/********************************************************************/
/* (de)interlace                                                    */
/********************************************************************/
static void intoolace(gint32 image_id)
{
  gint    x, n, width, height, dest_height;
  gint32  layers_number, number;
  gint32  first_line_length, second_line_length;
  gint32 *layers, first_layer_id, second_layer_id;
  gchar  *layer_name, *buffer;
  gchar  *first_buffer , *second_buffer , *dest_buffer ;
  gchar  *first_pointer, *second_pointer, *dest_pointer;
  gboolean      from_first;
  GimpDrawable *drawable, *first_drawable, *second_drawable;
  GimpPixelRgn  first_region, second_region, dest_region;

  layers=gimp_image_get_layers(image_id, &layers_number);

  /* check image/ layer size  */
  drawable=gimp_drawable_get(layers[0]);
  width =drawable->width ;
  height=dest_height=drawable->height;
  gimp_drawable_detach(drawable);
  for(number=layers_number-1; number>0; number--)
  {
    drawable=gimp_drawable_get(layers[number]);
    if(width !=drawable->width || height!=drawable->height)
    {
      g_message("intool: All layers must be of the same size.");
      return;
    }
    gimp_drawable_detach(drawable);
  }

  if(parameter.deinterlace)
  {
    /* deinterlace */

    gimp_progress_init("deinterlacing film...");  
    /* loop through layers */
    for(number=layers_number-1; number>=0; number--)
    {
      /* create second layer */
      first_layer_id  =layers[number];
      second_layer_id=gimp_layer_copy(first_layer_id);
      gimp_image_add_layer(image_id,second_layer_id,number);

      gimp_progress_update(((gdouble)layers_number-number-0.5)/layers_number);
      deinterlace(first_layer_id ,parameter.even_first?TRUE :FALSE);
      gimp_progress_update(((gdouble)layers_number-number)/layers_number);
      deinterlace(second_layer_id,parameter.even_first?FALSE:TRUE );

      layer_name=gimp_drawable_get_name(first_layer_id);
      buffer=g_new(gchar,strlen(layer_name)+10);
      strcpy(buffer,layer_name);
      strcat(buffer,parameter.even_first?" (even)":" (odd)");
      gimp_drawable_set_name(first_layer_id,buffer);
      strcpy(buffer,layer_name);
      strcat(buffer,parameter.even_first?" (odd)":" (even)");
      gimp_drawable_set_name(second_layer_id,buffer);
    }
    dest_height=height/2;
  }
  else
  {
    /* interlace */
    gimp_progress_init("interlacing film...");

    /* init buffers */
    first_buffer =g_new(guchar,8*width*height);
    second_buffer=g_new(guchar,4*width*height);
    dest_buffer  =g_new(guchar,8*width*height);
  
    /* loop through layers */
    for(number=layers_number-1; number>=1; number-=2)
    {
      /* get layer ID */
      first_layer_id =layers[parameter.even_first?number  :number-1];
      second_layer_id=layers[parameter.even_first?number-1:number  ];

      /* double layer size? */
      if(!parameter.keep_aspect)
      {
	dest_height=2*height;
	gimp_layer_resize(first_layer_id,width,dest_height,0,0);
      }
      else
	dest_height=height;

      /* get drawable */
      first_drawable =gimp_drawable_get(first_layer_id );
      second_drawable=gimp_drawable_get(second_layer_id);
      first_line_length =width*first_drawable->bpp;
      second_line_length=width*second_drawable->bpp;

      /* initialize regions */
      gimp_pixel_rgn_init(&first_region ,first_drawable ,
			  0,0,width,height     ,FALSE,FALSE);
      gimp_pixel_rgn_init(&second_region,second_drawable,
			  0,0,width,height     ,FALSE,FALSE);
      gimp_pixel_rgn_init(&dest_region  ,first_drawable ,
			  0,0,width,dest_height,TRUE ,TRUE );

      gimp_pixel_rgn_get_rect(&first_region ,first_buffer ,
			      0,0,width,height     );
      gimp_pixel_rgn_get_rect(&second_region,second_buffer,
			      0,0,width,height     );
      gimp_pixel_rgn_get_rect(&dest_region  ,dest_buffer  ,
			      0,0,width,dest_height);

      first_pointer =first_buffer +first_line_length *height;
      second_pointer=second_buffer+second_line_length*height;
      dest_pointer  =dest_buffer  +first_line_length *dest_height;

      from_first=FALSE;
      if(parameter.keep_aspect && height%2==0)
      {
	first_pointer -=first_line_length;
      }
      if(parameter.keep_aspect && height%2!=0)
      {
	second_pointer-=second_line_length;
	from_first=TRUE;
      }

      while(dest_pointer>dest_buffer)
      {
	if(from_first)
	{
	  for(x=0; x<width; x++)
	  {
	    first_pointer -=first_drawable->bpp;
	    dest_pointer  -=first_drawable->bpp;
	    for(n=0; n<3; n++) dest_pointer[n]=first_pointer[n];
	  }
	  if(parameter.keep_aspect) first_pointer -=first_line_length;
	  from_first=FALSE;
	}
	else
	{
	  for(x=0; x<width; x++)
	  {
	    second_pointer-=second_drawable->bpp;
	    dest_pointer  -=first_drawable->bpp;
	    for(n=0; n<3; n++) dest_pointer[n]=second_pointer[n];
	  }
	  if(parameter.keep_aspect) second_pointer-=second_line_length;
	  from_first=TRUE;
	}
      }

      gimp_pixel_rgn_set_rect(&dest_region,dest_buffer,
			      0,0,width,dest_height);
      buffer=g_strdup_printf("frame %d/%d",
			     layers_number-number-1,layers_number-number);
      gimp_drawable_set_name(first_layer_id,buffer);
      g_free(buffer);
      gimp_progress_update(((gdouble)layers_number-number)/layers_number);

      /* Tidy up dirty drawable, ... */
      gimp_drawable_flush(first_drawable);
      gimp_drawable_merge_shadow(first_drawable->drawable_id, TRUE);
      gimp_drawable_update(first_drawable->drawable_id,0,0,width,dest_height);
      gimp_drawable_detach(first_drawable);
      gimp_drawable_detach(second_drawable);
      gimp_image_remove_layer(image_id,second_layer_id);
    }
    if(number==0) gimp_image_remove_layer(image_id,layers[0]);

    g_free(first_buffer );
    g_free(second_buffer);
    g_free(dest_buffer  );
  }
  if(!parameter.keep_aspect)
    gimp_image_resize(image_id,width,dest_height,0,0);
  gimp_displays_flush();
}


/********************************************************************/
/* user interface                                                   */
/********************************************************************/

#define RESPONSE_INTER   1
#define RESPONSE_DEINTER 2

/* exit dialog */
static void dialog_callback(GtkWidget *widget,
                            gint response_id, gpointer data)
{
  parameter.deinterlace=FALSE; 
  switch(response_id)
  {
  case RESPONSE_DEINTER:
    parameter.deinterlace=TRUE;
  case RESPONSE_INTER:
    parameter_ok=TRUE;
  default:
    break;
  } 
  gtk_widget_destroy(widget);
}


/*  main dialog *****************************************************/
static gint get_parameters()
{
  GtkWidget *dlg;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *selection;

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
		       "Interlace",     RESPONSE_INTER,
		       "Deinterlace",   RESPONSE_DEINTER,
                       GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,
                       NULL);
  g_signal_connect(dlg,"response",G_CALLBACK(dialog_callback),NULL);
  g_signal_connect(dlg,"destroy", G_CALLBACK(gtk_main_quit),  NULL);

  /*  parameter settings  */
  frame=gtk_frame_new("(De)Interlace Film");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_border_width(GTK_CONTAINER(frame),5);
  gtk_box_pack_start(GTK_BOX (GTK_DIALOG(dlg)->vbox),frame,TRUE,TRUE,0);

  table=gtk_table_new(3,2,FALSE);
  gtk_container_border_width(GTK_CONTAINER(table),10);
  gtk_container_add(GTK_CONTAINER(frame),table);

  selection=
    gimp_option_menu_new2(FALSE,G_CALLBACK(gimp_menu_item_update),
                          &parameter.even_first,
                          (gpointer) parameter.even_first,
                          "Odd Lines first (normal for DV)",
			  (gpointer) 0,  NULL,
                          "Even Lines first",
			  (gpointer) 1, NULL,
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,1,2,0,1,
                   GTK_EXPAND|GTK_FILL,0,0,0);
  gtk_widget_show(selection);

  selection=
    gimp_option_menu_new2(FALSE,G_CALLBACK(gimp_menu_item_update),
                          &parameter.keep_aspect,
                          (gpointer) parameter.keep_aspect,
                          "change aspect ratio, keep total number of lines",
			  (gpointer) 0,  NULL,
                          "keep aspect ratio, interpolate/drop lines",
			  (gpointer) 1, NULL,
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,1,2,1,2,
                   GTK_EXPAND|GTK_FILL,0,0,0);
  gtk_widget_show(selection);

  gtk_widget_show(table);
  gtk_widget_show(frame);
  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}
