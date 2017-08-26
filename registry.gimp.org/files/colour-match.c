/* Install Command: gimptool-2.0 --install colour-match.c
 * License: GPLv2.0
 * Based on the algorithm from ColorMapRelative plugin by Lars Clausen Copyright (C) 2008  Amplex A/S (lars@amplex.dk)
 * which was based on code by Ling Xu and Emanuele Zattin
 * Copyright © 2009 Aareus Inc.
 */

#include<libgimp/gimp.h>
#include<libgimp/gimpui.h>
#include<gtk/gtk.h>

static void query(void);
static void run(const gchar*name,gint nparams,const GimpParam*param,gint*nreturn_vals,GimpParam**return_vals);
GimpPlugInInfo PLUG_IN_INFO={NULL,NULL,query,run};
MAIN();
static void query(void)
	{
	static GimpParamDef args[]=
		{
		{GIMP_PDB_INT32,"run_mode","Run mode"},
		{GIMP_PDB_IMAGE,"image","Input image"},
		{GIMP_PDB_DRAWABLE,"drawable","Input drawable"},
		{GIMP_PDB_INT32,"source_h","Source Hue"},
		{GIMP_PDB_INT32,"source_s","Source Saturation"},
		{GIMP_PDB_INT32,"source_v","Source Value"},
		{GIMP_PDB_INT32,"target_h","Target Hue"},
		{GIMP_PDB_INT32,"target_s","Target Saturation"},
		{GIMP_PDB_INT32,"target_v","Target Value"}
		};
	gimp_install_procedure("plug-in-colour-match","Colour Match","Easily map colour shades keeping highlights and fidelity.","Arel Welgan / Aareus Inc.","Copyright © 2009 Aareus Inc.",
		"2009","Colour _Match","RGB*",GIMP_PLUGIN,G_N_ELEMENTS(args),0,args,NULL);
	gimp_plugin_menu_register("plug-in-colour-match","<Image>/Colors");
	}

typedef struct{GimpHSV source,target;}parameters;
static parameters config={{0.12,0.72,0.50},{0.24,0.30,0.80}};
static GtkColorSelection*source_widget,*target_widget;
static GdkColor source_colour,target_colour;
static GimpPreview*preview;
static gint current_running;
static void update(GtkWidget*widget,GimpDrawable*drawable);
static void map(GimpDrawable*drawable);
static void run(const gchar*name,gint nparams,const GimpParam*param,gint*nreturn_vals,GimpParam**return_vals)
	{
	static GimpParam values[1];
	GimpDrawable*drawable;

	values[0].type=GIMP_PDB_STATUS;
	values[0].data.d_status=GIMP_PDB_SUCCESS;
	*nreturn_vals=1;
	*return_vals=values;
	gimp_get_data("plug-in-relative-colour-map",&config);
	drawable=gimp_drawable_get(param[2].data.d_drawable);
	if(drawable->bpp<3)
		{
		gimp_message("Does not work on grayscale or colour-indexed images.");
		return;
		}

	if(param[0].data.d_int32==GIMP_RUN_INTERACTIVE)
		{
		GtkWidget*dialog,*outer,*box,*frame;
		GimpRGB rgb;
		gimp_hsv_to_rgb(&config.source,&rgb),source_colour.red=(gint16)(rgb.r*65535.0),source_colour.green=(gint16)(rgb.g*65535.0),source_colour.blue=(gint16)(rgb.b*65535.0);
		gimp_hsv_to_rgb(&config.target,&rgb),target_colour.red=(gint16)(rgb.r*65535.0),target_colour.green=(gint16)(rgb.g*65535.0),target_colour.blue=(gint16)(rgb.b*65535.0);
		gimp_ui_init("plug-in-relative-colour-map",TRUE);
		dialog=gimp_dialog_new("Relative Colour Map","plug-in-relative-colour-map",NULL,0,gimp_standard_help_func,NULL,
			GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,GTK_STOCK_OK,GTK_RESPONSE_OK,NULL);
		gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),GTK_RESPONSE_OK,GTK_RESPONSE_CANCEL,-1);
		gimp_window_set_transient(GTK_WINDOW(dialog));

		outer=gtk_hbox_new(FALSE,12);
		gtk_container_set_border_width(GTK_CONTAINER(outer),12);
		gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),outer);
		preview=(GimpPreview*)gimp_drawable_preview_new(drawable,NULL);
		gtk_box_pack_start(GTK_BOX(outer),(GtkWidget*)preview,TRUE,TRUE,0);
		g_signal_connect(preview,"invalidated",G_CALLBACK(update),drawable);
		box=gtk_vbox_new(TRUE,12);
		gtk_container_add(GTK_CONTAINER(outer),box);

		frame=gtk_frame_new("");
		gtk_label_set_markup((GtkLabel*)gtk_frame_get_label_widget((GtkFrame*)frame),"<b><i>Source Colour</i></b>");
		gtk_frame_set_shadow_type((GtkFrame*)frame,GTK_SHADOW_ETCHED_OUT);
		gtk_box_pack_start_defaults(GTK_BOX(box),frame);
		source_widget=(GtkColorSelection*)gtk_color_selection_new();
		gtk_color_selection_set_has_opacity_control(source_widget,FALSE);
		gtk_color_selection_set_previous_color(source_widget,&source_colour);
		gtk_color_selection_set_current_color(source_widget,&source_colour);
		gtk_container_add(GTK_CONTAINER(frame),(GtkWidget*)source_widget);
  		g_signal_connect_swapped(source_widget,"color-changed",G_CALLBACK(gimp_preview_invalidate),preview);

		frame=gtk_frame_new("");
		gtk_label_set_markup((GtkLabel*)gtk_frame_get_label_widget((GtkFrame*)frame),"<b><i>Target Colour</i></b>");
		gtk_frame_set_shadow_type((GtkFrame*)frame,GTK_SHADOW_ETCHED_OUT);
		gtk_box_pack_start_defaults(GTK_BOX(box),frame);
		target_widget=(GtkColorSelection*)gtk_color_selection_new();
		gtk_color_selection_set_has_opacity_control(target_widget,FALSE);
		gtk_color_selection_set_previous_color(target_widget,&target_colour);
		gtk_color_selection_set_current_color(target_widget,&target_colour);
		gtk_container_add(GTK_CONTAINER(frame),(GtkWidget*)target_widget);
		g_signal_connect_swapped(target_widget,"color-changed",G_CALLBACK(gimp_preview_invalidate),preview);

		gtk_widget_show_all(dialog);
		if(gimp_dialog_run(GIMP_DIALOG(dialog))!=GTK_RESPONSE_OK)return;
		gtk_color_selection_get_current_color(source_widget,&source_colour);
		gtk_color_selection_get_current_color(target_widget,&target_colour);
        gtk_widget_destroy(dialog);

		rgb.r=(double)source_colour.red/65535.0,rgb.g=(double)source_colour.green/65535.0,rgb.b=(double)source_colour.blue/65535.0;
		gimp_rgb_to_hsv(&rgb,&config.source);
		rgb.r=(double)target_colour.red/65535.0,rgb.g=(double)target_colour.green/65535.0,rgb.b=(double)target_colour.blue/65535.0;
		gimp_rgb_to_hsv(&rgb,&config.target);

		gimp_set_data("plug-in-relative-colour-map",&config,sizeof(config));
		}
	else if(param[0].data.d_int32==GIMP_RUN_NONINTERACTIVE&&nparams==9)
		{
		config.source.h=param[3].data.d_int32,config.source.s=param[4].data.d_int32,config.source.v=param[5].data.d_int32;
		config.target.h=param[6].data.d_int32,config.target.s=param[7].data.d_int32,config.target.v=param[8].data.d_int32;
		}
	gimp_progress_init("Relative Colour Map");
	map(drawable);
	}

static void update(GtkWidget*widget,GimpDrawable*drawable)
	{
	GimpRGB rgb;
	GimpPixelRgn in,out;
	gint left,top,width,height,status,statmax;
	guchar*data,*edata;
	gint x,y,r,run_code;
	GimpHSV hsv;
	double hdiff,sdiff1,sdiff2,vdiff1,vdiff2;
	run_code=++current_running;

	gtk_color_selection_get_current_color(source_widget,&source_colour);
	gtk_color_selection_get_current_color(target_widget,&target_colour);

	rgb.r=(double)source_colour.red/65535.0,rgb.g=(double)source_colour.green/65535.0,rgb.b=(double)source_colour.blue/65535.0;
	gimp_rgb_to_hsv(&rgb,&config.source);
	rgb.r=(double)target_colour.red/65535.0,rgb.g=(double)target_colour.green/65535.0,rgb.b=(double)target_colour.blue/65535.0;
	gimp_rgb_to_hsv(&rgb,&config.target);


	hdiff=config.source.h-config.target.h;
	if(config.source.s>0.0)
		{
		sdiff1=config.target.s/config.source.s;
		sdiff2=config.target.s>config.source.s?(1.0-config.target.s)/(1.0-config.source.s):1.0;
		}
	else sdiff1=sdiff2=-1.0;
	if(config.source.v>0.0)
		{
		vdiff1=config.target.v/config.source.v;
		vdiff2=config.target.v>config.source.v?(1.0-config.target.v)/(1.0-config.source.v):1.0;
		}
	else vdiff1=vdiff2=-1.0;

	gimp_preview_get_position(preview,&left,&top);
	gimp_preview_get_size(preview,&width,&height);

	gimp_pixel_rgn_init(&in,drawable,left,top,width,height,FALSE,FALSE);
	edata=data=g_malloc(in.bpp*width*height);
	gimp_pixel_rgn_get_rect(&in,data,left,top,width,height);
	for(x=0;x<width*height;x++,data+=in.bpp)
		{
		if(run_code!=current_running)return;
		gimp_rgb_to_hsv4(data,&hsv.h,&hsv.s,&hsv.v);
		hsv.h-=hdiff;
		while(hsv.h>1.0)hsv.h-=1.0;
		while(hsv.h<0.0)hsv.h+=1.0;
		if(sdiff1!=-1)hsv.s=((sdiff1>1)&(hsv.s>config.source.s))?(1.0-(1.0f-hsv.s)*sdiff2):(hsv.s*sdiff1);
		if(vdiff1!=-1)hsv.v=((vdiff1>1)&(hsv.v>config.source.v))?(1.0-(1.0f-hsv.v)*vdiff2):(hsv.v*vdiff1);
		gimp_hsv_to_rgb4(data,hsv.h,hsv.s,hsv.v);
		}
	gimp_preview_draw_buffer(preview,edata,width*in.bpp);
	g_free(edata);
	}

static void map(GimpDrawable*drawable)
	{
	GimpPixelRgn in,out,*rgn[]={&in,&out};
	gpointer pn;
	gint left,top,right,bottom,status,statmax;
	guchar*datai,*datao;
	gint x,y,a,r,run_code;
	GimpHSV hsv;
	double hdiff,sdiff1,sdiff2,vdiff1,vdiff2;
	run_code=++current_running;
	hdiff=config.source.h-config.target.h;
	if(config.source.s>0.0)
		{
		sdiff1=config.target.s/config.source.s;
		sdiff2=config.target.s>config.source.s?(1.0-config.target.s)/(1.0-config.source.s):1.0;
		}
	else sdiff1=sdiff2=-1.0;
	if(config.source.v>0.0)
		{
		vdiff1=config.target.v/config.source.v;
		vdiff2=config.target.v>config.source.v?(1.0-config.target.v)/(1.0-config.source.v):1.0;
		}
	else vdiff1=vdiff2=-1.0;

	gimp_drawable_mask_bounds(drawable->drawable_id,&left,&top,&right,&bottom);
	gimp_pixel_rgn_init(&in,drawable,left,top,right-left,bottom-top,FALSE,FALSE);
	gimp_pixel_rgn_init(&out,drawable,left,top,right-left,bottom-top,TRUE,TRUE);
	status=0,statmax=(bottom-top)/gimp_tile_height()+(right-left)/gimp_tile_width();
	if(run_code!=current_running)return;
	for(pn=gimp_pixel_rgns_register2(2,rgn);pn!=NULL;pn=gimp_pixel_rgns_process(pn))
		{
		datai=in.data,datao=out.data;
		r=in.rowstride-in.w*in.bpp;
		for(y=0;y<in.h;y++,datai+=r,datao+=r)for(x=0;x<in.w;x++,datai+=in.bpp,datao+=out.bpp)
			{
			if(run_code!=current_running)return;
			gimp_rgb_to_hsv4(datai,&hsv.h,&hsv.s,&hsv.v);
			hsv.h-=hdiff;
			while(hsv.h>1.0)hsv.h-=1.0;
			while(hsv.h<0.0)hsv.h+=1.0;
			if(sdiff1!=-1)hsv.s=((sdiff1>1)&(hsv.s>config.source.s))?(1.0-(1.0f-hsv.s)*sdiff2):(hsv.s*sdiff1);
			if(vdiff1!=-1)hsv.v=((vdiff1>1)&(hsv.v>config.source.v))?(1.0-(1.0f-hsv.v)*vdiff2):(hsv.v*vdiff1);
			gimp_hsv_to_rgb4(datao,hsv.h,hsv.s,hsv.v);
			for(a=3;a<=in.bpp;a++)datao[a]=datai[a];
			}
		status++;
		gimp_progress_update((double)(status)/statmax);
		}
	gimp_drawable_flush(drawable);
	gimp_drawable_merge_shadow(drawable->drawable_id,TRUE);
	gimp_drawable_update(drawable->drawable_id,left,top,right-left,bottom-top);
	gimp_drawable_detach(drawable);
	gimp_displays_flush();
	}
