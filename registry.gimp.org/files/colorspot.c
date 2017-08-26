/*¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯*\
|   COLOR SPOTTER 1.0               |
|   Plug-in for GIMP                |
|   Copyright François Gingras      |
|   September 16th, 2009            |
|                                   |
|   Use & distribute as you please, |
|   but please give credit! Thanks! |
\*__________________________________*/

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <math.h>

GtkWidget *spinbuttonR, *spinbuttonG, *spinbuttonB, *colorButton;
GimpRGB selectedColorButton;

typedef struct
{
	gint red;
	gint green;
	gint blue;
	gdouble intensity;
	gint colorSelection;
	gboolean relative;
	gboolean preview;
} Parameters;

static void query (void);
static gboolean colorspot_dialog (GimpDrawable *drawable);
static void run (	const gchar		*name,
					gint			nparams,
					const GimpParam	*param,
					gint			*nreturn_vals,
					GimpParam		**return_vals);
static void colorSpotter (GimpDrawable *drawable, GimpPreview  *preview);
static void set_relative_absolute( GtkToggleButton *button, Parameters *bvals );
static void set_color_selection( GtkToggleButton *button, Parameters *bvals );
static void colorButtonCallback( GimpColorButton *button, Parameters *bvals );

GimpPlugInInfo PLUG_IN_INFO =
{
	NULL,
	NULL,
	query,
	run
};

MAIN()

/* Set up default values for options */
static Parameters bvals =
{
	0, 0, 0, 1.33, 3, FALSE, TRUE
};


static void query (void)
{
	static GimpParamDef args[] =
	{
		{
			GIMP_PDB_INT32,
			"run-mode",
			"Run mode"
		},
	{
		GIMP_PDB_IMAGE,
		"image",
		"Input image"
	},
	{
		GIMP_PDB_DRAWABLE,
		"drawable",
		"Input drawable"
	}
	};

	gimp_install_procedure (
		"plug-in-colorspotter",
		"Color Spotter",
		"Recolorizes image's pixels based on their similarity with a specified color",
		"François Gingras",
		"Copyright François Gingras",
		"2009",
		"_Color spotter...",
		"RGB*",
		GIMP_PLUGIN,
		G_N_ELEMENTS (args), 0,
		args, NULL);

	gimp_plugin_menu_register (	"plug-in-colorspotter", "<Image>/Filters/Misc");
}

static void run (
	const gchar		*name,
	gint			nparams,
	const GimpParam	*param,
	gint			*nreturn_vals,
	GimpParam		**return_vals)
{
	static GimpParam	values[1];
	GimpPDBStatusType	status = GIMP_PDB_SUCCESS;
	GimpRunMode			run_mode;
	GimpDrawable		*drawable;

	// Setting mandatory output values
	*nreturn_vals = 1;
	*return_vals  = values;

	values[0].type = GIMP_PDB_STATUS;
	values[0].data.d_status = status;
	run_mode = param[0].data.d_int32;
	drawable = gimp_drawable_get (param[2].data.d_drawable);

	switch (run_mode)
	{
		case GIMP_RUN_INTERACTIVE:
		/* Get options last values if needed */
		gimp_get_data ("plug-in-colorspotter", &bvals);

		/* Display the dialog */
		if (! colorspot_dialog (drawable)) return;
		break;

		case GIMP_RUN_NONINTERACTIVE:
		if (nparams != 10) status = GIMP_PDB_CALLING_ERROR;
		if (status == GIMP_PDB_SUCCESS)
		{
			bvals.red = param[3].data.d_int32;
			bvals.green = param[4].data.d_int32;
			bvals.blue = param[5].data.d_int32;
			bvals.intensity = param[6].data.d_int32;
			bvals.colorSelection = param[7].data.d_int32;
			bvals.relative = param[8].data.d_int32;
			bvals.preview = FALSE;
		}
		break;

		case GIMP_RUN_WITH_LAST_VALS:
		/*  Get options' last values if needed  */
		gimp_get_data ("plug-in-colorspotter", &bvals);
		break;

		default: break;
	}
	
	if (!bvals.preview) gimp_progress_init ("Color spotting in progress...");
	colorSpotter(drawable, NULL);
	gimp_displays_flush ();
	gimp_drawable_detach (drawable);
	
	if (run_mode == GIMP_RUN_INTERACTIVE) gimp_set_data ("plug-in-colorspotter", &bvals, sizeof (Parameters));
}

static void colorSpotter (GimpDrawable *drawable, GimpPreview  *preview)
{
	gint i, j, k, channels, x1, y1, x2, y2, sum, sum2;
	GimpPixelRgn rgn_in, rgn_out;
	guchar *inRect, *outRect, r, g, b, rs, gs, bs;
	gdouble maxDifference = 0;
	gulong *differences;
	gint width, height;
	GimpRGB selectedColor;

	if(preview)
	{
		gimp_preview_get_position (preview, &x1, &y1);
		gimp_preview_get_size (preview, &width, &height);
		x2 = x1 + width;
		y2 = y1 + height;
	}
	else
	gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);

	channels = gimp_drawable_bpp (drawable->drawable_id);
	gimp_tile_cache_ntiles (4 * (drawable->width / gimp_tile_width () + 1));
	sum2 = (x2-x1)*(y2-y1+1)*channels;

	// Let's initialize our input and output drawables
	gimp_pixel_rgn_init (	&rgn_in,
							drawable,
							x1, y1,
							x2 - x1, y2 - y1, 
							FALSE, FALSE);
	gimp_pixel_rgn_init (	&rgn_out,
							drawable,
							x1, y1,
							x2 - x1, y2 - y1, 
							TRUE, TRUE);

	inRect = g_new (guchar, channels * (x2 - x1) * (y2 - y1));
	outRect = g_new (guchar, channels * (x2 - x1) * (y2 - y1));

	gimp_pixel_rgn_get_rect (	&rgn_in,
								inRect,
								x1,
								y1,
								x2-x1,
								y2-y1);

	switch(bvals.colorSelection)
	{
		// RGB
		case 3: gimp_rgb_set_uchar (&selectedColor, bvals.red, bvals.green, bvals.blue);
				break;

		// Foreground color
		case 2: if(!gimp_context_get_foreground(&selectedColor)) gimp_rgb_set_uchar (&selectedColor, bvals.red, bvals.green, bvals.blue);
				break;

		// Background color
		case 1: if(!gimp_context_get_background(&selectedColor)) gimp_rgb_set_uchar (&selectedColor, bvals.red, bvals.green, bvals.blue);
				break;

		// Color button
		case 0:	selectedColor.r = selectedColorButton.r;
				selectedColor.g = selectedColorButton.g;
				selectedColor.b = selectedColorButton.b;
				break;
		
		default: gimp_rgb_set_uchar (&selectedColor, bvals.red, bvals.green, bvals.blue);
				 g_print("ERROR!!!\n");
				break;
	}
	gimp_rgb_get_uchar (&selectedColor, &rs, &gs, &bs);
	
	// Fill the differences table first
	differences = g_new (gulong, (x2-x1) * (y2-y1));
	for (i = y1; i < y2; i++) for (j = x1; j < x2; j++)
	{
		sum = channels * ((x2-x1) * (i-y1) + (j-x1));
		r = inRect[sum];
		g = inRect[sum + 1];
		b = inRect[sum + 2];
		differences[sum/channels] = pow(fabs(r-rs) + fabs(g-gs) +fabs(b-bs), bvals.intensity);
		if(!bvals.relative && differences[sum/channels]>255) differences[sum/channels] = 255;
		if (!preview && sum % (gint)floor((sum2)/66+1) < channels) gimp_progress_update ((gdouble)sum*2 / (gdouble)(3*sum2+1));
	}
	
	if(bvals.relative)
	{
		for(i=0; i < (x2-x1) * (y2-y1); i++) maxDifference = (differences[i] > maxDifference) ? differences[i] : maxDifference;
		for(i=0; i < (x2-x1) * (y2-y1); i++) differences[i] = differences[i] * 255 / maxDifference;
	}
		
	// Set the new colors
	for (i = y1; i < y2; i++) for (j = x1; j < x2; j++)
	{
		sum = channels * ((x2-x1) * (i-y1) + (j-x1));
		for (k = 0; k < channels; k++)
		{
			outRect[sum+k] = differences[sum/channels];
			if(outRect[sum+k]<0) outRect[sum+k] = 0;
			if(k>=3) outRect[sum+k] = inRect[sum+k];
		}
		if (!preview && sum % (gint)floor(sum2/34+1) < channels) gimp_progress_update (0.66+(gdouble)sum / (gdouble)(3*sum2+1));
	}
	gimp_pixel_rgn_set_rect (&rgn_out, outRect, x1, y1, x2 - x1, y2-y1);
	gimp_progress_update (1);
	
	// Free the rectangles for which we allocated some memory earlier
	g_free(inRect);
	g_free(outRect);
	g_free(differences);
	
	//  Let's update the image
	if(preview)	gimp_drawable_preview_draw_region (GIMP_DRAWABLE_PREVIEW (preview), &rgn_out);
	else
	{
		gimp_drawable_flush (drawable);
		gimp_drawable_merge_shadow (drawable->drawable_id, TRUE);
		gimp_drawable_update (drawable->drawable_id, x1, y1, x2 - x1, y2 - y1);
	}
}

static gboolean colorspot_dialog (GimpDrawable *drawable)
{
	GtkWidget *dialog;
	GtkWidget *vbox_main, *vbox_color;
	GtkWidget *hbox_color1, *hbox_color2, *hbox_intensity;
	GtkWidget *frame_main, *frame_intensity;
	GtkWidget *alignment_main, *alignment_intensity;
	GtkWidget *spinbuttonIntensity;
	GtkObject *spinbuttonIntensity_adj, *spinbuttonR_adj, *spinbuttonG_adj, *spinbuttonB_adj;
	GtkWidget *frame_label_color, *frame_label_intensity;
	gboolean   run;
	GtkWidget *preview;
    GtkWidget *button_abs, *button_rel, *button_color_selection1, *button_color_selection2, *button_color_selection3, *button_color_selection4;
    GSList    *group_abs_rel, *group_color_selection;

	// General
	gimp_ui_init ("ui_spottage", FALSE);

	dialog = gimp_dialog_new (	"Color spotting", "ui_spottage",
								NULL, 0,
								gimp_standard_help_func, "plug-in-colorspotter",
								GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
								GTK_STOCK_OK,     GTK_RESPONSE_OK,
								NULL);

	vbox_main = gtk_vbox_new (FALSE, 6);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), vbox_main);

	// Preview
	preview = gimp_drawable_preview_new (drawable, &bvals.preview);
	gtk_box_pack_start (GTK_BOX (vbox_main), preview, TRUE, TRUE, 0);
	g_signal_connect_swapped (preview, "invalidated", G_CALLBACK (colorSpotter), drawable);

	// Color
	frame_main = gtk_frame_new (NULL);
	gtk_box_pack_start (GTK_BOX (vbox_main), frame_main, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (frame_main), 5);
	
	frame_label_color = gtk_label_new ("Color to spot");
	gtk_frame_set_label_widget (GTK_FRAME (frame_main), frame_label_color);
	gtk_label_set_use_markup (GTK_LABEL (frame_label_color), TRUE);
	
	alignment_main = gtk_alignment_new (0, 0, 0, 0);
	gtk_container_add (GTK_CONTAINER (frame_main), alignment_main);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment_main), 5, 5, 5, 5);
	
	vbox_color = gtk_vbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (alignment_main), vbox_color);
	
	hbox_color1 = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox_color), hbox_color1, TRUE, FALSE, 5);
	
	button_color_selection1 = gtk_radio_button_new_with_mnemonic (NULL, "R_GB");
	gtk_box_pack_start (GTK_BOX (hbox_color1), button_color_selection1, TRUE, TRUE, 0);
	group_color_selection = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_color_selection1));
	g_signal_connect (G_OBJECT (button_color_selection1), "toggled", G_CALLBACK (set_color_selection), &bvals);
	g_signal_connect_swapped (G_OBJECT (button_color_selection1), "toggled", G_CALLBACK (gimp_preview_invalidate), preview);
	
	spinbuttonR_adj = gtk_adjustment_new (0, 0, 255, 1, 10, 0);
	spinbuttonG_adj = gtk_adjustment_new (0, 0, 255, 1, 10, 0);
	spinbuttonB_adj = gtk_adjustment_new (0, 0, 255, 1, 10, 0);
	spinbuttonR = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonR_adj), 1, 0);
	spinbuttonG = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonG_adj), 1, 0);
	spinbuttonB = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonB_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_color1), spinbuttonR, TRUE, FALSE, 5);
	gtk_box_pack_start (GTK_BOX (hbox_color1), spinbuttonG, TRUE, FALSE, 5);
	gtk_box_pack_start (GTK_BOX (hbox_color1), spinbuttonB, TRUE, FALSE, 5);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonR), TRUE);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonG), TRUE);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonB), TRUE);
	g_signal_connect_swapped (spinbuttonR_adj, "value_changed", G_CALLBACK (gimp_preview_invalidate), preview);
	g_signal_connect_swapped (spinbuttonG_adj, "value_changed", G_CALLBACK (gimp_preview_invalidate), preview);
	g_signal_connect_swapped (spinbuttonB_adj, "value_changed", G_CALLBACK (gimp_preview_invalidate), preview);
	g_signal_connect (spinbuttonR_adj, "value_changed", G_CALLBACK (gimp_int_adjustment_update), &bvals.red);
	g_signal_connect (spinbuttonG_adj, "value_changed", G_CALLBACK (gimp_int_adjustment_update), &bvals.green);
	g_signal_connect (spinbuttonB_adj, "value_changed", G_CALLBACK (gimp_int_adjustment_update), &bvals.blue);
	
	button_color_selection2 = gtk_radio_button_new_with_mnemonic(group_color_selection, "_Foreground color");
	group_color_selection = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_color_selection2));
	g_signal_connect (G_OBJECT (button_color_selection2), "toggled", G_CALLBACK (set_color_selection), &bvals);
	g_signal_connect_swapped (G_OBJECT (button_color_selection2), "toggled", G_CALLBACK (gimp_preview_invalidate), preview);
	gtk_box_pack_start (GTK_BOX (vbox_color), button_color_selection2, TRUE, TRUE, 0);

	button_color_selection3 = gtk_radio_button_new_with_mnemonic(group_color_selection, "_Background color");
	group_color_selection = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_color_selection3));
	g_signal_connect (G_OBJECT (button_color_selection3), "toggled", G_CALLBACK (set_color_selection), &bvals);
	g_signal_connect_swapped (G_OBJECT (button_color_selection3), "toggled", G_CALLBACK (gimp_preview_invalidate), preview);
	gtk_box_pack_start (GTK_BOX (vbox_color), button_color_selection3, TRUE, TRUE, 0);
	
	hbox_color2 = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox_color), hbox_color2, TRUE, FALSE, 5);

	button_color_selection4 = gtk_radio_button_new_with_mnemonic(group_color_selection, "_Select color:");
	group_color_selection = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_color_selection4));
	g_signal_connect (G_OBJECT (button_color_selection4), "toggled", G_CALLBACK (set_color_selection), &bvals);
	g_signal_connect_swapped (G_OBJECT (button_color_selection4), "toggled", G_CALLBACK (gimp_preview_invalidate), preview);
	gtk_box_pack_start (GTK_BOX (hbox_color2), button_color_selection4, TRUE, TRUE, 0);
	
	colorButton = gimp_color_button_new ("Choose color to spot", 100, 15, &selectedColorButton, 0);
	gtk_box_pack_start (GTK_BOX (hbox_color2), colorButton, FALSE, FALSE, 0);
	gtk_widget_set_sensitive (colorButton, FALSE);
	g_signal_connect_swapped (G_OBJECT (colorButton), "color-changed", G_CALLBACK (gimp_preview_invalidate), preview);
	g_signal_connect (G_OBJECT (colorButton), "color-changed", G_CALLBACK (colorButtonCallback), NULL);
	
	// Intensity
	frame_intensity = gtk_frame_new (NULL);
	gtk_box_pack_start (GTK_BOX (vbox_main), frame_intensity, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (frame_intensity), 5);
	
	frame_label_intensity = gtk_label_new ("Differenciation intensity");
	gtk_frame_set_label_widget (GTK_FRAME (frame_intensity), frame_label_intensity);
	gtk_label_set_use_markup (GTK_LABEL (frame_label_intensity), TRUE);
	
	alignment_intensity = gtk_alignment_new (0, 0, 0, 0);
	gtk_container_add (GTK_CONTAINER (frame_intensity), alignment_intensity);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment_intensity), 5, 15, 15, 5);

	hbox_intensity = gtk_hbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (alignment_intensity), hbox_intensity);
	
	spinbuttonIntensity_adj = gtk_adjustment_new (1.33, 0.01, 7.00, 0.01, 0.1, 0);
	spinbuttonIntensity = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonIntensity_adj), 0.01, 2); // Climb rate of 0.01, 2 decimals
	gtk_box_pack_start (GTK_BOX (hbox_intensity), spinbuttonIntensity, TRUE, FALSE, 5);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonIntensity), TRUE); // Only numeric values allowed
	g_signal_connect_swapped (spinbuttonIntensity_adj,	"value_changed", G_CALLBACK (gimp_preview_invalidate), preview);
	g_signal_connect (spinbuttonIntensity_adj, "value_changed", G_CALLBACK (gimp_double_adjustment_update), &bvals.intensity);
	
	// Absolute / relative
	button_abs = gtk_radio_button_new_with_mnemonic (NULL, "_Absolute");
	gtk_box_pack_start (GTK_BOX (hbox_intensity), button_abs, TRUE, TRUE, 0);
	group_abs_rel = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_abs));
	g_signal_connect (G_OBJECT (button_abs), "toggled", G_CALLBACK (set_relative_absolute), &bvals);
	g_signal_connect_swapped (G_OBJECT (button_abs), "toggled", G_CALLBACK (gimp_preview_invalidate), preview);

	button_rel = gtk_radio_button_new_with_mnemonic(group_abs_rel, "_Relative");
	group_abs_rel = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button_rel));
	gtk_box_pack_start (GTK_BOX (hbox_intensity), button_rel, TRUE, TRUE, 0);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button_abs), TRUE);

	// General
	colorSpotter (drawable, GIMP_PREVIEW (preview));
	gtk_widget_show_all (dialog);
	run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);
	gtk_widget_destroy (dialog);

	return run;
}

static void set_relative_absolute( GtkToggleButton *button, Parameters *bvals )
{
	bvals->relative = !(bvals->relative);
}
static void set_color_selection( GtkToggleButton *button, Parameters *bvals )
{
	bvals->colorSelection = g_slist_index (gtk_radio_button_get_group (GTK_RADIO_BUTTON (button)), button);
	
	if(bvals->colorSelection == 3)
	{
		gtk_widget_set_sensitive (spinbuttonR, TRUE);
		gtk_widget_set_sensitive (spinbuttonG, TRUE);
		gtk_widget_set_sensitive (spinbuttonB, TRUE);
	}
	else
	{
		gtk_widget_set_sensitive (spinbuttonR, FALSE);
		gtk_widget_set_sensitive (spinbuttonG, FALSE);
		gtk_widget_set_sensitive (spinbuttonB, FALSE);
	}
	if(bvals->colorSelection == 0)
	{
		gtk_widget_set_sensitive (colorButton, TRUE);
	}
	else
	{
		gtk_widget_set_sensitive (colorButton, FALSE);
	}
}

static void colorButtonCallback( GimpColorButton *button, Parameters *null )
{
	gimp_color_button_get_color(button, &selectedColorButton);
}
