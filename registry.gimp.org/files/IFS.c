/*¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯*\
|   IFS 1.0                         |
|   Plug-in for GIMP                |
|   Copyright François Gingras      |
|   July 9th, 2010                  |
|                                   |
|   Use & distribute as you please, |
|   but please give credit! Thanks! |
\*__________________________________*/

#define IFS_SETTINGS_FILE "ifs.txt"
#define IFS_TEXT_LENGTH 128

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

// ----------------- GLOBAL DECLARATIONS ----------------------
GtkWidget *checkButtonSupersampling, *checkButtonSpecifyTargetSize, *spinbuttonTargetSizeH, *spinbuttonTargetSizeV;
GtkObject *spinbuttonTargetSizeH_adj, *spinbuttonTargetSizeV_adj;
gint32 id;
gboolean faux = FALSE, vrai = TRUE;

typedef struct
{
	gint nbFunctions;
	gint nbIterations;
	gint targetWidth, targetHeight;
	gdouble *a, *b, *c, *d, *e, *f;
	char ifsName[IFS_TEXT_LENGTH];
	gboolean supersampling;
	gboolean specifyTargetSize;
	gboolean useCartesianCoordinates;
	gboolean resizeAfterEachIteration;
} Parameters;

static Parameters bvals =
{
	0, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, FALSE, "", FALSE, FALSE, FALSE, TRUE
};


// ----------------- FUNCTION DECLARATIONS -----------------------
static void query (void);
static gboolean ifs_dialog (GimpDrawable *drawable);
static void run (	const gchar		*name,
					gint			nparams,
					const GimpParam	*param,
					gint			*nreturn_vals,
					GimpParam		**return_vals);
static void ifs (GimpDrawable *drawable);
static void setDefaultParameters();
static void freeAllocatedMemory();
static void allocateMemoryFunctions();
static void saveOptionsLastValues();
static gboolean loadOptionsLastValues();
static void callback_comboBoxifsSelection(GtkComboBox *combo, gboolean *widthOrHeight);
static void callback_supersampling(GtkWidget *widget, gboolean *supersampling);
static void callback_specifyTargetSize(GtkWidget *widget, gboolean *specifyTargetSize);
static void callback_updateTargetSize(GtkWidget *widget, gboolean *widthOrHeight);
double minimum (double a, double b);
double maximum (double a, double b);
double arrondir (double a);
void getContractionRatio (gint hOrig, gint vOrig, double *hRatio, double *vRatio, gint *final_width, gint *final_height);

// ----------------- MACROS AND STUFF ----------------------------
GimpPlugInInfo PLUG_IN_INFO =
{
	NULL,
	NULL,
	query,
	run
};

MAIN()

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
		"plug-in-ifs",
		"IFS",
		"Generates the result of a finite number of iterations of an IFS over an image",
		"François Gingras",
		"Copyright François Gingras",
		"2010",
		"_IFS...",
		"RGB*, GRAY*",
		GIMP_PLUGIN,
		G_N_ELEMENTS (args), 0,
		args, NULL);

	gimp_plugin_menu_register (	"plug-in-ifs", "<Image>/Filters/Misc");
}


// ----------------- MAIN RUN FUNCTION ---------------------------
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

	*nreturn_vals = 1;
	*return_vals  = values;
	values[0].type = GIMP_PDB_STATUS;
	values[0].data.d_status = status;
	run_mode = param[0].data.d_int32;
	drawable = gimp_drawable_get (param[2].data.d_drawable);

	if(!loadOptionsLastValues()) setDefaultParameters();
	
	switch (run_mode)
	{
		case GIMP_RUN_INTERACTIVE:
			if (! ifs_dialog (drawable)) return;
			break;

		default: break;
	}
	
	ifs(drawable);
	gimp_displays_flush ();
	gimp_drawable_detach (drawable);
	
	freeAllocatedMemory();
}

// ----------------- MAIN PLUGIN ---------------------------
static void ifs (GimpDrawable *drawable)
{
	gint x1, y1, x2, y2;
	gint width, height, final_width, final_height;
	gint channels;
	gint i, j;
	gint32 image;
	gint32 layers[bvals.nbFunctions];
	gint32 retvals;
	const GimpParam	*params;
	char *drawableName, progressString[40];
	gdouble hRatio, vRatio;
	
	// Get the useful constants
	id = drawable->drawable_id;
	image = gimp_drawable_get_image(id);
	gimp_image_undo_group_start(image);
	drawableName = gimp_drawable_get_name(id);
	if(TRUE) id = gimp_image_merge_visible_layers(image, GIMP_EXPAND_AS_NECESSARY);
	
	if(bvals.specifyTargetSize)
	{
		gimp_drawable_mask_bounds (id, &x1, &y1, &x2, &y2);
		width = x2 - x1;
		height = y2 - y1;
		getContractionRatio (width, height, &hRatio, &vRatio, &final_width, &final_height);
		final_width = bvals.targetWidth;
		final_height = bvals.targetHeight;
		width = final_width / pow(hRatio, (double)bvals.nbIterations);
		height = final_height / pow(vRatio, (double)bvals.nbIterations);
		gimp_layer_scale(id, width, height, FALSE);
	}
	channels = gimp_drawable_bpp (id);
	gimp_drawable_get_name(id);
	
	if(bvals.useCartesianCoordinates)
	{
		gimp_image_flip(image, GIMP_ORIENTATION_VERTICAL);
	}
	
	if(!bvals.resizeAfterEachIteration)
	{
		gimp_drawable_mask_bounds (id, &x1, &y1, &x2, &y2);
		width = x2 - x1;
		height = y2 - y1;
	}
	for(i=0; i<bvals.nbIterations; i++)
	{
		if(bvals.resizeAfterEachIteration)
		{
			gimp_drawable_mask_bounds (id, &x1, &y1, &x2, &y2);
			width = x2 - x1;
			height = y2 - y1;
		}
		for(j=0; j<bvals.nbFunctions; j++)
		{
			sprintf(progressString, "Iteration %d/%d, function %d/%d  -  %2.0f%%", i+1, bvals.nbIterations, j+1, bvals.nbFunctions, 100*(i*bvals.nbFunctions + j)/(1.0*bvals.nbIterations*bvals.nbFunctions));
			gimp_progress_init(progressString);
			layers[j] = gimp_layer_new_from_drawable(id, image);
			gimp_image_add_layer(image, layers[j], -1);
			if(bvals.useCartesianCoordinates)
			{
				layers[j] = gimp_drawable_transform_matrix_default(	layers[j],
																	bvals.a[j],	bvals.c[j], bvals.e[j]*width,
																	bvals.b[j], bvals.d[j],	bvals.f[j]*height,
																	0,			0,			1,
																	bvals.supersampling, GIMP_TRANSFORM_RESIZE_ADJUST);
			}
			else
			{
				layers[j] = gimp_drawable_transform_matrix_default(	layers[j],
																	bvals.a[j],	bvals.b[j], bvals.e[j]*width,
																	bvals.c[j], bvals.d[j],	bvals.f[j]*height,
																	0,			0,			1,
																	bvals.supersampling, GIMP_TRANSFORM_RESIZE_ADJUST);
			}
		}
		gimp_image_remove_layer(image, id);
		for(j=0; j<bvals.nbFunctions-2; j++) layers[bvals.nbFunctions-1] = gimp_image_merge_down(image, layers[bvals.nbFunctions-1], GIMP_EXPAND_AS_NECESSARY);
		if(bvals.nbFunctions > 1) id = gimp_image_merge_down(image, layers[bvals.nbFunctions-1], GIMP_EXPAND_AS_NECESSARY);
		else id = layers[0];
		if(bvals.resizeAfterEachIteration)
		{
			params = gimp_run_procedure ("plug_in_autocrop_layer", &retvals, GIMP_PDB_INT32, GIMP_RUN_NONINTERACTIVE, GIMP_PDB_IMAGE, image, GIMP_PDB_DRAWABLE, id, GIMP_PDB_END);
			gimp_image_resize_to_layers(image);
		}
	}
	if(bvals.useCartesianCoordinates)
	{
		gimp_image_flip(image, GIMP_ORIENTATION_VERTICAL);
	}
	if(!bvals.resizeAfterEachIteration)
	{
		params = gimp_run_procedure ("plug_in_autocrop_layer", &retvals, GIMP_PDB_INT32, GIMP_RUN_NONINTERACTIVE, GIMP_PDB_IMAGE, image, GIMP_PDB_DRAWABLE, id, GIMP_PDB_END);
		gimp_image_resize_to_layers(image);
	}

	gimp_drawable_set_name(id, drawableName);
	gimp_image_undo_group_end(image);
	
	saveOptionsLastValues();
}

// ----------------- MAIN DIALOG ---------------------------
static gboolean ifs_dialog (GimpDrawable *drawable)
{
	GtkWidget *dialog;
	GtkWidget *vbox_main;
	GtkWidget *hbox_iterations, *hbox_ifs_selection, *hbox_target_size, *hbox_supersampling, *vbox_options;
	GtkWidget *frame_iterations, *frame_ifs_selection, *frame_options;
	GtkWidget *alignment_iterations, *alignment_ifs_selection, *alignment_options;
	GtkWidget *spinbuttonIterations, *comboBoxifsSelection;
	GtkObject *spinbuttonIterations_adj;
	GtkWidget *frame_label_iterations, *frame_label_ifs_selection, *frame_label_options, *label_times, *label_pixels;
	gboolean   run, widthOrHeight = FALSE;
	char *predefinedIFS[] = {	"Sierpinski triangle",
								"Sierpinski carpet",
								"Pythagoras tree",
								"Hex flake",
								"T-square",
								"Self affine set",
								"Cantor dust",
								"Vicsek snowflake",
								"Pentaflake"
							};
	gint nb_predefined_ifs = 9, i;

	// General
	id = drawable->drawable_id;
	gimp_ui_init ("ui_ifs", FALSE);

	dialog = gimp_dialog_new (	"IFS", "ui_ifs",
								NULL, 0,
								gimp_standard_help_func, "plug-in-ifs",
								GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
								GTK_STOCK_OK,     GTK_RESPONSE_OK,
								NULL);

	vbox_main = gtk_vbox_new (FALSE, 6);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), vbox_main);
	
	// Iterations
	frame_iterations = gtk_frame_new (NULL);
	gtk_box_pack_start (GTK_BOX (vbox_main), frame_iterations, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (frame_iterations), 5);
	
	frame_label_iterations = gtk_label_new ("Number of iterations");
	gtk_frame_set_label_widget (GTK_FRAME (frame_iterations), frame_label_iterations);
	gtk_label_set_use_markup (GTK_LABEL (frame_label_iterations), TRUE);
	
	alignment_iterations = gtk_alignment_new (0, 0, 0, 0);
	gtk_container_add (GTK_CONTAINER (frame_iterations), alignment_iterations);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment_iterations), 5, 15, 15, 5);

	hbox_iterations = gtk_hbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (alignment_iterations), hbox_iterations);
	
	spinbuttonIterations_adj = gtk_adjustment_new (bvals.nbIterations, 1, 99, 1, 5, 0);
	g_signal_connect_after (spinbuttonIterations_adj, "value_changed", G_CALLBACK (callback_updateTargetSize), &widthOrHeight);
	spinbuttonIterations = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonIterations_adj), 1, 0); // Climb rate, number of decimals
	gtk_box_pack_start (GTK_BOX (hbox_iterations), spinbuttonIterations, TRUE, FALSE, 5);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonIterations), TRUE); // Only numeric values allowed
	g_signal_connect (spinbuttonIterations_adj, "value_changed", G_CALLBACK (gimp_int_adjustment_update), &bvals.nbIterations);
	
	// Fractal selection
	frame_ifs_selection = gtk_frame_new (NULL);
	gtk_box_pack_start (GTK_BOX (vbox_main), frame_ifs_selection, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (frame_ifs_selection), 5);
	
	frame_label_ifs_selection = gtk_label_new ("Choose your IFS");
	gtk_frame_set_label_widget (GTK_FRAME (frame_ifs_selection), frame_label_ifs_selection);
	gtk_label_set_use_markup (GTK_LABEL (frame_label_ifs_selection), TRUE);
	
	alignment_ifs_selection = gtk_alignment_new (0, 0, 0, 0);
	gtk_container_add (GTK_CONTAINER (frame_ifs_selection), alignment_ifs_selection);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment_ifs_selection), 5, 15, 15, 5);

	hbox_ifs_selection = gtk_hbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (alignment_ifs_selection), hbox_ifs_selection);
	
	comboBoxifsSelection = gtk_combo_box_new_text ();
	for(i=0; i<nb_predefined_ifs; i++)
	{
		gtk_combo_box_insert_text (GTK_COMBO_BOX (comboBoxifsSelection), i, predefinedIFS[i]);
		if(!strcmp(bvals.ifsName, predefinedIFS[i])) gtk_combo_box_set_active (GTK_COMBO_BOX (comboBoxifsSelection), i);
	}
	gtk_box_pack_start (GTK_BOX (hbox_ifs_selection), comboBoxifsSelection, TRUE, FALSE, 5);
	g_signal_connect( G_OBJECT( comboBoxifsSelection ), "changed", G_CALLBACK( callback_comboBoxifsSelection ), &widthOrHeight);
	
	// Misc options
	frame_options = gtk_frame_new (NULL);
	gtk_box_pack_start (GTK_BOX (vbox_main), frame_options, TRUE, FALSE, 0);
	
	frame_label_options = gtk_label_new ("Options");
	gtk_frame_set_label_widget (GTK_FRAME (frame_options), frame_label_options);
	
	alignment_options = gtk_alignment_new (0, 0, 0, 0);
	gtk_container_add (GTK_CONTAINER (frame_options), alignment_options);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment_options), 5, 5, 5, 5);

	vbox_options = gtk_vbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (alignment_options), vbox_options);
		
	hbox_supersampling = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox_options), hbox_supersampling, FALSE, FALSE, 5);
	
	checkButtonSupersampling = gtk_check_button_new_with_mnemonic("_Supersampling (slower)");
	gtk_box_pack_start (GTK_BOX (hbox_supersampling), checkButtonSupersampling, FALSE, FALSE, 5);
	g_signal_connect (checkButtonSupersampling, "clicked", G_CALLBACK (callback_supersampling), &bvals.supersampling);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSupersampling), bvals.supersampling);
	
	hbox_target_size = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox_options), hbox_target_size, FALSE, FALSE, 5);
	
	checkButtonSpecifyTargetSize = gtk_check_button_new_with_mnemonic("Specify target si_ze : ");
	gtk_box_pack_start (GTK_BOX (hbox_target_size), checkButtonSpecifyTargetSize, FALSE, FALSE, 5);
	g_signal_connect (checkButtonSpecifyTargetSize, "clicked", G_CALLBACK (callback_specifyTargetSize), &bvals.specifyTargetSize);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize), bvals.specifyTargetSize);
	g_print("%d\n", bvals.specifyTargetSize);

	spinbuttonTargetSizeH_adj = gtk_adjustment_new (1, 1, 999999, 1, 100, 0);
	spinbuttonTargetSizeH = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonTargetSizeH_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_target_size), spinbuttonTargetSizeH, TRUE, FALSE, 5);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonTargetSizeH), TRUE);
	g_signal_connect_after (spinbuttonTargetSizeH_adj, "value_changed", G_CALLBACK (callback_updateTargetSize), &faux);
	
	label_times = gtk_label_new ("X");
	gtk_box_pack_start (GTK_BOX (hbox_target_size), label_times, TRUE, FALSE, 5);

	spinbuttonTargetSizeV_adj = gtk_adjustment_new (1, 1, 999999, 1, 100, 0);
	spinbuttonTargetSizeV = gtk_spin_button_new (GTK_ADJUSTMENT (spinbuttonTargetSizeV_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_target_size), spinbuttonTargetSizeV, TRUE, FALSE, 5);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbuttonTargetSizeV), TRUE);
	g_signal_connect_after (spinbuttonTargetSizeV_adj, "value_changed", G_CALLBACK (callback_updateTargetSize), &vrai);
	
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize), FALSE);
	callback_updateTargetSize(label_times, &widthOrHeight);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize), bvals.specifyTargetSize);
	gtk_widget_set_sensitive(spinbuttonTargetSizeH, bvals.specifyTargetSize);
	gtk_widget_set_sensitive(spinbuttonTargetSizeV, bvals.specifyTargetSize);
		
	label_pixels = gtk_label_new ("pixels");
	gtk_box_pack_start (GTK_BOX (hbox_target_size), label_pixels, TRUE, FALSE, 5);
	
	// General
	gtk_widget_show_all (dialog);
	run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);
	gtk_widget_destroy (dialog);
	return run;
}

static void setDefaultParameters()
{
	bvals.nbFunctions = 3;
	bvals.nbIterations = 3;
	strcpy(bvals.ifsName, "Sierpinski triangle");
	allocateMemoryFunctions();
	bvals.a[0] = 0.5;	bvals.b[0] = 0;		bvals.e[0] = 0;
	bvals.c[0] = 0;		bvals.d[0] = 0.5;	bvals.f[0] = 0;
	bvals.a[1] = 0.5;	bvals.b[1] = 0;		bvals.e[1] = 0.25;
	bvals.c[1] = 0;		bvals.d[1] = 0.5;	bvals.f[1] = 0.5;
	bvals.a[2] = 0.5;	bvals.b[2] = 0;		bvals.e[2] = 0.5;
	bvals.c[2] = 0;		bvals.d[2] = 0.5;	bvals.f[2] = 0;
	bvals.supersampling = FALSE;
	bvals.specifyTargetSize = FALSE;
	bvals.useCartesianCoordinates = TRUE;
	bvals.resizeAfterEachIteration = TRUE;
}

static void freeAllocatedMemory()
{
	if (bvals.a)
	{
		g_free(bvals.a); bvals.a = NULL;
		g_free(bvals.b); bvals.b = NULL;
		g_free(bvals.c); bvals.c = NULL;
		g_free(bvals.d); bvals.d = NULL;
		g_free(bvals.e); bvals.e = NULL;
		g_free(bvals.f); bvals.f = NULL;
	}
}

static void allocateMemoryFunctions()
{
	bvals.a = g_new(gdouble, bvals.nbFunctions);
	bvals.b = g_new(gdouble, bvals.nbFunctions);
	bvals.c = g_new(gdouble, bvals.nbFunctions);
	bvals.d = g_new(gdouble, bvals.nbFunctions);
	bvals.e = g_new(gdouble, bvals.nbFunctions);
	bvals.f = g_new(gdouble, bvals.nbFunctions);
}

static void saveOptionsLastValues()
{
	// À compléter avec les autres paramètres du plugin
	int i;
	FILE *settingsFile;
	gchar *settingsFilePath;
	
	settingsFilePath = gimp_personal_rc_file(IFS_SETTINGS_FILE);
	settingsFile = fopen (settingsFilePath,"w");
	g_free(settingsFilePath);
	
	if(settingsFile)
	{
		fprintf(settingsFile, "nbFunctions = %d\n", bvals.nbFunctions);
		fprintf(settingsFile, "ifsName = %s\n", bvals.ifsName);
		for(i=0; i<bvals.nbFunctions; i++)
		{
			fprintf(settingsFile, "a[%d] = %f\n", i, bvals.a[i]);
			fprintf(settingsFile, "b[%d] = %f\n", i, bvals.b[i]);
			fprintf(settingsFile, "c[%d] = %f\n", i, bvals.c[i]);
			fprintf(settingsFile, "d[%d] = %f\n", i, bvals.d[i]);
			fprintf(settingsFile, "e[%d] = %f\n", i, bvals.e[i]);
			fprintf(settingsFile, "f[%d] = %f\n", i, bvals.f[i]);
		}
		fprintf(settingsFile, "nbIterations = %d\n", bvals.nbIterations);
		fprintf(settingsFile, "supersampling = %d\n", bvals.supersampling);
		fprintf(settingsFile, "specifyTargetSize = %d\n", bvals.specifyTargetSize);
		fprintf(settingsFile, "useCartesianCoordinates = %d\n", bvals.useCartesianCoordinates);
		fprintf(settingsFile, "resizeAfterEachIteration = %d\n", bvals.resizeAfterEachIteration);
		
		fclose(settingsFile);
	}
}

static gboolean loadOptionsLastValues()
{
	// À compléter avec les autres paramètres du plugin
	int i;
	FILE *settingsFile;
	gchar *settingsFilePath;
	char dummyString[IFS_TEXT_LENGTH];
	float floatTemp;
	
	settingsFilePath = gimp_personal_rc_file(IFS_SETTINGS_FILE);
	settingsFile = fopen (settingsFilePath,"r");
	g_free(settingsFilePath);
	
	if(settingsFile)
	{
		fscanf(settingsFile, "nbFunctions = %d\n", &bvals.nbFunctions);
		fscanf(settingsFile, "ifsName = ");
		fgets(bvals.ifsName, IFS_TEXT_LENGTH, settingsFile);
		bvals.ifsName[strlen(bvals.ifsName)-1] = '\0';
		allocateMemoryFunctions();
		for(i=0; i<bvals.nbFunctions; i++)
		{
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.a[i] = floatTemp;
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.b[i] = floatTemp;
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.c[i] = floatTemp;
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.d[i] = floatTemp;
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.e[i] = floatTemp;
			fscanf(settingsFile, "%s = %f\n", dummyString, &floatTemp); bvals.f[i] = floatTemp;
		}
		fscanf(settingsFile, "nbIterations = %d\n", &bvals.nbIterations);
		fscanf(settingsFile, "supersampling = %d\n", &bvals.supersampling);
		fscanf(settingsFile, "specifyTargetSize = %d\n", &bvals.specifyTargetSize);
		fscanf(settingsFile, "useCartesianCoordinates = %d\n", &bvals.useCartesianCoordinates);
		fscanf(settingsFile, "resizeAfterEachIteration = %d\n", &bvals.resizeAfterEachIteration);
		
		fclose(settingsFile);
		return TRUE;
	}
	return FALSE;
}

static void callback_comboBoxifsSelection(GtkComboBox *combo, gboolean *widthOrHeight)
{
	gchar *string = gtk_combo_box_get_active_text( combo );
	freeAllocatedMemory();
	
	if(!strcmp("Sierpinski triangle", string))
	{
		strcpy(bvals.ifsName, "Sierpinski triangle");
		bvals.nbFunctions = 3;
		allocateMemoryFunctions();
		bvals.a[0] = 0.5;	bvals.b[0] = 0;		bvals.e[0] = 0;
		bvals.c[0] = 0;		bvals.d[0] = 0.5;	bvals.f[0] = 0;
		bvals.a[1] = 0.5;	bvals.b[1] = 0;		bvals.e[1] = 0.25;
		bvals.c[1] = 0;		bvals.d[1] = 0.5;	bvals.f[1] = 0.5;
		bvals.a[2] = 0.5;	bvals.b[2] = 0;		bvals.e[2] = 0.5;
		bvals.c[2] = 0;		bvals.d[2] = 0.5;	bvals.f[2] = 0;
		bvals.useCartesianCoordinates = TRUE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Sierpinski carpet", string))
	{
		strcpy(bvals.ifsName, "Sierpinski carpet");
		bvals.nbFunctions = 8;
		allocateMemoryFunctions();
		bvals.a[0] = 0.3333;	bvals.b[0] = 0;			bvals.e[0] = 0;
		bvals.c[0] = 0;			bvals.d[0] = 0.3333;	bvals.f[0] = 0;
		bvals.a[1] = 0.3333;	bvals.b[1] = 0;			bvals.e[1] = 0.3333;
		bvals.c[1] = 0;			bvals.d[1] = 0.3333;	bvals.f[1] = 0;
		bvals.a[2] = 0.3333;	bvals.b[2] = 0;			bvals.e[2] = 2*0.3333;
		bvals.c[2] = 0;			bvals.d[2] = 0.3333;	bvals.f[2] = 0;
		bvals.a[3] = 0.3333;	bvals.b[3] = 0;			bvals.e[3] = 2*0.3333;
		bvals.c[3] = 0;			bvals.d[3] = 0.3333;	bvals.f[3] = 0.3333;
		bvals.a[4] = 0.3333;	bvals.b[4] = 0;			bvals.e[4] = 2*0.3333;
		bvals.c[4] = 0;			bvals.d[4] = 0.3333;	bvals.f[4] = 2*0.3333;
		bvals.a[5] = 0.3333;	bvals.b[5] = 0;			bvals.e[5] = 0.3333;
		bvals.c[5] = 0;			bvals.d[5] = 0.3333;	bvals.f[5] = 2*0.3333;
		bvals.a[6] = 0.3333;	bvals.b[6] = 0;			bvals.e[6] = 0;
		bvals.c[6] = 0;			bvals.d[6] = 0.3333;	bvals.f[6] = 2*0.3333;
		bvals.a[7] = 0.3333;	bvals.b[7] = 0;			bvals.e[7] = 0;
		bvals.c[7] = 0;			bvals.d[7] = 0.3333;	bvals.f[7] = 0.3333;
		bvals.useCartesianCoordinates = FALSE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Pythagoras tree", string))
	{
		strcpy(bvals.ifsName, "Pythagoras tree");
		bvals.nbFunctions = 3;
		allocateMemoryFunctions();
		bvals.a[0] = 1;		bvals.b[0] = 0;		bvals.e[0] = 0;
		bvals.c[0] = 0;		bvals.d[0] = 1;		bvals.f[0] = 0;
		bvals.a[1] = 0.5;	bvals.b[1] = -0.5;	bvals.e[1] = 1;
		bvals.c[1] = 0.5;	bvals.d[1] = 0.5;	bvals.f[1] = -1;
		bvals.a[2] = -0.5;	bvals.b[2] = 0.5;	bvals.e[2] = 0;
		bvals.c[2] = 0.5;	bvals.d[2] = 0.5;	bvals.f[2] = -1;
		bvals.useCartesianCoordinates = FALSE;
		bvals.resizeAfterEachIteration = FALSE;
	}
	if(!strcmp("Hex flake", string))
	{
		strcpy(bvals.ifsName, "Hex flake");
		bvals.nbFunctions = 7;
		allocateMemoryFunctions();
		bvals.a[0] = 0.3333;	bvals.b[0] = 0;			bvals.e[0] = 0.3333;
		bvals.c[0] = 0;			bvals.d[0] = 0.3333;	bvals.f[0] = 0;
		bvals.a[1] = 0.3333;	bvals.b[1] = 0;			bvals.e[1] = 0.3333;
		bvals.c[1] = 0;			bvals.d[1] = 0.3333;	bvals.f[1] = 0.3333;
		bvals.a[2] = 0.3333;	bvals.b[2] = 0;			bvals.e[2] = 0.3333;
		bvals.c[2] = 0;			bvals.d[2] = 0.3333;	bvals.f[2] = 0.6666;
		bvals.a[3] = 0.3333;	bvals.b[3] = 0;			bvals.e[3] = 0;
		bvals.c[3] = 0;			bvals.d[3] = 0.3333;	bvals.f[3] = 0.3333/2;
		bvals.a[4] = 0.3333;	bvals.b[4] = 0;			bvals.e[4] = 0;
		bvals.c[4] = 0;			bvals.d[4] = 0.3333;	bvals.f[4] = 0.3333/2+0.3333;
		bvals.a[5] = 0.3333;	bvals.b[5] = 0;			bvals.e[5] = 0.6666;
		bvals.c[5] = 0;			bvals.d[5] = 0.3333;	bvals.f[5] = 0.3333/2;
		bvals.a[6] = 0.3333;	bvals.b[6] = 0;			bvals.e[6] = 0.6666;
		bvals.c[6] = 0;			bvals.d[6] = 0.3333;	bvals.f[6] = 0.3333/2+0.3333;
		bvals.useCartesianCoordinates = FALSE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("T-square", string))
	{
		strcpy(bvals.ifsName, "T-square");
		bvals.nbFunctions = 5;
		allocateMemoryFunctions();
		bvals.a[0] = 0.5;	bvals.b[0] = 0;		bvals.e[0] = 0.75;
		bvals.c[0] = 0;		bvals.d[0] = 0.5;	bvals.f[0] = 0.75;
		bvals.a[1] = 0.5;	bvals.b[1] = 0;		bvals.e[1] = -0.25;
		bvals.c[1] = 0;		bvals.d[1] = 0.5;	bvals.f[1] = 0.75;
		bvals.a[2] = 0.5;	bvals.b[2] = 0;		bvals.e[2] = 0.75;
		bvals.c[2] = 0;		bvals.d[2] = 0.5;	bvals.f[2] = -0.25;
		bvals.a[3] = 0.5;	bvals.b[3] = 0;		bvals.e[3] = -0.25;
		bvals.c[3] = 0;		bvals.d[3] = 0.5;	bvals.f[3] = -0.25;
		bvals.a[4] = 1;		bvals.b[4] = 0;		bvals.e[4] = 0;
		bvals.c[4] = 0;		bvals.d[4] = 1;		bvals.f[4] = 0;
		bvals.useCartesianCoordinates = FALSE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Self affine set", string))
	{
		strcpy(bvals.ifsName, "Self affine set");
		bvals.nbFunctions = 5;
		allocateMemoryFunctions();
		bvals.a[0] = 0.333;	bvals.b[0] = 0;		bvals.e[0] = 0;
		bvals.c[0] = 0;		bvals.d[0] = 0.333;	bvals.f[0] = 0.333;
		bvals.a[1] = 0.333;	bvals.b[1] = 0;		bvals.e[1] = 0.333;
		bvals.c[1] = 0;		bvals.d[1] = 0.333;	bvals.f[1] = 0.333;
		bvals.a[2] = 0.333;	bvals.b[2] = 0;		bvals.e[2] = 0.666;
		bvals.c[2] = 0;		bvals.d[2] = 0.333;	bvals.f[2] = 0.333;
		bvals.a[3] = 0.333;	bvals.b[3] = 0;		bvals.e[3] = 0.666;
		bvals.c[3] = 0;		bvals.d[3] = 0.333;	bvals.f[3] = 0;
		bvals.a[4] = 0.333;	bvals.b[4] = 0;		bvals.e[4] = 0;
		bvals.c[4] = 0;		bvals.d[4] = 0.333;	bvals.f[4] = 0;
		bvals.useCartesianCoordinates = TRUE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Cantor dust", string))
	{
		strcpy(bvals.ifsName, "Cantor dust");
		bvals.nbFunctions = 4;
		allocateMemoryFunctions();
		bvals.a[0] = 0.3333;	bvals.b[0] = 0;			bvals.e[0] = 0;
		bvals.c[0] = 0;			bvals.d[0] = 0.3333;	bvals.f[0] = 0;
		bvals.a[1] = 0.3333;	bvals.b[1] = 0;			bvals.e[1] = 0.666;
		bvals.c[1] = 0;			bvals.d[1] = 0.3333;	bvals.f[1] = 0;
		bvals.a[2] = 0.3333;	bvals.b[2] = 0;			bvals.e[2] = 0;
		bvals.c[2] = 0;			bvals.d[2] = 0.3333;	bvals.f[2] = 0.666;
		bvals.a[3] = 0.3333;	bvals.b[3] = 0;			bvals.e[3] = 0.666;
		bvals.c[3] = 0;			bvals.d[3] = 0.3333;	bvals.f[3] = 0.666;
		bvals.useCartesianCoordinates = TRUE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Vicsek snowflake", string))
	{
		strcpy(bvals.ifsName, "Vicsek snowflake");
		bvals.nbFunctions = 5;
		allocateMemoryFunctions();
		bvals.a[0] = 0.3333;	bvals.b[0] = 0;			bvals.e[0] = 0.333;
		bvals.c[0] = 0;			bvals.d[0] = 0.3333;	bvals.f[0] = 0.333;
		bvals.a[1] = 0.3333;	bvals.b[1] = 0;			bvals.e[1] = 0.333;
		bvals.c[1] = 0;			bvals.d[1] = 0.3333;	bvals.f[1] = 0;
		bvals.a[2] = 0.3333;	bvals.b[2] = 0;			bvals.e[2] = 0;
		bvals.c[2] = 0;			bvals.d[2] = 0.3333;	bvals.f[2] = 0.333;
		bvals.a[3] = 0.3333;	bvals.b[3] = 0;			bvals.e[3] = 0.333;
		bvals.c[3] = 0;			bvals.d[3] = 0.3333;	bvals.f[3] = 0.666;
		bvals.a[4] = 0.3333;	bvals.b[4] = 0;			bvals.e[4] = 0.666;
		bvals.c[4] = 0;			bvals.d[4] = 0.3333;	bvals.f[4] = 0.333;
		bvals.useCartesianCoordinates = TRUE;
		bvals.resizeAfterEachIteration = TRUE;
	}
	if(!strcmp("Pentaflake", string))
	{
		strcpy(bvals.ifsName, "Pentaflake");
		bvals.nbFunctions = 6;
		allocateMemoryFunctions();
		bvals.a[0] = 0.382;		bvals.b[0] = 0;			bvals.e[0] = 0.118;
		bvals.c[0] = 0;			bvals.d[0] = 0.382;		bvals.f[0] = 0;
		bvals.a[1] = 0.382;		bvals.b[1] = 0;			bvals.e[1] = 0.5;
		bvals.c[1] = 0;			bvals.d[1] = 0.382;		bvals.f[1] = 0;
		bvals.a[2] = 0.382;		bvals.b[2] = 0;			bvals.e[2] = 0;
		bvals.c[2] = 0;			bvals.d[2] = 0.382;		bvals.f[2] = 0.382;
		bvals.a[3] = 0.382;		bvals.b[3] = 0;			bvals.e[3] = 0.618;
		bvals.c[3] = 0;			bvals.d[3] = 0.382;		bvals.f[3] = 0.382;
		bvals.a[4] = 0.382;		bvals.b[4] = 0;			bvals.e[4] = 0.309;
		bvals.c[4] = 0;			bvals.d[4] = 0.382;		bvals.f[4] = 0.618;
		bvals.a[5] = -0.382;	bvals.b[5] = 0;			bvals.e[5] = 0.691;
		bvals.c[5] = 0;			bvals.d[5] = -0.382;	bvals.f[5] = 0.618;
		bvals.useCartesianCoordinates = TRUE;
		bvals.resizeAfterEachIteration = TRUE;
	}

	g_free( string );
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSupersampling), bvals.supersampling);
	*widthOrHeight = bvals.specifyTargetSize;
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize), FALSE);
	callback_updateTargetSize(GTK_WIDGET(combo), widthOrHeight);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize), *widthOrHeight);
}

void callback_supersampling (GtkWidget *widget, gboolean *supersampling)
{
	if (GTK_TOGGLE_BUTTON (widget)->active) *supersampling = TRUE;
	else *supersampling = FALSE;
}

void callback_specifyTargetSize (GtkWidget *widget, gboolean *specifyTargetSize)
{
	if (GTK_TOGGLE_BUTTON (widget)->active)
	{
		*specifyTargetSize = TRUE;
		gtk_widget_set_sensitive(spinbuttonTargetSizeH, TRUE);
		gtk_widget_set_sensitive(spinbuttonTargetSizeV, TRUE);
	}
	else
	{
		*specifyTargetSize = FALSE;
		gtk_widget_set_sensitive(spinbuttonTargetSizeH, FALSE);
		gtk_widget_set_sensitive(spinbuttonTargetSizeV, FALSE);
	}
}

void callback_updateTargetSize (GtkWidget *widget, gboolean *widthOrHeight)
{
	gint x1, x2, y1, y2, width, height, final_width, final_height;
	double hRatio, vRatio;
	gint newWidth, newHeight;
	
	gimp_drawable_mask_bounds (id, &x1, &y1, &x2, &y2);
	width = x2 - x1;
	height = y2 - y1;
	getContractionRatio (width, height, &hRatio, &vRatio, &final_width, &final_height);
	
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (checkButtonSpecifyTargetSize)))
	{
		if(*widthOrHeight) // C'est le height qui a été modifié par l'usager
		{
			newHeight = gtk_adjustment_get_value(GTK_ADJUSTMENT(spinbuttonTargetSizeV_adj));
			newWidth = (((double)newHeight / pow(vRatio, (double)bvals.nbIterations)) / height) * width * pow(hRatio, (double)bvals.nbIterations);
			g_signal_handlers_block_by_func(spinbuttonTargetSizeH_adj, G_CALLBACK(callback_updateTargetSize), &faux);
			gtk_adjustment_set_value(GTK_ADJUSTMENT(spinbuttonTargetSizeH_adj), newWidth);
			gimp_int_adjustment_update (GTK_ADJUSTMENT(spinbuttonTargetSizeH_adj), &newWidth);
			g_signal_handlers_unblock_by_func(spinbuttonTargetSizeH_adj, G_CALLBACK(callback_updateTargetSize), &faux);
		}
		else
		{
			newWidth = gtk_adjustment_get_value(GTK_ADJUSTMENT(spinbuttonTargetSizeH_adj));
			newHeight = (((double)newWidth / pow(hRatio, (double)bvals.nbIterations)) / width) * height * pow(vRatio, (double)bvals.nbIterations);
			g_signal_handlers_block_by_func(spinbuttonTargetSizeV_adj, G_CALLBACK(callback_updateTargetSize), &vrai);
			gtk_adjustment_set_value(GTK_ADJUSTMENT(spinbuttonTargetSizeV_adj), newHeight);
			gimp_int_adjustment_update (GTK_ADJUSTMENT(spinbuttonTargetSizeV_adj), &newHeight);
			g_signal_handlers_unblock_by_func(spinbuttonTargetSizeV_adj, G_CALLBACK(callback_updateTargetSize), &vrai);
		}
		bvals.targetWidth = newWidth;
		bvals.targetHeight = newHeight;
	}
	else
	{		
		gtk_adjustment_set_value(GTK_ADJUSTMENT(spinbuttonTargetSizeH_adj), final_width);
		gtk_adjustment_set_value(GTK_ADJUSTMENT(spinbuttonTargetSizeV_adj), final_height);
		gimp_int_adjustment_update (GTK_ADJUSTMENT(spinbuttonTargetSizeH_adj), &final_width);
		gimp_int_adjustment_update (GTK_ADJUSTMENT(spinbuttonTargetSizeV_adj), &final_height);
		bvals.targetWidth = final_width;
		bvals.targetHeight = final_height;
	}
}

double minimum (double a, double b)
{
	return a>b?b:a;
}
double maximum (double a, double b)
{
	return a>b?a:b;
}
double arrondir (double a)
{
	double b = floor(a);
	return a-b>=0.5?b+1:b;
}

void getContractionRatio (gint hOrig, gint vOrig, double *hRatio, double *vRatio, gint *final_width, gint *final_height)
{
	gint i;
	gdouble x1, x2, y1, y2;
	gdouble x_min = 9999999, x_max = 0, y_min = 9999999, y_max = 0;
	for(i=0; i<bvals.nbFunctions; i++)
	{
		x1 = minimum(minimum(bvals.a[i], bvals.c[i]), minimum(0, bvals.a[i]+bvals.c[i]));
		x2 = maximum(maximum(bvals.a[i], bvals.c[i]), maximum(0, bvals.a[i]+bvals.c[i]));
		y1 = minimum(minimum(bvals.b[i], bvals.d[i]), minimum(0, bvals.b[i]+bvals.d[i]));
		y2 = maximum(maximum(bvals.b[i], bvals.d[i]), maximum(0, bvals.b[i]+bvals.d[i]));
		x_min = minimum(x_min, x1+bvals.e[i]);
		x_max = maximum(x_max, x2+bvals.e[i]);
		y_min = minimum(y_min, y1+bvals.f[i]);
		y_max = maximum(y_max, y2+bvals.f[i]);
	}
	*hRatio = x_max - x_min;
	*vRatio = y_max - y_min;
	*final_width = arrondir(pow(*hRatio, (double)bvals.nbIterations) * hOrig);
	*final_height = arrondir(pow(*vRatio, (double)bvals.nbIterations) * vOrig);
	return;
}
