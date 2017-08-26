/* Export Layers To File Plug-in for Gimp 
 * Copyright (C) 2013 Sashi Kumar <ksashikumark93@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <string.h>

#include <glib/gstdio.h>

#include <gtk/gtk.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>


#define PLUG_IN_PROC   "plug-in-export-layers-to-file"
#define PLUG_IN_BINARY "Export layers to file"
#define PLUG_IN_ROLE   "gimp-export-layers-to-file"

typedef struct 
{
  gint32            drawable_id;
  const gchar      *layer_name;
  GtkWidget        *checkbox;
  GtkWidget        *thumbnail;
  gboolean          select;
} LayerData;
 
typedef struct 
{
  gboolean     format_select;
  gboolean     uri_select;
  gboolean     layers_select;  
} ExportData;

/*Global Variables*/

static LayerData     *layer_set = NULL;
static ExportData     export_set;
static GimpDrawable  *main_drawable = NULL;
static gint32         image_ID;
static gchar         *image_name = NULL;
static gint           nlayers;
static gint           num;
static gint32        *layers = NULL;
static GtkWidget     *dialog = NULL;
static GtkWidget     *button = NULL;
static GtkWidget     *combo = NULL;
static GtkWidget     *export = NULL;
static const gchar   *format = NULL;
static const gchar   *raw_filename = NULL;
static const gchar   *format_set[] = {
   "png", "jpg", "bmp" 
};
static gint           format_count = 3;


/*Local Function Declaration*/
static   void      query               (void);
static   void      run                 (const gchar      *name,
	  	                        gint              nparams,
	  	                        const GimpParam  *param,
	  	                        gint             *nreturn_vals,
	  	                        GimpParam       **return_vals);
static   gboolean  export_dialog       (GimpDrawable     *drawable); 
static   void      cb_select_layers    (GtkWidget        *widget,
                                        gpointer          layers_button);
static   void      cb_layers           (GtkWidget        *widget, 
                                        gpointer          layers_button);
static   void      cb_select           (GtkWidget        *widget, 
                                        gpointer          i);
static   void      cb_format           (GtkWidget        *widget,
                                        gpointer          combo);
static   void      cb_browse           (GtkWidget        *widget,
                                        gpointer          window);
static   void      cb_filepath         (GtkWidget        *widget,
                                        gpointer          data);
static   void      main_init           (void);
static   void      rec_layer_group     (gint32           *layer);
static   void      final_export        (void);
static   void      export_button_check (void);
static   void      create_combo_list   (void);
static   void      select_all_layers   (void);
static   void      deselect_all_layers (void);
static   void      init_layers         (void);
static   void      destroy_all         (void);
static   gboolean  check_sensitive     (void);
static   gboolean  final_check         (void);


const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,  /* init_proc  */
  NULL,  /* quit_proc  */
  query, /* query_proc */
  run    /* run_proc   */
};

MAIN ()

static void
query (void)
{

  static const GimpParamDef args[] =
  {
    { GIMP_PDB_INT32,        "run-mode",     "The run mode { RUN-INTERACTIVE (0) }" },
    { GIMP_PDB_IMAGE,        "image",        "Input image" },
    { GIMP_PDB_DRAWABLE,     "drawable",     "Input Drawable" },
  };

  gimp_install_procedure (PLUG_IN_PROC,
			  "Export sets of layers to a file",
			  "No Help till now",
			  "Sashi Kumar <ksashikumark93@gmail.com>",
			  "Sashi Kumar <ksashikumark93@gmail.com>",
			  "27th March 2013",
			  "Export Layers to File",
			  "RGB*, GRAY*",
			  GIMP_PLUGIN,
			  G_N_ELEMENTS (args), 0,
			  args, NULL);

  gimp_plugin_menu_register (PLUG_IN_PROC, "<Image>/File");
}


static void
run (const gchar      *name,
     gint              n_params,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam values[1];
  GimpDrawable *drawable;
  GimpRunMode run_mode;
  GimpPDBStatusType status = GIMP_PDB_SUCCESS;

  *nreturn_vals = 1;
  *return_vals = values;

  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  /*  Get the params  */
  drawable = gimp_drawable_get(param[2].data.d_drawable);
  main_drawable = drawable;
  image_ID = param[1].data.d_image;
  image_name = gimp_image_get_name(param[1].data.d_image);
  run_mode = (GimpRunMode) param[0].data.d_int32;

  main_init();
  if (run_mode == GIMP_RUN_INTERACTIVE) 
    {
      if (!export_dialog(drawable)) 
        status = GIMP_PDB_EXECUTION_ERROR;
    }

  if (status == GIMP_PDB_SUCCESS) 
    gimp_drawable_detach (drawable);
  
  destroy_all();
}

static gboolean
export_dialog (GimpDrawable *drawable)
{
  GtkWidget *layers_button;
  GtkWidget *check;
  GtkWidget *label1, *label2;
  GtkWidget *browse;
  GtkWidget *main_vbox;
  GtkWidget *hbox1, *hbox2, *hbox3;
  gboolean run;

  main_init();
  gimp_ui_init (PLUG_IN_BINARY, TRUE);


  dialog = gimp_dialog_new (PLUG_IN_BINARY, "export-to-file",
                            NULL, 0,
                            gimp_standard_help_func, PLUG_IN_PROC,
                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            NULL);
  
  export = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                  "Done",
                                  GTK_RESPONSE_OK); 
  export_button_check ();
  
  gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

  main_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
  gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
                      main_vbox, TRUE, TRUE, 0);
  
  hbox1 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox1, FALSE, FALSE, 0);

  layers_button = gtk_button_new_with_mnemonic("Select Layers");
  gtk_box_pack_start (GTK_BOX (hbox1), layers_button, TRUE, TRUE, 0);
  g_signal_connect(layers_button, "clicked",
                      G_CALLBACK(cb_select_layers), (gpointer) dialog);

  check = gtk_check_button_new_with_label("All Layers");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), FALSE);
  gtk_box_pack_start (GTK_BOX (hbox1), check, TRUE, TRUE, 0);
  g_signal_connect(check, "clicked", 
                   G_CALLBACK (cb_layers), (gpointer) layers_button);
  
  
  hbox2 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox2, FALSE, FALSE, 0);

  label1 = gtk_label_new ("Select Format:");
  gtk_box_pack_start (GTK_BOX (hbox2), label1, TRUE, TRUE, 0);
  
  combo = gtk_combo_box_new_text ();
  create_combo_list ();
  gtk_box_pack_start (GTK_BOX (hbox2), combo, TRUE, TRUE, 0);
  g_signal_connect(combo, "changed",
                   G_CALLBACK (cb_format), (gpointer) combo);

  
  hbox3 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox3, FALSE, FALSE, 0);

  label2 = gtk_label_new ("Destination:");
  gtk_box_pack_start (GTK_BOX (hbox3), label2, TRUE, TRUE, 0);

  browse = gtk_button_new_with_mnemonic("Browse");
  gtk_box_pack_start (GTK_BOX (hbox3), browse, TRUE, TRUE, 0);
  g_signal_connect(browse, "clicked",
                   G_CALLBACK(cb_browse), (gpointer) dialog);
  
  gtk_widget_show_all (dialog);

  run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);
  if (run)
    final_export ();
  gtk_widget_destroy (dialog);

  return run;
}
  
static void 
main_init (void)
{
  gint32 *root_layer;
  gint i;
  gint nroot_layer;
  export_set.format_select = FALSE;
  export_set.uri_select = FALSE;
  export_set.layers_select = FALSE;
  nlayers = 0;
  num = 0;
 
  layers = g_new0 (gint32, 50);
  root_layer =  gimp_image_get_layers (image_ID, &nroot_layer);

  for (i = 0; i < nroot_layer; i++)
    {
      rec_layer_group (&root_layer[i]);
    }   
  init_layers ();
}



static void 
rec_layer_group (gint32 *layer)
{
  gint32 *children;
  gint num_children, j;
    
  if (gimp_item_is_group (*layer))
    {
      children = gimp_item_get_children (*layer, &num_children);

      for (j = 0; j < num_children; j++)
        rec_layer_group (&children[j]);
    }
  else
    {
      nlayers = nlayers + 1;
      layers[num++] = *layer;
    }

}

static void
cb_select_layers (GtkWidget *Widget,
                  gpointer   dialog)
{
  gint i;
  GtkWidget *window;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *scrolled_window;
  GtkWidget *hbox;

  window = gtk_dialog_new_with_buttons ("Select Layers", 
                                        GTK_WINDOW (dialog),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        NULL);
  button = gtk_dialog_add_button (GTK_DIALOG (window),
                                  "Done",
                                  GTK_RESPONSE_OK); 
  gtk_widget_set_sensitive (button, check_sensitive ()); 
  gtk_widget_set_size_request (window, 225, 350);

  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (scrolled_window), 5);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);

  gtk_box_pack_start (GTK_BOX ((gtk_dialog_get_content_area(GTK_DIALOG(window)))),       
                      scrolled_window, TRUE, TRUE, 0);

  table = gtk_table_new (nlayers, 2, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE (table), 1);
  gtk_table_set_col_spacings (GTK_TABLE (table), 1);
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled_window), table);

  for (i = 0; i < nlayers; i++)
    {    
      hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5); 
      gtk_table_attach (GTK_TABLE (table), hbox,
	                0, 1, i, i+1,
                        GTK_FILL, GTK_FILL,
                        15, 15);

      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
      gtk_box_pack_start (GTK_BOX(hbox), frame, FALSE, FALSE,0);
  
      layer_set[i].thumbnail = gtk_image_new();
      gtk_image_set_from_pixbuf (GTK_IMAGE (layer_set[i].thumbnail), 
                                gimp_drawable_get_thumbnail(layer_set[i].drawable_id,
                                40,40,
                                GIMP_PIXBUF_SMALL_CHECKS) );
      gtk_container_add (GTK_CONTAINER (frame), layer_set[i].thumbnail);
    
      layer_set[i].checkbox = gtk_check_button_new_with_label(layer_set[i].layer_name);
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layer_set[i].checkbox), layer_set[i].select);

      gtk_table_attach_defaults (GTK_TABLE (table), layer_set[i].checkbox,
                                 1, 2, i, i+1);
      g_signal_connect(layer_set[i].checkbox, "clicked", 
                       G_CALLBACK (cb_select), (gpointer) i);
    }
  
  gtk_widget_show_all (window);
  
  if (gtk_dialog_run (GTK_DIALOG (window)) == GTK_RESPONSE_OK)
    export_set.layers_select = TRUE;
 
  gtk_widget_destroy (window);
  export_button_check ();
}

static void
cb_layers (GtkWidget *widget, 
           gpointer   layers_button)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
    {
      gtk_widget_set_sensitive (layers_button, FALSE);
      select_all_layers ();
    }
  else
    {
      gtk_widget_set_sensitive (layers_button, TRUE);
      deselect_all_layers ();
    }
  export_button_check ();
}

static void
cb_select (GtkWidget *widget,
           gpointer   i)
{
  gint t = (gint) i;

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
    {
      gint t = (gint) i;
      layer_set[t].select = TRUE;
      gtk_widget_set_sensitive (button, check_sensitive());
    }
  else
    {
      layer_set[t].select = FALSE;
      gtk_widget_set_sensitive (button, check_sensitive());
    }
  export_button_check ();
}

static void
cb_format (GtkWidget *widget,
           gpointer   combo)
{
  switch (gtk_combo_box_get_active (GTK_COMBO_BOX (combo)))
    {
      case 0: 
        export_set.format_select = TRUE;
        format = g_strdup (format_set[0]);
        break;

      case 1:
        export_set.format_select = TRUE;
        format = g_strdup (format_set[1]);
        break;
   
      case 2:
        export_set.format_select = TRUE;
        format = g_strdup (format_set[2]);
        break;

      default:
        export_set.format_select = FALSE;
    }
  export_button_check ();
}

static void
cb_browse (GtkWidget        *widget,
           gpointer          dialog)
{
  GtkWidget *window;
  window = gtk_file_chooser_dialog_new ("Select Folder",
                                        GTK_WINDOW (dialog),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_OK,
                                        NULL);

  gtk_dialog_set_alternative_button_order (GTK_DIALOG (window),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);
  gtk_dialog_set_default_response (GTK_DIALOG (window), GTK_RESPONSE_OK);

  gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (window),
                                                  TRUE);


  gtk_window_present (GTK_WINDOW (window));  
  
  if (gtk_dialog_run (GTK_DIALOG (window)) == GTK_RESPONSE_OK)
    {
      raw_filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (window));
      export_set.uri_select = TRUE;
    }

  g_signal_connect (window, "destroy",
                    G_CALLBACK (gtk_widget_destroyed),
                    &window);
  gtk_widget_destroy (window);
  export_button_check ();
}

static gboolean
final_check ()
{
  if (export_set.format_select && export_set.uri_select && export_set.layers_select)
    return TRUE;
  else
    return FALSE;
}

static void
final_export ()
{
  gint i;
  gchar *path;

  for (i = 0; i < nlayers; i++)
    {
      GimpParam *return_vals;
      gint nreturn_vals;

      path = g_strconcat (raw_filename,
                          "-",
                          layer_set[i].layer_name,
                          ".",
                          format,
                          NULL);

      if (layer_set[i].select)
        {
           return_vals = gimp_run_procedure ("gimp-file-save",
                                            &nreturn_vals,
                                            GIMP_PDB_INT32,    1,
                                            GIMP_PDB_IMAGE,    image_ID,
                                            GIMP_PDB_DRAWABLE, layer_set[i].drawable_id,
                                            GIMP_PDB_STRING,   path,
                                            GIMP_PDB_STRING,   path,
                                            GIMP_PDB_END);
          
          gimp_destroy_params (return_vals, nreturn_vals);
        }

      g_free (path);     
    }  

}

static void
export_button_check ()
{
  if (final_check())
      gtk_widget_set_sensitive (export, TRUE);
  else
      gtk_widget_set_sensitive (export, FALSE);
}

static void 
create_combo_list (void)
{
  gint i;
  
  for (i = 0; i < format_count; i++)
    gtk_combo_box_append_text (GTK_COMBO_BOX (combo), format_set[i]);
}

static void 
select_all_layers ()
{
  gint i;
  export_set.layers_select = TRUE;
  for (i = 0; i < nlayers; i++)
    layer_set[i].select = TRUE;
}

static void
deselect_all_layers ()
{
  gint i;
  export_set.layers_select = FALSE;
  for (i = 0; i < nlayers; i++)
    layer_set[i].select = FALSE;
}


static void
init_layers ()
{
  gint i;
  layer_set = g_new0 (LayerData, nlayers);
  for (i = 0; i < nlayers; i++)
    {
      layer_set[i].drawable_id = layers[i];
      layer_set[i].layer_name = gimp_item_get_name (layer_set[i].drawable_id);
      layer_set[i].select = FALSE;
    }
}

static gboolean
check_sensitive ()
{
  gint i;
  
  for (i = 0; i < nlayers; i++)
    {
      if (layer_set[i].select)
        return TRUE;
    }
  return FALSE;
}

static void
destroy_all ()
{
  gimp_drawable_detach (main_drawable);
  g_free (layer_set);
  g_free (image_name);
  g_free (layers);
  g_free ((void*)format);
  g_free ((void*)raw_filename);
}
