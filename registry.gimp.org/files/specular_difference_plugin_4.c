/*
 * Specular Difference plug-in for GIMP.
 *
 * Copyright (C) 2011 PA Terry.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <libgimp/gimp.h>
#include <gtk-2.0/gtk/gtk.h>
#include <libgimpwidgets/gimpwidgets.h>
#include <libgimpwidgets/gimppreview.h>
#include <libgimp/gimpuitypes.h>
#include <libgimp/gimpdrawable.h>
#include <libgimp/gimpdrawablepreview.h>

char *sdp_help_info[] =
{
"Specular Difference plugin for GIMP.",

"(C) PA Terry 2011.",

"Released under the GNU GPL Version 3.",

"This plugin is for isolating specular light in a scene. It uses two photographs of the same\n\
scene - one filtered by a polariser and one unfiltered. The filtered image is subtracted\n\
from the unfiltered one, leaving largely specular light.\n\
\n\
To use this plugin, select the layers that contain the unfiltered and filtered images and\n\
then adjust the noise reduction and brightness settings as needed. It's important to\n\
ensure that both images are carefully aligned."
};

struct sdp_layer_config
{
  gint id;
  gint pos_x;
  gint pos_y;
  gint width;
  gint height;
  gint channels;
  gint alpha_channel;
  gint opacity;
};

struct sdp_config
{
  gint image_id;
  gint noise_reduction;
  gint brightness;
  gboolean subtract_alphas_set; 
  gboolean subtract_alphas_okay;
  gboolean use_opacities;
  struct sdp_layer_config drawable;
  struct sdp_layer_config primary_layer;
  struct sdp_layer_config subtraction_layer;
  GtkWidget *subtract_alphas_check_box;
  GimpPreview *preview;
};


/* Subtract one image from another, making the specified noise reduction and brightness
 * adjustments. This function does the processing for both the preview and the main
 * image.
 */
static void sdp_do_subtraction(struct sdp_config *sdc, 
                               GimpPixelRgn *primary_region_in, GimpPixelRgn *subtraction_region_in, 
                               GimpPixelRgn *drawable_region_in, GimpPixelRgn *drawable_region_out,
                               gboolean preview)
{
  guchar *primary_inrow, *subtraction_inrow;
  guchar *drawable_inrow, *drawable_outrow, val;
  gint primary_layer_row, primary_layer_column;
  gint subtraction_layer_row, subtraction_layer_column;
  gint primary_layer_fetch_position, primary_layer_fetch_adjustment, primary_layer_fetch_width;
  gint subtraction_layer_fetch_position, subtraction_layer_fetch_adjustment, subtraction_layer_fetch_width;
  gint primary_layer_offset_x, primary_layer_offset_y;
  gint subtraction_layer_offset_x, subtraction_layer_offset_y;
  guchar primary_layer_val, subtraction_layer_val, drawable_layer_val; 
  gint primary_layer_opacity, subtraction_layer_opacity;
  gboolean alpha_subtraction;
  gint i, j, k;
  guint tmp_1, tmp_2;

  /* The positioning is handled a little differently if we're drawing to a layer other than the primary
   * layer or the preview. In this case, we want to draw where the drawable overlaps the subtraction.
   */
  if (!preview && sdc->drawable.id != sdc->primary_layer.id) {
    primary_layer_offset_x = sdc->drawable.pos_x - sdc->primary_layer.pos_x;
    primary_layer_offset_y = sdc->drawable.pos_y - sdc->primary_layer.pos_y;
    subtraction_layer_offset_x = sdc->drawable.pos_x - sdc->subtraction_layer.pos_x;
    subtraction_layer_offset_y = sdc->drawable.pos_y - sdc->subtraction_layer.pos_y;
  } else {
    primary_layer_offset_x = sdc->drawable.pos_x;
    primary_layer_offset_y = sdc->drawable.pos_y;
    subtraction_layer_offset_x = sdc->primary_layer.pos_x + sdc->drawable.pos_x - sdc->subtraction_layer.pos_x;
    subtraction_layer_offset_y = sdc->primary_layer.pos_y + sdc->drawable.pos_y - sdc->subtraction_layer.pos_y;
  }

  /* Allocate memory for input and output tile rows. */
  primary_inrow = g_new(guchar, sdc->drawable.width * sdc->primary_layer.channels);
  subtraction_inrow = g_new(guchar, sdc->drawable.width * sdc->subtraction_layer.channels);
  drawable_inrow = g_new(guchar, sdc->drawable.width * sdc->drawable.channels);
  drawable_outrow = g_new(guchar, sdc->drawable.width * sdc->drawable.channels);

  /* Determine if the subtraction uses alpha layers. */
  if (sdc->subtract_alphas_set && sdc->subtract_alphas_okay &&
      sdc->drawable.alpha_channel == sdc->primary_layer.alpha_channel &&
      sdc->drawable.alpha_channel == sdc->subtraction_layer.alpha_channel)
  {
    alpha_subtraction = TRUE;
  } else {
    alpha_subtraction = FALSE;
  }

  /* Set layer opacities. */
  if (sdc->use_opacities) {
    primary_layer_opacity = sdc->primary_layer.opacity;
    subtraction_layer_opacity = sdc->subtraction_layer.opacity;
  } else {
    primary_layer_opacity = 255;
    subtraction_layer_opacity = 255;
  }

  for (i = 0; i < sdc->drawable.height; i++) {
    gimp_pixel_rgn_get_row(drawable_region_in, drawable_inrow, sdc->drawable.pos_x, 
                           i + sdc->drawable.pos_y, sdc->drawable.width);

    primary_layer_row = i + primary_layer_offset_y;
    subtraction_layer_row = i + subtraction_layer_offset_y;

    /* Write the row from the drawable layer if one of the following is true:
     *   a) The primary row is out of bounds.
     *   b) The first column of the primary row is past the end bound.
     *   c) The subtraction row is out of bounds.
     *   d) The first column of the subtraction row is past the end bound.
     */
    if (primary_layer_row < 0 || primary_layer_row >= sdc->primary_layer.height ||
        primary_layer_offset_x >= sdc->primary_layer.width ||
        subtraction_layer_row < 0 || subtraction_layer_row >= sdc->subtraction_layer.height ||
        subtraction_layer_offset_x >= sdc->subtraction_layer.width)
    {
      for (j = 0; j < sdc->drawable.width; j++) {
        for (k = 0; k < sdc->drawable.channels; k++) {
          drawable_outrow[(sdc->drawable.channels * j) + k] = drawable_inrow[(sdc->drawable.channels * j) + k];
        }
      }
    } else {
      /* The primary layer x offset may be negative and unlike a negative y offset
       * the whole row can't be ignored. In this case, adjust the fetch position to
       * zero and keep track of the adjustment that was made.
       */
      if (primary_layer_offset_x < 0) {
        primary_layer_fetch_adjustment = -primary_layer_offset_x;
      } else {
        primary_layer_fetch_adjustment = 0;
      }
      /* Ensure that the start and end points for get_row() are within bounds.
       */
      primary_layer_fetch_position = primary_layer_offset_x + primary_layer_fetch_adjustment;
      if (primary_layer_fetch_position + sdc->drawable.width > sdc->primary_layer.width) {
        primary_layer_fetch_width = sdc->primary_layer.width - primary_layer_fetch_position;
      } else {
        primary_layer_fetch_width = sdc->drawable.width;
      }
      /* Fetch the row.
       */
      gimp_pixel_rgn_get_row(primary_region_in, primary_inrow, primary_layer_fetch_position, 
                             primary_layer_row, primary_layer_fetch_width);
      
      /* 
       * Same again for the subtraction layer. 
       */
      if (subtraction_layer_offset_x < 0) {
        subtraction_layer_fetch_adjustment = -subtraction_layer_offset_x;
      } else {
        subtraction_layer_fetch_adjustment = 0;
      }
      subtraction_layer_fetch_position = subtraction_layer_offset_x + subtraction_layer_fetch_adjustment;
      if (subtraction_layer_fetch_position + sdc->drawable.width > sdc->subtraction_layer.width) {
        subtraction_layer_fetch_width = sdc->subtraction_layer.width - subtraction_layer_fetch_position;
      } else {
        subtraction_layer_fetch_width = sdc->drawable.width;
      }
      gimp_pixel_rgn_get_row(subtraction_region_in, subtraction_inrow, subtraction_layer_fetch_position, 
                             subtraction_layer_row, subtraction_layer_fetch_width);

      /* Determine if the subtraction uses alpha layers. */
      
      for (j = 0; j < sdc->drawable.width; j++) {

        primary_layer_column = j + primary_layer_offset_x;
        subtraction_layer_column = j + subtraction_layer_offset_x;

        /* If the translated column is out of bounds, write the pixel from the drawable layer.
         */
        if (primary_layer_column < 0 || primary_layer_column >= sdc->primary_layer.width ||
            subtraction_layer_column < 0 || subtraction_layer_column >= sdc->subtraction_layer.width) 
        {
          for (k = 0; k < sdc->drawable.channels; k++) {
            drawable_outrow[(sdc->drawable.channels * j) + k] = drawable_inrow[(sdc->drawable.channels * j) + k];
          }
        } else {
          /* Translation is in bounds so we can safely subtract.
           */
          // for (k = 0; k < sdc->drawable.channels; k++) {
          for (k = sdc->drawable.channels - 1; k >= 0; k--) {
            if (k < sdc->primary_layer.channels && k < sdc->subtraction_layer.channels)
            {
              primary_layer_val = primary_inrow[(sdc->primary_layer.channels * (j - primary_layer_fetch_adjustment)) + k];
              subtraction_layer_val = subtraction_inrow[(sdc->subtraction_layer.channels * (j - subtraction_layer_fetch_adjustment)) + k];

              if (k != sdc->drawable.alpha_channel || alpha_subtraction) {

                tmp_1 = (guint) ((primary_layer_val + 255 - sdc->noise_reduction) * (primary_layer_opacity)) / 255;
                tmp_2 = (guint) (subtraction_layer_val * subtraction_layer_opacity) / 255;
                if (tmp_1 > tmp_2) {
                  drawable_layer_val = (guchar) (tmp_1 - tmp_2);
                } else {
                  drawable_layer_val = 0;
                }

                tmp_1 = (guint) ((drawable_layer_val * sdc->brightness) / 100);
                if (tmp_1 < 256) {
                  drawable_layer_val = (guchar) tmp_1;
                } else {
                  drawable_layer_val = 255;
                } 
              } else {
                drawable_layer_val = 255;
              }
            } else {
              drawable_layer_val = drawable_inrow[(sdc->drawable.channels * j) + k];
            }
            drawable_outrow[(sdc->drawable.channels * j) + k] = drawable_layer_val;
          }
        }
      }
    }

    gimp_pixel_rgn_set_row(drawable_region_out, drawable_outrow, sdc->drawable.pos_x, 
                           i + sdc->drawable.pos_y, sdc->drawable.width);

    if (!preview && i % 10 == 0) {
      gimp_progress_update((gdouble) i / (gdouble) sdc->drawable.height);
    }
  }

  g_free(primary_inrow);
  g_free(subtraction_inrow);
  g_free(drawable_inrow);
  g_free(drawable_outrow);

  return;
}


/* 
 * Functions to validate and fill out the details of the layers and drawable.
 */
static gboolean sdp_validate_drawable(gint drawable_id)
{
  if (drawable_id == 0) {
    return(FALSE);
  }

  if (!gimp_drawable_is_valid(drawable_id)) {
    return(FALSE);
  }

  return(TRUE);
}


static gboolean sdp_validate_layer(struct sdp_layer_config *slc)
{
  if (!sdp_validate_drawable(slc->id)) {
    return(FALSE);
  }

  if (!gimp_drawable_offsets(slc->id, &(slc->pos_x), &(slc->pos_y))) {
    return(FALSE);
  }

  slc->width = gimp_drawable_width(slc->id);
  slc->height = gimp_drawable_height(slc->id);
  slc->channels = gimp_drawable_bpp(slc->id);
  slc->opacity = (gint) ((gimp_layer_get_opacity(slc->id) * 255.0) / 100.0);

  if (gimp_drawable_has_alpha(slc->id)) {
    if (gimp_drawable_is_gray(slc->id)) {
      slc->alpha_channel = 1;
    } else if (gimp_drawable_is_rgb(slc->id)) {
      slc->alpha_channel = 3;
    } else {
      slc->alpha_channel = -1;
    }
  } else {
    slc->alpha_channel = -1;
  }

  return(TRUE);
}


static gboolean sdp_validate_subtraction(struct sdp_config *sdc)
{
  gint primary_layer_has_alpha = 0; 
  gint subtraction_layer_has_alpha = 0;

  if (!sdp_validate_layer(&(sdc->primary_layer))) {
    return(FALSE);
  }
  if (!sdp_validate_layer(&(sdc->subtraction_layer))) {
    return(FALSE);
  }
  if (!sdp_validate_layer(&(sdc->drawable))) {
    return(FALSE);
  }

  if (sdc->primary_layer.id == sdc->subtraction_layer.id) {
    return(FALSE);
  }

  if (sdc->primary_layer.alpha_channel > -1) {
    primary_layer_has_alpha = 1;
  }
  if (sdc->subtraction_layer.alpha_channel > -1) {
    subtraction_layer_has_alpha = 1;
  }
  if (sdc->primary_layer.channels - primary_layer_has_alpha != sdc->subtraction_layer.channels - subtraction_layer_has_alpha) {
    g_message("Error: both layers must have the same number of channels.");
    return(FALSE);
  }

  return(TRUE);
}


/*
 * Subtract the secondary layer from the primary layer in a preview window.
 */
static void sdp_subtract_layer_preview(GimpPreview *preview, struct sdp_config *sdc)
{
  gint x1, y1, width, height;
  GimpDrawable *drawable, *primary_layer, *subtraction_layer;
  GimpPixelRgn primary_region_in, subtraction_region_in, drawable_region_in, drawable_region_out;

  if (!sdp_validate_subtraction(sdc)) {
    return;
  }

  gimp_preview_get_position(preview, &(sdc->drawable.pos_x), &(sdc->drawable.pos_y));
  gimp_preview_get_size(preview, &(sdc->drawable.width), &(sdc->drawable.height));

  drawable = gimp_drawable_get(sdc->drawable.id);
  primary_layer = gimp_drawable_get(sdc->primary_layer.id);
  subtraction_layer = gimp_drawable_get(sdc->subtraction_layer.id);

  /* Allocate a tile cache for each GimpPixelRgn. */
  gimp_tile_cache_ntiles(4 * (drawable->width / gimp_tile_width() + 1));

  /* Initialise read GimpPixelRgns for the primary, subtraction and drawable layers.
   * Initialise a write region for the drawable.
   */
  gimp_pixel_rgn_init(&primary_region_in, primary_layer, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&subtraction_region_in, subtraction_layer, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&drawable_region_in, drawable, sdc->drawable.pos_x, sdc->drawable.pos_y,
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&drawable_region_out, drawable, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, TRUE, TRUE);

  sdp_do_subtraction(sdc, &primary_region_in, &subtraction_region_in, &drawable_region_in, &drawable_region_out, TRUE);

  gimp_drawable_preview_draw_region(GIMP_DRAWABLE_PREVIEW(preview), &drawable_region_out);

  return;
}


/* Subtract the secondary layer from the primary layer, 
 * writing the result to the current layer.
 */ 
static void sdp_subtract_layer(struct sdp_config *sdc)
{
  gint x1, y1, x2, y2, width, height;
  GimpDrawable *drawable, *primary_layer, *subtraction_layer;
  GimpPixelRgn primary_region_in, subtraction_region_in, drawable_region_in, drawable_region_out;

  if (!sdp_validate_subtraction(sdc)) {
    return;
  }

  gimp_progress_init("Specular difference...");

  gimp_drawable_mask_bounds(sdc->drawable.id, &x1, &y1, &x2, &y2);
  sdc->drawable.pos_x = x1;
  sdc->drawable.pos_y = y1;
  sdc->drawable.width = x2 - x1;
  sdc->drawable.height = y2 - y1;

  drawable = gimp_drawable_get(sdc->drawable.id);
  primary_layer = gimp_drawable_get(sdc->primary_layer.id);
  subtraction_layer = gimp_drawable_get(sdc->subtraction_layer.id);

  /* Allocate a tile cache for each GimpPixelRgn. */
  gimp_tile_cache_ntiles(4 * (drawable->width / gimp_tile_width() + 1));

  /* Initialise read GimpPixelRgns for the primary, subtraction and drawable layers.
   * Initialise a write region for the drawable, which will be merged later with a
   * call to gimp_drawable_merge_shadow().
   */
  gimp_pixel_rgn_init(&primary_region_in, primary_layer, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&subtraction_region_in, subtraction_layer, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&drawable_region_in, drawable, sdc->drawable.pos_x, sdc->drawable.pos_y,
                      sdc->drawable.width, sdc->drawable.height, FALSE, FALSE);
  gimp_pixel_rgn_init(&drawable_region_out, drawable, sdc->drawable.pos_x, sdc->drawable.pos_y, 
                      sdc->drawable.width, sdc->drawable.height, TRUE, TRUE);

  sdp_do_subtraction(sdc, &primary_region_in, &subtraction_region_in, &drawable_region_in, &drawable_region_out, FALSE);

  /* Update the modified region. */
  gimp_drawable_flush(drawable);
  gimp_drawable_merge_shadow(drawable->drawable_id, TRUE);
  gimp_drawable_free_shadow(drawable->drawable_id);
  gimp_drawable_update(drawable->drawable_id, sdc->drawable.pos_x, sdc->drawable.pos_y, sdc->drawable.width, sdc->drawable.height);

  return;
}


/* 
 * Functions to handle UI events.
 */
static void sdp_primary_layer_changed(GtkComboBox *combo, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;
  gint layer_index, n_layers, *layer_ids;

  if (!gimp_image_is_valid(sdc->image_id)) {
    g_message("Error: invalid image.");
    return;
  }

  layer_ids = gimp_image_get_layers(sdc->image_id, &n_layers);

  if (n_layers < 1) {
    g_message("Error: image has no layers");
    return;
  }

  layer_index = gtk_combo_box_get_active(combo) - 1;

  if (layer_index > n_layers) {
    g_message("Error: a layer has been deleted");
    return;
  }

  if (layer_index < 0) {
    g_message("Error: unable to determine active layer");
    return;
  }

  sdc->primary_layer.id = layer_ids[layer_index];

  sdp_validate_layer(&(sdc->primary_layer));

  if (sdc->subtraction_layer.id != 0) {

    sdp_validate_layer(&(sdc->subtraction_layer));

    if (sdc->primary_layer.alpha_channel > -1 &&
        sdc->primary_layer.alpha_channel == sdc->subtraction_layer.alpha_channel)
    {
      sdc->subtract_alphas_okay = TRUE;
    }
  }

  if (sdc->subtract_alphas_okay == TRUE) {
    gtk_widget_set_sensitive(sdc->subtract_alphas_check_box, TRUE);
  } else {
    gtk_widget_set_sensitive(sdc->subtract_alphas_check_box, FALSE);
  }

  gimp_preview_invalidate(sdc->preview);

  g_free(layer_ids);

  return;
}
 

static void sdp_subtraction_layer_changed(GtkComboBox *combo, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;
  gint layer_index, n_layers, *layer_ids;

  if (!gimp_image_is_valid(sdc->image_id)) {
    g_message("Error: invalid image.");
    return;
  }

  layer_ids = gimp_image_get_layers(sdc->image_id, &n_layers);

  if (n_layers < 1) {
    g_message("Error: image has no layers");
    return;
  }

  layer_index = gtk_combo_box_get_active(combo) - 1;

  if (layer_index > n_layers) {
    g_message("Error: a layer has been deleted");
    return;
  }

  if (layer_index < 0) {
    g_message("Error: unable to determine active layer");
    return;
  }

  sdc->subtraction_layer.id = layer_ids[layer_index];

  sdp_validate_layer(&(sdc->subtraction_layer));

  if (sdc->primary_layer.id != 0) {

    sdp_validate_layer(&(sdc->primary_layer));

    if (sdc->primary_layer.alpha_channel > -1 &&
        sdc->primary_layer.alpha_channel == sdc->subtraction_layer.alpha_channel)
    {
      sdc->subtract_alphas_okay = TRUE;
    }
  }

  if (sdc->subtract_alphas_okay == TRUE) {
    gtk_widget_set_sensitive(sdc->subtract_alphas_check_box, TRUE);
  } else {
    gtk_widget_set_sensitive(sdc->subtract_alphas_check_box, FALSE);
  }

  gimp_preview_invalidate(sdc->preview);

  g_free(layer_ids);

  return;
}


static void sdp_noise_reduction_changed(GtkAdjustment *adj, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;

  sdc->noise_reduction = (int) ((adj->value * 255.0) / 100.0);

  gimp_preview_invalidate(sdc->preview);

  return;
}


static void sdp_brightness_changed(GtkAdjustment *adj, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;

  sdc->brightness = (int) adj->value;

  gimp_preview_invalidate(sdc->preview);

  return;
}


static void sdp_subtract_alphas_toggled(GtkToggleButton *subtract_alphas, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;

  if (gtk_toggle_button_get_active(subtract_alphas)) {

    sdc->subtract_alphas_set = TRUE;

    if (sdc->primary_layer.alpha_channel > -1 &&
        sdc->primary_layer.alpha_channel == sdc->subtraction_layer.alpha_channel)
    {
      sdc->subtract_alphas_okay = TRUE;
      gimp_preview_invalidate(sdc->preview);
    }

  } else {
    sdc->subtract_alphas_set = FALSE;
    gimp_preview_invalidate(sdc->preview);
  }

  return;
}


static void sdp_use_opacities_toggled(GtkToggleButton *use_opacities, void *up)
{
  struct sdp_config *sdc = (struct sdp_config *) up;

  if (gtk_toggle_button_get_active(use_opacities)) {
    sdc->use_opacities = TRUE;
  } else {
    sdc->use_opacities = FALSE;
  }

  gimp_preview_invalidate(sdc->preview);

  return;
}


/* 
 * Help dialog box.
 */
void sdp_help_dialog(const gchar *help_id, gpointer help_data)
{
  GtkWidget *help_popup, *close_button, *main_table;
  GtkWidget *title_label, *copyright_label, *license_label, *description_label;;

  help_popup = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(help_popup), "Help");

  main_table = gtk_table_new(0, 4, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(main_table), 2);
  gtk_table_set_col_spacings(GTK_TABLE(main_table), 2);
  gtk_widget_show(main_table);
  gtk_container_add(GTK_CONTAINER(help_popup), main_table);

  title_label = gtk_label_new(sdp_help_info[0]);
  gtk_misc_set_padding(GTK_MISC(title_label), 15, 15);
  gtk_widget_show(title_label);
  gtk_table_attach(GTK_TABLE(main_table), title_label, 0, 1, 0, 1, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  copyright_label = gtk_label_new(sdp_help_info[1]);
  gtk_misc_set_padding(GTK_MISC(copyright_label), 10, 5);
  gtk_widget_show(copyright_label);
  gtk_table_attach(GTK_TABLE(main_table), copyright_label, 0, 1, 1, 2, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  license_label = gtk_label_new(sdp_help_info[2]);
  gtk_misc_set_padding(GTK_MISC(license_label), 10, 5);
  gtk_widget_show(license_label);
  gtk_table_attach(GTK_TABLE(main_table), license_label, 0, 1, 2, 3, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  description_label = gtk_label_new(sdp_help_info[3]);
  gtk_misc_set_padding(GTK_MISC(description_label), 10, 10);
  gtk_widget_show(description_label);
  gtk_table_attach(GTK_TABLE(main_table), description_label, 0, 1, 3, 4, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  close_button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
  gtk_container_set_border_width(GTK_CONTAINER(close_button), 15);
  gtk_widget_show(close_button);
  g_signal_connect_swapped(G_OBJECT(close_button), "clicked", G_CALLBACK(gtk_widget_destroy), (gpointer) help_popup);

  gtk_table_attach(GTK_TABLE(main_table), close_button, 0, 1, 4, 5, GTK_SHRINK, GTK_SHRINK, 0, 0);

  gtk_widget_show_all(help_popup);

  return;
}


/* 
 * Main dialog box.
 */
static gboolean sdp_main_dialog(struct sdp_config *sdc)
{
  GtkWidget *dialog, *main_vbox, *frame_1, *frame_2, *frame_3;
  GtkWidget *preview;
  GtkWidget *main_table, *table_1, *table_2, *table_3;
  GtkWidget *brightness_label, *brightness_label_align, *brightness_scale;
  GtkWidget *noise_reduction_label, *noise_reduction_label_align, *noise_reduction_scale;
  GtkObject *brightness_scale_adjustment, *noise_reduction_scale_adjustment;
  GtkWidget *primary_layer_combo_box, *subtraction_layer_combo_box;
  GtkWidget *subtract_alphas_check_box, *use_opacities_check_box;
  GimpDrawable *drawable;
  gint *layer_ids, n_layers, i;
  gchar *layer_name;
  gboolean ret;
  GtkWidget *help_popup;

  if (!gimp_image_is_valid(sdc->image_id)) {
    g_message("Error: invalid image.");
    return(FALSE);
  }

  if (!gimp_drawable_is_valid(sdc->drawable.id)) {
    g_message("Error: invalid drawable.");
    return(FALSE);
  }

  drawable = gimp_drawable_get(sdc->drawable.id);

  layer_ids = gimp_image_get_layers(sdc->image_id, &n_layers);

  /* Set up the standard dialogue box given by GIMP.
   */
  gimp_ui_init("specular-difference", FALSE);

  dialog = gimp_dialog_new("Specular Difference", "specular-difference", NULL, 0,
                           sdp_help_dialog, "plug-in-specular-difference",
                           GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                           GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);

  gimp_window_set_transient(GTK_WINDOW(dialog));

  main_vbox = gtk_vbox_new(FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), main_vbox);
  gtk_widget_show(main_vbox);

  /* Preview.
   */
  preview = gimp_drawable_preview_new(drawable, NULL);
  gtk_box_pack_start(GTK_BOX(main_vbox), preview, TRUE, TRUE, 0);
  gtk_widget_show(preview);
  sdc->preview = GIMP_PREVIEW(preview);

  /* The primary and subtraction layer pickers are put into a 2x1 table.
   * Note - counting rows and columns in gtk_table_new begins at 0.
   */
  frame_1 = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame_1), GTK_SHADOW_NONE);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame_1, FALSE, FALSE, 0);
  gtk_widget_show(frame_1);
 
  table_1 = gtk_table_new(1, 0, TRUE); 
  gtk_table_set_row_spacings(GTK_TABLE(table_1), 5);
  gtk_table_set_col_spacings(GTK_TABLE(table_1), 5);
  gtk_widget_show(table_1);
  gtk_container_add(GTK_CONTAINER(frame_1), table_1);

  /* Unfiltered layer combo box.
   */
  primary_layer_combo_box = gtk_combo_box_new_text();
  gtk_combo_box_insert_text(GTK_COMBO_BOX(primary_layer_combo_box), 0, "Unfiltered Layer");
  for (i = 0; i < n_layers; i++) {
    layer_name = gimp_drawable_get_name(layer_ids[i]);
    gtk_combo_box_insert_text(GTK_COMBO_BOX(primary_layer_combo_box), i + 1, layer_name);
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(primary_layer_combo_box), 0);
  gtk_widget_show(primary_layer_combo_box);
  gtk_table_attach(GTK_TABLE(table_1), primary_layer_combo_box, 0, 1, 0, 1, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  /* Filtered layer combo box.
   */
  subtraction_layer_combo_box = gtk_combo_box_new_text();
  gtk_combo_box_insert_text(GTK_COMBO_BOX(subtraction_layer_combo_box), 0, "Filtered Layer");
  for (i = 0; i < n_layers; i++) {
    layer_name = gimp_drawable_get_name(layer_ids[i]);
    gtk_combo_box_insert_text(GTK_COMBO_BOX(subtraction_layer_combo_box), i + 1, layer_name);
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(subtraction_layer_combo_box), 0);
  gtk_widget_show(subtraction_layer_combo_box);
  gtk_table_attach(GTK_TABLE(table_1), subtraction_layer_combo_box, 1, 2, 0, 1, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  /* Noise reduction and brightness labels and sliders.
   *
   * We'll put these in a 2x2 table. Each label gets aligned in the centre of its cell, which
   * looks ugly. The fix for this is to put the label into a GtkAlignment widget which is
   * then put into the table, rather than putting the label in directly. This makes it 
   * possible to align text on the left.
   *
   * The GtkScale widgets need GTK_FILL|GTK_EXPAND to work correctly, otherwise they'll be
   * sized as small as possible in the table making them unusable.
   */
  frame_2 = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame_2), GTK_SHADOW_NONE);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame_2, FALSE, FALSE, 0);
  gtk_widget_show(frame_2);

  table_2 = gtk_table_new(1, 1, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table_2), 5);
  gtk_table_set_col_spacings(GTK_TABLE(table_2), 5);
  gtk_widget_show(table_2);
  gtk_container_add(GTK_CONTAINER(frame_2), table_2);

  /* Noise reduction label and slider.
   */
  noise_reduction_label = gtk_label_new("Noise Reduction:");
  gtk_widget_show(noise_reduction_label);
  noise_reduction_label_align = gtk_alignment_new(0.0, 0.5, 0.0, 0.0);
  gtk_container_add(GTK_CONTAINER(noise_reduction_label_align), noise_reduction_label);
  gtk_widget_show(noise_reduction_label_align);
  gtk_table_attach(GTK_TABLE(table_2), noise_reduction_label_align, 0, 1, 0, 1, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  noise_reduction_scale_adjustment = gtk_adjustment_new(100.0, 0.0, 100.0, 1.0, 1.0, 0.0);
  noise_reduction_scale = gtk_hscale_new(GTK_ADJUSTMENT(noise_reduction_scale_adjustment));
  gtk_scale_set_digits(GTK_SCALE(noise_reduction_scale), 0);
  gtk_scale_set_value_pos(GTK_SCALE(noise_reduction_scale), GTK_POS_LEFT);
  gtk_widget_show(noise_reduction_scale);
  gtk_table_attach(GTK_TABLE(table_2), noise_reduction_scale, 1, 2, 0, 1, GTK_FILL|GTK_EXPAND|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  /* Brightness label and slider. 
   */
  brightness_label = gtk_label_new("Brightness:");
  gtk_widget_show(brightness_label);
  brightness_label_align = gtk_alignment_new(0.0, 0.5, 0.0, 0.0);
  gtk_container_add(GTK_CONTAINER(brightness_label_align), brightness_label);
  gtk_widget_show(brightness_label_align);
  gtk_table_attach(GTK_TABLE(table_2), brightness_label_align, 0, 1, 1, 2, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  brightness_scale_adjustment = gtk_adjustment_new(100.0, 100.0, 500.0, 1.0, 1.0, 0.0);
  brightness_scale = gtk_hscale_new(GTK_ADJUSTMENT(brightness_scale_adjustment));
  gtk_scale_set_digits(GTK_SCALE(brightness_scale), 0);
  gtk_scale_set_value_pos(GTK_SCALE(brightness_scale), GTK_POS_LEFT);
  gtk_widget_show(brightness_scale);
  gtk_table_attach(GTK_TABLE(table_2), brightness_scale, 1, 2, 1, 2, GTK_FILL|GTK_EXPAND|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  /* Subtract alphas and layer opacities tick boxes.
   */
  frame_3 = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame_3), GTK_SHADOW_NONE);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame_3, FALSE, FALSE, 0);
  gtk_widget_show(frame_3);

  table_3 = gtk_table_new(0, 1, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table_3), 5);
  gtk_table_set_col_spacings(GTK_TABLE(table_3), 5);
  gtk_widget_show(table_3);
  gtk_container_add(GTK_CONTAINER(frame_3), table_3);

  subtract_alphas_check_box = gtk_check_button_new_with_label("Subtract Alpha Channels");
  gtk_widget_set_sensitive(subtract_alphas_check_box, FALSE);
  gtk_widget_show(subtract_alphas_check_box);
  gtk_table_attach(GTK_TABLE(table_3), subtract_alphas_check_box, 0, 1, 0, 1, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  use_opacities_check_box = gtk_check_button_new_with_label("Use Layer Opacities");
  gtk_widget_set_sensitive(use_opacities_check_box, TRUE);
  gtk_widget_show(use_opacities_check_box);
  gtk_table_attach(GTK_TABLE(table_3), use_opacities_check_box, 0, 1, 1, 2, GTK_FILL|GTK_SHRINK, GTK_FILL|GTK_SHRINK, 0, 0);

  /* Wire up the UI.
   */
  g_signal_connect(preview, "invalidated", G_CALLBACK(sdp_subtract_layer_preview), sdc);
  g_signal_connect(G_OBJECT(primary_layer_combo_box), "changed", G_CALLBACK(sdp_primary_layer_changed), sdc);
  g_signal_connect(G_OBJECT(subtraction_layer_combo_box), "changed", G_CALLBACK(sdp_subtraction_layer_changed), sdc);
  g_signal_connect(G_OBJECT(noise_reduction_scale_adjustment), "value_changed", G_CALLBACK(sdp_noise_reduction_changed), sdc);
  g_signal_connect(G_OBJECT(brightness_scale_adjustment), "value_changed", G_CALLBACK(sdp_brightness_changed), sdc);
  g_signal_connect(G_OBJECT(subtract_alphas_check_box), "toggled", G_CALLBACK(sdp_subtract_alphas_toggled), sdc);
  g_signal_connect(G_OBJECT(use_opacities_check_box), "toggled", G_CALLBACK(sdp_use_opacities_toggled), sdc);

  /* Update the preview.
   */
  sdp_subtract_layer_preview(GIMP_PREVIEW(preview), sdc);

  /* Tidy up and return.
   */
  gtk_widget_show(dialog);

  sdc->subtract_alphas_check_box = subtract_alphas_check_box;

  ret = (gimp_dialog_run(GIMP_DIALOG(dialog)) == GTK_RESPONSE_OK);

  gtk_widget_destroy(dialog);

  return(ret);
}


/* This function gets called by GIMP to initialise the plugin.
 */
static void query(void)
{
  static GimpParamDef args[] = {
                                 { GIMP_PDB_INT32,    "run-mode", "Run mode" },
                                 { GIMP_PDB_IMAGE,    "image",    "Input image" },
                                 { GIMP_PDB_DRAWABLE, "drawable", "Input drawable" }
                               };

  gimp_install_procedure("plug-in-specular-difference",
                         "Specular Difference",
                         "Subtracts one layer from another to extract specular light",
                         "PA Terry",
                         "PA Terry",
                         "2011",
                         "_Specular Difference",
                         "RGB*, GRAY*",
                         GIMP_PLUGIN,
                         G_N_ELEMENTS(args),
                         0,
                         args,
                         NULL);

  if (!gimp_plugin_menu_register("plug-in-specular-difference", "<Image>/Filters/Misc")) {
    g_message("Error: unable to register plugin");
  }

  return;
}


/* This function gets called by GIMP every time the plugin is run.
 * 
 * The parameters passed for arg 3 are the run mode, image ID and drawable
 * ID respectively. They can be accessed using:
 *
 *   param[0].data.d_int32
 *   param[1].data.d_image
 *   param[2].data.d_drawable
 */
static void run(const gchar *name,
                gint nparams,
                const GimpParam *param,
                gint *nreturn_vals,
                GimpParam **return_vals)
{
  struct sdp_config sdc;
  static GimpParam values[1];
  GimpPDBStatusType status = GIMP_PDB_SUCCESS;
  GimpRunMode run_mode;
  GimpDrawable *drawable;

  /* Initialise config.
   */
  sdc.image_id = param[1].data.d_image;
  sdc.drawable.id = param[2].data.d_drawable;
  sdc.noise_reduction = 255;
  sdc.brightness = 100;
  sdc.primary_layer.id = 0;
  sdc.subtraction_layer.id = 0;
  sdc.subtract_alphas_set = FALSE;
  sdc.subtract_alphas_okay = FALSE;

  /* Set return values. 
   */
  values[0].type = GIMP_PDB_STATUS; /* Type: GimpPDBArgType */
  values[0].data.d_status = status; /* Type: GimpPDBStatusType */

  *nreturn_vals = 1;
  *return_vals = values;

  /* Get run mode to determine if we're being called interactively
   * or from a script.
   */
  run_mode = param[0].data.d_int32;

  switch (run_mode) {
    case GIMP_RUN_INTERACTIVE:
      /* Get plugin config.*/
      gimp_get_data("plug-in-specular-difference", &sdc);

      /* Display the dialog. */
      if (!sdp_main_dialog(&sdc)) {
        status = GIMP_PDB_EXECUTION_ERROR;
        return;
      }
      break;

    case GIMP_RUN_NONINTERACTIVE:
      if (nparams != 4) {
        status = GIMP_PDB_CALLING_ERROR;
        return;
      }
      break;

    case GIMP_RUN_WITH_LAST_VALS:
      /* Get plugin config. */
      gimp_get_data("plug-in-specular-difference", &sdc);
      break;

    default:
      break;
  }

  /* Do the work.
   */
  sdp_subtract_layer(&sdc);

  /* Tidy up.
   */
  gimp_displays_flush();
  drawable = gimp_drawable_get(param[2].data.d_drawable);
  gimp_drawable_detach(drawable);

  return;
}


GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,    /* init_proc */
  NULL,    /* quit_proc */
  query,   /* query_proc */
  run,     /* run_proc */
};


MAIN();
