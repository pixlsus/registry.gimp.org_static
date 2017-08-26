/*
 * Copyright (C) 1999 Winston Chang
 *                    <winstonc@cs.wisc.edu>
 *                    <winston@stdout.org>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#define PLUG_IN_PROC    "elsamuko-temperature"
#define PLUG_IN_BINARY  "elsamuko-temperature"

#define N_(x) (x)
#define _(x) (x)
#define uchar unsigned char

#define SCALE_WIDTH   120
#define ENTRY_WIDTH     5
#define SCALE           1

/* Uncomment this line to get a rough estimate of how long the plug-in
 * takes to run.
 */

/*  #define TIMER  */

typedef struct
{
    gdouble temperature;
    gint angle;
    gint exp_comp;
} TemperatureParams;

typedef struct
{
    gboolean  run;
} TemperatureInterface;

struct RGB {
    uchar r;
    uchar g;
    uchar b;
};

struct YUV {
    float y;
    float u;
    float v;
};
int CLAMP255(int in) {
    if(in<0) return 0;
    else if(in>255) return 255;
    else return in;
};
void RGB2YUV(const RGB& rgb, YUV& yuv)
{
    yuv.y = 0.299*(float)rgb.r +
            0.587*(float)rgb.g +
            0.114*(float)rgb.b;
    yuv.v = (float)rgb.r - yuv.y;
    yuv.u = (float)rgb.b - yuv.y;
};

void YUV2RGB(const YUV& yuv, RGB& rgb)
{
    rgb.r = CLAMP255(yuv.v + yuv.y);
    rgb.b = CLAMP255(yuv.u + yuv.y);
    rgb.g = CLAMP255((yuv.y - 0.299*(float)rgb.r - 0.114*(float)rgb.b)/0.587);
};

/* local function prototypes */
static inline gint coord( gint i, gint j, gint k, gint channels, gint width ) {
    return channels*( width*i + j ) + k;
};

static void      query (void);
static void      run   (const gchar      *name,
                        gint              nparams,
                        const GimpParam  *param,
                        gint             *nreturn_vals,
                        GimpParam       **return_vals);

static void    temperature_region     (GimpPixelRgn   *srcPTR,
                                       GimpPixelRgn   *dstPTR,
                                       gint            bpp,
                                       gdouble         temperature,
                                       gint            angle,
                                       gint            exposure,
                                       gint            x,
                                       gint            y,
                                       gint            width,
                                       gint            height,
                                       gboolean        show_progress);

static void      temperature        (GimpDrawable   *drawable,
                                     gdouble         temperature,
                                     gint            angle,
                                     gint            exposure);

static gboolean  temperature_dialog (GimpDrawable   *drawable);
static void      preview_update      (GimpPreview    *preview);


/* create a few globals, set default values */
static TemperatureParams temperature_params =
{
    0.0, /* default temperature   */
    30.0, /* default angle   */
    0, /* default exposure compensation */
};

/* Setting PLUG_IN_INFO */
const GimpPlugInInfo PLUG_IN_INFO =
{
    NULL,  /* init_proc  */
    NULL,  /* quit_proc  */
    query, /* query_proc */
    run,   /* run_proc   */
};


MAIN ();

static void
query (void)
{
    static const GimpParamDef args[] =
    {
        { GIMP_PDB_INT32,    (gchar*)"run-mode",    (gchar*)"The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    (gchar*)"image",       (gchar*)"(unused)" },
        { GIMP_PDB_DRAWABLE, (gchar*)"drawable",    (gchar*)"Drawable to draw on" },
        { GIMP_PDB_FLOAT,    (gchar*)"temperature", (gchar*)"Color temperature [-1 ..1]" },
        { GIMP_PDB_INT32,    (gchar*)"angle",       (gchar*)"YUV angle [1 .. 89]" },
        { GIMP_PDB_INT32,    (gchar*)"exposure",    (gchar*)"Exposure compensation [-50 .. 50]" },
    };

    gimp_install_procedure (PLUG_IN_PROC,
                            N_((gchar*)"Color temperature"),
                            (gchar*)"Color temperature.",
                            (gchar*)"elsamuko <elsamuko@web.de>",
                            (gchar*)"elsamuko",
                            (gchar*)"2011",
                            N_((gchar*)"_Color temperature..."),
                            (gchar*)"RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS (args), 0,
                            args, NULL);

    gimp_plugin_menu_register (PLUG_IN_PROC, (gchar*)"<Image>/Colors");
}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
    static GimpParam   values[1];
    GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
    GimpDrawable      *drawable;
    GimpRunMode        run_mode;
#ifdef TIMER
    GTimer            *timer = g_timer_new ();
#endif

    run_mode = (GimpRunMode)param[0].data.d_int32;

    *return_vals  = values;
    *nreturn_vals = 1;

    values[0].type          = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    //INIT_I18N ();

    /*
     * Get drawable information...
     */
    drawable = gimp_drawable_get (param[2].data.d_drawable);
    gimp_tile_cache_ntiles (2 * MAX (drawable->width  / gimp_tile_width () + 1 ,
                                     drawable->height / gimp_tile_height () + 1));

    switch (run_mode)
    {
    case GIMP_RUN_INTERACTIVE:
        gimp_get_data (PLUG_IN_PROC, &temperature_params);
        /* Reset default values show preview unmodified */

        /* initialize pixel regions and buffer */
        if (! temperature_dialog (drawable))
            return;

        break;

    case GIMP_RUN_NONINTERACTIVE:
        if (nparams != 6)
        {
            status = GIMP_PDB_CALLING_ERROR;
        }
        else
        {
            temperature_params.temperature = param[3].data.d_float;

            /* make sure there are legal values */
            if ((temperature_params.temperature > 1.0) ||
                    (temperature_params.temperature < -1.0))
                status = GIMP_PDB_CALLING_ERROR;
        }
        break;

    case GIMP_RUN_WITH_LAST_VALS:
        gimp_get_data (PLUG_IN_PROC, &temperature_params);
        break;

    default:
        break;
    }

    if (status == GIMP_PDB_SUCCESS)
    {
        drawable = gimp_drawable_get (param[2].data.d_drawable);

        /* here we go */
        temperature (drawable,
                     temperature_params.temperature,
                     temperature_params.angle,
                     temperature_params.exp_comp);

        gimp_displays_flush ();

        /* set data for next use of filter */
        if (run_mode == GIMP_RUN_INTERACTIVE)
            gimp_set_data (PLUG_IN_PROC,
                           &temperature_params, sizeof (TemperatureParams));

        gimp_drawable_detach(drawable);
        values[0].data.d_status = status;
    }

#ifdef TIMER
    g_printerr ("%f seconds\n", g_timer_elapsed (timer, NULL));
    g_timer_destroy (timer);
#endif
}

static void
temperature (GimpDrawable *drawable,
             gdouble       temperature,
             gint          angle,
             gint          exp_comp)
{
    GimpPixelRgn srcPR, destPR;
    gint         x1, y1, x2, y2;
    gint         width, height;
    int i;

    /* initialize pixel regions */
    gimp_pixel_rgn_init (&srcPR, drawable,
                         0, 0, drawable->width, drawable->height, FALSE, FALSE);
    gimp_pixel_rgn_init (&destPR, drawable,
                         0, 0, drawable->width, drawable->height, TRUE, TRUE);

    /* Get the input */
    gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);
    width = x2-x1;
    height = y2-y1;

    temperature_region (&srcPR, &destPR, drawable->bpp,
                        temperature,
                        angle,
                        exp_comp,
                        x1, y1, width, height,
                        TRUE);

    gimp_drawable_flush (drawable);
    gimp_drawable_merge_shadow (drawable->drawable_id, TRUE);
    gimp_drawable_update (drawable->drawable_id, x1, y1, width, height);
}

/* Copy the GIMP pixel region to an OpenCV image,
 * perform the face detection and copy the image back
 */
static void
temperature_region (GimpPixelRgn *srcPR,
                    GimpPixelRgn *destPR,
                    gint          bpp,
                    gdouble       temperature,
                    gint          angle,
                    gint          exp_comp,
                    gint          x,
                    gint          y,
                    gint          width,
                    gint          height,
                    gboolean      show_progress)
{
    printf( "\nL%i: **** Begin of temperature: **** \n", __LINE__);

    gint  i,j;
    gint  value;
    RGB rgb;
    YUV yuv;

    //Initialise memory
    guchar *src = g_new( guchar, bpp * width * height );
    guchar *dest = g_new( guchar, bpp * width * height );

    printf( "L%i: Channels: %i\n", __LINE__, bpp );
    printf( "L%i: x:%i, y:%i\n", __LINE__, x, y);
    printf( "L%i: w:%i, h:%i\n", __LINE__, width, height);

    //Save pixel regions in arrays
    gimp_pixel_rgn_get_rect( srcPR,
                             src,
                             x, y,
                             width, height );
    gimp_pixel_rgn_get_rect( destPR,
                             dest,
                             x, y,
                             width, height );

    // iterate over image
    gdouble ufak = cos((gdouble)angle/180 * M_PI);
    gdouble vfak = sin((gdouble)angle/180 * M_PI);

    size_t size = width*height*bpp;
    for(size_t i=0; i<size; i+=bpp)
    {
        memcpy(&rgb, src+i, 3);
        RGB2YUV(rgb,yuv);
        yuv.u -= 50*ufak*temperature;
        yuv.v += 50*vfak*temperature;
        yuv.y += exp_comp;
        YUV2RGB(yuv,rgb);
        memcpy(dest+i, &rgb, 3);
    }
    
    // copy alpha
    if(bpp==4)
    {
        for(size_t i=3; i<size; i+=bpp)
            *(dest+i)=*(src+i);
    }

    //save result in array
    gimp_pixel_rgn_set_rect( destPR, dest, x, y, width, height );

    g_free (dest);
    g_free (src);
    printf( "L%i: **** End of temperature **** \n", __LINE__);
}

static gboolean
temperature_dialog (GimpDrawable *drawable)
{
    GtkWidget *dialog;
    GtkWidget *main_vbox;
    GtkWidget *preview;
    GtkWidget *table;
    GtkObject *adj;
    gboolean   run;

    gimp_ui_init (PLUG_IN_BINARY, TRUE);

    dialog = gimp_dialog_new (_("Temperature"), PLUG_IN_BINARY,
                              NULL, (GtkDialogFlags)0,
                              gimp_standard_help_func, PLUG_IN_PROC,

                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                              GTK_STOCK_OK,     GTK_RESPONSE_OK,

                              NULL);

    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
            GTK_RESPONSE_OK,
            GTK_RESPONSE_CANCEL,
            -1);

    gimp_window_set_transient (GTK_WINDOW (dialog));

    main_vbox = gtk_vbox_new (FALSE, 12);
    gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
    gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
                       main_vbox);
    gtk_widget_show (main_vbox);

    preview = gimp_drawable_preview_new (drawable, NULL);
    gtk_box_pack_start (GTK_BOX (main_vbox), preview, TRUE, TRUE, 0);
    gtk_widget_show (preview);

    g_signal_connect (preview, "invalidated",
                      G_CALLBACK (preview_update),
                      NULL);

    table = gtk_table_new (3, 3, FALSE);
    gtk_table_set_col_spacings (GTK_TABLE (table), 6);
    gtk_table_set_row_spacings (GTK_TABLE (table), 6);
    gtk_box_pack_start (GTK_BOX (main_vbox), table, FALSE, FALSE, 0);
    gtk_widget_show (table);

    adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 0,
                                _("_Temperature:"), SCALE_WIDTH, ENTRY_WIDTH,
                                temperature_params.temperature, -1.0, 1.0, 0.01, 0.1, 2,
                                TRUE, 0, 0,
                                NULL, NULL);

    g_signal_connect (adj, "value-changed",
                      G_CALLBACK (gimp_double_adjustment_update),
                      &temperature_params.temperature);
    g_signal_connect_swapped (adj, "value-changed",
                              G_CALLBACK (gimp_preview_invalidate),
                              preview);

    adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 1,
                                _("_Angle:"), SCALE_WIDTH, ENTRY_WIDTH,
                                temperature_params.angle, 1, 89, 1, 5, 0,
                                TRUE, 0, 0,
                                NULL, NULL);

    g_signal_connect (adj, "value-changed",
                      G_CALLBACK (gimp_int_adjustment_update),
                      &temperature_params.angle);
    g_signal_connect_swapped (adj, "value-changed",
                              G_CALLBACK (gimp_preview_invalidate),
                              preview);

    adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 2,
                                _("_Exposure Compensation:"), SCALE_WIDTH, ENTRY_WIDTH,
                                temperature_params.exp_comp,
                                -50, 50, 1.0, 10.0, 0,
                                TRUE, 0, 0,
                                NULL, NULL);

    g_signal_connect (adj, "value-changed",
                      G_CALLBACK (gimp_int_adjustment_update),
                      &temperature_params.exp_comp);
    g_signal_connect_swapped (adj, "value-changed",
                              G_CALLBACK (gimp_preview_invalidate),
                              preview);

    gtk_widget_show (dialog);

    run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);

    gtk_widget_destroy (dialog);

    return run;
}

static void
preview_update (GimpPreview *preview)
{
    GimpDrawable *drawable;
    gint          x, y;
    gint          width, height;
    // gint          border;
    GimpPixelRgn  srcPR;
    GimpPixelRgn  destPR;

    drawable =
        gimp_drawable_preview_get_drawable (GIMP_DRAWABLE_PREVIEW (preview));

    gimp_pixel_rgn_init (&srcPR, drawable,
                         0, 0, drawable->width, drawable->height, FALSE, FALSE);
    gimp_pixel_rgn_init (&destPR, drawable,
                         0, 0, drawable->width, drawable->height, TRUE, TRUE);

    gimp_preview_get_position (preview, &x, &y);
    gimp_preview_get_size (preview, &width, &height);

    temperature_region (&srcPR, &destPR, drawable->bpp,
                        temperature_params.temperature,
                        temperature_params.angle,
                        temperature_params.exp_comp,
                        x, y, width, height,
                        FALSE);

    gimp_pixel_rgn_init (&destPR, drawable, x, y, width, height, FALSE, TRUE);
    gimp_drawable_preview_draw_region (GIMP_DRAWABLE_PREVIEW (preview), &destPR);
}
