/*
 * Copyright (C) 1999 Winston Chang
 *                    <winstonc@cs.wisc.edu>
 *                    <winston@stdout.org>
 * Copyright (C) 2010 Johannes Hanika
 *                    Michael Munzert
 *                    elsamuko
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
#include <math.h>
#include <assert.h>
#include <string.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#define PLUG_IN_PROC    "elsamuko-saturation"
#define PLUG_IN_BINARY  "elsamuko-saturation"

#define N_(x) (x)
#define _(x) (x)

#define SCALE_WIDTH   120
#define ENTRY_WIDTH     5

/* Uncomment this line to get a rough estimate of how long the plug-in
 * takes to run.
 */

/*  #define TIMER  */

typedef struct {
    gint   method;
} SaturationParams;

typedef struct {
    gboolean  run;
} SaturationInterface;

/* local function prototypes */
static inline gint coord( gint i, gint j, gint k, gint channels, gint width ) {
    return channels * ( width * i + j ) + k;
}

#define EPSILON 0.001
#define VMAX 256.f

// 1. sat = (max-min);
inline float calcSaturation_A( const float r, const float g, const float b ) {
    float v = fmax( fmax( r, g ), b );

    if( v < EPSILON ) {
        return 0;
    }

    float x = fmin( fmin( r, g ), b );
    return ( v - x );
}

// HSV-like
// 2. sat = 255 * (max-min)/max;
inline float calcSaturation_B( const float r, const float g, const float b ) {
    float v = fmax( fmax( r, g ), b );

    if( v < EPSILON ) {
        return 0;
    }

    float x = fmin( fmin( r, g ), b );
    return VMAX * ( v - x ) / v;
}

//
// 3. sat = 255 * (max-min)/sum;
inline float calcSaturation_C( const float r, const float g, const float b ) {
    float v = fmax( fmax( r, g ), b );

    if( v < EPSILON ) {
        return 0;
    }

    float x = fmin( fmin( r, g ), b );
    return VMAX * ( v - x ) / ( r + g + b );
}

// HSL-like
// 4. sat = 255 * (max-min)/(max+min);
inline float calcSaturation_D( const float r, const float g, const float b ) {
    float v = fmax( fmax( r, g ), b );

    if( v < EPSILON ) {
        return 0;
    }

    float x = fmin( fmin( r, g ), b );
    return VMAX * ( v - x ) / ( v + x );
}

static void      query( void );
static void      run( const gchar*      name,
                      gint              nparams,
                      const GimpParam*  param,
                      gint*             nreturn_vals,
                      GimpParam**       return_vals );

static void      doSaturationRegion( GimpPixelRgn*   srcPTR,
                                     GimpPixelRgn*   dstPTR,
                                     const gint     bpp,
                                     const gint     x,
                                     const gint     y,
                                     const gint     width,
                                     const gint     height,
                                     const gboolean show_progress );

static void      doSaturation( GimpDrawable*   drawable );
static gboolean  showSaturationDialog( GimpDrawable*   drawable );
static void      preview_update( GimpPreview*    preview );


/* create a few globals, set default values */
static SaturationParams saturation_params = {
    0 /* default method  */
};

/* Setting PLUG_IN_INFO */
const GimpPlugInInfo PLUG_IN_INFO = {
    NULL,  /* init_proc  */
    NULL,  /* quit_proc  */
    query, /* query_proc */
    run,   /* run_proc   */
};


MAIN()

static void
query( void ) {
    static const GimpParamDef args[] = {
        { GIMP_PDB_INT32, ( gchar* )"run-mode", ( gchar* )"The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE, ( gchar* )"image", ( gchar* )"(unused)" },
        { GIMP_PDB_DRAWABLE, ( gchar* )"drawable", ( gchar* )"Drawable to draw on" },
        { GIMP_PDB_INT32, ( gchar* )"method", ( gchar* )"Method, 0 (max-min), 1 HSV (max-min)/max, 2 (max-min)/sum, 3 HSL (max-min)/(max+min)" }
    };

    gimp_install_procedure( PLUG_IN_PROC,
                            N_( "Different saturation calculation methods." ),
                            "Different saturation calculation methods.",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "2013",
                            N_( "_Calc Saturation..." ),
                            "RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS( args ), 0,
                            args, NULL );

    gimp_plugin_menu_register( PLUG_IN_PROC, "<Image>/Colors/Components" );
}

static void
run( const gchar*      name,
     gint              nparams,
     const GimpParam*  param,
     gint*             nreturn_vals,
     GimpParam**       return_vals ) {
    static GimpParam   values[1];
    GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
    GimpDrawable*      drawable;
    GimpRunMode        run_mode;
#ifdef TIMER
    GTimer*            timer = g_timer_new();
#endif

    ( void ) name;

    run_mode = ( GimpRunMode )param[0].data.d_int32;

    *return_vals  = values;
    *nreturn_vals = 1;

    values[0].type          = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    //INIT_I18N ();

    /*
     * Get drawable information...
     */
    drawable = gimp_drawable_get( param[2].data.d_drawable );

    switch( run_mode ) {
        case GIMP_RUN_INTERACTIVE:
            gimp_get_data( PLUG_IN_PROC, &saturation_params );
            /* Reset default values show preview unmodified */

            /* initialize pixel regions and buffer */
            if( ! showSaturationDialog( drawable ) ) {
                return;
            }

            break;

        case GIMP_RUN_NONINTERACTIVE:

            if( nparams != 4 ) {
                status = GIMP_PDB_CALLING_ERROR;
            } else {
                saturation_params.method      = param[3].data.d_int32;

                /* make sure there are legal values */
                if( ( saturation_params.method < 0 ) || ( saturation_params.method > 3 ) ) {
                    printf( "L%i: Wrong input \n", __LINE__ );
                    status = GIMP_PDB_CALLING_ERROR;
                }
            }

            break;

        case GIMP_RUN_WITH_LAST_VALS:
            gimp_get_data( PLUG_IN_PROC, &saturation_params );
            break;

        default:
            break;
    }

    if( status == GIMP_PDB_SUCCESS ) {
        drawable = gimp_drawable_get( param[2].data.d_drawable );

        /* here we go */
        doSaturation( drawable );

        gimp_displays_flush();

        /* set data for next use of filter */
        if( run_mode == GIMP_RUN_INTERACTIVE ) {
            gimp_set_data( PLUG_IN_PROC, &saturation_params, sizeof( SaturationParams ) );
        }

        gimp_drawable_detach( drawable );
        values[0].data.d_status = status;
    }

#ifdef TIMER
    g_printerr( "%f seconds\n", g_timer_elapsed( timer, NULL ) );
    g_timer_destroy( timer );
#endif
}

static void doSaturation( GimpDrawable* drawable ) {
    GimpPixelRgn srcPR, destPR;
    gint         x1, y1, x2, y2;

    /* initialize pixel regions */
    gimp_pixel_rgn_init( &srcPR, drawable,
                         0, 0, drawable->width, drawable->height, FALSE, FALSE );
    gimp_pixel_rgn_init( &destPR, drawable,
                         0, 0, drawable->width, drawable->height, TRUE, TRUE );

    /* Get the input */
    gimp_drawable_mask_bounds( drawable->drawable_id, &x1, &y1, &x2, &y2 );

    doSaturationRegion( &srcPR, &destPR, drawable->bpp,
                        x1, y1, x2 - x1, y2 - y1, TRUE );

    gimp_drawable_flush( drawable );
    gimp_drawable_merge_shadow( drawable->drawable_id, TRUE );
    gimp_drawable_update( drawable->drawable_id, x1, y1, x2 - x1, y2 - y1 );
}


/* Copy the selected region to a gint rgb array, run the saturation calculation
 * and copy it back to the selected region.
 */
static void doSaturationRegion( GimpPixelRgn*  srcPR,
                                GimpPixelRgn*  destPR,
                                const gint     bpp,
                                const gint     x,
                                const gint     y,
                                const gint     width,
                                const gint     height,
                                const gboolean show_progress ) {

    printf( "L%i: Saturation begin\n", __LINE__ );
    printf( "L%i: Channels: %i\n", __LINE__, bpp );
    printf( "L%i: x:%i, y:%i\n", __LINE__, x, y );
    printf( "L%i: w:%i, h:%i\n", __LINE__, width, height );

    guchar*     src  = 0;
    guchar*     dest = 0;
    int i = 0;
    int j = 0;
    gint error = FALSE;

    src   = g_new( guchar, bpp * width * height );
    dest  = g_new( guchar, bpp * width * height );

    gimp_pixel_rgn_get_rect( srcPR, src, x, y, width, height );

    if( show_progress ) {
        gimp_progress_init( _( "Calculating saturation..." ) );
    }

    // the 4 different saturation functions
    typedef float( *satFunc )( const float, const float, const float );
    satFunc methods[4];
    methods[0] = &calcSaturation_A;
    methods[1] = &calcSaturation_B;
    methods[2] = &calcSaturation_C;
    methods[3] = &calcSaturation_D;

    if( saturation_params.method < 4 ) {
        for( i = 0; i < height; i++ ) { // rows
            for( j = 0; j < width; j++ ) { // columns

                float r = src[coord( i, j, 0, bpp, width )];
                float g = src[coord( i, j, 1, bpp, width )];
                float b = src[coord( i, j, 2, bpp, width )];

                // calculate saturation here
                float saturation = ( *methods[ saturation_params.method ] )( r, g, b );

                // some clamping for the 8 bits
                int rounded = ( int )( saturation );

                rounded = rounded > 255 ? 255 : rounded;
                rounded = rounded < 0 ? 0 : rounded;

                dest[coord( i, j, 0, bpp, width )] = rounded;
                dest[coord( i, j, 1, bpp, width )] = rounded;
                dest[coord( i, j, 2, bpp, width )] = rounded;

                // copy alpha
                if( bpp == 4 ) {
                    dest[coord( i, j, 3, bpp, width )] = src[coord( i, j, 3, bpp, width )];;
                }

            }

            if( show_progress && ( ( i % 20 ) == 0 ) ) {
                gimp_progress_update( ( float ) i / height );
            }
        }
    } else {
        error = TRUE;
    }

    if( error ) {
        gimp_pixel_rgn_set_rect( destPR, src, x, y, width, height );
    } else { // this one should be default
        gimp_pixel_rgn_set_rect( destPR, dest, x, y, width, height );
    }

    if( show_progress ) {
        gimp_progress_update( 1.0 );
    }

    //tidy up
    g_free( dest );
    g_free( src );
    printf( "L%i: Saturation finished\n\n", __LINE__ );
}

static gboolean
showSaturationDialog( GimpDrawable* drawable ) {
    GtkWidget* dialog;
    GtkWidget* main_vbox;
    GtkWidget* preview;
    gboolean   run;
    GtkWidget* frame;
    GtkWidget* hbox;
    GtkWidget* button, *button2, *button3, *button4;

    gimp_ui_init( PLUG_IN_BINARY, TRUE );

    dialog = gimp_dialog_new( _( "Calculate Saturation" ), PLUG_IN_BINARY,
                              NULL, ( GtkDialogFlags )0,
                              gimp_standard_help_func, PLUG_IN_PROC,
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                              GTK_STOCK_OK,     GTK_RESPONSE_OK,
                              NULL );

    gtk_dialog_set_alternative_button_order( GTK_DIALOG( dialog ),
            GTK_RESPONSE_OK,
            GTK_RESPONSE_CANCEL,
            -1 );

    gimp_window_set_transient( GTK_WINDOW( dialog ) );

    main_vbox = gtk_vbox_new( FALSE, 12 );
    gtk_container_set_border_width( GTK_CONTAINER( main_vbox ), 12 );
    gtk_container_add( GTK_CONTAINER( gtk_dialog_get_content_area( GTK_DIALOG( dialog ) ) ),
                       main_vbox );
    gtk_widget_show( main_vbox );

    preview = gimp_drawable_preview_new( drawable, NULL );
    gtk_box_pack_start( GTK_BOX( main_vbox ), preview, TRUE, TRUE, 0 );
    gtk_widget_show( preview );

    g_signal_connect( preview, "invalidated",
                      G_CALLBACK( preview_update ),
                      NULL );

    hbox = gtk_hbox_new( FALSE, 12 );
    gtk_box_pack_start( GTK_BOX( main_vbox ), hbox, FALSE, FALSE, 0 );
    gtk_widget_show( hbox );

    frame = gimp_int_radio_group_new( TRUE, _( "Method" ),
                                      G_CALLBACK( gimp_radio_button_update ),
                                      &saturation_params.method, saturation_params.method,
                                      _( "\t\t(max-min)" ), 0, &button,
                                      _( "\t\t(max-min) / sum" ), 2, &button3,
                                      _( "HSV:\t(max-min) / max" ), 1, &button2,
                                      _( "HSL:\t(max-min) / (max+min)" ), 3, &button4,
                                      NULL );

    g_signal_connect_swapped( button, "toggled",
                              G_CALLBACK( gimp_preview_invalidate ),
                              preview );
    g_signal_connect_swapped( button2, "toggled",
                              G_CALLBACK( gimp_preview_invalidate ),
                              preview );
    g_signal_connect_swapped( button3, "toggled",
                              G_CALLBACK( gimp_preview_invalidate ),
                              preview );
    g_signal_connect_swapped( button4, "toggled",
                              G_CALLBACK( gimp_preview_invalidate ),
                              preview );

    gtk_box_pack_start( GTK_BOX( hbox ), frame, FALSE, FALSE, 0 );
    gtk_widget_show( frame );


    gtk_widget_show( dialog );
    run = ( gimp_dialog_run( GIMP_DIALOG( dialog ) ) == GTK_RESPONSE_OK );
    gtk_widget_destroy( dialog );

    return run;
}

static void
preview_update( GimpPreview* preview ) {
    GimpDrawable* drawable;
    gint          x, y;
    gint          width, height;
    GimpPixelRgn  srcPR;
    GimpPixelRgn  destPR;

    drawable =
        gimp_drawable_preview_get_drawable( GIMP_DRAWABLE_PREVIEW( preview ) );

    gimp_pixel_rgn_init( &srcPR, drawable,
                         0, 0, drawable->width, drawable->height, FALSE, FALSE );
    gimp_pixel_rgn_init( &destPR, drawable,
                         0, 0, drawable->width, drawable->height, TRUE, TRUE );

    gimp_preview_get_position( preview, &x, &y );
    gimp_preview_get_size( preview, &width, &height );

    doSaturationRegion( &srcPR, &destPR, drawable->bpp,
                        x, y, width, height,
                        FALSE );

    gimp_pixel_rgn_init( &destPR, drawable, x, y, width, height, FALSE, TRUE );
    gimp_drawable_preview_draw_region( GIMP_DRAWABLE_PREVIEW( preview ), &destPR );
}

