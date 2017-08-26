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

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

//THREADS
#include <pthread.h> /* Posix 1003.1c threads */

#define PLUG_IN_PROC    "plug-in-depthmap"
#define PLUG_IN_BINARY  "elsamuko-depthmap"

#define SCALE_WIDTH   120
#define ENTRY_WIDTH     5

typedef struct {
    gint    radius;
    gint    parallax;
} DepthmapParams;

typedef struct {
    gboolean  run;
} DepthmapInterface;

/* local function prototypes */
inline gint coord( gint x, gint y, gint k, gint channels, gint width ) {
    return channels*( width*y + x ) + k;
};

static void      query( void );
static void      run( const gchar      *name,
                      gint              nparams,
                      const GimpParam  *param,
                      gint             *nreturn_vals,
                      GimpParam       **return_vals );

static void      depthmap( GimpDrawable   *drawable );

static gboolean  depthmap_dialog( GimpDrawable   *drawable );

/* create a few globals, set default values */
static DepthmapParams depthmap_params = {
    2,   /* default radius    */
    5    /* default parallax  */
};

/* Setting PLUG_IN_INFO */
const GimpPlugInInfo PLUG_IN_INFO = {
    NULL,  /* init_proc  */
    NULL,  /* quit_proc  */
    query, /* query_proc */
    run,   /* run_proc   */
};

MAIN();

static void query( void ) {
    static const GimpParamDef args[] = {
        { GIMP_PDB_INT32,    "run-mode",  "The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    "image",     "image" },
        { GIMP_PDB_DRAWABLE, "drawable",  "drawable" },
        { GIMP_PDB_FLOAT,    "radius",    "Fragment radius" },
        { GIMP_PDB_FLOAT,    "parallax",  "Horizontal parallax" }
    };

    gimp_install_procedure( PLUG_IN_PROC,
                            "Render a depthmap out of two stereo layers",
                            "Render a depthmap out of two stereo layers",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "2009",
                            "_Depthmap...",
                            "GRAY*, RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS( args ), 0,
                            args, NULL );

    gimp_plugin_menu_register( PLUG_IN_PROC, "<Image>/Filters/Map" );
}

static void run( const gchar      *name,
                 gint              nparams,
                 const GimpParam  *param,
                 gint             *nreturn_vals,
                 GimpParam       **return_vals ) {
    static GimpParam   values[1];
    GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
    GimpDrawable      *drawable;
    GimpRunMode        run_mode;

    run_mode = param[0].data.d_int32;

    *return_vals  = values;
    *nreturn_vals = 1;

    values[0].type          = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    /*
     * Get drawable information...
     */
    drawable = gimp_drawable_get( param[2].data.d_drawable );

    switch ( run_mode ) {
        case GIMP_RUN_INTERACTIVE:
            gimp_get_data( PLUG_IN_PROC, &depthmap_params );
            /* Reset default values show preview unmodified */

            /* initialize pixel regions and buffer */
            if ( ! depthmap_dialog( drawable ) )
                return;

            break;

        case GIMP_RUN_NONINTERACTIVE:
            if ( nparams != 6 ) {
                status = GIMP_PDB_CALLING_ERROR;
            } else {
                depthmap_params.radius = param[3].data.d_int32;
                depthmap_params.parallax = param[4].data.d_int32;
            }
            break;

        case GIMP_RUN_WITH_LAST_VALS:
            gimp_get_data( PLUG_IN_PROC, &depthmap_params );
            break;

        default:
            break;
    }

    if ( status == GIMP_PDB_SUCCESS ) {
        drawable = gimp_drawable_get( param[2].data.d_drawable );

        /* here we go */
        depthmap( drawable );

        if ( run_mode != GIMP_RUN_NONINTERACTIVE )
            gimp_displays_flush();

        /* set data for next use of filter */
        if ( run_mode == GIMP_RUN_INTERACTIVE )
            gimp_set_data( PLUG_IN_PROC,
                           &depthmap_params, sizeof( DepthmapParams ) );

        gimp_drawable_detach( drawable );
        values[0].data.d_status = status;
    }
}

static void depthmap( GimpDrawable *drawable ) {

    printf( "\n****Begin of depthmap.****\n" );

    const gint32 image_ID = gimp_drawable_get_image( drawable->drawable_id );
    gimp_image_undo_group_start( image_ID );
    const gint width    = gimp_image_width( image_ID );
    const gint height   = gimp_image_height( image_ID );
    const gint parallax = depthmap_params.parallax;
    const gint radius = depthmap_params.radius;

    //define layers
    gint num_of_layers;
    gint *layers = gimp_image_get_layers( image_ID, &num_of_layers );
    gint leftlayer = layers[0];
    gint rightlayer = layers[1];

    //add alpha if necessary
    gimp_layer_add_alpha( leftlayer );
    gimp_layer_add_alpha( rightlayer );

    //define drawables
    GimpDrawable *leftdrawable = gimp_drawable_get( leftlayer );
    GimpDrawable *rightdrawable = gimp_drawable_get( rightlayer );
    const gint channels = gimp_drawable_bpp( drawable->drawable_id );

    gint32 depthmap = gimp_layer_new( image_ID,
                                      "Depthmap",
                                      width,
                                      height,
                                      GIMP_RGBA_IMAGE,
                                      100,
                                      GIMP_NORMAL_MODE );
    gimp_image_add_layer( image_ID, depthmap, 0 );
    GimpDrawable *depthdrawable = gimp_drawable_get( depthmap );

    printf( "    Image ID: %i\n",           image_ID );
    printf( "    Number of Layers: %i\n",   num_of_layers );
    printf( "    Leftlayer: %i\n",          leftlayer );
    printf( "    Rightlayer: %i\n",         rightlayer );
    printf( "    Depthlayer: %i\n",         depthmap );
    printf( "    Number of Channels: %i\n", channels );

    //select Regions
    GimpPixelRgn prLeftIn;
    GimpPixelRgn prRightIn;
    GimpPixelRgn prDepthOut;

    gimp_pixel_rgn_init( &prLeftIn,
                         leftdrawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    gimp_pixel_rgn_init( &prRightIn,
                         rightdrawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    gimp_pixel_rgn_init( &prDepthOut,
                         depthdrawable,
                         0, 0,
                         width, height,
                         TRUE, TRUE );
    printf( "    Pixel regions initiated.\n" );

    guchar *rectleft;
    guchar *rectright;
    guchar *rectdepth;

    //Initialise memory
    rectleft = g_new( guchar, channels * width * height );
    rectright = g_new( guchar, channels * width * height );
    rectdepth = g_new( guchar, channels * width * height );

    //Save stereo images in array
    gimp_pixel_rgn_get_rect( &prLeftIn,
                             rectleft,
                             0, 0,
                             width, height );
    gimp_pixel_rgn_get_rect( &prRightIn,
                             rectright,
                             0, 0,
                             width, height );

    //Algorithm begins here:
    gint x;
    gint y;
    int i;
    gint diff;
    gint diffnew;
    gint k;
    gint xk;
    gint yk;
    gint besthit;
    for ( y = radius; y < height - radius; y++ ) {
        for ( x = parallax + radius; x < ( width - radius ); x++ ) {
            diff = 10000000;
            diffnew = 0;
            besthit = 0;
            //search at the left side for the best fit
            for ( i = 0; i > -parallax; i-- ) {
                diffnew = 0;
                for ( k = 0; k < channels; k++ ) {
                    for ( xk = -radius; xk <= radius; xk++ ) {
                        for ( yk = -radius; yk <= radius; yk++ ) {
                            diffnew += abs(( gint )rectright[coord( x+i+xk,y+yk,k,channels,width )] -
                                           ( gint )rectleft[coord( x+xk,y+yk,k,channels,width )] );
                        }
                    }
                }
                //set besthit, if the new diff is lower than old diff
                if ( diffnew < diff ) {
                    besthit = -i;
                    diff = diffnew;
                }
            }
            //set depth
            for ( i = 0; i < ( channels - 1 ); i++ ) {
                rectdepth[coord( x,y,i,channels,width )] = besthit;
            }
            rectdepth[coord( x,y,( channels-1 ),channels,width )] = 255;
        }
        if ( y % 10 == 0 ) {
            gimp_progress_update(( gdouble )( y / ( gdouble ) gimp_image_height( image_ID ) ) );
        }
    }

    //save depthmap in array
    gimp_pixel_rgn_set_rect( &prDepthOut,
                             rectdepth,
                             0, 0,
                             width, height );

    //free memory
    g_free( rectleft );
    g_free( rectright );
    g_free( rectdepth );

    //gimp_drawable_flush (gimp_drawable_get(depthmap));
    gimp_drawable_merge_shadow( depthmap, TRUE );
    gimp_drawable_update( depthmap,
                          0, 0,
                          gimp_image_width( image_ID ),
                          gimp_image_height( image_ID ) );

    printf( "  Pixel copied and set.\n" );
    gimp_levels_stretch( depthmap );

    gimp_image_undo_group_end( image_ID );
    printf( "****End of depthmap.****\n" );
}


static gboolean depthmap_dialog( GimpDrawable *drawable ) {
    GtkWidget *dialog;
    GtkWidget *main_vbox;
    GtkWidget *table;
    GtkObject *adj;
    gboolean   run;

    gimp_ui_init( PLUG_IN_BINARY, TRUE );

    dialog = gimp_dialog_new( "Depthmap", PLUG_IN_BINARY,
                              NULL, 0,
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

    table = gtk_table_new( 3, 2, FALSE );
    gtk_table_set_col_spacings( GTK_TABLE( table ), 6 );
    gtk_table_set_row_spacings( GTK_TABLE( table ), 6 );
    gtk_box_pack_start( GTK_BOX( main_vbox ), table, FALSE, FALSE, 0 );
    gtk_widget_show( table );

    adj = gimp_scale_entry_new( GTK_TABLE( table ), 0, 0,
                                "_Radius:", SCALE_WIDTH, ENTRY_WIDTH,
                                depthmap_params.radius, 1, 10, 1, 5, 1,
                                TRUE, 0, 0,
                                NULL, NULL );
    g_signal_connect( adj, "value-changed",
                      G_CALLBACK( gimp_int_adjustment_update ),
                      &depthmap_params.radius );

    adj = gimp_scale_entry_new( GTK_TABLE( table ), 0, 1,
                                "_Parallax:", SCALE_WIDTH, ENTRY_WIDTH,
                                depthmap_params.parallax, 1, 50, 1, 5, 2,
                                TRUE, 0, 0,
                                NULL, NULL );
    g_signal_connect( adj, "value-changed",
                      G_CALLBACK( gimp_int_adjustment_update ),
                      &depthmap_params.parallax );

    gtk_widget_show( dialog );

    run = ( gimp_dialog_run( GIMP_DIALOG( dialog ) ) == GTK_RESPONSE_OK );

    gtk_widget_destroy( dialog );

    return run;
}

