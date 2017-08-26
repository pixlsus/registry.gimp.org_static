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

#define PLUG_IN_PROC    "plug-in-heatmap"
#define PLUG_IN_BINARY  "elsamuko-heatmap"

#define SCALE_WIDTH   120
#define ENTRY_WIDTH     5

typedef struct {
    gboolean  run;
} HeatmapInterface;


struct RGB {
    guchar r;
    guchar g;
    guchar b;
};

struct YUV {
    float y;
    float u;
    float v;
};

int CLAMP255( int in ) {

    if( in < 0 ) {
        return 0;
    }

    if( in > 255 ) {
        return 255;
    }

    return in;
}

void RGB2YUV( const struct RGB* rgb, struct YUV* yuv ) {
    yuv->y = 0.299   * ( float )rgb->r +
             0.587 * ( float )rgb->g +
             0.114  * ( float )rgb->b;
    yuv->v = ( float )rgb->r - yuv->y;
    yuv->u = ( float )rgb->b - yuv->y;
}

void YUV2RGB( const struct YUV* yuv, struct RGB* rgb ) {
    rgb->r = CLAMP255( yuv->v + yuv->y );
    rgb->b = CLAMP255( yuv->u + yuv->y );
    rgb->g = CLAMP255( ( yuv->y - 0.299 * ( float )rgb->r - 0.114 * ( float )rgb->b ) / 0.587 );
}


/* local function prototypes */
inline gint coord( gint x, gint y, gint k, gint channels, gint width ) {
    return channels * ( width * y + x ) + k;
}

static void      query( void );
static void      run( const gchar*      name,
                      gint              nparams,
                      const GimpParam*  param,
                      gint*             nreturn_vals,
                      GimpParam**       return_vals );

static void      heatmap( GimpDrawable*   drawable );

/* Setting PLUG_IN_INFO */
const GimpPlugInInfo PLUG_IN_INFO = {
    NULL,  /* init_proc  */
    NULL,  /* quit_proc  */
    query, /* query_proc */
    run,   /* run_proc   */
};

MAIN()

static void query( void ) {
    static const GimpParamDef args[] = {
        { GIMP_PDB_INT32,    "run-mode",  "The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    "image",     "image" },
        { GIMP_PDB_DRAWABLE, "drawable",  "drawable" }
    };

    gimp_install_procedure( PLUG_IN_PROC,
                            "Render a heatmap",
                            "Render a heatmap",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "2012",
                            "_Heatmap...",
                            "RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS( args ), 0,
                            args, NULL );

    gimp_plugin_menu_register( PLUG_IN_PROC, "<Image>/Colors" );
}

static void run( const gchar*      name,
                 gint              nparams,
                 const GimpParam*  param,
                 gint*             nreturn_vals,
                 GimpParam**       return_vals ) {

    ( void ) name; //unused

    static GimpParam   values[1];
    GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
    GimpDrawable*      drawable;
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

    switch( run_mode ) {
        case GIMP_RUN_INTERACTIVE:
        case GIMP_RUN_NONINTERACTIVE:
        case GIMP_RUN_WITH_LAST_VALS:

            if( nparams != 3 ) {
                status = GIMP_PDB_CALLING_ERROR;
            }

            break;

        default:
            break;
    }

    if( status == GIMP_PDB_SUCCESS ) {
        drawable = gimp_drawable_get( param[2].data.d_drawable );

        /* here we go */
        heatmap( drawable );

        if( run_mode != GIMP_RUN_NONINTERACTIVE ) {
            gimp_displays_flush();
        }

        gimp_drawable_detach( drawable );
        values[0].data.d_status = status;
    }
}

static void heatmap( GimpDrawable* drawable ) {

    printf( "\n****Begin of heatmap.****\n" );

    const gint32 image_ID = gimp_drawable_get_image( drawable->drawable_id );
    gimp_image_undo_group_start( image_ID );
    const gint width    = gimp_image_width( image_ID );
    const gint height   = gimp_image_height( image_ID );

    //define layers
    gint num_of_layers = 0;
    gint* layers = gimp_image_get_layers( image_ID, &num_of_layers );
    gint sourcelayer = layers[0];

    //define drawables
    GimpDrawable* sourcedrawable = gimp_drawable_get( sourcelayer );
    const gint channels = gimp_drawable_bpp( drawable->drawable_id );

    gint32 heatmap = gimp_layer_new( image_ID,
                                     "Heatmap",
                                     width,
                                     height,
                                     GIMP_RGBA_IMAGE,
                                     100,
                                     GIMP_NORMAL_MODE );
    gimp_image_add_layer( image_ID, heatmap, 0 );
    GimpDrawable* heatdrawable = gimp_drawable_get( heatmap );

    printf( "    Image ID: %i\n",           image_ID );
    printf( "    Number of Layers: %i\n",   num_of_layers );
    printf( "    Source layer: %i\n",       sourcelayer );
    printf( "    Heat layer: %i\n",         heatmap );
    printf( "    Number of Channels: %i\n", channels );

    //select Regions
    GimpPixelRgn prSource;
    GimpPixelRgn prHeat;

    gimp_pixel_rgn_init( &prSource,
                         sourcedrawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    gimp_pixel_rgn_init( &prHeat,
                         heatdrawable,
                         0, 0,
                         width, height,
                         TRUE, TRUE );
    printf( "    Pixel regions initiated.\n" );

    guchar* rectsource;
    guchar* rectheat;

    //Initialise memory
    rectsource = g_new( guchar, channels * width * height );
    rectheat = g_new( guchar, 4 * width * height );

    //Save source images in array
    gimp_pixel_rgn_get_rect( &prSource,
                             rectsource,
                             0, 0,
                             width, height );

    //Algorithm begins here:
    gint x;
    gint y;

    gint heat = 0;

    struct RGB rgb;
    struct YUV yuv;

    int angle = 180 - 30;
    gdouble cosa = cos( ( gdouble )angle / 180 * M_PI );
    gdouble sina = sin( ( gdouble )angle / 180 * M_PI );

    for( y = 0; y < height; y++ ) {
        for( x = 0; x < width; x++ ) {

            rgb.r = ( gint )rectsource[coord( x, y, 0, channels, width )];
            rgb.g = ( gint )rectsource[coord( x, y, 1, channels, width )];
            rgb.b = ( gint )rectsource[coord( x, y, 2, channels, width )];

            RGB2YUV( &rgb, &yuv );

            heat = CLAMP255( 0.5 * ( cosa * yuv.u + sina * yuv.v + 255 ) );

            //set heat
            rectheat[coord( x, y, 0, 4, width )] = heat;
            rectheat[coord( x, y, 1, 4, width )] = heat;
            rectheat[coord( x, y, 2, 4, width )] = heat;
            rectheat[coord( x, y, 3, 4, width )] = 255;

        }

        if( y % 10 == 0 ) {
            gimp_progress_update( ( gdouble )( y / ( gdouble ) gimp_image_height( image_ID ) ) );
        }
    }

    //save heatmap in array
    gimp_pixel_rgn_set_rect( &prHeat,
                             rectheat,
                             0, 0,
                             width, height );

    //free memory
    g_free( rectsource );
    g_free( rectheat );

    gimp_drawable_flush( gimp_drawable_get( heatmap ) );
    gimp_drawable_merge_shadow( heatmap, TRUE );
    gimp_drawable_update( heatmap,
                          0, 0,
                          gimp_image_width( image_ID ),
                          gimp_image_height( image_ID ) );

    printf( "  Pixel copied and set.\n" );
    gimp_levels_stretch( heatmap );

    gimp_image_undo_group_end( image_ID );
    printf( "****End of heatmap.****\n" );
}

