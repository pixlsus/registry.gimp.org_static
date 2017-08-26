/*
 * Copyright (C) 1999 Winston Chang
 *                    <winstonc@cs.wisc.edu>
 *                    <winston@stdout.org>
 * Copyright (C) 2010 <elsamuko@web.de>
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

// copymove.c
//
// Copyright (c) 2007-2008 John Graham-Cumming
//
// Image copy/move forgery detector using the technique described in
// 'Detection of Copy-Move Forgery in Digital Images', Fridrich,
// Soukal and Lukas
//
// http://www.ws.binghamton.edu/fridrich/Research/copymove.pdf
//
// ---------------------------------------------------------------------------
//
// Briefly the algorithm goes like this:
//
// Slide a 16x16 block across the entire image from left hand corner
// to bottom right hand corner.  For each 16x16 block perform a
// discrete cosine transform on it and then quantize the 16x16 block
// using an expanded version of the standard JPEG quantization matrix.
//
// Each quantized DCT transformed is stored in a matrix with one row
// per (x,y) position in the original image (the (x,y) being the upper
// left hand corner of the 16x16 block being examined.
//
// The resulting matrix is lexicographically sorted and then rows that
// match in the matrix are identified.  For each pair of matching rows
// (x1,y1) and (x2,y2) the shift vector (x1-x2,y1-y2) (normalized by
// swapping if necessary so that the first value is +ve) is computed
// and for each shift vector a count is kept of the number of times it
// is seen.
//
// Finally the shift vectors with a count > some threshold are
// examined, the corresponding pair of positions in the image are
// found and the 16x16 blocks they represent are highlighted.
//
// ---------------------------------------------------------------------------
//
// This file is part of part of copy-move
//
// copy-move is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation; either version 2 of the License,
// or (at your option) any later version.
//
// copy-move is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with shimmer; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
//
// ---------------------------------------------------------------------------

//
// install it with
// CC=g++ CFLAGS=-O3 LIBS=-lpthread gimptool-2.0 --install elsamuko-copy-move.c
//

#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <pthread.h>   /* Posix 1003.1c threads */

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <glib.h>
#include <glib/gprintf.h>

#define PLUG_IN_PROC    "elsamuko-copy-move"
#define PLUG_IN_BINARY  "elsamuko-copy-move"

#define max(a,b) (((a)>(b))?a:b)
#define min(a,b) (((a)<(b))?a:b)

using namespace std;

// Used as part of the index for finding block of 16x16 pixels in the
// original image once the vectors have been sorted
struct position {
    int i;
    int x;
    int y;
};

class void_data {
public:
    vector<vector<vector<vector<double> > > > pre;
    vector<vector<double> > pixels;
    vector<vector<double> > q16;
    gint quality;
    int w16;
    int h16;
    int first_row;
    int last_row;
    int thread;
};

// This is the matrix of 16x16 blocks after transformation and
// quantization
int* matrix;
struct position* gindex;
// pthread_mutex_t MUTEX;

typedef struct {
    gint    quality;
    gint    threshold;
} CopymoveParams;

typedef struct {
    gboolean  run;
} CopymoveInterface;

/* local function prototypes */
inline gint coord( gint x, gint y, gint k, gint channels, gint width ) {
    return channels*( width*y + x ) + k;
};

// Function to do lexicographic compare on two rows in the matrix using
// the index.
int compare( /*(struct position *)*/ const void* a, /*struct position**/ const void* b );
void* funcDCT(void* data);
static void      query( void );
static void      run( const gchar      *name,
                      gint              nparams,
                      const GimpParam  *param,
                      gint             *nreturn_vals,
                      GimpParam       **return_vals );
static void      copymove( GimpDrawable   *drawable );
static gboolean  copymove_dialog( GimpDrawable   *drawable );

/* create a few globals, set default values */
static CopymoveParams copymove_params = {
    1,   /* default quality    */
    10   /* default threshold  */
};

/* Setting PLUG_IN_INFO */
const GimpPlugInInfo PLUG_IN_INFO = {
    NULL,  /* init_proc  */
    NULL,  /* quit_proc  */
    query, /* query_proc */
    run,   /* run_proc   */
};

MAIN();

static void
query (void)
{
    static const GimpParamDef args[] =
    {
        { GIMP_PDB_INT32,    (gchar*)"run-mode", (gchar*)"The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    (gchar*)"image",    (gchar*)"Input image (unused)"         },
        { GIMP_PDB_DRAWABLE, (gchar*)"drawable", (gchar*)"Input drawable"               },
        { GIMP_PDB_FLOAT,    (gchar*)"int",      (gchar*)"Blur factor   (0-10)"          },
        { GIMP_PDB_DRAWABLE, (gchar*)"int",      (gchar*)"Pair Treshold (1-20)"         }
    };

    gimp_install_procedure( PLUG_IN_PROC,
                            "Copy Move Detection",
                            "Copy Move Detection",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "2010",
                            "_Copy Move...",
                            "RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS( args ), 0,
                            args, NULL );

    gimp_plugin_menu_register( PLUG_IN_PROC, "<Image>/Image" );
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

    run_mode = (GimpRunMode)param[0].data.d_int32;

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
        gimp_get_data( PLUG_IN_PROC, &copymove_params );
        /* Reset default values show preview unmodified */

        /* Initialize pixel regions and buffer */
        if ( ! copymove_dialog( drawable ) )
            return;

        break;

    case GIMP_RUN_NONINTERACTIVE:
        if ( nparams != 6 ) {
            status = GIMP_PDB_CALLING_ERROR;
        } else {
            copymove_params.quality = param[3].data.d_int32;
            copymove_params.threshold = param[4].data.d_int32;
        }
        break;

    case GIMP_RUN_WITH_LAST_VALS:
        gimp_get_data( PLUG_IN_PROC, &copymove_params );
        break;

    default:
        break;
    }

    if ( status == GIMP_PDB_SUCCESS ) {
        drawable = gimp_drawable_get( param[2].data.d_drawable );

        /* Here we go */
        copymove( drawable );

        if ( run_mode != GIMP_RUN_NONINTERACTIVE )
            gimp_displays_flush();

        /* Set data for next use of filter */
        if ( run_mode == GIMP_RUN_INTERACTIVE )
            gimp_set_data( PLUG_IN_PROC,
                           &copymove_params, sizeof( CopymoveParams ) );

        gimp_drawable_detach( drawable );
        values[0].data.d_status = status;
    }
}

static void
copymove( GimpDrawable *drawable ) {

    printf( "\nL%i: ****Begin of copymove.****\n", __LINE__ );

    const gint32 image_ID = gimp_drawable_get_image( drawable->drawable_id );
    gimp_image_undo_group_start( image_ID );
    const gint width  = gimp_image_width( image_ID );
    const gint height = gimp_image_height( image_ID );
    // The number of overlapping 16 pixel blocks that can appear across and down in the image
    const int h16 = height - 16 + 1;
    const int w16 = width  - 16 + 1;
    const gint channels = gimp_drawable_bpp( drawable->drawable_id );

    printf( "L%i: Image ID: %i\n",          __LINE__, image_ID );
    printf( "L%i: Number of Channels: %i\n",__LINE__, channels );

    // Select Regions
    GimpPixelRgn region;
    gimp_pixel_rgn_init( &region,
                         drawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    printf( "L%i: Pixel region initiated.\n", __LINE__ );

    // Initialise memory
    guchar *rectangle = g_new( guchar, channels * width * height );

    // Save image in array
    gimp_pixel_rgn_get_rect( &region,
                             rectangle,
                             0, 0,
                             width, height );


    // Algorithm begins here:
    printf( "L%i: Begin algorithm.\n", __LINE__ );
    gint x;
    gint y;

    // This is the standard JPEG chrominance quantization matrix
    double q8[8][8] = { { 4, 4, 6, 11, 24, 24, 24, 24 },
        { 4, 5, 6, 16, 24, 24, 24, 24 },
        { 6, 6, 14, 24, 24, 24, 24, 24 },
        { 11, 16, 24, 24, 24, 24, 24, 24 },
        { 24, 24, 24, 24, 24, 24, 24, 24 },
        { 24, 24, 24, 24, 24, 24, 24, 24 },
        { 24, 24, 24, 24, 24, 24, 24, 24 },
        { 24, 24, 24, 24, 24, 24, 24, 24 }
    };

    // This is expanded to a 16x16 matrix as described in Fridrich's paper
    // and is filled in by code in main()
    vector<vector<double> > q16(16,vector<double>(16));


    // Build the extended quantization matrix
    printf( "L%i: Analyzing image (%d x %d)\n", __LINE__, height, width );
    int i, j;
    for ( i = 0; i < 8; ++i ) {
        for ( j = 0; j < 8; ++j ) {
            q16[i][j] = q8[i][j] * 2.5;
        }
    }

    q16[0][0] = 2.0 * q8[0][0];

    for ( i = 8; i < 16; ++i ) {
        for ( j = 0; j < 8; ++j ) {
            q16[i][j] = q8[0][7] * 2.5;
        }
    }

    for ( i = 8; i < 16; ++i ) {
        for ( j = 8; j < 16; ++j ) {
            q16[i][j] = q8[7][7] * 2.5;
        }
    }

    for ( i = 0; i < 8; ++i ) {
        for ( j = 8; j < 16; ++j ) {
            q16[i][j] = q8[7][0] * 2.5;
        }
    }

    // Allocate memory
    size_t matrix_size = w16 * h16 * 16 * 16 * sizeof( int );
    size_t index_size = w16 * h16 * sizeof( struct position );
    printf( "L%i: Max possible size: %i \n", __LINE__, sizeof(size_t) );
    printf( "L%i: Matrix size: %i \n", __LINE__, matrix_size );
    printf( "L%i: Index size: %i \n", __LINE__, index_size );
    fflush(0);

    matrix = (int*)      malloc( matrix_size );
    gindex  = (position*) malloc( index_size  );
    memset( matrix, 0, matrix_size );
    memset( gindex, 0, index_size );

    // Precompute coefficients to use in the DCT to save time
    printf( "L%i: Precomputing... \n", __LINE__ );
    fflush(0);

    // 16^4 dimension vector
    vector<vector<vector<vector<double> > > > pre (4, vector<vector<vector<double> > >(4, vector<vector<double> >(16,vector<double>(16))));

    int u;
    int v;
    double pi16th = 3.141592654/16.0;
    for ( u = 0; u < 4; ++u ) {
        for ( v = 0; v < 4; ++v ) {
            for ( j = 0; j < 16; ++j ) {
                for ( i = 0; i < 16; ++i ) {
                    pre[u][v][i][j] = cos( pi16th * ( (double)i + 0.5 ) * (double)u ) *
                                      cos( pi16th * ( (double)j + 0.5 ) * (double)v );
                }
            }
        }
    }

    //iterate over image
    printf( "L%i: Read luminosity values \n", __LINE__ );
    fflush(0);

    //double pixels[height][width];
    vector<vector<double> > pixels(height,vector<double>(width));
    for ( y = 0; y < height; y++ ) {
        // printf( "L%i: row:%i\n", __LINE__, y );
        // fflush(0);
        for ( x = 0; x < width; x++ ) {
            double pixel = (double) rectangle[coord( x, y, 0, channels, width )] * 0.299
                           + (double) rectangle[coord( x, y, 1, channels, width )] * 0.587
                           + (double) rectangle[coord( x, y, 2, channels, width )] * 0.114;
            pixel -= 128;
            pixel = round(pixel);
            pixels[y][x] = pixel;
        }
    }

    //some scoping
    printf( "L%i: Building DCT transformed matrix \n", __LINE__ );
    fflush(0);
    do {
        // void_data data;
        // data.pre = pre;
        // data.pixels = pixels;
        // data.q16 = q16;
        // data.quality=copymove_params.quality;
        // data.w16=w16;
        // data.h16=h16;
        // data.first_row=0;
        // data.last_row=w16;
        // funcDCT((void*) &data);
        
        // Set number of threads
        int no_of_threads = 4;
        vector<pthread_t> threads(no_of_threads);

        // Array to indicate actual thread
        vector<void_data> count(no_of_threads);
        int first = 0;
        int last  = w16/no_of_threads;
        int step  = (int) w16/no_of_threads;
        for (int i=0; i< no_of_threads; i++) {
            void_data data;
            data.pre = pre;
            data.pixels = pixels;
            data.q16 = q16;
            data.quality=copymove_params.quality;
            data.w16=w16;
            data.h16=h16;
            data.first_row=first;
            data.last_row=last;
            data.thread=i;
            count[i]=data;
            first=last+1;
            if (i==no_of_threads-2) last=w16;
            else last+=step;
        }

        // Call threads
        for (int i=0; i< no_of_threads; i++) {
            pthread_create( &threads[i], NULL, funcDCT, &(count[i]) );
        }

        // Wait for threads to end
        for (int i=0; i< no_of_threads; i++) {
            pthread_join( threads[i], NULL );
        }

    } while(false);


    // At this point the matrix has been created and now needs to
    // be sorted and copied sections must be detected
    printf( "100%%\nL%i: Sorting index into lexicographic order... \n", __LINE__ );
    qsort( &gindex[0], w16 * h16, sizeof( struct position ), &compare /*matrix*/ );
    printf( "L%i: Building shift vectors... \n", __LINE__ );
    fflush(0);

    // Build shift vectors the recognize blocks of identical pixels
    int* shift = (int*)malloc( sizeof( int ) * width * height * 2 );
    for ( i = 0; i < width; ++i ) {
        for ( j = 0; j < height*2; ++j ) {
            shift[j * width + i] = 0;
        }
    }

    int last_percent = 0;
    for ( i = 0; i < w16 * h16 - 1; ++i ) {
        if ( ( 100 * i / ( w16 * h16 -1 ) ) > last_percent ) {
            printf( "%d%% ", last_percent );
            fflush(0);
            last_percent += 5;
        }

        if ( compare( &gindex[i], &gindex[i+1] ) == 0 ) {
            int sx = gindex[i].x - gindex[i+1].x;
            int sy = gindex[i].y - gindex[i+1].y;

            if ( sx < 0 ) {
                sx = -sx;
                sy = -sy;
            }

            sy += height;
            ++shift[sy * width + sx];
        }
    }

    printf( "100%%\nL%i: Creating cloned images... \n", __LINE__ );
    fflush(0);

    // Duplicate the original color image and shade areas of the image
    // that appear to be duplicated by finding shift vectors with a
    // count above the threshold
    for ( i = 0; i < width; ++i ) {
        for ( j = 0; j < height*2; ++j ) {
            if ( ( ( i > 16 ) || ( ( abs(j-height) > 16 ) ) ) &&
                    ( shift[j * width + i] > copymove_params.threshold ) ) {
                printf( "L%i: Shift vector (%d,%d) has count %d\n",__LINE__, i, j - height,
                        shift[j * width + i] );

                //add layer
                gint32 new_layer = gimp_layer_new( image_ID,
                                                   "Copy Move",
                                                   width,
                                                   height,
                                                   GIMP_RGBA_IMAGE,
                                                   100,
                                                   GIMP_NORMAL_MODE );
                gimp_layer_add_alpha( new_layer );
                gimp_image_add_layer( image_ID, new_layer, 0 );
                GimpDrawable *new_drawable = gimp_drawable_get( new_layer );
                GimpPixelRgn new_region;
                gimp_pixel_rgn_init( &new_region,
                                     new_drawable,
                                     0, 0,
                                     width, height,
                                     TRUE, TRUE );
                guchar *new_rectangle;
                new_rectangle = g_new( guchar, 4 * width * height );
                memset( new_rectangle, 0, width*height*4*sizeof(guchar) );

                int sx = i;
                int sy = j - height;

                int k;
                for ( k = 0; k < w16 * h16 - 1; ++k ) {
                    if ( compare( &gindex[k], &gindex[k+1] ) == 0 ) {
                        int sxx = gindex[k].x - gindex[k+1].x;
                        int syy = gindex[k].y - gindex[k+1].y;
                        if ( sxx < 0 ) {
                            sxx = -sxx;
                            syy = -syy;
                        }

                        if ( sx == sxx ) {
                            if ( sy == syy ) {
                                int x;
                                int y;
                                int c;

                                for ( c = k; c < k+2; ++c ) {
                                    for ( x = gindex[c].x; x < gindex[c].x + 16; ++x ) {
                                        for ( y = gindex[c].y; y < gindex[c].y + 16; ++y ) {
                                            new_rectangle[coord(x, y, 3, 4, width )] = 255;
                                            if ( gindex[k].x < gindex[k+1].x ) {
                                                if ( c == k ) {
                                                    new_rectangle[coord(x, y, 0, 4, width )] = 255;
                                                } else {
                                                    new_rectangle[coord(x, y, 1, 4, width )] = 255;
                                                }
                                            } else {
                                                if ( c != k ) {
                                                    new_rectangle[coord(x, y, 0, 4, width )] = 255;
                                                } else {
                                                    new_rectangle[coord(x, y, 1, 4, width )] = 255;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                //save result in array
                gimp_pixel_rgn_set_rect( &new_region,
                                         new_rectangle,
                                         0, 0,
                                         width, height );
                g_free( new_rectangle );
                gimp_drawable_merge_shadow( new_layer, TRUE );
                gimp_drawable_update( new_layer,
                                      0, 0,
                                      gimp_image_width( image_ID ),
                                      gimp_image_height( image_ID ) );

            }
        }
    }

    //free memory
    g_free( rectangle );

    //end
    gimp_image_undo_group_end( image_ID );
    printf( "L%i: ****End of copymove.****\n", __LINE__ );
}


static gboolean
copymove_dialog( GimpDrawable *drawable ) {
    GtkWidget *dialog;
    GtkWidget *main_vbox;
    GtkWidget *table;
    GtkObject *adj;
    gboolean   run;

    gimp_ui_init( PLUG_IN_BINARY, TRUE );

    dialog = gimp_dialog_new( "Copymove", PLUG_IN_BINARY,
                              NULL, GtkDialogFlags(0),
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
                                "_Quality:", 200, 5,
                                copymove_params.quality, 0, 10, 1, 2, 0,
                                TRUE, 0, 0,
                                NULL, NULL );
    g_signal_connect( adj, "value-changed",
                      G_CALLBACK( gimp_int_adjustment_update ),
                      &copymove_params.quality );

    adj = gimp_scale_entry_new( GTK_TABLE( table ), 0, 1,
                                "_Threshold:", 200, 5,
                                copymove_params.threshold, 1, 100, 1, 2, 0,
                                TRUE, 0, 0,
                                NULL, NULL );
    g_signal_connect( adj, "value-changed",
                      G_CALLBACK( gimp_int_adjustment_update ),
                      &copymove_params.threshold );

    gtk_widget_show( dialog );
    run = ( gimp_dialog_run( GIMP_DIALOG( dialog ) ) == GTK_RESPONSE_OK );
    gtk_widget_destroy( dialog );
    return run;
}

int compare( /*(struct position *)*/const void* a, /*struct position**/const void* b )
{
    int * m_a = &matrix[ ((struct position *)a)->i * 16 * 16 ];
    int * m_b = &matrix[ ((struct position *)b)->i * 16 * 16 ];
    int i;

    for ( i = 0; i < 16 * 16; ++i ) {
        if ( m_a[i] < m_b[i] ) {
            return -1;
        }
        if ( m_a[i] > m_b[i] ) {
            return 1;
        }
    }
    return 0;
}

void* funcDCT(void* in) {
    void_data* data = reinterpret_cast<void_data*>(in);
    double sqrt_116 = 0.25; //sqrt(1.0/16.0);
    double sqrt_216 = 0.35355339059327379; //sqrt(2.0/16.0);
    int a,d,u,v,j,i;
    int last_percent=0;
    int i_pos=data->first_row*data->h16;
    for ( a = data->first_row; a < data->last_row; ++a ) {
        if(data->thread==0){
            //pthread_mutex_lock (&MUTEX);
            int progress = 100 * (a - data->first_row)/(data->last_row - data->first_row);
            if(progress > last_percent){
                printf( "%i%% ", last_percent );
                fflush(0);
                last_percent+=5;
            }
            //pthread_mutex_unlock (&MUTEX);
        }

        for ( d = 0; d < data->h16; ++d ) {
            gindex[i_pos].i = i_pos;
            gindex[i_pos].x = a;
            gindex[i_pos].y = d;

            // The result of the DCT is stored in this matrix and is
            // computed as we scan the image.  First it is set to 0.
            double dct[16][16];
            for ( i = 0; i < 16; ++i ) {
                for ( j = 0; j < 16; ++j ) {
                    dct[i][j] = 0;
                }
            }

            // Compute one step of the DCT based on the pixel
            // that we are looking at
            double pixel;
            for ( u = 0; u < 4; ++u ) {
                for ( v = 0; v < 4; ++v ) {
                    for ( j = 0; j < 16; ++j ) {
                        for ( i = 0; i < 16; ++i ) {
                            pixel = data->pixels[j + d][a + i];
                            dct[u][v] += pixel * data->pre[u][v][i][j];
                        }
                    }
                }
            }

            // Here the DCT has been computed and needs to be quantized
            for ( u = 0; u < 16; ++u ) {
                for ( v = 0; v < 16; ++v ) {
                    dct[u][v] *= (u==0)?sqrt_116:sqrt_216;
                    dct[u][v] *= (v==0)?sqrt_116:sqrt_216;
                    dct[u][v] /= data->quality;
                    dct[u][v] /= data->q16[u][v];
                    dct[u][v] = round( dct[u][v] );
                }
            }

            // Now take the resulting quantized DCT matrix and insert
            // it into the matrix
            for ( i = 0; i < 16; ++i ) {
                for ( j = 0; j < 16; ++j ) {
                    matrix[ i_pos * 16 * 16 + j + 16 * i ] =
                        (int)dct[i][j];
                }
            }
            ++i_pos;
        }
    }
    return (void*)0;
}