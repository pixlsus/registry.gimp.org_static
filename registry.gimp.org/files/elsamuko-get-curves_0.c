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
#include <time.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <glib.h>
#include <glib/gprintf.h>

#define PLUG_IN_PROC    "elsamuko-get-curves"
#define PLUG_IN_BINARY  "elsamuko-get-curves"

/* local function prototypes */
inline gint coord( gint x, gint y, gint k, gint channels, gint width ) {
    return channels*( width*y + x ) + k;
};

static void      writeChannel(FILE *file,
                              char *channel,
                              gdouble *data,
                              int dummy);
static void      firstNonZero(gdouble* red);
static void      lastNonOne(gdouble* red);
static void      query( void );
static void      run( const gchar      *name,
                      gint              nparams,
                      const GimpParam  *param,
                      gint             *nreturn_vals,
                      GimpParam       **return_vals );
static void      getcurves( GimpDrawable   *drawable );

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
        { GIMP_PDB_INT32,    "run-mode", "The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    "image",    "Input image (unused)"         },
        { GIMP_PDB_DRAWABLE, "drawable", "Input drawable"               },
    };

    gimp_install_procedure( PLUG_IN_PROC,
                            "Calculate RGB curves from two images",
                            "Calculate RGB curves from two images",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "2010",
                            "_Get RGB Curves...",
                            "RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS( args ), 0,
                            args, NULL );

    gimp_plugin_menu_register( PLUG_IN_PROC, "<Image>/Colors" );
}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
    GimpDrawable      *drawable;
    GimpRunMode        run_mode;
    GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
    static GimpParam   values[1];

    values[0].type          = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    *nreturn_vals = 1;
    *return_vals  = values;

    if (strcmp (name, PLUG_IN_PROC) != 0 || nparams < 3)
    {
        values[0].data.d_status = GIMP_PDB_CALLING_ERROR;
        return;
    }

    run_mode = param[0].data.d_int32;
    drawable = gimp_drawable_get (param[2].data.d_drawable);

    /*
     *  Make sure the drawable type is appropriate.
     */
    if (gimp_drawable_is_rgb (drawable->drawable_id) ||
            gimp_drawable_is_gray (drawable->drawable_id))
    {
        gimp_progress_init ("Calculating...");

        getcurves (drawable);

        if (run_mode != GIMP_RUN_NONINTERACTIVE)
        {
            gimp_displays_flush ();
        }
    }
    else
    {
        status = GIMP_PDB_EXECUTION_ERROR;
    }

    values[0].data.d_status = status;
    gimp_drawable_detach (drawable);
}

static void getcurves( GimpDrawable *drawable ) {

    printf( "\nL%i:****Begin of getcurves.****\n", __LINE__ );

    const gint32 image_ID = gimp_drawable_get_image( drawable->drawable_id );
    
    gimp_image_undo_group_start( image_ID );
    const gint width  = gimp_image_width( image_ID );
    const gint height = gimp_image_height( image_ID );

    // define layers
    gint num_of_layers;
    gint *layers = gimp_image_get_layers( image_ID, &num_of_layers );
    if (num_of_layers < 2 ) {
        gimp_message("Error: Number of layers is less than two.");
        printf( "L%i: Error: Number of layers is less than two.", __LINE__);
        return;
    }
    gint firstlayer = layers[0];
    gint secondlayer = layers[1];

    // add alpha if necessary
    gimp_layer_add_alpha( firstlayer );
    gimp_layer_add_alpha( secondlayer );

    // define drawables
    GimpDrawable *firstdrawable = gimp_drawable_get( firstlayer );
    GimpDrawable *seconddrawable = gimp_drawable_get( secondlayer );
    const gint channels = gimp_drawable_bpp( drawable->drawable_id );

    printf( "L%i: Image ID: %i\n",          __LINE__, image_ID );
    printf( "L%i: Number of Layers: %i\n",  __LINE__, num_of_layers );
    printf( "L%i: Firstlayer: %i\n",        __LINE__, firstlayer );
    printf( "L%i: Secondlayer: %i\n",       __LINE__, secondlayer );
    printf( "L%i: Number of Channels: %i\n",__LINE__, channels );

    // select Regions
    GimpPixelRgn prFirst;
    GimpPixelRgn prSecond;

    gimp_pixel_rgn_init( &prFirst,
                         firstdrawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    gimp_pixel_rgn_init( &prSecond,
                         seconddrawable,
                         0, 0,
                         width, height,
                         FALSE, FALSE );
    printf( "L%i: Pixel regions initiated.\n", __LINE__ );

    guchar *rectFirst;
    guchar *rectSecond;

    // initialise memory
    rectFirst = g_new( guchar, channels * width * height );
    rectSecond = g_new( guchar, channels * width * height );

    // save stereo images in array
    gimp_pixel_rgn_get_rect( &prFirst,
                             rectFirst,
                             0, 0,
                             width, height );
    gimp_pixel_rgn_get_rect( &prSecond,
                             rectSecond,
                             0, 0,
                             width, height );
                             

    // algorithm begins here:
    printf( "L%i: Begin algorithm.\n", __LINE__ );
    gint x;
    gint y;
    int c;

    // sums of values
    gdouble red[256];
    gdouble green[256];
    gdouble blue[256];

    // frequencies
    gdouble red_f[256];
    gdouble green_f[256];
    gdouble blue_f[256];

    // defaults
    for ( c = 0; c < 256; c++ ) {
        red[c]   = 0.0;
        green[c] = 0.0;
        blue[c]  = 0.0;
        red_f[c]   = 0.0;
        green_f[c] = 0.0;
        blue_f[c]  = 0.0;
    }
    
    //iterate over image
    for ( y = 0; y < height; y++ ) {
        for ( x = 0; x < width; x++ ) {
            c = (int) rectSecond[coord( x, y, 0, channels, width )];
            // if(c>255) printf( "(%i,%i):%i\n",x,y,c);
            red[c] += (gint) rectFirst[coord( x, y, 0, channels, width )];
            red_f[c]++;
            
            c = (int) rectSecond[coord( x, y, 1, channels, width )];
            green[c] += (gint) rectFirst[coord( x, y, 1, channels, width )];
            green_f[c]++;
             
            c = (int) rectSecond[coord( x, y, 2, channels, width )];
            blue[c] += (gint) rectFirst[coord( x, y, 2, channels, width )];
            blue_f[c]++;
        }

        if ( y % 10 == 0 ) {
            gimp_progress_update(( gdouble )( y / ( gdouble ) gimp_image_height( image_ID ) ) );
        }
    }

    //free memory
    g_free( rectFirst );
    g_free( rectSecond );

    
    //calculate new values
    for ( c = 0; c < 256; c++ ) {
        
        if(red_f[c])   red[c]   /= 255.0*red_f[c];
        if(green_f[c]) green[c] /= 255.0*green_f[c];
        if(blue_f[c])  blue[c]  /= 255.0*blue_f[c];
        
        if(red[c]>1.0)   red[c]   = 1.0;
        if(green[c]>1.0) green[c] = 1.0;
        if(blue[c]>1.0)  blue[c]  = 1.0;
    }
    
    
    //set first value to non-zero
    firstNonZero(red);
    firstNonZero(green);
    firstNonZero(blue);

    //set last values to non-one
    lastNonOne(red);
    lastNonOne(green);
    lastNonOne(blue);

    //remove roots
    for ( c = 1; c < 256; c++ ) {
        
        if(!red_f[c])   red[c]   = red[c-1];
        if(!green_f[c]) green[c] = green[c-1];
        if(!blue_f[c])  blue[c]  = blue[c-1];
        
    }
    
    printf( "L%i: End algorithm.\n", __LINE__ );

    //write into curves file
    printf( "L%i: Write data to curve file.\n", __LINE__ );
    FILE *file;
    gint i,j;
    gint error = FALSE;

    char filename[100];
    char pathname[PATH_MAX + 1];
    
    time_t rawtime;
    time ( &rawtime );
    strftime (filename, 100,"curve_%Y-%m-%d_%H:%M:%S",localtime ( &rawtime ));
    sprintf (pathname, "%s/curves/%s", gimp_directory(), filename);
    printf( "L%i: Filename:%s\n", __LINE__ , pathname);

    file = fopen (pathname,"w");
    if (file != NULL)
    {
        //begin
        fputs ( "# GIMP curves tool settings\n\n", file );
        
        // dummies
        gdouble value[256];
        gdouble alpha[256];

        //channels
        writeChannel(file, "value", value, 1);
        writeChannel(file, "red",   red,   0);
        writeChannel(file, "green", green, 0);
        writeChannel(file, "blue",  blue,  0);
        writeChannel(file, "alpha", value, 1);

        //end
        fputs ( "# end of curves tool settings\n", file );
        
        fclose ( file );

    } else {
        gimp_message("Error: Could not open curves file.");
        printf( "L%i: Error: Could not open curves file.", __LINE__);
        error = TRUE;
    }

    //end
    gimp_image_undo_group_end( image_ID );
    printf( "L%i:****End of getcurves.****\n", __LINE__ );
}

static void writeChannel( FILE* file, char* channel, gdouble* data , int dummy) {

    // header
    fputs ( "(time 0)\n", file );
    fprintf ( file, "(channel %s)\n", channel);
    fputs ( "(curve\n", file );
    fputs ( "       (curve-type free)\n", file );
    fputs ( "       (n-points 17)\n", file );
    
    fputs ( "       (points 34", file );
    int i;
    for (i=0; i<34; i++) {
        fputs ( " -1.0", file );
    }
    fputs ( ")\n", file );
    
    fprintf ( file, "       (n-samples 256)\n");
    fprintf ( file, "       (samples 256");

    // dummy for alpha and value
    if (dummy) {
        for (i=0; i<256; i++) {
            gchar value[100];
            g_ascii_formatd (value, 100, "%f", i/255.0);
            fprintf ( file, " %s", value);
        }
    }
    
    // r,g,b
    else {
        for (i=0; i<256; i++) {
            gchar value[100];
            g_ascii_formatd (value, 100, "%f", data[i]);
            fprintf ( file, " %s", value);
        }
    }
    
    // end
    fprintf ( file, "))\n");
    
}

static void firstNonZero(gdouble* red){
    gdouble red_first=0;
    int c=0;
    
    while(red[c]==0){
        c++;
    }
    red_first=red[c];
    
    c=0;
    while(red[c]==0){
        red[c]=red_first;
        c++;
    }
}

static void lastNonOne(gdouble* red){
    gdouble red_last=0;
    int c=255;
    
    while(red[c]==0){
        c--;
    }
    red_last=red[c];
    
    c=255;
    while(red[c]==0){
        red[c]=red_last;
        c--;
    }
}

