/*
* Copyright (C) 1999 Winston Chang
*                    <winstonc@cs.wisc.edu>
*                    <winston@stdout.org>
* Copyright (C) 2009 elsamuko
*                    <elsamuko@web.de>
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

// simple GIMP-octave interface, compile with:
// gimptool-2.0 --install gimp-octave.c

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/param.h>

#include <gtk/gtk.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#include "embed.h"

#define PLUG_IN_PROC    "elsamuko-gimp-octave"
#define PLUG_IN_BINARY  "elsamuko-gimp-octave"
#define WORKING_DIR     ".gimp-octave"
#define MATRIX_IN       "matrix_in.txt"
#define MATRIX_OUT      "matrix_out.txt"
#define OCTAVE_SCRIPT        "gimp.m"
#define USE_OCTAVE_EMBEDDED FALSE

typedef struct
{
    char cinput[PATH_MAX + 1];
    char coutput[PATH_MAX + 1];
    char cscript[PATH_MAX + 1];
    char workingdir[PATH_MAX + 1];
} OctaveParams;


typedef struct
{
    gboolean  run;
} OctaveInterface;


/* local function prototypes */
inline gint coord( gint x, gint y, gint k, gint channels, gint width ) {
    return channels*( width*y + x ) + k;
};
void             reset_default();
gint             write_matrix(guchar *layerIn,
                              char   *filename,
                              gint    bpp,
                              gint    height,
                              gint    width);
gint             read_matrix(guchar *layerOut,
                             char   *filename,
                             gint    bpp,
                             gint    height,
                             gint    width);
static void      query (void);
static void      run   (const gchar      *name,
                        gint              nparams,
                        const GimpParam  *param,
                        gint             *nreturn_vals,
                        GimpParam       **return_vals);
static void      octave_region     (GimpPixelRgn   *srcPTR,
                                    GimpPixelRgn   *dstPTR,
                                    gint            bpp,
                                    gint            x,
                                    gint            y,
                                    gint            width,
                                    gint            height);
static void      octave             (GimpDrawable   *drawable);
static gboolean  octave_dialog      (GimpDrawable   *drawable);
static void      preview_update     (GimpPreview    *preview);


/* create a few globals, set default values */
static OctaveParams octave_params =
{
    "" /* default input       */
    "" /* default output      */
    "" /* default script      */
    "" /* default working dir */
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
        { GIMP_PDB_INT32,    (gchar*)"run-mode",  (gchar*)"The run mode { RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }" },
        { GIMP_PDB_IMAGE,    (gchar*)"image",     (gchar*)"Image" },
        { GIMP_PDB_DRAWABLE, (gchar*)"drawable",  (gchar*)"Drawable to draw on" }
        //{ GIMP_PDB_STRING,   (gchar*)"input",     (gchar*)"Filename of the input matrix (nyi)" },
        //{ GIMP_PDB_STRING,   (gchar*)"output",    (gchar*)"Filename of the output matrix (nyi)" },
        //{ GIMP_PDB_STRING,   (gchar*)"script",    (gchar*)"Filename of the Octave script (nyi)" }
    };

    gimp_install_procedure (PLUG_IN_PROC,
                            "Call Gnu Octave from GIMP",
                            "Call Gnu Octave from GIMP",
                            "elsamuko <elsamuko@web.de>",
                            "elsamuko",
                            "1999-2010",
                            "_GIMP Octave",
                            "GRAY*, RGB*",
                            GIMP_PLUGIN,
                            G_N_ELEMENTS (args), 0,
                            args, NULL);

    gimp_plugin_menu_register (PLUG_IN_PROC, "<Image>/Filters/Mathematics");
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

    // set working env
    reset_default();

    run_mode = (GimpRunMode)param[0].data.d_int32;

    *return_vals  = values;
    *nreturn_vals = 1;

    values[0].type          = GIMP_PDB_STATUS;
    values[0].data.d_status = status;

    /*
    * Get drawable information...
    */
    drawable = gimp_drawable_get (param[2].data.d_drawable);
    gimp_tile_cache_ntiles (2 * MAX (drawable->width  / gimp_tile_width () + 1 ,
                                     drawable->height / gimp_tile_height () + 1));

    switch (run_mode)
    {
    case GIMP_RUN_INTERACTIVE:
        gimp_get_data (PLUG_IN_PROC, &octave_params);
        /* Reset default values show preview unmodified */

        /* initialize pixel regions and buffer */
        if (! octave_dialog (drawable))
            return;
        break;

    case GIMP_RUN_NONINTERACTIVE:
        if (nparams != 3)
        {
            status = GIMP_PDB_CALLING_ERROR;
        }
        else
        {
            //reset_default();
        }
        break;

    case GIMP_RUN_WITH_LAST_VALS:
        gimp_get_data (PLUG_IN_PROC, &octave_params);
        break;

    default:
        break;
    }

    if (status == GIMP_PDB_SUCCESS)
    {
        drawable = gimp_drawable_get (param[2].data.d_drawable);

        /* here we go */
        octave (drawable);

        gimp_displays_flush ();

        /* set data for next use of filter */
        if (run_mode == GIMP_RUN_INTERACTIVE)
            gimp_set_data (PLUG_IN_PROC, &octave_params, sizeof (OctaveParams));

        gimp_drawable_detach(drawable);
        values[0].data.d_status = status;
    }
}


static void
octave (GimpDrawable *drawable)
{
    GimpPixelRgn srcPR, destPR;
    gint         x1, y1, x2, y2;
    gint         x, y, width, height;

    /* initialize pixel regions */
    gimp_pixel_rgn_init (&srcPR, drawable,
                         0, 0, drawable->width, drawable->height, FALSE, FALSE);
    gimp_pixel_rgn_init (&destPR, drawable,
                         0, 0, drawable->width, drawable->height, TRUE, TRUE);

    /* Get the input */
    gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);

    x=x1;
    y=y1;
    width=x2-x1;
    height=y2-y1;

    /* Run */
    octave_region (&srcPR, &destPR, drawable->bpp, x, y, width, height);

    gimp_drawable_flush (drawable);
    gimp_drawable_merge_shadow (drawable->drawable_id, TRUE);
    gimp_drawable_update (drawable->drawable_id, x1, y1, x2 - x1, y2 - y1);
}


///--------------------------------------------------------------------------------------------------------------------------
gint
read_matrix(guchar *layerOut,
            char   *filename,
            gint    bpp,
            gint    height,
            gint    width)
{
    gint i,j,k,num;
    FILE *matrix_out;
    gint rbpp, rwidth, rheight;
    gint error = FALSE;

    matrix_out = fopen (octave_params.coutput,"r");
    if (matrix_out != NULL)
    {
        // # name: matrix_out
        // # type: matrix
        // # ndims: 3
        char first = '#';
        char str[101];
        while (first == '#') {
            fgets ( str, 100, matrix_out );
            first=str[0];
        }
        sscanf ( str, "%i %i %i", &rheight, &rwidth, &rbpp);
        printf( "L%i: rwidth = %i, rheight = %i, rbpp = %i\n", __LINE__, rheight, rwidth, rbpp);

        if (width==rwidth && height==rheight && bpp==rbpp) { //check size
            for ( k = 0; k < bpp; k++ ) {
                for ( i = 0; i < width; i++ ) {
                    for ( j = 0; j < height; j++ ) {
                        fscanf ( matrix_out, "%i", &num);
                        layerOut[coord( i, j, k, bpp, width )] = num;
                    }
                }
            }
        } else {
            gimp_message("Error: Size of return matrix is not equal to input matrix.");
            printf( "L%i: Error: Size of return matrix is not equal to input matrix.\n", __LINE__);
            error = TRUE;
        }
        fclose (matrix_out);
    }
    else {
        gimp_message("Error: Could not open return matrix.");
        printf( "L%i: Error: Could not open return matrix.\n", __LINE__);
        error = TRUE;
    }

    return error; //zero, if there is no problem
};

gint
write_matrix(guchar *layerIn,
             char   *filename,
             gint    bpp,
             gint    height,
             gint    width)
{
    gint i,j,k;
    FILE *matrix_in;
    gint error = FALSE;

    matrix_in = fopen( octave_params.cinput, "w" );
    if ( matrix_in != NULL )
    {
        // # Created by GIMP-Octave
        // # name: matrix_in
        // # type: matrix
        // # ndims: 3
        fputs ("# Created by GIMP-Octave\n", matrix_in);
        fputs ("# name:  matrix_in\n", matrix_in);
        fputs ("# type:  matrix\n", matrix_in);
        fprintf ( matrix_in, "# ndims: %i\n", 3);
        fprintf ( matrix_in, "%i %i %i\n", height, width, bpp);

        for ( k = 0; k < bpp; k++ ) {
            for ( i = 0; i < width; i++ ) {
                for ( j = 0; j < height; j++ ) {
                    fprintf ( matrix_in, "%i\n", (gint)layerIn[coord( i, j, k, bpp, width )]);
                }
            }
        }
        fclose (matrix_in);
    } else {
        gimp_message("Error: Could not open input matrix.");
        printf( "L%i: Error: Could not open input matrix.\n", __LINE__);
        error = TRUE;
    }

    return error; //zero, if there is no problem
};

/*
*  Write the layer to an octave-readable matrix,
*  then start the octave script, which reads,
*  modifies and returns a matrix, which is then
*  read back in by this plugin.
*/
static void
octave_region (GimpPixelRgn *srcPR,
               GimpPixelRgn *destPR,
               gint          bpp,
               gint          x,
               gint          y,
               gint          width,
               gint          height)
{
    guchar *layerIn;
    guchar *layerOut;
    int ret;
    gint error = FALSE;

    layerIn  = g_new( guchar, bpp * width * height );
    layerOut = g_new( guchar, bpp * width * height );
    gimp_pixel_rgn_get_rect( srcPR,
                             layerIn,
                             x, y,
                             width, height );

    printf( "\nL%i: -----------------------Begin Octave------------------------\n", __LINE__);

    /*
     *  GIMP -> MatrixIn
     */
    error = write_matrix(layerIn, octave_params.cinput,
                         bpp, height, width);

    /*
     *  MatrixIn -> Octave -> MatrixOut
     */
    if (!error) {
        if (USE_OCTAVE_EMBEDDED) { // run Octave embedded
            int argc = 4;
            char* argv[] = { (char*)"octave",
                             (char*)"--silent",
                             (char*)"--persist",
                             (char*)octave_params.cscript
                           };
            printf( "L%i: %s\n", __LINE__, (char*)octave_params.cscript);
            octave_init( argc, argv );
            octave_exit();
        } else { // run Octave via system call
            char command[PATH_MAX + 1];
            strcpy ( command, "octave --silent " );
            strcat ( command, octave_params.cscript );
            printf( "L%i: %s\n", __LINE__, command );
            ret = system ( command );
        }
    }

    /*
     *  MatrixOut -> GIMP
     */
    if (!error) {
        error = read_matrix(layerOut, octave_params.coutput,
                            bpp, height, width);
    }

    if (error) {
        gimp_pixel_rgn_set_rect( destPR, layerIn, x, y, width, height );
    } else { // this one should be default
        gimp_pixel_rgn_set_rect( destPR, layerOut, x, y, width, height );
    }

    g_free( layerIn );
    g_free( layerOut );
    printf( "L%i: -----------------------Finish Octave------------------------\n", __LINE__);
}
///--------------------------------------------------------------------------------------------------------------------------


static gboolean
octave_dialog (GimpDrawable *drawable)
{
    GtkWidget *dialog;
    GtkWidget *main_vbox;
    GtkWidget *preview;
    GtkWidget *button_update;
    gboolean   run;

    //default entries:
    reset_default();

    gimp_ui_init (PLUG_IN_BINARY, TRUE);

    dialog = gimp_dialog_new ("GIMP Octave", PLUG_IN_BINARY,
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

    // Preview
    preview = gimp_drawable_preview_new (drawable, NULL);
    gtk_box_pack_start (GTK_BOX (main_vbox), preview, TRUE, TRUE, 0);
    gtk_widget_show (preview);

    g_signal_connect (preview, "invalidated",
                      G_CALLBACK (preview_update),
                      NULL);

    // Update Button
    button_update = gtk_button_new_with_label("Update");
    gtk_box_pack_start (GTK_BOX (main_vbox), button_update, TRUE, TRUE, 0);
    gtk_widget_show (button_update);
    g_signal_connect_swapped (button_update, "released",
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
    GimpPixelRgn  srcPR;
    GimpPixelRgn  destPR;

    drawable = gimp_drawable_preview_get_drawable (GIMP_DRAWABLE_PREVIEW (preview));

    gimp_pixel_rgn_init ( &srcPR, drawable, 0, 0, drawable->width, drawable->height, FALSE, FALSE);
    gimp_pixel_rgn_init (&destPR, drawable, 0, 0, drawable->width, drawable->height, TRUE, TRUE);

    gimp_preview_get_position (preview, &x, &y);
    gimp_preview_get_size (preview, &width, &height);

    octave_region (&srcPR, &destPR, drawable->bpp, x, y, width, height);

    gimp_pixel_rgn_init (&destPR, drawable, x, y, width, height, FALSE, TRUE);
    gimp_drawable_preview_draw_region (GIMP_DRAWABLE_PREVIEW (preview), &destPR);
}

void reset_default() {
    char* home = getenv("HOME");

    // set standard input/output/script strings
    strcpy ( octave_params.cinput,  home );
    sprintf (octave_params.workingdir, "%s/%s", home, WORKING_DIR);
    sprintf (octave_params.cinput,  "%s/%s/%s", home, WORKING_DIR, MATRIX_IN);
    sprintf (octave_params.coutput, "%s/%s/%s", home, WORKING_DIR, MATRIX_OUT);
    sprintf (octave_params.cscript, "%s/%s/%s", home, WORKING_DIR, OCTAVE_SCRIPT);

    //create working dir, if it doesn't exist
    mkdir( octave_params.workingdir, S_IRWXU );

    // if there is no Octave script, create one:
    int acc = access( octave_params.cscript, R_OK );
    if (acc == -1)
    {
        FILE* pFile = fopen (octave_params.cscript,"w");
        if (pFile != NULL) {
            fputs ("%----Begin of Octave script, don't edit:--\n"
                   "cd ~/.gimp-octave\n"
                   "load matrix_in.txt;\n"
                   "size(matrix_in)\n"
                   "matrix_out = matrix_in;\n"
                   "%-----------------------------------------\n"
                   "\n"
                   "\n"
                   "\n"
                   "\n"
                   "%----Put your own stuff here:-------------\n"
                   "matrix_out(:,:,1)=fftshift(matrix_out(:,:,1));\n"
                   "\n"
                   "\n"
                   "%-----------------------------------------\n"
                   "\n"
                   "\n"
                   "\n"
                   "\n"
                   "%----End of Octave script, don't edit:----\n"
                   "matrix_out = round(matrix_out);\n"
                   "matrix_out(matrix_out(:)<0)=0;\n"
                   "matrix_out(matrix_out(:)>255)=255;\n"
                   "size(matrix_out)\n"
                   "dims = ndims (matrix_out)\n"
                   "if (dims == 2)\n"
                   "    matrix_out = resize (matrix_out, [size(matrix_out, 1), size(matrix_out, 2), 1]);\n"
                   "end\n"
                   "% If you want to save the output matrix, uncomment the following line:\n"
                   "% save(strcat(\"matrix_\", strftime (\"%Y-%m-%d_%H-%M-%S\",localtime(time)),\".txt\"), \"matrix_out\");\n"
                   "save matrix_out.txt matrix_out;\n"
                   "%-----------------------------------------\n"
                   "\n"
                   "\n",pFile);
            fclose (pFile);
        }
    }
}

//EOF

