/*
 * This work is licensed under the Creative Commons Attribution 2.5
 * Canada License. To view a copy of this license, visit
 * http://creativecommons.org/licenses/by/2.5/ca/ or send a letter to
 * Creative Commons, 171 Second Street, Suite 300, San Francisco,
 * California, 94105, USA.
 *
 * *********************************************************
 * * UPSIZE: An image upsizing plugin featuring two modes. *
 * *****************************************************************************
 * * 1. Smooth (IBFNBQH): Image upsizing with Interpolatory Box Filtered       *
 * *                      Natural BiQuadratic Histosplines.                    *
 * *                      [This is the default option.]                        *
 * *                                                                           *
 * * 2.  Sharp  (EANBQH): Exact Area image upsizing with Natural BiQuadratic   *
 * *                      Histosplines.                                        *
 * *****************************************************************************
 *
 * For more details regarding these methods, see Fast Exact Area Image
 * Upsampling with Natural Biquadratic Histosplines by Nicolas
 * Robidoux, Adam Turcotte, Minglun Gong and Annie Tousignant,
 * pp.85-96 of Image Analysis and Recognition, 5th International
 * Conference, ICIAR 2008, Póvoa de Varzim, Portugal, June 25-27,
 * 2008. Proceedings, Aurelio C. Campilho, Mohamed S. Kamel (Eds.).
 * Lecture Notes in Computer Science 5112, Springer 2008, ISBN
 * 978-3-540-69811-1. Only the "Sharp" version is explicitly
 * discussed; "Smooth" differs in that it is box filtered (and in that
 * this implementation uses a different image size convention than the
 * "Sharp" resampler).
 */

/*
 * Upsize -- image resizing plug-in for The Gimp image manipulation
 * program Copyright (C) 2009 Nicolas Robidoux and Adam Turcotte
 *
 * LAST UPDATED:
 * March 25, 2011
 *  - Fixed Smooth/Sharp radio button callback.
 *  - Uses gimp_image_insert_layer() for Gimp 2.7 and newer
 *     instead of gimp_image_add_layer(), which has been deprecated.
 *
 * INSTALLATION INSTRUCTIONS:
 * Make sure the libgimp2.0-dev package is installed, then type:
 * gimptool-2.0 --install upsize.c
 *
 * NOTE:
 * This current version of the code has some unneeded
 * redundancies. Manana.
 */

/*
 * Natural boundary conditions mean that the normal derivative of the
 * histopolant (average matching reconstructed intensity profile) is
 * zero at the boundary. A alternate version of the code could use
 * not-a-knot boundary conditions instead (weaker variational
 * principle but would produce a scheme which is exact on affine
 * intensity profiles).
 */

/*
 * This source code can generate plug-ins which use either 16bit or
 * 8bit arrays to store the B-Spline coefficients after they have been
 * computed "in the vertical direction."
 *
 * More details: This tensor scheme requires one tridiagonal solve per
 * column, and one tridiagonal solve per row. The column solves are
 * performed first, with the results stored in integer arrays. The
 * results are retrieved when the row solves are performed.
 *
 * Storing the intermediate results in 8bit saves approximately half
 * of the memory compared to 16bit (not counting the input and output
 * images themselves), at the cost of introducing intensity errors
 * which are at most 1.
 *
 * If EIGHT_BIT_STORAGE is #defined, the half-computed B-spline
 * coefficients will be stored in an 8bit integer array. Otherwise,
 * they will be stored in a 16bit integer array.
 */
/* #define EIGHT_BIT_STORAGE */

/*
 * Increase these limits on the image dimensions as appropriate. To
 * our surprise, we have not encountered problems with index overflow
 * (in coarse_to_fine_coefficients, notably).
 */
#define MAX_WIDTH  65534
#define MAX_HEIGHT 65534

/*
 * This source code can generate executables which printf short
 * reports of time spent if the gimp was started from a terminal.
 *
 * If TIMING_REPORT is #defined, the time elapsed in the key part of
 * the program is sent to standard output.
 */
/* #define TIMING_REPORT */

/*
 * The ".5f +" is so that truncation (which the cast from float to int
 * effects for positive values) performs round to nearest. Negative
 * values are clamped later on to 0, so the fact that truncation
 * brings things "up" for negative results is not a concern. Halfway
 * points between nonnnegative integers are mapped to the larger
 * integer, which is acceptable---maybe even good if the image is
 * overexposed---but not important: round to nearest, with any
 * convention (banker's, toward zero---which may have advantages---or
 * toward +infty, which is the case now, would also be fine).  Note
 * that an implicit cast to guchar is performed after clamping.
 */
#define F_ROUND_AND_CLAMP_0255(x)                                       \
  ({                                                                    \
    gint rx = .5f + (x);                                                \
    guchar out =                                                        \
     (rx>=0) ? ( (rx<=255) ? (guchar)rx : (guchar)255 ) : (guchar)0;    \
    out;                                                                \
  })

/*
 * Key matrix factorization coefficients when computing B-Spline
 * coefficients:
 *
 * For natural boundary conditions:
 * C0      = 1/5
 * C1      = 5/19                = 1 / ( 4 - C0 )
 * C2      = 19/71               = 1 / ( 4 - C1 )
 * C3      = 71/265              = 1 / ( 4 - C2 )
 * C4      = 265/989             = 1 / ( 4 - C3 )
 * C5      = 989/3691            = 1 / ( 4 - C4 )
 * CINFTY  = 2 - sqrt(3)         = limit of this recurrence relation
 * CLAST   = 1 / ( 5 - C_INFTY ) = ( 3 - sqrt(3) ) / 6
 *
 * Convergence of the coefficients for natural boundary conditions: In
 * 32 bit single precision, C6 and all subsequent ones are equal to
 * CINFTY. Although C5 is different from CINFTY in this precision, it
 * differs from it only by 1.271371644 * 10^-7, just enough to be
 * different in standard single precision, but nonetheless an
 * insignificant amount (.2679491f instead of .2679492f). However, in
 * the interest of preventing integer overflow, we actually set use C5
 * as a proxy for CINFTY in the computations which take place prior to
 * storing partically computed results into int8s. This is discussed
 * further below.
 */
#define C0_F        .2000000f
#define MINUS_C0_F -.2000000f
#define C1_F        .2631579f
#define MINUS_C1_F -.2631579f
#define C2_F        .2676056f
#define MINUS_C2_F -.2676056f
#define C3_F        .2679245f
#define MINUS_C3_F -.2679245f
#define C4_F        .2679474f
#define MINUS_C4_F -.2679474f
#define C5_F        .2679491f
#define MINUS_C5_F -.2679491f
#define CLAST_F     .2113249f
/*
 * In the interest of making sure that we don't overflow when
 * converting partially computed coefficients to integer data types,
 * in the computations which come prior to the cast we use C5 as a
 * proxy for CINFTY: C5 turns out to be the truncated value of CINFTY,
 * which is what we want. In the computations which take place after
 * the partially computed B-spline coefficients have been converted
 * back to floats, we use the correctly rounded value of CINFTY.
 */
#define CINFTY_F        .2679492f
#define MINUS_CINFTY_F -.2679492f

/*
 * Implicitly, the code rescales the 0..255 uchar values so that they
 * fit in -255 to 255. Because the infinity norm of the inverse of the
 * tensor component of the "interpolation matrix" is 1/2, the computed
 * coefficients fit into -127.5 to 127.5, which we later encode into 0
 * to 65535.
 *
 * The purpose of FIT_TO_RANGE is to turn pixel values ranging from 0
 * to 255 to values ranging from -255 to 255. Values in this range are
 * mapped (arbitrarily close to when the matrix is large) values
 * strictly in the range -127.5 to 127.5 by the inverse of the matrix
 * A =
 *
 * 5 1
 * 1 4 1
 *   1 4 1
 *     1 4 1
 *       1 4 1
 *         ...
 *           1 5
 *
 * which is the matrix for natural boundary conditions.
 *
 * FIT_TO_SCALE scales by a factor of 2 (and shifts).
 *
 * One can understand the factor of two as being part of solving when
 * the matrix is actually half the one shown above. This half-matrix
 * has an interpretation in terms of rescaling B-splines.
 *
 * The advantage of this interpretation is that this scaling
 * (asymptotically) keeps the range invariant:
 *
 * If all entries of y are within -127.5 to 127.5 (inclusive), and x
 * is the solution of A'x = y, then all entries of x are within the
 * same range (exclusive), provided
 *
 * A' =
 * 2.5 .5
 * .5   2 .5
 *     .5  2 .5
 *        .5  2 .5
 *            ...
 *           .5 2.5
 *
 * Halving the matrix is equivalent to doubling the RHS.
 *
 * Yet a third interpretation: In order to store results using the
 * first version of the matrix, we double them (at any stage), and
 * then have to halve them when we want to use them.
 *
 * The important thing to keep in mind for later is that the values
 * are scaled by 2.
 */
#define FIT_TO_RANGE(x) ( 2 * ( (gint) (x) ) - 255 )
/*
 * The above can be rewritten using integer arithmetic, at the
 * cost of an additional (explicit) intermediate cast:
 */
/* #define FIT_TO_RANGE(x) ({gint in = (x); (in+in) - 255;}) */

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <math.h>
#include <stdlib.h>

/* Struct for scale parameters: */
typedef struct
{
  gint     width;
  gint     height;
  gboolean mode; /* 0 = smooth, 1 = sharp */
} ScaleVals;

/* Enum for reset button: */
enum {
  RESPONSE_RESET
};

/* Use svals to access scale params. */
static ScaleVals svals;

/* Dialog callback */
gdouble _upsize_width;
gdouble _upsize_height;

/* Aspect ratio */
gdouble _upsize_m_over_n;

static void query (void);

static void run   (const gchar      *name,
                         gint        nparams,
                   const GimpParam  *param,
                         gint       *nreturn_vals,
                         GimpParam **return_vals);

static gint32 scale_up_smooth (gint32        image_ID,
                               GimpDrawable *drawable,
                               gint          m,
                               gint          n,
                               gint          mm,
                               gint          nn,
                               gint          channels);

static void first_past_index (gint o,
                              gint oo,
                              gint first_past_kk[]);

static void smooth_coarse_to_fine_coefficients (gint   o,
                                                gint   oo,
                                                gint   first_past_kk[],
                                                gfloat left[],
                                                gfloat center[],
                                                gfloat right[],
                                                gfloat farright[]);

static gint32 scale_up_sharp (gint32        image_ID,
                              GimpDrawable *drawable,
                              gint          m,
                              gint          n,
                              gint          mm,
                              gint          nn,
                              gint          channels);

static void last_overlapping_index (gint o,
                                    gint oo,
                                    gint last_overlapping_kk[]);

static void coarse_to_fine_coefficients (gint   o,
                                         gint   oo,
                                         gint   last_overlapping_kk[],
                                         gfloat left[],
                                         gfloat center[],
                                         gfloat right[],
                                         gfloat farright[]);

static gboolean scale_dialog (gint32 image_ID,
                              gint   m,
                              gint   n);

static void scale_callback (GtkWidget *widget,
                            gpointer   data);

static void set_mode (GtkRadioButton *button,
                      ScaleVals      *svals );

GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,
  NULL,
  query,
  run
};

MAIN()

static void
query (void)
{
  gchar *help_path;
  gchar *help_uri;

  static GimpParamDef args[] =
  {
    { GIMP_PDB_INT32,    "run-mode",  "Run mode"         },
    { GIMP_PDB_IMAGE,    "image",     "Input image"      },
    { GIMP_PDB_DRAWABLE, "drawable",  "Input drawable"   },
    { GIMP_PDB_INT32,    "width",     "Width of output"  },
    { GIMP_PDB_INT32,    "height",    "Height of output" }
  };

  static GimpParamDef ret[] =
  {
    { GIMP_PDB_IMAGE,    "image_out", "Output image"     }
  };

  /* use gimp_data_directory()/upsize-help for path */
  help_path = g_build_filename (gimp_data_directory(), "upsize-help", NULL);
  help_uri = g_filename_to_uri (help_path, NULL, NULL);
  gimp_plugin_help_register (help_path, help_uri);
  g_free (help_path);
  g_free (help_uri);

  gimp_install_procedure (
    "Upsize",
    "Upsize",
    "Image enlargement with biquadratic histosplines",
    "Nicolas Robidoux, Adam Turcotte",
    "Copyright Nicolas Robidoux, Adam Turcotte",
    "2007--2009",
    "_Upsize",
    "RGB*, GRAY*",
    GIMP_PLUGIN,
    G_N_ELEMENTS (args),
    G_N_ELEMENTS (ret),
    args,
    ret);

  gimp_plugin_menu_register ("Upsize",
                             "<Image>/Image");
}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
#ifdef TIMING_REPORT
  GTimer *timer;
#endif

  static GimpParam  values[2];
  GimpPDBStatusType status = GIMP_PDB_SUCCESS;
  GimpRunMode       run_mode;
  GimpDrawable     *drawable;
  gint32            image_ID;
  gint32            new_image_ID;

  gint m;        /* Height (in pixels) of the input image. */
  gint n;        /* Width  (in pixels) of the input image. */
  gint mm;       /* Height (in pixels) of the output image. */
  gint nn;       /* Width  (in pixels) of the output image. */
  gint channels; /* Number of colour channels of the input image. */

  /*
   * Set mandatory output values:
   */
  *nreturn_vals = 2;
  *return_vals  = values;

  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = status;

  /*
   * Getting run_mode (don't display a dialog in NONINTERACTIVE mode):
   */
  run_mode = param[0].data.d_int32;

  image_ID = param[1].data.d_int32;

  /*
   * Get the specified drawable:
   */
  drawable = gimp_drawable_get (param[2].data.d_drawable);

  _upsize_height = m = drawable->height;
  _upsize_width = n = drawable->width;
  channels = gimp_drawable_bpp (drawable->drawable_id);

  /*
   * Display warning if the input image is not large enough:
   */
  if ( (m<7) || (n<8) )
  {
    g_message (
         "Only enlarges images at least\n8 pixels wide\nand 7 pixels tall.\n");
    return;
  }

  switch (run_mode)
  {
    svals.mode = 1;
    case GIMP_RUN_INTERACTIVE:
      /* Initialize: */
      svals.height = m;
      svals.width  = n;
      _upsize_m_over_n = (gdouble) m / n;
      /* Display the dialog: */
      if (! scale_dialog (image_ID, m, n))
        return;

      mm = svals.height;
      nn = svals.width;

      /*
       * Set up a tile cache large enough to contain a column of input
       * tiles, or a row of output tiles:
       */
      gimp_tile_cache_ntiles (((nn>m) ? nn : m) / gimp_tile_width () + 1);

#ifdef TIMING_REPORT
      timer = g_timer_new ();
#ifdef EIGHT_BIT_STORAGE
      g_print ("Upsize (with uint8 coefficient storage) is taking...\n");
#else
      g_print ("Upsize is taking...\n");
#endif
#endif
      if (svals.mode)
        new_image_ID =
          scale_up_sharp  (image_ID, drawable, m, n, mm, nn, channels);
      else
        new_image_ID =
          scale_up_smooth (image_ID, drawable, m, n, mm, nn, channels);
#ifdef TIMING_REPORT
      g_print ("%g seconds to complete the computation.\n",
               g_timer_elapsed (timer, NULL));
      g_timer_destroy (timer);
#endif
      values[1].type = GIMP_PDB_IMAGE;
      values[1].data.d_image = new_image_ID;
      gimp_displays_flush ();  /* flush to send data to core */
      gimp_drawable_detach (drawable); /* then detach */

      /* Set options in the core */
      gimp_set_data ("Upsize", &svals, sizeof (ScaleVals));

      if (new_image_ID != -1)
        gimp_display_new(new_image_ID);

      break;

    case GIMP_RUN_NONINTERACTIVE:
      if (nparams != 5)
        status = GIMP_PDB_CALLING_ERROR;
      if (status == GIMP_PDB_SUCCESS)
      {
        svals.width = param[3].data.d_int32;
        svals.height = param[4].data.d_int32;
        svals.mode = param[5].data.d_int32;
        mm = svals.height;
        nn = svals.width;

        /*
         * Set up a tile cache large enough to contain a column of input
         * tiles, or a row of output tiles:
         */
        gimp_tile_cache_ntiles (((nn>m) ? nn : m) / gimp_tile_width () + 1);

#ifdef TIMING_REPORT
        timer = g_timer_new ();
#ifdef EIGHT_BIT_STORAGE
        g_print ("Upsize (with uint8 coefficient storage) is taking...\n");
#else
        g_print ("Upsize is taking...\n");
#endif
#endif
      if (svals.mode)
        new_image_ID =
          scale_up_sharp  (image_ID, drawable, m, n, mm, nn, channels);
      else
        new_image_ID =
          scale_up_smooth (image_ID, drawable, m, n, mm, nn, channels);
#ifdef TIMING_REPORT
        g_print ("%g seconds to complete the computation.\n",
                 g_timer_elapsed (timer, NULL));
        g_timer_destroy (timer);
#endif
        values[1].type = GIMP_PDB_IMAGE;
        values[1].data.d_image = new_image_ID;
        gimp_displays_flush ();  /* flush to send data to core... */
        gimp_drawable_detach (drawable); /* ...then detach */

        /* Set options in the core */
        gimp_set_data ("Upsize", &svals, sizeof (ScaleVals));
      }
      break;

    case GIMP_RUN_WITH_LAST_VALS:
      gimp_get_data ("Upsize", &svals);
      mm = svals.height;
      nn = svals.width;

      /*
       * Set up a tile cache large enough to contain a column of input
       * tiles, or a row of output tiles:
       */
      gimp_tile_cache_ntiles (((nn>m) ? nn : m) / gimp_tile_width () + 1);

#ifdef TIMING_REPORT
      timer = g_timer_new ();
#ifdef EIGHT_BIT_STORAGE
      g_print ("Upsize (with uint8 coefficient storage) is taking...\n");
#else
      g_print ("Upsize is taking...\n");
#endif
#endif
      if (svals.mode)
        new_image_ID =
          scale_up_sharp  (image_ID, drawable, m, n, mm, nn, channels);
      else
        new_image_ID =
          scale_up_smooth (image_ID, drawable, m, n, mm, nn, channels);
#ifdef TIMING_REPORT
      g_print ("%g seconds to complete the computation.\n",
               g_timer_elapsed (timer, NULL));
      g_timer_destroy (timer);
#endif
      values[1].type = GIMP_PDB_IMAGE;
      values[1].data.d_image = new_image_ID;
      gimp_displays_flush ();  /* flush to send data to core */
      gimp_drawable_detach (drawable); /* then detach */

      break;

    default:
      break;
  }

  return;
}

static gint32
scale_up_smooth (gint32        image_ID,
                 GimpDrawable *drawable,
                 gint          m,
                 gint          n,
                 gint          mm,
                 gint          nn,
                 gint          channels)
{
  gint first_past_ii[m-1];
  gint first_past_jj[n-1];

  gfloat top[mm];
  gfloat middle[mm];
  gfloat bottom[mm];
  gfloat farbottom[mm];

  gfloat left[nn];
  gfloat center[nn];
  gfloat right[nn];
  gfloat farright[nn];

  GimpDrawable *new_drawable;
  gint32 new_image_ID;
  gint32 new_layer_ID;

  GimpPixelRgn input_image, output_image;

  /*
   * Useful iterator bounds:
   */
  gint nn_times_channels                = nn * channels;
  gint n_times_channels                 = n * channels;
  gint n_minus_1_times_channels         = (n-1) * channels;
  gint n_minus_7_times_channels         = (n-7) * channels;
  gint n_minus_8_times_channels         = n_minus_7_times_channels - channels;
  gint n_minus_2                        = n - 2;
  gint m_times_channels                 = m * channels;
  gint m_minus_6_times_channels         = (m - 6) * channels;
  gint m_minus_7_times_channels         = m_minus_6_times_channels - channels;
  gint m_minus_2                        = m - 2;

  /*
   * Utility iterators:
   */
  gint k; /* Index related to the number of pixel values in an image
             row (or column). If the image has, say, three channels,
             to traverse a row one needs to visit 3n entries; k is
             used for that. */
  gint c; /* Index (usually) related to the number of color channels
             (3 for RGB, 1 for Grayscale...). */

  gint kk;

  /*
   * Row and column indices:
   */
  gint i; /* Index of the input row under consideration. */
  gint j; /* Index of the input column under consideration. */

  gint ii; /* Index of the output row under consideration. */
  gint last_ii; /* Local loop bound for ii. */

  gint jj; /* Index of the output column under consideration. */
  gint last_jj; /* Local loop bound for jj. */

  /*
   * Computed coefficients/partial values:
   */
  gfloat top_ii;
  gfloat middle_ii;
  gfloat bottom_ii;
  gfloat farbottom_ii;

  gfloat left_jj;
  gfloat center_jj;
  gfloat right_jj;
  gfloat farright_jj;

#ifdef TIMING_REPORT
  GTimer *local_timer;
  gfloat local_time;
#endif

  gfloat a_top_left[channels];
  gfloat a_top_center[channels];
  gfloat a_top_right[channels];
  gfloat a_top_farright[channels];

  gfloat a_middle_left[channels];
  gfloat a_middle_center[channels];
  gfloat a_middle_right[channels];
  gfloat a_middle_farright[channels];

  gfloat a_bottom_left[channels];
  gfloat a_bottom_center[channels];
  gfloat a_bottom_right[channels];
  gfloat a_bottom_farright[channels];

  gfloat a_farbottom_left[channels];
  gfloat a_farbottom_center[channels];
  gfloat a_farbottom_right[channels];
  gfloat a_farbottom_farright[channels];

  gfloat coef_left[channels];
  gfloat coef_center[channels];
  gfloat coef_right[channels];
  gfloat coef_farright[channels];

  /*
   * Computation of the B-spline coefficients:
   */

  gfloat *a_row_p1, *a_row_p2;

  /*
   * Rescaled partially computed B-Spline coefficients are stored in
   * a.
   */
#ifdef EIGHT_BIT_STORAGE
  guchar *a, *a_ptr;
  a = g_new (guchar,  n * m_times_channels);
#else
  guint16 *a, *a_ptr;
  a = g_new (guint16, n * m_times_channels);
#endif

#ifdef TIMING_REPORT
  local_timer = g_timer_new ();
  local_time  = g_timer_elapsed (local_timer, NULL);
#endif

  first_past_index(m, mm, first_past_ii);
  first_past_index(n, nn, first_past_jj);

#ifdef TIMING_REPORT
  g_print ("%g seconds for the first_past_index calls.\n",
           g_timer_elapsed (local_timer, NULL)-local_time);
  local_time = g_timer_elapsed (local_timer, NULL);
#endif

  /*
   * Computation of the tensor components of the linear transformation
   * from B-spline coefficents to fine cell averages (actually fine
   * cell integrals, since we have already rescaled by the reciprocal
   * of the fine cell areas):
   */
  smooth_coarse_to_fine_coefficients(m, mm, first_past_ii,
                                     top, middle, bottom, farbottom);
  smooth_coarse_to_fine_coefficients(n, nn, first_past_jj,
                                     left, center, right, farright);

#ifdef TIMING_REPORT
  g_print ("%g seconds for the smooth_coarse_to_fine_coefficients calls.\n",
           g_timer_elapsed (local_timer, NULL)-local_time);
#endif

  /*
   * Initialize the input pixel region:
   */
  gimp_pixel_rgn_init (&input_image, drawable, 0, 0, n, m, FALSE, FALSE);

  /*
   * Set up an informative progress bar:
   */
  gimp_progress_init (
    "Upsizing with biquadratic histosplines");

  gimp_progress_update(.005);

#ifdef TIMING_REPORT
  local_time  = g_timer_elapsed (local_timer, NULL);
#endif

  {
    /*
     * Column-based (row by row) forward substitution, performed one
     * column at a time.
     */

    guchar *input_col, *input_col_p; /* Storage and moving pointer for
                                        input image columns. */
    gfloat *a_col, *a_col_p1, *a_col_p2; /* The computation is
                                            performed in the a_col
                                            array, accessed using the
                                            two corresponding
                                            pointers. */

    input_col = g_new (guchar, m_times_channels);
    a_col     = g_new (gfloat, m_times_channels);

    for (j=0; j<n; j++)
    {
      /*
       * The code for this section has been structured so as to make
       * obvious multiply-add operations.
       */

      /*
       * Read one column of intensity values into input_col.
       */
      gimp_pixel_rgn_get_col (&input_image, input_col, j, 0, m);

      /*
       * Non-asymptotic LU factorization entries:
       */
      input_col_p = input_col;
      a_col_p1    = a_col;
      a_col_p2    = a_col;

      for (c=channels; c; c--, a_col_p1++, input_col_p++)
        *a_col_p1 =                        FIT_TO_RANGE( *input_col_p );

      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C0_F + FIT_TO_RANGE( *input_col_p );

      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C1_F + FIT_TO_RANGE( *input_col_p );

      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C2_F + FIT_TO_RANGE( *input_col_p );

      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C3_F + FIT_TO_RANGE( *input_col_p );

      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C4_F + FIT_TO_RANGE( *input_col_p );

      /*
       * This is the first spot where we use C5 as a proxy for CINFTY.
       */
      /*
       * Asymptotic (within roundoff) LU factorization entries, except
       * for the very last one:
       */
      for (k=m_minus_7_times_channels; k; k--,
             a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = *a_col_p2 * MINUS_C5_F + FIT_TO_RANGE( *input_col_p );

      /*
       * Last step of column-based (row by row) forward substitution,
       * and first step of column-based backward substitution,
       * performed in one step on the very last entries:
       */
      for (c=channels; c; c--, a_col_p1++, a_col_p2++, input_col_p++)
        *a_col_p1 = ( *a_col_p2 * MINUS_CINFTY_F
                      + FIT_TO_RANGE( *input_col_p ) ) * CLAST_F;

      /*
       * Remainder of the row-based back substitution (we just took
       * care of the very last row, which is why the indices are
       * rewound the way they are).
       */
      a_col_p1--;
      a_col_p2 = a_col_p1;
      for (c=channels; c; c--)
        a_col_p1--;

      /*
       * This is the second spot where we use C5 as a proxy for
       * CINFTY:
       */
      for (k=m_minus_6_times_channels; k; k--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C5_F;

      for (c=channels; c; c--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C4_F;

      for (c=channels; c; c--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C3_F;

      for (c=channels; c; c--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C2_F;

      for (c=channels; c; c--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C1_F;

      for (c=channels; c; c--, a_col_p1--, a_col_p2--)
        *a_col_p1 = ( *a_col_p1 - *a_col_p2 ) * C0_F;

      a_col_p1 = a_col;
      a_ptr = a+j*channels;

      /*
       * Pack the partially computed coefficients into uint16/uint8:
       *
       * Convert the column of partially computed B-spline
       * coefficients into uint8 or uint16, and append the column to
       * the end of uint array of B-spline coefficients a.
       *
       * In the case of uint16, we store the range -127.5 to 127.5 by
       * affine transforming it to 0 to 65535, combined with rounding
       * (toward +infty for no reason other than expediency; Banker's
       * rounding would be fine). Using cast (= truncate) to effect
       * the rounding, we get that the rounded value is obtained by
       * multiplying by 257 to get values in the range -32767.5 to
       * 32676.5, then adding 32768 so that truncation rounds
       * correctly (with ties resolved towards +infinity
       * ("white"---definitely not Banker's rounding)). Note that the
       * inverse of A keeps values strictly within -127.5 to 127.5
       * (these bounds are NOT included).
       *
       * In the case of storage into uint8, things are a bit
       * simpler. However, we must still perform the shift in float
       * arithmetic prior to the implicit cast, because otherwise we
       * mess up the emulation of rounding. Rounding emulation is the
       * reason we use uint8 even though int8 are naturally in a range
       * which most closely resembles -127.5 to 127.5.
       */
      for (i=m; i; i--, a_ptr+=n_minus_1_times_channels)
        for (c=channels; c; c--, a_col_p1++, a_ptr++)
#ifdef EIGHT_BIT_STORAGE
          *a_ptr = *a_col_p1 + 128.f;
#else
          *a_ptr = 257.f * *a_col_p1 + 32768.f;
#endif
    }

#ifdef TIMING_REPORT
  g_print ("%g seconds for forward substitution.\n",
           g_timer_elapsed (local_timer, NULL)-local_time);
  g_timer_destroy (local_timer);
#endif

    gimp_progress_update(.005+((gdouble)m*n)/((gdouble)mm*nn)*.14);

    g_free(a_col);
    g_free(input_col);
  }

  /*
   * Create new image to place scaled drawable inside.
   */
  new_image_ID = gimp_image_new (nn, mm, gimp_image_base_type (image_ID));
  new_layer_ID = gimp_layer_new (new_image_ID, "Background",
                                 nn, mm,
                                 gimp_drawable_type (drawable->drawable_id),
                                 100,
                                 GIMP_NORMAL_MODE);

  /*
   * Use gimp_image_insert_layer if version is at least 2.7.0.
   */
  if (GIMP_MAJOR_VERSION >= 2 && GIMP_MINOR_VERSION >= 7)
    gimp_image_insert_layer (new_image_ID, new_layer_ID, -1, 0);
  else
    gimp_image_add_layer (new_image_ID, new_layer_ID, 0);
  new_drawable = gimp_drawable_get (new_layer_ID);

  /*
   * Init the output pixel region:
   */
  gimp_pixel_rgn_init (&output_image, new_drawable, 0, 0, nn, mm, TRUE, TRUE);

  {
    gfloat *a_middle, *a_bottom, *a_farbottom, *a_top;
    gfloat *a_middle_p, *a_bottom_p, *a_farbottom_p, *a_top_p, *a_temp;

    /*
     * Storage for a pixel row to be output to new image buffer:
     */
    guchar *output_row;
    guchar *output_row_ptr;

    gint nn_minus_1 = nn-1;

    a_top       = g_new (gfloat, n_times_channels);
    a_middle    = g_new (gfloat, n_times_channels);
    a_bottom    = g_new (gfloat, n_times_channels);
    a_farbottom = g_new (gfloat, n_times_channels);

    /*
     * Temporary storage for a row of the output image:
     */
    output_row  = g_new (guchar, nn_times_channels);

    a_ptr = a;

    /*
     * ADAM AND NICOLAS: WHEN WE HAVE TIME, REWRITE SO NO FLOP IS
     * INVOLVED (using integer arithmetic):
     */
#ifdef EIGHT_BIT_STORAGE
#define UNSCALING ( (gfloat) (.5) )
#define UNSCALED_SHIFT (-85)
#else
#define UNSCALING ( (gfloat) (1./514.) )
#define UNSCALED_SHIFT (-21845)
#endif

    /*
     * Row solve on a_middle:
     */

    a_row_p1 = a_middle;

    for (kk=n_times_channels; kk; kk--, a_row_p1++, a_ptr++)
      *a_row_p1 = ( (gint) *a_ptr + UNSCALED_SHIFT ) * UNSCALING;

    /*
     * Forward substitution:
     */

    /*
     * We'll take care of the very first pixel on the way back.
     */
    a_row_p1 = a_middle+channels;
    a_row_p2 = a_middle;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C0_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C1_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C2_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C3_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C4_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C5_F;

    /*
     * Because integer overflow is not a risk any more---we do clamp
     * values later on, and we'd have to even in exact arithmetic---we
     * use the rounded value of CINFTY instead of its truncated one.
     */
    for (kk=n_minus_8_times_channels; kk; kk--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_CINFTY_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 = ( *a_row_p2 * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;

    a_row_p1--;
    a_row_p2 = a_row_p1;
    for (c=channels; c; c--)
      a_row_p1--;

    for (kk=n_minus_7_times_channels; kk; kk--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * CINFTY_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C5_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C4_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C3_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C2_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C1_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C0_F;

    /*
     * Row solve on a_bottom:
     */

    a_row_p1 = a_bottom;

    for (kk=n_times_channels; kk; kk--, a_row_p1++, a_ptr++)
      *a_row_p1 = ( (gint) *a_ptr + UNSCALED_SHIFT ) * UNSCALING;

    /*
     * Forward substitution:
     */

    /*
     * We'll take care of the very first pixel on the way back.
     */
    a_row_p1 = a_bottom+channels;
    a_row_p2 = a_bottom;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C0_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C1_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C2_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C3_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C4_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C5_F;

    /*
     * Because integer overflow is not a risk any more---we do clamp
     * values later on, and we'd have to even in exact arithmetic---we
     * use the rounded value of CINFTY instead of its truncated one.
     */
    for (kk=n_minus_8_times_channels; kk; kk--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_CINFTY_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 = ( *a_row_p2 * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;

    a_row_p1--;
    a_row_p2 = a_row_p1;
    for (c=channels; c; c--)
      a_row_p1--;

    for (kk=n_minus_7_times_channels; kk; kk--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * CINFTY_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C5_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C4_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C3_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C2_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C1_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C0_F;

    /*
     * Row solve on a_farbottom:
     */

    a_row_p1 = a_farbottom;

    for (kk=n_times_channels; kk; kk--, a_row_p1++, a_ptr++)
      *a_row_p1 = ( (gint) *a_ptr + UNSCALED_SHIFT ) * UNSCALING;

    /*
     * Forward substitution:
     */

    /*
     * We'll take care of the very first pixel on the way back.
     */
    a_row_p1 = a_farbottom+channels;
    a_row_p2 = a_farbottom;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C0_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C1_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C2_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C3_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C4_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_C5_F;

    /*
     * Because integer overflow is not a risk any more---we do clamp
     * values later on, and we'd have to even in exact arithmetic---we
     * use the rounded value of CINFTY instead of its truncated one.
     */
    for (kk=n_minus_8_times_channels; kk; kk--, a_row_p1++, a_row_p2++)
      *a_row_p1 += *a_row_p2 * MINUS_CINFTY_F;

    for (c=channels; c; c--, a_row_p1++, a_row_p2++)
      *a_row_p1 = ( *a_row_p2 * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;

    a_row_p1--;
    a_row_p2 = a_row_p1;
    for (c=channels; c; c--)
      a_row_p1--;

    for (kk=n_minus_7_times_channels; kk; kk--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * CINFTY_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C5_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C4_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C3_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C2_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C1_F;

    for (c=channels; c; c--, a_row_p1--, a_row_p2--)
      *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C0_F;

    gimp_progress_update(.005+((gdouble)m*n)/((gdouble)mm*nn)*.15);

    last_ii = first_past_ii[0];

    for (ii=0; ii<last_ii; ii++)
      {
        /*
         * Move output_row_ptr to the beginning of output_row:
         */
        output_row_ptr = output_row;

        middle_ii    = middle[ii];
        bottom_ii    = bottom[ii];
        farbottom_ii = farbottom[ii];

        a_middle_p    = a_middle;
        a_bottom_p    = a_bottom;
        a_farbottom_p = a_farbottom;

        for (c=0; c<channels; c++)
          {
            a_middle_center[c]      = *a_middle_p++;
            a_bottom_center[c]      = *a_bottom_p++;
            a_farbottom_center[c]   = *a_farbottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            a_middle_right[c]       = *a_middle_p++;
            a_bottom_right[c]       = *a_bottom_p++;
            a_farbottom_right[c]    = *a_farbottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            a_middle_farright[c]    = *a_middle_p++;
            a_bottom_farright[c]    = *a_bottom_p++;
            a_farbottom_farright[c] = *a_farbottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            coef_center[c]   =   middle_ii    *      a_middle_center[c]
                               + bottom_ii    *      a_bottom_center[c]
                               + farbottom_ii *   a_farbottom_center[c];
            coef_right[c]    =   middle_ii    *       a_middle_right[c]
                               + bottom_ii    *       a_bottom_right[c]
                               + farbottom_ii *    a_farbottom_right[c];
            coef_farright[c] =   middle_ii    *    a_middle_farright[c]
                               + bottom_ii    *    a_bottom_farright[c]
                               + farbottom_ii * a_farbottom_farright[c];
          }

        last_jj = first_past_jj[0];

        for (jj=0; jj<last_jj; jj++)
          {
            center_jj   =   center[jj];
            right_jj    =    right[jj];
            farright_jj = farright[jj];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_center[c]   * center_jj
                                        + coef_right[c]    * right_jj
                                        + coef_farright[c] * farright_jj);
          }

        for (j=1; j<n_minus_2; j++)
          {
            for (c=0; c<channels; c++)
              {
                a_middle_left[c]        =  a_middle_center[c];
                a_middle_center[c]      =  a_middle_right[c];
                a_middle_right[c]       =  a_middle_farright[c];
                a_middle_farright[c]    = *a_middle_p++;

                a_bottom_left[c]        =  a_bottom_center[c];
                a_bottom_center[c]      =  a_bottom_right[c];
                a_bottom_right[c]       =  a_bottom_farright[c];
                a_bottom_farright[c]    = *a_bottom_p++;

                a_farbottom_left[c]     =  a_farbottom_center[c];
                a_farbottom_center[c]   =  a_farbottom_right[c];
                a_farbottom_right[c]    =  a_farbottom_farright[c];
                a_farbottom_farright[c] = *a_farbottom_p++;

                coef_left[c]     =   middle_ii    *        a_middle_left[c]
                                   + bottom_ii    *        a_bottom_left[c]
                                   + farbottom_ii *     a_farbottom_left[c];
                coef_center[c]   =   middle_ii    *      a_middle_center[c]
                                   + bottom_ii    *      a_bottom_center[c]
                                   + farbottom_ii *   a_farbottom_center[c];
                coef_right[c]    =   middle_ii    *       a_middle_right[c]
                                   + bottom_ii    *       a_bottom_right[c]
                                   + farbottom_ii *    a_farbottom_right[c];
                coef_farright[c] =   middle_ii    *    a_middle_farright[c]
                                   + bottom_ii    *    a_bottom_farright[c]
                                   + farbottom_ii * a_farbottom_farright[c];
              }

            last_jj = first_past_jj[j];

            for (; jj<last_jj; jj++)
              {
                left_jj     =     left[jj];
                center_jj   =   center[jj];
                right_jj    =    right[jj];
                farright_jj = farright[jj];

                for (c=0; c<channels; c++)
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(       coef_left[c] * left_jj
                                            +   coef_center[c] * center_jj
                                            +    coef_right[c] * right_jj
                                            + coef_farright[c] * farright_jj);
              }
          }

        /*
         * Now, we deal with j = n-2.
         */
        for (c=0; c<channels; c++)
          {
            a_middle_left[c]      = a_middle_center[c];
            a_middle_center[c]    = a_middle_right[c];
            a_middle_right[c]     = a_middle_farright[c];

            a_bottom_left[c]      = a_bottom_center[c];
            a_bottom_center[c]    = a_bottom_right[c];
            a_bottom_right[c]     = a_bottom_farright[c];

            a_farbottom_left[c]   = a_farbottom_center[c];
            a_farbottom_center[c] = a_farbottom_right[c];
            a_farbottom_right[c]  = a_farbottom_farright[c];

            coef_left[c]   =   middle_ii    * a_middle_left[c]
                             + bottom_ii    * a_bottom_left[c]
                             + farbottom_ii * a_farbottom_left[c];
            coef_center[c] =   middle_ii    * a_middle_center[c]
                             + bottom_ii    * a_bottom_center[c]
                             + farbottom_ii * a_farbottom_center[c];
            coef_right[c]  =   middle_ii    * a_middle_right[c]
                             + bottom_ii    * a_bottom_right[c]
                             + farbottom_ii * a_farbottom_right[c];
          }

        last_jj = nn_minus_1;

        for (; jj<last_jj; jj++)
          {
            left_jj   =   left[jj];
            center_jj = center[jj];
            right_jj  =  right[jj];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                        + coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj );
          }

        /*
         * Now, we deal with j = n-1 (only one fine pixel left):
         */
        for (c=0; c<channels; c++)
          {
            a_middle_left[c]      = a_middle_center[c];
            a_middle_center[c]    = a_middle_right[c];

            a_bottom_left[c]      = a_bottom_center[c];
            a_bottom_center[c]    = a_bottom_right[c];

            a_farbottom_left[c]   = a_farbottom_center[c];
            a_farbottom_center[c] = a_farbottom_right[c];

            coef_left[c]   =   middle_ii    * a_middle_left[c]
                             + bottom_ii    * a_bottom_left[c]
                             + farbottom_ii * a_farbottom_left[c];
            coef_center[c] =   middle_ii    * a_middle_center[c]
                             + bottom_ii    * a_bottom_center[c]
                             + farbottom_ii * a_farbottom_center[c];
          }

        left_jj   =   left[nn_minus_1];
        center_jj = center[nn_minus_1];

        for (c=0; c<channels; c++)
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                    + coef_center[c] * center_jj );
        /*
         * Write out the row:
         */
        gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);
      }

    gimp_progress_update(.005+((gdouble)m*n)/((gdouble)mm*nn)*.17);

    /*
     * Now, we deal with the second and later coarse rows, up to the
     * third to last, inclusive.
     */
    for (i=1; i<m_minus_2; i++)
      {
        a_temp      = a_top;
        a_top       = a_middle;
        a_middle    = a_bottom;
        a_bottom    = a_farbottom;
        a_farbottom = a_temp;

        /*
         * Row solve on a_farbottom:
         */

        a_row_p1 = a_farbottom;

        for (kk=n_times_channels; kk; kk--, a_row_p1++, a_ptr++)
          *a_row_p1 = ( (gint) *a_ptr + UNSCALED_SHIFT ) * UNSCALING;

        /*
         * Forward substitution:
         */

        /*
         * We'll take care of the very first pixel on the way back.
         */
        a_row_p1 = a_farbottom+channels;
        a_row_p2 = a_farbottom;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C0_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C1_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C2_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C3_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C4_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_C5_F;

        /*
         * Because integer overflow is not a risk any more---we do clamp
         * values later on, and we'd have to even in exact arithmetic---we
         * use the rounded value of CINFTY instead of its truncated one.
         */
        for (kk=n_minus_8_times_channels; kk; kk--, a_row_p1++, a_row_p2++)
          *a_row_p1 += *a_row_p2 * MINUS_CINFTY_F;

        for (c=channels; c; c--, a_row_p1++, a_row_p2++)
          *a_row_p1 = ( *a_row_p2 * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;

        a_row_p1--;
        a_row_p2 = a_row_p1;
        for (c=channels; c; c--)
          a_row_p1--;

        for (kk=n_minus_7_times_channels; kk; kk--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * CINFTY_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C5_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C4_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C3_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C2_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C1_F;

        for (c=channels; c; c--, a_row_p1--, a_row_p2--)
          *a_row_p1 = ( *a_row_p1 - *a_row_p2 ) * C0_F;

        last_ii = first_past_ii[i];

        for (; ii<last_ii; ii++)
          {
            output_row_ptr = output_row;

            top_ii       =       top[ii];
            middle_ii    =    middle[ii];
            bottom_ii    =    bottom[ii];
            farbottom_ii = farbottom[ii];

            a_top_p       = a_top;
            a_middle_p    = a_middle;
            a_bottom_p    = a_bottom;
            a_farbottom_p = a_farbottom;

            for (c=0; c<channels; c++)
              {
                a_top_center[c]       = *a_top_p++;
                a_middle_center[c]    = *a_middle_p++;
                a_bottom_center[c]    = *a_bottom_p++;
                a_farbottom_center[c] = *a_farbottom_p++;
              }

            for (c=0; c<channels; c++)
              {
                a_top_right[c]       = *a_top_p++;
                a_middle_right[c]    = *a_middle_p++;
                a_bottom_right[c]    = *a_bottom_p++;
                a_farbottom_right[c] = *a_farbottom_p++;
              }

            for (c=0; c<channels; c++)
              {
                a_top_farright[c]       = *a_top_p++;
                a_middle_farright[c]    = *a_middle_p++;
                a_bottom_farright[c]    = *a_bottom_p++;
                a_farbottom_farright[c] = *a_farbottom_p++;
              }

            for (c=0; c<channels; c++)
              {
                coef_center[c]   =         top_ii * a_top_center[c]
                                   +    middle_ii * a_middle_center[c]
                                   +    bottom_ii * a_bottom_center[c]
                                   + farbottom_ii * a_farbottom_center[c];
                coef_right[c]    =         top_ii * a_top_right[c]
                                   +    middle_ii * a_middle_right[c]
                                   +    bottom_ii * a_bottom_right[c]
                                   + farbottom_ii * a_farbottom_right[c];
                coef_farright[c] =         top_ii * a_top_farright[c]
                                   +    middle_ii * a_middle_farright[c]
                                   +    bottom_ii * a_bottom_farright[c]
                                   + farbottom_ii * a_farbottom_farright[c];
              }

            last_jj = first_past_jj[0];

            for (jj=0; jj<last_jj; jj++)
              {
                center_jj   =   center[jj];
                right_jj    =    right[jj];
                farright_jj = farright[jj];

                for (c=0; c<channels; c++)
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(     coef_center[c] * center_jj
                                            +    coef_right[c] * right_jj
                                            + coef_farright[c] * farright_jj);
              }

            for (j=1; j<n_minus_2; j++)
              {
                for (c=0; c<channels; c++)
                  {
                    a_top_left[c]           = a_top_center[c];
                    a_top_center[c]         = a_top_right[c];
                    a_top_right[c]          = a_top_farright[c];
                    a_top_farright[c]       = *a_top_p++;

                    a_middle_left[c]        = a_middle_center[c];
                    a_middle_center[c]      = a_middle_right[c];
                    a_middle_right[c]       = a_middle_farright[c];
                    a_middle_farright[c]    = *a_middle_p++;

                    a_bottom_left[c]        = a_bottom_center[c];
                    a_bottom_center[c]      = a_bottom_right[c];
                    a_bottom_right[c]       = a_bottom_farright[c];
                    a_bottom_farright[c]    = *a_bottom_p++;

                    a_farbottom_left[c]     = a_farbottom_center[c];
                    a_farbottom_center[c]   = a_farbottom_right[c];
                    a_farbottom_right[c]    = a_farbottom_farright[c];
                    a_farbottom_farright[c] = *a_farbottom_p++;

                    coef_left[c]     =      top_ii    * a_top_left[c]
                                       + middle_ii    * a_middle_left[c]
                                       + bottom_ii    * a_bottom_left[c]
                                       + farbottom_ii * a_farbottom_left[c];
                    coef_center[c]   =      top_ii    * a_top_center[c]
                                       + middle_ii    * a_middle_center[c]
                                       + bottom_ii    * a_bottom_center[c]
                                       + farbottom_ii * a_farbottom_center[c];
                    coef_right[c]    =      top_ii    * a_top_right[c]
                                       + middle_ii    * a_middle_right[c]
                                       + bottom_ii    * a_bottom_right[c]
                                       + farbottom_ii * a_farbottom_right[c];
                    coef_farright[c] =      top_ii    * a_top_farright[c]
                                       + middle_ii    * a_middle_farright[c]
                                       + bottom_ii    * a_bottom_farright[c]
                                      + farbottom_ii * a_farbottom_farright[c];
                  }

                last_jj = first_past_jj[j];

                for (; jj<last_jj; jj++)
                  {
                    left_jj     =     left[jj];
                    center_jj   =   center[jj];
                    right_jj    =    right[jj];
                    farright_jj = farright[jj];

                    for (c=0; c<channels; c++)
                      *output_row_ptr++ =
                        F_ROUND_AND_CLAMP_0255(       coef_left[c] * left_jj
                                                +   coef_center[c] * center_jj
                                                +    coef_right[c] * right_jj
                                            + coef_farright[c] * farright_jj );
                  }
              }

            /*
             * Now, we deal with j = n-2.
             */
            for (c=0; c<channels; c++)
              {
                a_top_left[c]         = a_top_center[c];
                a_top_center[c]       = a_top_right[c];
                a_top_right[c]        = a_top_farright[c];

                a_middle_left[c]      = a_middle_center[c];
                a_middle_center[c]    = a_middle_right[c];
                a_middle_right[c]     = a_middle_farright[c];

                a_bottom_left[c]      = a_bottom_center[c];
                a_bottom_center[c]    = a_bottom_right[c];
                a_bottom_right[c]     = a_bottom_farright[c];

                a_farbottom_left[c]   = a_farbottom_center[c];
                a_farbottom_center[c] = a_farbottom_right[c];
                a_farbottom_right[c]  = a_farbottom_farright[c];

                coef_left[c]   =         top_ii * a_top_left[c]
                                 +    middle_ii * a_middle_left[c]
                                 +    bottom_ii * a_bottom_left[c]
                                 + farbottom_ii * a_farbottom_left[c];
                coef_center[c] =         top_ii * a_top_center[c]
                                 +    middle_ii * a_middle_center[c]
                                 +    bottom_ii * a_bottom_center[c]
                                 + farbottom_ii * a_farbottom_center[c];
                coef_right[c]  =         top_ii * a_top_right[c]
                                 +    middle_ii * a_middle_right[c]
                                 +    bottom_ii * a_bottom_right[c]
                                 + farbottom_ii * a_farbottom_right[c];
              }

            last_jj = nn_minus_1;

            for (; jj<last_jj; jj++)
              {
                left_jj   =   left[jj];
                center_jj = center[jj];
                right_jj  =  right[jj];

                for (c=0; c<channels; c++)
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                            + coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
              }

            /*
             * Now, we deal with j = n-1 (the very last fine pixel).
             */
            for (c=0; c<channels; c++)
              {
                a_top_left[c]         = a_top_center[c];
                a_top_center[c]       = a_top_right[c];

                a_middle_left[c]      = a_middle_center[c];
                a_middle_center[c]    = a_middle_right[c];

                a_bottom_left[c]      = a_bottom_center[c];
                a_bottom_center[c]    = a_bottom_right[c];

                a_farbottom_left[c]   = a_farbottom_center[c];
                a_farbottom_center[c] = a_farbottom_right[c];

                coef_left[c]   =         top_ii * a_top_left[c]
                                 +    middle_ii * a_middle_left[c]
                                 +    bottom_ii * a_bottom_left[c]
                                 + farbottom_ii * a_farbottom_left[c];
                coef_center[c] =         top_ii * a_top_center[c]
                                 +    middle_ii * a_middle_center[c]
                                 +    bottom_ii * a_bottom_center[c]
                                 + farbottom_ii * a_farbottom_center[c];
              }

            left_jj   =   left[nn_minus_1];
            center_jj = center[nn_minus_1];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                        + coef_center[c] * center_jj );

            gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);
          }
      }

    gimp_progress_update(.99);

    /*
     * Now, we deal with the second to last coarse row:
     */
    a_temp      = a_top;
    a_top       = a_middle;
    a_middle    = a_bottom;
    a_bottom    = a_farbottom;
    a_farbottom = a_temp;

    last_ii = mm - 1;

    for (; ii<last_ii; ii++)
      {
        output_row_ptr = output_row;

        top_ii    =    top[ii];
        middle_ii = middle[ii];
        bottom_ii = bottom[ii];

        a_top_p    = a_top;
        a_middle_p = a_middle;
        a_bottom_p = a_bottom;

        for (c=0; c<channels; c++)
          {
            a_top_center[c]      = *a_top_p++;
            a_middle_center[c]   = *a_middle_p++;
            a_bottom_center[c]   = *a_bottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            a_top_right[c]       = *a_top_p++;
            a_middle_right[c]    = *a_middle_p++;
            a_bottom_right[c]    = *a_bottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            a_top_farright[c]    = *a_top_p++;
            a_middle_farright[c] = *a_middle_p++;
            a_bottom_farright[c] = *a_bottom_p++;
          }

        for (c=0; c<channels; c++)
          {
            coef_center[c]   =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c]
                               + bottom_ii * a_bottom_center[c];
            coef_right[c]    =      top_ii * a_top_right[c]
                               + middle_ii * a_middle_right[c]
                               + bottom_ii * a_bottom_right[c];
            coef_farright[c] =      top_ii * a_top_farright[c]
                               + middle_ii * a_middle_farright[c]
                               + bottom_ii * a_bottom_farright[c];
          }

        last_jj = first_past_jj[0];

        for (jj=0; jj<last_jj; jj++)
          {
            center_jj   =   center[jj];
            right_jj    =    right[jj];
            farright_jj = farright[jj];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_center[c]   * center_jj
                                         + coef_right[c]    * right_jj
                                      + coef_farright[c] * farright_jj );
          }

        for (j=1; j<n_minus_2; j++)
          {
            for (c=0; c<channels; c++)
              {
                a_top_left[c]        = a_top_center[c];
                a_top_center[c]      = a_top_right[c];
                a_top_right[c]       = a_top_farright[c];
                a_top_farright[c]    = *a_top_p++;

                a_middle_left[c]     = a_middle_center[c];
                a_middle_center[c]   = a_middle_right[c];
                a_middle_right[c]    = a_middle_farright[c];
                a_middle_farright[c] = *a_middle_p++;

                a_bottom_left[c]     = a_bottom_center[c];
                a_bottom_center[c]   = a_bottom_right[c];
                a_bottom_right[c]    = a_bottom_farright[c];
                a_bottom_farright[c] = *a_bottom_p++;

                coef_left[c]     =      top_ii * a_top_left[c]
                                   + middle_ii * a_middle_left[c]
                                   + bottom_ii * a_bottom_left[c];
                coef_center[c]   =      top_ii * a_top_center[c]
                                   + middle_ii * a_middle_center[c]
                                   + bottom_ii * a_bottom_center[c];
                coef_right[c]    =      top_ii * a_top_right[c]
                                   + middle_ii * a_middle_right[c]
                                   + bottom_ii * a_bottom_right[c];
                coef_farright[c] =      top_ii * a_top_farright[c]
                                   + middle_ii * a_middle_farright[c]
                                   + bottom_ii * a_bottom_farright[c];
              }

            last_jj = first_past_jj[j];

            for (; jj<last_jj; jj++)
              {
                left_jj     =     left[jj];
                center_jj   =   center[jj];
                right_jj    =    right[jj];
                farright_jj = farright[jj];

                for (c=0; c<channels; c++)
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(       coef_left[c] * left_jj
                                            +   coef_center[c] * center_jj
                                            +    coef_right[c] * right_jj
                                            + coef_farright[c] * farright_jj );
              }
          }

        /*
         * Now, we deal with j = n-2:
         */
        for (c=0; c<channels; c++)
          {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];
            a_top_right[c]     = a_top_farright[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];
            a_middle_right[c]  = a_middle_farright[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];
            a_bottom_right[c]  = a_bottom_farright[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
            coef_right[c]  =      top_ii * a_top_right[c]
                             + middle_ii * a_middle_right[c]
                             + bottom_ii * a_bottom_right[c];
          }

        last_jj = nn_minus_1;

        for (; jj<last_jj; jj++)
          {
            left_jj   =   left[jj];
            center_jj = center[jj];
            right_jj  =  right[jj];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                        + coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj );
          }

        /*
         * Now, we deal with j = n-1 (last fine pixel):
         */
        for (c=0; c<channels; c++)
          {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
          }

        left_jj   =   left[nn_minus_1];
        center_jj = center[nn_minus_1];

        for (c=0; c<channels; c++)
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                    + coef_center[c] * center_jj );

        gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);
      }

    /*
     * Compute the last coarse row:
     */
    a_temp   = a_top;
    a_top    = a_middle;
    a_middle = a_bottom;
    a_bottom = a_temp;

    output_row_ptr = output_row;

    top_ii    = top[ii];
    middle_ii = middle[ii];

    a_top_p    = a_top;
    a_middle_p = a_middle;

    for (c=0; c<channels; c++)
      {
        a_top_center[c]      = *a_top_p++;
        a_middle_center[c]   = *a_middle_p++;
      }

    for (c=0; c<channels; c++)
      {
        a_top_right[c]       = *a_top_p++;
        a_middle_right[c]    = *a_middle_p++;
      }

    for (c=0; c<channels; c++)
      {
        a_top_farright[c]    = *a_top_p++;
        a_middle_farright[c] = *a_middle_p++;
      }

    for (c=0; c<channels; c++)
      {
        coef_center[c]   =      top_ii * a_top_center[c]
                           + middle_ii * a_middle_center[c];
        coef_right[c]    =      top_ii * a_top_right[c]
                           + middle_ii * a_middle_right[c];
        coef_farright[c] =      top_ii * a_top_farright[c]
                           + middle_ii * a_middle_farright[c];
      }

    last_jj = first_past_jj[0];

    for (jj=0; jj<last_jj; jj++)
      {
        center_jj   =   center[jj];
        right_jj    =    right[jj];
        farright_jj = farright[jj];

        for (c=0; c<channels; c++)
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(   coef_center[c]   * center_jj
                                    + coef_right[c]    * right_jj
                                    + coef_farright[c] * farright_jj );
      }

    for (j=1; j<n_minus_2; j++)
      {
        for (c=0; c<channels; c++)
          {
            a_top_left[c]        = a_top_center[c];
            a_top_center[c]      = a_top_right[c];
            a_top_right[c]       = a_top_farright[c];
            a_top_farright[c]    = *a_top_p++;

            a_middle_left[c]     = a_middle_center[c];
            a_middle_center[c]   = a_middle_right[c];
            a_middle_right[c]    = a_middle_farright[c];
            a_middle_farright[c] = *a_middle_p++;

            coef_left[c]     =      top_ii * a_top_left[c]
                               + middle_ii * a_middle_left[c];
            coef_center[c]   =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c];
            coef_right[c]    =      top_ii * a_top_right[c]
                               + middle_ii * a_middle_right[c];
            coef_farright[c] =      top_ii * a_top_farright[c]
                               + middle_ii * a_middle_farright[c];
          }

        last_jj = first_past_jj[j];

        for (; jj<last_jj; jj++)
          {
            left_jj     =     left[jj];
            center_jj   =   center[jj];
            right_jj    =    right[jj];
            farright_jj = farright[jj];

            for (c=0; c<channels; c++)
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(       coef_left[c] * left_jj
                                        +   coef_center[c] * center_jj
                                        +    coef_right[c] * right_jj
                                        + coef_farright[c] * farright_jj );
          }
      }

    /*
     * Now, we deal with j = n-2.
     */
    for (c=0; c<channels; c++)
      {
        a_top_left[c]      = a_top_center[c];
        a_top_center[c]    = a_top_right[c];
        a_top_right[c]     = a_top_farright[c];

        a_middle_left[c]   = a_middle_center[c];
        a_middle_center[c] = a_middle_right[c];
        a_middle_right[c]  = a_middle_farright[c];

        coef_left[c]   =      top_ii * a_top_left[c]
                         + middle_ii * a_middle_left[c];
        coef_center[c] =      top_ii * a_top_center[c]
                         + middle_ii * a_middle_center[c];
        coef_right[c]  =      top_ii * a_top_right[c]
                         + middle_ii * a_middle_right[c];
      }

    last_jj = nn_minus_1;

    for (; jj<last_jj; jj++)
      {
        left_jj   =   left[jj];
        center_jj = center[jj];
        right_jj  =  right[jj];

        for (c=0; c<channels; c++)
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                    + coef_center[c] * center_jj
                                    +  coef_right[c] * right_jj );
      }

    /*
     * Now, we deal with j = n-1:
     */
    for (c=0; c<channels; c++)
      {
        a_top_left[c]      = a_top_center[c];
        a_top_center[c]    = a_top_right[c];

        a_middle_left[c]   = a_middle_center[c];
        a_middle_center[c] = a_middle_right[c];

        coef_left[c]   =      top_ii * a_top_left[c]
          + middle_ii * a_middle_left[c];
        coef_center[c] =      top_ii * a_top_center[c]
          + middle_ii * a_middle_center[c];
      }

    left_jj   =   left[nn_minus_1];
    center_jj = center[nn_minus_1];

    for (c=0; c<channels; c++)
      *output_row_ptr++ =
        F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                + coef_center[c] * center_jj );

    gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

    g_free(a_farbottom);
    g_free(a_bottom);
    g_free(a_middle);
    g_free(a_top);
    g_free(output_row);
  }

  g_free(a);

  gimp_drawable_flush (new_drawable);
  gimp_drawable_merge_shadow (new_drawable->drawable_id, FALSE);
  gimp_drawable_update (new_drawable->drawable_id, 0, 0, nn, mm);
  gimp_drawable_detach (new_drawable);

  return new_image_ID;
}

static void
first_past_index (gint o,
                  gint oo,
                  gint first_past_kk[])
{
  /*
   * The comments use the variables which are relevant to the use of
   * this function in the horizontal direction. That is, n is o, nn is
   * oo, j is k, and jj is kk.
   *
   * This subprogram puts in first_past_jj[j], for j in 0..n-2, the
   * index of the first fine cell with a "center" which is not to the
   * left of the center of the j+1th coarse cell (the last cell has
   * index n-1, and so has no right neighbour). An alternate
   * description: first_past_jj[j] is the index of the first fine cell
   * with center >= j+3/2. This index matters for the following
   * reason: It is the index at which the rightmost relevant B-Spline
   * coefficient shifts to the right (center = j+3/2 could go either
   * way).
   *
   * This code uses a general convention that the input pixels have
   * unit sides, and that (describing things in 1D) the global
   * coordinate of the left endpoint of an input cell is exactly its
   * index. That is, the first coarse (input) cell goes from 0 to 1,
   * the last coarse cell from n-1 to n.
   *
   * Because of this convention, there is an alternate description of
   * first_past_jj[j]: Because coarse cell centers are 1/2 to the
   * right of left boundaries, first_past_jj is also the index of the
   * first fine cell for which the left end of the box filtering mask
   * (which has diameter 1/2) is >= j+1.  The very first "box" starts
   * at zero, and it shifts by h at every index shift. This gives jj*h
   * >= j+1, that is, it is the first jj which violates jj*(n-1) <
   * (j+1)*(nn-1).
   *
   * Because the jth cell is [j,j+1], and choosing the very first fine
   * cell center to be at 1/2 (same as the very first coarse cell
   * center), so that the very last fine, and coarse, cell center is
   * n-1/2, we have that as the fine index runs from 0 to nn-1, a
   * distance of n-1 is covered, so that
   *
   * h = (n-1)/(nn-1)
   *
   * (the usual step for interpolation methods). Consequently,
   * first_past_jj[j] is the first index jj for which
   *
   * 1/2 + jj * h >= j + 3/2;
   *
   * in integer arithmetic:
   *
   * jj * (n-1) >= (j+1) * (nn-1).
   *
   * Alternately, it is the first index for which the condition
   *
   * jj * (n-1) < (j+1) * (nn-1)
   *
   * does not hold.
   */

  gint o_minus_1 = o-1;

  gint k = 0;

  if (oo>o)
  {
    gint kk = 1;
    gint kk_times_o_minus_1 = o_minus_1;
    gint oo_minus_1 = oo-1;
    gint k_plus_1_times_oo_minus_1 = oo_minus_1;

    do {
      while (kk_times_o_minus_1 < k_plus_1_times_oo_minus_1)
        {
          kk++;
          kk_times_o_minus_1 += o_minus_1;
        }
      first_past_kk[k]=kk;
      kk++;
      kk_times_o_minus_1 += o_minus_1;
      k++;
      k_plus_1_times_oo_minus_1 += oo_minus_1;
    } while (k<o_minus_1);
  }
  else
  {
    do
      {
        first_past_kk[k] = k+1;
      }
    while (++k<o_minus_1);
  }
}

/*
 * smooth_coarse_to_fine_coefficients ()
 *
 * This function is called twice: once in the "vertical" direction,
 * and once in the "horizontal" direction.
 *
 * The comments use the variables which are relevant to the use of
 * this function in the horizontal direction. That is, n is o, nn is
 * oo, j is k, and jj is kk.
 *
 * "INPUTS:" number of coarse cells n
 *           number of fine cells nn
 *           one int array with at least n entries: first_past_jj[]
 *
 * "OUTPUTS:" four double arrays with at least nn entries:
 *            first[], second[], third[], fourth[]
 */
static void
smooth_coarse_to_fine_coefficients (gint   o,
                                    gint   oo,
                                    gint   first_past_kk[],
                                    gfloat first[],
                                    gfloat second[],
                                    gfloat third[],
                                    gfloat fourth[])
{
  /*
   * Possible improvements: use long or unsigned integers to prevent
   * integer overflow: integers as large as n times nn are computed.
   * This does not appear to have been a problem, but it could be.
   */

  /*
   * Possible improvement: Use left/right symmetry to eliminate the
   * computation of half the coefficients. This is a Theta(m+n)
   * improvement (probably not worth the trouble). Because of the way
   * indices "stay left" for fine cells which overlap two coarse
   * cells, this is not as simple as it looks, unless there are no
   * overlapping fine cells, that is, unless the magnification factor
   * is an integer.
   */

  /*
   * Formulas for the antiderivatives of the cardinal basis functions,
   * with constants of integration set so that they are equal to zero
   * at the left boundary of the cell of interest: They are structured
   * a la Horner to minimize flops. Because x is Theta(1), some mild
   * cancellation will occur.
   */

  /*
   * The following describe the centered B-Spline (applicable in the
   * interior):
   */
#define LEFT_BSPLINE(x)            ( (x) *         (x) *       (x))
#define CENTER_BSPLINE(x)          ( (x) * (  3. - (x) * (-3.+(x)+(x))))
#define RIGHT_BSPLINE(x)           ( (x) * (  3. + (x) * (-3.+(x))))
  /*
   * The coordinate x is with respect to the left boundary point of
   * the applicable cell. That is: x = 1 for LEFT_BSPLINE corresponds
   * to x = 0 for CENTER_BSPLINE, and x = 1 for CENTER_BSPLINE
   * corresponds to x = 0 for RIGHT_BSPLINE.
   */

  /*
   * For natural boundary conditions, LEFT_BDRY_LEFT_SPLINE and
   * RIGHT_BDRY_RIGHT_SPLINE are identical to those which apply in the
   * interior. Consequently, they are redundant and could be removed
   * from the code.
   */
#define LEFT_BDRY_SPLINE(x)        ( (x) * (  6. -         (x)*(x)))
#define LEFT_BDRY_LEFT_SPLINE(x)   ( (x) *                 (x)*(x))
#define RIGHT_BDRY_SPLINE(x)       ( (x) * (  3. + (x) * ( 3.- (x))))
#define RIGHT_BDRY_RIGHT_SPLINE(x) ( (x) * (  3. + (x) * (-3.+ (x))))

 /*
  * jj * (n-1) / (nn-1) (in rational arithmetic)
  *
  * is the absolute coordinate of the left endpoint of the index jj
  * fine cell (recall that jj starts at zero).
  *
  * The distance between fine cell centers (and fine cell integration
  * left boundaries, as well as right boundaries) is (n-1)/(nn-1).
  *
  * Now, when the fine cell center is at a coarse cell center or just
  * past it, the coordinate, with respect to the left boundary of the
  * coarse cell, of the left boundary point of the integration mask is
  *
  * jj * (n-1) / (nn-1) - floor of this value.
  *
  * Now, first_past[j-1] is the first jj for which the left boundary
  * point of the integration mask is not to the left of j.
  * Consequently, this is the desired floor. This implies that we set
  *
  * x = jj * (n-1) / (nn-1) - j
  *
  * when we start iterating at the jj value equal to the previous
  * first_past, which happens to be the value of j at loop entry. In
  * order to minimize round off, we actually rewrite as
  *
  * x = ( jj * (n-1) - j * (nn-1) ) / (nn-1).
  */

  /*
   * For convenience, three coefficients---the last one equal to
   * zero---are given for the very first pixel in each direction, and
   * only two for the final pixel in each direction. For the second
   * and second to last coarse cells, three are given. Otherwise, four
   * coefficients are given.
   */

  gint k;
  gint kk;

  gint o_minus_1  = o-1;
  gint o_minus_2  = o_minus_1 - 1;
  gint oo_minus_1 = oo-1;

  gdouble one_over_oo_minus_1 = 1./(oo_minus_1);
  gdouble h = (o-1) * one_over_oo_minus_1;

  gdouble x = h;

  gint first_past = first_past_kk[0];

  /* first[0] = 0.f; */
  second[0] = 5.f;
  third[0]  = 1.f;
  fourth[0] = 0.f;

  for (kk=1; kk<first_past; kk++, x+=h)
    {
      /*    first[kk]  = 0.f; */
      /* second[kk] = ( 5. - LEFT_BDRY_SPLINE(x) ) + RIGHT_BSPLINE(x); */
      second[kk] = 5. + x * ( -3. + x * ( -3. + x + x ) );
      /* third[kk]  = ( 1. - LEFT_BSPLINE(x) ) + CENTER_BSPLINE(x); */
      third[kk]  = 1. + 3. * x * ( 1. + x * ( 1. - x ) );
      fourth[kk] = LEFT_BSPLINE(x);
    }

  for (k=1; k<o_minus_2; k++)
    {
      x = ( kk*o_minus_1 - k*oo_minus_1 ) * one_over_oo_minus_1;
      first_past = first_past_kk[k];

      for (; kk<first_past; kk++, x+=h)
      {
        /* first[kk]  = 1. - RIGHT_BSPLINE(x); */
        first[kk]  = 1. + x * ( -3. + x * ( 3. - x ) );
        /* second[kk] = ( 4. - CENTER_BSPLINE(x) ) + RIGHT_BSPLINE(x); */
        second[kk] = 4. + 3. * ( x * x ) * ( -2. + x );
        /* third[kk]  = ( 1. - LEFT_BSPLINE(x) ) + CENTER_BSPLINE(x); */
        third[kk]  = 1. + 3. * ( x * ( 1. + x * ( 1. - x ) ) );
        fourth[kk] = LEFT_BSPLINE(x);
      }
    }

  x = ( kk*o_minus_1 - (o_minus_2)*oo_minus_1 ) * one_over_oo_minus_1;
  first_past = oo - 1;

  for (; kk<first_past; kk++, x+=h)
    {
      /* first[kk]  = 1. - RIGHT_BSPLINE(x); */
      first[kk]  = 1. + x * ( -3. + x * ( 3. - x ) );
      /* second[kk] = ( 4. - CENTER_BSPLINE(x) ) + RIGHT_BSPLINE(x); */
      second[kk] = 4. + 3. * ( ( x * x ) * ( -2. + x ) );
      /* third[kk]  = ( 1. - LEFT_BSPLINE(x) ) + RIGHT_BDRY_SPLINE(x); */
      third[kk]  = 1. + x * ( 3. + x * ( 3. - ( x + x ) ) );
      /*      fourth[kk] = 0.f; */
    }

  first[first_past]  = 1.f;
  second[first_past] = 5.f;
  /*  third[first_past]  = 0.f; */
  /*  fourth[first_past] = 0.f; */
}

static gint32
scale_up_sharp (gint32        image_ID,
                GimpDrawable *drawable,
                gint          m,
                gint          n,
                gint          mm,
                gint          nn,
                gint          channels)
{
  GimpDrawable *new_drawable;
  gint32 new_image_ID;
  gint32 new_layer_ID;

  GimpPixelRgn input_image, output_image;

  /*
   * Useful iterator bound:
   */
  gint m_times_channels = m * channels;

  /*
   * Utility iterator:
   */
  gint c; /* Index usually related to the number of color channels
             (3 for RGB, 1 for Grayscale...). */

  /*
   * Row and column indices:
   */
  gint i; /* Index of the input row under consideration. */
  gint j; /* Index of the input column under consideration. */

  /*
   * Computation of the B-spline coefficients:
   */

  /*
   * Rescaled partially computed B-Spline coefficients are stored in
   * a.
   */
#ifdef EIGHT_BIT_STORAGE
  guchar *a, *a_ptr;
  a = g_new (guchar,  n * m_times_channels);
#else
  guint16 *a, *a_ptr;
  a = g_new (guint16, n * m_times_channels);
#endif

  /*
   * Initialize the input pixel region:
   */
  gimp_pixel_rgn_init (&input_image, drawable, 0, 0, n, m, FALSE, FALSE);

  /*
   * Set up a progress bar:
   */
  gimp_progress_init (
    "Sharp upsizing with average matching biquadratic splines");
  gimp_progress_update(.01);

  {
    /*
     * Column-based (row by row) forward substitution, performed one
     * column at a time:
     */

    guchar *input_col, *input_col_p; /* Storage and moving pointer for
                                        input image columns. */
    gfloat *a_col, *a_col_p1, *a_col_p2; /* The computation is
                                            performed in the a_col
                                            array, accessed using the
                                            two corresponding
                                            pointers. */

    gint n_minus_1_times_channels = (n-1) * channels;
    gint m_minus_6_times_channels = m_times_channels - 6 * channels;
    gint m_minus_7_times_channels = m_minus_6_times_channels - channels;

    input_col = g_new (guchar, m_times_channels);
    a_col     = g_new (gfloat, m_times_channels);

    /*
     * The code for this section has been structured so as to make
     * obvious multiply-add operations.
     */

    j = 0;
    do {
      /*
       * Read one column of intensity values into input_col:
       */
      gimp_pixel_rgn_get_col (&input_image, input_col, j, 0, m);

      /*
       * Non-asymptotic LU factorization entries:
       */
      a_col_p2    = a_col;
      a_col_p1    = a_col;
      input_col_p = input_col;

      c = channels;
      do {
        *a_col_p1++ =                FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      c = channels;
      do {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C0_F + FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      c = channels;
      do {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C1_F + FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      c = channels;
      do {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C2_F + FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      c = channels;
      do {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C3_F + FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      c = channels;
      do {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C4_F + FIT_TO_RANGE( *input_col_p++ );
      } while (--c);

      /*
       * This is the first spot where we use C5 as a proxy for CINFTY:
       */
      /*
       * Asymptotic (within roundoff) LU factorization entries, except
       * for the very last one:
       */
      c = m_minus_7_times_channels;
      while (c--) {
        *a_col_p1++ = *a_col_p2++
                      * MINUS_C5_F + FIT_TO_RANGE( *input_col_p++ );
      }

      /*
       * Last step of column-based (row by row) forward substitution,
       * and first step of column-based backward substitution,
       * performed in one step on the very last entries:
       */

      c = channels;
      do {
        *a_col_p1++ = ( *a_col_p2++ * MINUS_CINFTY_F
                        + FIT_TO_RANGE( *input_col_p++ ) ) * CLAST_F;
      } while (--c);

      /*
       * Remainder of the row-based back substitution (we just took
       * care of the very last row, which is why the indices are
       * rewound the way they are):
       */
      a_col_p2 = --a_col_p1;

      c = channels;
      do {
        --a_col_p1;
      } while (--c);

      /*
       * This is the second spot where we use C5 as a proxy for
       * CINFTY:
       */

      c = m_minus_6_times_channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C5_F;
        --a_col_p1;
      } while (--c);

      c = channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C4_F;
        --a_col_p1;
      } while (--c);

      c = channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C3_F;
        --a_col_p1;
      } while (--c);

      c = channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C2_F;
        --a_col_p1;
      } while (--c);

      c = channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C1_F;
        --a_col_p1;
      } while (--c);

      c = channels;
      do {
        *a_col_p1 = ( *a_col_p1 - *a_col_p2-- ) * C0_F;
        --a_col_p1;
      } while (--c);

      a_col_p1 = a_col;
      a_ptr = a+j*channels;

      /*
       * Pack the partially computed coefficients into uint16/uint8:
       *
       * Convert the column of partially computed B-spline
       * coefficients into uint8 or uint16, and append the column to
       * the end of uint array of B-spline coefficients a.
       *
       * In the case of uint16, we store the range -127.5 to 127.5 by
       * affine transforming it to 0 to 65535, combined with rounding
       * (toward +infty for no reason other than expediency; Banker's
       * rounding would be fine). Using cast (= truncate) to effect
       * the rounding, we get that the rounded value is obtained by
       * multiplying by 257 to get values in the range -32767.5 to
       * 32676.5, then adding 32768 so that truncation rounds
       * correctly (with ties resolved towards +infinity
       * ("white"---definitely not Banker's rounding)). Note that the
       * inverse of A keeps values strictly within -127.5 to 127.5
       * (these bounds are NOT included).
       *
       * In the case of storage into uint8, things are a bit
       * simpler. However, we must still perform the shift in float
       * arithmetic prior to the implicit cast, because otherwise we
       * mess up the emulation of rounding. Rounding emulation is the
       * reason we use uint8 even though int8 are naturally in a range
       * which most closely resembles -127.5 to 127.5.
       */
      i = m;
      do {

        c = channels;
        do {
#ifdef EIGHT_BIT_STORAGE
          *a_ptr++ =         *a_col_p1++ + 128.f;
#else
          *a_ptr++ = 257.f * *a_col_p1++ + 32768.f;
#endif
        } while (--c);

        a_ptr += n_minus_1_times_channels;

      } while (--i);

    } while (++j<n);

    g_free(a_col);
    g_free(input_col);
  }

  {
    gint last_overlapping_ii[m-1];
    gint last_overlapping_jj[n-1];

    gfloat top[mm];
    gfloat middle[mm];
    gfloat bottom[mm];
    gfloat farbottom[m-1];

    gfloat left[nn];
    gfloat center[nn];
    gfloat right[nn];
    gfloat farright[n-1];

    /*
     * Computation of the tensor components of the linear
     * transformation from B-spline coefficents to fine cell averages
     * (actually fine cell integrals, since we have alread rescaled by
     * the reciprocal of the fine cell areas):
     */
    last_overlapping_index(m, mm, last_overlapping_ii);
    last_overlapping_index(n, nn, last_overlapping_jj);

    coarse_to_fine_coefficients(m, mm, last_overlapping_ii,
                                top, middle, bottom, farbottom);
    coarse_to_fine_coefficients(n, nn, last_overlapping_jj,
                                left, center, right, farright);

    {
      gint ii; /* Index of the output row under consideration. */
      gint last_ii; /* Local loop bound for ii. */

      gint jj; /* Index of the output column under consideration. */
      gint last_jj; /* Local loop bound for jj. */

      /*
       * Computed coefficients/partial values:
       */
      gfloat top_ii;
      gfloat middle_ii;
      gfloat bottom_ii;
      gfloat farbottom_i;

      gfloat left_jj;
      gfloat center_jj;
      gfloat right_jj;
      gfloat farright_j;

      gfloat a_top_left[channels];
      gfloat a_top_center[channels];
      gfloat a_top_right[channels];
      gfloat a_top_farright[channels];

      gfloat a_middle_left[channels];
      gfloat a_middle_center[channels];
      gfloat a_middle_right[channels];
      gfloat a_middle_farright[channels];

      gfloat a_bottom_left[channels];
      gfloat a_bottom_center[channels];
      gfloat a_bottom_right[channels];
      gfloat a_bottom_farright[channels];

      gfloat a_farbottom_left[channels];
      gfloat a_farbottom_center[channels];
      gfloat a_farbottom_right[channels];
      gfloat a_farbottom_farright[channels];

      gfloat coef_left[channels];
      gfloat coef_center[channels];
      gfloat coef_right[channels];

      gfloat *a_middle, *a_bottom, *a_farbottom, *a_top;
      gfloat *a_middle_p, *a_bottom_p, *a_farbottom_p, *a_top_p, *a_temp;

      /*
       * Scale parameter used in row solves:
       */
      gfloat unscaling;

      /*
       * Storage for a pixel row to be output to new image buffer:
       */
      guchar *output_row;
      guchar *output_row_ptr;

      gfloat *a_row_p1, *a_row_p2;

      gint n_minus_2 = n - 2;
      gint m_minus_2 = m - 2;
      gint n_times_channels = n * channels;
      gint n_minus_7_times_channels = n_times_channels - 7 * channels;
      gint n_minus_8_times_channels = n_minus_7_times_channels - channels;

      a_middle    = g_new (gfloat, n_times_channels);
      a_bottom    = g_new (gfloat, n_times_channels);
      a_farbottom = g_new (gfloat, n_times_channels);
      a_top       = g_new (gfloat, n_times_channels);

      /*
       * Temporary storage for a row of the output image:
       */
      output_row  = g_new (guchar, nn * channels);

      {
        /*
         * mm_nn_over_m_n is one over the area of the fine cells, assuming
         * that the coarse cells have area 1.
         */
        gdouble mm_nn_over_m_n = ( (gdouble)mm * (gdouble)nn )
                                 / ( (gdouble)m * (gdouble)n );

#ifdef EIGHT_BIT_STORAGE
       unscaling = .5f * mm_nn_over_m_n;
#define UNSCALED_SHIFT (-85)
#else
       unscaling = (float) ( mm_nn_over_m_n / 514. );
#define UNSCALED_SHIFT (-21845)
#endif
      }

      /*
       * Create new image to place scaled drawable inside.
       */
      new_image_ID = gimp_image_new (nn, mm, gimp_image_base_type (image_ID));
      new_layer_ID =
        gimp_layer_new (new_image_ID, "Background",
                        nn, mm,
                        gimp_drawable_type (drawable->drawable_id),
                        100,
                        GIMP_NORMAL_MODE);

      /*
       * Use gimp_image_insert_layer if version is at least 2.7.0.
       */
      if (GIMP_MAJOR_VERSION >= 2 && GIMP_MINOR_VERSION >= 7)
        gimp_image_insert_layer (new_image_ID, new_layer_ID, -1, 0);
      else
        gimp_image_add_layer (new_image_ID, new_layer_ID, 0);
      new_drawable = gimp_drawable_get (new_layer_ID);

      /*
       * Init the output pixel region:
       */
      gimp_pixel_rgn_init (&output_image, new_drawable, 0, 0,
                           nn, mm, TRUE, TRUE);

      /*
       * Given that we will not be storing results back into guchars
       * until the very end, we start by undoing the effect of
       * shifting by .5 to store -32767.5..32767.5 in int16, as well
       * as of the FIT_TO_RANGE rescaling.  (If this was in 3D, we
       * would postpone this "unscaling" until dealing with the very
       * last direction, or we would allocate a float "slab" instead
       * of a row.)
       *
       * Once this is done, we can compute solutions taking results at
       * face value.
       *
       * We also divide by the fine cell areas, which allows us to
       * compute with integrals instead of averages later on.
       *
       * NATURAL BOUNDARY CONDITIONS:
       *
       * Given the solution z of
       *
       * A ( m2 * z + b2 ) = m1 * y + b1,
       *
       * from which one gets that
       *
       * y = A ( m2/m1 * z + b2/m1 ) - b1/m1,
       *
       * so that
       *
       * A^{-1} y = m2/m1 * z + b2/m1 - b1/m1 * A^{-1} 1
       *
       * This gives us the solution of A x = y.
       *
       * For natural boundary conditions, m1 = 2, b1 = -255.
       *
       * Now, z is the value stored as a uint16, so that m2 * z + b2
       * represents the value prior the conversion. Since the conversion
       * is
       *
       * z = 257 * a + 32767 + 1/2
       *
       * we have that the solution a we had prior to normalization was
       *
       * ( 2z - 65535 )/514, so that m2 = 1/257 and b2 = -255/2.
       *
       * It is easy to see that A^{-1} 1 = 1/6 when A =
       *
       * 5 1
       * 1 4 1
       *   1 4 1
       *     1 4 1
       *       1 4 1
       *         ...
       *           1 5
       *
       * (Here, m is a scalar, and b and c are constant shifts. There
       * is some abuse of notation in the above.)
       *
       * This implies that the shift if -85/2 (=b2/m1 - b1/m1 * 1/6)
       * and the scaling is m2/m1 = 1/514.
       *
       * If we are using uchars for storage, things are a bit
       * different:
       *
       * m1 and b1 are as before, but m2 = 1 and b2 = -127.5. This
       * gives shift = -85/2 and scaling = 1/2.
       *
       * The corresponding constants, unscaling and unscaled_shift are
       * defined in the calling program just prior to the first call.
       */

      a_ptr = a;

      /*
       * Row solve on a_middle:
       */

      a_row_p1 = a_middle;

      c = n_times_channels;
      do {
        *a_row_p1++ = ( (gint) *a_ptr++ + UNSCALED_SHIFT ) * unscaling;
      } while (--c);

      /*
       * Forward substitution:
       */

      /*
       * We'll take care of the very first pixel on the way back.
       */
      a_row_p2 = a_middle;
      a_row_p1 = a_middle+channels;

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C0_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C1_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C2_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C3_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C4_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C5_F;
      } while (--c);

      /*
       * Because integer overflow is not a concern any more---we do
       * clamp values later on, and we'd have to even in exact
       * arithmetic---we use the rounded value of CINFTY instead of
       * its truncated one.
       */
      c = n_minus_8_times_channels;
      while (c--) {
        *a_row_p1++ += *a_row_p2++ * MINUS_CINFTY_F;
      }

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p2++ * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;
        ++a_row_p1;
      } while (--c);

      a_row_p2 = --a_row_p1;

      c = channels;
      do {
        --a_row_p1;
      } while (--c);

      c = n_minus_7_times_channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * CINFTY_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C5_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C4_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C3_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C2_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C1_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C0_F;
        --a_row_p1;
      } while (--c);

      /*
       * Row solve on a_bottom:
       */
      a_row_p1 = a_bottom;

      c = n_times_channels;
      do {
        *a_row_p1++ = ( (gint) *a_ptr++ + UNSCALED_SHIFT ) * unscaling;
      } while (--c);

      a_row_p2 = a_bottom;
      a_row_p1 = a_bottom+channels;

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C0_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C1_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C2_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C3_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C4_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C5_F;
      } while (--c);

      c = n_minus_8_times_channels;
      while (c--) {
        *a_row_p1++ += *a_row_p2++ * MINUS_CINFTY_F;
      }

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p2++ * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;
        ++a_row_p1;
      } while (--c);

      a_row_p2 = --a_row_p1;

      c = channels;
      do {
        --a_row_p1;
      } while (--c);

      c = n_minus_7_times_channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * CINFTY_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C5_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C4_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C3_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C2_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C1_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C0_F;
        --a_row_p1;
      } while (--c);

      /*
       * Row solve on a_farbottom:
       */
      a_row_p1 = a_farbottom;

      c = n_times_channels;
      do {
        *a_row_p1++ = ( (gint) *a_ptr++ + UNSCALED_SHIFT ) * unscaling;
      } while (--c);

      a_row_p2 = a_farbottom;
      a_row_p1 = a_farbottom+channels;

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C0_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C1_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C2_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C3_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C4_F;
      } while (--c);

      c = channels;
      do {
        *a_row_p1++ += *a_row_p2++ * MINUS_C5_F;
      } while (--c);

      c = n_minus_8_times_channels;
      while (c--) {
        *a_row_p1++ += *a_row_p2++ * MINUS_CINFTY_F;
      }

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p2++ * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;
        ++a_row_p1;
      } while (--c);

      a_row_p2 = --a_row_p1;

      c = channels;
      do {
        --a_row_p1;
      } while (--c);

      c = n_minus_7_times_channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * CINFTY_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C5_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C4_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C3_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C2_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C1_F;
        --a_row_p1;
      } while (--c);

      c = channels;
      do {
        *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C0_F;
        --a_row_p1;
      } while (--c);

      last_ii = last_overlapping_ii[0];

      for (ii=0; ii<last_ii; ii++)
        {
          /*
           * Move output_row_ptr to the beginning of output_row:
           */
          output_row_ptr = output_row;

          middle_ii = middle[ii];
          bottom_ii = bottom[ii];

          a_middle_p = a_middle;
          a_bottom_p = a_bottom;

          c = 0;
          do {
            a_middle_center[c]  = *a_middle_p++;
            a_bottom_center[c]  = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_middle_right[c]    = *a_middle_p++;
            a_bottom_right[c]    = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_middle_farright[c] = *a_middle_p++;
            a_bottom_farright[c] = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            coef_center[c] =   middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
            coef_right[c]  =   middle_ii * a_middle_right[c]
                             + bottom_ii * a_bottom_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[0];

          for (jj=0; jj<last_jj; jj++)
            {
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
              } while (++c<channels);
            }

          center_jj  =   center[jj];
          right_jj   =    right[jj];
          farright_j = farright[0];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                      +  coef_right[c] * right_jj
                                      + (   middle_ii * a_middle_farright[c]
                                          + bottom_ii * a_bottom_farright[c]
                                            ) * farright_j );
          } while (++c<channels);

          jj++;

          j = 1;
          do {
            c = 0;
            do {
              a_middle_left[c]     = a_middle_center[c];
              a_middle_center[c]   = a_middle_right[c];
              a_middle_right[c]    = a_middle_farright[c];
              a_middle_farright[c] = *a_middle_p++;

              a_bottom_left[c]     = a_bottom_center[c];
              a_bottom_center[c]   = a_bottom_right[c];
              a_bottom_right[c]    = a_bottom_farright[c];
              a_bottom_farright[c] = *a_bottom_p++;

              coef_left[c]   =   middle_ii * a_middle_left[c]
                               + bottom_ii * a_bottom_left[c];
              coef_center[c] =   middle_ii * a_middle_center[c]
                               + bottom_ii * a_bottom_center[c];
              coef_right[c]  =   middle_ii * a_middle_right[c]
                               + bottom_ii * a_bottom_right[c];
            } while (++c<channels);

            last_jj = last_overlapping_jj[j];

            while (jj<last_jj)
              {
                left_jj   =   left[jj];
                center_jj = center[jj];
                right_jj  =  right[jj];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                            + coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
                } while (++c<channels);

                jj++;
              }

            left_jj    =    left[jj];
            center_jj  =  center[jj];
            right_jj   =   right[jj];
            farright_j = farright[j];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                        + coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj
                                        + (   middle_ii * a_middle_farright[c]
                                            + bottom_ii * a_bottom_farright[c]
                                              ) * farright_j );
            } while (++c<channels);

            jj++;

          } while (++j<n_minus_2);

          /*
           * Now, we deal with j = n-2.
           */
          c = 0;
          do {
            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];
            a_middle_right[c]  = a_middle_farright[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];
            a_bottom_right[c]  = a_bottom_farright[c];

            coef_left[c]   =   middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =   middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
            coef_right[c]  =   middle_ii * a_middle_right[c]
                             + bottom_ii * a_bottom_right[c];
          } while (++c<channels);


          last_jj = last_overlapping_jj[n_minus_2] + 1; /* The "+ 1"
                                                           is because
                                                           there is no
                                                           farright to
                                                           deal with
                                                           in any
                                                           case.
                                                           Consequently,
                                                           no
                                                           exception
                                                           needs to be
                                                           made for
                                                           the last
                                                           overlapping
                                                           fine
                                                           cell. */

          while (jj<last_jj)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj
                                          +  coef_right[c] * right_jj );
              } while (++c<channels);

              jj++;
            }

          /*
           * Now, we deal with j = n-1:
           */
          c = 0;
          do {
            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];

            coef_left[c]   =   middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =   middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
          } while (++c<channels);

          while (jj<nn)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj );
              } while (++c<channels);

              jj++;
            }

          /*
           * Write out the row:
           */
          gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

        }

      /*
       * Now, we take care of the last fine row which overlaps the
       * first coarse row. Right now, ii = last_ii:
       */
      output_row_ptr = output_row;

      middle_ii   = middle[ii];
      bottom_ii   = bottom[ii];
      farbottom_i = farbottom[0];

      a_middle_p    = a_middle;
      a_bottom_p    = a_bottom;
      a_farbottom_p = a_farbottom;

      c = 0;
      do {
        a_middle_center[c]      = *a_middle_p++;
        a_bottom_center[c]      = *a_bottom_p++;
        a_farbottom_center[c]   = *a_farbottom_p++;
      } while (++c<channels);

      c = 0;
      do {
        a_middle_right[c]       = *a_middle_p++;
        a_bottom_right[c]       = *a_bottom_p++;
        a_farbottom_right[c]    = *a_farbottom_p++;
      } while (++c<channels);

      c = 0;
      do {
        a_middle_farright[c]    = *a_middle_p++;
        a_bottom_farright[c]    = *a_bottom_p++;
        a_farbottom_farright[c] = *a_farbottom_p++;
      } while (++c<channels);

      c = 0;
      do {
        coef_center[c] =     middle_ii * a_middle_center[c]
                         +   bottom_ii * a_bottom_center[c]
                         + farbottom_i * a_farbottom_center[c];
        coef_right[c]  =     middle_ii * a_middle_right[c]
                         +   bottom_ii * a_bottom_right[c]
                         + farbottom_i * a_farbottom_right[c];
      } while (++c<channels);

      last_jj = last_overlapping_jj[0];

      for (jj=0; jj<last_jj; jj++)
        {
          center_jj = center[jj];
          right_jj  =  right[jj];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                      +  coef_right[c] * right_jj );
          } while (++c<channels);
        }

      center_jj  =  center[jj];
      right_jj   =   right[jj];
      farright_j = farright[0];

      c = 0;
      do {
        *output_row_ptr++ =
          F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                  +  coef_right[c] * right_jj
                                  + (     middle_ii * a_middle_farright[c]
                                      +   bottom_ii * a_bottom_farright[c]
                                      + farbottom_i * a_farbottom_farright[c]
                                          ) * farright_j );
      } while (++c<channels);

      jj++;

      j = 1;
      do {
        c = 0;
        do {
          a_middle_left[c]        = a_middle_center[c];
          a_middle_center[c]      = a_middle_right[c];
          a_middle_right[c]       = a_middle_farright[c];
          a_middle_farright[c]    = *a_middle_p++;

          a_bottom_left[c]        = a_bottom_center[c];
          a_bottom_center[c]      = a_bottom_right[c];
          a_bottom_right[c]       = a_bottom_farright[c];
          a_bottom_farright[c]    = *a_bottom_p++;

          a_farbottom_left[c]     = a_farbottom_center[c];
          a_farbottom_center[c]   = a_farbottom_right[c];
          a_farbottom_right[c]    = a_farbottom_farright[c];
          a_farbottom_farright[c] = *a_farbottom_p++;

          coef_left[c]   =     middle_ii * a_middle_left[c]
                           +   bottom_ii * a_bottom_left[c]
                           + farbottom_i * a_farbottom_left[c];
          coef_center[c] =     middle_ii * a_middle_center[c]
                           +   bottom_ii * a_bottom_center[c]
                           + farbottom_i * a_farbottom_center[c];
          coef_right[c]  =     middle_ii * a_middle_right[c]
                           +   bottom_ii * a_bottom_right[c]
                           + farbottom_i * a_farbottom_right[c];
        } while (++c<channels);

        last_jj = last_overlapping_jj[j];

        while (jj<last_jj)
          {
            left_jj   =   left[jj];
            center_jj = center[jj];
            right_jj  =  right[jj];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                        + coef_center[c] * center_jj
                                        + coef_right[c]  * right_jj );
            } while (++c<channels);

            jj++;
          }

        left_jj    =    left[jj];
        center_jj  =  center[jj];
        right_jj   =   right[jj];
        farright_j = farright[j];

        c = 0;
        do {
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                    + coef_center[c] * center_jj
                                    +  coef_right[c] * right_jj
                                  + (      middle_ii * a_middle_farright[c]
                                       +    bottom_ii * a_bottom_farright[c]
                                       +  farbottom_i * a_farbottom_farright[c]
                                           ) * farright_j );
        } while (++c<channels);

        jj++;

      } while (++j<n_minus_2);

      /*
       * Now, we deal with j = n-2:
       */
      c = 0;
      do {
        a_middle_left[c]      = a_middle_center[c];
        a_middle_center[c]    = a_middle_right[c];
        a_middle_right[c]     = a_middle_farright[c];

        a_bottom_left[c]      = a_bottom_center[c];
        a_bottom_center[c]    = a_bottom_right[c];
        a_bottom_right[c]     = a_bottom_farright[c];

        a_farbottom_left[c]   = a_farbottom_center[c];
        a_farbottom_center[c] = a_farbottom_right[c];
        a_farbottom_right[c]  = a_farbottom_farright[c];

        coef_left[c]   =     middle_ii * a_middle_left[c]
                         +   bottom_ii * a_bottom_left[c]
                         + farbottom_i * a_farbottom_left[c];
        coef_center[c] =     middle_ii * a_middle_center[c]
                         +   bottom_ii * a_bottom_center[c]
                         + farbottom_i * a_farbottom_center[c];
        coef_right[c]  =     middle_ii * a_middle_right[c]
                         +   bottom_ii * a_bottom_right[c]
                         + farbottom_i * a_farbottom_right[c];
      } while (++c<channels);

      last_jj = last_overlapping_jj[n_minus_2] + 1;

      while (jj<last_jj)
        {
          left_jj   =   left[jj];
          center_jj = center[jj];
          right_jj  =  right[jj];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                      + coef_center[c] * center_jj
                                      + coef_right[c]  * right_jj );
          } while (++c<channels);

          jj++;
        }

      /*
       * Now, we deal with j = n-1:
       */
      c = 0;
      do {
        a_middle_left[c]      = a_middle_center[c];
        a_middle_center[c]    = a_middle_right[c];

        a_bottom_left[c]      = a_bottom_center[c];
        a_bottom_center[c]    = a_bottom_right[c];

        a_farbottom_left[c]   = a_farbottom_center[c];
        a_farbottom_center[c] = a_farbottom_right[c];

        coef_left[c]   =     middle_ii * a_middle_left[c]
                         +   bottom_ii * a_bottom_left[c]
                         + farbottom_i * a_farbottom_left[c];
        coef_center[c] =     middle_ii * a_middle_center[c]
                         +   bottom_ii * a_bottom_center[c]
                         + farbottom_i * a_farbottom_center[c];
      } while (++c<channels);

      while (jj<nn)
        {
          left_jj   =   left[jj];
          center_jj = center[jj];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                      + coef_center[c] * center_jj );
          } while (++c<channels);

        jj++;
      }

      gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

      ii++;

      gimp_progress_update(.01+.2*(m*(gdouble)n)/(mm*(gdouble)nn));

      /*
       * Now, we deal with the second and later coarse rows, up to,
       * and including, the third to last:
       */
      i = 1;
      do {
        a_temp      = a_top;
        a_top       = a_middle;
        a_middle    = a_bottom;
        a_bottom    = a_farbottom;
        a_farbottom = a_temp;

        /*
         * Row solve on a_farbottom:
         */
        a_row_p1 = a_farbottom;

        c = n_times_channels;
        do {
          *a_row_p1++ = ( (gint) *a_ptr++ + UNSCALED_SHIFT ) * unscaling;
        } while (--c);

        a_row_p2 = a_farbottom;
        a_row_p1 = a_farbottom+channels;

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C0_F;
        } while (--c);

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C1_F;
        } while (--c);

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C2_F;
        } while (--c);

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C3_F;
        } while (--c);

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C4_F;
        } while (--c);

        c = channels;
        do {
          *a_row_p1++ += *a_row_p2++ * MINUS_C5_F;
        } while (--c);

        c = n_minus_8_times_channels;
        while (c--) {
          *a_row_p1++ += *a_row_p2++ * MINUS_CINFTY_F;
        }

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p2++ * MINUS_CINFTY_F + *a_row_p1 ) * CLAST_F;
          ++a_row_p1;
        } while (--c);

        a_row_p2 = --a_row_p1;

        c = channels;
        do {
          --a_row_p1;
        } while (--c);

        c = n_minus_7_times_channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * CINFTY_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C5_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C4_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C3_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C2_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C1_F;
          --a_row_p1;
        } while (--c);

        c = channels;
        do {
          *a_row_p1 = ( *a_row_p1 - *a_row_p2-- ) * C0_F;
          --a_row_p1;
        } while (--c);

        last_ii = last_overlapping_ii[i];

        while (ii<last_ii)
          {
            output_row_ptr = output_row;

            top_ii    =    top[ii];
            middle_ii = middle[ii];
            bottom_ii = bottom[ii];

            a_top_p       = a_top;
            a_middle_p    = a_middle;
            a_bottom_p    = a_bottom;

            c = 0;
            do {
              a_top_center[c]      = *a_top_p++;
              a_middle_center[c]   = *a_middle_p++;
              a_bottom_center[c]   = *a_bottom_p++;
            } while (++c<channels);

            c = 0;
            do {
              a_top_right[c]       = *a_top_p++;
              a_middle_right[c]    = *a_middle_p++;
              a_bottom_right[c]    = *a_bottom_p++;
            } while (++c<channels);

            c = 0;
            do {
              a_top_farright[c]    = *a_top_p++;
              a_middle_farright[c] = *a_middle_p++;
              a_bottom_farright[c] = *a_bottom_p++;
            } while (++c<channels);

            c = 0;
            do {
              coef_center[c] =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c]
                               + bottom_ii * a_bottom_center[c];
              coef_right[c]  =      top_ii * a_top_right[c]
                               + middle_ii * a_middle_right[c]
                               + bottom_ii * a_bottom_right[c];
            } while (++c<channels);

            last_jj = last_overlapping_jj[0];

            for (jj=0; jj<last_jj; jj++)
              {
                center_jj = center[jj];
                right_jj  =  right[jj];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
                } while (++c<channels);
              }

            center_jj  =  center[jj];
            right_jj   =   right[jj];
            farright_j = farright[0];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj
                                        + (       top_ii * a_top_farright[c]
                                            +  middle_ii * a_middle_farright[c]
                                            +  bottom_ii * a_bottom_farright[c]
                                                  ) * farright_j );
            } while (++c<channels);

            jj++;

            j = 1;
            while (j<n_minus_2)
              {
                c = 0;
                do {
                  a_top_left[c]        = a_top_center[c];
                  a_top_center[c]      = a_top_right[c];
                  a_top_right[c]       = a_top_farright[c];
                  a_top_farright[c]    = *a_top_p++;

                  a_middle_left[c]     = a_middle_center[c];
                  a_middle_center[c]   = a_middle_right[c];
                  a_middle_right[c]    = a_middle_farright[c];
                  a_middle_farright[c] = *a_middle_p++;

                  a_bottom_left[c]     = a_bottom_center[c];
                  a_bottom_center[c]   = a_bottom_right[c];
                  a_bottom_right[c]    = a_bottom_farright[c];
                  a_bottom_farright[c] = *a_bottom_p++;

                  coef_left[c]   =      top_ii * a_top_left[c]
                                   + middle_ii * a_middle_left[c]
                                   + bottom_ii * a_bottom_left[c];
                  coef_center[c] =      top_ii * a_top_center[c]
                                   + middle_ii * a_middle_center[c]
                                   + bottom_ii * a_bottom_center[c];
                  coef_right[c]  =      top_ii * a_top_right[c]
                                   + middle_ii * a_middle_right[c]
                                   + bottom_ii * a_bottom_right[c];
                } while (++c<channels);

                last_jj = last_overlapping_jj[j];

                while (jj<last_jj)
                  {
                    left_jj   =   left[jj];
                    center_jj = center[jj];
                    right_jj  =  right[jj];

                    c = 0;
                    do {
                      *output_row_ptr++ =
                        F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                                + coef_center[c] * center_jj
                                                + coef_right[c]  * right_jj );
                    } while (++c<channels);

                    jj++;
                  }

                left_jj    =    left[jj];
                center_jj  =  center[jj];
                right_jj   =   right[jj];
                farright_j = farright[j];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                            + coef_center[c] * center_jj
                                            + coef_right[c]  * right_jj
                                          + (    top_ii * a_top_farright[c]
                                             + middle_ii * a_middle_farright[c]
                                             + bottom_ii * a_bottom_farright[c]
                                                 ) * farright_j );
                } while (++c<channels);

                jj++;

                j++;

              }

            /*
             * Now, we deal with j = n-2:
             */
            c = 0;
            do {
              a_top_left[c]      = a_top_center[c];
              a_top_center[c]    = a_top_right[c];
              a_top_right[c]     = a_top_farright[c];

              a_middle_left[c]   = a_middle_center[c];
              a_middle_center[c] = a_middle_right[c];
              a_middle_right[c]  = a_middle_farright[c];

              a_bottom_left[c]   = a_bottom_center[c];
              a_bottom_center[c] = a_bottom_right[c];
              a_bottom_right[c]  = a_bottom_farright[c];

              coef_left[c]   =      top_ii * a_top_left[c]
                               + middle_ii * a_middle_left[c]
                               + bottom_ii * a_bottom_left[c];
              coef_center[c] =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c]
                               + bottom_ii * a_bottom_center[c];
              coef_right[c]  =      top_ii * a_top_right[c]
                               + middle_ii * a_middle_right[c]
                               + bottom_ii * a_bottom_right[c];
            } while (++c<channels);

            last_jj = last_overlapping_jj[n_minus_2] + 1;

            while (jj<last_jj)
              {
                left_jj   =   left[jj];
                center_jj = center[jj];
                right_jj  =  right[jj];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                            + coef_center[c] * center_jj
                                            + coef_right[c]  * right_jj );
                } while (++c<channels);

                jj++;
              }

            /*
             * Now, we deal with j = n-1:
             */
            c = 0;
            do {
              a_top_left[c]      = a_top_center[c];
              a_top_center[c]    = a_top_right[c];

              a_middle_left[c]   = a_middle_center[c];
              a_middle_center[c] = a_middle_right[c];

              a_bottom_left[c]   = a_bottom_center[c];
              a_bottom_center[c] = a_bottom_right[c];

              coef_left[c]   =      top_ii * a_top_left[c]
                               + middle_ii * a_middle_left[c]
                               + bottom_ii * a_bottom_left[c];
              coef_center[c] =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c]
                               + bottom_ii * a_bottom_center[c];
            } while (++c<channels);

            while (jj<nn)
              {
                left_jj   =   left[jj];
                center_jj = center[jj];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                            + coef_center[c] * center_jj );
                } while (++c<channels);

                jj++;
              }

            gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

            ii++;
          }

        /*
         * Now, we take care of the last fine row which overlaps the
         * current coarse row. Right now, ii = last_ii:
         */
        output_row_ptr = output_row;

        top_ii      = top[ii];
        middle_ii   = middle[ii];
        bottom_ii   = bottom[ii];
        farbottom_i = farbottom[i];

        a_top_p       = a_top;
        a_middle_p    = a_middle;
        a_bottom_p    = a_bottom;
        a_farbottom_p = a_farbottom;

        c = 0;
        do {
          a_top_center[c]         = *a_top_p++;
          a_middle_center[c]      = *a_middle_p++;
          a_bottom_center[c]      = *a_bottom_p++;
          a_farbottom_center[c]   = *a_farbottom_p++;
        } while (++c<channels);

        c = 0;
        do {
          a_top_right[c]          = *a_top_p++;
          a_middle_right[c]       = *a_middle_p++;
          a_bottom_right[c]       = *a_bottom_p++;
          a_farbottom_right[c]    = *a_farbottom_p++;
        } while (++c<channels);

        c = 0;
        do {
          a_top_farright[c]       = *a_top_p++;
          a_middle_farright[c]    = *a_middle_p++;
          a_bottom_farright[c]    = *a_bottom_p++;
          a_farbottom_farright[c] = *a_farbottom_p++;
        } while (++c<channels);

        c = 0;
        do {
          coef_center[c] =        top_ii * a_top_center[c]
                           +   middle_ii * a_middle_center[c]
                           +   bottom_ii * a_bottom_center[c]
                           + farbottom_i * a_farbottom_center[c];
          coef_right[c]  =        top_ii * a_top_right[c]
                           +   middle_ii * a_middle_right[c]
                           +   bottom_ii * a_bottom_right[c]
                           + farbottom_i * a_farbottom_right[c];
        } while (++c<channels);

        last_jj = last_overlapping_jj[0];

        for (jj=0; jj<last_jj; jj++)
          {
            center_jj = center[jj];
            right_jj  =  right[jj];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj );
            } while (++c<channels);

          }

        center_jj = center[jj];
        right_jj  =  right[jj];
        farright_j = farright[0];

        c = 0;
        do {
          *output_row_ptr++ =
            F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                    +  coef_right[c] * right_jj
                                    + (        top_ii * a_top_farright[c]
                                        +   middle_ii * a_middle_farright[c]
                                        +   bottom_ii * a_bottom_farright[c]
                                        + farbottom_i * a_farbottom_farright[c]
                                               ) * farright_j );
        } while (++c<channels);

        jj++;

        j = 1;
        do {
          c = 0;
          do {
            a_top_left[c]           = a_top_center[c];
            a_top_center[c]         = a_top_right[c];
            a_top_right[c]          = a_top_farright[c];
            a_top_farright[c]       = *a_top_p++;

            a_middle_left[c]        = a_middle_center[c];
            a_middle_center[c]      = a_middle_right[c];
            a_middle_right[c]       = a_middle_farright[c];
            a_middle_farright[c]    = *a_middle_p++;

            a_bottom_left[c]        = a_bottom_center[c];
            a_bottom_center[c]      = a_bottom_right[c];
            a_bottom_right[c]       = a_bottom_farright[c];
            a_bottom_farright[c]    = *a_bottom_p++;

            a_farbottom_left[c]     = a_bottom_center[c];
            a_farbottom_center[c]   = a_bottom_right[c];
            a_farbottom_right[c]    = a_bottom_farright[c];
            a_farbottom_farright[c] = *a_farbottom_p++;

            coef_left[c]   =        top_ii * a_top_left[c]
                             +   middle_ii * a_middle_left[c]
                             +   bottom_ii * a_bottom_left[c]
                             + farbottom_i * a_farbottom_left[c];
            coef_center[c] =        top_ii * a_top_center[c]
                             +   middle_ii * a_middle_center[c]
                             +   bottom_ii * a_bottom_center[c]
                             + farbottom_i * a_farbottom_center[c];
            coef_right[c]  =        top_ii * a_top_right[c]
                             +   middle_ii * a_middle_right[c]
                             +   bottom_ii * a_bottom_right[c]
                             + farbottom_i * a_farbottom_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[j];

          while (jj<last_jj)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                          + coef_center[c] * center_jj
                                          + coef_right[c]  * right_jj );
              } while (++c<channels);

              jj++;
            }

          left_jj    =    left[jj];
          center_jj  =  center[jj];
          right_jj   =   right[jj];
          farright_j = farright[j];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                      + coef_center[c] * center_jj
                                      + coef_right[c]  * right_jj
                                      + (      top_ii * a_top_farright[c]
                                        +   middle_ii * a_middle_farright[c]
                                        +   bottom_ii * a_bottom_farright[c]
                                        + farbottom_i * a_farbottom_farright[c]
                                               ) * farright_j );
            } while (++c<channels);

          jj++;

        } while (++j<n_minus_2);

        c = 0;
        do {
          a_top_left[c]         = a_top_center[c];
          a_top_center[c]       = a_top_right[c];
          a_top_right[c]        = a_top_farright[c];

          a_middle_left[c]      = a_middle_center[c];
          a_middle_center[c]    = a_middle_right[c];
          a_middle_right[c]     = a_middle_farright[c];

          a_bottom_left[c]      = a_bottom_center[c];
          a_bottom_center[c]    = a_bottom_right[c];
          a_bottom_right[c]     = a_bottom_farright[c];

          a_farbottom_left[c]   = a_farbottom_center[c];
          a_farbottom_center[c] = a_farbottom_right[c];
          a_farbottom_right[c]  = a_farbottom_farright[c];

          coef_left[c]   =        top_ii * a_top_left[c]
                           +   middle_ii * a_middle_left[c]
                           +   bottom_ii * a_bottom_left[c]
                           + farbottom_i * a_farbottom_left[c];
          coef_center[c] =        top_ii * a_top_center[c]
                           +   middle_ii * a_middle_center[c]
                           +   bottom_ii * a_bottom_center[c]
                           + farbottom_i * a_farbottom_center[c];
          coef_right[c]  =        top_ii * a_top_right[c]
                           +   middle_ii * a_middle_right[c]
                           +   bottom_ii * a_bottom_right[c]
                           + farbottom_i * a_farbottom_right[c];
        } while (++c<channels);

        last_jj = last_overlapping_jj[n_minus_2] + 1;

        while (jj<last_jj)
          {
            left_jj   =   left[jj];
            center_jj = center[jj];
            right_jj  =  right[jj];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                        + coef_center[c] * center_jj
                                        + coef_right[c]  * right_jj );
            } while (++c<channels);

            jj++;
          }

        /*
         * Now, we deal with j = n-1:
         */
        c = 0;
        do {
          a_top_left[c]         = a_top_center[c];
          a_top_center[c]       = a_top_right[c];

          a_middle_left[c]      = a_middle_center[c];
          a_middle_center[c]    = a_middle_right[c];

          a_bottom_left[c]      = a_bottom_center[c];
          a_bottom_center[c]    = a_bottom_right[c];

          a_farbottom_left[c]   = a_farbottom_center[c];
          a_farbottom_center[c] = a_farbottom_right[c];

          coef_left[c]   =        top_ii * a_top_left[c]
                           +   middle_ii * a_middle_left[c]
                           +   bottom_ii * a_bottom_left[c]
                           + farbottom_i * a_farbottom_left[c];
          coef_center[c] =        top_ii * a_top_center[c]
                           +   middle_ii * a_middle_center[c]
                           +   bottom_ii * a_bottom_center[c]
                           + farbottom_i * a_farbottom_center[c];
        } while (++c<channels);

        while (jj<nn)
          {
            left_jj   =   left[jj];
            center_jj = center[jj];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(   coef_left[c]   * left_jj
                                        + coef_center[c] * center_jj );
            } while (++c<channels);

            jj++;
          }

        gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

        ii++;
      } while (++i<m_minus_2);

      gimp_progress_update(.75);

      /*
       * Now, we deal with the second to last coarse row:
       */
      a_temp      = a_top;
      a_top       = a_middle;
      a_middle    = a_bottom;
      a_bottom    = a_farbottom;
      a_farbottom = a_temp;

      last_ii = last_overlapping_ii[m_minus_2] + 1; /* The "+1" comes
                                                       from the fact
                                                       that no
                                                       exception needs
                                                       to be made for
                                                       farbottom,
                                                       because there
                                                       is none. */

      while (ii<last_ii)
        {
          output_row_ptr = output_row;

          top_ii    =    top[ii];
          middle_ii = middle[ii];
          bottom_ii = bottom[ii];

          a_top_p    = a_top;
          a_middle_p = a_middle;
          a_bottom_p = a_bottom;

          c = 0;
          do {
            a_top_center[c]      = *a_top_p++;
            a_middle_center[c]   = *a_middle_p++;
            a_bottom_center[c]   = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_top_right[c]       = *a_top_p++;
            a_middle_right[c]    = *a_middle_p++;
            a_bottom_right[c]    = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_top_farright[c]    = *a_top_p++;
            a_middle_farright[c] = *a_middle_p++;
            a_bottom_farright[c] = *a_bottom_p++;
          } while (++c<channels);

          c = 0;
          do {
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
            coef_right[c]  =      top_ii * a_top_right[c]
                             + middle_ii * a_middle_right[c]
                             + bottom_ii * a_bottom_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[0];

          for (jj=0; jj<last_jj; jj++)
            {
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                          +  coef_right[c] * right_jj );
              } while (++c<channels);
            }

          center_jj  = center[jj];
          right_jj   =  right[jj];
          farright_j = farright[0];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                      +  coef_right[c] * right_jj
                                      + (      top_ii * a_top_farright[c]
                                          + middle_ii * a_middle_farright[c]
                                          + bottom_ii * a_bottom_farright[c]
                                               ) * farright_j );
          } while (++c<channels);

          jj++;

          j = 1;
          do {
              c = 0;
              do {
                a_top_left[c]        = a_top_center[c];
                a_top_center[c]      = a_top_right[c];
                a_top_right[c]       = a_top_farright[c];
                a_top_farright[c]    = *a_top_p++;

                a_middle_left[c]     = a_middle_center[c];
                a_middle_center[c]   = a_middle_right[c];
                a_middle_right[c]    = a_middle_farright[c];
                a_middle_farright[c] = *a_middle_p++;

                a_bottom_left[c]     = a_bottom_center[c];
                a_bottom_center[c]   = a_bottom_right[c];
                a_bottom_right[c]    = a_bottom_farright[c];
                a_bottom_farright[c] = *a_bottom_p++;

                coef_left[c]   =      top_ii * a_top_left[c]
                                 + middle_ii * a_middle_left[c]
                                 + bottom_ii * a_bottom_left[c];
                coef_center[c] =      top_ii * a_top_center[c]
                                 + middle_ii * a_middle_center[c]
                                 + bottom_ii * a_bottom_center[c];
                coef_right[c]  =      top_ii * a_top_right[c]
                                 + middle_ii * a_middle_right[c]
                                 + bottom_ii * a_bottom_right[c];
              } while (++c<channels);

              last_jj = last_overlapping_jj[j];

              for (; jj<last_jj; jj++)
                {
                  left_jj   =   left[jj];
                  center_jj = center[jj];
                  right_jj  =  right[jj];

                  c = 0;
                  do {
                    *output_row_ptr++ =
                      F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                              + coef_center[c] * center_jj
                                              +  coef_right[c] * right_jj );
                  } while (++c<channels);
                }

              left_jj    =    left[jj];
              center_jj  =  center[jj];
              right_jj   =   right[jj];
              farright_j = farright[j];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj
                                          +  coef_right[c] * right_jj
                                          + (      top_ii * a_top_farright[c]
                                             + middle_ii * a_middle_farright[c]
                                             + bottom_ii * a_bottom_farright[c]
                                                   ) * farright_j );
              } while (++c<channels);

              jj++;

            } while (++j<n_minus_2);

          /*
           * Now, we deal with j = n-2:
           */
          c = 0;
          do {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];
            a_top_right[c]     = a_top_farright[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];
            a_middle_right[c]  = a_middle_farright[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];
            a_bottom_right[c]  = a_bottom_farright[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
            coef_right[c]  =      top_ii * a_top_right[c]
                             + middle_ii * a_middle_right[c]
                             + bottom_ii * a_bottom_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[n_minus_2] + 1;

          while (jj<last_jj)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj
                                          +  coef_right[c] * right_jj );
              } while (++c<channels);

              jj++;
            }

          /*
           * Now, we deal with j = n-1:
           */
          c = 0;
          do {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];

            a_bottom_left[c]   = a_bottom_center[c];
            a_bottom_center[c] = a_bottom_right[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c]
                             + bottom_ii * a_bottom_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c]
                             + bottom_ii * a_bottom_center[c];
          } while (++c<channels);

          while (jj<nn)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj );
              } while (++c<channels);

              jj++;
            }

          gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

          ii++;
        }

      /*
       * Compute the last coarse row:
       */
      a_temp   = a_top;
      a_top    = a_middle;
      a_middle = a_bottom;
      a_bottom = a_temp;

      while (ii<mm)
        {
          output_row_ptr = output_row;

          top_ii    = top[ii];
          middle_ii = middle[ii];

          a_top_p    = a_top;
          a_middle_p = a_middle;

          c = 0;
          do {
            a_top_center[c]     = *a_top_p++;
            a_middle_center[c]  = *a_middle_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_top_right[c]       = *a_top_p++;
            a_middle_right[c]    = *a_middle_p++;
          } while (++c<channels);

          c = 0;
          do {
            a_top_farright[c]    = *a_top_p++;
            a_middle_farright[c] = *a_middle_p++;
          } while (++c<channels);

          c = 0;
          do {
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c];
            coef_right[c]  =      top_ii * a_top_right[c]
                             + middle_ii * a_middle_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[0];

          for (jj=0; jj<last_jj; jj++)
            {
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                          + coef_right[c]  * right_jj );
              } while (++c<channels);
            }

          center_jj = center[jj];
          right_jj  =  right[jj];
          farright_j = farright[0];

          c = 0;
          do {
            *output_row_ptr++ =
              F_ROUND_AND_CLAMP_0255(   coef_center[c] * center_jj
                                      + coef_right[c]  * right_jj
                                      + (      top_ii * a_top_farright[c]
                                          + middle_ii * a_middle_farright[c]
                                               ) * farright_j );
          } while (++c<channels);

          jj++;

          j = 1;
          do {
            c = 0;
            do {
              a_top_left[c]        = a_top_center[c];
              a_top_center[c]      = a_top_right[c];
              a_top_right[c]       = a_top_farright[c];
              a_top_farright[c]    = *a_top_p++;

              a_middle_left[c]     = a_middle_center[c];
              a_middle_center[c]   = a_middle_right[c];
              a_middle_right[c]    = a_middle_farright[c];
              a_middle_farright[c] = *a_middle_p++;

              coef_left[c]   =      top_ii * a_top_left[c]
                               + middle_ii * a_middle_left[c];
              coef_center[c] =      top_ii * a_top_center[c]
                               + middle_ii * a_middle_center[c];
              coef_right[c]  =      top_ii * a_top_right[c]
                               + middle_ii * a_middle_right[c];
            } while (++c<channels);

            last_jj = last_overlapping_jj[j];

            for (; jj<last_jj; jj++)
              {
                left_jj   =   left[jj];
                center_jj = center[jj];
                right_jj  =  right[jj];

                c = 0;
                do {
                  *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                            + coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
                } while (++c<channels);
              }

            left_jj    =    left[jj];
            center_jj  =  center[jj];
            right_jj   =   right[jj];
            farright_j = farright[j];

            c = 0;
            do {
              *output_row_ptr++ =
                F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                        + coef_center[c] * center_jj
                                        +  coef_right[c] * right_jj
                                        + (      top_ii * a_top_farright[c]
                                            + middle_ii * a_middle_farright[c]
                                                 ) * farright_j );
            } while (++c<channels);

            jj++;
          } while (++j<n_minus_2);

          /*
           * Now, we deal with j = n-2:
           */
          c = 0;
          do {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];
            a_top_right[c]     = a_top_farright[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];
            a_middle_right[c]  = a_middle_farright[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c];
            coef_right[c]  =      top_ii * a_top_right[c]
                             + middle_ii * a_middle_right[c];
          } while (++c<channels);

          last_jj = last_overlapping_jj[n_minus_2] + 1;

          while (jj<last_jj)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];
              right_jj  =  right[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                    F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                            + coef_center[c] * center_jj
                                            +  coef_right[c] * right_jj );
                } while (++c<channels);

              jj++;
            }

          /*
           * Now, we deal with j = n-1:
           */
          c = 0;
          do {
            a_top_left[c]      = a_top_center[c];
            a_top_center[c]    = a_top_right[c];

            a_middle_left[c]   = a_middle_center[c];
            a_middle_center[c] = a_middle_right[c];

            coef_left[c]   =      top_ii * a_top_left[c]
                             + middle_ii * a_middle_left[c];
            coef_center[c] =      top_ii * a_top_center[c]
                             + middle_ii * a_middle_center[c];
          } while (++c<channels);

          while (jj<nn)
            {
              left_jj   =   left[jj];
              center_jj = center[jj];

              c = 0;
              do {
                *output_row_ptr++ =
                  F_ROUND_AND_CLAMP_0255(     coef_left[c] * left_jj
                                          + coef_center[c] * center_jj );
              } while (++c<channels);

              jj++;
            }

          gimp_pixel_rgn_set_row (&output_image, output_row, 0, ii, nn);

          ii++;
        }

      g_free(output_row);
      g_free(a_farbottom);
      g_free(a_top);
      g_free(a_bottom);
      g_free(a_middle);
    }

  g_free(a);

  }

  gimp_drawable_flush (new_drawable);
  gimp_drawable_merge_shadow (new_drawable->drawable_id, FALSE);
  gimp_drawable_update (new_drawable->drawable_id, 0, 0, nn, mm);
  gimp_drawable_detach (new_drawable);

  return new_image_ID;
}

static void
last_overlapping_index (gint o,
                        gint oo,
                        gint last_overlapping_kk[])
{
  /*
   * The comments use the variables which are relevant to the use of
   * this function in the horizontal direction. That is, n is o, nn is
   * oo, j is k, and jj is kk.
   *
   * This code uses a general convention that the input pixels have
   * unit sides, and that (describing things in 1D) the global
   * coordinate of the left endpoint of an input cell is exactly its
   * index. That, is the first input cell goes from 0 to 1, the last
   * one from n-1 to n. Given that,
   *
   * jj * n / nn (in rational arithmetic)
   *
   * is the absolute coordinate of the left endpoint of the index jj
   * fine cell (recall that jj starts at zero).  Given that the left
   * endpoints j of the coarse cells are 0, 1, 2... this implies that
   *
   * (j * nn ) / n (in integer arithmetic)
   *
   * is the index of the first fine cell which is contained in the
   * index j coarse cell, so that
   *
   * ((j+1) * nn ) / n - 1 (in integer arithmetic)
   *
   * is the index of the last fine cell which overlaps the index j
   * coarse cell.
   *
   * There is another way of caracterizing this index: It is the
   * largest jj such that jj * n < j + 1. Using this caracterization
   * allows one to avoid integer divisions, and because by sweeping
   * through the jjs we know approximately what this index is, few
   * iterations are necessary. The key idea is as follows: whenever
   * the product of the next candidate jj by n initially fails the
   * above condition, the current jj is last_jj for this value of j.
   *
   * If n=nn, the last overlapping index is equal to the index.
   */
  gint o_minus_1 = o-1; /* Useful iteration bound. */

  gint k = 0;

  if (oo>o)
  {
    gint k_plus_one_times_oo = oo;

    gint kk = 0;
    gint kk_plus_one_times_o = o;

    do
    {
      ++kk; /* Because 1 < n <= nn, we know that the first overlapping
               jj can't be the last overlapping jj, so we can
               increment without checking the condition. */
      kk_plus_one_times_o += o;

      while (kk_plus_one_times_o < k_plus_one_times_oo)
      {
        ++kk; /* If the next jj satisfies the key condition, make it jj. */
        kk_plus_one_times_o += o;
      }

      last_overlapping_kk[k] = kk; /* We have found the last
                                      overlapping jj for the current
                                      value of j. */
      k_plus_one_times_oo += oo;
    }
    while (++k<o_minus_1);
  }
  else
  {
    do
    {
      last_overlapping_kk[k] = k;
    }
    while (++k<o_minus_1);
  }
}

/*
 * coarse_to_fine_coefficients ()
 *
 * This function is called twice: once in the "vertical" direction,
 * and once in the "horizontal" direction.
 *
 * The comments use the variables which are relevant to the use of
 * this function in the horizontal direction. That is, n is o, nn is
 * oo, j is k, and jj is kk.
 *
 * "INPUTS:" number of coarse cells n
 *           number of fine cells nn
 *           one int array with at least n-1 entries: last_overlapping_jj[]
 *
 * "OUTPUTS:" two double arrays with at least nn entries: left, center.
 *            one double array with at least nn-(nn/n) entries: right
 *            one double array with at least n entries: farright.
 *
 * Note that the very last entry of farright gets some storage, but
 * should not be used (incorrect value). The program could be
 * rewritten so that it only uses the first n-1 entries of farright.
 *
 * Also, the first nn/n + 1 entries of left should not be used (they do
 * not receive a value).
 *
 */
static void
coarse_to_fine_coefficients (gint   o,
                             gint   oo,
                             gint   last_overlapping_kk[],
                             gfloat left[],
                             gfloat center[],
                             gfloat right[],
                             gfloat farright[])
{
  /*
   * Possible improvements: use long or unsigned integers to prevent
   * integer overflow: integers as large as n times nn are computed.
   * This does not appear to have been a problem, but it could be.
   */

  /*
   * Possible improvement: Use left/right symmetry to eliminate the
   * computation of half the coefficients. This is a Theta(m+n)
   * improvement (probably not worth the trouble). Because of the way
   * indices "stay left" for fine cells which overlap two coarse
   * cells, this is not as simple as it looks, unless there are no
   * overlapping fine cells, that is, unless the magnification factor
   * is an integer.
   */

  /*
   * Formulas for the antiderivatives of the cardinal basis functions,
   * with constants of integration set so that they are equal to zero
   * at the left boundary of the cell of interest: They are structured
   * a la Horner to minimize flops. Because x is Theta(1), some mild
   * cancellation will occur.
   */

  /*
   * The following describe the centered B-Spline (applicable in the
   * interior):
   */
#define LEFT_BSPLINE(x)            ( (x) *         (x) *       (x))
#define CENTER_BSPLINE(x)          ( (x) * (  3. - (x) * (-3.+(x)+(x))))
#define RIGHT_BSPLINE(x)           ( (x) * (  3. + (x) * (-3.+(x))))

  /*
   * For natural boundary conditions, LEFT_BDRY_LEFT_SPLINE and
   * RIGHT_BDRY_RIGHT_SPLINE are identical to those which apply in the
   * interior. Consequently, they are redundant and could be removed
   * from the code.
   */
#define LEFT_BDRY_SPLINE(x)        ( (x) * (  6. -         (x)*(x)))
#define LEFT_BDRY_LEFT_SPLINE(x)   ( (x) *                 (x)*(x))
#define RIGHT_BDRY_SPLINE(x)       ( (x) * (  3. + (x) * ( 3.- (x))))
#define RIGHT_BDRY_RIGHT_SPLINE(x) ( (x) * (  3. + (x) * (-3.+ (x))))
#define BDRY_INTEGRAL_LEFT_BDRY_SPLINE       (5.)
#define BDRY_INTEGRAL_LEFT_BDRY_LEFT_SPLINE  (1.)

  gdouble one_over_oo = 1./oo;
  gdouble h = o * one_over_oo;

 /*
  * jj * n / nn (in rational arithmetic)
  *
  * is the absolute coordinate of the left endpoint of the index jj
  * fine cell (recall that jj starts at zero).  Given that the left
  * endpoints j of the coarse cells are 0, 1, 2... this implies that
  *
  * (j * nn ) / n (in integer arithmetic)
  *
  * is the index of the first fine cell which is contained in the
  * index j coarse cell, so that
  *
  * ((j+1) * nn ) / n - 1 (in integer arithmetic)
  *
  * is the index of the last fine cell which overlaps the index j
  * coarse cell.
  *
  * There is another way of caracterizing this index: It is the
  * largest jj such that jj * n < j + 1. Using this caracterization
  * allows one to avoid integer divisions, and because we usually know
  * approximately what this index is, few iterations allow one to find
  * it.
  */

  gint k;
  gint kk;

  gint o_minus_1 = o-1;

  gdouble x = 0.;

  gdouble previous_integral_l;
  gdouble previous_integral_c = 0.;
  gdouble previous_integral_r = 0.;

  gdouble integral_l;
  gdouble integral_c;
  gdouble integral_r;

  gint last_kk = last_overlapping_kk[0];

  for (kk=0; kk<last_kk; kk++)
  {
    x += h;

    integral_c = LEFT_BDRY_SPLINE(x);
    center[kk] = integral_c - previous_integral_c;
    previous_integral_c = integral_c;

    /*
     * The integral which is contributed from the right coarse cell
     * (integral_r) is computed using the left piece of the
     * appropriate B-spline. This left/right duality is taken into
     * account throughout the code.
     */
    integral_r = LEFT_BDRY_LEFT_SPLINE(x);
    right[kk] = integral_r - previous_integral_r;
    previous_integral_r = integral_r;
  }

  /*
   * At this point, jj = last_jj, and the contributions to the last
   * fine cell which overlaps the first coarse cell are to be
   * computed.  Two cases may occur this last overlapping fine cell
   * may overlap the second coarse cell, or it may end right at the
   * first coarse cell's right boundary (located at the absolute
   * coordinate equal to 1.). It turns out that using the next piece
   * of antiderivative (after correcting for the different constants
   * of integration and local coordinate) works in both cases because,
   * in the no overlap case, the left and center pieces agree at their
   * common boundary once corrected for the different choices of
   * global antiderivatives.
   */

  /*
   * Alternate formula for x:
   *
   * x = ( kk+1 ) * h - 1.;
   */
  x = ( (kk+1) * o - oo ) * one_over_oo;

  /*
   * Because we are crossing to the next coarse cell, we use the right
   * piece of the B-splines to compute the "center" contribution. If it
   * happens that this fine cell ends right at the boundary of the first
   * coarse cell, RIGHT_BSPLINE(x) turns out to be 0, and consequently
   * the second coarse cell does not contribute to center[jj], as should
   * be.
   */
  previous_integral_l = RIGHT_BSPLINE(x);

  center[kk] =
    previous_integral_l
    + ( BDRY_INTEGRAL_LEFT_BDRY_SPLINE - previous_integral_c );


  previous_integral_c = CENTER_BSPLINE(x);
  right[kk] =
    previous_integral_c
    + ( BDRY_INTEGRAL_LEFT_BDRY_LEFT_SPLINE - previous_integral_r );


  previous_integral_r = LEFT_BSPLINE(x); /* There is an hidden "- 0.". */
  farright[0] = previous_integral_r;

  /*
   * The very last coarse cell (the nth one, which has index n-1) is
   * an exception, just like the very first one, hence the
   * j<n_minus_1.
   */
  k = 1;
  do {
    last_kk = last_overlapping_kk[k];

    for (kk++; kk<last_kk; kk++)
    {
      x += h;

      integral_l = RIGHT_BSPLINE(x);
      left[kk] = integral_l - previous_integral_l;
      previous_integral_l = integral_l;

      integral_c = CENTER_BSPLINE(x);
      center[kk] = integral_c - previous_integral_c;
      previous_integral_c = integral_c;

      integral_r = LEFT_BSPLINE(x);
      right[kk] = integral_r - previous_integral_r;
      previous_integral_r = integral_r;
    }

    /*
     * x = ( kk+1 ) * h - (k + 1);
     *
     * is the distance from the left endpoint of the current coarse
     * cell to the right endpoint of the current fine cell.
     *
     * When we use large images, this quantity, which uses theta(nn)
     * quantities to compute a theta(1) value, may suffer from round
     * off error. Although we have not seen evidence of this---and
     * consequently the above formula, most likely, could most likely
     * used safely, especially if the output images are not too
     * large---we use integer arithmetic to compute this quantity.
     *
     *One disadvantage is that we may suffer from integer overflow if
     *the images are large. This could be fixed using long integers.
     *
     * NICOLAS: ADD A COMMENT SOMEWHERE ABOUT INTEGER OVERFLOW IF n *
     * nn or m * mm is larger than maxint.
     */

    x = ( (kk+1)*o - (k+1)*oo ) * one_over_oo;

    left[kk] = 1. - previous_integral_l;

    previous_integral_l = RIGHT_BSPLINE(x);
    center[kk] = previous_integral_l + ( 4. - previous_integral_c );

    previous_integral_c = CENTER_BSPLINE(x);
    right[kk] = previous_integral_c + ( 1. - previous_integral_r );

    previous_integral_r = LEFT_BSPLINE(x);
    farright[k] = previous_integral_r;
  } while (++k<o_minus_1);

  /*
   * Now, we deal with the very last coarse cell.
   */

  /*
   * First, we need to correct the computed value which is wrong
   * because we used the centered B-spline instead of the right
   * boundary one. This correction needs to be done only if the
   * last_jj cell overlaps the very last one, since otherwise the last
   * cell did not comtribute anything anyway, and consequently no
   * correction is needed. This is what the "if n divide nn" condition
   * checks.
   *
   * Note that farright[j] = farright[last_j] = farright[n-1] may also
   * be incorrect, but this value is never used, so there is no need
   * to correct it.
   */

  if ( oo%o )
  {
    right[kk] -= previous_integral_c;
    previous_integral_c = RIGHT_BDRY_SPLINE(x);
    right[kk] += previous_integral_c;
  }

  kk++;

  /*
   * Now we can proceed with the fine cells which have not received
   * values (those which are fully contained in the last coarse cell).
   */
  while (kk<oo)
    {
      x += h;

      integral_l = RIGHT_BDRY_RIGHT_SPLINE(x);
      left[kk] = integral_l - previous_integral_l;
      previous_integral_l = integral_l;

      integral_c = RIGHT_BDRY_SPLINE(x);
      center[kk] = integral_c - previous_integral_c;
      previous_integral_c = integral_c;

      kk++;
    }
}

static gboolean
scale_dialog (gint32 image_ID,
              gint   m,
              gint   n)
{
  GtkWidget *dialog;
  GtkWidget *main_vbox;
  GtkWidget *main_hbox;
  GtkWidget *frame;
  GtkWidget *frame_label;
  GtkWidget *alignment;
  GtkWidget *size_entry;
  GtkWidget *aspect_chain;
  GtkWidget *smooth_mode;
  GtkWidget *sharp_mode;
  GtkWidget *mode_frame;
  GtkWidget *mode_align;
  GtkWidget *mode_box;
  GtkWidget *mode_label;
  GimpUnit   unit;
  gdouble    xres;
  gdouble    yres;
  gint       run;
  gboolean   not_done;

  gimp_ui_init ("Upsize", FALSE);

  dialog = gimp_dialog_new ("Upsize", "Upsize",
                            NULL, 0,
                            gimp_standard_help_func, "Upsize",
                            GIMP_STOCK_RESET, RESPONSE_RESET,
                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            GTK_STOCK_OK,     GTK_RESPONSE_OK,
                            NULL);

  main_vbox = gtk_vbox_new (FALSE, 12);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), main_vbox);
  gtk_widget_show (main_vbox);

  frame = gtk_frame_new (NULL);
  gtk_widget_show (frame);
  gtk_box_pack_start (GTK_BOX (main_vbox), frame, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (frame), 6);

  alignment = gtk_alignment_new (0.5f, 0.5f, 0.0f, 0.0f);
  gtk_widget_show (alignment);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 6, 6, 6, 6);

  main_hbox = gtk_hbox_new (FALSE, 12);
  gtk_widget_show (main_hbox);
  gtk_container_add (GTK_CONTAINER (alignment), main_hbox);

  /*  Get the image resolution and unit  */
  gimp_image_get_resolution (image_ID, &xres, &yres);
  unit = gimp_image_get_unit (image_ID);

  size_entry = gimp_size_entry_new(2, unit, "%a", TRUE, TRUE, FALSE,
                                   8,
                                   GIMP_SIZE_ENTRY_UPDATE_SIZE);

  /*  set the unit back to pixels, since most times we will want pixels */
  gimp_size_entry_set_unit (GIMP_SIZE_ENTRY (size_entry), GIMP_UNIT_PIXEL);

  /*  set the resolution to the image resolution  */
  gimp_size_entry_set_resolution (GIMP_SIZE_ENTRY (size_entry), 0, xres, TRUE);
  gimp_size_entry_set_resolution (GIMP_SIZE_ENTRY (size_entry), 1, yres, TRUE);

  /*  set the size (in pixels) that will be treated as 0% and 100%  */
  gimp_size_entry_set_size (GIMP_SIZE_ENTRY (size_entry), 0, 0.0, n);
  gimp_size_entry_set_size (GIMP_SIZE_ENTRY (size_entry), 1, 0.0, m);

  /*  set upper and lower limits (in pixels)  */
  gimp_size_entry_set_refval_boundaries (GIMP_SIZE_ENTRY (size_entry),
                                         0, n, MAX_WIDTH);
  gimp_size_entry_set_refval_boundaries (GIMP_SIZE_ENTRY (size_entry),
                                         1, m, MAX_HEIGHT);

  gtk_table_set_row_spacing (GTK_TABLE (size_entry), 0, 1);
  gtk_table_set_col_spacings (GTK_TABLE (size_entry), 6);
  gtk_table_set_col_spacing (GTK_TABLE (size_entry), 2, 12);

  /*  initialize the values  */
  gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (size_entry), 0, n);
  gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (size_entry), 1, m);

  /*  attach labels  */
  gimp_size_entry_attach_label (GIMP_SIZE_ENTRY (size_entry), "Width",
                                0, 1, 0.0f);
  gimp_size_entry_attach_label (GIMP_SIZE_ENTRY (size_entry), "Height",
                                0, 2, 0.0f);

  gtk_widget_show (size_entry);
  gtk_box_pack_start (GTK_BOX (main_hbox), size_entry, FALSE, FALSE, 0);

  /*  put a chain_button beside the size_entries  */
  aspect_chain = gimp_chain_button_new (GIMP_CHAIN_BOTTOM);
  gimp_chain_button_set_active (GIMP_CHAIN_BUTTON (aspect_chain), TRUE);
  gtk_table_attach_defaults (GTK_TABLE (size_entry), aspect_chain, 1, 3, 2, 3);
  gtk_widget_show (aspect_chain);

  frame_label = gtk_label_new ("<b>Image Size</b>");
  gtk_widget_show (frame_label);
  gtk_frame_set_label_widget (GTK_FRAME (frame), frame_label);
  gtk_label_set_use_markup (GTK_LABEL (frame_label), TRUE);

  g_signal_connect (size_entry, "value-changed",
                    G_CALLBACK (scale_callback),
                    aspect_chain);

  /* Mode selector (sharp/smooth) radio buttons */
  mode_frame = gtk_frame_new (NULL);
  gtk_widget_show (mode_frame);
  gtk_box_pack_start (GTK_BOX (main_vbox), mode_frame, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (mode_frame), 6);

  mode_label  = gtk_label_new ("<b>Resampling Mode</b>");
  gtk_widget_show (mode_label);
  gtk_frame_set_label_widget (GTK_FRAME (mode_frame), mode_label);
  gtk_label_set_use_markup (GTK_LABEL (mode_label), TRUE);

  mode_align = gtk_alignment_new (0.5f, 0.5f, 0.0f, 0.0f);
  gtk_widget_show (mode_align);
  gtk_container_add (GTK_CONTAINER (mode_frame), mode_align);
  gtk_alignment_set_padding (GTK_ALIGNMENT (mode_align), 6, 6, 6, 6);

  mode_box = gtk_hbox_new (FALSE, 12);
  gtk_widget_show (mode_box);
  gtk_container_add (GTK_CONTAINER (mode_align), mode_box);

  smooth_mode = gtk_radio_button_new_with_mnemonic (NULL, "S_mooth");
  g_signal_connect (G_OBJECT (smooth_mode), "toggled", 
                    G_CALLBACK (set_mode),
                    &svals);
  sharp_mode  = gtk_radio_button_new_with_mnemonic_from_widget (
                  GTK_RADIO_BUTTON (smooth_mode), "S_harp");
  //g_signal_connect (G_OBJECT (sharp_mode), "toggled",
  //                  G_CALLBACK (set_mode),
  //                  &svals);
  gtk_box_pack_start (GTK_BOX (mode_box), smooth_mode, TRUE, TRUE, 2);
  gtk_box_pack_start (GTK_BOX (mode_box), sharp_mode,  TRUE, TRUE, 2);
  gtk_widget_show (smooth_mode);
  gtk_widget_show (sharp_mode);

  gtk_widget_show (dialog);
  not_done = TRUE;

  do
  {
    run = gimp_dialog_run (GIMP_DIALOG (dialog));
    switch (run)
    {
      case RESPONSE_RESET:
      gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (size_entry), 0, n);
      gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (size_entry), 1, m);
      gimp_chain_button_set_active (GIMP_CHAIN_BUTTON (aspect_chain), TRUE);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (smooth_mode), TRUE);
      break;

      default:
      /* grab width and height from size entry boxes */
      svals.width  = RINT (gimp_size_entry_get_refval
                           (GIMP_SIZE_ENTRY (size_entry), 0));
      svals.height = RINT (gimp_size_entry_get_refval
                           (GIMP_SIZE_ENTRY (size_entry), 1));
      not_done = FALSE;
      break;
    }
  } while (not_done);

  gtk_widget_destroy (dialog);

  return run == GTK_RESPONSE_OK;
}

static void
scale_callback (GtkWidget *widget,
                gpointer   data)
{
  gdouble new_width;
  gdouble new_height;

  new_width  = gimp_size_entry_get_refval (GIMP_SIZE_ENTRY (widget), 0);
  new_height = gimp_size_entry_get_refval (GIMP_SIZE_ENTRY (widget), 1);

  if (gimp_chain_button_get_active (GIMP_CHAIN_BUTTON (data)))
  {
    if (new_height != _upsize_height)
    {
      _upsize_height = new_height;
      _upsize_width = (int) (new_height / _upsize_m_over_n + 0.5);
      gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (widget), 0, _upsize_width);
    }
    else
    {
      _upsize_width = new_width;
      _upsize_height = (int) (new_width * _upsize_m_over_n + 0.5);
      gimp_size_entry_set_refval (GIMP_SIZE_ENTRY (widget), 1, _upsize_height);
    }
  }
  else
  {
    _upsize_width  = new_width;
    _upsize_height = new_height;
  }
}

static void
set_mode (GtkRadioButton *button,
          ScaleVals      *svals )
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
    svals->mode = 0;
  else
    svals->mode = 1;
}
