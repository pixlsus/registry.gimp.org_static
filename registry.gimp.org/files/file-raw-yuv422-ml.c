/* Raw YUV-422 image loader and saver plugin 1.0 for MagicLantern silent snapshot images.
 *
 * by Wolfgang Hofer [hof@gimp.org]
 *
 * Inital version tested with Gimp 2.6.11
 *
 * This plugin is not based on any other plugin.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* INSTALL: gimptool-2.0 --install file-raw-yuv422-ml.c */

//#include "config.h"

#include <errno.h>
#include <string.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <math.h>
#include <glib.h>
#include <glib/gstdio.h>



#ifdef G_OS_WIN32
#include <io.h>
#endif

#include "libgimp/gimp.h"
#include "libgimp/gimpui.h"



//#include "libgimp/stdplugins-intl.h"

/* for compile without nls translation use dummys nls-macros
 * (for standalone compile without having config.h)
 */

#ifndef N_
#define N_(x)  x
#define _(x)   x
#define INIT_I18N()
#endif





#define LOAD_PROC      "file-raw-yuv-422-load"
#define SAVE_PROC      "file-raw-yuv-422-save"
#define PLUG_IN_BINARY "file-raw-yuv-422"
#define PREVIEW_WIDTH   350
#define PREVIEW_HEIGHT  233


/* average 2 byte per pixel for yuv422 rawdata images
 * note that each pixel has 1 byte for the y value
 * but the 2nd byte is either u or v value alternating each pixel
 * (e.g. u and v have only half horizontal resolution)
 */
#define AVG_BPP_YUV422  2
#define MAX_SUB_IMAGES_X  5
#define MAX_SUB_IMAGES_Y  5

#define LOCAL_RESPONSE_RESET 1

#define PACKET_STATUS_EMPTY            0
#define PACKET_STATUS_CONVERTED        1
#define PACKET_STATUS_TRANSFER_TO_GIMP 2

#define MAX_PKT_HEIGHT 64         /* gimp_tile_height */
#define MAX_SUPPORTED_THREADS 32

typedef enum
{
  RAW_UYVY,                    /* YUV-422 Image  2 byte per pixel order is U,Y,V,Y  where u and v is same value for 2 pixels */
  RAW_YUYV                     /* YUV-422 Image  2 byte per pixel order is Y,U,Y,V  where u and v is same value for 2 pixels */
} RawType;


typedef struct
{
  gboolean       isInitialized;      /* TRUE in case values from last run available */
  gint32         numProcessors;      /* number */
  gint32         sub_image_width;    /* width of the raw subimage                   */
  gint32         sub_image_height;   /* height of the raw subimage                  */
  gint32         sub_images_x;       /* number of horizontal subimages              */
  gint32         sub_images_y;       /* number of vertical subimages                */
  RawType        image_type;         /* type of image (YUYV, UYVY)                  */
} RawConfig;

typedef struct
{
  RawConfig     *rawConfigPtr;
  gint32         file_size;
  gint           offsetY;
  gint           offsetU;
  gint           offsetV;
  
  gboolean       do_progress;         /* show progress while load/save procedure     */
  gboolean       enable_preview;
  gint32         upd_timertag;
  FILE          *fp_preview;
} RawProcessing;   /* rpPtr */


typedef struct
{
  FILE         *fp;        /* pointer to the already open file */
  GimpDrawable *drawable;  /* gimp drawable                    */
  GimpPixelRgn  region;    /* gimp pixel region                */
  gint32        image_id;  /* gimp image id                    */
} RawGimpData;

typedef struct
{
  void          *next;
  gint           packet_status; 
  
  gint32         packet_x;             /* packet start x coordinate (in the final gimp image) */
  gint32         packet_y;             /* packet start x coordinate (in the final gimp image) */
  gint32         packet_height;        /* packet height in pixels (upto 64) */
  gint32         packet_width;         /* packet width in pixels ALWAYS == sub_image_width */
  gint32         packet_seek_pos;
  gint32         packet_rgb_buffer_size;
  guchar        *packet_rgb_buffer;
  
} RawPacket;



typedef struct
{
  gint       tid;          /* thread index (for debug logging purpose) */
  gint       bpp;          /* byte per rgb pixel (3 or 4) */
  RawPacket *packetList;   /* root of the packet list to process */
  guchar    *packet_yuv_buffer;  /* large enough to hold the full packet in YUV 422 representation 
                                  * (AVG_BPP_YUV422 * sub_image_width * MAX_PKT_HEIGHT) 
                                  */  
  const char     *filename;   /* the raw YUV .422 file to load */
  GMutex         *elemMutex;
  RawProcessing  *rpPtr;
  
} RawPacketLoaderThreadUserData;


typedef struct
{
  RawPacketLoaderThreadUserData  usrDataTab[MAX_SUPPORTED_THREADS];
  gint                           numberOfWorkerThreads;
  gint                           numberOfScheduledPackets;
  
} PacketScheduleData;


static void              query             (void);
static void              run               (const gchar      *name,
                                            gint              nparams,
                                            const GimpParam  *param,
                                            gint             *nreturn_vals,
                                            GimpParam       **return_vals);


/* prototypes for support functions */

static gint              get_gimprc_num_processors();
static void              set_yuv_offsets(RawProcessing *rpPtr, RawConfig  *rawConfigPtr);

static void              convert_row_from_rgb_to_yuv(RawProcessing  *rpPtr, guchar *row,
                                            guchar *rowRawYuv,
                                            gint32 rowWidth,
                                            gint bpp);

static void              convert_row_from_yuv_2_rgb(RawProcessing  *rpPtr, guchar *row,
                                            guchar *rowRawYuv,
                                            gint32 rowWidth,
                                            gint   bpp);
static gboolean          raw_load_yuv422   (RawProcessing    *rpPtr,
                                            RawGimpData      *data,
                                            gint              bpp);

static gint32            get_file_size     (const gchar      *filename);
static void              guess_yuv422_width_and_height(gint32 *subImagesX,
                                            gint32 *subImagesY,
                                            gint32 *guessWidth, 
                                            gint32 *guessHeight, 
                                            gint32  fileSize);
static void              set_tile_cache   (guint widthInPixels,
                                           guint heightInPixels);
static gint              read_and_convert_line (RawProcessing  *rpPtr, RawConfig  *rawConfigPtr,
                                           guchar   *previewRowRgb,
                                           guchar   *bufRgb,
                                           guchar   *localBufYuv,
                                           gint32    preview_width,
                                           gint32    y,
                                           gdouble   scaleStepsizeX,
                                           gint      bpp,
                                           gint32    file_size);

/* prototypes for the mulithreaded load functions */
static gpointer          raw_packet_loader_thread(gpointer data);
static void              schedule_packet_lists(const char *filename, RawConfig *rawConfigPtr
                                              , PacketScheduleData *psdPtr, gint bpp, RawProcessing *rpPtr);
static void              free_packet_schedule_resources(PacketScheduleData *psdPtr);
static gint32            load_image_multithread_by_packet(RawProcessing  *rpPtr
                                              , const char *filename, RawGimpData *data);





/* prototypes for the load and save  functions */
static gint32            load_image        (RawProcessing  *rpPtr, const gchar      *filename,
                                            GError          **error);
static GimpPDBStatusType save_image        (RawProcessing  *rpPtr, const gchar      *filename,
                                            gint32            image_id,
                                            gint32            drawable_id,
                                            GError          **error);

/* gui functions */
static void              trigger_preview_update    (GimpPreviewArea   *preview);
static void              preview_update_timer_job  (GimpPreviewArea   *preview);

static gboolean          load_dialog       (RawProcessing  *rpPtr, const gchar       *filename);
static gboolean          save_dialog       (RawConfig  *rawConfigPtr, const gchar       *filename,
                                            gint32             image_id,
                                            gint32             drawable_id);


const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,   /* init_proc  */
  NULL,   /* quit_proc  */
  query,  /* query_proc */
  run,    /* run_proc   */
};

MAIN()

#define NUMBER_OF_SAVE_ARGS 8

static void
query (void)
{
  static const GimpParamDef load_args[] =
  {
    { GIMP_PDB_INT32,  "run-mode",     "Interactive, non-interactive" },
    { GIMP_PDB_STRING, "filename",     "The name of the file to load" },
    { GIMP_PDB_STRING, "raw-filename", "The name entered"             }
  };

  static const GimpParamDef load_return_vals[] =
  {
    { GIMP_PDB_IMAGE, "image", "Output image" }
  };

  static const GimpParamDef save_args[] =
  {
    { GIMP_PDB_INT32,    "run-mode",     "Interactive, non-interactive" },
    { GIMP_PDB_IMAGE,    "image",        "Input image"                  },
    { GIMP_PDB_DRAWABLE, "drawable",     "Drawable to save"             },
    { GIMP_PDB_STRING,   "filename",     "The name of the file to save the image in" },
    { GIMP_PDB_STRING,   "raw-filename", "The name entered"             },
    { GIMP_PDB_INT32,    "raw-type",     "0: UYVY 1:YUYV byte order." },
    { GIMP_PDB_INT32,    "subimages-x",  "number of horizontal subimage tiles 1 .. 5." },
    { GIMP_PDB_INT32,    "subimages-y",  "number of vertical subimage tiles 1 .. 5." }
  };

  gimp_install_procedure (LOAD_PROC,
                          "Load raw YUV422 (YCbCr) images, specifying image information",
                          "Load raw YCbCr .422 image files that were shot in SILENT mode with Canon EOS Cameras "
                          "that are running the free MagicLantern software add on (using raw_type 0:UYVY). "
                          "load supports both single images and MagicLantern HiRes images, that are "
                          "tiled images built up as a matrix of NxM subimages in one file. "
                          "This file load plug-in uses a singlethread and a multithreaded implementation "
                          "that runs faster on systems with more than one Processor. "
                          "(but uses more memory resources than the singlethreded implementation) "
                          "Note that there are other YUV colormodel variants that are not supported here. "
                          "Read more about YUV variants at http://www.fourcc.org/fccyvrgb.php "
                          "Note that there is another plug-in for the GIMP that supports a variety of raw file types "
                          "at https://github.com/marcelteun/GIMP-raw-file-load (but this does NOT work properly "
                          "on .422 files written by MagicLantern).",
                          "Wolfgang Hofer, hof@gimp.org",
                          "Wolfgang Hofer, hof@gimp.org",
                          "Oct 2012",
                          N_("Raw YUV422 image data"),
                          NULL,
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (load_args),
                          G_N_ELEMENTS (load_return_vals),
                          load_args, load_return_vals);

  gimp_register_load_handler(LOAD_PROC, "UYVY, YUYV, YUV-422, 422", "");

  gimp_install_procedure (SAVE_PROC,
                          "Dump images to disk in raw YUV422(YCbCr) format",
                          "Dump images to disk in raw YCbCr format "
                          "optionally as single image or tiled as matrix of subimages "
                          "(same style as MagicLantern HiRes silent mode pictures)",
                          "Wolfgang Hofer, hof@gimp.org",
                          "Wolfgang Hofer, hof@gimp.org",
                          "Oct 2012",
                          N_("Raw image data"),
                          "GRAY, RGB, RGBA",
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (save_args), 0,
                          save_args, NULL);

  gimp_register_save_handler (SAVE_PROC, "UYVY, YUYV, YUV-422, 422", "");
}

static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
  static GimpParam   values[2];
  GimpRunMode        run_mode;
  GimpPDBStatusType  status = GIMP_PDB_SUCCESS;
  GError            *error  = NULL;
  gint32             image_id;
  gint32             drawable_id;
  gint32 guessWidth;
  gint32 guessHeight;
  gint32 subImagesX;
  gint32 subImagesY;
  RawConfig     *rawConfigPtr;
  RawProcessing *rpPtr;

  INIT_I18N ();

  run_mode = param[0].data.d_int32;

  *nreturn_vals = 1;
  *return_vals  = values;

  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;

  /* allocate config structure and fill with defaults */
  rawConfigPtr = g_new0 (RawConfig, 1);

  rawConfigPtr->isInitialized  = FALSE;
  rawConfigPtr->numProcessors  = get_gimprc_num_processors();
  rawConfigPtr->sub_image_width    = 0;
  rawConfigPtr->sub_image_height   = 0;
  rawConfigPtr->sub_images_x   = 1;
  rawConfigPtr->sub_images_y   = 1;
  rawConfigPtr->image_type     = RAW_UYVY;

  rpPtr = g_new0 (RawProcessing, 1);
  rpPtr->rawConfigPtr = rawConfigPtr;
  rpPtr->fp_preview = NULL;

  set_yuv_offsets(rpPtr, rawConfigPtr);


  if (strcmp (name, LOAD_PROC) == 0)
    {
      gimp_get_data (LOAD_PROC, rawConfigPtr);
      rpPtr->do_progress = (run_mode == GIMP_RUN_INTERACTIVE);

      rpPtr->file_size = get_file_size(param[1].data.d_string /* const char *filename */);
      guess_yuv422_width_and_height(&subImagesX, &subImagesY
                                   , &guessWidth, &guessHeight
                                   , rpPtr->file_size 
                                   );
      if ((guessWidth != 0) && (guessHeight != 0))
        {
          /* use actual values on valid guess, else keep last used values */
          rawConfigPtr->sub_image_width = guessWidth;
          rawConfigPtr->sub_image_height = guessHeight;
          rawConfigPtr->sub_images_x = subImagesX;
          rawConfigPtr->sub_images_y = subImagesY;
          
        }

      if (rawConfigPtr->sub_image_width > 0)
        {
          set_tile_cache(rawConfigPtr->sub_image_width * rawConfigPtr->sub_images_x, rawConfigPtr->sub_image_height);
        }
      else
        {
          set_tile_cache(3000, 2);
        }


      if (run_mode == GIMP_RUN_INTERACTIVE)
        {
          if ((rawConfigPtr->sub_image_width == 0) || (rawConfigPtr->sub_image_height == 0))
            {
              rawConfigPtr->sub_image_width    = PREVIEW_WIDTH;
              rawConfigPtr->sub_image_height   = PREVIEW_HEIGHT;
              rawConfigPtr->sub_images_x = 1;
              rawConfigPtr->sub_images_y = 1;
            }


          rpPtr->fp_preview = g_fopen(param[1].data.d_string, "rb");
          if (rpPtr->fp_preview == NULL)
            {
              g_set_error (&error,
                           G_FILE_ERROR, g_file_error_from_errno (errno),
                           _("Could not open '%s' for reading: %s"),
                           gimp_filename_to_utf8 (param[1].data.d_string),
                           g_strerror (errno));

              status = GIMP_PDB_EXECUTION_ERROR;
            }
          else
            {
              if (! load_dialog (rpPtr, param[1].data.d_string))
                status = GIMP_PDB_CANCEL;

              fclose (rpPtr->fp_preview);
              rpPtr->fp_preview = NULL;
            }
        }
      else if ((run_mode == GIMP_RUN_WITH_LAST_VALS)
           ||  (run_mode == GIMP_RUN_NONINTERACTIVE))
        {
          if ((rawConfigPtr->sub_image_width == 0) || (rawConfigPtr->sub_image_height == 0))
            {
              /* cant run because no valid size information available */
              status = GIMP_PDB_EXECUTION_ERROR;
            }
        }
      else
        {
          /*  unknown mode is not supported. */
          status = GIMP_PDB_CALLING_ERROR;
        }


      if (status == GIMP_PDB_SUCCESS)
        {
          image_id = load_image (rpPtr, param[1].data.d_string, &error);

          if (image_id != -1)
            {
              if (run_mode == GIMP_RUN_INTERACTIVE)
                {
                  rawConfigPtr->isInitialized = TRUE;
                  gimp_set_data (LOAD_PROC, rawConfigPtr, sizeof (RawConfig));
                }

              *nreturn_vals = 2;
              values[1].type         = GIMP_PDB_IMAGE;
              values[1].data.d_image = image_id;
            }
          else
            {
              status = GIMP_PDB_EXECUTION_ERROR;
            }
        }
    }
  else if (strcmp (name, SAVE_PROC) == 0)
    {
      gimp_get_data (SAVE_PROC, rawConfigPtr);
      rpPtr->do_progress = (run_mode == GIMP_RUN_INTERACTIVE);

      image_id    = param[1].data.d_int32;
      drawable_id = param[2].data.d_int32;

      /* calculate expected filesize for raw yuv422 file */
      rpPtr->file_size = gimp_drawable_width(drawable_id) * gimp_drawable_height(drawable_id) * AVG_BPP_YUV422;
      guess_yuv422_width_and_height( &subImagesX, &subImagesY
                                   , &guessWidth, &guessHeight
                                   , rpPtr->file_size
                                   );
      if ((guessWidth * subImagesX == gimp_drawable_width(drawable_id)) 
      &&  (guessHeight * subImagesY == gimp_drawable_height(drawable_id)))
        {
          /* use actual values on valid guess, else keep last used values */
          rawConfigPtr->sub_image_width = guessWidth;
          rawConfigPtr->sub_image_height = guessHeight;
          rawConfigPtr->sub_images_x = subImagesX;
          rawConfigPtr->sub_images_y = subImagesY;
          
        }

      if ((run_mode == GIMP_RUN_WITH_LAST_VALS)
      &&  (rawConfigPtr->isInitialized != TRUE))
        {
          /* no values from previos run of the SAVE_PROC
           * in the same session found
           * in this case behave same as interactive mode.
           */
          run_mode = GIMP_RUN_INTERACTIVE;
        }

      if (run_mode == GIMP_RUN_INTERACTIVE)
        {

          if (nparams != NUMBER_OF_SAVE_ARGS)
            {
              status = GIMP_PDB_CALLING_ERROR;
            }
          else if (! save_dialog (rawConfigPtr, param[3].data.d_string,
                                  image_id, drawable_id))
            {
              status = GIMP_PDB_CANCEL;
            }
          else
            {
              rawConfigPtr->isInitialized = TRUE;
              gimp_set_data (SAVE_PROC, rawConfigPtr, sizeof (RawConfig));
            }
        }
      else if (run_mode == GIMP_RUN_WITH_LAST_VALS)
        {
          if (nparams != NUMBER_OF_SAVE_ARGS)
            {
              status = GIMP_PDB_CALLING_ERROR;
            }
        }
      else if (run_mode == GIMP_RUN_NONINTERACTIVE)
        {
          if (nparams != NUMBER_OF_SAVE_ARGS)
            {
              status = GIMP_PDB_CALLING_ERROR;
            }
          else
            {
              rawConfigPtr->image_type = (param[5].data.d_int32 == 1) ? RAW_YUYV : RAW_UYVY;
              rawConfigPtr->sub_images_x = CLAMP(param[6].data.d_int32, 1, 5);
              rawConfigPtr->sub_images_y = CLAMP(param[7].data.d_int32, 1, 5);

            }
        }
      else
        {
          /* unknown run_mode is not supported
           *
           */
          status = GIMP_PDB_CALLING_ERROR;
        }

      if (status == GIMP_PDB_SUCCESS)
        {
          status = save_image (rpPtr, param[3].data.d_string, image_id, drawable_id,
                               &error);
        }

    }

  g_free (rawConfigPtr);

  if (status != GIMP_PDB_SUCCESS && error)
    {
      *nreturn_vals = 2;
      values[1].type          = GIMP_PDB_STRING;
      values[1].data.d_string = error->message;
    }

  values[0].data.d_status = status;
}



/* ----------------------------------
 * babl_convert_rgb_2_yuv
 * ----------------------------------
 * converts one pixel from RGB colormodel to Ycbcr colormodel.
 * variant uses same calculation formula as used in the babl library:
 *
 *     red   = linear_to_gamma_2_2 (red);
 *     green = linear_to_gamma_2_2 (green);
 *     blue  = linear_to_gamma_2_2 (blue);
 *
 *     luminance = 0.299 * red + 0.587 * green + 0.114 * blue;
 *     cb        = -0.168736 * red - 0.331264 * green + 0.5 * blue;
 *     cr        = 0.5 * red - 0.418688 * green - 0.081312 * blue;
 *
 * note that babl uses double values in range [0.0, 1.0] for r,g,b and luminance
 */
// static void
// babl_convert_rgb_2_yuv(guchar *rgbPtr, guchar *yPtr, gchar *uPtr, gchar *vPtr)
// {
//   gdouble  red;
//   gdouble  green;
//   gdouble  blue;
//   gdouble  luminance, cb, cr;
// 
//   red   = rgbPtr[0];
//   green = rgbPtr[1];
//   blue  = rgbPtr[2];
//  
//   luminance = 0.299 * red + 0.587 * green + 0.114 * blue;
//   cb        = -0.168736 * red - 0.331264 * green + 0.5 * blue;
//   cr        = 0.5 * red - 0.418688 * green - 0.081312 * blue;
// 
// 
//   *yPtr = CLAMP(rint(luminance), 0, 255);
//   *uPtr = CLAMP(rint(cb), -128, 127);
//   *vPtr = CLAMP(rint(cr), -128, 127);
// 
// 
// }  /* end babl_convert_rgb_2_yuv */


/* ----------------------------------
 * gint32babl_convert_rgb_2_yuv
 * ----------------------------------
 * converts one pixel from RGB colormodel to Ycbcr colormodel.
 * variant uses same calculation formula as used in the babl library:
 *
 *     red   = linear_to_gamma_2_2 (red);
 *     green = linear_to_gamma_2_2 (green);
 *     blue  = linear_to_gamma_2_2 (blue);
 *
 *     luminance = 0.299 * red + 0.587 * green + 0.114 * blue;
 *     cb        = -0.168736 * red - 0.331264 * green + 0.5 * blue;
 *     cr        = 0.5 * red - 0.418688 * green - 0.081312 * blue;
 *
 * note that babl uses double values in range [0.0, 1.0] for r,g,b and luminance
 * TODO integer implementation ....
 */
static inline void
convert_rgb_2_yuv(guchar *rgbPtr, guchar *yPtr, gchar *uPtr, gchar *vPtr)
{
  gint32  red;
  gint32  green;
  gint32  blue;
  gint32  luminance, cb, cr;

  red   = rgbPtr[0];
  green = rgbPtr[1];
  blue  = rgbPtr[2];
 
  luminance =  299000 * red + 587000 * green + 114000 * blue;
  cb        = -168736 * red - 331264 * green + 500000 * blue;
  cr        =  500000 * red - 418688 * green - 81312 * blue;


  *yPtr =   CLAMP(((luminance + 500000) / 1000000), 0, 255);
  if (cb >= 0)
    *uPtr = CLAMP(((cb + 500000) / 1000000), -128, 127);
  else
    *uPtr = CLAMP(((cb - 500000) / 1000000), -128, 127);
  if (cr >= 0)
    *vPtr = CLAMP(((cr + 500000) / 1000000), -128, 127);
  else
    *vPtr = CLAMP(((cr - 500000) / 1000000), -128, 127);


}  /* end gint32babl_convert_rgb_2_yuv */



/* ----------------------------------
 * babl_convert_yuv_2_rgb
 * ----------------------------------
 * converts one pixel from Ycbcr colormdel to RGB colormodel.
 * variant uses same calculation formula as used in the babl library:
 *
 *     red   = 1.0 * luminance + 0.0 * cb + 1.40200 * cr;
 *     green = 1.0 * luminance - 0.344136 * cb - 0.71414136 * cr;
 *     blue  = 1.0 * luminance + 1.772 * cb + 0.0 * cr;
 *
 *     red   = gamma_2_2_to_linear (red);
 *     green = gamma_2_2_to_linear (green);
 *     blue  = gamma_2_2_to_linear (blue);
 *
 * applying the gamma_2_2_to_linear made my test images too dark
 * and leads to more loss of information when the pixel is converted
 * to 8 bit rgb as it is require here. Therfore the gamma_2_2_to_linear is skipped in my
 * conversion variant.
 * (note that babl stores rgb as double datatype per channel)
 */
// static void
// babl_convert_yuv_2_rgb(guchar *yPtr, gchar *uPtr, gchar *vPtr, guchar *rgbPtr)
// {
//   gdouble  red;
//   gdouble  green;
//   gdouble  blue;
//   gdouble  luminance, cb, cr;
// 
//   luminance = *yPtr;
//   cb = *uPtr;
//   cr = *vPtr;
// 
//   /* note that Y is unsigned but cb and cr are all signed values */
//   red   = 1.0 * luminance + 1.40200 * cr;
//   green = 1.0 * luminance - 0.344136 * cb - 0.71414136 * cr;
//   blue  = 1.0 * luminance + 1.772 * cb;
// 
//   rgbPtr[0] = CLAMP(rint(red),   0, 255);
//   rgbPtr[1] = CLAMP(rint(green), 0, 255);
//   rgbPtr[2] = CLAMP(rint(blue),  0, 255);
// 
// 
//}  /* end babl_convert_yuv_2_rgb */


/* ----------------------------------
 * gint32babl_convert_yuv_2_rgb
 * ----------------------------------
 * converts one pixel from Ycbcr colormdel to RGB colormodel.
 * variant uses same calculation formula as used in the babl library:
 *
 *     red   = 1.0 * luminance + 0.0 * cb + 1.40200 * cr;
 *     green = 1.0 * luminance - 0.344136 * cb - 0.71414136 * cr;
 *     blue  = 1.0 * luminance + 1.772 * cb + 0.0 * cr;
 *
 *     red   = gamma_2_2_to_linear (red);
 *     green = gamma_2_2_to_linear (green);
 *     blue  = gamma_2_2_to_linear (blue);
 *
 * applying the gamma_2_2_to_linear made my test images too dark
 * and leads to more loss of information when the pixel is converted
 * to 8 bit rgb as it is require here. Therfore the gamma_2_2_to_linear is skipped in my
 * conversion variant.
 * Fuerther calculations are done as integer multiplied by 1000000
 * that gaves the same quality e.g precision of the results in my tests.
 * Note that integers made processing remarkable faster.
 */
static inline void
convert_yuv_2_rgb(guchar *yPtr, gchar *uPtr, gchar *vPtr, guchar *rgbPtr)
{
  gint32  red;
  gint32  green;
  gint32  blue;
  gint32  luminance, cb, cr;

  luminance = *yPtr;
  cb = *uPtr;
  cr = *vPtr;

  /* note that Y is unsigned but cb and cr are all signed values */
  red   = 1000000 * luminance + 1402000 * cr;
  green = 1000000 * luminance -  344136 * cb - 714141 * cr;
  blue  = 1000000 * luminance + 1772000 * cb;

  rgbPtr[0] = CLAMP(((500000 + red)   / 1000000),  0, 255);
  rgbPtr[1] = CLAMP(((500000 + green) / 1000000),  0, 255);
  rgbPtr[2] = CLAMP(((500000 + blue)  / 1000000),  0, 255);


}  /* end gint32babl_convert_yuv_2_rgb */


/* ----------------------------------
 * set_yuv_offsets
 * ----------------------------------
 * set offsets according to image_type.
 * Defines order how to read/write the Y,U,V components.
 * Note that luminance Y is present for each pixel,
 * but U,V colorcomponents are valid for 2 pixels 
 * (e.g. have half horizontal resolution)
 */
static gint              get_gimprc_num_processors()
{
  gchar *value_string;
  gint   num_processors;

  num_processors = 1;
  
  value_string = gimp_gimprc_query("num-processors");
  if(value_string != NULL)
  {
     num_processors = CLAMP(atol(value_string), 1, MAX_SUPPORTED_THREADS);
     g_free(value_string);
  }
  return(num_processors);
  
}

/* ----------------------------------
 * set_yuv_offsets
 * ----------------------------------
 * set offsets according to image_type.
 * Defines order how to read/write the Y,U,V components.
 * Note that luminance Y is present for each pixel,
 * but U,V colorcomponents are valid for 2 pixels 
 * (e.g. have half horizontal resolution)
 */
static void
set_yuv_offsets(RawProcessing *rpPtr, RawConfig  *rawConfigPtr)
{
  if (rawConfigPtr == NULL)
    {
      rawConfigPtr = rpPtr->rawConfigPtr;
      rpPtr->offsetU = 0;
      rpPtr->offsetY = 1;
      rpPtr->offsetV = 2;
      
      if (rawConfigPtr == NULL)
        return;
    }

  switch (rawConfigPtr->image_type)
    {
      case RAW_YUYV:
        rpPtr->offsetY = 0;
        rpPtr->offsetU = 1;
        /* note that offset 2 holds the Y component value of next pixel */
        rpPtr->offsetV = 3; 
        break;
      case RAW_UYVY:
      default:
        rpPtr->offsetU = 0;
        rpPtr->offsetY = 1;
        rpPtr->offsetV = 2;  
        /* note that offest 3 holds the y component value of next pixel */
        break;
    }
  
}  /* end set_yuv_offsets */




/* ----------------------------------
 * get_file_size
 * ----------------------------------
 * retuirn size of the spcified file or -1 on errors.
 */
static gint32
get_file_size(const char *filename)
{
  struct stat  l_stat_buf;

  /* get File Length */
  if (0 != g_stat(filename, &l_stat_buf))
  {
    printf("file:%s:\n  file does not exist or is not readable.\n"
            , filename
            );
    /* stat error (file does not exist) */
    return(-1);
  }

  return (l_stat_buf.st_size);

}  /* end get_file_size */


/* ----------------------------------
 * guess_yuv422_width_and_height
 * ----------------------------------
 * guess width and height of the raw YUV422 file by checking for typical
 * image resulutions. (such as written by by MagicLantern software
 * running on Canon EOS Cameras using RAW_UYVY byte order)
 * OUT: guessWidth and guessHeight  (may contain 0 if plausible guess was NOT possible)
 */
static void
guess_yuv422_width_and_height(gint32 *subImagesX, gint32 *subImagesY
                             ,gint32 *guessWidth, gint32 *guessHeight
                             ,gint32 fileSize)
{
#define MAX_RESOLOUTIONS 23
#define MAX_MODES 9
#define MAX_ASPECTS 2
   static gint32 typical_hires_modes_array[MAX_MODES][2] = {
    {1, 1},  /* 1*X  1*Y  single image (lo-res) */
    {1, 2},  /* 1*X  2*Y  hi-res composed via 2 subimages */
    {2, 2},  /* 2*X  2*Y  */
    {3, 2},  /* 2*X  3*Y  */
    {3, 3},  /* 3*X  3*Y  */
    {4, 3},  /* 3*X  4*Y  */
    {4, 4},  /* 4*X  4*Y  */
    {5, 4},  /* 4*X  4*Y  */
    {5, 5}   /* 5*X  5*Y  */
  };

   static gint32 typical_resolutions_array[MAX_RESOLOUTIONS][2] = {
    {1120, 746},  /* Canon EOS 5D2, 5x             */
    {1872, 1080}, /* Canon EOS 5D2, REC            */
    {1024, 680},  /* Canon EOS 5D2/50D standby     */
    {1560, 884},  /* Canon EOS 50D REC             */
    {944, 632},   /* Canon EOS 50D/500D 5x         */
    {928, 616},   /* Canon EOS 500D photo          */
    {1576, 1048}, /* Canon EOS 500D 1080p          */
    {1576, 632},  /* Canon EOS 500D 720p           */
    {720, 480},   /* Canon EOS 500D 480p, normal LV buffer */
    {1056, 704},  /* Canon EOS 550D/60D photo/stby */
    {1720, 974},  /* Canon EOS 550D/60D 1080p      */
    {1280, 580},  /* Canon EOS 550D/60D 720p       */
    {640, 480},   /* obvious :P          */
    {1024, 680},  /* Canon EOS 550D/60D 480p stby  */
    {1056, 756},  /* Canon EOS 600D USB            */
    {1728, 972},  /* Canon EOS 600D REC 3x         */
    {1680, 945},  /* Canon EOS 600D REC 1x         */
    {1280, 560},  /* Canon EOS 600D 720p           */
    {1152, 768},  /* Canon EOS 5D3 5x              */
    {1904, 1270}, /* Canon EOS 5D3 1x              */
    {1920, 1080}, /* Canon EOS HDMI FullHD         */
    {1920, 540},  /* Canon EOS HDMI 720p           */
    {960, 540}    /* Canon EOS HDMI 640 crop       */
  };
  gdouble typical_aspect_table[MAX_ASPECTS] = {
   (4.0/3.0),
   (16.0/9.0)
  };
  gdouble  numberOfPixels;
  gint     idx;
  gint     idm;

  *guessWidth = 0;
  *guessHeight = 0;
  *subImagesX = 1;
  *subImagesY = 1;

  if ((fileSize < 4)
  || ((fileSize & 1) != 0))
    {
        printf("file size:%d bytes is not plausible (failed to guess width/height)\n"
          , (int)fileSize
          );
      return;
    }

  /* 1st guess based on known typical formats */
  for (idm=0; idm < MAX_MODES; idm++)
    {
      gint32   subX;
      gint32   subY;
       
      subX = typical_hires_modes_array[idm][0];
      subY = typical_hires_modes_array[idm][1];

      for (idx=0; idx < MAX_RESOLOUTIONS; idx++)
        {
          guint w;
          guint h;

          w = typical_resolutions_array[idx][0];
          h = typical_resolutions_array[idx][1];

          if (fileSize == w*subX * h*subY * AVG_BPP_YUV422)
            {
              *guessWidth = w;
              *guessHeight = h;
              *subImagesX = subX;
              *subImagesY = subY;
              return;
            }
        }
    }

  /* 2nd guess based on typical width / height ratios */
  numberOfPixels = fileSize / 2;
  for(idx = 0; idx < MAX_ASPECTS; idx++)
    {
      gdouble wd, hd;
      guint   w, h;

      wd = sqrt (numberOfPixels * typical_aspect_table[idx]);
      hd = wd / typical_aspect_table[idx];
      w = wd;
      h = hd;
      if ((w * h) == numberOfPixels)
      {
        *guessWidth = w;
        *guessHeight = h;
        return;
      }
    }

  printf("size:%d bytes (failed to guess width/height)\n"
          , (int)fileSize
          );

}  /* end guess_yuv422_width_and_height */


/* ----------------------------------------
 * set_tile_cache
 * ----------------------------------------
 * set up tile cache to speed up row by row processing
 * (to avoid excessive tile swapping at least all tiles of the processed row must fit into the tile cache)
 */
static void
set_tile_cache(guint widthInPixels, guint heightInPixels)
{
    gulong cache_ntiles;
    gulong regionTileWidth;
    gulong regionTileHeight;

    regionTileWidth = 1 + (widthInPixels / gimp_tile_width()) ;
    regionTileHeight = 1 + (heightInPixels / gimp_tile_height()) ;

    cache_ntiles = regionTileWidth * regionTileHeight;

    gimp_tile_cache_ntiles (CLAMP(cache_ntiles, 32, 5000));

}  /* end set_tile_cache */



/* new image handle functions */





/* ---------------------------
 * convert_row_from_rgb_to_yuv
 * ---------------------------
 */
static void
convert_row_from_rgb_to_yuv(RawProcessing *rpPtr, guchar *row, guchar *rowRawYuv, gint32 rowWidth, gint bpp)
{
  guchar *yPtr;   /* Y is unsigned */
  gchar  *uPtr;   /* U, V are signed */
  gchar  *vPtr;
  guchar *rgbPtr;
  gint32 pixelPairCount;

  rgbPtr = row;
  uPtr = (gchar *)&rowRawYuv[rpPtr->offsetU];
  yPtr =          &rowRawYuv[rpPtr->offsetY];
  vPtr = (gchar *)&rowRawYuv[rpPtr->offsetV];

  for(pixelPairCount = 0; pixelPairCount < rowWidth/2; pixelPairCount++)
    {
       gchar   uTab[2];  /* U, V are signed */
       gchar   vTab[2];
       gint    avgU;
       gint    avgV;

       convert_rgb_2_yuv(rgbPtr, yPtr, &uTab[0], &vTab[0]);
       /* advance y foreach handled pixel */
       yPtr += AVG_BPP_YUV422;
       rgbPtr += bpp;

       convert_rgb_2_yuv(rgbPtr, yPtr, &uTab[1], &vTab[1]);
       rgbPtr += bpp;


       avgU = (uTab[0] + uTab[1]) / 2;
       avgV = (vTab[0] + vTab[1]) / 2;

       *uPtr = CLAMP(avgU, -128, 127);
       *vPtr = CLAMP(avgV, -128, 127);

       *uPtr = uTab[0];  /// debug disable average, pick from even pixel only
       *vPtr = vTab[0];

       /* advance u and v only once per 2 handled pixels
        * (but y foreach handled pixel)
        */
       yPtr += AVG_BPP_YUV422;
       uPtr += 4;
       vPtr += 4;


    }

}  /* end convert_row_from_rgb_to_yuv */


/* ---------------------------
 * read_and_convert_line
 * ---------------------------
 * read one pixelrow in full width from image at specified y coordinate.
 * Note that ML HiRes images are composed as matrix of subimages.
 * therefore reading a full line requires multiple seek/read operations
 * (one for each relevant subimage).
 * this procedure is intended for preview purpose.
 */
static gint
read_and_convert_line (RawProcessing  *rpPtr, RawConfig  *rawConfigPtr,
           guchar     *previewRowRgb,
           guchar     *bufRgb,        /* holds one RGB line in full image size( sub_image_width * sub_images_x * bpp) */
           guchar     *localBufYuv,   /* holds one YUV line in sub image size ( sub_image_width * AVG_BPP_YUV422) */
           gint32      preview_width,
           gint32      y,
           gdouble     scaleStepsizeX,
           gint        bpp,
           gint32      file_size
           )
{
  gint32  len;   /* length of one pixel line in a local subimage in bytes (that can be fetch by one read operation) */
  gint32  pos;   /* position to feth one line of a local subimage */
  gint    subX;  /* horizontal subimage position */
  gint    subY;  /* vertical subimage position */
  gint    localY;
  gint32  subImageYuvSize; /* size of one YUV422 encoded sub image in bytes */
  gint32  sumImagesOffset;
  gint    subRowstrideRgb;
  
  
  len = rawConfigPtr->sub_image_width * AVG_BPP_YUV422;
  subRowstrideRgb = rawConfigPtr->sub_image_width * bpp;

  subY = y / rawConfigPtr->sub_image_height;
  localY = y - (subY * rawConfigPtr->sub_image_height);
  subImageYuvSize = rawConfigPtr->sub_image_width * rawConfigPtr->sub_image_height * AVG_BPP_YUV422;
  sumImagesOffset = subY * (subImageYuvSize * rawConfigPtr->sub_images_x);

  for(subX=0; subX < rawConfigPtr->sub_images_x; subX++)
    {
      pos = sumImagesOffset 
          + (subX * subImageYuvSize)
          + (rawConfigPtr->sub_image_width * localY * AVG_BPP_YUV422);

      if (pos < file_size)
        {
          size_t       len_read;
          fseek (rpPtr->fp_preview, pos, SEEK_SET);
          
          len_read = fread(localBufYuv, 1, len, rpPtr->fp_preview);
        
          if (len_read != len)
            {
              memset (&bufRgb[subX * rawConfigPtr->sub_image_width * bpp], 0xFF, subRowstrideRgb);
            }
          else
            {
              convert_row_from_yuv_2_rgb(rpPtr, &bufRgb[subX * rawConfigPtr->sub_image_width * bpp]
                                            , localBufYuv, rawConfigPtr->sub_image_width, bpp);
            }
        }
      else
        {
          memset (&bufRgb[subX * rawConfigPtr->sub_image_width * bpp], 0xFF, subRowstrideRgb);
        }
    }


  /* scale_one_preview_line */
  {
    gint x;
    gint sX;
    guchar *wrRgbPtr;
    
    wrRgbPtr = &previewRowRgb[0];
    
    for(x=0; x < preview_width; x++)
      {
        gint posX;
        gdouble scaleDX;

        scaleDX = x * scaleStepsizeX;
        sX = rint(scaleDX);
        if (sX <  (rawConfigPtr->sub_image_width * rawConfigPtr->sub_images_x))
          {
            posX = sX * bpp;
            wrRgbPtr[0] = bufRgb[posX + 0];
            wrRgbPtr[1] = bufRgb[posX + 1];
            wrRgbPtr[2] = bufRgb[posX + 2];
          }
        else 
          {
            wrRgbPtr[0] = 0xFF;
            wrRgbPtr[1] = 0xFF;
            wrRgbPtr[2] = 0xFF;
          }
        
        wrRgbPtr += bpp;
      }
  }

  return 0;

}  /* end read_and_convert_line */



/* ---------------------------
 * convert_row_from_yuv_2_rgb
 * ---------------------------
 */
static void
convert_row_from_yuv_2_rgb(RawProcessing  *rpPtr, guchar *row, guchar *rowRawYuv, gint32 rowWidth, gint bpp)
{
  guchar *yPtr;  /* Y is unsigned */
  gchar  *uPtr;  /* U,V are signed */
  gchar  *vPtr;
  guchar *rgbPtr;
  gint32 colIndexX;

  rgbPtr = row;
  uPtr = (gchar *)&rowRawYuv[rpPtr->offsetU];
  yPtr =          &rowRawYuv[rpPtr->offsetY];
  vPtr = (gchar *)&rowRawYuv[rpPtr->offsetV];

  for(colIndexX = 0; colIndexX < rowWidth; colIndexX++)
    {
       convert_yuv_2_rgb(yPtr, uPtr, vPtr, rgbPtr);
       rgbPtr += bpp;

       /* advance y for each pixel, distance to next y value is 2 */
       yPtr += AVG_BPP_YUV422;

       if ((colIndexX & 1) != 0)
         {
           /* advance u and v only on odd pixels (distance to next value of same type is 4) */
           uPtr += 4;
           vPtr += 4;
         }
    }

}  /* end convert_row_from_yuv_2_rgb */


/* ---------------------------
 * raw_load_yuv422
 * ---------------------------
 * this procedure supports loading into 3, 4 bpp drawable RGB or RGBA
 */
static gboolean
raw_load_yuv422 (RawProcessing  *rpPtr,
                 RawGimpData *data,
                 gint         bpp)
{
  gdouble progressMaxPixels;
  gdouble progressPixelCount;
  
  gint subX;          /* horizontal subimage position */
  gint subY;          /* vertical subimage position */
  gint32 rowSizeYUV;  /* row size of YUV subimage in bytes */
        
  gint subOffsetX;  /* horizontal offset of sub image in pixels */
  gint subOffsetY;  /* vertical offset of sub image in pixels */
  
  RawConfig  *rawConfigPtr = rpPtr->rawConfigPtr;
  
  guchar *row = NULL;
  guchar *rowRawYuv = NULL;   /* one row  of a sub YUV422 image */
  gint    rowIndexY;

  progressPixelCount = 0;
  progressMaxPixels = (rawConfigPtr->sub_image_width * rawConfigPtr->sub_images_x)
                    * (rawConfigPtr->sub_image_height * rawConfigPtr->sub_images_y);

  set_yuv_offsets(rpPtr, rawConfigPtr);

  rowRawYuv = g_try_malloc (rawConfigPtr->sub_image_width * AVG_BPP_YUV422);
  if (! rowRawYuv)
    return FALSE;

  row = g_try_malloc (rawConfigPtr->sub_image_width * bpp);
  if (! row)
    return FALSE;

  /* init row (0xFF results in white (and full opaque in case alpha channel is present) */
  if (bpp > 3)
    memset (row, 0xFF, (rawConfigPtr->sub_image_width * bpp));

  /* YUV-422 uses (average) 2 byte per pixel */
  rowSizeYUV = rawConfigPtr->sub_image_width * AVG_BPP_YUV422;

  for(subY=0; subY < rawConfigPtr->sub_images_y; subY++)
    {
      subOffsetY = subY * rawConfigPtr->sub_image_height;
      for(subX=0; subX < rawConfigPtr->sub_images_x; subX++)
        {
          subOffsetX = subX * rawConfigPtr->sub_image_width;

          for(rowIndexY = 0; rowIndexY < rawConfigPtr->sub_image_height; rowIndexY++)
            {
              if (! fread (rowRawYuv, rowSizeYUV, 1, data->fp))
                {
                  g_printerr ("fread failed\n");
                  memset (row, 0xFF, (rawConfigPtr->sub_image_width * bpp));
                }
              else
                {
                  convert_row_from_yuv_2_rgb(rpPtr, row, rowRawYuv, rawConfigPtr->sub_image_width, bpp);
                }


              gimp_pixel_rgn_set_row(&data->region, row, subOffsetX, subOffsetY + rowIndexY, rawConfigPtr->sub_image_width);
              
              if (rpPtr->do_progress)
                {
                  progressPixelCount += rawConfigPtr->sub_image_width;
                  gimp_progress_update (CLAMP((progressPixelCount/progressMaxPixels), 0.0, 1.0));
                }

            }


        }  /* end subX loop */
    }      /* end subY loop */


  g_free (row);
  g_free (rowRawYuv);

  return TRUE;

}  /* end raw_load_yuv422 */


/* end new image handle functions */


/* ----------------------------------
 * save_image
 * ----------------------------------
 * Save image supports the MagicLantern typical style where one HiRes image is composed
 * as matrix of subimage areas that are written into one common file.
 * HiRes mode 2x2 (sub_images_x = 2; sub_images_y = 2) is composed from 4 subimages.
 *   +-----------+----------+
 *   | subimg 0  | subimg 1 |  
 *   +-----------+----------+
 *   | subimg 2  | subimg 3 |  
 *   +-----------+----------+
 * E.g. save behaves like saving top left area, top right are, bottom left area and bottom right
 * area to 4 separated yuv422 files and concateneates their content into one resulting file.
 *
 * Note that the load procedure expects this kind of HiRes images as default for the typical
 * file sizes written by MagicLantern.
 *
 * Set sub_images_x = 1; sub_images_y = 1 to disable this MagicLantern typical style .
 */
static GimpPDBStatusType
save_image (RawProcessing  *rpPtr, const gchar  *filename,
            gint32        image_id,
            gint32        drawable_id,
            GError      **error)
{
  GimpDrawable     *drawable;
  GimpPixelRgn      pixel_rgn;
  gint32            bpp;
  gint32            sub_image_width, sub_image_height;  /* refers to sub_image dimensions */
  gint32            sub_images_x;   /* number of horizontal subimages */
  gint32            sub_images_y;   /* number of vertical subimages */
  gboolean          have_alpha = 0;
  FILE             *fp;
  GimpPDBStatusType ret = GIMP_PDB_SUCCESS;
  RawConfig  *rawConfigPtr = rpPtr->rawConfigPtr;

  bpp        = gimp_drawable_bpp (drawable_id);
  have_alpha = gimp_drawable_has_alpha (drawable_id);

  if (gimp_drawable_is_indexed (drawable_id))
    {
      ret = GIMP_PDB_EXECUTION_ERROR;
      g_printerr ("indexed mode images are not supported.\n");
      return;
    }

  if ((bpp != 3) && (bpp !=4))
    {
      ret = GIMP_PDB_EXECUTION_ERROR;
      g_printerr ("gray images and modes with bpp not 3 or 4 are not supported.\n");
      return;
    }

  set_yuv_offsets(rpPtr, rawConfigPtr);

  if (rpPtr->do_progress)
    gimp_progress_init_printf (_("Writing '%s'"), gimp_filename_to_utf8 (filename));

  /* get info about the current image */
  drawable = gimp_drawable_get (drawable_id);

  sub_image_width  = drawable->width / rawConfigPtr->sub_images_x;
  sub_image_height = drawable->height / rawConfigPtr->sub_images_y;
  
  if ((sub_image_width * rawConfigPtr->sub_images_x == drawable->width)
  &&  (sub_image_height * rawConfigPtr->sub_images_y == drawable->height))
    {
      /* write as matrix of subimages (same partitioning style as MagicLantern HiRes images */
      sub_images_x = rawConfigPtr->sub_images_x;
      sub_images_y = rawConfigPtr->sub_images_y;
    }
  else
    {
      /* image size is NOT tileable without rest,  force write as single YUV422 raw image*/
      sub_image_width  = drawable->width;
      sub_image_height = drawable->height;
      sub_images_x = 1;
      sub_images_y = 1;
    }

  set_tile_cache(drawable->width, sub_image_height);

  fp = g_fopen (filename, "wb");

  if (! fp)
    {
      g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                   _("Could not open '%s' for writing: %s"),
                   gimp_filename_to_utf8 (filename), g_strerror (errno));
      ret = GIMP_PDB_EXECUTION_ERROR;
      return;
    }

  else
    {
      gdouble progressMaxPixels;
      gdouble progressPixelCount;
      guchar *bufRgb;
      guchar *bufYuv;
      gint    rowIndexY;
      gint    subOffsetX;  /* horizontal offset of sub image in pixels */
      gint    subOffsetY;  /* vertical offset of sub image in pixels */
      gint subX;  /* horizontal subimage position */
      gint subY;  /* vertical subimage position */

      gimp_pixel_rgn_init (&pixel_rgn, drawable,
                       0, 0, drawable->width, drawable->height,
                       FALSE, FALSE);
      bufRgb = g_new (guchar, sub_image_width * bpp);
      bufYuv = g_new (guchar, sub_image_width * AVG_BPP_YUV422);

      progressPixelCount = 0;
      progressMaxPixels = (sub_image_width * sub_images_x) * (sub_image_height * sub_images_y);

      for(subY=0; subY < sub_images_y; subY++)
        {
          subOffsetY = subY * sub_image_height;
          for(subX=0; subX < sub_images_x; subX++)
            {
              subOffsetX = subX * sub_image_width;

              for(rowIndexY = 0; rowIndexY < sub_image_height; rowIndexY++)
                {
                  gimp_pixel_rgn_get_row (&pixel_rgn, bufRgb, subOffsetX, subOffsetY + rowIndexY, sub_image_width);
                  convert_row_from_rgb_to_yuv(rpPtr, bufRgb, bufYuv, sub_image_width, bpp);


                  if (! fwrite (bufYuv, sub_image_width * AVG_BPP_YUV422, 1, fp))
                    {
                      ret = GIMP_PDB_EXECUTION_ERROR;
                      break;
                    }

                  if (rpPtr->do_progress)
                    {
                      progressPixelCount += sub_image_width;
                      gimp_progress_update (CLAMP((progressPixelCount/progressMaxPixels), 0.0, 1.0));
                    }

                }


            }   /* end subX loop */
         }      /* end subY loop */


      fclose (fp);
      g_free(bufRgb);
      g_free(bufYuv);

    }
  
  gimp_drawable_detach (drawable);

  return ret;

}  /* end save_image */



/* ----------------------------------------
 * raw_packet_loader_thread
 * ----------------------------------------
 * This procedure typically runs in 2 or more parallel worker threads.
 * Each worker thread has its own list of scheduled packets to process.
 * This includes readin the packet (via thread local own filehandle) 
 * and YUV --> RGB conversion.
 * Note that all packet buffers are already allocated (by the main thread packet scheduler)
 */
static gpointer
raw_packet_loader_thread(gpointer data)
{
  FILE *fp;
  RawPacketLoaderThreadUserData *usrPtr;
  RawPacket *pkt;
  RawPacket *nextPkt = NULL;

  usrPtr = (RawPacketLoaderThreadUserData *)data;
  
// printf("PKT_thread [%d] 1  filename:%s\n", usrPtr->tid, usrPtr->filename);      

  fp = g_fopen (usrPtr->filename, "rb");
  if (fp != NULL)
    {
      for(pkt = usrPtr->packetList; pkt != NULL; pkt = nextPkt)
        {
          size_t len;
          size_t len_read;
       
          fseek (fp, pkt->packet_seek_pos, SEEK_SET);
          len = AVG_BPP_YUV422 * pkt->packet_width * pkt->packet_height;
          len_read = fread(usrPtr->packet_yuv_buffer, 1, len, fp);
          if (len_read != len)
            {
              memset (pkt->packet_rgb_buffer, 0xFF, pkt->packet_rgb_buffer_size);
            }
          else
            {
//               printf("thread[%d] convert_row_from_yuv_2_rgb (fread OK pos:%d len:%d)\n"
//                   , usrPtr->tid
//                   , pkt->packet_seek_pos
//                   , len
//                   );
              convert_row_from_yuv_2_rgb(usrPtr->rpPtr, pkt->packet_rgb_buffer, usrPtr->packet_yuv_buffer
                                   , pkt->packet_width * pkt->packet_height, usrPtr->bpp);

            }
       
          g_mutex_lock(usrPtr->elemMutex);
          pkt->packet_status = PACKET_STATUS_CONVERTED;
          nextPkt = pkt->next;
          g_mutex_unlock(usrPtr->elemMutex);
        }
      fclose(fp);
    }
  else
    {
      /* clear rgb buffers in all packets to White and set PACKET_STATUS_CONVERTED
       * (required because the main thread waits for all packets to be converted as finish condition)
       */
      for(pkt = usrPtr->packetList; pkt != NULL; pkt = nextPkt)
        {
          memset (pkt->packet_rgb_buffer, 0xFF, pkt->packet_rgb_buffer_size);
          g_mutex_lock(usrPtr->elemMutex);
          pkt->packet_status = PACKET_STATUS_CONVERTED;
          nextPkt = pkt->next;
          g_mutex_unlock(usrPtr->elemMutex);
        }
    }
  
}  /* end raw_packet_loader_thread */


/* ----------------------------------------
 * schedule_packet_lists
 * ----------------------------------------
 * This procedure creates packet lists (one separate list for each worker thread)
 * all packets have sub_image_width and gimp_tile_heihgt (or less for the last remaining packet(s).
 */
static void
schedule_packet_lists(const char *filename, RawConfig *rawConfigPtr
  , PacketScheduleData *psdPtr, gint bpp, RawProcessing *rpPtr)
{
  gint   stdPacketHeight;
  gint   subX;  /* horizontal subimage position */
  gint   subY;  /* vertical subimage position */
  gint32 rowSizeYUV;
  gint32 subImageSizeYUV;
  gint   subOffsetX;       /* horizontal offset of sub image in pixels */
  gint   subOffsetY;       /* vertical offset of sub image in pixels */
  gint   rowIndexY;
  gint   tid;              /* thread id */
  RawPacket *lastPkt[MAX_SUPPORTED_THREADS];

    
  stdPacketHeight = MAX_PKT_HEIGHT;
  
   
  /* YUV-422 uses (average) 2 byte per pixel */
  rowSizeYUV = rawConfigPtr->sub_image_width * AVG_BPP_YUV422;
  subImageSizeYUV = rowSizeYUV * rawConfigPtr->sub_image_height;

  /* one of the processors is required for main thread processing,
   * all additional processors shall be used for the worker threads.
   * CLAMP is used to make sure that there is at least one worker thrad
   * (even on single processor systems)
   */
  psdPtr->numberOfWorkerThreads = 
    CLAMP(rawConfigPtr->numProcessors -1, 1, MAX_SUPPORTED_THREADS);
  psdPtr->numberOfScheduledPackets = 0;
  
  /* prepare user data for each worker thread */
  for (tid=0; tid < psdPtr->numberOfWorkerThreads; tid++)
    {
      psdPtr->usrDataTab[tid].tid = tid;
      psdPtr->usrDataTab[tid].bpp = bpp;
      psdPtr->usrDataTab[tid].packetList = NULL;
      psdPtr->usrDataTab[tid].packet_yuv_buffer = g_malloc(AVG_BPP_YUV422 * rawConfigPtr->sub_image_width * stdPacketHeight);
      psdPtr->usrDataTab[tid].filename = filename;
      psdPtr->usrDataTab[tid].elemMutex = g_mutex_new();
      psdPtr->usrDataTab[tid].rpPtr = rpPtr;
      lastPkt[tid] = NULL;
    }

  /* create packets and assign them to the worker threads */
  tid = 0;
  for(subY=0; subY < rawConfigPtr->sub_images_y; subY++)
    {
      gint32  upperSubimagesOffsetYUV;
      
      
      subOffsetY = subY * rawConfigPtr->sub_image_height;
      upperSubimagesOffsetYUV = subY * rawConfigPtr->sub_images_x * subImageSizeYUV ;
      
      for(subX=0; subX < rawConfigPtr->sub_images_x; subX++)
        {
          gint32 leftSubimagesOffsetYUV;
          
          subOffsetX = subX * rawConfigPtr->sub_image_width;
          leftSubimagesOffsetYUV = subX * subImageSizeYUV;
  
          for(rowIndexY = 0; rowIndexY < rawConfigPtr->sub_image_height; rowIndexY += stdPacketHeight)
            {
              RawPacket *pkt;
            
              pkt = g_new0 (RawPacket, 1);
              pkt->next = NULL;
              pkt->packet_status = PACKET_STATUS_EMPTY; 
              pkt->packet_x = subX * rawConfigPtr->sub_image_width;
              pkt->packet_y = rowIndexY + (subY * rawConfigPtr->sub_image_height);
              pkt->packet_height = MIN(stdPacketHeight, rawConfigPtr->sub_image_height - rowIndexY);
              pkt->packet_width = rawConfigPtr->sub_image_width;
              pkt->packet_seek_pos = (rowIndexY * rowSizeYUV) + upperSubimagesOffsetYUV + leftSubimagesOffsetYUV;
              pkt->packet_rgb_buffer_size = pkt->packet_width * pkt->packet_height * bpp;
              pkt->packet_rgb_buffer = g_malloc(pkt->packet_rgb_buffer_size);

              psdPtr->numberOfScheduledPackets++;
      
              /* add packet to end of worker thread specific packet lists */
              if (lastPkt[tid] == NULL)
                {
                  psdPtr->usrDataTab[tid].packetList = pkt;
                  lastPkt[tid] = pkt;
                }
              else
                {
                  lastPkt[tid]->next = pkt;
                  lastPkt[tid] = pkt;
                }
              
              tid++;
              if (tid >= psdPtr->numberOfWorkerThreads)
                tid = 0;

           }
        }  /* end subX loop */
    }      /* end subY loop */

}  /* end schedule_packet_lists */


/* ------------------------------
 * free_packet_schedule_resources
 * ------------------------------
 */
static void
free_packet_schedule_resources(PacketScheduleData *psdPtr)
{
  gint   tid;              /* thread id */

  for (tid=0; tid < psdPtr->numberOfWorkerThreads; tid++)
    {
      RawPacket *pkt;
      RawPacket *nextPkt;

      /* free all packets and packet's rgb buffers */
      nextPkt = NULL;
      for(pkt = psdPtr->usrDataTab[tid].packetList; pkt != NULL; pkt = nextPkt)
        {
          nextPkt = pkt->next;
          g_free(pkt->packet_rgb_buffer);
          g_free(pkt);
        }
      
      /* free yuv buffers and mutex */
      g_free(psdPtr->usrDataTab[tid].packet_yuv_buffer);
      g_mutex_free(psdPtr->usrDataTab[tid].elemMutex);
    }

}  /* end free_packet_schedule_resources */


/* ----------------------------------
 * load_image_multithread_by_packet
 * ----------------------------------
 */
static gint32
load_image_multithread_by_packet(RawProcessing  *rpPtr, const char *filename, RawGimpData *data)
{
  GThread             *workerThread[MAX_SUPPORTED_THREADS];
  PacketScheduleData   psdData;
  PacketScheduleData  *psdPtr;
  RawConfig           *rawConfigPtr;
  gint                 tid;
  gint                 bpp;
  gint                 packetsDoneCounter;
  RawPacket           *currPkt[MAX_SUPPORTED_THREADS];
  gdouble              progressMaxPixels;
  gdouble              progressPixelCount;
  gboolean             joinable;
  RawPacketLoaderThreadUserData *usrPtr;
      
  g_thread_init(NULL);
  joinable = TRUE;

  
  rawConfigPtr = rpPtr->rawConfigPtr;
  psdPtr = &psdData;
  

  bpp = data->drawable->bpp;

  progressPixelCount = 0;
  progressMaxPixels = (rawConfigPtr->sub_image_width * rawConfigPtr->sub_images_x)
                    * (rawConfigPtr->sub_image_height * rawConfigPtr->sub_images_y);
  
  schedule_packet_lists(filename, rawConfigPtr, psdPtr, bpp, rpPtr);
  
  /* start worker threads */
  for (tid=0; tid < psdPtr->numberOfWorkerThreads; tid++)
    {
      usrPtr = &psdPtr->usrDataTab[tid];
      currPkt[tid] = usrPtr->packetList;
      workerThread[tid] = g_thread_create((GThreadFunc)raw_packet_loader_thread
                        , &psdPtr->usrDataTab[tid]  /* data */
                        , joinable
                        , NULL  /* GError **error (NULL dont report errors) */
                        );
    }
  
  packetsDoneCounter = 0;
  while(packetsDoneCounter < psdPtr->numberOfScheduledPackets)
    {
      gint32  oldPacketsDone;
      oldPacketsDone = packetsDoneCounter;
      
      for (tid=0; tid < psdPtr->numberOfWorkerThreads; tid++)
        {
          RawPacket *pkt;
          
          pkt = currPkt[tid];
          if (pkt == NULL)
            {
              continue; /* no more packets available for thread tid */
            }
          usrPtr = &psdPtr->usrDataTab[tid];
          
          g_mutex_lock(usrPtr->elemMutex);
          if (pkt->packet_status == PACKET_STATUS_CONVERTED)
            {
              pkt->packet_status = PACKET_STATUS_TRANSFER_TO_GIMP;
              g_mutex_unlock(usrPtr->elemMutex);

              /* transferToGimp and progress handling */
              gimp_pixel_rgn_set_rect (&data->region   /* GimpPixelRgn *pr */
                                       , pkt->packet_rgb_buffer
                                       , pkt->packet_x
                                       , pkt->packet_y
                                       , pkt->packet_width
                                       , pkt->packet_height
                                       );
                                                         
              if (rpPtr->do_progress)
                {
                  progressPixelCount += (pkt->packet_width * pkt->packet_height);
                  gimp_progress_update (CLAMP((progressPixelCount/progressMaxPixels), 0.0, 1.0));
                }

              currPkt[tid] = pkt->next;
              packetsDoneCounter++;

            }
          else
            {
              g_mutex_unlock(usrPtr->elemMutex);
            }
        }
        if(oldPacketsDone == packetsDoneCounter)
          g_usleep(1000);  /* sleep 0.01 seconds */

    }

  for (tid=0; tid < psdPtr->numberOfWorkerThreads; tid++)
    {
      //printf("before g_thread_join workerThread[%d]\n", tid);
      g_thread_join(workerThread[tid]);
    }

  free_packet_schedule_resources(psdPtr);

}  /* end load_image_multithread_by_packet */


/* ----------------------------------
 * load_image
 * ----------------------------------
 */
static gint32
load_image (RawProcessing  *rpPtr, const gchar  *filename,
            GError      **error)
{
  RawGimpData       *data;
  gint32             layer_id = -1;
  GimpImageType      ltype    = GIMP_RGB;
  GimpImageBaseType  itype    = GIMP_RGB_IMAGE;
  gint               bpp = 0;
  gint32             composed_width;
  gint32             composed_height;
  RawConfig         *rawConfigPtr;

  rawConfigPtr = rpPtr->rawConfigPtr;
  data = g_new0 (RawGimpData, 1);
  data->fp = NULL;

  rpPtr->file_size = get_file_size (filename);


  composed_width = rawConfigPtr->sub_image_width * rawConfigPtr->sub_images_x;
  composed_height = rawConfigPtr->sub_image_height * rawConfigPtr->sub_images_y;
  
 
// printf("load_image composed_wxh: %d x %d   sub_image_width:%d sub_image_height:%d  sub_images_x:%d sub_images_y:%d numProcessors:%d\n"
//   , (int)composed_width
//   , (int)composed_height
//   , (int)rawConfigPtr->sub_image_width
//   , (int)rawConfigPtr->sub_image_height
//   , (int)rawConfigPtr->sub_images_x
//   , (int)rawConfigPtr->sub_images_y
//   , (int)rawConfigPtr->numProcessors
//   );
  
  
  
  /* make sure we don't load image bigger than file size */
  if (composed_height > (rpPtr->file_size / composed_width / AVG_BPP_YUV422))
    {
      //  rawConfigPtr->sub_image_height = rpPtr->file_size / rawConfigPtr->sub_image_width / AVG_BPP_YUV422 / rawConfigPtr->sub_images_x;
      g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                   _("Could not open '%s' height: %d larger than filesize / (width * 2)"),
                   gimp_filename_to_utf8 (filename), composed_height);
      return -1;
    }

  if (rpPtr->do_progress)
    gimp_progress_init_printf (_("Opening '%s'"), gimp_filename_to_utf8 (filename));

  bpp   = 3;
  ltype = GIMP_RGB_IMAGE;
  itype = GIMP_RGB;



  data->image_id = gimp_image_new (composed_width,
                                   composed_height,
                                   itype);
  gimp_image_set_filename(data->image_id, filename);
  layer_id = gimp_layer_new (data->image_id, _("Background"),
                             composed_width, composed_height, ltype,
                             100, GIMP_NORMAL_MODE);
  gimp_image_add_layer (data->image_id, layer_id, 0);

  data->drawable = gimp_drawable_get (layer_id);

  gimp_pixel_rgn_init (&data->region, data->drawable,
                       0, 0, composed_width, composed_height,
                       TRUE, FALSE);

  if (rawConfigPtr->numProcessors > 1)
    {
      /* load parallel with numProcessors
       * (where each thread opens its own filehandle)
       */
      load_image_multithread_by_packet(rpPtr, filename, data);
    }
  else
    {
      data->fp = g_fopen (filename, "rb");
      if (! data->fp)
        {
          g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                       _("Could not open '%s' for reading: %s"),
                       gimp_filename_to_utf8 (filename), g_strerror (errno));
          gimp_drawable_detach (data->drawable);
          return -1;
        }

      raw_load_yuv422 (rpPtr, data, bpp);

      fclose (data->fp);
    }

  gimp_drawable_flush (data->drawable);
  gimp_drawable_detach (data->drawable);


  return data->image_id;

}  /* end load_image */




/* misc GUI stuff */

/* ----------------------------------
 * trigger_preview_update
 * ----------------------------------
 * trigger preview update rendering via timer job.
 * Note: rendering starts after a short delay.
 *   when more preview relevant settings change within very short time,
 *   those changes will cause only one preview render request,
 *   and rendering is done only once with the latest settings.
 */
static void
trigger_preview_update (GimpPreviewArea *preview)
{
  RawProcessing  *rpPtr;
  
  if (preview == NULL)
    return;

  rpPtr = (RawProcessing *)g_object_get_data (G_OBJECT (preview), "rpPtr");
  if (rpPtr == NULL)
    return;

  /* add a timer with 70 millesec delay
   * (no need to do that if the there is already a pending
   *  timer request. if there is no pending request rpPtr->upd_timertag == -1)
   *
   * The rpPtr->enable_preview typically is FALSE before the preview widget is created
   * and goes to FALSE immediate before destroying the dialog witget(s).
   * therfore ignore render requests when rpPtr->enable_preview is not TRUE.
   */
  if ((rpPtr->upd_timertag < 0)
  &&  (rpPtr->enable_preview == TRUE))
    {
      rpPtr->upd_timertag = (gint32) g_timeout_add(70, (GtkFunction)preview_update_timer_job, preview);
    }

}  /* end trigger_preview_update */


/* ----------------------------------
 * preview_update_timer_job
 * ----------------------------------
 * preview renders full image content (down) scaled to preview size.
 * current implementation reads only picked pixelrows.
 * scaling uses low qaulity row/column picking algorithm 
 * that is able to run fast enough for preveiw purpose.
 *
 * This procedure typically runs as timer job that is triggered when one
 * or more preview relevant settings have changed.
 */
static void
preview_update_timer_job (GimpPreviewArea *preview)
{
  gint     width;
  gint     height;
  gint     bpp;
  gint32   pos;
  gint     x, y;
  RawProcessing  *rpPtr;
  RawConfig      *rawConfigPtr;
  static          RawConfig  *localRawConfigPtr = NULL;
  

  if (preview == NULL)
    return;

  rpPtr = (RawProcessing *)g_object_get_data (G_OBJECT (preview), "rpPtr");
  if (rpPtr == NULL)
    return;

  if (rpPtr->enable_preview != TRUE)
    return;
  
  rawConfigPtr = rpPtr->rawConfigPtr;
  if (rawConfigPtr == NULL)
    return;

  if(rpPtr->upd_timertag >= 0)
    {
      g_source_remove(rpPtr->upd_timertag);
      rpPtr->upd_timertag = -1;
    }

  if(localRawConfigPtr != NULL)
    {
      printf("previous render job is still busy\n");  /// DEBUG
    
      /* previous render job is still busy, trigger next attempt after short delay */
      trigger_preview_update(preview);
      return;
    }

  localRawConfigPtr = g_new0 (RawConfig, 1);
  
  /* clear preview to white */
  gimp_preview_area_fill (preview,
                          0, 0, preview->width, preview->height,
                          255, 255, 255);


  /* check valid filedescriptor before preview render attempt
   * (may have been closed since render request was triggered)
   */
  if (rpPtr->fp_preview != NULL)
    {
      guchar *previewRowRgb;
      guchar *bufRgb;
      guchar *localBufYuv;

      gdouble   scaleStepsizeX;
      gdouble   scaleStepsizeY;

       /* make a snapshot copy of current settings used while preview is rendering
        */
       memcpy(localRawConfigPtr, rawConfigPtr, sizeof(RawConfig));

       width  = MIN (localRawConfigPtr->sub_image_width * localRawConfigPtr->sub_images_x,  preview->width);
       height = MIN (localRawConfigPtr->sub_image_height * localRawConfigPtr->sub_images_y, preview->height);
       bpp = 3;  /* preview bpp */

       previewRowRgb = g_malloc0 (width * bpp);
       bufRgb = g_malloc0 (localRawConfigPtr->sub_image_width * MAX(5, localRawConfigPtr->sub_images_x) * bpp);
       localBufYuv = g_malloc0 (localRawConfigPtr->sub_image_width * AVG_BPP_YUV422);


       scaleStepsizeX = (gdouble)(localRawConfigPtr->sub_image_width * localRawConfigPtr->sub_images_x) / (gdouble)preview->width;
       scaleStepsizeY = scaleStepsizeX;


//        printf("preview_update wxh: %d x %d   sub_image_width:%d sub_image_height:%d  sub_images_x:%d sub_images_y:%d  pv:%d x %d scaleStepsizeX:%f\n"
//            , (int)width
//            , (int)height
//            , (int)localRawConfigPtr->sub_image_width
//            , (int)localRawConfigPtr->sub_image_height
//            , (int)localRawConfigPtr->sub_images_x
//            , (int)localRawConfigPtr->sub_images_y
//            , (int)  preview->width
//            , (int)  preview->height
//            , (float)scaleStepsizeX
//            );


       set_yuv_offsets(rpPtr, localRawConfigPtr);
   
       for (y = 0; y < height; y++)
         {
           gdouble scaleDY;
           gint32 sY;
           
           scaleDY = y * scaleStepsizeY;
           sY = rint(scaleDY);
           if (sY < (localRawConfigPtr->sub_image_height * localRawConfigPtr->sub_images_y))
             {
                read_and_convert_line (rpPtr, localRawConfigPtr,
                                       previewRowRgb, bufRgb, localBufYuv,
                                       width, sY, scaleStepsizeX, bpp,
                                       rpPtr->file_size);
             }
           else
             {
                 memset (previewRowRgb, 0xFF, width * bpp);
             }

           gimp_preview_area_draw (preview, 0, y, width, 1,
                               GIMP_RGB_IMAGE, previewRowRgb, width * bpp);
         }

       g_free (previewRowRgb);
       g_free (localBufYuv);
       g_free (bufRgb);

    }  /* end if fp_preview  != NULL */

  g_free (localRawConfigPtr); 
  localRawConfigPtr = NULL;

}  /* end preview_update_timer_job */


/* ----------------------------------
 * load_dialog
 * ----------------------------------
 */
static gboolean
load_dialog (RawProcessing  *rpPtr, const gchar *filename)
{
#define MAX_BCK_VALS 5
  GtkWidget *dialog;
  GtkWidget *main_vbox;
  GtkWidget *preview;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *combo;
  GtkWidget *button;
  GtkObject *adj;
  GtkObject *adjTab[MAX_BCK_VALS];
  gdouble    bckValue[MAX_BCK_VALS];
  RawType    bck_image_type;
  gint       ibck;
  
  gint32      maxEdgeLengthInPixel;
  gint32      response;
  gboolean    run;
  RawConfig  *rawConfigPtr;
  
  ibck  = 0;
  rpPtr->file_size = get_file_size (filename);
  rpPtr->upd_timertag   = -1;
  rawConfigPtr = rpPtr->rawConfigPtr;
  bck_image_type = rawConfigPtr->image_type;
  
  maxEdgeLengthInPixel = rpPtr->file_size / AVG_BPP_YUV422;

  gimp_ui_init (PLUG_IN_BINARY, TRUE);

  dialog = gimp_dialog_new (_("Load Image from Raw YUV422 Data"), PLUG_IN_BINARY,
                            NULL, 0,
                            gimp_standard_help_func, LOAD_PROC,

                            GIMP_STOCK_RESET, LOCAL_RESPONSE_RESET,
                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            GTK_STOCK_OPEN,   GTK_RESPONSE_OK,

                            NULL);

  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           LOCAL_RESPONSE_RESET,
                                           -1);

  main_vbox = gtk_vbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 12);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), main_vbox);
  gtk_widget_show (main_vbox);

  frame = gtk_frame_new (NULL);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
  gtk_box_pack_start (GTK_BOX (main_vbox), frame, TRUE, TRUE, 0);
  gtk_widget_show (frame);

  preview = gimp_preview_area_new ();
  gtk_widget_set_size_request (preview, PREVIEW_WIDTH, PREVIEW_HEIGHT);
  gtk_container_add (GTK_CONTAINER (frame), preview);
  g_object_set_data (G_OBJECT (preview), "rpPtr", rpPtr);
  rpPtr->enable_preview = TRUE;
  gtk_widget_show (preview);

  g_signal_connect_after (preview, "size-allocate",
                          G_CALLBACK (trigger_preview_update),
                          NULL);

  frame = gimp_frame_new (_("Image"));
  gtk_box_pack_start (GTK_BOX (main_vbox), frame, FALSE, FALSE, 0);
  gtk_widget_show (frame);

  table = gtk_table_new (4, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 6);
  gtk_table_set_row_spacings (GTK_TABLE (table), 4);
  gtk_container_add (GTK_CONTAINER (frame), table);
  gtk_widget_show (table);

  combo = gimp_int_combo_box_new (_("UYVY"),     RAW_UYVY,
                                  _("YUYV"),     RAW_YUYV,
                                  NULL);
  gimp_int_combo_box_set_active (GIMP_INT_COMBO_BOX (combo),
                                 rawConfigPtr->image_type);
  gimp_table_attach_aligned (GTK_TABLE (table), 0, 0,
                             _("Image _Type:"), 0.0, 0.5,
                             combo, 2, FALSE);

  g_signal_connect (combo, "changed",
                    G_CALLBACK (gimp_int_combo_box_get_active),
                    &rawConfigPtr->image_type);
  g_signal_connect_swapped (combo, "changed",
                            G_CALLBACK (trigger_preview_update),
                            preview);




  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 2,
                              _("Number of Processors:"), -1, 9,
                              rawConfigPtr->numProcessors, 1, MAX_SUPPORTED_THREADS, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Number of processors that shall be used "
                                "for loading the image in parallel"),
                              NULL);
  adjTab[ibck] = adj;
  bckValue[ibck] = gtk_adjustment_get_value(GTK_ADJUSTMENT(adj));
  ibck++;

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->numProcessors);




  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 3,
                              _("_Width of SubImage:"), -1, 9,
                              rawConfigPtr->sub_image_width, 1, maxEdgeLengthInPixel, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Width of the tiles in the sub image matrix "
                                "(or width for simple images with matrix 1x1)"),
                              NULL);
  adjTab[ibck] = adj;
  bckValue[ibck] = gtk_adjustment_get_value(GTK_ADJUSTMENT(adj));
  ibck++;

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_image_width);
  g_signal_connect_swapped (adj, "value-changed",
                            G_CALLBACK (trigger_preview_update),
                            preview);

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 4,
                              _("_Height of SubImage:"), -1, 9,
                              rawConfigPtr->sub_image_height, 1, maxEdgeLengthInPixel, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Height of the tiles in the sub image matrix "
                                "(or height for simple images with matrix 1x1)"),
                              NULL);
  adjTab[ibck] = adj;
  bckValue[ibck] = gtk_adjustment_get_value(GTK_ADJUSTMENT(adj));
  ibck++;

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_image_height);
  g_signal_connect_swapped (adj, "value-changed",
                            G_CALLBACK (trigger_preview_update),
                            preview);

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 5,
                              _("SubImagesX:"), -1, 9,
                              rawConfigPtr->sub_images_x, 1, MAX_SUB_IMAGES_X, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Number of horizontal tiles in the sub image matrix "
                                "in MagicLantern typical HiRes images. (use 1 for simple image)"),
                              NULL);
  adjTab[ibck] = adj;
  bckValue[ibck] = gtk_adjustment_get_value(GTK_ADJUSTMENT(adj));
  ibck++;

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_images_x);
  g_signal_connect_swapped (adj, "value-changed",
                            G_CALLBACK (trigger_preview_update),
                            preview);

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 6,
                              _("SubImagesY:"), -1, 9,
                              rawConfigPtr->sub_images_y, 1, MAX_SUB_IMAGES_X, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Number of vertical tiles in the sub image matrix "
                                "in MagicLantern typical HiRes images. (use 1 for simple image)"),
                              NULL);
  adjTab[ibck] = adj;
  bckValue[ibck] = gtk_adjustment_get_value(GTK_ADJUSTMENT(adj));
  ibck++;

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_images_y);
  g_signal_connect_swapped (adj, "value-changed",
                            G_CALLBACK (trigger_preview_update),
                            preview);
 
  while (TRUE)
    {
      gtk_widget_show (dialog);
      response = gimp_dialog_run (GIMP_DIALOG (dialog));
    

      run = (response == GTK_RESPONSE_OK);
      
      if(response == LOCAL_RESPONSE_RESET)
        {
          gimp_int_combo_box_set_active (GIMP_INT_COMBO_BOX (combo),
                                 bck_image_type);
          for(ibck = 0; ibck < MAX_BCK_VALS; ibck++)
            {
              gtk_adjustment_set_value (GTK_ADJUSTMENT (adjTab[ibck]), bckValue[ibck]);
            }
          trigger_preview_update((GimpPreviewArea*)preview);
        }
      else
        {
          break;
        }
    }
  /* disable all further preview render attempts (via timer)
   * while detroying the dialog. 
   */
  rpPtr->enable_preview = FALSE;  
  
  gtk_widget_destroy (dialog);

  return run;

}  /* end load_dialog */


/* ----------------------------------
 * save_dialog
 * ----------------------------------
 */
static gboolean
save_dialog (RawConfig  *rawConfigPtr, const gchar *filename,
             gint32       image_id,
             gint32       drawable_id)
{
  GtkWidget *dialog;
  GtkObject *adj;
  GtkWidget *table;
  GtkWidget *main_vbox;
  GtkWidget *frame;
  GtkWidget *combo;
  gboolean   run;

  gimp_ui_init (PLUG_IN_BINARY, TRUE);

  dialog = gimp_dialog_new (_("Raw YUV-422 Image Save"), PLUG_IN_BINARY,
                            NULL, 0,
                            gimp_standard_help_func, SAVE_PROC,

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
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), main_vbox,
                      FALSE, FALSE, 0);
  gtk_widget_show (main_vbox);


  frame = gimp_frame_new (_("Image"));
  gtk_box_pack_start (GTK_BOX (main_vbox), frame, FALSE, FALSE, 0);
  gtk_widget_show (frame);


  table = gtk_table_new (4, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (table), 6);
  gtk_table_set_row_spacings (GTK_TABLE (table), 4);
  gtk_container_add (GTK_CONTAINER (frame), table);
  gtk_widget_show (table);


  combo = gimp_int_combo_box_new (_("UYVY"),     RAW_UYVY,
                                  _("YUYV"),     RAW_YUYV,
                                  NULL);
  gimp_int_combo_box_set_active (GIMP_INT_COMBO_BOX (combo),
                                 rawConfigPtr->image_type);
  gimp_table_attach_aligned (GTK_TABLE (table), 0, 0,
                             _("Image _Type:"), 0.0, 0.5,
                             combo, 2, FALSE);

  g_signal_connect (combo, "changed",
                    G_CALLBACK (gimp_int_combo_box_get_active),
                    &rawConfigPtr->image_type);



  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 2,
                              _("SubImagesX:"), -1, 9,
                              rawConfigPtr->sub_images_x, 1, MAX_SUB_IMAGES_X, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Number of horizontal tiles in the sub image matrix "
                                "in MagicLantern typical HiRes images. (use 1 for simple image)"),
                              NULL);

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_images_x);

  adj = gimp_scale_entry_new (GTK_TABLE (table), 0, 3,
                              _("SubImagesY:"), -1, 9,
                              rawConfigPtr->sub_images_y, 1, MAX_SUB_IMAGES_X, 1, 10, 0,
                              TRUE, 0.0, 0.0,
                              _("Number of vertical tiles in the sub image matrix "
                                "in MagicLantern typical HiRes images. (use 1 for simple image)"),
                              NULL);

  g_signal_connect (adj, "value-changed",
                    G_CALLBACK (gimp_int_adjustment_update),
                    &rawConfigPtr->sub_images_y);
 

  gtk_widget_show (dialog);

  run = (gimp_dialog_run (GIMP_DIALOG (dialog)) == GTK_RESPONSE_OK);

  gtk_widget_destroy (dialog);

  return run;

}  /* end save_dialog */
