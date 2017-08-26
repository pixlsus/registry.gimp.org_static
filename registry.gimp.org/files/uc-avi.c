/* uc-avi
 *
 * This is a plug-in for the GIMP 2.0
 *
 * Copyright (C) Reinhard Geisler
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License (version 2) as 
 * published by the Free Software Foundation.
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

/* This plug-in loads/saves uncompressed AVI in/from
 * multilayer images.
 */

/*
 * Installation:
 * gimptool --install uc-avi.c
 *
 * Enjoy!
 * 
 */

/*
 * Revision History
 *
 * v 0.6a import corrected for mplayer 1.0
 * v 0.6  first gimp-2.0 version
 *
 * v 0.4a import corrected for mplayer 1.0
 * v 0.4  release version, gimp 1.2/2.0 split point
 * v 0.3? test versions of: image comment, mencoder pipe import,
 *        import bugfixes, vertical flip negative height images,
 *        improved error/debugging messages
 * v 0.3  parasites, user interface, new chunk handling,
 *        mencoder import
 * v 0.2? more tolerance to messy AVIs
 * v 0.2  (hopefully) last fill byte bugfix, check stream number,
 *        added load to grayscale, added index chunk (since there
 *        is one special program of one well-known company which
 *        wants to have an index...)
 * v 0.1? several bugfixes, fun with fill bytes (lack of docu) :-(
 * v 0.1  first light ;-)
 */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <signal.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>


#define PLUG_IN_NAME    "uc-avi"
#define PLUG_IN_VERSION "uc-AVI 0.6a"
#define PLUG_IN_DATE    "4.2001-9.2004"


/******************************************************************************/
/* begin header file for future external codecs... (?)                        */
/******************************************************************************/

/* type of pointer to load & save handler */
typedef
gboolean tLoader(guchar *dest, guchar *source, gint32 chunk_length,
		 GimpImageType drawable_type);

typedef
gint32 tSaver(guchar *dest, guchar *source, GimpImageType drawable_type);

/* the codecs for uncompressed data */
static tLoader load_dib;
static tSaver  save_dib;

/* handler definition */
static struct
{
  guchar  token[5];
  tLoader *loader;
  tSaver  *saver;
} handler[]=
{
  {"DIB ",load_dib,save_dib},
  {"RGB ",load_dib,save_dib},
  {"RAW ",load_dib,save_dib},
  {"",NULL,NULL} /* no more entries */
};

/* AVI parameters */
typedef struct
{
  tLoader  *loader;
  tSaver   *saver;
  gboolean lsb_first;/* byte order (TRUE: Intel, FALSE: Motorola) */
  /* AVI header part */
  guint32  delay;     /* time delay between frames  */
  guint32  avirate;   /* avi data rate (byte/sec)   */
  guint32  reserve;   /* reserved                   */
  guint32  flags;     /* misc. Flags                */
  guint32  timescale; /*                            */
  guint32  pb_rate;   /* playback data rate         */
  /* stream header part */
  gchar    handler[5];/* handler identifier         */
  guint32  scale;     /* scale factor of framerate  */
  guint32  framerate; /* framerate                  */
  guint32  frames;    /* number of frames           */
  guint32  ssize;     /* sample size (0: different) */
  /* stream format part */
  guint32  width;     /* width of a frame           */
  guint32  height;    /* height of a frame          */
  gboolean flip;      /* vertical flip, if height<0 */
  guint16  planes;    /* planes per frame (1)       */
  guint16  bpp;       /* bit per pixel              */
  guint16  bypp;      /* byte per pixel             */
  guint32  rowsize;   /* byte per row               */
  gchar    compr[5];  /* compression identifier     */
  guint32  size;      /* size (byte) of a frame     */
  guint32  xppm;      /* X pels per meter           */
  guint32  yppm;      /* Y pels per meter           */
  guint32  colors;    /* number of colors           */
  guint32  icolors;   /* number of important colors */
  guchar   colormap[256*3]; /* colormap red/green/blue                 */
  gboolean wrong_index;     /* this indicates wrong colortable indices */
  gboolean gray_only;        /* colormap is gray? */
  gchar    *comment;  /* comment, UTF-8 code        */
  gboolean avih_ok;
  gboolean strh_ok;
  gboolean strf_ok;
  gboolean colormap_ok;
} tAviInfo;
extern tAviInfo avi_info;

/* end of header file *********************************************************/


/******************************************************************************/
/* mencoder stuff                                                             */
/******************************************************************************/
#ifndef UC_AVI_MENCODER
  #define UC_AVI_MENCODER "mencoder"
#endif

#define UC_AVI_MENCODER_BASICS " -really-quiet -nosound -ovc raw -vf flip,format=bgr24 "

#ifdef DEBUG
  #define UC_AVI_MENCODER_OPTIONS UC_AVI_MENCODER_BASICS
#else
  #define UC_AVI_MENCODER_OPTIONS UC_AVI_MENCODER_BASICS"1>/dev/null 2>/dev/null"
#endif

#ifndef UC_AVI_TMP_DIR
  #define UC_AVI_TMP_DIR "/tmp"
#endif

/* sometimes mencoder leaves the first dc chunk empty, so we need this patch: */
#define MENCODER_MIN_FRAMES 2

/* name for uncompressed file / pipe */
gchar *tmpfilename=NULL;
gboolean use_pipe=FALSE;


/******************************************************************************/
/* image comment constants                                                    */
/******************************************************************************/
#define DEFAULT_COMMENT ""        /* default text (UTF-8)              */
#define FIRST_CHUNK_LENGTH 0x800  /* length of complete leading stuff  */
#define COMMENT_MAX 0x500         /* mult. of 4, fits in 0x800-header  */


/******************************************************************************/
/* Prototypes                                                                 */
/******************************************************************************/

/* communication to the GIMP */
static void   query      (void);
static void   run(const gchar      *name,
                  gint              nparams,
                  const GimpParam  *param,
                  gint             *nreturn_vals,
                  GimpParam       **return_vals);

/* load and save functions */
static gint32 load_image(gchar  *filename, gboolean dialog);
static gint32 save_image(gchar  *filename,
			 gint32  image_ID,
			 gint32  drawable_ID);


/* user interface */
static gboolean load_dialog(void);
static gboolean save_dialog(gint32 drawable_ID);

/******************************************************************************/
/* Variables                                                                  */
/******************************************************************************/

/* plug-in info */
GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,  /* init_proc  */
  NULL,  /* quit_proc  */
  query, /* query_proc */
  run,   /* run_proc   */
};

/* AVI parameters */
tAviInfo avi_info;


/* uc-avi load values */
typedef struct
{
  gboolean to_rgb;      /* import 8 bit to RGB? */
  guint32  first_frame; /* first frame to load  */
  guint32  last_frame;  /* last frame to load   */
}tAviLoadVals;

static tAviLoadVals avi_load_vals =
{
  FALSE, /* import 8 bit to RGB? */
  0,     /* first frame to load  */
  0,     /* last frame to load   */
};


/* uc-avi save values */
typedef struct
{
  guint32  scale;     /* scale factor of framerate */
  guint32  framerate; /* framerate                 */
  gint32   bpp;       /* bit per pixel             */
}tAviSaveVals;

static tAviSaveVals avi_save_vals =
{
  1,  /* scale factor of framerate */
  25, /* framerate                 */
  24, /* bit per pixel             */
};


/* input buffer */
static gint32 input_length=0x10000;
static guchar *input=NULL;


/* run mode: working interactive or quiet? */
static GimpRunMode run_mode;

/* status of parameter input: if TRUE: use plug-in; if FALSE: cancel */
static gboolean parameter_ok=FALSE;

/* load dialog, tmp variables */
static gint32         load_to_rgb;
static GtkAdjustment *first_slider;
static GtkAdjustment *last_slider;

/* load dialog, image/import size */
static GtkWidget *import_size_label;
static GtkWidget *image_size_label;


/******************************************************************************/
/* procedures                                                                 */
/******************************************************************************/
MAIN()
     

static void query(void)
{
  static GimpParamDef load_args[]=
  {
    {GIMP_PDB_INT32, "run_mode",    "Interactive, non-interactive"},
    {GIMP_PDB_STRING,"filename",    "The name of the file to load"},
    {GIMP_PDB_STRING,"raw_filename","The name entered"}
  };
  static GimpParamDef load_return_vals[]=
  {
    {GIMP_PDB_IMAGE,"image","Output image" }
  };
  static gint nload_args       = sizeof(load_args)/sizeof(load_args[0]);
  static gint nload_return_vals=(sizeof(load_return_vals)/
				 sizeof(load_return_vals[0]));

  static GimpParamDef save_args[] =
  {
    {GIMP_PDB_INT32,   "run_mode", "Interactive, non-interactive"},
    {GIMP_PDB_IMAGE,   "image",    "Input image"},
    {GIMP_PDB_DRAWABLE,"drawable", "Drawable to save"},
    {GIMP_PDB_STRING,  "filename", "The name of the file to save the image in"},
    {GIMP_PDB_STRING,  "raw_filename", "The name entered"}
  };
  static gint nsave_args=sizeof(save_args)/sizeof(save_args[0]);

  gimp_install_procedure("file_uc-avi_load",
                         "loading an AVI film",
                         "This plug-in loads uncompressed AVI files and "
			 "stores the video stream into a multilayer image. "
			 "If a stream name is present, it is taken as the "
			 "gimp-comment. Audio streams are discarded. "
			 "The frame range to load can be selected. ",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Load>/uncompressed AVI",
                         NULL,
                         GIMP_PLUGIN,
                         nload_args, nload_return_vals,
			 load_args, load_return_vals);
  
  gimp_install_procedure("file_uc-avi_save",
                         "saving an AVI film",
                         "This plug-in saves a multilayer image into "
			 "an uncompressed AVI file. "
			 "The gimp-comment can be edited and is taken as "
			 "the stream name.",
                         "Reinhard Geisler",
                         "Reinhard Geisler",
                         PLUG_IN_DATE,
                         "<Save>/uncompressed AVI",
                         "RGB*,GRAY*,INDEXED*",
                         GIMP_PLUGIN,
			 nsave_args, 0,
			 save_args, NULL);

  gimp_register_magic_load_handler("file_uc-avi_load","avi","","7,string,AVI ");
  gimp_register_save_handler      ("file_uc-avi_save","avi","");
}


/******************************************************************************/
static void run(const gchar *name, gint nparams, const GimpParam *param,
		gint *nreturn_vals, GimpParam **return_vals)
{
  static GimpParam  values[2];
  GimpPDBStatusType status=GIMP_PDB_SUCCESS;
  gint32        image_ID;
  gint32        drawable_ID;
  GimpParasite *parasite;

  *nreturn_vals=1;
  *return_vals =values;
  values[0].type         =GIMP_PDB_STATUS;
  values[0].data.d_status=GIMP_PDB_EXECUTION_ERROR;
  
  avi_info.comment=NULL;
  run_mode=param[0].data.d_int32;

  if(strcmp(name,"file_uc-avi_load")==0)
  {
    avi_load_vals.to_rgb     =FALSE;
    avi_load_vals.first_frame=0;
    avi_load_vals.last_frame =0;

    switch(run_mode)
    {
      case GIMP_RUN_WITH_LAST_VALS:
	/* try to get last values... but this is useless anyway. */
	gimp_get_data("file_uc-avi_load",&avi_load_vals);
      case GIMP_RUN_INTERACTIVE:
      case GIMP_RUN_NONINTERACTIVE:
	break;
      default:
	status=GIMP_PDB_CALLING_ERROR;
    }
    if(status==GIMP_PDB_SUCCESS)
    {
      image_ID=load_image(param[1].data.d_string,
			  run_mode==GIMP_RUN_INTERACTIVE?TRUE:FALSE);
      if(image_ID!=-1)
      {
	gimp_set_data("file_uc-avi_load",
		      &avi_load_vals,sizeof(avi_load_vals));
	if(avi_info.comment!=NULL)
	  gimp_image_attach_new_parasite(image_ID,
	    "gimp-comment",0,strlen(avi_info.comment)+1,avi_info.comment);
	gimp_image_attach_new_parasite(image_ID,
	  "uc-avi-save-options",0,sizeof(avi_save_vals),&avi_save_vals);
	*nreturn_vals=2;
	values[1].type        =GIMP_PDB_IMAGE;
	values[1].data.d_image=image_ID;
      }
      else status=parameter_ok?GIMP_PDB_EXECUTION_ERROR:GIMP_PDB_CANCEL;
    }
  }
  else if(strcmp(name,"file_uc-avi_save")==0)
  {
    image_ID   =param[1].data.d_int32;
    drawable_ID=param[2].data.d_int32;
    
    if(nparams!=5) status=GIMP_PDB_CALLING_ERROR;

    parasite=gimp_image_parasite_find(image_ID,"gimp-comment");
    if(parasite!=NULL)
    {
      avi_info.comment=g_strdup(parasite->data);
      gimp_parasite_free(parasite);
    }
    if(avi_info.comment==NULL) avi_info.comment=g_strdup(DEFAULT_COMMENT);

    switch(run_mode)
    {
      case GIMP_RUN_INTERACTIVE:
      case GIMP_RUN_WITH_LAST_VALS:
	/* try to get last values */
	gimp_get_data("file_uc-avi_save",&avi_save_vals);
      case GIMP_RUN_NONINTERACTIVE:

	/* try to get attached values */
	parasite=gimp_image_parasite_find(image_ID,"uc-avi-save-options");
	if(parasite!=NULL)
	{
	  avi_save_vals.scale=
	    ((tAviSaveVals*)parasite->data)->scale;
	  avi_save_vals.framerate=
	    ((tAviSaveVals*)parasite->data)->framerate;
	  avi_save_vals.bpp=
	    ((tAviSaveVals*)parasite->data)->bpp;
	  gimp_parasite_free(parasite);
	}
	if(run_mode==GIMP_RUN_INTERACTIVE)
        {
	  if(!save_dialog(drawable_ID)) status=GIMP_PDB_CANCEL;
	}
	break;
    }
    if(status==GIMP_PDB_SUCCESS)
    {
      if(save_image(param[3].data.d_string,image_ID,drawable_ID)==0)
      {
	gimp_set_data("file_uc-avi_save",
		      &avi_save_vals,sizeof(avi_save_vals));
	if(avi_info.comment!=NULL)
	{
	  gimp_image_parasite_detach(image_ID,"gimp-comment");
	  gimp_image_attach_new_parasite(image_ID,
            "gimp-comment",0,strlen(avi_info.comment)+1,avi_info.comment);
	}
	gimp_image_parasite_detach(image_ID,"uc-avi-save-options");
	gimp_image_attach_new_parasite(image_ID,
	  "uc-avi-save-options",0,sizeof(avi_save_vals),&avi_save_vals);
      }
      else status=GIMP_PDB_EXECUTION_ERROR;
    }
  }
  else status=GIMP_PDB_CALLING_ERROR;

  if(avi_info.comment!=NULL)
  {
    g_free(avi_info.comment);
    avi_info.comment=NULL;
  }

  if(input!=NULL)
  {
    g_free(input);
    input=NULL;
  }

  if(tmpfilename!=NULL)
  {
    remove(tmpfilename);
    g_free(tmpfilename);
    tmpfilename=NULL;
  }
  
  values[0].data.d_status=status;
}


/******************************************************************************/
/* helping hands for IO and interpreter                                       */
/******************************************************************************/


/* write 16 bit word **********************************************************/
static void put_word(guchar *dest, guint16 number)
{
  if(avi_info.lsb_first)
  {
    dest[0]=number&0xff;
    number>>=8;
    dest[1]=number&0xff;
  }
  else
  {
    dest[1]=number&0xff;
    number>>=8;
    dest[0]=number&0xff;
  }
}


/* write 32 bit long word *****************************************************/
static void put_long(guchar *dest, guint32 number)
{
  if(avi_info.lsb_first)
  {
    dest[0]=number&0xff;
    number>>=8;
    dest[1]=number&0xff;
    number>>=8;
    dest[2]=number&0xff;
    number>>=8;
    dest[3]=number&0xff;
  }
  else
  {    
    dest[3]=number&0xff;
    number>>=8;
    dest[2]=number&0xff;
    number>>=8;
    dest[1]=number&0xff;
    number>>=8;
    dest[0]=number&0xff;
  }
}


/* read 16 bit word ***********************************************************/
static guint16 get_word(guchar *source)
{
  return avi_info.lsb_first?
    source[0]+(guint16)0x100*source[1]:
    source[1]+(guint16)0x100*source[0];
}


/* read 32 bit long word ******************************************************/
static guint32 get_long(guchar *source)
{
  return avi_info.lsb_first?
    source[0]+
    0x100*(source[1]+0x100*(source[2]+((guint32)0x100*source[3]))):
    source[3]+
    0x100*(source[2]+0x100*(source[1]+((guint32)0x100*source[0])));
}


/* read avi header ************************************************************/
static void get_avih(guchar *buffer)
{
  if(get_long(buffer+0x04)<0x38) return;

  avi_info.delay    = get_long(buffer+0x08);
  avi_info.avirate  = get_long(buffer+0x0C);
  avi_info.reserve  = get_long(buffer+0x10);
  avi_info.flags    = get_long(buffer+0x14);
  avi_info.timescale= get_long(buffer+0x30);
  avi_info.pb_rate  = get_long(buffer+0x34);
  
  avi_info.avih_ok   =TRUE;
}


/* read video stream header ***************************************************/
static void get_strh(guchar *buffer)
{
  if(get_long(buffer+0x04)<0x30) return;

  memcpy(avi_info.handler,buffer+0x0C,4);
  avi_info.handler[4]=0;
  /*  TODO: check palette change flag! */
  avi_info.scale     = get_long(buffer+0x1C);
  avi_info.framerate = get_long(buffer+0x20);
  avi_info.frames    = get_long(buffer+0x28);
  avi_info.ssize     = get_long(buffer+0x34);

  avi_info.strh_ok   =TRUE;
}


/* read color map *************************************************************/
static gboolean get_colormap(guchar *source, guint32 position,
			     guint32 number)
{
  guchar *colormap;

  if(avi_info.bpp!=8) return TRUE;
  
  if(avi_info.colors==0  ||
     avi_info.colors>256 ||
     position+number> avi_info.colors) return FALSE;

  colormap=avi_info.colormap+position*3;
  for(; number>0; number--)
  {
    colormap[2]=*source++; /* B */
    colormap[1]=*source++; /* G */
    colormap[0]=*source++; /* R */
    source++;
    colormap+=3;
  }
  avi_info.gray_only=TRUE;
  colormap=avi_info.colormap;
  for(position=0; position<avi_info.colors; position++)
    if(colormap[0]!=colormap[1] || colormap[1]!=colormap[2])
    {
      avi_info.gray_only=FALSE;
      return TRUE;
    }
    else colormap+=3;
  return TRUE;
}


/* read video stream format ***************************************************/
static void get_strf(guchar *buffer)
{
  if(get_long(buffer+0x04)<0x28) return;
  
  avi_info.width     = get_long(buffer+0x0C);
  avi_info.height    = get_long(buffer+0x10);
  /* now a patch for really nasty images... */
  if(avi_info.height&0x80000000)
  {
    avi_info.height  =-avi_info.height;
    avi_info.flip    = TRUE;
  }
  else
    avi_info.flip    = FALSE;
  avi_info.planes    = get_word(buffer+0x14);
  avi_info.bpp       = get_word(buffer+0x16);
  memcpy(avi_info.compr,        buffer+0x18,4);
  avi_info.compr[4]  =0;
  avi_info.size      = get_long(buffer+0x1C);
  avi_info.xppm      = get_long(buffer+0x20);
  avi_info.yppm      = get_long(buffer+0x24);
  avi_info.colors    = get_long(buffer+0x28);
  avi_info.icolors   = get_long(buffer+0x2C);
  if(get_colormap(buffer+0x30,0,avi_info.colors)) avi_info.colormap_ok=TRUE;
  avi_info.bypp      = (avi_info.bpp+7)/8;
  if(avi_info.bypp!=1) avi_load_vals.to_rgb=TRUE;
  avi_info.rowsize   = ((avi_info.width*avi_info.bypp+3)/4)*4;
  
  avi_info.strf_ok   =TRUE;
}


/* read stream name ***********************************************************/
static void get_strn(guchar *buffer)
{
  gsize read, written;

  if(avi_info.comment!=NULL) g_free(avi_info.comment);
  avi_info.comment=g_locale_to_utf8(buffer+0x08,get_long(buffer+0x04),
                                    &read,&written,NULL);
}


/* get next chunk from file ***************************************************/
static gint32 next_chunk(FILE *fd)
{
  gint32 chunk_length=0;
  guchar store[8];

  /* init buffer? */
  if(input==NULL) input=g_malloc(input_length+8);
  if(fd   ==NULL) return -1;


  /* get identifier & length */
  if(fread(input,1,8,fd)!=8) return -1;
  chunk_length=get_long(input+4);
#ifdef DEBUG
  fprintf(stderr,"uc-avi: chunk %.4s, length 0x%.8x\n",input,chunk_length);
  fflush(stderr);
#endif
  if(chunk_length<0)  return -1;
  if(chunk_length==0) return  0;

  /* ...is this a list? */
  if(strncmp(input,"LIST",4)==0)
  {
    if(fread(input+8,1,4,fd)!=4) return -1;
#ifdef DEBUG
    fprintf(stderr,"uc-avi: LIST type is %.4s.\n",input+8);
    fflush(stderr);
#endif
    return 4;
  }

  /* enlarge buffer? */
  if(chunk_length>input_length) 
  {
#ifdef DEBUG
    fprintf(stderr,"uc-avi: enlarging buffer to 0x%.8x+8 bytes...\n",
	    chunk_length);
    fflush(stderr);
#endif
    memcpy(store,input,8);
    g_free(input);
    input_length=chunk_length;
    if((input=g_malloc(input_length+8))==NULL) return -1;
    memcpy(input,store,8);
  }

  /* get chunk */
  if(fread(input+8,1,chunk_length,fd)!=chunk_length)
  {
#ifdef DEBUG
    fprintf(stderr,"uc-avi: chunk NOT LOADED.\n");
    fflush(stderr);
#endif
    return -1;
  } 
  if(chunk_length%2) fgetc(fd);
  if(feof(fd))
  {
#ifdef DEBUG
    fprintf(stderr,"uc-avi: chunk NOT LOADED.\n");
    fflush(stderr);
#endif
    return -1;
  } 
  
#ifdef DEBUG
  fprintf(stderr,"uc-avi: chunk loaded.\n");
  fflush(stderr);
#endif
  return chunk_length;
}


/* close file, free buffer and report errors **********************************/
static gint32 cleanup(FILE *fd, gpointer buffer, const gchar *text, ...)
{
  va_list arg;
  
  va_start(arg,text);
  if(text!=NULL && run_mode==GIMP_RUN_INTERACTIVE)
  {
    if     (strstr(text,"%s")!=NULL) g_message(text,va_arg(arg,gchar*));
    else if(strstr(text,"%d")!=NULL) g_message(text,va_arg(arg,gint));
    else                             g_message(text);
  }
  if(fd!=NULL)     fclose(fd);
  if(buffer!=NULL) g_free(buffer);
  return -1;
}


/* import avi and position file to movi LIST **********************************/
static FILE *open_import_movi(gchar *infilename, guint32 last_frame)
{
  guint32 n, frames;
  gchar *import_call;
  gchar *import_pipe;
  FILE *fd;

  if(tmpfilename==NULL) return NULL;
  remove(tmpfilename);

  frames=last_frame+1;
  if(frames<MENCODER_MIN_FRAMES) frames=MENCODER_MIN_FRAMES;

#ifdef DEBUG
  fprintf(stderr,use_pipe?
	  "uc-avi: trying pipe import... ":
	  "uc-avi: trying file import... ");  
  fflush(stderr);
#endif

  if(use_pipe)
  {
    import_call=g_strdup_printf(UC_AVI_MENCODER UC_AVI_MENCODER_OPTIONS
				" -frames %d -o \'%s\' \'%s\' &",
				frames,tmpfilename,infilename);
    import_pipe=g_strdup_printf("mkfifo \'%s\'",tmpfilename); 
    if(system(import_pipe)!=0)
    {
#ifdef DEBUG
      fprintf(stderr,"failed to create pipe.\n");
      fflush(stderr);
#endif
      g_free(import_pipe);
      return NULL;
    }
    g_free(import_pipe);
  }
  else
    import_call=g_strdup_printf(UC_AVI_MENCODER UC_AVI_MENCODER_OPTIONS
				" -frames %d -o \'%s\' \'%s\'  ",
				frames,tmpfilename,infilename);
  
  /* call mencoder... */
  if(system(import_call)!=0)
  {
#ifdef DEBUG
    fprintf(stderr,"failed.\n");
    fflush(stderr);
#endif
    remove(tmpfilename);
    g_free(import_call);
    return NULL;
  }
#ifdef DEBUG
  fprintf(stderr,"ok.\n");
  fflush(stderr);
#endif
  g_free(import_call);

  if((fd=fopen(tmpfilename,"rb"))==NULL) 
  {
    remove(tmpfilename);
    return NULL;
  }

  for(n=0; TRUE; n++)
  {
    if(n>=input_length || fread(input+n,1,1,fd)!=1)
    {
      fclose(fd);
      remove(tmpfilename);
      return NULL;
    }
    if(input[n]=='i' && n>=0x0b &&
       memcmp(input+n-0x0b,"LIST",4)==0 &&
       memcmp(input+n-0x03,"movi",4)==0) break;
  }
#ifdef DEBUG
  fprintf(stderr,"uc-avi: movi LIST found.\n");
  fflush(stderr);
#endif
  return fd;
}


/* find & check first dc chunk, close file ************************************/
gboolean check_dc_chunk(FILE *fd)
{
  gint32 chunk_length;

  /* find the dc chunk... */
  do
  {
    chunk_length=next_chunk(fd);
    if(chunk_length<0)
    {
#ifdef DEBUG
      fprintf(stderr,"uc-avi: no dc chunk found.\n");
      fflush(stderr);
#endif
      fclose(fd);
      return FALSE;
    }
    if(chunk_length==0)
    {
      /* shift frame range to get at least one frame */
      if(run_mode==GIMP_RUN_NONINTERACTIVE)
      {
	avi_load_vals.first_frame++;
	avi_load_vals.last_frame++;
      }
#ifdef DEBUG
      fprintf(stderr,"uc-avi: ignored chunk of size 0.\n");
      fflush(stderr);
#endif
      continue;
    }
  }
  while(memcmp(input+0x02,"dc",2)!=0 || chunk_length==0);

  fclose(fd);

  /* ...and check the chunk size */
  if(get_long(input+0x04)<avi_info.width*avi_info.height*3) return FALSE;
#ifdef DEBUG
  fprintf(stderr,"uc-avi: dc chunk looks good.\n");
  fflush(stderr);
#endif
  return TRUE;
}


/* try mencoder import for unsupported codecs... ******************************/
static gboolean try_mencoder(gchar *infilename)
{
  FILE  *fd;

  if(tmpfilename==NULL) return FALSE;

  /* check if it works at all...  */
  /* open file, position at movi LIST */
  use_pipe=FALSE;
  if((fd=open_import_movi(infilename,0))==NULL) return FALSE;
  if(!check_dc_chunk(fd)) return FALSE;
  remove(tmpfilename);

  /* fine, this worked. now check if a pipe works...       */
  /* mencoder is not designed to work with output pipes,   */
  /* but since we already know the headers we'll just try. */
  use_pipe=TRUE;
  if((fd=open_import_movi(infilename,0))!=NULL)
    if(check_dc_chunk(fd)) return TRUE;

  /* no pipe. ok, then back to the file :-( */
  /* open file, position at movi LIST */
#ifdef DEBUG
  fprintf(stderr,"uc-avi: pipe import failed.\n");
  fflush(stderr);
#endif
  use_pipe=FALSE;
  if((fd=open_import_movi(infilename,0))==NULL) return FALSE;
#ifdef DEBUG
  fprintf(stderr,"uc-avi: file reimport ok.\n");
  fflush(stderr);
#endif
  return check_dc_chunk(fd);
}


/******************************************************************************/
/* load image                                                                 */
/******************************************************************************/
static gint32 load_image(gchar *filename, gboolean dialog)
{
  FILE         *fd;
  gboolean      video_found;
  guchar       *dest;
  gchar        *message;
  guint32       frame;
  gint32        stream_number, video_number=-1;
  gint32        chunk_length, n;
  gint32        list_length=0;
  gint32        image_ID=0;
  gint32        layer_ID=0;
  GimpDrawable *drawable;
  GimpImageType drawable_type;
  GimpPixelRgn  region;

  avi_info.wrong_index=FALSE;
  avi_info.gray_only  =FALSE;
  avi_info.avih_ok    =FALSE;
  avi_info.strh_ok    =FALSE;
  avi_info.strf_ok    =FALSE;
  avi_info.colormap_ok=FALSE;
  video_found         =FALSE;

  /* open the file */
  if((fd=fopen(filename,"rb"))==NULL)
    return cleanup(fd,NULL,"uc-AVI: Can't open file \'%s\'.",filename);

  /* init input buffer */
  next_chunk(NULL);

  /* RIFF is intel, RIFX is motorola byte order */
  if(fread(input,1,0x0c,fd)!=0x0c)
    return cleanup(fd,NULL,"uc-AVI: Premature end of file.");
  if(strncmp(input,"RIFF",4)==0)
    avi_info.lsb_first=TRUE;
  else if(strncmp(input,"RIFX",4)==0)
    avi_info.lsb_first=FALSE;
  else return cleanup(fd,NULL,"uc-AVI: No \'RIFF\' file.");
  if(strncmp(input+8,"AVI ",4))
    return cleanup(fd,NULL,"uc-AVI: No \'AVI \' file.");

  /* identify header list */
  if((chunk_length=next_chunk(fd))<0 ||
     strncmp(input,"LIST",4) || strncmp(input+8,"hdrl",4))
    return cleanup(fd,NULL,"uc-AVI: No \'hdrl\'.");

  while(strncmp(input,"LIST",4) || strncmp(input+0x08,"movi",4))
  {
    if((chunk_length=next_chunk(fd))<0)
      return cleanup(fd,NULL,"uc-AVI: Premature end of file in \'hdrl\'.");
    if(strncmp(input,"avih",4)==0)
    {
      get_avih(input);
    }
    else if(strncmp(input,"LIST",4)==0)
    {
      list_length=get_long(input+0x04)+0x08;
      if(     strncmp(input+0x08,"movi",4)==0) break;
      else if(strncmp(input+0x08,"strl",4)==0 && !video_found)
      {
	/* read stream list */
	while(list_length>chunk_length+0x08)
	{
	  list_length-=chunk_length+0x08;

	  /* get chunk */
	  if((chunk_length=next_chunk(fd))<0)
	    return cleanup(fd,NULL,
			   "uc-AVI: Premature end of file in \'strl\'.");

	  /* identify chunk */
	  if(     strncmp(input,"strh",4)==0)
	  {
	    get_strh(input);
	    if(strncmp(input+0x08,"vids",4)==0) video_found=TRUE;
	  }
	  else if(strncmp(input,"strf",4)==0)
	    get_strf(input);
	  else if(strncmp(input,"strn",4)==0)
	    get_strn(input);
	  else if(strncmp(input,"LIST",4)==0 && strncmp(input+0x08,"movi",4)==0)
	    break;
	  /* ignore unknown chunks and JUNK chunks */
	}
	/* valid only for video stream */
	if(!video_found)
	  avi_info.strh_ok=avi_info.strf_ok=avi_info.colormap_ok=FALSE;
      }
      else
      {
        /* ignore unknown LIST */
        while(list_length>chunk_length+0x08)
	{
	  list_length-=chunk_length+0x08;
	  /* get chunk */
	  if((chunk_length=next_chunk(fd))<0)
	    return cleanup(fd,NULL,
			   "uc-AVI: Premature end of file in unknown LIST.");
	}
      }
    }
    /* ignore unknown chunks and JUNK chunks */
  }

  /* ...now we are in the movi list */  

  if(!video_found)
    return cleanup(fd,NULL,"uc-AVI: No video stream found.");
  if(!avi_info.avih_ok)
    return cleanup(fd,NULL,"uc-AVI: No/corrupted avi header.");
  if(!avi_info.strh_ok)
    return cleanup(fd,NULL,"uc-AVI: No/corrupted video stream header.");
  if(!avi_info.strf_ok)
    return cleanup(fd,NULL,"uc-AVI: No/corrupted video stream format.");
  if(!avi_info.colormap_ok)
    return cleanup(fd,NULL,"uc-AVI: No/corrupted color map.");

  /* patch handler <-> compression confusion */
  if(memcmp(avi_info.handler,"\0\0\0\0",4)==0)
  {
    if(memcmp(avi_info.compr,  "\0\0\0\0",4)==0)
      memcpy(avi_info.handler,"DIB ",4);
    else
      memcpy(avi_info.handler,avi_info.compr,4);
  }

  /* identify handler */
  avi_info.loader=NULL;
  for(n=0; handler[n].loader!=NULL || handler[n].saver!=NULL; n++)
    if(memcmp(avi_info.handler,handler[n].token,4)==0)
    {
      avi_info.loader=handler[n].loader;
      avi_info.saver=handler[n].saver;
    }

  /* codec supported? mencoder import? */
  if(avi_info.loader==NULL)
  {
    fclose(fd);
    fd=NULL;
    /* try to open in tmp dir... */
    tmpfilename=g_strdup_printf("%s/gimp-ucavi-%d",
				UC_AVI_TMP_DIR,(gint)getpid());
    if(!try_mencoder(filename))
    {
      if(tmpfilename!=NULL) remove(tmpfilename);
      /* try to open in image dir.... */
      tmpfilename=g_strdup_printf("%s-ucavi-%d",
				  filename,(gint)getpid());
      if(!try_mencoder(filename))
	return cleanup(NULL,NULL,
		       "uc-AVI: Load handler \'%s\' not supported",
		       avi_info.handler);
    }
  }

  /* load dialog */
  if(dialog && !load_dialog()) return cleanup(fd,NULL,NULL);

  /* remember gint==int, so frames may overflow... */
  if(avi_load_vals.last_frame-avi_load_vals.first_frame>=INT_MAX)
  {
    cleanup(NULL,NULL,"uc-AVI Warning: Gimp can only handle "
	    "%d layers on this system.",(gint)INT_MAX);
    avi_load_vals.last_frame=avi_load_vals.first_frame+INT_MAX-1;
  }

  /* mencoder import */
  if(avi_info.loader==NULL)
  {
    if(run_mode!=GIMP_RUN_NONINTERACTIVE)
      gimp_progress_init("Trying mencoder to decode avi... please wait.");
    if((fd=open_import_movi(filename,avi_load_vals.last_frame))==NULL)
      return cleanup(NULL,NULL,"uc-AVI: Mencoder import failed.");

    /* patch info variables */
    memcpy(avi_info.handler,"DIB ",4);
    memcpy(avi_info.compr,"\0\0\0\0",4);
    avi_info.loader     =load_dib;
    avi_info.frames     =avi_load_vals.last_frame+1;
    avi_info.bypp       =3;
    avi_info.bpp        =24;
    /* remember: mencoder doesn't generate fill bytes */
    avi_info.rowsize    =avi_info.width*avi_info.bypp;
    avi_load_vals.to_rgb=TRUE;
  }

  if(run_mode!=GIMP_RUN_NONINTERACTIVE)
  {
    message=g_strdup_printf("Loading %s:", filename);
    gimp_progress_init(message);
    g_free(message);
  }

  /* we are still in the movi list... */
  /* create new image */
  if(avi_load_vals.to_rgb)
  {
    drawable_type=GIMP_RGB_IMAGE;
    image_ID=gimp_image_new(avi_info.width,avi_info.height,GIMP_RGB);
  }
  else if (avi_info.gray_only)
  {
    drawable_type=GIMP_GRAY_IMAGE;
    image_ID=gimp_image_new(avi_info.width,avi_info.height,GIMP_GRAY);
  }
  else
  {
    drawable_type=GIMP_INDEXED_IMAGE;
    image_ID=gimp_image_new(avi_info.width,avi_info.height,GIMP_INDEXED);
    gimp_image_set_cmap(image_ID,avi_info.colormap,avi_info.colors);
  }
  gimp_image_set_filename(image_ID,filename);

  /* create layer buffer */
  dest=g_malloc((avi_load_vals.to_rgb?3:1)*avi_info.width*avi_info.height);

  /* loop through frames */
  for(frame=0; frame<=avi_load_vals.last_frame ; frame++)
  {
    if(avi_load_vals.last_frame && (run_mode!=GIMP_RUN_NONINTERACTIVE))
      gimp_progress_update((gdouble)frame/avi_load_vals.last_frame);

    /* find video frame... */
    video_found=FALSE;
    while(!video_found)
    {
      /* get next chunk */
      if((chunk_length=next_chunk(fd))<0)
	return cleanup(fd,dest,
		       "uc-AVI: Premature end of file in \'movi\' LIST "
		       "before frame %d.",frame);

      /* data in rec chunk? */
      if(strncmp(input,"LIST",4)==0)
      {
	if(strncmp(input+8,"rec ",4))
	  return cleanup(fd,dest,
			 "uc-AVI: Unknown LIST no in \'movi\' LIST "
			 "before frame %d.",frame);
	else if((chunk_length=next_chunk(fd))<0)
	  return cleanup(fd,dest,
			 "uc-AVI: Premature end of file in \'rec \' LIST "
			 "before frame %d.",frame);
      }

      /* idx chunk is the end of movi, so we have a problem... */
      if(strncmp(input+2,"idx1",4)==0)
	 return cleanup(fd,dest,
			"uc-AVI: premature \'idx1\' chunk "
			"before frame %d.",frame);

      /* ignore all non-video streams */
      if(strncmp(input+2,"db",2) &&
	 strncmp(input+2,"dc",2) &&
	 strncmp(input+2,"pc",2))
	continue;

      /* get stream number */
      stream_number=get_word(input);

      /* first frame? stream number ok? */
      if(video_number<0) video_number=stream_number;
      else if(video_number!=stream_number) continue;

      /* change palette? */
      if(strncmp(input+0x02,"pc",2)==0)
      {
	if(!get_colormap(input+0x0c,input[0x09],input[0x0a]))
	  return cleanup(fd,dest,"uc-AVI: "
			 "Wrong color number for 8 bit color depth"
			 "in pc chunk before frame %d.",frame);
	if(!avi_load_vals.to_rgb)
	  return cleanup(fd,dest,"uc-AVI: Colormap change not "
			 "supported for grayscale/indexed.\n"
			 "Use import to RGB.");
	gimp_image_set_cmap(image_ID,avi_info.colormap,avi_info.colors);
      }
      if(strncmp(input+2,"db",2)==0 ||
	 strncmp(input+2,"dc",2)==0)
	video_found=TRUE;
    }

    /* only load selected range */
    if(frame<avi_load_vals.first_frame) continue;

    /* now a special patch for weird avis */
    if(chunk_length==0)
    {
      cleanup(NULL,NULL,
	      "uc-AVI Warning: Ignored frame %d (size 0).",frame);
      continue;
    }

    /* decode frame data */
    if(!avi_info.loader(dest,input+8,chunk_length,drawable_type))
      return cleanup(fd,dest,"uc-AVI: Error while decoding frame %d",frame);

    /* create new layer */
    if(avi_info.framerate && avi_info.scale)
      sprintf(input,"frame %d, %.2f sec",frame,
	      (double)frame/avi_info.framerate*avi_info.scale);
    else
      sprintf(input,"frame %d",frame);

    layer_ID=gimp_layer_new(image_ID,
				input,
				avi_info.width, avi_info.height,
				drawable_type,
				100,
				GIMP_NORMAL_MODE);
    gimp_image_add_layer(image_ID, layer_ID, 0);
    drawable=gimp_drawable_get(layer_ID);
    gimp_pixel_rgn_init(&region,drawable,
			0,0,avi_info.width,avi_info.height,
			TRUE,FALSE);

    /* copy frame into new layer */
    gimp_pixel_rgn_set_rect(&region,dest,
			    0,0,avi_info.width,avi_info.height);
    
    /* Tidy up dirty drawable */
    gimp_drawable_flush(drawable);
    gimp_drawable_update(drawable->drawable_id,0,0,
			 avi_info.width,avi_info.height);
    gimp_drawable_detach(drawable);    
  }

  /* some idx1 chunks may follow...    */
  /* ...anyway we're done, forget them */

  /* tell the user about this messy AVI we just loaded... */
  if(avi_info.wrong_index)
    cleanup(NULL,NULL,"uc-AVI Warning: Index outside color table..."
	    "I'll take index 0 instead.");

  fclose(fd);
  g_free(dest);

  avi_save_vals.scale    =avi_info.scale;
  avi_save_vals.framerate=avi_info.framerate;
  avi_save_vals.bpp      =avi_info.bpp;

  gimp_displays_flush();
  
  return image_ID;
}


/******************************************************************************/
/* save image                                                                 */
/******************************************************************************/
static gint32 save_image(gchar  *filename,
			 gint32  image_ID,
			 gint32  drawable_ID)
{
  FILE *fd;
  guchar *source, *dest, *index, *pointer, *colormap, *comment_plain;
  guchar *ptr_hdrl, *ptr_strl;

  GimpImageType drawable_type;
  GimpDrawable *drawable;
  GimpPixelRgn  region;

  gsize   read, written;
  gint    offset_x, offset_y, number, *layers, comment_length=0;
  gint32  n, chunk_length, head_length, movi_length;


  /* set avi_info */
  avi_info.lsb_first = TRUE;
  avi_info.delay     = 1e6*avi_save_vals.scale/avi_save_vals.framerate+0.5;
  avi_info.avirate   = 0;
  avi_info.reserve   = 0;
  avi_info.flags     = 0x10;       /* there is an idx1 chunk! */
  avi_info.timescale = 0;
  avi_info.pb_rate   = 0;
  memcpy(avi_info.handler,"DIB ",5);
  avi_info.scale     = avi_save_vals.scale;
  avi_info.framerate = avi_save_vals.framerate;
  avi_info.ssize     = 0;
  avi_info.planes    = 1;
  memcpy(avi_info.compr,"\0\0\0\0",5);
  avi_info.xppm      = 0;
  avi_info.yppm      = 0;
  avi_info.colors    = 0;
  avi_info.icolors   = 0;

  /* set color depth, generate color map */
  switch(gimp_drawable_type(drawable_ID))
    {
    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:
      avi_info.bpp = 8;
      avi_info.bypp= 1;
      avi_info.colors=256;
      colormap=avi_info.colormap;
      for(n=0; n<avi_info.colors; n++)
      {
	*colormap++=n;
	*colormap++=n;
	*colormap++=n;
      }
      break;
    case GIMP_INDEXED_IMAGE:
    case GIMP_INDEXEDA_IMAGE:
      avi_info.bpp = 8;
      avi_info.bypp= 1;
      colormap=gimp_image_get_cmap(image_ID, &number);
      avi_info.colors=number;
      memcpy(avi_info.colormap,colormap,avi_info.colors*3);
      g_free(colormap);
      break;
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      if(avi_save_vals.bpp<15) avi_save_vals.bpp=24;
      avi_info.bypp= (avi_save_vals.bpp+7)/8;
      avi_info.bpp = avi_info.bypp*8;
#ifdef DEBUG
      fprintf(stderr,"uc-avi: %d bypp, %d bpp\n",avi_info.bypp,avi_info.bpp);
      fflush(stderr);
#endif
      break;
    }

  /* get image parameters */
  layers=gimp_image_get_layers(image_ID, &number);
  drawable=gimp_drawable_get(layers[0]);
  avi_info.frames    = number;
  avi_info.width     = drawable->width;
  avi_info.height    = drawable->height;
  avi_info.rowsize   = ((avi_info.width*avi_info.bypp+3)/4)*4;
  avi_info.size      = avi_info.height*avi_info.rowsize;
  gimp_drawable_detach(drawable);

  /* check layer sizes */
  for(number=avi_info.frames-1; number>0; number--)
  {
    drawable=gimp_drawable_get(layers[number]);
    if(avi_info.width!=drawable->width || avi_info.height!=drawable->height)
      return cleanup(NULL,NULL,"uc-AVI: All layers must be of the same size.");
    gimp_drawable_offsets(layers[number], &offset_x, &offset_y);
    if(offset_x || offset_y)
      cleanup(NULL,NULL,
	      "uc-AVI Warning: Shifting layer to offset 0.");
    gimp_drawable_detach(drawable);
  }

  /* init progress */
  if(run_mode!=GIMP_RUN_NONINTERACTIVE)
  {
    dest=g_strdup_printf("Saving %s:",filename);
    gimp_progress_init(dest);
    g_free(dest);
  }

  /* open the file */
  if((fd=fopen(filename,"wb"))==NULL)
    return cleanup(NULL,NULL,"uc-AVI: Can't open file %s.",filename);

  /* get buffer, write leading stuff... */
  dest=g_malloc(FIRST_CHUNK_LENGTH);
  head_length=0;

  /* write magic */
  pointer=dest+head_length;
  memcpy(  pointer+0x00,avi_info.lsb_first?"RIFF":"RIFX",4);
  /* length is just a first guess... */
  put_long(pointer+0x04,FIRST_CHUNK_LENGTH-8+
	   avi_info.frames*(avi_info.size+8));
  memcpy(  pointer+0x08,"AVI ",4          );
  head_length+=0x0c;
  
  /* write header list */
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"LIST",4          );
  ptr_hdrl=pointer+0x04;
  memcpy(  pointer+0x08,"hdrl",4          );
  head_length+=0x0c;
  
  /* write avi header */
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"avih",4          );
  put_long(pointer+0x04,0x38              );
  put_long(pointer+0x08,avi_info.delay    );
  put_long(pointer+0x0C,avi_info.avirate  );
  put_long(pointer+0x10,avi_info.reserve  );
  put_long(pointer+0x14,avi_info.flags    );
  put_long(pointer+0x18,avi_info.frames   );
  put_long(pointer+0x1c,0                 ); /* init frame */
  put_long(pointer+0x20,1                 ); /* number of streams */
  put_long(pointer+0x24,avi_info.size     );
  put_long(pointer+0x28,avi_info.width    );
  put_long(pointer+0x2c,avi_info.height   );
  put_long(pointer+0x30,avi_info.timescale);
  put_long(pointer+0x34,avi_info.pb_rate  );
  put_long(pointer+0x38,0                 ); /* start time */
  put_long(pointer+0x3c,0                 ); /* chunk size */
  head_length+=0x40;

  /* write stream header list */    
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"LIST",4          );
  ptr_strl=pointer+0x04;
  memcpy(  pointer+0x08,"strl",4          );
  head_length+=0x0c;

  /* write stream header */    
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"strh",4          );
  put_long(pointer+0x04,0x40              );
  memcpy(  pointer+0x08,"vids",4          );
  memcpy(  pointer+0x0C,avi_info.handler,4);
  put_long(pointer+0x10,0                 ); /* stream flags */
  put_long(pointer+0x14,avi_info.reserve  );
  put_long(pointer+0x18,0                 ); /* init frame */
  put_long(pointer+0x1c,avi_info.scale    ); /* scale */
  put_long(pointer+0x20,avi_info.framerate);
  put_long(pointer+0x24,0                 ); /* start time */
  put_long(pointer+0x28,avi_info.frames   );
  put_long(pointer+0x2c,avi_info.size     );
  put_long(pointer+0x30,0                 ); /* quality */
  put_long(pointer+0x34,0                 ); /* sample size */
  put_long(pointer+0x38,0                 ); /* ??? */
  put_long(pointer+0x3c,0                 ); /* ??? */
  put_long(pointer+0x40,avi_info.width    );
  put_long(pointer+0x44,avi_info.height   );
  head_length+=0x48;
  
  /* write stream format */    
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"strf",4          );
  put_long(pointer+0x04,0x28+avi_info.colors*4);
  put_long(pointer+0x08,0x28              );
  put_long(pointer+0x0c,avi_info.width    );
  put_long(pointer+0x10,avi_info.height   );
  put_word(pointer+0x14,1                 ); /* planes */
  put_word(pointer+0x16,avi_info.bpp      );
  memcpy(  pointer+0x18,avi_info.compr,  4);
  put_long(pointer+0x1c,avi_info.size     );
  put_long(pointer+0x20,avi_info.xppm     );
  put_long(pointer+0x24,avi_info.yppm     );
  put_long(pointer+0x28,avi_info.colors   );
  put_long(pointer+0x2c,avi_info.icolors  );
  head_length+=0x30;

  /* write color map */
  pointer=dest+head_length;
  colormap=avi_info.colormap;
  for(n=0; n<avi_info.colors; n++)
  {
    *pointer++=colormap[2]; /* B */
    *pointer++=colormap[1]; /* G */
    *pointer++=colormap[0]; /* R */
    *pointer++=0;
    colormap+=3;
  }
  head_length+=avi_info.colors*4;

  /* write stream name */
  if(avi_info.comment!=NULL)
  {
    comment_plain=g_locale_from_utf8(avi_info.comment,-1,&read,&written,NULL);
    comment_length=((strlen(comment_plain)+1+3)/4)*4;
    if(comment_length>COMMENT_MAX)
    {
      comment_length=COMMENT_MAX;
      cleanup(NULL,NULL,
	      "uc-AVI Warning: Image Comment too long. "
	      "Truncated to %d Characters.",(gint)(COMMENT_MAX-1));
      comment_plain[comment_length-1]=0;
    }

    pointer=dest+head_length;
    memcpy(  pointer+0x00,"strn",4        );
    put_long(pointer+0x04,  comment_length);
    memset(  pointer+0x08,0,comment_length);
    strcpy(  pointer+0x08,comment_plain);
    head_length+=0x8+comment_length;
    g_free(comment_plain);
  }
  
  /* update LIST length */
  pointer=dest+head_length;
  put_long(ptr_hdrl, pointer-ptr_hdrl-4);
  put_long(ptr_strl, pointer-ptr_strl-4);

  /* write JUNK */
  memcpy(  pointer+0x00,"JUNK",4          );
  chunk_length=FIRST_CHUNK_LENGTH-head_length-0x14;
  put_long(pointer+0x04,  chunk_length);
  memset(  pointer+0x08,0,chunk_length);
  head_length+=0x08+chunk_length;

  /* write movi LIST */
  pointer=dest+head_length;
  memcpy(  pointer+0x00,"LIST",4          );
  /* length is just a first guess... */
  put_long(pointer+0x04,(avi_info.size+8)*avi_info.frames);
  memcpy(  pointer+0x08,"movi",4          );
  head_length+=0x0c;
  movi_length=4;

  /* ...and write it to the file. */
  if(fwrite(dest,1,head_length,fd)!=head_length)
    return cleanup(fd,dest,"uc-AVI: Error writing file %s.",filename);

  /* allocate buffers */
  g_free(dest);
  source=g_malloc(avi_info.width*avi_info.height*4+
		  avi_info.size+0x08+
		  avi_info.frames*0x10+0x08);
  dest=source+avi_info.width*avi_info.height*4;
  index=dest+avi_info.size+0x08;
  memcpy(index,"idx1",4);
  put_long(index+0x04,avi_info.frames*0x10);
  pointer=index+0x08;

  /* identify handler */
  for(n=0; handler[n].loader!=NULL || handler[n].saver!=NULL ; n++)
    if(memcmp(avi_info.handler,handler[n].token,4)==0)
    {
      avi_info.loader=handler[n].loader;
      avi_info.saver=handler[n].saver;
    }
  if(avi_info.saver==NULL)
    return cleanup(fd,source,"uc-AVI: Save handler \"%s\" not supported",
		   avi_info.handler);

  /* loop through layers */
  for(number=avi_info.frames-1; number>=0; number--)
  {
    /* show progress */
    if(avi_info.frames && (run_mode!=GIMP_RUN_NONINTERACTIVE))
      gimp_progress_update(
	(gdouble)(avi_info.frames-number)/avi_info.frames);
    
    /* select layer */
    drawable_type=gimp_drawable_type(layers[number]);
    drawable=gimp_drawable_get(layers[number]);

    /* initialize source region */
    gimp_pixel_rgn_init(&region,drawable,0,0,
			drawable->width,drawable->height,FALSE,FALSE);

    /* get layer */
    gimp_pixel_rgn_get_rect(&region,source,0,0,
			    drawable->width,drawable->height);

    /* code frame data */
    chunk_length=avi_info.saver(dest,source,drawable_type);
    if(chunk_length%2) chunk_length++;
    if(chunk_length==0)
      return cleanup(fd,source,"uc-AVI: Error while coding data");

    /* write index entry */
    memcpy  (pointer+0x00,dest,0x04);
    put_long(pointer+0x04,0x10);
    put_long(pointer+0x08,movi_length);
    memcpy(pointer+0x0c,dest+0x04,0x04);
    pointer+=0x10;

    /* write data to file */
    if(fwrite(dest,1,chunk_length+8,fd)==chunk_length+8)
      movi_length+=chunk_length+8;
    else
      return cleanup(fd,source,"uc-AVI: Error writing file %s.",filename);
  }

  /* write index to file*/
  if(fwrite(index,1,
	    avi_info.frames*0x10+0x08,fd)!=avi_info.frames*0x10+0x08)
    return cleanup(fd,source,"uc-AVI: Error writing file %s.",filename);

  /* write the real file length to the file... */
  if(fseek(fd,4,SEEK_SET)==0)
  {
    put_long(index,
	     head_length-8-4+movi_length+avi_info.frames*0x10+0x08);
    if(fwrite(index,1,4,fd)!=4)
      return cleanup(fd,source,"uc-AVI: Error writing file %s.",filename);
  }
  /* write the real movi length to the file... */
  if(fseek(fd,head_length-8,SEEK_SET)==0)
  {
    put_long(dest,movi_length);
    if(fwrite(dest,1,4,fd)!=4)
      return cleanup(fd,source,"uc-AVI: Error writing file %s.",filename);
  }

  g_free(source);
  fclose(fd);
  return 0;
}


/******************************************************************************/
/* user interface                                                             */
/******************************************************************************/


/* load dialog callback *******************************************************/
static void load_dialog_callback(GtkWidget *widget,
                                 gint response_id, gpointer data)
{
  if(response_id==GTK_RESPONSE_OK)
  {
    if(avi_info.bypp==1) avi_load_vals.to_rgb=load_to_rgb?TRUE:FALSE;
    avi_load_vals.first_frame=first_slider->value;
    avi_load_vals.last_frame =last_slider->value;
#ifdef DEBUG
    fprintf(stderr,"uc-avi: loading frames %d to %d...\n",
           avi_load_vals.first_frame,avi_load_vals.last_frame);
#endif    
    parameter_ok=TRUE;
  }
  gtk_widget_destroy(widget);
}


/* save dialog callback *******************************************************/
static void save_dialog_callback(GtkWidget *widget,
                                 gint response_id, GtkTextBuffer *textbuffer)
{
  GtkTextIter  start;
  GtkTextIter  stop;

  if(response_id==GTK_RESPONSE_OK)
  {
    /* get image comment */
    if(avi_info.comment!=NULL) g_free(avi_info.comment);
    gtk_text_buffer_get_bounds(textbuffer,&start,&stop);
    avi_info.comment=gtk_text_buffer_get_text(textbuffer,&start,&stop,FALSE);
    parameter_ok=TRUE;
  }
  gtk_widget_destroy(widget);
}


/* load updates ***************************************************************/
void size_update(GtkWidget *size_label, gchar *size_text, guint32 size_num)
{
  gchar      text[101];

  if     ( size_num        <(guint32)0x1000)
    sprintf(text,size_text,size_num,"Bytes");
  else if((size_num/=0x400)<(guint32)0x1000)
    sprintf(text,size_text,size_num,"KB");
  else if((size_num/=0x400)<(guint32)0x1000)
    sprintf(text,size_text,size_num,"MB");
  else
    sprintf(text,size_text,size_num/(guint32)0x400,"GB");
  gtk_label_set_text(GTK_LABEL(size_label),text);
}

void import_size_update(void)
{
  if(first_slider   ==NULL) return;
  if(avi_info.loader!=NULL) return;
  size_update(import_size_label,"This will create a temporary file of %d %s.",
	      3*avi_info.width*avi_info.height*
	      (guint32)(last_slider->value+1));
}

void image_size_update(void)
{
  if(first_slider==NULL) return;
  size_update(image_size_label,"Resulting Image Size: %d %s",
	      (load_to_rgb?3:1)*avi_info.width*avi_info.height*
	      (guint32)(last_slider->value-first_slider->value+1));
}

void drawable_type_update(GtkWidget *widget, gint32 *value)
{
  gimp_menu_item_update(widget,value);
  load_to_rgb=*value;
  image_size_update();
}

static void range_slider_update(GtkAdjustment *one_slider,
				GtkAdjustment *other_slider)
{
  if(first_slider->value>last_slider->value)
    gtk_adjustment_set_value(one_slider,other_slider->value);
  if(first_slider!=NULL)
  {
    import_size_update();
    image_size_update();
  }
}


/* save updates ***************************************************************/
void framerate_update(GtkAdjustment *adjustment, gpointer data)
{
  avi_save_vals.framerate=adjustment->value;
  if(avi_save_vals.framerate!=adjustment->value)
  {
    avi_save_vals.framerate=adjustment->value*100+0.5;
    avi_save_vals.scale=100;
  }
  else avi_save_vals.scale=1;
}

void colordepth_update(GtkWidget *widget, gint32 *value)
{
  const guint16 colordepth[]={16,24,32};
  gimp_menu_item_update(widget,value);
  avi_save_vals.bpp=colordepth[*value];
#ifdef DEBUG
  fprintf(stderr,"ucavi: colordepth %d (%d bit).\n",*value,colordepth[*value]);
  fflush(stderr);
#endif
}


/* create menu ****************************************************************/
GtkWidget *create_menu(GtkWidget *table, gint x, gint y,
		       const gchar *title,
		       const gchar *name[],
		       gint32 *result,
		       GtkSignalFunc update_function)
{
  GtkWidget *label;
  GtkWidget *selection;

  if(title!=NULL)
  {
    label=gtk_label_new(title);
    gtk_table_attach_defaults(GTK_TABLE(table),label,1,2,y,y+1);
    gtk_misc_set_alignment(GTK_MISC(label),1.0,0.5);
    gtk_widget_show(label);
    x++;
  }
  selection=
    gimp_option_menu_new2(FALSE,GTK_SIGNAL_FUNC(update_function),
                          result,
                          (gpointer) *result,
                          name[0],(gpointer) 0, NULL,
                          name[1],(gpointer) 1, NULL,
                          name[2],(gpointer) 2, NULL,
                          NULL);
  gtk_table_attach(GTK_TABLE(table),selection,x,x+1,y,y+1,
                   GTK_FILL,0,0,0);

  gtk_widget_show(selection);

  return selection;
}


/* create label ***************************************************************/
GtkWidget *create_label(gchar *string, gdouble num, GtkWidget *table,
			gint left, gint right, gint row)
{
  GtkWidget *label;
  gchar     *string_2;

  if(num>=0)
  {
    string_2=g_strdup_printf("%g",num);
    label=gtk_label_new(string_2);
    g_free(string_2);
    gtk_table_attach(GTK_TABLE(table),label,right,right+1,row,row+1,
		     GTK_FILL,GTK_FILL,0,0);
    gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
    gtk_widget_show(label);    
  }
  label=gtk_label_new(string);
  gtk_table_attach(GTK_TABLE(table),label,left,right,row,row+1,
		   GTK_FILL,GTK_FILL,0,0);
  gtk_misc_set_alignment(GTK_MISC(label),0.0,0.5);
  gtk_widget_show(label);
  return label;
}


/******************************************************************************/
/* load dialog                                                                */
/******************************************************************************/
static gboolean load_dialog(void)
{
  const  gchar *name_grayscale[]={"Grayscale","RGB", NULL};
  const  gchar *name_indexed[]  ={"Indexed",  "RGB", NULL};
  gint   row=0;
  gchar  *string;

  GtkWidget *dlg;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *window;
  GtkWidget *view;
  GtkTextBuffer *textbuffer;

  gchar **argv;
  gint argc;
  
  argc   =1;
  argv   =g_new(gchar*,1);
  argv[0]=g_strdup(PLUG_IN_NAME);
  
  gtk_init(&argc,&argv);
  gtk_rc_parse(gimp_gtkrc());
  
  /* open new dialog */
  dlg=gimp_dialog_new("Load AVI, "PLUG_IN_VERSION,"uc-avi",
                       NULL,0,
                       gimp_standard_help_func,"plug-in-uc-avi",
                       GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,
                       GTK_STOCK_OK,    GTK_RESPONSE_OK,
                       NULL);
  g_signal_connect(dlg,"response",G_CALLBACK(load_dialog_callback),NULL);
  g_signal_connect(dlg,"destroy", G_CALLBACK(gtk_main_quit),       NULL);
  
  /* image info */
  frame=gtk_frame_new("Image Information");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  table=gtk_table_new(4,4,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_table_set_col_spacing(GTK_TABLE(table),1,50);
  gtk_table_set_col_spacing(GTK_TABLE(table),3,50);
  gtk_container_add(GTK_CONTAINER(frame),table);
  row=0;

  /* frames, framerate, size */
  create_label("Frames:"     ,avi_info.frames,table,0,1,row);
  create_label("Size:"       ,-1.0           ,table,2,3,row);
  string=g_strdup_printf("%d x %d",avi_info.width,avi_info.height);
  create_label(string        ,-1.0           ,table,3,4,row);
  g_free(string);
  row++;
  create_label("Frame Rate:" ,(gdouble)avi_info.framerate/avi_info.scale,
	                                     table,0,1,row);
  create_label("Color Depth:",avi_info.bpp   ,table,2,3,row);
  gtk_table_set_row_spacing(GTK_TABLE(table),row,15);
  row++;

  gtk_widget_show(table);
  gtk_widget_show(frame);

  /* image comment */
  frame=gtk_frame_new("Image Comment (Stream Name)");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  gtk_widget_show(frame);

  window=gtk_scrolled_window_new(NULL,NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_container_set_border_width(GTK_CONTAINER(window),4);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(window),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_container_add(GTK_CONTAINER(frame),window);

  textbuffer=gtk_text_buffer_new(NULL);
  view=gtk_text_view_new_with_buffer(textbuffer);
  gtk_container_add(GTK_CONTAINER(window),view);
  
  if(avi_info.comment!=NULL)
    gtk_text_buffer_set_text(textbuffer,avi_info.comment,-1);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(view),FALSE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(view),GTK_WRAP_WORD);
  g_object_unref(textbuffer);

  gtk_widget_show(view);
  gtk_widget_show(window);

  /*  parameter settings  */
  frame=gtk_frame_new("Load Options");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  
  table=gtk_table_new(10,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_container_add(GTK_CONTAINER(frame),table);
  row=0;

  /* codec supported? */
  if(avi_info.loader==NULL)
  {
    create_label(avi_info.handler,-1,table,0,1,row);
    create_label("*hmmm* I don't know this codec... but mencoder seems to.",
		 -1,table,1,2,row++);
    create_label("I'll try mencoder to import it.",-1,table,1,2,row++);

    /* import size info, only for file import */
    import_size_label=create_label(NULL,-1,table,1,2,row);
    if(use_pipe) gtk_widget_hide(import_size_label);
    gtk_table_set_row_spacing(GTK_TABLE(table),row,15);
    row++;
  }

  /* colordepth menue, only for 8 bit Images */
  load_to_rgb=1;
  if(avi_info.bypp==1)
  {
    load_to_rgb=0;
    create_menu(table,1,row,"Import 8-Bit AVI to:",
		avi_info.gray_only?name_grayscale:name_indexed,
		&load_to_rgb,
		(GtkSignalFunc)drawable_type_update);
    gtk_table_set_row_spacing(GTK_TABLE(table),row++,15);
  }
  
  /* range sliders */
  create_label("Load Frame Range:",-1,table,1,2,row++);

  first_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,row++,"First Frame:",150,0,
			 0,0,avi_info.frames-1,
			 1.0,10.0,0,TRUE,0,0,NULL,NULL);
  last_slider=(GtkAdjustment*)
    gimp_scale_entry_new(GTK_TABLE(table),0,row,"Last Frame:",150,0,
			 avi_info.frames-1,0,avi_info.frames-1,
			 1.0,10.0,0,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(first_slider),"value_changed",
		     GTK_SIGNAL_FUNC(range_slider_update),
		     last_slider);
  gtk_signal_connect(GTK_OBJECT(last_slider),"value_changed",
		     GTK_SIGNAL_FUNC(range_slider_update),
		     first_slider);
  gtk_table_set_row_spacing(GTK_TABLE(table),row++,5);

  /* image size info */
  image_size_label=create_label(NULL,-1,table,1,2,row);
  import_size_update();
  image_size_update();
  gtk_widget_show(table);
  gtk_widget_show(frame);

  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}


/******************************************************************************/
/* save dialog                                                                */
/******************************************************************************/
static gboolean save_dialog(gint32 drawable_ID)
{
  const gchar *name_colordepth[]={"16 Bit","24 Bit", "32 Bit", NULL};
  gint32 default_colordepth;
  
  GtkWidget *dlg;
  GtkWidget *label;
  GtkObject *slider;
  GtkWidget *table;
  GtkWidget *frame;
  GtkWidget *view;
  GtkWidget *window;
  GtkTextBuffer *textbuffer;

  gchar **argv;
  gint argc;
  
  argc   =1;
  argv   =g_new(gchar*,1);
  argv[0]=g_strdup(PLUG_IN_NAME);
  
  gtk_init(&argc,&argv);
  gtk_rc_parse(gimp_gtkrc());
  
  /* open new dialog */
  dlg=gimp_dialog_new("Save AVI, "PLUG_IN_VERSION,"uc-avi",
                       NULL,0,
                       gimp_standard_help_func,"plug-in-uc-avi",
                       GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,
                       GTK_STOCK_OK,    GTK_RESPONSE_OK,
                       NULL);
  g_signal_connect(dlg,"destroy",G_CALLBACK(gtk_main_quit),NULL);

  /*  parameter settings  */
  frame=gtk_frame_new("Save Options");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  
  table=gtk_table_new(2,3,FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(table),4);
  gtk_table_set_col_spacings(GTK_TABLE(table),5);
  gtk_container_add(GTK_CONTAINER(frame),table);

  /* framerate slider */
  slider=gimp_scale_entry_new(GTK_TABLE(table),0,0,"Framerate:",150,0,
    (gdouble)avi_save_vals.framerate/avi_save_vals.scale,
    1,100,1.0,5.0,2,TRUE,0,0,NULL,NULL);
  gtk_signal_connect(GTK_OBJECT(slider),"value_changed",
                     GTK_SIGNAL_FUNC(framerate_update),NULL);
  gtk_table_set_row_spacing(GTK_TABLE(table),0,15);

  /* colordepth menue, only for 24 bit Images */
  switch(gimp_drawable_type(drawable_ID))
  {
    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:
    case GIMP_INDEXED_IMAGE:
    case GIMP_INDEXEDA_IMAGE:
      label=create_label("Color Depth:",8,table,1,2,1);
      gtk_misc_set_alignment(GTK_MISC(label),1.0,0.5);
      break;
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      default_colordepth=(avi_save_vals.bpp+7)/8-2;
      if(default_colordepth<0) default_colordepth=1;
      create_menu(table,1,1,"Color Depth:",
		  name_colordepth,&default_colordepth,
		  (GtkSignalFunc)colordepth_update);
  }
  gtk_widget_show(table);
  gtk_widget_show(frame);
  
  /* image comment */
  frame=gtk_frame_new("Image Comment (Stream Name)");
  gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(frame),6);
  gtk_box_pack_start_defaults(GTK_BOX(GTK_DIALOG(dlg)->vbox),frame);
  gtk_widget_show(frame);

  window=gtk_scrolled_window_new(NULL,NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_container_set_border_width(GTK_CONTAINER(window),4);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(window),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_container_add(GTK_CONTAINER(frame),window);

  textbuffer=gtk_text_buffer_new(NULL);
  view=gtk_text_view_new_with_buffer(textbuffer);
  gtk_container_add(GTK_CONTAINER(window),view);
  if(avi_info.comment!=NULL)
    gtk_text_buffer_set_text(textbuffer,avi_info.comment,-1);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(view),GTK_WRAP_WORD);
  g_object_unref (textbuffer);

  g_signal_connect(dlg,"response",G_CALLBACK(save_dialog_callback),textbuffer);

  gtk_widget_show(view);
  gtk_widget_show(window);

  gtk_widget_show(dlg);

  gtk_main();

  return parameter_ok;
}

/* end user interface *********************************************************/


/******************************************************************************/
/* begin codec file for future external codecs...                             */
/******************************************************************************/

/* headerfile         */
/* handler definition */

/* load handler for 'DIB ' ****************************************************/
static gboolean load_dib(guchar *dest, guchar *source, gint32 chunk_length, 
			 GimpImageType drawable_type)
{
  gint32 x, y, rowadd;
  gint R=2, G=1, B=0;
  guint16 word;
  guchar *colormap;
  

  if(memcmp(avi_info.compr,"DIB ",4) &&
     memcmp(avi_info.compr,"RGB ",4) &&
     memcmp(avi_info.compr,"RAW ",4) &&
     memcmp(avi_info.compr,"\0\0\0\0",4))
  {
    cleanup(NULL,"uc-AVI: Compression %s not supported",avi_info.compr);
    return FALSE;
  }

  if(avi_info.rowsize*avi_info.height!=chunk_length)
  {
    cleanup(NULL,NULL,"uc-AVI: Wrong buffer size (unsupported compression?)");
    return FALSE;
  }

  
  if(avi_info.flip)
  {
    rowadd=-avi_info.bypp*avi_info.width+avi_info.rowsize;
  }
  else
  {
    rowadd=-avi_info.bypp*avi_info.width-avi_info.rowsize;
    source+=(avi_info.height-1)*avi_info.rowsize;
  }

  for(y=0; y<avi_info.height; y++)
  {
    for(x=0; x<avi_info.width; x++)
    {
      switch(avi_info.bpp)
      {
	case 32:
	case 24:
	  *dest++=source[R];
	  *dest++=source[G];
	  *dest++=source[B];
	  break;
	case 15:
	case 16:
	  word=get_word(source);
	  *dest++=(word>>7) & 0x00f8; /* R */
	  *dest++=(word>>2) & 0x00f8; /* G */
	  *dest++=(word<<3) & 0x00f8; /* B */
	  break;
	case 8:
	  if(*source>=avi_info.colors)
	  {
	    avi_info.wrong_index=TRUE;
	    *source=0;
	  }
	  if(drawable_type==GIMP_RGB_IMAGE)
	  {
            colormap=avi_info.colormap+*source*3;
	    *dest++=*colormap++;
	    *dest++=*colormap++;
	    *dest++=*colormap++;
	  }
	  else *dest++=avi_info.gray_only?
		 avi_info.colormap[*source*3]:*source;
	  break;
	default:
	  cleanup(NULL,NULL,"uc-AVI: Color depth %d bit not supported",
		  (gint)avi_info.bpp);
	  return FALSE;
      }
      source+=avi_info.bypp;
    }
    source+=rowadd;
  }
  return TRUE;
}


/* save handler for 'DIB ' ****************************************************/
static gint32 save_dib(guchar *dest, guchar *source,
		       GimpImageType drawable_type)
{
  gint32 x, y, alpha=0;
  gint R=2, G=1, B=0, A=3;
  guint16 word;

  if(memcmp(avi_info.compr,"DIB ",4) &&
     memcmp(avi_info.compr,"RGB ",4) &&
     memcmp(avi_info.compr,"RAW ",4) &&
     memcmp(avi_info.compr,"\0\0\0\0",4))
  {
    cleanup(NULL,"uc-AVI: Compression %s not supported",avi_info.compr);
    return 0;
  }
  
  if(avi_info.rowsize*avi_info.height!=avi_info.size)
  {
    cleanup(NULL,NULL,"uc-AVI: Wrong buffer size (unsupported compression?)");
    return 0;
  }

  /* with alpha channel? */
  if(drawable_type==GIMP_RGBA_IMAGE  ||
     drawable_type==GIMP_GRAYA_IMAGE ||
     drawable_type==GIMP_INDEXEDA_IMAGE) alpha=1;

  /* set chunk identifier */
  memcpy(dest,"00db",4);
  put_long(dest+4,avi_info.size);

  memset(dest+8,0,avi_info.size);
  dest+=8+(avi_info.height-1)*avi_info.rowsize;

  for(y=0; y<avi_info.height; y++)
  {
    for(x=0; x<avi_info.width; x++)
    {
      switch(avi_info.bpp)
      {
	case 32:
	  dest[R]=*source++;
	  dest[G]=*source++;
	  dest[B]=*source++;
	  dest[A]=0;
	  break;
	case 24:
	  dest[R]=*source++;
	  dest[G]=*source++;
	  dest[B]=*source++;
	  break;
	case 15:
	case 16:
	  word =((guint16)*source++<<7) & 0x7c00; /* R */
	  word|=((guint16)*source++<<2) & 0x03e0; /* G */
	  word|=((guint16)*source++>>3) & 0x001f; /* B */
	  put_word(dest,word);
	  break;
	case 8:
	  *dest=*source++;
	  break;
	default:
	  cleanup(NULL,NULL,"uc-AVI: Color depth %d bit not supported",
		  (gint)avi_info.bpp);
	  return 0;
      }
      source+=alpha;
      dest+=avi_info.bypp;
    }
    dest-=avi_info.bypp*avi_info.width+avi_info.rowsize;  
  }
  return avi_info.size;
}

/* end of codec file **********************************************************/
