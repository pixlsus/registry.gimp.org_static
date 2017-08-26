#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
Copyright: Alexander Hadjiivanov 2010
Licence: GPL
Installation: copy the "batchwork.py" file into your $HOME/.gimp-2.x/plug-ins directory.
On Linux and Mac OSX the file must be executable.
The scripts appear under the <Image>/Tools/Batchwork/ menu entry

Version 1.04
  In Watermark:
    - Added options to select the position and/or number of text lines in the bump map watermark script

Version 1.03
  In Watermark:
    - The text layer is now created directly in the original image, which is faster and saves memory
    - Moved the menu entry to "<Image>/Tools/Batch/Watermark"
    - Added a new type of watermarking which can be used in case the visual impact on the image
      must be minimal

* The bump map technique yields

Version 1.02
  In Watermark:
    - Function names and variable names cleaned up and made consistent
    - Moved the menu entry to "<Image>/Tools/Batch..." in order to bring all scripts under the same hood
    - Added a dropdown option for the file extension
    - Added a special case for "JPG"/"JPEG"
    - The glob is now case-insensitive
  Others:
    - Added a script for batch resizing which is aware of the image orientation

Version 1.01
  - Added an option for choosing the output directory in the Watermark script

Version 1.0
  - Initial release

'''

from gimpfu import *
import os, glob, re
# i18n
import gettext

'''
Batch watermarking with a bump map
Based on the iccii "Texturizer" script (ported from script-fu to python-fu and added a batch mode)
          WARNING
Please double-check whether you have selected the correct output directory. If the
input directory and the output directory are the same, the script will replace the
original files.
'''

locale_directory = gimp.locale_directory
gettext.install( "gimp20-batchwork", locale_directory, unicode=True )

BatchWmBump_help = _("Watermarking of multiple images with a bump map.")
BatchWmBump_description = BatchWmBump_help

def python_fu_batchwm_bump(
  indirectory,
  outdirectory,
  ext,
  angle,
  elevation,
  depth,
  waterlevel,
  ambient,
  repetitions,
  position,
  fontname,
  copyright):

  if os.path.exists( u''+indirectory ):
    if ext == 0:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Ee][Gg]"
      infilepathnames = glob.glob( inglobpattern )
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Gg]"
      infilepathnames.extend(glob.glob( inglobpattern ))
    elif ext==1:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Gg][Ii][Ff]"
      infilepathnames = glob.glob( inglobpattern )
    elif ext==2:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Pp][Nn][Gg]"
      infilepathnames = glob.glob( inglobpattern )

    if os.path.exists( u''+outdirectory ):
      if infilepathnames:
        messagebox = pdb.gimp_message_get_handler( )
        pdb.gimp_message_set_handler( 2 ) # send messages in error console
        # Start the process
        for infilepathname in infilepathnames:
          img = pdb.gimp_file_load( infilepathname, infilepathname )
          imglayer= img.layers[0]
          imgwidth = pdb.gimp_image_width(img)
          imgheight = pdb.gimp_image_height(img)

          # Process the files
          # Perform some initialisation procedures
          texture_layer = gimp.Layer(img, _("Texture"), imgwidth,imgheight,RGBA_IMAGE,100,NORMAL_MODE)
          pdb.gimp_image_undo_disable(img)
          pdb.gimp_image_add_layer(img,texture_layer, 0)
          pdb.gimp_drawable_fill(texture_layer, 3)
          # Initialise the font with size = 1px in order to avoid meetings with dragons
          font_size=1

          # Create the texture layer from the string containing the copyright text
          textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
          if imgwidth>imgheight:
            if repetitions==0:
              while textBoxWidth < 2*imgwidth/3:
                font_size=font_size + 1
                textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
              if position==0:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              elif position==1:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/2,copyright,0,True,(font_size - 1),PIXELS,fontname)
              elif position==2:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
            elif repetitions==1:
              while textBoxWidth < 3*imgwidth/5:
                font_size=font_size + 1
                textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/3,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
            elif repetitions==2:
              while textBoxWidth < imgwidth/3:
                font_size=font_size + 1
                textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/8,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,5*imgwidth/8,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/8,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,5*imgwidth/8,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
          else:
            while textBoxWidth < 4*imgwidth/5:
              font_size=font_size + 1
              textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
            if repetitions==0:
              if position==0:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              elif position==1:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/2,copyright,0,True,(font_size - 1),PIXELS,fontname)
              elif position==2:
                texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
            elif repetitions==1:
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
            elif repetitions==2:
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,imgheight/2,copyright,0,True,(font_size - 1),PIXELS,fontname)
              texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/6,5*imgheight/6,copyright,0,True,(font_size - 1),PIXELS,fontname)

          pdb.gimp_drawable_set_visible(imglayer,False)
          texture_layer=pdb.gimp_image_merge_visible_layers(img,CLIP_TO_IMAGE)
          pdb.gimp_drawable_set_visible(imglayer,True)
          pdb.plug_in_bump_map(img,imglayer,texture_layer,angle,elevation,depth, 0, 0, waterlevel, ambient, True, False, 0)
          pdb.gimp_image_remove_layer(img,texture_layer)
          outfilepathname=re.sub(u''+indirectory,u''+outdirectory,infilepathname)
          pdb.gimp_file_save(img,imglayer,outfilepathname,outfilepathname)
          pdb.gimp_image_delete(img)

        # Give some feedback
        pdb.gimp_message("Watermarking completed successfully!")
        pdb.gimp_message_set_handler( messagebox )
      else:
        pdb.gimp_message( _("There are no files in %s") %(indirectory ) )
    else:
      pdb.gimp_message( _("%s doesn't exist") %(outdirectory) )
  else:
    pdb.gimp_message( _("%s doesn't exist") %(indirectory) )

'''
Batch watermarking with a difference map
Creates a transparent layer with text, which is merged with the original image layer in
the "Soft light" mode. Depending on the opacity of the text layer, the visual impact on
the image is minimal (with the default settings, the watermark is practically
invisible and does not distract the eye even when applied to low-resolution images,
which can not be said for the bump map technique). The copyright text can be extracted
as follows by using the original non-watermarked image as a "key":

0*. Rescale the original ("key") image to the same resolution as the watermarked one
  (skip this step if they are the same size to begin with)
1. Paste the key image as a new layer into the watermarked image
2. Merge down the two layers in the "Difference" mode. What is left is the copyright
  text (plus some noise, depending on the file format)
3. The text is most probably still too faint to read, so boost the levels (Colours/Levels...)
  until the text becomes readable

          WARNING
Please double-check whether you have selected the correct output directory. If the
input directory and the output directory are the same, the script will replace the
original files.
'''

BatchWmDiff_help = _("Watermarking of multiple images with a difference map.")
BatchWmDiff_description = BatchWmDiff_help

def python_fu_batchwm_diff(
  indirectory,
  outdirectory,
  ext,
  opacity,
  fontname,
  copyright):

  if os.path.exists( u''+indirectory ):
    if ext == 0:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Ee][Gg]"
      infilepathnames = glob.glob( inglobpattern )
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Gg]"
      infilepathnames.extend(glob.glob( inglobpattern ))
    elif ext==1:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Gg][Ii][Ff]"
      infilepathnames = glob.glob( inglobpattern )
    elif ext==2:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Pp][Nn][Gg]"
      infilepathnames = glob.glob( inglobpattern )

    if os.path.exists( u''+outdirectory ):
      if infilepathnames:
        messagebox = pdb.gimp_message_get_handler( )
        pdb.gimp_message_set_handler( 2 ) # send messages in error console
        # Start the process
        for infilepathname in infilepathnames:
          img = pdb.gimp_file_load( infilepathname, infilepathname )
          imglayer= img.layers[0]
          imgwidth = pdb.gimp_image_width(img)
          imgheight = pdb.gimp_image_height(img)
          # Process the files
          # Perform some initialisation procedure
          texture_layer = gimp.Layer(img, _("Texture"), imgwidth,imgheight,RGBA_IMAGE,opacity,SOFTLIGHT_MODE)
          pdb.gimp_image_undo_disable(img)
          pdb.gimp_image_add_layer(img,texture_layer, 0)
          pdb.gimp_drawable_fill(texture_layer, 3)
          font_size=1

          # Create the texture layer from the string containing the copyright text
          textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
          if imgwidth>imgheight:

            while textBoxWidth < imgwidth/2:
              font_size=font_size + 1
              textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
            texture_layer=pdb.gimp_text_fontname( img,texture_layer,imgwidth/2,2*imgheight/3,copyright,0,True,(font_size - 1),PIXELS,fontname)
          else:
            while textBoxWidth < imgwidth:
              font_size=font_size + 1
              textBoxWidth,textBoxHeight,fontAscent,fontDescent = pdb.gimp_text_get_extents_fontname(copyright, font_size, PIXELS, fontname)
            texture_layer=pdb.gimp_text_fontname( img,texture_layer,0,2*imgheight/3,copyright,0,True,(font_size - 1),PIXELS,fontname)

          texture_layer=pdb.gimp_image_flatten(img)
          outfilepathname=re.sub(u''+indirectory,u''+outdirectory,infilepathname)
          pdb.gimp_file_save( img,texture_layer,outfilepathname,outfilepathname )
          pdb.gimp_image_delete(img)

        # Give some feedback
        pdb.gimp_message("Watermarking completed successfully!")
        pdb.gimp_message_set_handler( messagebox )
      else:
        pdb.gimp_message( _("There are no files in %s") %(indirectory ) )
    else:
      pdb.gimp_message( _("%s doesn't exist") %(outdirectory) )
  else:
    pdb.gimp_message( _("%s doesn't exist") %(indirectory) )


'''
Batch resizing of images. Automatically detects whether the orientation is landscape
or portrait and resizes the image accordingly. The only input is the new value for the
"long side" of the images, which can be either the width of the height. This ensures,
for example, that the resulting images will fit into a square <div></div> element with
a side equal to the new "long side".
'''

BatchResize_help = _("Watermarking of multiple images.")
BatchResize_description = BatchResize_help

def python_fu_batch_resize(
  indirectory,
  outdirectory,
  ext,
  new_long_side,
  interpolation):

  if os.path.exists( u''+indirectory ):
    if ext == 0:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Ee][Gg]"
      infilepathnames = glob.glob( inglobpattern )
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Jj][Pp][Gg]"
      infilepathnames.extend(glob.glob( inglobpattern ))
    elif ext==1:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Gg][Ii][Ff]"
      infilepathnames = glob.glob( inglobpattern )
    elif ext==2:
      inglobpattern = u''+indirectory + os.sep + '*.' + "[Pp][Nn][Gg]"
      infilepathnames = glob.glob( inglobpattern )

    if os.path.exists( u''+outdirectory ):
      if infilepathnames:
        messagebox = pdb.gimp_message_get_handler( )
        pdb.gimp_message_set_handler( 2 )

        # Start the process
        for infilepathname in infilepathnames:

          img = pdb.gimp_file_load( infilepathname, infilepathname )
          imglayer= img.layers[0]

          # Process the files
          imgwidth = pdb.gimp_image_width(img)
          imgheight = pdb.gimp_image_height(img)

          if imgwidth>imgheight:
            aspect_ratio=float(imgwidth)/float(imgheight)
            pdb.gimp_image_scale_full(img,new_long_side,int(round(new_long_side/aspect_ratio)),interpolation)
          else:
            aspect_ratio=float(imgheight)/float(imgwidth)
            pdb.gimp_image_scale_full(img,int(round(new_long_side/aspect_ratio)),new_long_side,interpolation)

          outfilepathname=re.sub(u''+indirectory,u''+outdirectory,infilepathname)
          pdb.gimp_file_save( img,imglayer,outfilepathname,outfilepathname )
          pdb.gimp_image_delete(img)

        # Give some feedback
        pdb.gimp_message("Resizing completed successfully!")
        pdb.gimp_message_set_handler( messagebox )
      else:
        pdb.gimp_message( _("There are no files in %s") %(indirectory ) )
    else:
      pdb.gimp_message( _("%s doesn't exist") %(outdirectory) )
  else:
    pdb.gimp_message( _("%s doesn't exist") %(indirectory) )


# Register all plugins

register(
   "python-fu-batchwm-bump",
   BatchWmBump_description,
   BatchWmBump_help,
   "Alexander Hadjiivanov",
   "GPL License",
   "2010",
   _("Bump map"),
   "",
   [
      (PF_DIRNAME, "indirectory", _("Input Directory"), os.getcwd() ),
      (PF_DIRNAME, "outdirectory", _("Output Directory"), os.getcwd() ),
      (PF_OPTION, "ext", _("Extension"), 0, ["JPEG", "GIF", "PNG"]),
      (PF_SPINNER, "angle", _("Angle"), 135, (0,360,1)),
      (PF_SPINNER, "elevation", _("Elevation"), 40.0, (0.5,90,0.5)),
      (PF_SPINNER, "depth", _("Depth"), 20, (1,65,1)),
      (PF_SPINNER, "waterlevel", _("Waterlevel"), 5, (0,255,1)),
      (PF_SPINNER, "ambient", _("Ambient"), 30, (0,255,1)),
    (PF_OPTION, "repetitions", _("Number of text lines"), 0, ["1","2","3 or 4 depending on orientation"]),
    (PF_OPTION, "position", _("Position"), 1, ["top","middle","bottom"]),
      (PF_FONT, "fontname", _("Font"), "Sans"),
      (PF_STRING, "copyright", _("Copyright info"), "© your_copyright_info"),
   ],
   [],
   python_fu_batchwm_bump,
   menu="<Image>/Tools/Batchwork/Watermark...",
   domain=( "gimp20-batchwork", locale_directory)
   )

register(
   "python-fu-batchwm-diff",
   BatchWmDiff_description,
   BatchWmDiff_help,
   "Alexander Hadjiivanov",
   "GPL License",
   "2010",
   _("Difference map"),
   "",
   [
      (PF_DIRNAME, "indirectory", _("Input Directory"), os.getcwd() ),
      (PF_DIRNAME, "outdirectory", _("Output Directory"), os.getcwd() ),
      (PF_OPTION, "ext", _("Extension"), 0, ["JPEG", "GIF", "PNG"]),
      (PF_SPINNER, "opacity", _("Opacity"), 5, (0,100,0.1)),
      (PF_FONT, "fontname", _("Font"), "Sans"),
      (PF_STRING, "copyright", _("Copyright info"), "© your_copyright_info"),
   ],
   [],
   python_fu_batchwm_diff,
   menu="<Image>/Tools/Batchwork/Watermark...",
   domain=( "gimp20-batchwork", locale_directory)
   )

register(
   "python-fu-batch-resize",
   BatchResize_description,
   BatchResize_help,
   "Alexander Hadjiivanov",
   "GPL License",
   "2010",
   _("Resize"),
   "",
   [
      (PF_DIRNAME, "indirectory", _("Input Directory"), os.getcwd() ),
      (PF_DIRNAME, "outdirectory", _("Output Directory"), os.getcwd() ),
      (PF_OPTION, "ext", _("Extension"), 0, ["JPEG", "GIF", "PNG"]),
      (PF_SPINNER, "new_long_side", _("New long side (px):"), 640, (10, 3000, 1)),
      (PF_OPTION, "interpolation", _("Interpolation"), 3, ["None", "Linear", "Cubic", "Lanczos"]),
   ],
   [],
   python_fu_batch_resize,
   menu="<Image>/Tools/Batchwork",
   domain=( "gimp20-batchwork", locale_directory)
   )


main()
