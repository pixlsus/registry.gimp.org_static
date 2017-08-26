#! /usr/bin/env python
#--------------------------------------------------------------
# webThumb - Python-fu script
#
#------------------ Modification History ----------------------
# Date        Author	Description
# 2009/01/06  SWheeler	Script baselined
# 2009/01/23  SWheeler  v0.9 - beta release
#--------------------------------------------------------------
import os
import fnmatch
from gimpfu import *
import logging

FORMAT = '%(lineno)d:%(levelname)s %(asctime)s : %(message)s'
logging.basicConfig(filename=r'\gimp_plugin.log', level=logging.INFO, format=FORMAT)
pdb = gimp.pdb

def webThumb(timg, tdrawable, dbgFlag, processDir, displayOn, maxPixels, filename):
  if processDir == 1:
    pathname = str(filename.rsplit('\\',1)[0])
    dirlist = os.listdir(pathname)
  else:
    dirlist = [filename]
  
  if dbgFlag == 1:
    logit = logging.getLogger('MyPlugin')
    logit.info('webThumb: start')
    logit.info('      timg: ' + str(timg))
    logit.info(' tdrawable: ' + str(tdrawable))
    logit.info('   dbgFlag: ' + str(dbgFlag))
    logit.info('processDir: ' + str(processDir))
    logit.info(' displayOn: ' + str(displayOn))
    logit.info('maxPixels: ' + str(maxPixels))
    logit.info('  filename: ' + filename)
    logit.info('  pathname: ' + pathname)
    logit.info('   dirlist: ' + str(dirlist))
    logit.info('       cwd: ' + str(os.getcwd()))
    os.chdir(str(pathname))    
    logit.info('       cwd: ' + str(os.getcwd()))
    
  # Load each file in turn
  # scale image based on maxPixels value
  # Save as both color and grayscale thumbnails
  for file in dirlist:
    if fnmatch.fnmatch(file, '*.jpg'):
      if dbgFlag == 1: logit.info('  filename: ' + file)
      
      img = pdb.gimp_file_load(pathname + '\\' + file, pathname + '\\' + file)
      img_width = pdb.gimp_image_width(img)
      img_height = pdb.gimp_image_height(img)
      
      if img_width > img_height:
        scaleval = (img_width * 1.0) / maxPixels
      else:
        scaleval = (img_height * 1.0) / maxPixels
        
      new_width = img_width / scaleval
      new_height = img_height / scaleval
  
      if dbgFlag == 1:
        logit.info(' img_width: ' + str(img_width))
        logit.info('img_height: ' + str(img_height))
        logit.info('  scaleval: ' + str(scaleval))
        logit.info(' new_width: ' + str(new_width))
        logit.info('new_height: ' + str(new_height))
        
      pdb.gimp_displays_flush

      if displayOn == 1: display = pdb.gimp_display_new(img)

      pdb.gimp_image_scale(img,new_width,new_height)
      pdb.gimp_image_flatten(img)
      drawable = pdb.gimp_image_get_active_layer(img)
      pdb.plug_in_sharpen(img,drawable,25)
      pdb.gimp_displays_flush()
      pdb.gimp_image_flatten(img)
      
      drawable = pdb.gimp_image_get_active_layer(img)
      pdb.file_jpeg_save(img,drawable,pathname + '\\th_' + file, pathname + '\\th_' + file, 0.30, 0, 1, 0, "", 0, 1, 0, 0);

      pdb.gimp_desaturate(drawable);
      pdb.file_jpeg_save(img,drawable,pathname + '\\thgs_' + file, pathname + '\\thgs_' + file, 0.30, 0, 1, 0, "", 0, 1, 0, 0);

      if displayOn == 1: pdb.gimp_display_delete(display)
      
      # end: if fnmatch.fnmatch(file, '*.jpg')
      
    # End: for file in dirlist:
    
  if dbgFlag == 1:
    logit.info('webThumb: end')

register (
  "webThumb",
  "Create Thumbnail of .jpg image File",
  "Creates a separate thumbnail of the selected image file(s), pre-pends 'th_' (color image) and 'thgs_' (grayscale image) to the filename." + \
  "  WebThumb takes maxPixels as the maximum size of the largest aspect of the image, and then sizes the opposite aspect to maintain the original ratio.\n\n" + \
  "dbgFlag: default=0, set to 1 to output debug messages to \gimp_plugin.log\n\n" + \
  "processDir: default=0, set to 1 to process all .jpg files in the directory that 'filename' exists in\n\n" + \
  "processDir: default=0, set to 1 to process all .jpg files in the directory that 'filename' exists in\n\n" + \
  "displayOn: default=0, set to 1 to have GIMP create visual windows of the work being done (if you have nothing better to do with your time :-} )\n\n" + \
  "maxPixels: default=256, set to the max pixel size for your thumbnail.",
  "Steven Wheeler; StevenWheel@gmail.com",
  "2009",
  "20090123",
  "<Image>/Filters/Python-Plugins/_WebThumb...",
  "",
  [
      (PF_INT, "dbgFlag", "Debug on (1)", 0),
      (PF_INT, "processDir", "Process dir (1)", 0),
      (PF_INT, "displayOn", "Display on (1)", 0),
      (PF_INT, "maxPixels", "MaxPixels (256)", 256),
      (PF_FILENAME, "filename", "Filename of the image to create thumbnail from", "")
  ],
  [],
  webThumb
  )

main()
