'''
Created on Apr 20, 2013
@author: github.com/pablojvarela

A GIMP Python plugin to create PNG thumbnails out of PDF files' first page.
This plug-in must be put in The GIMP's script directory (e.g., $HOME/.gimp-1.2/plugins).
For interactive invocation, run The GIMP and go to Tools -> PDF Cover Thumbnail...
New width and height are in pixels.

Licensed under GPL 2.0
'''


import os
from gimpfu import *

pdb = gimp.pdb

def thumb(img, imgdraw, pdf_folder, png_width, png_height):
    print "Hoa!"
    
    for root, dirs, files in os.walk(pdf_folder):
       print "walking %s" % pdf_folder
       for f in files:
           pdf = os.path.join(root, f)
           filename, ext = os.path.splitext(pdf)
           if (ext == '.pdf'):
               print "found PDF: %s" % pdf
               image, imagew, imageh =  pdb.file_pdf_load_thumb(pdf, png_height)
               pdb.gimp_image_scale(image, png_width, png_height)
               pdb.file_png_save(image, image.layers[0], filename+".png", filename +".png", 0, 9, 1, 1, 1, 1, 1)


register(
        "python_fu_pdf_thumbnail",
        "Make a thumbnail out of the first page in a PDF file",
        "Make a thumbnail out of the first page in a PDF file",
        "Pablo J. Varela",
        "Pablo J. Varela",
        "2013",
        "<Image>/Tools/PDF Cover Thumbnail...",
        "*",      # Alternately use RGB, RGB*, GRAY*, INDEXED etc.
        [
            (PF_STRING, "pdf_folder", "Folder to crawl for PDF files", "~/"),
            (PF_INT, "png_width", "Width of thumbnail (px)", 500),
            (PF_INT, "png_height", "Height of thumbnail (px)", 500),
        ],
        [],
        thumb)

main()
