#!/usr/bin/env python

import sys, os, re
from gimpfu import *

def tifgray(img, drawable, target="", subdirs=0, quit=1):
    src = os.path.abspath(img.filename)
    src = os.path.dirname(src)
    if target=="": target=src
    if subdirs: go_rek(src, target)
    else:go(src, target)
# endewhen 
    if quit: pdb.gimp_quit(1)

def go(src, target):
    #dir = os.basedir(src)
    for mf in os.listdir(src):
        srn = os.path.join(src, mf)
        dest_new = os.path.join(target, mf[:-4]+'_gray.tif')
        img = load(srn)
        if not img: continue            #dest_new = os.path.splitext(srn)[0] + '_1.tif'
        #print "Converting image..."
        else: work(img, dest_new)
        
def go_rek(src, target):
    # fuer jedes Bild im Verzeichnis:
    for dn, dns, f in os.walk(src):
        for mf in f:
            srn = os.path.join(dn, mf)
            dest_new = os.path.join(target, mf[:-4]+'_gray.tif')
            img = load(srn)
            if not img: continue
            #dest_new = os.path.splitext(srn)[0] + '_1.tif'
            else: work(img, dest_new)
            
def load(srn):
    if srn[-4:] == ".jpg" or srn[-4:] == ".JPG" or srn[-4:] == ".jpeg" or srn[-4:] == ".JPEG": return pdb.file_jpeg_load(srn, srn)
    elif srn[-4:] == ".png" or srn[-4:] == ".PNG": return pdb.file_png_load(srn, srn)
    else:
        if srn[-4:] == ".tif" or srn[-4:] == ".TIF"or srn[-4:] == ".tiff" or srn[-4:] == ".TIFF": return pdb.file_tiff_load(srn, srn)
        else: return 0

    #print "Converting image..."
def work(img, dest_new):
# 1. in graustufen umwandeln
    pdb.gimp_image_convert_grayscale(img)

# 2. unscharf maskieren
    pdb.plug_in_unsharp_mask(img, img.layers[0], 3.0, 0.30, 0)
# 3. anpassen der werte
    pdb.gimp_levels(img.layers[0], 0, 0, 255, 1.0, 25, 252)
    #print "Saving file %s..." % dest_new
# 4. speichern
    pdb.file_tiff_save(img, img.layers[0], dest_new, dest_new, 0)


register("batch_convert2tifgray",
        "Converts every jpg-, png- and tif-Image into grayscale-Tif's, use unsharp-mask and adjust levels for offset-printing; works also in Subdirectories",
        "Converts every jpg-, png- and tif-Image into grayscale-Tif's, use unsharp-mask and adjust levels for offset-printing; works also in Subdirectories",
        "oliver killgus", "oliver killgus", "2008",
        "<Image>/Filters/batch/convert2tifgray",
        "",  
        [ 
            (PF_STRING, "target", "Name targetdir!", ""),
            (PF_BOOL, "subdirs", "Include subdirs.", False),
            (PF_BOOL, "quit", "Quit app when finished.", True)
        ],
        [],
        tifgray)

main()
