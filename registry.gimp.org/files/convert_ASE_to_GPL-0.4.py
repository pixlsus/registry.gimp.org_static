#!/usr/bin/env python
# Author: Chris Mohler
# Copyright 2008 Chris Mohler
# License: GPL v3
# Portions of this code were taken from easyrgb.com
# GIMP plugin to convert Kuler (ASE) palettes to GIMP (GPL) palettes

from gimpfu import *
import sys,  os,  re
from struct import unpack_from,  unpack
import StringIO

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

gimp_dir = gimp.directory # get GIMP directory
#print ("GIMP dir: " + gimp_dir)

pal_dir = os.path.join(gimp_dir,  "palettes") # Palette directory
#print ("Palette Dir: " + pal_dir)

def ase_converter(p,  this_file):
    
    # print "ASE file: " + this_file
    gimp.progress_init() # init progress Bar
    
    # Strip of the "file:///" prefix - if called by nautilus, etc
    if this_file.startswith('file:///'):
        this_file = this_file.replace('file:///', '/')
        
    try:
        pal_gpl = "GIMP Palette\nName: " # start GPL (GIMP Palette) file
        NUL = chr(0) # NULL byte
        SoH = chr(1) # Start of ASE separator byte
        f = open(this_file) #open ASE file
        ase_header = f.read(4) # first 4 bytes
        #print("ASE Header: " + ase_header)
        if ase_header == "ASEF": # first 4 bytes should be "ASEF" if this is an ASE file
            ase_version = f.read(4) # next 4 bytes are version - we don't need this.
            ase_swatches_data = f.read() # rest of the file should be swatch data
            #print ("ASE Swatches data: " + ase_swatches_data)
            ase_swatches = ase_swatches_data.split(SoH + NUL + NUL + NUL) # split by seperator
            
            pal_len = ase_swatches.pop(0) # This seems to be the total file length - I don't think we need it
            pal_title_data = ase_swatches.pop(0).lstrip() # This is the title of the palette
            pal_title = ""
            
            # remove strange characters in the palette name
            for c in pal_title_data:
                char = unpack('>s',  c) # Little endian string
                if re.match("[A-Za-z0-9_\- ]",  char[0]):
                    pal_title += char[0] # only add if A-Z or 0-9 - FIXME - need to handle UTF-8?
            # print ("Palette title: " + pal_title)
            
            # Small palettes look better in one column
            if len(ase_swatches) <= 10: num_cols = "1"
            else: num_cols = "0"
            pal_gpl += pal_title + "\nColumns: " + num_cols + "\n#\n" #Finish GPL header info
            
            # Iterate over each swatch "chunk"
            num_colors = 0
            for swatch in ase_swatches:
                # print swatch
                swatch_buffer = StringIO.StringIO(swatch)
                swatch_buffer.seek(19) # What the heck?  Now Hex colors?
                swatch_mode = "UNKNOWN"
                sm_read = swatch_buffer.read(3)
                # print sm_read
                if sm_read == "RGB":
                    swatch_mode = "RGB"
                elif sm_read == "LAB":
                    swatch_mode = "LAB"
                else: swatch_buffer.seek(-3,  1)
                if swatch_mode == "UNKNOWN":
                    sm_read = swatch_buffer.read(4)
                    #print sm_read
                    if sm_read == "CMYK":
                        swatch_mode = "CMYK"
                # print "Color mode:" + swatch_mode
                if swatch_mode == "RGB": # only operate on RGB swatches - FIXME - LAB, HSV, GRAY should be dealt with next
                    swatch_buffer.seek(1,  1) # Skip one byte
                    swatch_colors = swatch_buffer.read()
                    red = unpack_from('>f',  swatch_colors) #read 4 bytes, unpack to float - little endian
                    green = unpack_from('>f',  swatch_colors,  4) #next 4 bytes
                    blue = unpack_from('>f',  swatch_colors,  8) #next 4 bytes
                    red = (red[0] * 255) # multiply float by 255
                    red = int(round(red,  0)) # round to int
                    green = (green[0] * 255)
                    green = int(round(green,  0))
                    blue = (blue[0] * 255)
                    blue = int(round(blue,  0))
                    #print ("RGB: " + red,  green ,  blue)
                    num_colors = num_colors + 1
                    # Add swatch RGB values to the GPL file
                    pal_gpl += str(red) + "\t" + str(green) + "\t" + str(blue) + "\t" + "Color_" + str(num_colors) + "\n"
                elif swatch_mode =="LAB":
                    swatch_buffer.seek(1,  1) # Skip one byte
                    swatch_colors = swatch_buffer.read()
                    lab_L = unpack_from('>f',  swatch_colors) #read 4 bytes, unpack to float - little endian
                    lab_A = unpack_from('>f',  swatch_colors,  4) #next 4 bytes
                    lab_B = unpack_from('>f',  swatch_colors,  8) #next 4 bytes
                    
                    #print "L: " + str(lab_L[0]) +  " A: " + str(lab_A[0]) + " B: " + str(lab_B[0])
                    
                    # Courtesy of EasyRGB.com - convert Lab to XYZ
                    lab_L = lab_L[0] * 100 #move dec point
                    var_Y = ( lab_L + 16 ) / 116
                    var_X = lab_A[0] / 500 + var_Y
                    var_Z = var_Y - lab_B[0] / 200
                    if ( var_Y**3 > 0.008856 ): var_Y = var_Y**3
                    else: var_Y = ( var_Y - 16 / 116 ) / 7.787
                    if ( var_X**3 > 0.008856 ): var_X = var_X**3
                    else: var_X = ( var_X - 16 / 116 ) / 7.787
                    if ( var_Z**3 > 0.008856 ): var_Z = var_Z**3
                    else: var_Z = ( var_Z - 16 / 116 ) / 7.787
                    # Correct the white point - Observer= 2 deg, Illuminant= D65
                    ref_X =  95.047  
                    ref_Y = 100.000
                    ref_Z = 108.883
                    X = ref_X * var_X        
                    Y = ref_Y * var_Y    
                    Z = ref_Z * var_Z 
                    
                    #print "X: " + str(X) + " Y: " + str(Y) + " Z: " + str(Z)
                    
                    #Courtesy of EasyRGB.com - convert XYZ to RGB
                    var_X = X / 100        #X from 0 to  95.047      (Observer = 2 deg, Illuminant = D65)
                    var_Y = Y / 100        #Y from 0 to 100.000
                    var_Z = Z / 100        #Z from 0 to 108.883
                    var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
                    var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
                    var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570
                    if ( var_R > 0.0031308 ): var_R = 1.055 * ( var_R **( 1 / 2.4 ) ) - 0.055
                    else:                     var_R = 12.92 * var_R
                    if ( var_G > 0.0031308 ): var_G = 1.055 * ( var_G **( 1 / 2.4 ) ) - 0.055
                    else:                    var_G = 12.92 * var_G                    
                    if ( var_B > 0.0031308 ): var_B = 1.055 * ( var_B**( 1 / 2.4 ) ) - 0.055
                    else:                     var_B = 12.92 * var_B
                    R = int(round((var_R * 255),  0))
                    G = int(round((var_G * 255),  0))
                    B = int(round((var_B * 255),  0))
                    # Check for out-of-bounds values - there has to be a prettier way :)
                    if R < 0: R=0
                    if R > 255: R=255
                    if G < 0: G=0
                    if G > 255: G=255
                    if B < 0: B=0
                    if B > 255: B=255
            
                    #print "R: " + str(R) + " G: " + str(G) + " B: " + str(B)
                    num_colors = num_colors + 1
                    pal_gpl += str(R) + "\t" + str(G) + "\t" + str(B) + "\t" + "Color_" + str(num_colors) + "\n"
                    
                elif swatch_mode =="CMYK":
                    #swatch_buffer.seek(4,  1) #skip four bytes
                    swatch_colors = swatch_buffer.read()
                    cmyk_C = unpack_from('>f',  swatch_colors) #read 4 bytes, unpack to float - little endian
                    cmyk_M = unpack_from('>f',  swatch_colors,  4) #next 4 bytes
                    cmyk_Y = unpack_from('>f',  swatch_colors,  8) #next 4 bytes
                    cmyk_K = unpack_from('>f',  swatch_colors,  12) #next 4 bytes
                    #print cmyk_C,  cmyk_M,  cmyk_Y,  cmyk_K
                    # Convert CMYK to CMY
                    C = ( cmyk_C[0] * ( 1 - cmyk_K[0] ) + cmyk_K[0] )
                    M = ( cmyk_M [0]* ( 1 - cmyk_K[0] ) + cmyk_K[0] )
                    Y = ( cmyk_Y[0] * ( 1 - cmyk_K[0] ) + cmyk_K[0] )
                    # Convert CMY to RGB
                    R = (( 1 - C ) * 255) 
                    G = (( 1 - M ) * 255)
                    B = (( 1 - Y ) * 255) 
                    R = int(round(R,  0))
                    G = int(round(G,  0))
                    B = int(round(B,  0))
                    #Check for out-of-bounds
                    if R < 0: R=0
                    if R > 255: R=255
                    if G < 0: G=0
                    if G > 255: G=255
                    if B < 0: B=0
                    if B > 255: B=255
                    #print R,  G,  B
                    
                    num_colors = num_colors + 1
                    #Add color to GPL swatch file
                    pal_gpl += str(R) + "\t" + str(G) + "\t" + str(B) + "\t" + "Color_" + str(num_colors) + "\n"
                    
            #print pal_gpl
            pal_file = os.path.join(pal_dir,  (pal_title + ".gpl")) # target GPL path and file name
            #print pal_file
            if os.path.isfile(pal_file): # display error if GPL file exists
                pdb.gimp_message("Error: the file " + pal_file  + " exists.")
            elif num_colors == 0:
                pdb.gimp_message("Error: Could not import any colors from the file " + this_file  + ".")
            else: 
                pf = open(pal_file,  'w') #open palette file in write mode
                pf.write(pal_gpl) # write GPL data
                pf.close() # close file handle
                gimp.progress_update(1.0) # fill progress bar
                pdb.gimp_palettes_refresh() # refresh palette list
                pdb.gimp_context_set_palette(pal_title) #select new palette
    except Exception, e:
        #  Something went wrong - bailing out!
        pdb.gimp_message("Unexpected error.  Try starting GIMP from a console to see what went wrong.")
        print '%s: %s' % (e.__class__.__name__, e) # FIXME - should be able to get the line number also
        if isinstance(e, SystemExit): 
            raise # take the exit
    except:
        print 'Nonstandard Exception %r: %r' % __import__('sys').exc_info()[:2]
        pdb.gimp_quit(0)
        sys.exit()

register(
    "python-fu-convert-ase",
    "Convert Kuler ASE palette to GIMP palette",
    "Convert ASE palette from Kuler into a GIMP palette.",
    "Chris Mohler",
    "Chris Mohler",
    "2008",
    "<Palettes>/Import _Kuler palette...", 
    "",
    [
        (PF_PALETTE, "palette",  _("Palette"), ""),
        (PF_FILE, "this_file", "Kuler ASE File", ""),
    ],
    [],
    ase_converter, 
    domain=("gimp20-python", gimp.locale_directory)
    )

main()
