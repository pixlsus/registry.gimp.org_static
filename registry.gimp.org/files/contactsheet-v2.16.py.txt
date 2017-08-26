#!/usr/bin/env python
# -*- coding: UTF-8 -*-

"""software_license
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
"""
## VERSION HISTORY
##
## Version 2.16 release date 28-nov-2011
## Fixed type error integer expexted got float
##
## Version 2.15 release date 25-dec-2010
## Fixed mbcs decode not read by Linux
##
## Version 2.14 release date 27-nov-2010
## Fixed impossible to read foreign characters
## Fixed now skip non-image files with images extensions
## change: numbering files now same as on paper (request G.Sprik) 
##
## Version 2.13 release date 16 june 2010
## added several new sheetsizes including two banners
##
## Version 2.12 release date 04-jan-2010
## added an option "filename with extension?", Default is with extension.
## added updated Dutch language pack
## added version history
##
## Version 2.11, release date 30-oct-2009
## added .bmp support
## added version number in the title bar
##
## Version 2.10 (released 22-sept-2009) 
## Added printing capabilities. You have to experiment with your printer for the best
##         results. Very important: the plugin uses the default Gimp builtin printer driver
##         (will be changed in future releases). Unfortunatly I cannot set any of the
##         parameters (pagesize, margin, etc) you have to set the parameters of your printer
##         first :(. Set margins in your printerdriver to zero and choose your correct
##         papersize. 
## Added .ps/.eps support. The special .ps/.eps version of contactsheet has been removed.
##         Remember to add ghostview/ghostscript to GIMP if you want to select an .eps/.ps
##         type image otherwise contactsheet refused to work. For this reason selecting
##         "all registered types" in imagetypes will exclude .ep/.eps types of images. 
## FIXED: "Under certain conditions it was possible that not all possible rows were placed
##         on contactsheet". Problem has been solved. 
## Code has been partly rewritten.
##
## version 2.06 
## FIXED: "It is possible that not all possible rows are placed on the contactsheet. These
##         rows are placed on the next contactsheet." A miscalculation has been corrected.
##         It was only visible when a large number of images per row was choosen without the
##         filename option. 
## Margin round the image can now be changed in tenth of a mm. 
## Maximum number of images per row has been increased to 256. 
## Maximum number of rows has been increased to 256. 
## Maximum number of DPI has been increased to 1000.
##
## version 2.05 
## A little 'error' (a testlog command has not been set to off) has been corrected, nothing
##         else has been changed. Look for "../tmp/gimp.log" on your system, delete this file.
##
## version 2.04 
## FIXED: "Set topmargin at least 5 mm otherwise contactsheet page will not be visible." Topmargin can now be as low as possible. It is the user responsebility to take care of the printable margin. For instance my printer (HP CP1700d) the left-right margin should be set at least 5mm otherwise that part of the image wil not be printed. 
## FIXED: "Textfile (if option is checked) is not emptied wih each batch run so it will
##         grow to infinity. You have to manually empty/delete this file." Textfile is now
##         emptied before information is written.
##
## version 2.03 
## Margin round the image can now be changed from zero on. 
## Added "sorted images" as option. 
## Updated Dutch language pack to v1.2. 
## Updated localization file. 
## FIXED: "Files with an extension .j .jp .jpe .p .pc .t .ti .x .xc .pn , will be seen as
##         images. If it is not a imagefile the plugin stops. Remove/move/rename all files
##         with those extensions." It is not longer necessary to remove these files.
##
## version 2.02 
## Margin round the image can now be changed. 
## Font size can now be choosen. Be aware that fontsize automatically will be scaled down
##      to fit the image width. 
## Added .XCF support. 
## Added a choice to save a text filelist from the printed images with the directory name
##      where the image is located. Useful if you have a lot of images in several directory's.
##      Name is the same as the contactpage name but with the .TXT extension. 
## Code has partly been rewritten.
##
## version 2.01 
## Dutch language pack added. 

## version 2.00 
## Now possible to make contactsheet of a whole directory including subdirs. 
## Two radiobuttons replaced by option buttons. 
## Added several paperformats, including lettersized papers. 
## Images are sorted in the way the systems sort the images. 
## Added extension in the filename. 
## Added support for pcx format. 
## Added making contactsheets of all registered formats in one batch. 
## Number of rows is now limited to paperheight (automatic scaled down if necessary). 
## Images are automatically scaled and symmetrically placed within square minipages. 
## Contactsheet modified to work also on Gimp version 2.6. 

## version first 
## Robin has made his final release of contactsheet only working up to Gimp version 2.4. 


# Based on Batch resize code by Carol Spears
# Modified by Robin Gilham for contact sheet plugin
# Contactsheet plugin modified by Elmar Sullock Enzlin at moroquendo@gmail.com
# See for details my website at www.sullockenzlin.demon.nl


import os
import os.path
import gimp
from gimpfu import *
from math import ceil, floor

#==============================================================================
#================= localization with "contactsheet.mo" ========================
#==============================================================================
gettext.install("contactsheet", gimp.locale_directory, unicode=True) 


#==============================================================================
#=========== function only used for testpurposes and error logging ============
#==============================================================================
def Log(text):
    filename = ("c:/tmp/gimp.log")
    f=file(filename, "a+")
    f.write(text+"\n")
    f.close()
    return


#==============================================================================
#=============== Makes an overview of the images and directorys ===============
#==============================================================================
# input: text:              text to save
#        contact_location:  directory to save
#        contact_name:      filename of the textfile is the same in the registersection
# output: txt file with imagename and dirname were the image is located
#==============================================================================
def LogFileName(text, contact_location, contact_name, FirstRun):
    Filename = (contact_location + '/' + contact_name + '.txt')
    if (FirstRun == True):
        if (os.path.isfile(Filename) == True):
            os.remove(Filename)
            
    f=file(Filename, "a+")
    f.write(text + "\n")
    f.close()
    return


#==============================================================================
#================= get images from eventually all dirs ========================
#==============================================================================
# input: FileType:          array contains one or more extensions
#        original_location: directory to start
#        all_subdirs:       bool to include subdirectory's too
#        DirFileList:       bool to give a txt list of images with their dir
# output: images:           array with images
#==============================================================================
def get_images(FileType, original_location, all_subdirs, DirFileList, SortedImages):
    images = []

    if (FileType == 7):
        FileType = '.jpg .jpeg .JPG .JPEG .png .PNG .tiff .tif .TIF .TIFF .pcx .PCX .xcf .XCF .ps .PS .eps .EPS .bmp .BMP'
    elif(FileType == 6):
        FileType = '.bmp .BMP'        
    elif (FileType == 5):
        FileType = '.ps .eps'        
    elif (FileType == 4):
        FileType = '.xcf .XCF'
    elif (FileType == 3):
        FileType = '.pcx .PCX'
    elif (FileType == 2):
        FileType = '.tiff .tif .TIF .TIFF'
    elif (FileType == 1):
        FileType = '.png .PNG'
    elif(FileType == 0):
        FileType = '.jpg .jpeg .JPG .JPEG'
    else:
        FileType = ''
        Log("error in file type")

    if (all_subdirs == True):                       #include all subdirectory's
       for dirpath, dirnames, filenames in os.walk(original_location.decode('idna'), topdown=True):
           for filename in filenames:
              basename, ext = os.path.splitext(filename)
              if ((len(ext)>2) and (ext in FileType)):
                 imagefile = os.path.join(dirpath, filename)
                 original_image = {'extension':ext,'base_name':basename,'image_file':imagefile}
                 if os.path.isfile(imagefile):
                    images.append(original_image)
    else:                                           #only the choosen directory
        for filename in os.listdir(original_location.decode('idna')):
            basename, ext = os.path.splitext(filename)
            if ((len(ext)>2) and (ext in FileType)):
                imagefile = os.path.join(original_location, filename)
                original_image = {'extension':ext,'base_name':basename,'image_file':imagefile}
                if os.path.isfile(imagefile):
                    images.append(original_image)

    if (SortedImages == False):
        return images
    else:
        return sorted(images)

## or images.sort() #sort list alpha numeric

                    
#==============================================================================
#================= create a text list with file and dirnames ==================
#==============================================================================
# input: images:            array contains the images etc.
#        contact_location:  directory to save
#        contact_name:      name of the txt file
# output: txt file with imagename and dirname were the image is located
#==============================================================================
def Make_DirFile_List(images, contact_location,  contact_name):
    files = images[0:len(images)]
    FirstRun = True
    for file in files:
        filename = file['image_file']
        LogFileName(filename, contact_location, contact_name, FirstRun)
        FirstRun = False


#==============================================================================
#================= save contact sheet as a png file ===========================
#==============================================================================
def save_png(image, drawable, new_filelocation, use_comment):
    compression = 9
    interlace, bkgd = False, False
    gama, offs, phys = False, False, False
    time, svtrans = True, False
    pdb.file_png_save2(image, drawable, new_filelocation, new_filelocation,
                       interlace, compression, bkgd, gama, offs, phys, time,
                       use_comment, svtrans) 

#==============================================================================
#================= save contact sheet as a jpg file ===========================
#==============================================================================
def save_jpeg(image, name, comment=""):
    jpeg_save_defaults = (0.85, 0.0, 1, 0, "", 1, 0, 0, 0)
    args = list(jpeg_save_defaults)
    args[4] = comment

    pdb.file_jpeg_save(image, image.active_layer, name, name, *args)


#==============================================================================
#================= generate a thumb ===========================================
#==============================================================================
def generate_thumb(filename,Thumb_width,Thumb_height):

    #Log('Entering generate_thumb routine')
    valid_image = True
    try:
        img = pdb.gimp_file_load(filename,filename)     #load image
    except RuntimeError:
        Log('Not a valid image: ' + filename)
        valid_image = False
        img=[]
        return img, 0, 0, valid_image
    
    #now resize the loaded image proportionally
    if (img.width>img.height):
        #landscape so scale height proportionally
        ratio = img.width/float(img.height)
        new = (Thumb_width,int(Thumb_width/ratio))
        #if the new height exceed max. thumb height
        #then scale to max. thumb height
        if (new[1]>Thumb_height):
            new = (int(Thumb_height*ratio),Thumb_height)
    else:
        #portrait so scale width proportionally
        ratio = img.width/float(img.height)
        new = (int(Thumb_height*ratio),Thumb_height)
        if (new[0]>Thumb_width):
            new = (Thumb_width,int(Thumb_width/ratio))
    #now resize the image
    pdb.gimp_image_scale(img,new[0],new[1])
    return img,new[0],new[1], valid_image     #modified: added the x- and y-size


#==============================================================================
#================= modify fontsize so it fits thumbwidth ======================
#==============================================================================
def CalcFontSize(text,Font,Size,CalcTextHeight,max_width):
    #this procedure calculates the text size to fit within the
    #width param, the text is reduced until the width is small enough
    txtw,txtH,txte,txtd = pdb.gimp_text_get_extents_fontname(text,Size,PIXELS,Font)
    if (txtw<=max_width):
        return Size,txtw
    while ((txtw>max_width) and (Size>0)):
        Size = Size -1
        txtw,txtH,txte,txtd = pdb.gimp_text_get_extents_fontname(text,Size,PIXELS,Font)
    return Size,txtw


#==============================================================================
#===================== calculate papersize in pixels ==========================
#===================== given sizes are mm =====================================
#==============================================================================
def CalcPaperSize(ContactSize, dpi):
    if (ContactSize == 0):             #Jumbo
        width,height = (102,152)
    elif (ContactSize == 1):           #6x8
        width,height = (152,203)
    elif (ContactSize == 2):           #8x10
        width,height = (203,254)        
    elif (ContactSize == 3):           #A4
        width,height = (210,297)
    elif (ContactSize == 4):           #A3
        width,height = (297,420)
    elif (ContactSize == 5):           #A2
        width,height = (420,594)
    elif (ContactSize == 6):           #A1
        width,height = (594,841)
    elif (ContactSize == 7):           #A0
        width,height = (841,1189)
    elif (ContactSize == 8):           #Letter
        width,height = (216,279)
    elif (ContactSize == 9):           #Legal
        width,height = (216,356)
    elif (ContactSize == 10):           #Tabloid
        width,height = (279,432)
    elif (ContactSize == 11):           #banner A4 width
        width,height = (210,1000)
    elif (ContactSize == 12):           #banner A3 width
        width,height = (297,1000)
    else:
        width,height = (210,297)
        Log("error in pagesize, pagesize doesnot exist")

    width = int((width/25.4)*dpi)              # calculate width in px
    height = int((height/25.4)*dpi)            # calculate height in px

    return width, height                       #size in pixels


#==============================================================================
#================= main routine generate contact sheet ========================
#==============================================================================
def Contact_Sheet(file_type, location, all_subdirs, inc_filename, inc_extension,
                  contact_name, contact_type, contact_location, contact_size,
                  dpi, orient, num_col, num_rows, PageBorderLR, PageBorderTB,
                  mmTHUMB_MARGIN, mmFONT_SIZE, Dump_Filename_list, Sorted_Images,
                  Print_Contactsheet):
    
    #collect 'all' images in the choosen directory and subdirs
    images = get_images(file_type, location, all_subdirs,
                        Dump_Filename_list, Sorted_Images)
    num_images = len(images)                        #calculate number of images    
    
    #if necessary make a txt file with image name and image directory
    if (Dump_Filename_list == True):
        Make_DirFile_List(images, contact_location, contact_name)

    #make  a new drawing canvas of the correct size
    width,height = CalcPaperSize(contact_size, dpi)     #dimensions in px
    
    #calculate the required size for the thumbs based on the number of images
    #per row. Sizes are in px and floored with maximum error of one px
    LEFT_PAGE_BORDER = (PageBorderLR/25.4)*dpi    
    RIGHT_PAGE_BORDER = (PageBorderLR/25.4)*dpi
    BOTTOM_PAGE_BORDER = (PageBorderTB/25.4)*dpi
    THUMB_MARGIN = (mmTHUMB_MARGIN/25.4)*dpi
    FONT_SIZE = (mmFONT_SIZE/25.4)*dpi
    TOP_PAGE_BORDER = (PageBorderTB/25.4)*dpi + FONT_SIZE  #include sheet .. of ..
    
    #number of rows is limited so is ThumbsPerSheet
    #Thumb sizes are in px, based on dpi setting
    if (orient=="port"):
        Thumb_width = ((width- LEFT_PAGE_BORDER - RIGHT_PAGE_BORDER)/num_col)- 2*THUMB_MARGIN
        if (inc_filename == True):
            UsableRows = floor((height- TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER)/
                        (Thumb_width + FONT_SIZE + 2*THUMB_MARGIN))
        else:
            UsableRows = floor((height- TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER)/
                        (Thumb_width + 2*THUMB_MARGIN))
    else:
        Thumb_width = ((height- LEFT_PAGE_BORDER - RIGHT_PAGE_BORDER)/num_col)- 2*THUMB_MARGIN
        if (inc_filename == True):
            UsableRows = floor((width- TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER)/
                        (Thumb_width + FONT_SIZE + 2*THUMB_MARGIN))
        else:
            UsableRows = floor((width- TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER)/
                        (Thumb_width + 2*THUMB_MARGIN))

    Thumb_height = Thumb_width
            
    #Number of chosen rows can never be bigger then usable rows
    if (num_rows > UsableRows):
        num_rows = UsableRows

    ThumbsPerSheet = int(num_col*num_rows)              #added 'int' for python v2.6
    img_no = 1
    for sheetcount in range(int(ceil(num_images/float(ThumbsPerSheet)))):
    
        if (orient=="land"):
            sheetimg = gimp.Image(height,width,RGB)
            bklayer = gimp.Layer(sheetimg,"Background",height,width,
                                 RGB_IMAGE,100,NORMAL_MODE)
        else:
            sheetimg = gimp.Image(width,height,RGB)
            bklayer = gimp.Layer(sheetimg,"Background",width,height,
                                 RGB_IMAGE,100,NORMAL_MODE)

        sheetimg.disable_undo()
        sheetimg.add_layer(bklayer,0)

        #set the image resolution
        sheetimg.resolution = (float(dpi), float(dpi))
        
        #now calculate sizes; printable sheet dimensions are given
        #in px based on the dpi setting
        Canvas_width = sheetimg.width - LEFT_PAGE_BORDER - RIGHT_PAGE_BORDER 
        Canvas_height = sheetimg.height - TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER
        #print ("Canvas width %d height %d" % ( Canvas_width,Canvas_height))
        
        #Log(str(sheetimg.resolution))
        #now fill with white, How to fill with a other color?????
        bklayer.fill(WHITE_FILL)
#        gimp.set_background(bk_color)
#        gimp-bklayer-fill(bklayer, BG-IMAGE-FILL)
#        Log (str(bk_color))
#        bklayer.fill(bk_color)
#        Log(str(gimp.get_background()))
        
        bklayer.flush()
        sheetdsp = gimp.Display(sheetimg)
        #print "sheet display" + str(sheetdsp)
        gimp.displays_flush()        
        
        txtw,CalcTextHeight,txte,txtd = pdb.gimp_text_get_extents_fontname( _("Sheet %03d of %03d") %
                                            (sheetcount+1,int(ceil(num_images/float(ThumbsPerSheet)))),
                                             FONT_SIZE,PIXELS,"Arial")

##        txtfloat = pdb.gimp_text_fontname(sheetimg, sheetimg.active_layer,    #only for me
##                        LEFT_PAGE_BORDER, TOP_PAGE_BORDER-CalcTextHeight,
##                        _("Sheet %03d of %03d"),
##                         contactsheet designed by R, Gilham (za) and E. Sullock Enzlin (nl)"
##                        % (sheetcount+1,int(ceil(num_images/float(ThumbsPerSheet)))),
##                        -1, False, FONT_SIZE, PIXELS, "Arial")

        txtfloat = pdb.gimp_text_fontname(sheetimg, sheetimg.active_layer,
                        LEFT_PAGE_BORDER, TOP_PAGE_BORDER-CalcTextHeight,
                        _("Sheet %03d of %03d") % (sheetcount+1,int(ceil(num_images/float(ThumbsPerSheet)))),
                        -1, False, FONT_SIZE, PIXELS, "Arial")        
        pdb.gimp_floating_sel_anchor(txtfloat)
        
        CalcTextHeight =0
        txtw,txth,txte,txtd = (0,0,0,0)
        if (inc_filename == True):
            txtw,CalcTextHeight,txte,txtd = pdb.gimp_text_get_extents_fontname(images[0]['base_name'],
                                                    FONT_SIZE,PIXELS,"Arial")
            
        #print "CalcText Height %d " %(CalcTextHeight)

        files = images[sheetcount*ThumbsPerSheet:(sheetcount+1)*ThumbsPerSheet]
        #now for each of the image files generate a thumbnail
        rcount = 0
        ccount = 0
        #generate thumb 
        for file in files:
            thumbimg,x_size,y_size,valid_image = generate_thumb(file['image_file'],Thumb_width,Thumb_height)
            if valid_image == False:
                continue                                #next image
            
            cpy = pdb.gimp_edit_copy(thumbimg.active_layer)
            #center image within its minipage
            if (x_size>y_size):
                #landscape image, center vertical
                y_offset = (Thumb_width - y_size)/2
                x_offset = 0
            else:
                #portrait image, center horizontal
                x_offset = (Thumb_height - x_size)/2
                y_offset = 0

            gimp.delete(thumbimg)
            #now paste the new thumb into contact sheet
            newselect = pdb.gimp_edit_paste(sheetimg.active_layer,True)
            #print str(newselect)
            #print str(newselect.offsets)
            #positition in top left corner 
            newselect.translate(-newselect.offsets[0],-newselect.offsets[1])
            #now position in correct position, modified with x- and y-offset
            xpos = LEFT_PAGE_BORDER + ccount * (Thumb_width + (2 * THUMB_MARGIN))+ THUMB_MARGIN  + x_offset
            ypos = TOP_PAGE_BORDER + rcount * (Thumb_height + (2 * THUMB_MARGIN)
                                               + CalcTextHeight) + THUMB_MARGIN + y_offset
            xpos = int(xpos)    #changed to int: on ubuntu type error integer expected got float.
            ypos = int(ypos)
                
            newselect.translate(xpos,ypos)
            pdb.gimp_floating_sel_anchor(newselect)
            
            if (inc_filename == True):
                if (inc_extension == True):
                    ThumbName = file['base_name'] + file['extension']
                else:
                    ThumbName = file['base_name']
                    
                Size,txtwidth = CalcFontSize(ThumbName,"Arial",FONT_SIZE,CalcTextHeight,Thumb_width)                
                #calculate text position, round the center of the image
                txt_xpos = xpos + (Thumb_width - txtwidth)/2 - x_offset
                txt_ypos = ypos+Thumb_height+THUMB_MARGIN-y_offset

                txtfloat = pdb.gimp_text_fontname(sheetimg, sheetimg.active_layer, txt_xpos,
                                                  txt_ypos, ThumbName,-1, False, Size, PIXELS, "Arial")                 
                pdb.gimp_floating_sel_anchor(txtfloat)
                
            ccount = ccount + 1
            if (ccount>= num_col):
                ccount = 0
                rcount = rcount + 1
            gimp.displays_flush()

        #save contactsheet
        contact_filename = contact_name + "_%03d" % (sheetcount+1) + contact_type
        contact_full_filename = os.path.join(contact_location, contact_filename)
        #print "File to save " + contact_full_filename
        if (contact_type == ".jpg"):
            save_jpeg(sheetimg,contact_full_filename,"")
        else:
            save_png(sheetimg,pdb.gimp_image_get_active_drawable(sheetimg),contact_full_filename,False)

        if (Print_Contactsheet == True):
            pdb.file_print_gtk(sheetimg)

        gimp.delete(sheetimg)
        pdb.gimp_display_delete(sheetdsp) 


register(
        "python_fu_contact_sheet_V216",
        _("Generates a contact sheet(s) for a directory of images. If you find this script useful or any bugs I would love to hear from you robin.gilham@gmail.com\nHeck you could even consider a donation"),
        _("Generates contact sheet(s) with a configurable number of thumbnails for all files located in a directory"),
        "Robin Gilham, E. Sullock Enzlin",
        "Licensed under the GPL v2",
        "2008, 2009, 2010, 2011",
        "<Toolbox>/Xtns/Batch/Contact Sheet V2.15",
        "",
        [
        (PF_OPTION, "file_type"  ,_("File type: "), 0 ,[".jpg", ".png", ".tif", ".pcx",
                                                        ".xcf",".eps",".bmp", _("all registered formats")]),
        (PF_DIRNAME, "location", _('Generate contact sheet of\n all files in this directory: '), ""),
        (PF_BOOL, "all_subdirs", _("Include all subdirs? "), False),        
        (PF_BOOL, "inc_filename", _('Include filename on contact sheet?'), True),
        (PF_BOOL, "inc_extension", _('Filename with extension?'), True),
        (PF_STRING, "contact_name",  _('Contact sheet base name: '), _('contact_sheet')),
        (PF_RADIO, "contact_type", _('Contact sheet image type: '), ".jpg", (("jpg", ".jpg"), ("png", ".png"))),
        (PF_DIRNAME, "contact_location", _('Where the contact sheet should be saved in: '), ""),
        (PF_OPTION, "contact_size", _("Contact page sheet size: "), 3,
                 ["Jumbo (10.2x15.2 cm)",
                  "6x8 (15.2x20.3 cm)",
                  "8x10 (20.3x25.4 cm)",
                  "A4 (20.9x29.7 cm)",
                  "A3 (29.7x42.0 cm)",
                  "A2 (42,0x59,4 cm)",
                  "A1 (59,4x84,1 cm)",
                  "A0 (84,1x118,9 cm)",
                  "Letter (8.5x11 in)",
                  "Legal (8.5x14 in)",
                  "Tabloid (11x17 in)",
                  "banner 1 (20,9x100 cm)",
                  "banner 2 (29,7x100 cm)",]),
        (PF_SPINNER, "dpi", _("Contact sheet resolution"), 150,(72,1000,1)),
        (PF_RADIO, "orient", _("Orientation:"), "port", ((_("portrait"), "port"), (_("landscape"),"land"))),
        (PF_SPINNER, "num_col", _("Number of images per row"), 4, (1,256,1)),
        (PF_SPINNER, "num_rows", _("Number of rows"), 5, (1,256,1)),
        (PF_SPINNER, "PageBorderLR", _("Left/Right Page border [mm]"), 15, (1,32,1)),        
        (PF_SPINNER, "PageBorderTB", _("Top/Bottom Page border [mm]"), 15, (1,32,1)),
        (PF_SPINNER, "mmThumb_Margin", _("Margin round image [mm]"), 2, (0,10,0.1)),
        (PF_SPINNER, "mmFont_Size", _("Font size [mm]"), 3, (2,5,1)),
        (PF_BOOL, "Dump_Filename_list", _('Include filenamelist?'), False),
        (PF_BOOL, "Sorted_Images", _('Images sorted alphanumeric?'), False),
        (PF_BOOL, "Print_Contactsheet", _('Directly print contactsheet?'), False)
        ##(PF_COLOR, "bk_color", ('bacckgroundcolor?'), (0,0,0))
        ],
        [],
        Contact_Sheet,
        menu="<Save>",
        domain=("contactsheet", gimp.locale_directory)
        )

main()
