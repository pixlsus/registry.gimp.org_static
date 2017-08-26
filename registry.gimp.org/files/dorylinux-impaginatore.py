#!/usr/bin/env python

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
# dorylinux impaginatore | dorylinux@dorianorossi.com
# Basato sul codice "Batch resize" di Carol Spears


import os
import os.path
from gimpfu import *
from math import ceil

#le costanti qui sotto sono espresse in mm e sono:bordi e margini x le immagini da impaginare..
#...e la grandezza del carattere in caso si chieda di stampare i nomi delle immagini

mmLEFT_PAGE_BORDER = 0
mmRIGHT_PAGE_BORDER = 0
mmTOP_PAGE_BORDER = 0
mmBOTTOM_PAGE_BORDER = 0
mmTHUMB_MARGIN = 0
mmFONT_SIZE = 4

def Log(text):
    #f=file("/tmp/gimp.log","a+")
    #f.write(text+"\n")
    #f.close()
    return

def get_images(original_type, original_location):
    images = []
    
    for filename in os.listdir(original_location):

        basename, ext = os.path.splitext(filename)
        if ((len(ext)>0) and (ext in original_type)):
            imagefile = os.path.join(original_location, filename)
            original_image = {'base_name':basename,'image_file':imagefile}
            if os.path.isfile(imagefile):
                images.append(original_image)
                Log(str(original_image))
    return sorted(images)
#questi i parametri di preconfigurazione in caso di salvataggio png
def save_png(image, drawable, new_filelocation, use_comment):
    compression = 9
    interlace, bkgd = False, False
    gama, offs, phys = False, False, False
    time, svtrans = True, False
    pdb.file_png_save2(image, drawable, new_filelocation, new_filelocation, interlace, compression, bkgd, gama, offs, phys, time, use_comment, svtrans) 

#questi i parametri di preconfigurazione in caso di salvataggio jpg
#ho settato il salvataggio senza perdita (il primo 1 dopo la parentesi nel rigo 64)
#se vuoi abbassare il peso/qualita' cambialo, es: 0.5, per comprimere al 50%
def save_jpeg(image, name, comment=""):
    jpeg_save_defaults = (1, 0.0, 1, 0, "", 1, 0, 0, 0)
    args = list(jpeg_save_defaults)
    args[4] = comment
    #e qui vado a dire a gimp di salvare...
    pdb.file_jpeg_save(image, image.active_layer, name, name, *args)

def generate_thumb(filename,Thumb_width,Thumb_height):
    Log(filename)
    img = pdb.gimp_file_load(filename,filename)
    #...controllo se l'immagine e' orizzontale...
    if (img.width>img.height):
        #...faccio due conti per trovare le nuove misure...
        ratio = img.width/float(img.height)
        new = (Thumb_width,int(Thumb_width/ratio))
        #...ndemo vanti...
        if (new[1]>Thumb_height):
            new = (int(Thumb_height*ratio),Thumb_height)
        # ....se non era orizzontale per forze di cose e' verticale....
    else:
        #...e quindi faccio due conti per trovare le nuove misure...
        ratio = img.width/float(img.height)
        new = (int(Thumb_height*ratio),Thumb_height)
        if (new[0]>Thumb_width):
            new = (Thumb_width,int(Thumb_width/ratio))
    #...ora ordino a gimp di scalare l'immagine....
    pdb.gimp_image_scale(img,new[0],new[1])
    return img

def CalcFontSize(text,Font,Size,CalcTextHeight,max_width):
    #...qua calcolo che misura deve avere il carattere per poter scrivere il nome
    # del file, se richiesto nel front-end e farcelo stare nell'impaginato...
    txtw,txtH,txte,txtd = pdb.gimp_text_get_extents_fontname(text,Size,PIXELS,Font)
    if (txtw<=max_width):
        return Size
    while ((txtw>max_width) and (Size>0)):
        Size = Size -1
        txtw,txtH,txte,txtd = pdb.gimp_text_get_extents_fontname(text,Size,PIXELS,Font)
    return Size
    
def Dorylinux_Impaginatore(file_type, location, inc_filename, contact_name, contact_type, contact_location, contact_size, dpi,orient,num_col,num_rows):
    images = get_images(file_type,location)
    num_images = len(images)

    #qua imposto i formati che poi l'interfaccia utente propone (al rigo 244)
    #inizializzo le variabili "width e height" per partire con qualcosa
    #poi le variabili varieranno a seconda della scelta
    width,height = (210,297)
    if (contact_size =="40x80"):
        width,height = (406,812)

    elif (contact_size =="40x60"):
        width,height = (406,610)

    elif (contact_size =="30x40"):
        width,height = (305,406)

    elif (contact_size =="20x30"):
        width,height = (203,305)

    elif (contact_size =="A3"):
        width,height = (297,420)    

    elif (contact_size =="A4"):
        width,height = (210,297)

    elif (contact_size =="A5"):
        width,height = (148,210)

    elif (contact_size =="A6"):
        width,height = (105,148)

    #Log(str(width))
    #Log(str(height))
    width = int((width/25.4)*int(dpi))
    height = int((height/25.4)*int(dpi))

    #calcolo la misura necessaria per farci stare i vari file nell'impaginato
    #rispettando le richieste impostate nel front-end

    #converto le misure a pixel
    LEFT_PAGE_BORDER = int((mmLEFT_PAGE_BORDER/25.4)*float(dpi))
    RIGHT_PAGE_BORDER = int((mmRIGHT_PAGE_BORDER/25.4)*float(dpi))
    TOP_PAGE_BORDER = int((mmTOP_PAGE_BORDER/25.4)*float(dpi))
    BOTTOM_PAGE_BORDER = int((mmBOTTOM_PAGE_BORDER/25.4)*float(dpi))
    THUMB_MARGIN = int((mmTHUMB_MARGIN/25.4)*float(dpi))
    FONT_SIZE = int((mmFONT_SIZE/25.4)*float(dpi))
    ThumbsPerSheet = num_col*num_rows
    img_no = 1
    for sheetcount in range(int(ceil(len(images)/float(ThumbsPerSheet)))):
    
        if (orient=="land"):
            sheetimg = gimp.Image(height,width,RGB)
            bklayer = gimp.Layer(sheetimg,"Background",height,width,RGB_IMAGE,100,NORMAL_MODE)
            sheetimg.disable_undo()
            sheetimg.add_layer(bklayer,0)
        else:
            sheetimg = gimp.Image(width,height,RGB)
            bklayer = gimp.Layer(sheetimg,"Background",width,height,RGB_IMAGE,100,NORMAL_MODE)
            sheetimg.disable_undo()
            sheetimg.add_layer(bklayer,0)
        #Log(str(sheetimg))
        #Log(str(sheetimg.resolution))
        #set the image resolution
        sheetimg.resolution = (float(dpi), float(dpi))
        #now calculate sizes
        Canvas_width = sheetimg.width - LEFT_PAGE_BORDER - RIGHT_PAGE_BORDER        
        Canvas_height = sheetimg.height - TOP_PAGE_BORDER - BOTTOM_PAGE_BORDER
        bklayer.fill(WHITE_FILL)
        bklayer.flush()
        sheetdsp = gimp.Display(sheetimg)
        gimp.displays_flush()        
        
        txtw,CalcTextHeight,txte,txtd = pdb.gimp_text_get_extents_fontname("Sheet %03d of %03d" % (sheetcount+1,int(ceil(len(images)/float(ThumbsPerSheet)))),FONT_SIZE,PIXELS,"Arial")
        txtfloat = pdb.gimp_text_fontname(sheetimg, sheetimg.active_layer, LEFT_PAGE_BORDER, TOP_PAGE_BORDER-CalcTextHeight, "Sheet %03d of %03d"  % (sheetcount+1,int(ceil(len(images)/float(ThumbsPerSheet)))), -1, False, FONT_SIZE, PIXELS, "Arial")
        pdb.gimp_floating_sel_anchor(txtfloat)
        
        CalcTextHeight =0
        txtw,txth,txte,txtd = (0,0,0,0)
        if (inc_filename == True):
            txtw,CalcTextHeight,txte,txtd = pdb.gimp_text_get_extents_fontname(images[0]['base_name'],FONT_SIZE,PIXELS,"Arial")
                    
        #print "CalcText Height %d " %(CalcTextHeight)
        Thumb_width = (Canvas_width/num_col)-2*THUMB_MARGIN
        Thumb_height = Canvas_height/num_rows-2*THUMB_MARGIN - CalcTextHeight
        
        files = images[sheetcount*ThumbsPerSheet:(sheetcount+1)*ThumbsPerSheet]
        #...ndemo vanti...
        rcount = 0
        ccount = 0
        #genero le miniature....
        for file in files:
            thumbimg=generate_thumb(file['image_file'],Thumb_width,Thumb_height)
            cpy = pdb.gimp_edit_copy(thumbimg.active_layer)
            gimp.delete(thumbimg)
            #...copio l'immagine sull'impaginato...
            newselect = pdb.gimp_edit_paste(sheetimg.active_layer,True)
            #print str(newselect)
            #print str(newselect.offsets)
            #posiziono la prima in alto a sx
            newselect.translate(-newselect.offsets[0],-newselect.offsets[1])
            #...posiziono correttamente le immagini....
            xpos = LEFT_PAGE_BORDER + ccount * (Thumb_width + (2 * THUMB_MARGIN)) + THUMB_MARGIN
            ypos = TOP_PAGE_BORDER + rcount * (Thumb_height + (2 * THUMB_MARGIN)+ CalcTextHeight) + THUMB_MARGIN 
            newselect.translate(xpos,ypos)
            pdb.gimp_floating_sel_anchor(newselect)
            
            if (inc_filename == True):
                Size = CalcFontSize(file['base_name'],"Arial",FONT_SIZE,CalcTextHeight,Thumb_width)
                txtfloat = pdb.gimp_text_fontname(sheetimg, sheetimg.active_layer, xpos-THUMB_MARGIN, ypos+Thumb_height+THUMB_MARGIN, file['base_name'], -1, False, Size, PIXELS, "Arial")
                pdb.gimp_floating_sel_anchor(txtfloat)
                
            ccount = ccount + 1
            if (ccount>= num_col):
                ccount = 0
                rcount = rcount + 1
            gimp.displays_flush()
        
        contact_filename = contact_name + "_%03d" % (sheetcount) + contact_type
        contact_full_filename = os.path.join(contact_location, contact_filename)
        #print "File to save " + contact_full_filename
        if (contact_type == ".jpg"):
            #save_jpeg(sheetimg,contact_full_filename,"Fatto con GIMP e Dorylinux Impaginatore plugin")
            save_jpeg(sheetimg,contact_full_filename,"")
        else:
            save_png(sheetimg,pdb.gimp_image_get_active_drawable(sheetimg),contact_full_filename,False)
        gimp.delete(sheetimg)
        pdb.gimp_display_delete(sheetdsp) 

register(
        "python_fu_dorylinux_impaginatore",
        "dorylinux > impaginatore (v. 1.0)\nQuesto script impagina in batch delle immagini contenute in una cartella su un foglio di lavoro tra quelli proposti...\n\nP.S: passa a Ubuntu/Linux....",
        "dorylinux@dorianorossi.com",
        "dorylinux",
        "Licenza GPL v2",
        "2008",
        "<Toolbox>/Xtns/Batch/dorylinux > impaginatore",
        "",
        [
        (PF_RADIO, "file_type", "Elaboro solo i file in formato:", ".jpg .jpeg .JPG", (("jpg", ".jpg .jpeg .JPG"), ("png", ".png .PNG"), ("tiff", ".tiff .tif .TIF .TIFF"))),
        (PF_DIRNAME, "location", "...che trovo nella cartella...", ""),
        (PF_BOOL, "inc_filename", "...vuoi che scriva i nomi/file nell'impaginato?", False),
        (PF_STRING, "contact_name",  "...salvo l'impaginato col nome...", "impaginato"),
        (PF_RADIO, "contact_type", "...nel formato...", ".jpg", (("jpg (consigliato per immagini)", ".jpg"), ("png (consigliato per grafica)", ".png"))),
        (PF_DIRNAME, "contact_location", "...e lo salvero' esattamente qui'.....", ""),
        (PF_RADIO, "contact_size", "...impagino nel formato...", "40x80", (("40x80 (40,6x81,2 cm)", "40x80"),("40x60 (40,6x61 cm)", "40x60"), ("30x40 (30,5x40,6 cm)", "30x40"),("20x30 (20,3x30,5 cm)", "20x30"), ("A3 (21x42 cm)", "A3"), ("A4 (21x29,7 cm)", "A4"),("A5 (14,8x21 cm)", "A5"), ("A6 (10,5x14,8 cm)", "A6"))),

        (PF_RADIO, "dpi", "...lo preparo con una risoluzione di...", "200", (("300 dpi (tipografica/offset)","300"),("254 dpi (stampa fotografica 1)","254"),("200 dpi (stampa fotografica 2)","200"),("72 dpi (monitor)", "72"))), 
        (PF_RADIO, "orient", "...l'impaginato deve essere...", "land", (("verticale", "port"), ("orizzontale","land"))),
        (PF_SPINNER, "num_col", "...numero di immagini per riga...", 2, (1,32,1)),
        (PF_SPINNER, "num_rows", "...numero di immagini per colonna...", 2, (1,32,1))
        ],
        [],
        Dorylinux_Impaginatore)


main()
