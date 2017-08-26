#!/usr/bin/env python
# -*- coding: utf8 -*-

# *************************************************************************** #
#                                                                             #
#      Version 0.1 - 2008-02-08                                               #
#      Copyright (C) 2008 Marco Crippa - Ivan Signorino                       #
#                                                                             #
#      This program is free software; you can redistribute it and/or          #
#      modify it under the terms of the GNU General Public License            #
#      as published by the Free Software Foundation; either version 2         #
#      of the License, or (at your option) any later version.                 #
#                                                                             #
#      This program is distributed in the hope that it will be useful,        #
#      but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
#      GNU General Public License for more details.                           #
#                                                                             #
#      You should have received a copy of the GNU General Public License      #
#      along with this program; if not, write to the                          #
#      Free Software Foundation, Inc.,                                        #
#      51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA           #
#                                                                             #
# *************************************************************************** #

from gimpfu import *
from ming import *
import os

global lista_img
global file_salva

global vel
vel=0
file_salva=""
lista_img=[]

def Text(testo2, colore, font,font_size,antialias,velocita,base_nome,dir_save):
    global lista_img
    global vel
    
    testo=testo2.decode('utf8')
    vel=int(velocita)
    testo_w,testo_h,testo_a,testo_d = pdb.gimp_text_get_extents_fontname(testo,font_size,PIXELS,font)
    pdb.gimp_context_set_foreground(colore)
    for i in range (1,len(testo)+1):
        img = gimp.Image(testo_w,testo_h,RGB)
        bklayer = gimp.Layer(img,"Background",testo_w,testo_h,RGB_IMAGE,100,NORMAL_MODE)
        img.add_layer(bklayer,0)
        img.disable_undo()
        txtfloat = pdb.gimp_text_fontname(img, bklayer, 0, 0, testo[0:i], -1, antialias, font_size, PIXELS, font)
        pdb.gimp_floating_sel_to_layer(txtfloat)
        pdb.gimp_layer_resize_to_image_size(txtfloat)
        est_num=""
        file_nome=""
        if i<10:
            est_num="00"
        elif 9<i<100:
            est_num="0"
            
        file_nome=base_nome+"_"+est_num+str(i)+".png"
        contact_full_filename = os.path.join(dir_save, file_nome)
        lista_img.append(contact_full_filename)
        pdb.gimp_file_save(img,pdb.gimp_image_get_active_layer(img),contact_full_filename,contact_full_filename)    
        img.enable_undo()
        gimp.delete(img)
    esegui_fun()

def esegui_fun():
    global lista_img
    global file_salva        
    global vel
    
    Ming_useSWFVersion(7)
    img_temp=SWFBitmap(lista_img[0])
    width=img_temp.getWidth()
    height=img_temp.getHeight()

    m = SWFMovie()
    frame=int(vel*len(lista_img))
    m.setFrames(frame)
    m.setDimension(width, height)
    m.setRate(25)

    for ind_temp in range(0, len(lista_img)):

        os.system("png2dbl "+lista_img[ind_temp])

    for ind_temp in range(0, len(lista_img)):
        file_img_temp=lista_img[ind_temp]
        file_img_temp=file_img_temp[:-4]
        file_img_temp=file_img_temp+".dbl"
        b=SWFBitmap(file_img_temp)
        i=m.add(b)
        for a in range (0,int(vel)):
            m.nextFrame()
        m.remove(i)
        exist=os.path.isfile(file_img_temp)
        if exist==True:
            os.remove(file_img_temp)
        else:
            pass

    
    if file_salva=="":
        perc_temp=lista_img[0]
        perc_temp=perc_temp[:-8]
        perc=perc_temp+".swf"
        file_salva=perc
    else:
        pass

    m.save(file_salva)

    for ind_temp in range(0, len(lista_img)):
        file_img_temp=lista_img[ind_temp]
        exist=os.path.isfile(file_img_temp)
        if exist==True:
            os.remove(file_img_temp)
        else:
            pass

    fine()

def fine():
    pdb.gimp_message("Operation completed succesfully")



if __name__ == '__main__':

    register(
        "py_typewriter",
        N_("Flash movie generator with \ntypewriter effect on text"),
        "Effect typewriter",
        "Marco Crippa",
        "Marco Crippa",
        "2007",
        N_("Typewriter"),
        "",
       [
            (PF_STRING, "testo", "Text:", ""),
            (PF_COLOR, "colore", "Color:", (0,0,0)),
            (PF_FONT, "font","Font:", "Verdana"),
            (PF_SPINNER, "font_size", "Font Size:", 12, (0, 9999999999, 1)),
            (PF_BOOL,"antialias", "Antialias:", 0),
            (PF_STRING, "velocita",  "Velocity:", "8"),
            (PF_STRING, "base_nome",  "File name:", ""),
            (PF_DIRNAME, "dir_save", "Save in:", ""),
       ],
       [],
        Text,
        menu="<Toolbox>/Xtns/Typewriter")

main()


