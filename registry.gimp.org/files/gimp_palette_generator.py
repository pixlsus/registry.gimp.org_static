#!/usr/bin/env python
#******************************#
#Marco Crippa                  #
#http://krypt77.altervista.org/#
#******************************#

# *************************************************************************** #
#                                                                             #
#      Version 0.1 - 2008-02-08                                               #
#      Copyright (C) 2008 Marco Crippa                                        #
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

import os,colorsys,math,string,pango
import pygtk
pygtk.require('2.0')
import gtk, gobject
import grapefruit
from gimpfu import *

global armonia
armonia="Mono"
global ruota
ruota="RGB"
global col_primario
global col_complementare
global col_secondarioA
global col_secondarioB
col_primario=""
col_complementare=""
col_secondarioA=""
col_secondarioB=""
global page_attiva
page_attiva=0
global percorso
percorso=""

version=gimp.version

if version[0]>1 and version[1]>4:
    percorso="<Toolbox>/Colors"
else:
    percorso="<Toolbox>/Xtns"

def palette():
    Color_palette()
    gtk.main()


class Color_palette: 

    def __init__(self): 
        #contenitori
        self.contenitore_gen = gtk.VBox()
        self.box_gen = gtk.VBox()
        self.box_colore = gtk.HBox(True,0)
        self.box_ruota = gtk.HBox(True,0)
        self.box_armonia = gtk.HBox(True,0)
        self.box_colore_base = gtk.HBox(True,0)
        self.box_colore_compl = gtk.HBox(True,0)
        self.box_colore_sec0l = gtk.HBox(True,0)
        self.box_colore_sec02 = gtk.HBox(True,0)
        self.box_web = gtk.HBox(True,0)
        self.export = gtk.HBox(True,0)
        self.box_colore.set_size_request(750,250)
        
        self.contenitore_gen.pack_start(self.box_gen, False, False, 0)
        self.contenitore_gen.pack_start(self.box_colore, False, True, 3)
        self.contenitore_gen.pack_start(self.box_ruota, False, True, 5)
        self.contenitore_gen.pack_start(self.box_armonia, False, True, 3)
        self.contenitore_gen.pack_start(self.box_colore_base, False, True, 3)
        self.contenitore_gen.pack_start(self.box_colore_compl, False, True, 3)
        self.contenitore_gen.pack_start(self.box_colore_sec0l, False, True, 3)
        self.contenitore_gen.pack_start(self.box_colore_sec02, False, True, 3)
        self.contenitore_gen.pack_start(self.box_web, False, True, 3)
        self.contenitore_gen.pack_start(self.export, False, True, 3)
        
        #Creo la finestra
        self.win = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.win.connect("delete_event", self.delete_event)
        self.win.connect("destroy", self.destroy)
        self.win.set_title("Palette Generator 1.0")
        self.win.set_position(gtk.WIN_POS_CENTER)
        self.win.set_default_size(400, 400)
##        self.win.set_resizable(True)
        self.win.set_resizable(False)
        self.win.set_border_width(2)
        self.icon = self.win.render_icon(gtk.STOCK_NO, gtk.ICON_SIZE_BUTTON)
        self.win.set_icon(self.icon)        
        self.label = gtk.Label("")
        self.win.add(self.contenitore_gen)        
        self.box_gen.pack_start(self.label, True, True, 1)
        
        
        self.color=gtk.ColorSelection()
        self.color.set_has_palette(True)
        self.color.set_current_color(gtk.gdk.color_parse("#FF0000"))
##        self.color.set_has_opacity_control(True)
##        self.color.set_current_alpha(65535)
##        self.color.connect("color-changed", self.cambia_colore)
        self.color.connect("color-changed", self.cambia_colore)
        self.box_colore.pack_start(self.color, False, False, 1)


        #scelta tipo ruota
        label_ruota = gtk.Label("Choose wheel:")
        self.box_ruota.pack_start(label_ruota, False, False, 0)

        tipo_rgb = gtk.RadioButton(None, "RGB")
        tipo_rgb.connect("toggled", self.cambia_ruota, "RGB")
        tipo_rgb.set_active(True)
        self.box_ruota.pack_start(tipo_rgb, False, False, 0)
        tipo_ryb = gtk.RadioButton(tipo_rgb, "RYB")
        tipo_ryb.connect("toggled", self.cambia_ruota, "RYB")
        self.box_ruota.pack_start(tipo_ryb, False, False, 0)


        #scelta tipo armonia
        label_armonia = gtk.Label("Choose armony:\n\n")
        self.box_armonia.pack_start(label_armonia, False, False, 0)

        box_mono = gtk.VBox()
        label_mono=gtk.Label("")
        tipo_armonia = gtk.RadioButton(None, "Mono")
        tipo_armonia.connect("toggled", self.cambia_armonia, "Mono")
        tipo_armonia.set_active(True)
        box_mono.pack_start(tipo_armonia, False, False, 0)
        box_mono.pack_start(label_mono, False, False, 0)
        self.box_armonia.pack_start(box_mono, False, False, 0)

        box_complement = gtk.VBox()
        label_complement=gtk.Label("")
        tipo_complement = gtk.RadioButton(tipo_armonia, "Complement")
        tipo_complement.connect("toggled", self.cambia_armonia, "Complement")
        box_complement.pack_start(tipo_complement, False, False, 0)
        box_complement.pack_start(label_complement, False, False, 0)
        self.box_armonia.pack_start(box_complement, False, False, 0)

        box_triadic = gtk.VBox()
        start_triadic=gtk.Adjustment(120,0,120,1,10,0)
        self.angolo_triadic=gtk.SpinButton(start_triadic,1.0,0)
        self.angolo_triadic.connect("value-changed", self.cambia_colore)
        tipo_triad = gtk.RadioButton(tipo_armonia, "Triadic")
        tipo_triad.connect("toggled", self.cambia_armonia, "Triadic")
        box_triadic.pack_start(tipo_triad, False, False, 0)
        box_triadic.pack_start(self.angolo_triadic, False, False, 0)
        self.box_armonia.pack_start(box_triadic, False, False, 0)

        box_tetrad = gtk.VBox()
        start_tetrad=gtk.Adjustment(30,5,90,1,10,0)
        self.angolo_tetrad=gtk.SpinButton(start_tetrad,1.0,0)
        self.angolo_tetrad.connect("value-changed", self.cambia_colore)
        tipo_tetrad = gtk.RadioButton(tipo_armonia, "Tetradic")
        tipo_tetrad.connect("toggled", self.cambia_armonia, "Tetradic")
        box_tetrad.pack_start(tipo_tetrad, False, False, 0)
        box_tetrad.pack_start(self.angolo_tetrad, False, False, 0)
        self.box_armonia.pack_start(box_tetrad, False, False, 0)

        box_analogous = gtk.VBox()
        start_analogous=gtk.Adjustment(30,0,180,1,10,0)
        self.angolo_analogous=gtk.SpinButton(start_analogous,1.0,0)
        self.angolo_analogous.connect("value-changed", self.cambia_colore)
        tipo_analogous = gtk.RadioButton(tipo_armonia, "Analogous")
        tipo_analogous.connect("toggled", self.cambia_armonia, "Analogous")
        box_analogous.pack_start(tipo_analogous, False, False, 0)
        box_analogous.pack_start(self.angolo_analogous, False, False, 0)
        self.box_armonia.pack_start(box_analogous, False, False, 0)

        #box colore base
        self.color01 = gtk.EventBox()
        self.color01.set_size_request(30,30)
##        self.color01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#000000"))
        self.box_colore_base.pack_start(self.color01, False, True, 1)
        self.label_c01=gtk.Label("#000000")
        self.label_c01.set_selectable(True)
        self.box_colore_base.pack_start(self.label_c01, False, True, 1)
        self.color02 = gtk.EventBox()
        self.color02.set_size_request(30,30)
##        self.color02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#000000"))
        self.box_colore_base.pack_start(self.color02, False, True, 1)
        self.label_c02=gtk.Label("#000000")
        self.label_c02.set_selectable(True)
        self.box_colore_base.pack_start(self.label_c02, False, True, 1)
        self.color03 = gtk.EventBox()
        self.color03.set_size_request(30,30)
##        self.color03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#000000"))
        self.box_colore_base.pack_start(self.color03, False, True, 1)
        self.label_c03=gtk.Label("#000000")
        self.label_c03.set_selectable(True)
        self.box_colore_base.pack_start(self.label_c03, False, True, 1)
        self.color04 = gtk.EventBox()
        self.color04.set_size_request(30,30)
##        self.color04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#000000"))
        self.box_colore_base.pack_start(self.color04, False, True, 1)
        self.label_c04=gtk.Label("#000000")
        self.label_c04.set_selectable(True)
        self.box_colore_base.pack_start(self.label_c04, False, True, 1)
        self.color05 = gtk.EventBox()
        self.color05.set_size_request(30,30)
##        self.color05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#000000"))
        self.box_colore_base.pack_start(self.color05, False, True, 1)
        self.label_c05=gtk.Label("#000000")
        self.label_c05.set_selectable(True)
        self.box_colore_base.pack_start(self.label_c05, False, True, 1)

        #box colore complementare
        self.color06 = gtk.EventBox()
        self.box_colore_compl.pack_start(self.color06, False, True, 1)
        self.label_c06=gtk.Label("")
        self.label_c06.set_selectable(True)
        self.box_colore_compl.pack_start(self.label_c06, False, True, 1)
        self.color07 = gtk.EventBox()
        self.box_colore_compl.pack_start(self.color07, False, True, 1)
        self.label_c07=gtk.Label("")
        self.label_c07.set_selectable(True)
        self.box_colore_compl.pack_start(self.label_c07, False, True, 1)
        self.color08 = gtk.EventBox()
        self.box_colore_compl.pack_start(self.color08, False, True, 1)
        self.label_c08=gtk.Label("")
        self.label_c08.set_selectable(True)
        self.box_colore_compl.pack_start(self.label_c08, False, True, 1)
        self.color09 = gtk.EventBox()
        self.box_colore_compl.pack_start(self.color09, False, True, 1)
        self.label_c09=gtk.Label("")
        self.label_c09.set_selectable(True)
        self.box_colore_compl.pack_start(self.label_c09, False, True, 1)
        self.color10 = gtk.EventBox()
        self.box_colore_compl.pack_start(self.color10, False, True, 1)
        self.label_c10=gtk.Label("")
        self.label_c10.set_selectable(True)
        self.box_colore_compl.pack_start(self.label_c10, False, True, 1)

        #box colore secondario 1
        self.color11 = gtk.EventBox()
        self.box_colore_sec0l.pack_start(self.color11, False, True, 1)
        self.label_c11=gtk.Label("")
        self.label_c11.set_selectable(True)
        self.box_colore_sec0l.pack_start(self.label_c11, False, True, 1)
        self.color12 = gtk.EventBox()
        self.box_colore_sec0l.pack_start(self.color12, False, True, 1)
        self.label_c12=gtk.Label("")
        self.label_c12.set_selectable(True)
        self.box_colore_sec0l.pack_start(self.label_c12, False, True, 1)
        self.color13 = gtk.EventBox()
        self.box_colore_sec0l.pack_start(self.color13, False, True, 1)
        self.label_c13=gtk.Label("")
        self.label_c13.set_selectable(True)
        self.box_colore_sec0l.pack_start(self.label_c13, False, True, 1)
        self.color14 = gtk.EventBox()
        self.box_colore_sec0l.pack_start(self.color14, False, True, 1)
        self.label_c14=gtk.Label("")
        self.label_c14.set_selectable(True)
        self.box_colore_sec0l.pack_start(self.label_c14, False, True, 1)
        self.color15 = gtk.EventBox()
        self.box_colore_sec0l.pack_start(self.color15, False, True, 1)
        self.label_c15=gtk.Label("")
        self.label_c15.set_selectable(True)
        self.box_colore_sec0l.pack_start(self.label_c15, False, True, 1)

        #box colore secondario 2
        self.color16 = gtk.EventBox()
        self.box_colore_sec02.pack_start(self.color16, False, True, 1)
        self.label_c16=gtk.Label("")
        self.label_c16.set_selectable(True)
        self.box_colore_sec02.pack_start(self.label_c16, False, True, 1)
        self.color17 = gtk.EventBox()
        self.box_colore_sec02.pack_start(self.color17, False, True, 1)
        self.label_c17=gtk.Label("")
        self.label_c17.set_selectable(True)
        self.box_colore_sec02.pack_start(self.label_c17, False, True, 1)
        self.color18 = gtk.EventBox()
        self.box_colore_sec02.pack_start(self.color18, False, True, 1)
        self.label_c18=gtk.Label("")
        self.label_c18.set_selectable(True)
        self.box_colore_sec02.pack_start(self.label_c18, False, True, 1)
        self.color19 = gtk.EventBox()
        self.box_colore_sec02.pack_start(self.color19, False, True, 1)
        self.label_c19=gtk.Label("")
        self.label_c19.set_selectable(True)
        self.box_colore_sec02.pack_start(self.label_c19, False, True, 1)
        self.color20 = gtk.EventBox()
        self.box_colore_sec02.pack_start(self.color20, False, True, 1)
        self.label_c20=gtk.Label("")
        self.label_c20.set_selectable(True)
        self.box_colore_sec02.pack_start(self.label_c20, False, True, 1)

        #box web safe
        self.web=gtk.CheckButton("Color 'web safe'")
        self.web.set_active(False)
        self.web.connect("toggled", self.cambia_colore)
        self.box_web.pack_start(self.web, False, True, 1)
        
        label_dark=gtk.Label("Darker")
        start_dark=gtk.Adjustment(0,-100,100,1,10,0)
        self.dark=gtk.SpinButton(start_dark,1.0,0)
        self.dark.connect("value-changed", self.cambia_colore)
        box_dark = gtk.HBox(True,0)
        box_dark.pack_start(label_dark, False, True, 1)
        box_dark.pack_start(self.dark, False, True, 1)
        self.box_web.pack_start(box_dark, False, True, 1)

        box_es = gtk.HBox(False, 0)
        image_es = gtk.Image()
        image_es.set_from_stock(gtk.STOCK_INDEX,gtk.ICON_SIZE_BUTTON)
        label_es = gtk.Label()
        label_es.set_text("Example page")
        box_es.pack_start(image_es, False, False, 1)
        box_es.pack_start(label_es, False, False, 1)
        carica_es = gtk.Button()
        carica_es.connect("clicked", self.Pagina_test)
        carica_es.set_name("aggiungi")
        carica_es.add(box_es)
        self.box_web.pack_start(carica_es, True, True, 1)


        #salva palette gimp
        label_gimp = gtk.Label("Save Gimp/Inkscape palette")
        self.export.pack_start(label_gimp, False, True, 1)
        box_save = gtk.HBox(False, 0)
        image_save = gtk.Image()
        image_save.set_from_stock(gtk.STOCK_SAVE,gtk.ICON_SIZE_BUTTON)
        label_save = gtk.Label("Save palette")
        box_save.pack_start(image_save, False, False, 1)
        box_save.pack_start(label_save, False, False, 1)
        self.salva_gimp = gtk.Button()
        self.salva_gimp.connect("clicked", self.salva_palette)
        self.salva_gimp.set_size_request(296,31)
        self.salva_gimp.add(box_save)
        self.export.pack_start(self.salva_gimp, False, True, 1)

        self.cambia_colore(self)
        self.box_colore.show_all()
        self.box_ruota.show_all()
        self.box_armonia.show_all()
        self.box_colore_base.show_all()
        self.box_web.show_all()
        self.export.show_all()
        self.contenitore_gen.show()
        self.win.show()

    def delete_event(self, widget, event, data=None):
        return False
    
    def destroy(self, widget, data=None):
        self.win.destroy()
        return gtk.main_quit()

    def destroy_page(self, widget, data=None):
        global page_attiva
        
        page_attiva=0
        self.win_pages.destroy()

    def cambia_colore(self,event):
        global armonia
        global ruota
        global col_primario
        global col_complementare
        global col_secondarioA
        global col_secondarioB
        
        colore_base = self.color.get_current_color()
        
        rosso=(colore_base.red/65535.0)
        verde=(colore_base.green/65535.0)
        blu=(colore_base.blue/65535.0)
        
        col_primario = grapefruit.Color.NewFromRgb(rosso,verde,blu)

        if self.web.get_active()==True:
            col_primario1=col_primario.RgbToWebSafe(col_primario.rgb[0],col_primario.rgb[1],col_primario.rgb[2], False)
            col_primario = grapefruit.Color.NewFromRgb(col_primario1[0],col_primario1[1],col_primario1[2])
        else:
            pass

        self.c01_hex=col_primario.RgbToHtml(col_primario.rgb[0],col_primario.rgb[1],col_primario.rgb[2])
        self.label_c01.set_text(self.c01_hex.upper())
        self.color01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))

        if armonia=="Mono":
            #colore primario
            data="primario"
            self.cambia_colori(data)

        elif armonia=="Complement":

            if ruota=="RGB":
                col_complementare=col_primario.ComplementaryColor('rgb')
            elif ruota=="RYB":
                col_complementare=col_primario.ComplementaryColor('ryb')
            else:
                pass
            #colore primario
            data="primario"
            self.cambia_colori(data)

            #colore complementare
            self.c06_hex=col_complementare.RgbToHtml(col_complementare.rgb[0],col_complementare.rgb[1],col_complementare.rgb[2])
            self.label_c06.set_text(self.c06_hex.upper())
            self.color06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))

            data="complementare"
            self.cambia_colori(data)

        elif armonia=="Triadic":
            ang=self.angolo_triadic.get_value()
            if ruota=="RGB":
                triadic=col_primario.TriadicScheme(ang,'rgb')
            elif ruota=="RYB":
                triadic=col_primario.TriadicScheme(ang,'ryb')
            else:
                pass
            
            #colore primario
            data="primario"
            self.cambia_colori(data)

            #colore secondario A
            self.c11_hex=triadic[0].RgbToHtml(triadic[0].rgb[0],triadic[0].rgb[1],triadic[0].rgb[2])
            self.label_c11.set_text(self.c11_hex.upper())
            self.color11.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))

            col_secondarioA = grapefruit.Color.NewFromRgb(triadic[0].rgb[0], triadic[0].rgb[1], triadic[0].rgb[2])
            data="secondarioA"
            self.cambia_colori(data)

            #colore secondario B
            self.c16_hex=triadic[1].RgbToHtml(triadic[1].rgb[0],triadic[1].rgb[1],triadic[1].rgb[2])
            self.label_c16.set_text(self.c16_hex.upper())
            self.color16.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))

            col_secondarioB = grapefruit.Color.NewFromRgb(triadic[1].rgb[0], triadic[1].rgb[1], triadic[1].rgb[2])
            data="secondarioB"
            self.cambia_colori(data)
            
        elif armonia=="Tetradic":
            ang=self.angolo_tetrad.get_value()

            if ruota=="RGB":
                tetradic=col_primario.TetradicScheme(ang,'rgb')
            elif ruota=="RYB":
                tetradic=col_primario.TetradicScheme(ang,'ryb')
            else:
                pass
            
            #colore primario
            data="primario"
            self.cambia_colori(data)

            #colore complementare
            self.c06_hex=tetradic[1].RgbToHtml(tetradic[1].rgb[0],tetradic[1].rgb[1],tetradic[1].rgb[2])
            self.label_c06.set_text(self.c06_hex.upper())
            self.color06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))

            col_complementare = grapefruit.Color.NewFromRgb(tetradic[1].rgb[0], tetradic[1].rgb[1], tetradic[1].rgb[2])
            data="complementare"
            self.cambia_colori(data)

            #colore secondario A
            self.c11_hex=tetradic[0].RgbToHtml(tetradic[0].rgb[0],tetradic[0].rgb[1],tetradic[0].rgb[2])
            self.label_c11.set_text(self.c11_hex.upper())
            self.color11.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))

            col_secondarioA = grapefruit.Color.NewFromRgb(tetradic[0].rgb[0], tetradic[0].rgb[1], tetradic[0].rgb[2])
            data="secondarioA"
            self.cambia_colori(data)
            
            #colore secondario B
            self.c16_hex=tetradic[2].RgbToHtml(tetradic[2].rgb[0],tetradic[2].rgb[1],tetradic[2].rgb[2])
            self.label_c16.set_text(self.c16_hex.upper())
            self.color16.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))

            col_secondarioB = grapefruit.Color.NewFromRgb(tetradic[2].rgb[0], tetradic[2].rgb[1], tetradic[2].rgb[2])
            data="secondarioB"
            self.cambia_colori(data)

        elif armonia=="Analogous":
            ang=self.angolo_analogous.get_value()
            if ruota=="RGB":
                analogous=col_primario.AnalogousScheme(ang,'rgb')
            elif ruota=="RYB":
                analogous=col_primario.AnalogousScheme(ang,'ryb')
            else:
                pass
            
            #colore primario
            data="primario"
            self.cambia_colori(data)
            
            #colore secondario A
            self.c11_hex=analogous[0].RgbToHtml(analogous[0].rgb[0],analogous[0].rgb[1],analogous[0].rgb[2])
            self.label_c11.set_text(self.c11_hex.upper())
            self.color11.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))

            col_secondarioA = grapefruit.Color.NewFromRgb(analogous[0].rgb[0], analogous[0].rgb[1], analogous[0].rgb[2])
            data="secondarioA"
            self.cambia_colori(data)
            
            #colore secondario B
            self.c16_hex=analogous[1].RgbToHtml(analogous[1].rgb[0],analogous[1].rgb[1],analogous[1].rgb[2])
            self.label_c16.set_text(self.c16_hex.upper())
            self.color16.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))

            col_secondarioB = grapefruit.Color.NewFromRgb(analogous[1].rgb[0], analogous[1].rgb[1], analogous[1].rgb[2])
            data="secondarioB"
            self.cambia_colori(data)
        else:
            pass    
            
        self.refresh_page()

    def cambia_colori(self,data):
        global col_primario
        global col_complementare
        global col_secondarioA
        global col_secondarioB

        darker=abs(self.dark.get_value()/100.0)

        #luminosita
        if self.dark.get_value() >= 0:
            col_primario_temp = grapefruit.Color.NewFromRgb(col_primario.rgb[0], col_primario.rgb[1], col_primario.rgb[2])
            col_primario = grapefruit.Color.NewFromHsl(col_primario_temp.hsl[0], col_primario_temp.hsl[1], col_primario_temp.hsl[2]).DarkerColor(darker)
        else:
            col_primario_temp = grapefruit.Color.NewFromRgb(col_primario.rgb[0], col_primario.rgb[1], col_primario.rgb[2])
            col_primario = grapefruit.Color.NewFromHsl(col_primario_temp.hsl[0], col_primario_temp.hsl[1], col_primario_temp.hsl[2]).LighterColor(darker)
        
        #web safe
        if self.web.get_active()==True:
            col_primario1=col_primario.RgbToWebSafe(col_primario.rgb[0],col_primario.rgb[1],col_primario.rgb[2], False)
            col_primario = grapefruit.Color.NewFromRgb(col_primario1[0],col_primario1[1],col_primario1[2])
        else:
            pass

        #colore primario
        mono_col1=col_primario.MonochromeScheme()

        #ordinamento per luminosita
        col_primari=[]
        col_primari.append((mono_col1[0].hsl[2],mono_col1[0]))
        col_primari.append((mono_col1[1].hsl[2],mono_col1[1]))
        col_primari.append((mono_col1[2].hsl[2],mono_col1[2]))
        col_primari.append((mono_col1[3].hsl[2],mono_col1[3]))
        col_primari.sort()

        self.c02_hex=mono_col1[0].RgbToHtml(col_primari[0][1].rgb[0],col_primari[0][1].rgb[1],col_primari[0][1].rgb[2])
        self.c03_hex=mono_col1[1].RgbToHtml(col_primari[1][1].rgb[0],col_primari[1][1].rgb[1],col_primari[1][1].rgb[2])
        self.c04_hex=mono_col1[2].RgbToHtml(col_primari[2][1].rgb[0],col_primari[2][1].rgb[1],col_primari[2][1].rgb[2])
        self.c05_hex=mono_col1[3].RgbToHtml(col_primari[3][1].rgb[0],col_primari[3][1].rgb[1],col_primari[3][1].rgb[2])

##        c02_hex=mono_col1[0].RgbToHtml(mono_col1[0].rgb[0],mono_col1[0].rgb[1],mono_col1[0].rgb[2])
##        c03_hex=mono_col1[1].RgbToHtml(mono_col1[1].rgb[0],mono_col1[1].rgb[1],mono_col1[1].rgb[2])
##        c04_hex=mono_col1[2].RgbToHtml(mono_col1[2].rgb[0],mono_col1[2].rgb[1],mono_col1[2].rgb[2])
##        c05_hex=mono_col1[3].RgbToHtml(mono_col1[3].rgb[0],mono_col1[3].rgb[1],mono_col1[3].rgb[2])

        self.color02.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c02_hex))
        self.color03.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c03_hex))
        self.color04.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c04_hex))
        self.color05.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c05_hex))

        self.label_c02.set_text(self.c02_hex.upper())
        self.label_c03.set_text(self.c03_hex.upper())
        self.label_c04.set_text(self.c04_hex.upper())
        self.label_c05.set_text(self.c05_hex.upper())

        if data=="complementare":
            #luminosita
            if self.dark.get_value() >= 0:
                col_complementare_temp = grapefruit.Color.NewFromRgb(col_complementare.rgb[0], col_complementare.rgb[1], col_complementare.rgb[2])
                col_complementare = grapefruit.Color.NewFromHsl(col_complementare_temp.hsl[0], col_complementare_temp.hsl[1], col_complementare_temp.hsl[2]).DarkerColor(darker)
            else:
                col_complementare_temp = grapefruit.Color.NewFromRgb(col_complementare.rgb[0], col_complementare.rgb[1], col_complementare.rgb[2])
                col_complementare = grapefruit.Color.NewFromHsl(col_complementare_temp.hsl[0], col_complementare_temp.hsl[1], col_complementare_temp.hsl[2]).LighterColor(darker)

            #colore complementare
            mono01=[]
            mono02=[]
            mono03=[]
            mono04=[]
            
            mono_col2=col_complementare.MonochromeScheme()
            
            #web safe
            if self.web.get_active()==True:
                mono_temp=mono_col2[0].RgbToWebSafe(mono_col2[0].rgb[0],mono_col2[0].rgb[1],mono_col2[0].rgb[2], False)
                mono01 = grapefruit.Color.NewFromRgb(mono_temp[0],mono_temp[1],mono_temp[2])
                mono_temp=mono_col2[0].RgbToWebSafe(mono_col2[1].rgb[0],mono_col2[1].rgb[1],mono_col2[1].rgb[2], False)
                mono02 = grapefruit.Color.NewFromRgb(mono_temp[0],mono_temp[1],mono_temp[2])
                mono_temp=mono_col2[0].RgbToWebSafe(mono_col2[2].rgb[0],mono_col2[2].rgb[1],mono_col2[2].rgb[2], False)
                mono03 = grapefruit.Color.NewFromRgb(mono_temp[0],mono_temp[1],mono_temp[2])
                mono_temp=mono_col2[0].RgbToWebSafe(mono_col2[3].rgb[0],mono_col2[3].rgb[1],mono_col2[3].rgb[2], False)
                mono04 = grapefruit.Color.NewFromRgb(mono_temp[0],mono_temp[1],mono_temp[2])
            else:
                mono01=grapefruit.Color.NewFromRgb(mono_col2[0].rgb[0],mono_col2[0].rgb[1],mono_col2[0].rgb[2])
                mono02=grapefruit.Color.NewFromRgb(mono_col2[1].rgb[0],mono_col2[1].rgb[1],mono_col2[1].rgb[2])
                mono03=grapefruit.Color.NewFromRgb(mono_col2[2].rgb[0],mono_col2[2].rgb[1],mono_col2[2].rgb[2])
                mono04=grapefruit.Color.NewFromRgb(mono_col2[3].rgb[0],mono_col2[3].rgb[1],mono_col2[3].rgb[2])

            #ordinamento per luminosita
            col_compl=[]
            col_compl.append((mono01.hsl[2],mono01))
            col_compl.append((mono02.hsl[2],mono02))
            col_compl.append((mono03.hsl[2],mono03))
            col_compl.append((mono04.hsl[2],mono04))
            col_compl.sort()

            self.c07_hex=mono_col2[0].RgbToHtml(col_compl[0][1].rgb[0],col_compl[0][1].rgb[1],col_compl[0][1].rgb[2])
            self.c08_hex=mono_col2[1].RgbToHtml(col_compl[1][1].rgb[0],col_compl[1][1].rgb[1],col_compl[1][1].rgb[2])
            self.c09_hex=mono_col2[2].RgbToHtml(col_compl[2][1].rgb[0],col_compl[2][1].rgb[1],col_compl[2][1].rgb[2])
            self.c10_hex=mono_col2[3].RgbToHtml(col_compl[3][1].rgb[0],col_compl[3][1].rgb[1],col_compl[3][1].rgb[2])

##            c07_hex=mono01.RgbToHtml(mono01.rgb[0],mono01.rgb[1],mono01.rgb[2])
##            c08_hex=mono02.RgbToHtml(mono02.rgb[0],mono02.rgb[1],mono02.rgb[2])
##            c09_hex=mono03.RgbToHtml(mono03.rgb[0],mono03.rgb[1],mono03.rgb[2])
##            c10_hex=mono04.RgbToHtml(mono04.rgb[0],mono04.rgb[1],mono04.rgb[2])

            self.color07.set_size_request(30,30)
            self.color08.set_size_request(30,30)
            self.color09.set_size_request(30,30)
            self.color10.set_size_request(30,30)

            self.color07.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c07_hex))
            self.color08.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c08_hex))
            self.color09.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c09_hex))
            self.color10.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c10_hex))

            self.label_c07.set_text(self.c07_hex.upper())
            self.label_c08.set_text(self.c08_hex.upper())
            self.label_c09.set_text(self.c09_hex.upper())
            self.label_c10.set_text(self.c10_hex.upper())

        if data=="secondarioA":
            #luminosita
            if self.dark.get_value() >= 0:
                col_secondarioA_temp = grapefruit.Color.NewFromRgb(col_secondarioA.rgb[0], col_secondarioA.rgb[1], col_secondarioA.rgb[2])
                col_secondarioA = grapefruit.Color.NewFromHsl(col_secondarioA_temp.hsl[0], col_secondarioA_temp.hsl[1], col_secondarioA_temp.hsl[2]).DarkerColor(darker)
            else:
                col_secondarioA_temp = grapefruit.Color.NewFromRgb(col_secondarioA.rgb[0], col_secondarioA.rgb[1], col_secondarioA.rgb[2])
                col_secondarioA = grapefruit.Color.NewFromHsl(col_secondarioA_temp.hsl[0], col_secondarioA_temp.hsl[1], col_secondarioA_temp.hsl[2]).LighterColor(darker)
            #colore secondario A
            secA01=[]
            secA02=[]
            secA03=[]
            secA04=[]
            
            mono_col3=col_secondarioA.MonochromeScheme()
            
            # web safe
            if self.web.get_active()==True:
                secA_temp=mono_col3[0].RgbToWebSafe(mono_col3[0].rgb[0],mono_col3[0].rgb[1],mono_col3[0].rgb[2], False)
                secA01 = grapefruit.Color.NewFromRgb(secA_temp[0],secA_temp[1],secA_temp[2])
                secA_temp=mono_col3[0].RgbToWebSafe(mono_col3[1].rgb[0],mono_col3[1].rgb[1],mono_col3[1].rgb[2], False)
                secA02 = grapefruit.Color.NewFromRgb(secA_temp[0],secA_temp[1],secA_temp[2])
                secA_temp=mono_col3[0].RgbToWebSafe(mono_col3[2].rgb[0],mono_col3[2].rgb[1],mono_col3[2].rgb[2], False)
                secA03 = grapefruit.Color.NewFromRgb(secA_temp[0],secA_temp[1],secA_temp[2])
                secA_temp=mono_col3[0].RgbToWebSafe(mono_col3[3].rgb[0],mono_col3[3].rgb[1],mono_col3[3].rgb[2], False)
                secA04 = grapefruit.Color.NewFromRgb(secA_temp[0],secA_temp[1],secA_temp[2])
            else:
                secA01=grapefruit.Color.NewFromRgb(mono_col3[0].rgb[0],mono_col3[0].rgb[1],mono_col3[0].rgb[2])
                secA02=grapefruit.Color.NewFromRgb(mono_col3[1].rgb[0],mono_col3[1].rgb[1],mono_col3[1].rgb[2])
                secA03=grapefruit.Color.NewFromRgb(mono_col3[2].rgb[0],mono_col3[2].rgb[1],mono_col3[2].rgb[2])
                secA04=grapefruit.Color.NewFromRgb(mono_col3[3].rgb[0],mono_col3[3].rgb[1],mono_col3[3].rgb[2])

            #ordinamento per luminosita
            col_secA=[]
            col_secA.append((secA01.hsl[2],secA01))
            col_secA.append((secA02.hsl[2],secA02))
            col_secA.append((secA03.hsl[2],secA03))
            col_secA.append((secA04.hsl[2],secA04))
            col_secA.sort()

            self.c12_hex=mono_col3[0].RgbToHtml(col_secA[0][1].rgb[0],col_secA[0][1].rgb[1],col_secA[0][1].rgb[2])
            self.c13_hex=mono_col3[1].RgbToHtml(col_secA[1][1].rgb[0],col_secA[1][1].rgb[1],col_secA[1][1].rgb[2])
            self.c14_hex=mono_col3[2].RgbToHtml(col_secA[2][1].rgb[0],col_secA[2][1].rgb[1],col_secA[2][1].rgb[2])
            self.c15_hex=mono_col3[3].RgbToHtml(col_secA[3][1].rgb[0],col_secA[3][1].rgb[1],col_secA[3][1].rgb[2])
            
##            c12_hex=secA01.RgbToHtml(secA01.rgb[0],secA01.rgb[1],secA01.rgb[2])
##            c13_hex=secA02.RgbToHtml(secA02.rgb[0],secA02.rgb[1],secA02.rgb[2])
##            c14_hex=secA03.RgbToHtml(secA03.rgb[0],secA03.rgb[1],secA03.rgb[2])
##            c15_hex=secA04.RgbToHtml(secA04.rgb[0],secA04.rgb[1],secA04.rgb[2])

            self.color12.set_size_request(30,30)
            self.color13.set_size_request(30,30)
            self.color14.set_size_request(30,30)
            self.color15.set_size_request(30,30)

            self.color12.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c12_hex))
            self.color13.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c13_hex))
            self.color14.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c14_hex))
            self.color15.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c15_hex))

            self.label_c12.set_text(self.c12_hex.upper())
            self.label_c13.set_text(self.c13_hex.upper())
            self.label_c14.set_text(self.c14_hex.upper())
            self.label_c15.set_text(self.c15_hex.upper())

        if data=="secondarioB":
            #luminosita
            if self.dark.get_value() >= 0:
                col_secondarioB_temp = grapefruit.Color.NewFromRgb(col_secondarioB.rgb[0], col_secondarioB.rgb[1], col_secondarioB.rgb[2])
                col_secondarioB = grapefruit.Color.NewFromHsl(col_secondarioB_temp.hsl[0], col_secondarioB_temp.hsl[1], col_secondarioB_temp.hsl[2]).DarkerColor(darker)
            else:
                col_secondarioB_temp = grapefruit.Color.NewFromRgb(col_secondarioB.rgb[0], col_secondarioB.rgb[1], col_secondarioB.rgb[2])
                col_secondarioB = grapefruit.Color.NewFromHsl(col_secondarioB_temp.hsl[0], col_secondarioB_temp.hsl[1], col_secondarioB_temp.hsl[2]).LighterColor(darker)
            #colore secondario B
            secB01=[]
            secB02=[]
            secB03=[]
            secB04=[]
            
            mono_col4=col_secondarioB.MonochromeScheme()
            
            #web safe
            if self.web.get_active()==True:
                secB_temp=mono_col4[0].RgbToWebSafe(mono_col4[0].rgb[0],mono_col4[0].rgb[1],mono_col4[0].rgb[2], False)
                secB01 = grapefruit.Color.NewFromRgb(secB_temp[0],secB_temp[1],secB_temp[2])
                secB_temp=mono_col4[0].RgbToWebSafe(mono_col4[1].rgb[0],mono_col4[1].rgb[1],mono_col4[1].rgb[2], False)
                secB02 = grapefruit.Color.NewFromRgb(secB_temp[0],secB_temp[1],secB_temp[2])
                secB_temp=mono_col4[0].RgbToWebSafe(mono_col4[2].rgb[0],mono_col4[2].rgb[1],mono_col4[2].rgb[2], False)
                secB03 = grapefruit.Color.NewFromRgb(secB_temp[0],secB_temp[1],secB_temp[2])
                secB_temp=mono_col4[0].RgbToWebSafe(mono_col4[3].rgb[0],mono_col4[3].rgb[1],mono_col4[3].rgb[2], False)
                secB04 = grapefruit.Color.NewFromRgb(secB_temp[0],secB_temp[1],secB_temp[2])
            else:
                secB01=grapefruit.Color.NewFromRgb(mono_col4[0].rgb[0],mono_col4[0].rgb[1],mono_col4[0].rgb[2])
                secB02=grapefruit.Color.NewFromRgb(mono_col4[1].rgb[0],mono_col4[1].rgb[1],mono_col4[1].rgb[2])
                secB03=grapefruit.Color.NewFromRgb(mono_col4[2].rgb[0],mono_col4[2].rgb[1],mono_col4[2].rgb[2])
                secB04=grapefruit.Color.NewFromRgb(mono_col4[3].rgb[0],mono_col4[3].rgb[1],mono_col4[3].rgb[2])

            #ordinamento per luminosita
            col_secB=[]
            col_secB.append((secB01.hsl[2],secB01))
            col_secB.append((secB02.hsl[2],secB02))
            col_secB.append((secB03.hsl[2],secB03))
            col_secB.append((secB04.hsl[2],secB04))
            col_secB.sort()

            self.c17_hex=mono_col4[0].RgbToHtml(col_secB[0][1].rgb[0],col_secB[0][1].rgb[1],col_secB[0][1].rgb[2])
            self.c18_hex=mono_col4[1].RgbToHtml(col_secB[1][1].rgb[0],col_secB[1][1].rgb[1],col_secB[1][1].rgb[2])
            self.c19_hex=mono_col4[2].RgbToHtml(col_secB[2][1].rgb[0],col_secB[2][1].rgb[1],col_secB[2][1].rgb[2])
            self.c20_hex=mono_col4[3].RgbToHtml(col_secB[3][1].rgb[0],col_secB[3][1].rgb[1],col_secB[3][1].rgb[2])
            
##            c17_hex=secB01.RgbToHtml(secB01.rgb[0],secB01.rgb[1],secB01.rgb[2])
##            c18_hex=secB02.RgbToHtml(secB02.rgb[0],secB02.rgb[1],secB02.rgb[2])
##            c19_hex=secB03.RgbToHtml(secB03.rgb[0],secB03.rgb[1],secB03.rgb[2])
##            c20_hex=secB04.RgbToHtml(secB04.rgb[0],secB04.rgb[1],secB04.rgb[2])
            
            self.color17.set_size_request(30,30)
            self.color18.set_size_request(30,30)
            self.color19.set_size_request(30,30)
            self.color20.set_size_request(30,30)

            self.color17.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c17_hex))
            self.color18.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c18_hex))
            self.color19.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c19_hex))
            self.color20.modify_bg(gtk.STATE_NORMAL,gtk.gdk.color_parse(self.c20_hex))

            self.label_c17.set_text(self.c17_hex.upper())
            self.label_c18.set_text(self.c18_hex.upper())
            self.label_c19.set_text(self.c19_hex.upper())
            self.label_c20.set_text(self.c20_hex.upper())

        else:
            pass
            
    def cambia_armonia(self,widget,data):
        global armonia
        btn=widget.get_active()
        if btn==True:
            if data=="Mono":
                armonia="Mono"
            elif data=="Complement":
                armonia="Complement"
            elif data=="Triadic":
                armonia="Triadic"
            elif data=="Tetradic":
                armonia="Tetradic"
            elif data=="Analogous":
                armonia="Analogous"
        else:
            pass        
        self.cambia_colore(widget)
        self.refresh_win()
        return armonia

    def cambia_ruota(self,widget,data):
        global ruota
        btn=widget.get_active()
        if btn==True:
            if data=="RGB":
                ruota="RGB"
            elif data=="RYB":
                ruota="RYB"
        else:
            pass        

        self.cambia_colore(widget)
        return ruota

    def refresh_win(self):
        global armonia

        if armonia=="Mono":
            self.box_colore_compl.hide()
            self.box_colore_sec0l.hide()
            self.box_colore_sec02.hide()
        elif armonia=="Complement":
            self.box_colore_compl.show_all()
            self.box_colore_sec0l.hide()
            self.box_colore_sec02.hide()
        elif armonia=="Triadic":
            self.box_colore_compl.hide()
            self.box_colore_sec0l.show_all()
            self.box_colore_sec02.show_all()
        elif armonia=="Tetradic":
            self.box_colore_compl.show_all()
            self.box_colore_sec0l.show_all()
            self.box_colore_sec02.show_all()
        elif armonia=="Analogous":
            self.box_colore_sec0l.show_all()
            self.box_colore_sec02.show_all()
            self.box_colore_compl.hide()
        else:
            pass        

    def crea_palette(self,dir,nome_file,estensione):
        global armonia
        global col_primario
        global col_complementare
        global col_secondarioA
        global col_secondarioB
        
        tutto=""
        testo01="GIMP Palette\nName: "+nome_file+"\nColumns: 5\n#\n"

        #colore primario
        c01_rgb=int(round(col_primario.rgb[0]*255)),int(round(col_primario.rgb[1]*255)),int(round(col_primario.rgb[2]*255))
        mono_col1=col_primario.MonochromeScheme()
        c02_rgb=int(round(mono_col1[0].rgb[0]*255)),int(round(mono_col1[0].rgb[1]*255)),int(round(mono_col1[0].rgb[2]*255))
        c03_rgb=int(round(mono_col1[1].rgb[0]*255)),int(round(mono_col1[1].rgb[1]*255)),int(round(mono_col1[1].rgb[2]*255))
        c04_rgb=int(round(mono_col1[2].rgb[0]*255)),int(round(mono_col1[2].rgb[1]*255)),int(round(mono_col1[2].rgb[2]*255))
        c05_rgb=int(round(mono_col1[3].rgb[0]*255)),int(round(mono_col1[3].rgb[1]*255)),int(round(mono_col1[3].rgb[2]*255))
        
        testo_primario=str(c01_rgb[0])+" "+str(c01_rgb[1])+" "+str(c01_rgb[2])+" primario_01"+"\n"+str(c02_rgb[0])+" "+str(c02_rgb[1])+" "+str(c02_rgb[2])+" primario_02"+"\n"+str(c03_rgb[0])+" "+str(c03_rgb[1])+" "+str(c03_rgb[2])+" primario_03"+"\n"+str(c04_rgb[0])+" "+str(c04_rgb[1])+" "+str(c04_rgb[2])+" primario_04"+"\n"+str(c05_rgb[0])+" "+str(c05_rgb[1])+" "+str(c05_rgb[2])+" primario_05"+"\n"
        
        if armonia=="Mono":
            tutto=testo01+testo_primario

        elif armonia=="Complement":
            #colore complementare
            c06_rgb=int(round(col_complementare.rgb[0]*255)),int(round(col_complementare.rgb[1]*255)),int(round(col_complementare.rgb[2]*255))
            mono_col2=col_complementare.MonochromeScheme()
            c07_rgb=int(round(mono_col2[0].rgb[0]*255)),int(round(mono_col2[0].rgb[1]*255)),int(round(mono_col2[0].rgb[2]*255))
            c08_rgb=int(round(mono_col2[1].rgb[0]*255)),int(round(mono_col2[1].rgb[1]*255)),int(round(mono_col2[1].rgb[2]*255))
            c09_rgb=int(round(mono_col2[2].rgb[0]*255)),int(round(mono_col2[2].rgb[1]*255)),int(round(mono_col2[2].rgb[2]*255))
            c10_rgb=int(round(mono_col2[3].rgb[0]*255)),int(round(mono_col2[3].rgb[1]*255)),int(round(mono_col2[3].rgb[2]*255))

            testo_complementare=str(c06_rgb[0])+" "+str(c06_rgb[1])+" "+str(c06_rgb[2])+" complementare_01"+"\n"+str(c07_rgb[0])+" "+str(c07_rgb[1])+" "+str(c07_rgb[2])+" complementare_02"+"\n"+str(c08_rgb[0])+" "+str(c08_rgb[1])+" "+str(c08_rgb[2])+" complementare_03"+"\n"+str(c09_rgb[0])+" "+str(c09_rgb[1])+" "+str(c09_rgb[2])+" complementare_04"+"\n"+str(c10_rgb[0])+" "+str(c10_rgb[1])+" "+str(c10_rgb[2])+" complementare_05"+"\n"
            tutto=testo01+testo_primario+testo_complementare

        elif armonia=="Triadic" or armonia=="Analogous":
            #colore secondario A
            c11_rgb=int(round(col_secondarioA.rgb[0]*255)),int(round(col_secondarioA.rgb[1]*255)),int(round(col_secondarioA.rgb[2]*255))
            mono_col3=col_secondarioA.MonochromeScheme()
            c12_rgb=int(round(mono_col3[0].rgb[0]*255)),int(round(mono_col3[0].rgb[1]*255)),int(round(mono_col3[0].rgb[2]*255))
            c13_rgb=int(round(mono_col3[1].rgb[0]*255)),int(round(mono_col3[1].rgb[1]*255)),int(round(mono_col3[1].rgb[2]*255))
            c14_rgb=int(round(mono_col3[2].rgb[0]*255)),int(round(mono_col3[2].rgb[1]*255)),int(round(mono_col3[2].rgb[2]*255))
            c15_rgb=int(round(mono_col3[3].rgb[0]*255)),int(round(mono_col3[3].rgb[1]*255)),int(round(mono_col3[3].rgb[2]*255))

            testo_secondarioA=str(c11_rgb[0])+" "+str(c11_rgb[1])+" "+str(c11_rgb[2])+" secondario_A_01"+"\n"+str(c12_rgb[0])+" "+str(c12_rgb[1])+" "+str(c12_rgb[2])+" secondario_A_02"+"\n"+str(c13_rgb[0])+" "+str(c13_rgb[1])+" "+str(c13_rgb[2])+" secondario_A_03"+"\n"+str(c14_rgb[0])+" "+str(c14_rgb[1])+" "+str(c14_rgb[2])+" secondario_A_04"+"\n"+str(c15_rgb[0])+" "+str(c15_rgb[1])+" "+str(c15_rgb[2])+" secondario_A_05"+"\n"

            #colore secondario B
            c16_rgb=int(round(col_secondarioB.rgb[0]*255)),int(round(col_secondarioB.rgb[1]*255)),int(round(col_secondarioB.rgb[2]*255))
            mono_col4=col_secondarioB.MonochromeScheme()
            c17_rgb=int(round(mono_col4[0].rgb[0]*255)),int(round(mono_col4[0].rgb[1]*255)),int(round(mono_col4[0].rgb[2]*255))
            c18_rgb=int(round(mono_col4[1].rgb[0]*255)),int(round(mono_col4[1].rgb[1]*255)),int(round(mono_col4[1].rgb[2]*255))
            c19_rgb=int(round(mono_col4[2].rgb[0]*255)),int(round(mono_col4[2].rgb[1]*255)),int(round(mono_col4[2].rgb[2]*255))
            c20_rgb=int(round(mono_col4[3].rgb[0]*255)),int(round(mono_col4[3].rgb[1]*255)),int(round(mono_col4[3].rgb[2]*255))
            
            testo_secondarioB=str(c16_rgb[0])+" "+str(c16_rgb[1])+" "+str(c16_rgb[2])+" secondario_B_01"+"\n"+str(c17_rgb[0])+" "+str(c17_rgb[1])+" "+str(c17_rgb[2])+" secondario_B_02"+"\n"+str(c18_rgb[0])+" "+str(c18_rgb[1])+" "+str(c18_rgb[2])+" secondario_B_03"+"\n"+str(c19_rgb[0])+" "+str(c19_rgb[1])+" "+str(c19_rgb[2])+" secondario_B_04"+"\n"+str(c20_rgb[0])+" "+str(c20_rgb[1])+" "+str(c20_rgb[2])+" secondario_B_05"+"\n"

            tutto=testo01+testo_primario+testo_secondarioA+testo_secondarioB

        elif armonia=="Tetradic":
            #colore complementare
            c06_rgb=int(round(col_complementare.rgb[0]*255)),int(round(col_complementare.rgb[1]*255)),int(round(col_complementare.rgb[2]*255))
            mono_col2=col_complementare.MonochromeScheme()
            c07_rgb=int(round(mono_col2[0].rgb[0]*255)),int(round(mono_col2[0].rgb[1]*255)),int(round(mono_col2[0].rgb[2]*255))
            c08_rgb=int(round(mono_col2[1].rgb[0]*255)),int(round(mono_col2[1].rgb[1]*255)),int(round(mono_col2[1].rgb[2]*255))
            c09_rgb=int(round(mono_col2[2].rgb[0]*255)),int(round(mono_col2[2].rgb[1]*255)),int(round(mono_col2[2].rgb[2]*255))
            c10_rgb=int(round(mono_col2[3].rgb[0]*255)),int(round(mono_col2[3].rgb[1]*255)),int(round(mono_col2[3].rgb[2]*255))
            testo_complementare=str(c06_rgb[0])+" "+str(c06_rgb[1])+" "+str(c06_rgb[2])+" complementare_01"+"\n"+str(c07_rgb[0])+" "+str(c07_rgb[1])+" "+str(c07_rgb[2])+" complementare_02"+"\n"+str(c08_rgb[0])+" "+str(c08_rgb[1])+" "+str(c08_rgb[2])+" complementare_03"+"\n"+str(c09_rgb[0])+" "+str(c09_rgb[1])+" "+str(c09_rgb[2])+" complementare_04"+"\n"+str(c10_rgb[0])+" "+str(c10_rgb[1])+" "+str(c10_rgb[2])+" complementare_05"+"\n"

            #colore secondario A
            c11_rgb=int(round(col_secondarioA.rgb[0]*255)),int(round(col_secondarioA.rgb[1]*255)),int(round(col_secondarioA.rgb[2]*255))
            mono_col3=col_secondarioA.MonochromeScheme()
            c12_rgb=int(round(mono_col3[0].rgb[0]*255)),int(round(mono_col3[0].rgb[1]*255)),int(round(mono_col3[0].rgb[2]*255))
            c13_rgb=int(round(mono_col3[1].rgb[0]*255)),int(round(mono_col3[1].rgb[1]*255)),int(round(mono_col3[1].rgb[2]*255))
            c14_rgb=int(round(mono_col3[2].rgb[0]*255)),int(round(mono_col3[2].rgb[1]*255)),int(round(mono_col3[2].rgb[2]*255))
            c15_rgb=int(round(mono_col3[3].rgb[0]*255)),int(round(mono_col3[3].rgb[1]*255)),int(round(mono_col3[3].rgb[2]*255))
            testo_secondarioA=str(c11_rgb[0])+" "+str(c11_rgb[1])+" "+str(c11_rgb[2])+" secondario_A_01"+"\n"+str(c12_rgb[0])+" "+str(c12_rgb[1])+" "+str(c12_rgb[2])+" secondario_A_02"+"\n"+str(c13_rgb[0])+" "+str(c13_rgb[1])+" "+str(c13_rgb[2])+" secondario_A_03"+"\n"+str(c14_rgb[0])+" "+str(c14_rgb[1])+" "+str(c14_rgb[2])+" secondario_A_04"+"\n"+str(c15_rgb[0])+" "+str(c15_rgb[1])+" "+str(c15_rgb[2])+" secondario_A_05"+"\n"

            #colore secondario B
            c16_rgb=int(round(col_secondarioB.rgb[0]*255)),int(round(col_secondarioB.rgb[1]*255)),int(round(col_secondarioB.rgb[2]*255))
            mono_col4=col_secondarioB.MonochromeScheme()
            c17_rgb=int(round(mono_col4[0].rgb[0]*255)),int(round(mono_col4[0].rgb[1]*255)),int(round(mono_col4[0].rgb[2]*255))
            c18_rgb=int(round(mono_col4[1].rgb[0]*255)),int(round(mono_col4[1].rgb[1]*255)),int(round(mono_col4[1].rgb[2]*255))
            c19_rgb=int(round(mono_col4[2].rgb[0]*255)),int(round(mono_col4[2].rgb[1]*255)),int(round(mono_col4[2].rgb[2]*255))
            c20_rgb=int(round(mono_col4[3].rgb[0]*255)),int(round(mono_col4[3].rgb[1]*255)),int(round(mono_col4[3].rgb[2]*255))
            testo_secondarioB=str(c16_rgb[0])+" "+str(c16_rgb[1])+" "+str(c16_rgb[2])+" secondario_B_01"+"\n"+str(c17_rgb[0])+" "+str(c17_rgb[1])+" "+str(c17_rgb[2])+" secondario_B_02"+"\n"+str(c18_rgb[0])+" "+str(c18_rgb[1])+" "+str(c18_rgb[2])+" secondario_B_03"+"\n"+str(c19_rgb[0])+" "+str(c19_rgb[1])+" "+str(c19_rgb[2])+" secondario_B_04"+"\n"+str(c20_rgb[0])+" "+str(c20_rgb[1])+" "+str(c20_rgb[2])+" secondario_B_05"+"\n"

            tutto=testo01+testo_primario+testo_complementare+testo_secondarioA+testo_secondarioB

        else:
            pass      

        file_palette=os.path.join(dir,nome_file+estensione)
        file_palette_temp = open(file_palette, "w")
        file_palette_temp.write(tutto)
        file_palette_temp.close()    
        

 

    def salva_palette(self, *args):
        
        self.fin_salva_file = gtk.FileChooserDialog("Salva la palette...",None,gtk.FILE_CHOOSER_ACTION_SAVE,(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,gtk.STOCK_SAVE, gtk.RESPONSE_OK))
        self.fin_salva_file.set_default_response(gtk.RESPONSE_OK)

        filter = gtk.FileFilter()
        filter.set_name("Palette")
        filter.add_pattern("*.gpl")
        self.fin_salva_file.add_filter(filter)
        filter = gtk.FileFilter()
        filter.set_name("Tutti i file")
        filter.add_pattern("*")
        self.fin_salva_file.add_filter(filter)        

        response = self.fin_salva_file.run()
        if response == gtk.RESPONSE_OK:


            file_salva=self.fin_salva_file.get_filename()

            nome_f_temp=os.path.split(file_salva)
            dir=nome_f_temp[0]
            file_ext=os.path.splitext(nome_f_temp[1])
            nome_file=file_ext[0]
            if file_ext[1]=="" or file_ext[1]!=".gpl":
                estensione=".gpl"
            else:
                estensione=file_ext[1]
            
            self.crea_palette(dir,nome_file,estensione)

        elif response == gtk.RESPONSE_CANCEL:
            pass
        self.fin_salva_file.destroy()


    def Pagina_test(self,event):
        global page_attiva
        page_attiva=1

        box_titolo = gtk.HBox()
        box_menu = gtk.HBox()
        box_testo = gtk.HBox()
        box_sottomenu = gtk.HBox()

        
        #Creo la finestra
        self.win_pages = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.win_pages.connect("delete_event", self.delete_event)
        self.win_pages.connect("destroy", self.destroy_page)
        self.win_pages.set_title("Example page")
        self.win_pages.set_position(gtk.WIN_POS_CENTER)
        self.win_pages.set_default_size(400, 400)
        self.win_pages.set_resizable(False)
        icon = self.win_pages.render_icon(gtk.STOCK_NO, gtk.ICON_SIZE_BUTTON)
        self.win_pages.set_icon(icon)        
        
        self.fixed = gtk.Fixed()
        self.fixed.set_size_request(400,400)
        self.win_pages.add(self.fixed)


        #box titolo
        self.titolo = gtk.EventBox()
        self.titolo.set_size_request(400,60)
        self.titolo.add(box_titolo)
        self.fixed.put(self.titolo,0,1)

        b01 = gtk.Label("")
        b01.set_size_request(80,40)
        self.box_b01 = gtk.EventBox()
        self.box_b01.set_size_request(60,30)
        self.box_b01.add(b01)
        self.fixed.put(self.box_b01,5,5)

        b02 = gtk.Label("")
        b02.set_size_request(80,40)
        self.box_b02 = gtk.EventBox()
        self.box_b02.set_size_request(60,30)
        self.box_b02.add(b02)
        self.fixed.put(self.box_b02,105,5)

        b03 = gtk.Label("")
        b03.set_size_request(80,40)
        self.box_b03 = gtk.EventBox()
        self.box_b03.set_size_request(60,30)
        self.box_b03.add(b03)
        self.fixed.put(self.box_b03,205,5)

        b04 = gtk.Label("")
        b04.set_size_request(80,40)
        self.box_b04 = gtk.EventBox()
        self.box_b04.set_size_request(60,30)
        self.box_b04.add(b04)
        self.fixed.put(self.box_b04,305,5)

        self.label01 = gtk.Label("")
        self.box_label01 = gtk.EventBox()
        self.box_label01.add(self.label01)
        self.fixed.put(self.box_label01,10,40)

        #box menu
        menu = gtk.EventBox()
        menu.set_size_request(400,50)
        menu.add(box_menu)
        menu.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(menu,0,61)

        self.bm_01 = gtk.Label("")
        self.bm_01.set_size_request(100,40)
        self.box_bm_01 = gtk.EventBox()
        self.box_bm_01.set_size_request(90,30)
        self.box_bm_01.add(self.bm_01)
        self.fixed.put(self.box_bm_01,5,70)

        self.bm_02 = gtk.Label("")
        self.bm_02.set_size_request(80,40)
        self.box_bm_02 = gtk.EventBox()
        self.box_bm_02.set_size_request(100,30)
        self.box_bm_02.add(self.bm_02)
        self.fixed.put(self.box_bm_02,100,70)

        self.bm_03 = gtk.Label("")
        self.bm_03.set_size_request(80,40)
        self.box_bm_03 = gtk.EventBox()
        self.box_bm_03.set_size_request(90,30)
        self.box_bm_03.add(self.bm_03)
        self.fixed.put(self.box_bm_03,205,70)

        self.bm_04 = gtk.Label("")
        self.bm_04.set_size_request(80,40)
        self.box_bm_04 = gtk.EventBox()
        self.box_bm_04.set_size_request(90,30)
        self.box_bm_04.add(self.bm_04)
        self.fixed.put(self.box_bm_04,300,70)

        #box testo
        testo = gtk.EventBox()
        testo.set_size_request(400,290)
        testo.add(box_testo)
        testo.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(testo,0,111)

        self.label02 = gtk.Label("")
        self.box_label02 = gtk.EventBox()
        self.box_label02.add(self.label02)
        self.box_label02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label02,10,112)

        label03 = gtk.Label("1 Ze Luis\n2 Jesualdo\n5 Samuelao\n3 Carlson\n8 Ze Edmir\n6 Ze Nazario\n4 Paulo da Mata Neves\n10 Rafailton\n7 Rivelinho\n9 Denilton\n11 Jedaias")
        self.box_label03 = gtk.EventBox()
        self.box_label03.add(label03)
        self.box_label03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label03,15,130)

        self.colonna = gtk.EventBox()
        self.colonna.set_size_request(2,240)
        self.fixed.put(self.colonna,200,110)
        
        self.quad01 = gtk.EventBox()
        self.quad01.set_size_request(12,12)
        self.fixed.put(self.quad01,220,115)
        
        self.label04 = gtk.Label("")
        self.box_label04 = gtk.EventBox()
        self.box_label04.add(self.label04)
        self.box_label04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label04,240,112)

        label04a = gtk.Label("12 Joao Soares\n15 Ze Waldemar\n21 Rubao\n18 Samuelinho\n14 Agenores\n20 Nazario\n19 Socratinho")
        self.box_label04a = gtk.EventBox()
        self.box_label04a.add(label04a)
        self.box_label04a.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label04a,230,130)

        self.quad02 = gtk.EventBox()
        self.quad02.set_size_request(12,12)
        self.fixed.put(self.quad02,220,260)
        
        self.label05 = gtk.Label("")
        self.box_label05 = gtk.EventBox()
        self.box_label05.add(self.label05)
        self.box_label05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label05,240,258)

        label05a = gtk.Label("Felipe dos Santos")
        self.box_label05a = gtk.EventBox()
        self.box_label05a.add(label05a)
        self.box_label05a.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("#ffffff"))
        self.fixed.put(self.box_label05a,230,275)

        self.quad03 = gtk.EventBox()
        self.quad03.set_size_request(120,28)
        self.fixed.put(self.quad03,210,300)

        self.quad04 = gtk.EventBox()
        self.quad04.set_size_request(12,12)
        self.fixed.put(self.quad04,220,308)

        self.label06 = gtk.Label("")
        self.box_label06 = gtk.EventBox()
        self.box_label06.add(self.label06)
        self.fixed.put(self.box_label06,240,305)

        self.quad05 = gtk.EventBox()
        self.quad05.set_size_request(120,28)
        self.fixed.put(self.quad05,210,330)

        self.quad06 = gtk.EventBox()
        self.quad06.set_size_request(12,12)
        self.fixed.put(self.quad06,220,338)

        self.label07 = gtk.Label("")
        self.box_label07 = gtk.EventBox()
        self.box_label07.add(self.label07)
        self.fixed.put(self.box_label07,240,335)
        
        #box sottomenu
        self.righa_01 = gtk.EventBox()
        self.righa_01.set_size_request(400,3)
        self.fixed.put(self.righa_01,0,362)

        self.sottomenu = gtk.EventBox()
        self.sottomenu.set_size_request(400,30)
        self.fixed.put(self.sottomenu,0,365)

        self.righa_02 = gtk.EventBox()
        self.righa_02.set_size_request(400,3)
        self.fixed.put(self.righa_02,0,395)

        self.sbm_01 = gtk.Label("")
        self.sbm_01.set_size_request(80,40)
        self.sbm_01.set_use_underline(True)
        self.box_sbm_01 = gtk.EventBox()
        self.box_sbm_01.set_size_request(60,30)
        self.box_sbm_01.add(self.sbm_01)
        self.fixed.put(self.box_sbm_01,54,365)

        self.colonna01 = gtk.EventBox()
        self.colonna01.set_size_request(2,20)
        self.fixed.put(self.colonna01,122,370)

        self.sbm_02 = gtk.Label("")
        self.sbm_02.set_size_request(80,40)
        self.box_sbm_02 = gtk.EventBox()
        self.box_sbm_02.set_size_request(60,30)
        self.box_sbm_02.add(self.sbm_02)
        self.fixed.put(self.box_sbm_02,132,365)

        self.colonna02 = gtk.EventBox()
        self.colonna02.set_size_request(2,20)
        self.fixed.put(self.colonna02,200,370)

        self.sbm_03 = gtk.Label("")
        self.sbm_03.set_size_request(80,40)
        self.box_sbm_03 = gtk.EventBox()
        self.box_sbm_03.set_size_request(60,30)
        self.box_sbm_03.add(self.sbm_03)
        self.fixed.put(self.box_sbm_03,210,365)

        self.colonna03 = gtk.EventBox()
        self.colonna03.set_size_request(2,20)
        self.fixed.put(self.colonna03,278,370)

        self.sbm_04 = gtk.Label("")
        self.sbm_04.set_size_request(80,40)
        self.box_sbm_04 = gtk.EventBox()
        self.box_sbm_04.set_size_request(60,30)
        self.box_sbm_04.add(self.sbm_04)
        self.fixed.put(self.box_sbm_04,288,365)

        self.refresh_page()
        self.win_pages.show_all()


    def refresh_page(self):
        global page_attiva
        global armonia
        
        if page_attiva==0:
            pass
        else:

            if armonia=="Mono":
                self.titolo.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.box_b01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.box_b02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.box_b03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c04_hex))
                self.box_b04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c05_hex))
                self.label01.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>La Selecao!</b></span>")
                self.box_label01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.bm_01.set_markup("<span foreground=\""+self.c03_hex+"\" >Lineups</span>")
                self.box_bm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.bm_02.set_markup("<span foreground=\""+self.c02_hex+"\" >Report</span>")
                self.box_bm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.bm_03.set_markup("<span foreground=\""+self.c02_hex+"\" >Events</span>")
                self.box_bm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.bm_04.set_markup("<span foreground=\""+self.c02_hex+"\" >Statistics</span>")
                self.box_bm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.label02.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>Formation: 3-4-3</b></span>")
                self.colonna.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.quad01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.label04.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>Substitutes:</b></span>")
                self.quad02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.label05.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>Coach:</b></span>")
                self.quad03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.quad04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.label06.set_markup("<span foreground=\""+self.c02_hex+"\" >Match</span>")
                self.box_label06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.quad05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.quad06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.label07.set_markup("<span foreground=\""+self.c03_hex+"\" >Info</span>")
                self.box_label07.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.righa_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.sottomenu.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.righa_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.sbm_01.set_markup("<u><span foreground=\""+self.c02_hex+"\" ><b>Lineups</b></span></u>")
                self.box_sbm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.colonna01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.sbm_02.set_markup("<u><span foreground=\""+self.c02_hex+"\" ><b>Report</b></span></u>")
                self.box_sbm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.colonna02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.sbm_03.set_markup("<u><span foreground=\""+self.c02_hex+"\" ><b>Events</b></span></u>")
                self.box_sbm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.colonna03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.sbm_04.set_markup("<u><span foreground=\""+self.c02_hex+"\" ><b>Statistics</b></span></u>")
                self.box_sbm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))     

            elif armonia=="Complement":
                self.titolo.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.box_b01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.box_b02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.box_b03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c04_hex))
                self.box_b04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c05_hex))
                self.label01.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>La Selecao!</b></span>")
                self.box_label01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.bm_01.set_markup("<span foreground=\""+self.c03_hex+"\" >Lineups</span>")
                self.box_bm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.bm_02.set_markup("<span foreground=\""+self.c02_hex+"\" >Report</span>")
                self.box_bm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.bm_03.set_markup("<span foreground=\""+self.c02_hex+"\" >Events</span>")
                self.box_bm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.bm_04.set_markup("<span foreground=\""+self.c02_hex+"\" >Statistics</span>")
                self.box_bm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.label02.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>Formation: 3-4-3</b></span>")
                self.colonna.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.quad01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.label04.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>Substitutes:</b></span>")
                self.quad02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.label05.set_markup("<span foreground=\""+self.c07_hex+"\" ><b>Coach:</b></span>")
                self.quad03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.quad04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c08_hex))
                self.label06.set_markup("<span foreground=\""+self.c08_hex+"\" >Match</span>")
                self.box_label06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.quad05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.quad06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.label07.set_markup("<span foreground=\""+self.c07_hex+"\" >Info</span>")
                self.box_label07.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.righa_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.sottomenu.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.righa_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.sbm_01.set_markup("<u><span foreground=\""+self.c08_hex+"\" ><b>Lineups</b></span></u>")
                self.box_sbm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.colonna01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.sbm_02.set_markup("<u><span foreground=\""+self.c08_hex+"\" ><b>Report</b></span></u>")
                self.box_sbm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.colonna02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.sbm_03.set_markup("<u><span foreground=\""+self.c08_hex+"\" ><b>Events</b></span></u>")
                self.box_sbm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.colonna03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.sbm_04.set_markup("<u><span foreground=\""+self.c08_hex+"\" ><b>Statistics</b></span></u>")
                self.box_sbm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))     

            elif armonia=="Triadic" or armonia=="Analogous":
                self.titolo.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.box_b01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.box_b02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.box_b03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c04_hex))
                self.box_b04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c05_hex))
                self.label01.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>La Selecao!</b></span>")
                self.box_label01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.bm_01.set_markup("<span foreground=\""+self.c13_hex+"\" >Lineups</span>")
                self.box_bm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.bm_02.set_markup("<span foreground=\""+self.c12_hex+"\" >Report</span>")
                self.box_bm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.bm_03.set_markup("<span foreground=\""+self.c12_hex+"\" >Events</span>")
                self.box_bm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.bm_04.set_markup("<span foreground=\""+self.c12_hex+"\" >Statistics</span>")
                self.box_bm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.label02.set_markup("<span foreground=\""+self.c17_hex+"\" ><b>Formation: 3-4-3</b></span>")
                self.colonna.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c17_hex))
                self.quad01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.label04.set_markup("<span foreground=\""+self.c17_hex+"\" ><b>Substitutes:</b></span>")
                self.quad02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c17_hex))
                self.label05.set_markup("<span foreground=\""+self.c16_hex+"\" ><b>Coach:</b></span>")
                self.quad03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c18_hex))
                self.quad04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c17_hex))
                self.label06.set_markup("<span foreground=\""+self.c17_hex+"\" >Match</span>")
                self.box_label06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c18_hex))
                self.quad05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.quad06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c18_hex))
                self.label07.set_markup("<span foreground=\""+self.c18_hex+"\" >Info</span>")
                self.box_label07.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.righa_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))
                self.sottomenu.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.righa_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))
                self.sbm_01.set_markup("<u><span foreground=\""+self.c13_hex+"\" ><b>Lineups</b></span></u>")
                self.box_sbm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.colonna01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.sbm_02.set_markup("<u><span foreground=\""+self.c13_hex+"\" ><b>Report</b></span></u>")
                self.box_sbm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.colonna02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.sbm_03.set_markup("<u><span foreground=\""+self.c13_hex+"\" ><b>Events</b></span></u>")
                self.box_sbm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.colonna03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.sbm_04.set_markup("<u><span foreground=\""+self.c13_hex+"\" ><b>Statistics</b></span></u>")
                self.box_sbm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))     
                
            elif armonia=="Tetradic":
                self.titolo.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.box_b01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.box_b02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c03_hex))
                self.box_b03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c04_hex))
                self.box_b04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c05_hex))
                self.label01.set_markup("<span foreground=\""+self.c02_hex+"\" ><b>La Selecao!</b></span>")
                self.box_label01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c01_hex))
                self.bm_01.set_markup("<span foreground=\""+self.c13_hex+"\" >Lineups</span>")
                self.box_bm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.bm_02.set_markup("<span foreground=\""+self.c12_hex+"\" >Report</span>")
                self.box_bm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.bm_03.set_markup("<span foreground=\""+self.c12_hex+"\" >Events</span>")
                self.box_bm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.bm_04.set_markup("<span foreground=\""+self.c12_hex+"\" >Statistics</span>")
                self.box_bm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.label02.set_markup("<span foreground=\""+self.c16_hex+"\" ><b>Formation: 3-4-3</b></span>")
                self.colonna.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c02_hex))
                self.quad01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.label04.set_markup("<span foreground=\""+self.c16_hex+"\" ><b>Substitutes:</b></span>")
                self.quad02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.label05.set_markup("<span foreground=\""+self.c16_hex+"\" ><b>Coach:</b></span>")
                self.quad03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.quad04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c17_hex))
                self.label06.set_markup("<span foreground=\""+self.c17_hex+"\" >Match</span>")
                self.box_label06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c16_hex))
                self.quad05.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.quad06.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c07_hex))
                self.label07.set_markup("<span foreground=\""+self.c07_hex+"\" >Info</span>")
                self.box_label07.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c06_hex))
                self.righa_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))
                self.sottomenu.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.righa_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c11_hex))
                self.sbm_01.set_markup("<u><span foreground=\""+self.c12_hex+"\" ><b>Lineups</b></span></u>")
                self.box_sbm_01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.colonna01.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.sbm_02.set_markup("<u><span foreground=\""+self.c12_hex+"\" ><b>Report</b></span></u>")
                self.box_sbm_02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.colonna02.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.sbm_03.set_markup("<u><span foreground=\""+self.c12_hex+"\" ><b>Events</b></span></u>")
                self.box_sbm_03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))
                self.colonna03.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c12_hex))
                self.sbm_04.set_markup("<u><span foreground=\""+self.c12_hex+"\" ><b>Statistics</b></span></u>")
                self.box_sbm_04.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(self.c13_hex))     

            else:
                pass   


 
    def main(self): 
        gtk.main()


register(
    "python-fu-palette",
    N_("It generates palette by following the harmonic schemes of color: monochromatic, complementary, triadic, tetrad and analogous"),
    "It generates palette by following the harmonic schemes of color: monochromatic, complementary, triadic, tetrad and analogous",
    "Marco Crippa",
    "Marco Crippa",
    "2009",
    N_("_Palette Generator"),
    "",
    [],
    [],
    palette,
    menu=percorso
    )

main()

