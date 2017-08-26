#!/usr/bin/env python
#******************************#
#Marco Crippa                  #
#http://krypt77.altervista.org/#
#******************************#

# *************************************************************************** #
#                                                                             #
#      Version 1.0 - 2009-09-01                                               #
#      Copyright (C) 2009 Marco Crippa                                        #
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

import os,pygtk
pygtk.require('2.0')
import gtk, gobject
from gimpfu import *

version=gimp.version

percorso="<Toolbox>/File/Save"

def iconizer(img):
    Iconizer(img)
    gtk.main()


class Iconizer: 

    def __init__(self,img): 
        #layout
        self.contenitore_gen = gtk.VBox()
        box_gen = gtk.VBox()
        box_tab = gtk.VBox()

        self.contenitore_gen.pack_start(box_gen, False, False, 0)

        #window
        self.win = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.win.connect("destroy", self.destroy)
        self.win.set_title("Win-Iconizer 1.0")
        self.win.set_position(gtk.WIN_POS_CENTER)
        self.win.set_default_size(400, 400)
        self.win.set_resizable(False)
        self.win.set_border_width(2)
        self.icon = self.win.render_icon(gtk.STOCK_NO, gtk.ICON_SIZE_BUTTON)
        self.win.set_icon(self.icon)        
        self.label = gtk.Label("Select the formats you would like to create:\n")
        self.win.add(self.contenitore_gen)        

        box_gen.pack_start(box_tab, True, True, 1)
        
        box_tab.pack_start(self.label, True, True, 1)

        tooltips = gtk.Tooltips()
        table = gtk.Table(8, 5, False)
        table.set_col_spacings(5)

        table.attach(gtk.Label(""), 0, 1, 0, 1)
        table.attach(gtk.Label("16"), 1, 2, 0, 1)
        table.attach(gtk.Label("24"), 2, 3, 0, 1)
        table.attach(gtk.Label("32"), 3, 4, 0, 1)
        table.attach(gtk.Label("48"), 4, 5, 0, 1)
        table.attach(gtk.Label("64"), 5, 6, 0, 1)
        table.attach(gtk.Label("128"), 6, 7, 0, 1)
        table.attach(gtk.Label("256"), 7, 8, 0, 1)
        table.attach(gtk.Label("512"), 8, 9, 0, 1)

        #1-bit 2 colors
        self.c_1b_16p = gtk.CheckButton("")
        self.c_1b_16p.set_size_request(20,20)

        self.c_1b_24p = gtk.CheckButton("")
        self.c_1b_24p.set_size_request(20,20)

        self.c_1b_32p = gtk.CheckButton("")
        self.c_1b_32p.set_size_request(20,20)

        self.c_1b_48p = gtk.CheckButton("")
        self.c_1b_48p.set_size_request(20,20)

        self.c_1b_64p = gtk.CheckButton("")
        self.c_1b_64p.set_size_request(20,20)

        self.c_1b_128p = gtk.CheckButton("")
        self.c_1b_128p.set_size_request(20,20)

        self.c_1b_256p = gtk.CheckButton("")
        self.c_1b_256p.set_size_request(20,20)

        self.c_1b_512p = gtk.CheckButton("")
        self.c_1b_512p.set_size_request(20,20)

        l_mono=gtk.Label("Mono")
        table.attach(l_mono, 0, 1, 1, 2)
        tooltips.set_tip(l_mono, "Mono - 1-bit")
        table.attach(self.c_1b_16p, 1, 2, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_24p, 2, 3, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_32p, 3, 4, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_48p, 4, 5, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_64p, 5, 6, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_128p, 6, 7, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_256p, 7, 8, 1, 2,xoptions=gtk.EXPAND)
        table.attach(self.c_1b_512p, 8, 9, 1, 2,xoptions=gtk.EXPAND)

        #4-bit 16 colors
        self.c_16c_16p = gtk.CheckButton("")
        self.c_16c_16p.set_size_request(20,20)

        self.c_16c_24p = gtk.CheckButton("")
        self.c_16c_24p.set_size_request(20,20)

        self.c_16c_32p = gtk.CheckButton("")
        self.c_16c_32p.set_size_request(20,20)

        self.c_16c_48p = gtk.CheckButton("")
        self.c_16c_48p.set_size_request(20,20)

        self.c_16c_64p = gtk.CheckButton("")
        self.c_16c_64p.set_size_request(20,20)

        self.c_16c_128p = gtk.CheckButton("")
        self.c_16c_128p.set_size_request(20,20)

        self.c_16c_256p = gtk.CheckButton("")
        self.c_16c_256p.set_size_request(20,20)

        self.c_16c_512p = gtk.CheckButton("")
        self.c_16c_512p.set_size_request(20,20)

        l_16c=gtk.Label("16 Colors")
        table.attach(l_16c, 0, 1, 2, 3)
        tooltips.set_tip(l_16c, "16 Colors - 4-bits")
        table.attach(self.c_16c_16p, 1, 2, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_24p, 2, 3, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_32p, 3, 4, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_48p, 4, 5, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_64p, 5, 6, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_128p, 6, 7, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_256p, 7, 8, 2, 3,xoptions=gtk.EXPAND)
        table.attach(self.c_16c_512p, 8, 9, 2, 3,xoptions=gtk.EXPAND)

        #8-bit 256 colors
        self.c_256c_16p = gtk.CheckButton("")
        self.c_256c_16p.set_size_request(20,20)

        self.c_256c_24p = gtk.CheckButton("")
        self.c_256c_24p.set_size_request(20,20)

        self.c_256c_32p = gtk.CheckButton("")
        self.c_256c_32p.set_size_request(20,20)

        self.c_256c_48p = gtk.CheckButton("")
        self.c_256c_48p.set_size_request(20,20)

        self.c_256c_64p = gtk.CheckButton("")
        self.c_256c_64p.set_size_request(20,20)

        self.c_256c_128p = gtk.CheckButton("")
        self.c_256c_128p.set_size_request(20,20)

        self.c_256c_256p = gtk.CheckButton("")
        self.c_256c_256p.set_size_request(20,20)

        self.c_256c_512p = gtk.CheckButton("")
        self.c_256c_512p.set_size_request(20,20)

        l_256c=gtk.Label("256 Colors")
        table.attach(l_256c, 0, 1, 3, 4)
        tooltips.set_tip(l_256c, "256 Colors - 8-bits")
        table.attach(self.c_256c_16p, 1, 2, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_24p, 2, 3, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_32p, 3, 4, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_48p, 4, 5, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_64p, 5, 6, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_128p, 6, 7, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_256p, 7, 8, 3, 4,xoptions=gtk.EXPAND)
        table.attach(self.c_256c_512p, 8, 9, 3, 4,xoptions=gtk.EXPAND)

        #32-bit 4.2 billion colors RGBA
        self.c_32b_16p = gtk.CheckButton("")
        self.c_32b_16p.set_size_request(20,20)

        self.c_32b_24p = gtk.CheckButton("")
        self.c_32b_24p.set_size_request(20,20)

        self.c_32b_32p = gtk.CheckButton("")
        self.c_32b_32p.set_size_request(20,20)

        self.c_32b_48p = gtk.CheckButton("")
        self.c_32b_48p.set_size_request(20,20)

        self.c_32b_64p = gtk.CheckButton("")
        self.c_32b_64p.set_size_request(20,20)

        self.c_32b_128p = gtk.CheckButton("")
        self.c_32b_128p.set_size_request(20,20)

        self.c_32b_256p = gtk.CheckButton("")
        self.c_32b_256p.set_size_request(20,20)

        self.c_32b_512p = gtk.CheckButton("")
        self.c_32b_512p.set_size_request(20,20)

        l_rgba=gtk.Label("RGB/A")
        table.attach(l_rgba, 0, 1, 4, 5)
        tooltips.set_tip(l_rgba, "RGB/A - 32-bits")
        table.attach(self.c_32b_16p, 1, 2, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_24p, 2, 3, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_32p, 3, 4, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_48p, 4, 5, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_64p, 5, 6, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_128p, 6, 7, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_256p, 7, 8, 4, 5,xoptions=gtk.EXPAND)
        table.attach(self.c_32b_512p, 8, 9, 4, 5,xoptions=gtk.EXPAND)

        box_tab.pack_start(table, True, True, 1)

        #icon type
        box_type = gtk.VBox(True, 5)

        default = gtk.RadioButton(None, "Default")
        default.connect("toggled", self.Set_Icon_Type, "default")
        default.set_active(False)
        tooltips.set_tip(default, "48x48 (256 colors, 16 colors)\n32x32 (256 colors, 16 colors)\n16x16 (256 colors, 16 colors)")
        box_type.pack_start(default, False, False, 0)

        win_vista = gtk.RadioButton(default, "Win Vista")
        win_vista.connect("toggled", self.Set_Icon_Type, "win_vista")
        tooltips.set_tip(win_vista, "Recommended:\n256x256 (RGB/A)\n64x64 (RGB/A)\n48x48 (RGB/A, 256 colors, 16 colors)\n32x32 (RGB/A, 256 colors, 16 colors)\n24x24 (RGB/A, 256 colors, 16 colors)\n16x16 (RGB/A, 256 colors, 16 colors)\nMinimum:\n256x256 (RGB/A)\n48x48 (RGB/A, 256 colors)\n32x32 (RGB/A, 256 colors)\n16x16 (RGB/A, 256 colors)\nOptional:\n256x256 (256 colors, 16 colors)\n64x64 (256 colors, 16 colors)")
        box_type.pack_start(win_vista, False, False, 0)

        win_xp = gtk.RadioButton(default, "Win XP")
        win_xp.connect("toggled", self.Set_Icon_Type, "win_xp")
        tooltips.set_tip(win_xp, "Recommended:\n48x48 (RGB/A, 256 colors, 16 colors)\n32x32 (RGB/A, 256 colors, 16 colors)\n24x24 (RGB/A, 256 colors, 16 colors)\n16x16 (RGB/A, 256 colors, 16 colors)\nMinimum:\n32x32 (RGB/A, 256 colors, 16 colors),\n16x16 (RGB/A, 256 colors, 16 colors)\nOptional:\n128x128 (RGB/A)")
        box_type.pack_start(win_xp, False, False, 0)

        win_95 = gtk.RadioButton(default, "Win 95 - Win 98 - Win ME - Win 2000")
        win_95.connect("toggled", self.Set_Icon_Type, "win_95")
        tooltips.set_tip(win_95, "Recommended:\n48x48 (256 colors, 16 colors)\n32x32 (256 colors, 16 colors)\n16x16 (256 colors, 16 colors)\nMinimum:\n32x32 (256 colors, 16 colors)\n16x16 (256 colors, 16 colors)")
        box_type.pack_start(win_95, False, False, 0)

        favico = gtk.RadioButton(default, "Favico")
        favico.connect("toggled", self.Set_Icon_Type, "favico")
        tooltips.set_tip(favico, "Recommended:\n32x32 (256 colors)\n16x16 (256 colors)")
        box_type.pack_start(favico, False, False, 0)

        custom = gtk.RadioButton(default, "Custom")
        custom.connect("toggled", self.Set_Icon_Type, "custom")
        tooltips.set_tip(custom, "Create your own icon")
        box_type.pack_start(custom, False, False, 0)

        box_gen.pack_start(gtk.Label(""), True, True, 1)
        box_gen.pack_start(box_type, True, True, 1)
        box_gen.pack_start(gtk.Label(""), True, True, 1)

        b_exe = gtk.Button()
        image = gtk.Image()
        image.set_from_stock(gtk.STOCK_EXECUTE,gtk.ICON_SIZE_BUTTON)
        b_exe.set_image(image)
        b_exe.set_label("Create")
        b_exe.set_size_request(80,35)
        b_exe.connect("clicked", self.Create_Icon,img)

        #progress bar
        self.pbar = gtk.ProgressBar()
        self.pbar.set_text("0 %")
        box_gen.pack_start(self.pbar, True, True, 5)

        box_gen.pack_start(b_exe, False, False, 1)

        #set default
        self.Set_Icon_Type(self,"default")

        self.win.show_all()

    def destroy(self, widget, data=None):
        self.win.destroy()
        return gtk.main_quit()

    def Set_Icon(self):
        #[pixel,colors]
        array_icon=[]
        #[dim]
        array_dim=[]

        #1-bit 2 colors
        if self.c_1b_16p.get_active():
        	array_icon.append([16,2])
        	array_dim.append(16)
        if self.c_1b_24p.get_active():
        	array_icon.append([24,2])
        	array_dim.append(24)
        if self.c_1b_32p.get_active():
        	array_icon.append([32,2])
        	array_dim.append(32)
        if self.c_1b_48p.get_active():
        	array_icon.append([48,2])
        	array_dim.append(48)
        if self.c_1b_64p.get_active():
        	array_icon.append([64,2])
        	array_dim.append(64)
        if self.c_1b_128p.get_active():
        	array_icon.append([128,2])
        	array_dim.append(128)
        if self.c_1b_256p.get_active():
        	array_icon.append([256,2])
        	array_dim.append(256)
        if self.c_1b_512p.get_active():
        	array_icon.append([512,2])
        	array_dim.append(512)
        #4-bit 16 colors
        if self.c_16c_16p.get_active():
        	array_icon.append([16,16])
        	array_dim.append(16)
        if self.c_16c_24p.get_active():
        	array_icon.append([24,16])
        	array_dim.append(24)
        if self.c_16c_32p.get_active():
        	array_icon.append([32,16])
        	array_dim.append(32)
        if self.c_16c_48p.get_active():
        	array_icon.append([48,16])
        	array_dim.append(48)
        if self.c_16c_64p.get_active():
        	array_icon.append([64,16])
        	array_dim.append(64)
        if self.c_16c_128p.get_active():
        	array_icon.append([128,16])
        	array_dim.append(128)
        if self.c_16c_256p.get_active():
        	array_icon.append([256,16])
        	array_dim.append(256)
        if self.c_16c_512p.get_active():
        	array_icon.append([512,16])
        	array_dim.append(512)
        #8-bit 256 colors
        if self.c_256c_16p.get_active():
        	array_icon.append([16,256])
        	array_dim.append(16)
        if self.c_256c_24p.get_active():
        	array_icon.append([24,256])
        	array_dim.append(24)
        if self.c_256c_32p.get_active():
        	array_icon.append([32,256])
        	array_dim.append(32)
        if self.c_256c_48p.get_active():
        	array_icon.append([48,256])
        	array_dim.append(48)
        if self.c_256c_64p.get_active():
        	array_icon.append([64,256])
        	array_dim.append(64)
        if self.c_256c_128p.get_active():
        	array_icon.append([128,256])
        	array_dim.append(128)
        if self.c_256c_256p.get_active():
        	array_icon.append([256,256])
        	array_dim.append(256)
        if self.c_256c_512p.get_active():
        	array_icon.append([512,256])
        	array_dim.append(512)
        #32-bit 4.2 billion colors RGBA
        if self.c_32b_16p.get_active():
        	array_icon.append([16,0])
        	array_dim.append(16)
        if self.c_32b_24p.get_active():
        	array_icon.append([24,0])
        	array_dim.append(24)
        if self.c_32b_32p.get_active():
        	array_icon.append([32,0])
        	array_dim.append(32)
        if self.c_32b_48p.get_active():
        	array_icon.append([48,0])
        	array_dim.append(48)
        if self.c_32b_64p.get_active():
        	array_icon.append([64,0])
        	array_dim.append(64)
        if self.c_32b_128p.get_active():
        	array_icon.append([128,0])
        	array_dim.append(128)
        if self.c_32b_256p.get_active():
        	array_icon.append([256,0])
        	array_dim.append(256)
        if self.c_32b_512p.get_active():
        	array_icon.append([512,0])
        	array_dim.append(512)

            
        return array_icon,array_dim
        

    def Create_Icon(self,widget,img):
       
        a_icons,set_dim=self.Set_Icon()
        set_dim.sort()
        set_dim.reverse()
        
        #create new image 
        icon=pdb.gimp_image_new(set_dim[0],set_dim[0],0)
        icon.disable_undo()

#***set progress*************************************
        count = 1
        percent=float(count)/(len(a_icons))
        fraz=0.0
        self.pbar.set_fraction(fraz)
#***progress*************************************

        for i in xrange(0,len(a_icons)):
            
#***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
            fraz=fraz+percent
#***progress*************************************

            #create temp image 
            t_img=pdb.gimp_image_new(a_icons[i][0],a_icons[i][0],0)
            t_img.disable_undo()

            #create a new layer from visible
            temp_ico=pdb.gimp_layer_new_from_visible(img,t_img,"")

            #add the new layer to the new image
            pdb.gimp_image_add_layer(t_img,temp_ico,0)
            #scale layer 
            pdb.gimp_layer_scale(temp_ico,a_icons[i][0],a_icons[i][0],False)
            #move the layer to 0 0
            pdb.gimp_layer_set_offsets(temp_ico,0,0)
            if a_icons[i][1]!=0:
                #change color depths
                pdb.gimp_convert_indexed(t_img, 0, 0, a_icons[i][1], True, False, "")
                #create new layer from visible to paste in icon image
                l_ico=pdb.gimp_layer_new_from_visible(t_img,icon,str(a_icons[i][0])+"p - "+str(a_icons[i][1])+"c")
            else:
                #create new layer from visible to paste in icon image
                l_ico=pdb.gimp_layer_new_from_visible(t_img,icon,str(a_icons[i][0])+"p - RGBA")
            t_img.enable_undo()
            #destroy temp image
            pdb.gimp_image_delete(t_img)

            #add the new layer to the new image
            pdb.gimp_image_add_layer(icon,l_ico,0)
            #move the layer to 0 0
            pdb.gimp_layer_set_offsets(l_ico,0,0)

        name_open_img=pdb.gimp_image_get_filename(img)
        pdb.gimp_image_set_filename(icon,os.path.splitext(name_open_img)[0]+".ico")
        icon.enable_undo()

#***progress*************************************
        if fraz==fraz:
            self.pbar.set_text("100 %")
            self.pbar.set_fraction(1.0)
#***progress*************************************
        pdb.gimp_display_new(icon)


    def Set_Icon_Type(self,widget,data):
        self.pbar.set_text("0 %")
        self.pbar.set_fraction(0.0)
        if data=="default":
            #1-bit 2 colors
            self.c_1b_16p.set_active(False)
            self.c_1b_24p.set_active(False)
            self.c_1b_32p.set_active(False)
            self.c_1b_48p.set_active(False)
            self.c_1b_64p.set_active(False)
            self.c_1b_128p.set_active(False)
            self.c_1b_256p.set_active(False)
            self.c_1b_512p.set_active(False)
            self.c_1b_16p.set_sensitive(False)
            self.c_1b_24p.set_sensitive(False)
            self.c_1b_32p.set_sensitive(False)
            self.c_1b_48p.set_sensitive(False)
            self.c_1b_64p.set_sensitive(False)
            self.c_1b_128p.set_sensitive(False)
            self.c_1b_256p.set_sensitive(False)
            self.c_1b_512p.set_sensitive(False)
            #4-bit 16 colors
            self.c_16c_16p.set_active(True)
            self.c_16c_24p.set_active(False)
            self.c_16c_32p.set_active(True)
            self.c_16c_48p.set_active(True)
            self.c_16c_64p.set_active(False)
            self.c_16c_128p.set_active(False)
            self.c_16c_256p.set_active(False)
            self.c_16c_512p.set_active(False)
            self.c_16c_16p.set_sensitive(False)
            self.c_16c_24p.set_sensitive(False)
            self.c_16c_32p.set_sensitive(False)
            self.c_16c_48p.set_sensitive(False)
            self.c_16c_64p.set_sensitive(False)
            self.c_16c_128p.set_sensitive(False)
            self.c_16c_256p.set_sensitive(False)
            self.c_16c_512p.set_sensitive(False)
            #8-bit 256 colors
            self.c_256c_16p.set_active(True)
            self.c_256c_24p.set_active(False)
            self.c_256c_32p.set_active(True)
            self.c_256c_48p.set_active(True)
            self.c_256c_64p.set_active(False)
            self.c_256c_128p.set_active(False)
            self.c_256c_256p.set_active(False)
            self.c_256c_512p.set_active(False)
            self.c_256c_16p.set_sensitive(False)
            self.c_256c_24p.set_sensitive(False)
            self.c_256c_32p.set_sensitive(False)
            self.c_256c_48p.set_sensitive(False)
            self.c_256c_64p.set_sensitive(False)
            self.c_256c_128p.set_sensitive(False)
            self.c_256c_256p.set_sensitive(False)
            self.c_256c_512p.set_sensitive(False)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_active(False)
            self.c_32b_24p.set_active(False)
            self.c_32b_32p.set_active(False)
            self.c_32b_48p.set_active(False)
            self.c_32b_64p.set_active(False)
            self.c_32b_128p.set_active(False)
            self.c_32b_256p.set_active(False)
            self.c_32b_512p.set_active(False)
            self.c_32b_16p.set_sensitive(False)
            self.c_32b_24p.set_sensitive(False)
            self.c_32b_32p.set_sensitive(False)
            self.c_32b_48p.set_sensitive(False)
            self.c_32b_64p.set_sensitive(False)
            self.c_32b_128p.set_sensitive(False)
            self.c_32b_256p.set_sensitive(False)
            self.c_32b_512p.set_sensitive(False)
        if data=="win_95":
            #1-bit 2 colors
            self.c_1b_16p.set_active(False)
            self.c_1b_24p.set_active(False)
            self.c_1b_32p.set_active(False)
            self.c_1b_48p.set_active(False)
            self.c_1b_64p.set_active(False)
            self.c_1b_128p.set_active(False)
            self.c_1b_256p.set_active(False)
            self.c_1b_512p.set_active(False)
            self.c_1b_16p.set_sensitive(False)
            self.c_1b_24p.set_sensitive(False)
            self.c_1b_32p.set_sensitive(False)
            self.c_1b_48p.set_sensitive(False)
            self.c_1b_64p.set_sensitive(False)
            self.c_1b_128p.set_sensitive(False)
            self.c_1b_256p.set_sensitive(False)
            self.c_1b_512p.set_sensitive(False)
            #4-bit 16 colors
            self.c_16c_16p.set_active(True)
            self.c_16c_24p.set_active(False)
            self.c_16c_32p.set_active(True)
            self.c_16c_48p.set_active(True)
            self.c_16c_64p.set_active(False)
            self.c_16c_128p.set_active(False)
            self.c_16c_256p.set_active(False)
            self.c_16c_512p.set_active(False)
            self.c_16c_16p.set_sensitive(False)
            self.c_16c_24p.set_sensitive(False)
            self.c_16c_32p.set_sensitive(False)
            self.c_16c_48p.set_sensitive(False)
            self.c_16c_64p.set_sensitive(False)
            self.c_16c_128p.set_sensitive(False)
            self.c_16c_256p.set_sensitive(False)
            self.c_16c_512p.set_sensitive(False)
            #8-bit 256 colors
            self.c_256c_16p.set_active(True)
            self.c_256c_24p.set_active(False)
            self.c_256c_32p.set_active(True)
            self.c_256c_48p.set_active(True)
            self.c_256c_64p.set_active(False)
            self.c_256c_128p.set_active(False)
            self.c_256c_256p.set_active(False)
            self.c_256c_512p.set_active(False)
            self.c_256c_16p.set_sensitive(False)
            self.c_256c_24p.set_sensitive(False)
            self.c_256c_32p.set_sensitive(False)
            self.c_256c_48p.set_sensitive(False)
            self.c_256c_64p.set_sensitive(False)
            self.c_256c_128p.set_sensitive(False)
            self.c_256c_256p.set_sensitive(False)
            self.c_256c_512p.set_sensitive(False)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_active(False)
            self.c_32b_24p.set_active(False)
            self.c_32b_32p.set_active(False)
            self.c_32b_48p.set_active(False)
            self.c_32b_64p.set_active(False)
            self.c_32b_128p.set_active(False)
            self.c_32b_256p.set_active(False)
            self.c_32b_512p.set_active(False)
            self.c_32b_16p.set_sensitive(False)
            self.c_32b_24p.set_sensitive(False)
            self.c_32b_32p.set_sensitive(False)
            self.c_32b_48p.set_sensitive(False)
            self.c_32b_64p.set_sensitive(False)
            self.c_32b_128p.set_sensitive(False)
            self.c_32b_256p.set_sensitive(False)
            self.c_32b_512p.set_sensitive(False)
        if data=="win_xp":
            #1-bit 2 colors
            self.c_1b_16p.set_active(False)
            self.c_1b_24p.set_active(False)
            self.c_1b_32p.set_active(False)
            self.c_1b_48p.set_active(False)
            self.c_1b_64p.set_active(False)
            self.c_1b_128p.set_active(False)
            self.c_1b_256p.set_active(False)
            self.c_1b_512p.set_active(False)
            self.c_1b_16p.set_sensitive(False)
            self.c_1b_24p.set_sensitive(False)
            self.c_1b_32p.set_sensitive(False)
            self.c_1b_48p.set_sensitive(False)
            self.c_1b_64p.set_sensitive(False)
            self.c_1b_128p.set_sensitive(False)
            self.c_1b_256p.set_sensitive(False)
            self.c_1b_512p.set_sensitive(False)
            #4-bit 16 colors
            self.c_16c_16p.set_active(True)
            self.c_16c_24p.set_active(True)
            self.c_16c_32p.set_active(True)
            self.c_16c_48p.set_active(True)
            self.c_16c_64p.set_active(False)
            self.c_16c_128p.set_active(False)
            self.c_16c_256p.set_active(False)
            self.c_16c_512p.set_active(False)
            self.c_16c_16p.set_sensitive(False)
            self.c_16c_24p.set_sensitive(False)
            self.c_16c_32p.set_sensitive(False)
            self.c_16c_48p.set_sensitive(False)
            self.c_16c_64p.set_sensitive(False)
            self.c_16c_128p.set_sensitive(False)
            self.c_16c_256p.set_sensitive(False)
            self.c_16c_512p.set_sensitive(False)
            #8-bit 256 colors
            self.c_256c_16p.set_active(True)
            self.c_256c_24p.set_active(True)
            self.c_256c_32p.set_active(True)
            self.c_256c_48p.set_active(True)
            self.c_256c_64p.set_active(False)
            self.c_256c_128p.set_active(False)
            self.c_256c_256p.set_active(False)
            self.c_256c_512p.set_active(False)
            self.c_256c_16p.set_sensitive(False)
            self.c_256c_24p.set_sensitive(False)
            self.c_256c_32p.set_sensitive(False)
            self.c_256c_48p.set_sensitive(False)
            self.c_256c_64p.set_sensitive(False)
            self.c_256c_128p.set_sensitive(False)
            self.c_256c_256p.set_sensitive(False)
            self.c_256c_512p.set_sensitive(False)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_active(True)
            self.c_32b_24p.set_active(True)
            self.c_32b_32p.set_active(True)
            self.c_32b_48p.set_active(True)
            self.c_32b_64p.set_active(False)
            self.c_32b_128p.set_active(False)
            self.c_32b_256p.set_active(False)
            self.c_32b_512p.set_active(False)
            self.c_32b_16p.set_sensitive(False)
            self.c_32b_24p.set_sensitive(False)
            self.c_32b_32p.set_sensitive(False)
            self.c_32b_48p.set_sensitive(False)
            self.c_32b_64p.set_sensitive(False)
            self.c_32b_128p.set_sensitive(False)
            self.c_32b_256p.set_sensitive(False)
            self.c_32b_512p.set_sensitive(False)
        if data=="win_vista":
            #1-bit 2 colors
            self.c_1b_16p.set_active(False)
            self.c_1b_24p.set_active(False)
            self.c_1b_32p.set_active(False)
            self.c_1b_48p.set_active(False)
            self.c_1b_64p.set_active(False)
            self.c_1b_128p.set_active(False)
            self.c_1b_256p.set_active(False)
            self.c_1b_512p.set_active(False)
            self.c_1b_16p.set_sensitive(False)
            self.c_1b_24p.set_sensitive(False)
            self.c_1b_32p.set_sensitive(False)
            self.c_1b_48p.set_sensitive(False)
            self.c_1b_64p.set_sensitive(False)
            self.c_1b_128p.set_sensitive(False)
            self.c_1b_256p.set_sensitive(False)
            self.c_1b_512p.set_sensitive(False)
            #4-bit 16 colors
            self.c_16c_16p.set_active(True)
            self.c_16c_24p.set_active(True)
            self.c_16c_32p.set_active(True)
            self.c_16c_48p.set_active(True)
            self.c_16c_64p.set_active(False)
            self.c_16c_128p.set_active(False)
            self.c_16c_256p.set_active(False)
            self.c_16c_512p.set_active(False)
            self.c_16c_16p.set_sensitive(False)
            self.c_16c_24p.set_sensitive(False)
            self.c_16c_32p.set_sensitive(False)
            self.c_16c_48p.set_sensitive(False)
            self.c_16c_64p.set_sensitive(False)
            self.c_16c_128p.set_sensitive(False)
            self.c_16c_256p.set_sensitive(False)
            self.c_16c_512p.set_sensitive(False)
            #8-bit 256 colors
            self.c_256c_16p.set_active(True)
            self.c_256c_24p.set_active(True)
            self.c_256c_32p.set_active(True)
            self.c_256c_48p.set_active(True)
            self.c_256c_64p.set_active(False)
            self.c_256c_128p.set_active(False)
            self.c_256c_256p.set_active(False)
            self.c_256c_512p.set_active(False)
            self.c_256c_16p.set_sensitive(False)
            self.c_256c_24p.set_sensitive(False)
            self.c_256c_32p.set_sensitive(False)
            self.c_256c_48p.set_sensitive(False)
            self.c_256c_64p.set_sensitive(False)
            self.c_256c_128p.set_sensitive(False)
            self.c_256c_256p.set_sensitive(False)
            self.c_256c_512p.set_sensitive(False)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_active(True)
            self.c_32b_24p.set_active(True)
            self.c_32b_32p.set_active(True)
            self.c_32b_48p.set_active(True)
            self.c_32b_64p.set_active(True)
            self.c_32b_128p.set_active(False)
            self.c_32b_256p.set_active(True)
            self.c_32b_512p.set_active(False)
            self.c_32b_16p.set_sensitive(False)
            self.c_32b_24p.set_sensitive(False)
            self.c_32b_32p.set_sensitive(False)
            self.c_32b_48p.set_sensitive(False)
            self.c_32b_64p.set_sensitive(False)
            self.c_32b_128p.set_sensitive(False)
            self.c_32b_256p.set_sensitive(False)
            self.c_32b_512p.set_sensitive(False)
        if data=="favico":
            #1-bit 2 colors
            self.c_1b_16p.set_active(False)
            self.c_1b_24p.set_active(False)
            self.c_1b_32p.set_active(False)
            self.c_1b_48p.set_active(False)
            self.c_1b_64p.set_active(False)
            self.c_1b_128p.set_active(False)
            self.c_1b_256p.set_active(False)
            self.c_1b_512p.set_active(False)
            self.c_1b_16p.set_sensitive(False)
            self.c_1b_24p.set_sensitive(False)
            self.c_1b_32p.set_sensitive(False)
            self.c_1b_48p.set_sensitive(False)
            self.c_1b_64p.set_sensitive(False)
            self.c_1b_128p.set_sensitive(False)
            self.c_1b_256p.set_sensitive(False)
            self.c_1b_512p.set_sensitive(False)
            #4-bit 16 colors
            self.c_16c_16p.set_active(False)
            self.c_16c_24p.set_active(False)
            self.c_16c_32p.set_active(False)
            self.c_16c_48p.set_active(False)
            self.c_16c_64p.set_active(False)
            self.c_16c_128p.set_active(False)
            self.c_16c_256p.set_active(False)
            self.c_16c_512p.set_active(False)
            self.c_16c_16p.set_sensitive(False)
            self.c_16c_24p.set_sensitive(False)
            self.c_16c_32p.set_sensitive(False)
            self.c_16c_48p.set_sensitive(False)
            self.c_16c_64p.set_sensitive(False)
            self.c_16c_128p.set_sensitive(False)
            self.c_16c_256p.set_sensitive(False)
            self.c_16c_512p.set_sensitive(False)
            #8-bit 256 colors
            self.c_256c_16p.set_active(True)
            self.c_256c_24p.set_active(False)
            self.c_256c_32p.set_active(True)
            self.c_256c_48p.set_active(False)
            self.c_256c_64p.set_active(False)
            self.c_256c_128p.set_active(False)
            self.c_256c_256p.set_active(False)
            self.c_256c_512p.set_active(False)
            self.c_256c_16p.set_sensitive(False)
            self.c_256c_24p.set_sensitive(False)
            self.c_256c_32p.set_sensitive(False)
            self.c_256c_48p.set_sensitive(False)
            self.c_256c_64p.set_sensitive(False)
            self.c_256c_128p.set_sensitive(False)
            self.c_256c_256p.set_sensitive(False)
            self.c_256c_512p.set_sensitive(False)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_active(False)
            self.c_32b_24p.set_active(False)
            self.c_32b_32p.set_active(False)
            self.c_32b_48p.set_active(False)
            self.c_32b_64p.set_active(False)
            self.c_32b_128p.set_active(False)
            self.c_32b_256p.set_active(False)
            self.c_32b_512p.set_active(False)
            self.c_32b_16p.set_sensitive(False)
            self.c_32b_24p.set_sensitive(False)
            self.c_32b_32p.set_sensitive(False)
            self.c_32b_48p.set_sensitive(False)
            self.c_32b_64p.set_sensitive(False)
            self.c_32b_128p.set_sensitive(False)
            self.c_32b_256p.set_sensitive(False)
            self.c_32b_512p.set_sensitive(False)
        if data=="custom":
            #1-bit 2 colors
            self.c_1b_16p.set_sensitive(True)
            self.c_1b_24p.set_sensitive(True)
            self.c_1b_32p.set_sensitive(True)
            self.c_1b_48p.set_sensitive(True)
            self.c_1b_64p.set_sensitive(True)
            self.c_1b_128p.set_sensitive(True)
            self.c_1b_256p.set_sensitive(True)
            self.c_1b_512p.set_sensitive(True)
            #4-bit 16 colors
            self.c_16c_16p.set_sensitive(True)
            self.c_16c_24p.set_sensitive(True)
            self.c_16c_32p.set_sensitive(True)
            self.c_16c_48p.set_sensitive(True)
            self.c_16c_64p.set_sensitive(True)
            self.c_16c_128p.set_sensitive(True)
            self.c_16c_256p.set_sensitive(True)
            self.c_16c_512p.set_sensitive(True)
            #8-bit 256 colors
            self.c_256c_16p.set_sensitive(True)
            self.c_256c_24p.set_sensitive(True)
            self.c_256c_32p.set_sensitive(True)
            self.c_256c_48p.set_sensitive(True)
            self.c_256c_64p.set_sensitive(True)
            self.c_256c_128p.set_sensitive(True)
            self.c_256c_256p.set_sensitive(True)
            self.c_256c_512p.set_sensitive(True)
            #32-bit 4.2 billion colors RGBA
            self.c_32b_16p.set_sensitive(True)
            self.c_32b_24p.set_sensitive(True)
            self.c_32b_32p.set_sensitive(True)
            self.c_32b_48p.set_sensitive(True)
            self.c_32b_64p.set_sensitive(True)
            self.c_32b_128p.set_sensitive(True)
            self.c_32b_256p.set_sensitive(True)            
            self.c_32b_512p.set_sensitive(True)            
        else:
            pass

    def main(self): 
        gtk.main()


register(
    "python-fu-iconizer",
    N_("Win-Iconizer plug-in converts all visible layers of a single image into a multi-layered image. The new image will contain all the sizes and all the standard bit depths that you chose."),
    "Win-Iconizer plug-in converts all visible layers of a single image into a multi-layered image. The new image will contain all the sizes and all the standard bit depths that you chose.",
    "Marco Crippa",
    "Marco Crippa",
    "2009",
    N_("Win-Iconizer"),
    "",
    [
    (PF_IMAGE, "a", "", None),
    ],
    [],
    iconizer,
    menu=percorso
    )

main()

