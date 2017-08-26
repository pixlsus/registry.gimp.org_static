#!/usr/bin/env python
# -*- coding: utf8 -*-

# *************************************************************************** #
#                                                                             #
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

from gimpfu import *
import os,math,random
import pygtk
pygtk.require('2.0')
import gtk, gobject

global laying
global texture_type
global dir_map

laying=0
texture_type=0
dir_map=""

def T_G():
    Tex_Generator()
    gtk.main()


class Tex_Generator: 

    def __init__(self): 
        #layout
        self.contenitore_gen = gtk.VBox()
        box_gen = gtk.VBox()
        box_dim_col = gtk.VBox()

        self.contenitore_gen.pack_start(box_gen, False, False, 0)
        box_gen.pack_start(box_dim_col, True, True, 1)

        #window
        self.win = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.win.connect("destroy", self.destroy)
        self.win.set_title("Texture Generator 1.1")
        self.win.set_position(gtk.WIN_POS_CENTER)
        self.win.set_default_size(400, 400)
        self.win.set_resizable(False)
        self.win.set_deletable(False)
        self.win.set_border_width(2)
        self.icon = self.win.render_icon(gtk.STOCK_NO, gtk.ICON_SIZE_BUTTON)
        self.win.set_icon(self.icon)        
        self.label = gtk.Label("Select the formats you would like to create:\n")
        self.win.add(self.contenitore_gen)      

        #tab dimension and color
        box_dimxy = gtk.HBox()
        #texture width
        box_dimx = gtk.HBox()
        label_dimx=gtk.Label("Texture Width:")
        label_dimx.set_size_request(120, 40)
        start_dim_x=gtk.Adjustment(1200,0,100000,1,10,0)
        self.texw=gtk.SpinButton(start_dim_x,1.0,0)
        box_dimx.pack_start(label_dimx, True, True, 1)
        box_dimx.pack_start(self.texw, True, True, 1)

        #texture height
        box_dimy = gtk.HBox()
        label_dimy=gtk.Label("Texture Height:")
        label_dimy.set_size_request(120, 40)
        start_dim_y=gtk.Adjustment(1200,0,100000,1,10,0)
        self.texh=gtk.SpinButton(start_dim_y,1.0,0)
        box_dimx.pack_start(label_dimy, True, True, 1)
        box_dimx.pack_start(self.texh, True, True, 1)

        box_dimxy.pack_start(box_dimx, True, True, 1)
        box_dimxy.pack_start(box_dimy, True, True, 1)
        box_dim_col.pack_start(box_dimxy, True, True, 1)

        box_strip_xy = gtk.HBox()
        #strip width
        box_stripx = gtk.HBox()
        label_stripx=gtk.Label("Strip Width:")
        label_stripx.set_size_request(120, 40)
        start_strip_x=gtk.Adjustment(300,0,100000,1,10,0)
        self.stripw=gtk.SpinButton(start_strip_x,1.0,0)
        self.stripw.connect("value-changed", self.Set_Basket_Width)
        box_stripx.pack_start(label_stripx, True, True, 1)
        box_stripx.pack_start(self.stripw, True, True, 1)

        #strip height
        box_stripy = gtk.HBox()
        label_stripy=gtk.Label("Strip Height:")
        label_stripy.set_size_request(120, 40)
        start_strip_y=gtk.Adjustment(150,0,100000,1,10,0)
        self.striph=gtk.SpinButton(start_strip_y,1.0,0)
        self.striph.connect("value-changed", self.Set_Basket_Height)
        box_stripx.pack_start(label_stripy, True, True, 1)
        box_stripx.pack_start(self.striph, True, True, 1)
        box_strip_xy.pack_start(box_stripx, True, True, 1)
        box_strip_xy.pack_start(box_stripy, True, True, 1)
        box_dim_col.pack_start(box_strip_xy, True, True, 1)

        #space
        box_gen.pack_start(gtk.Label(""), True, True, 1)

        #choise strip
        box_strip = gtk.HBox()
        self.gen_strip = gtk.RadioButton(None, "Generate Strip")
        self.gen_strip.connect("toggled", self.Generate_Load, "G")
        self.gen_strip.set_active(True)
        self.load_strip = gtk.RadioButton(self.gen_strip, "Load Strip")
        self.load_strip.connect("toggled", self.Generate_Load, "L")
        box_strip.pack_start(self.gen_strip, True, True, 0)
        box_strip.pack_start(self.load_strip, True, True, 0)
        box_gen.pack_start(box_strip, True, True, 0)

        #space
        box_gen.pack_start(gtk.Label(""), True, True, 1)

        box_gen_load = gtk.HBox()

        box_cstrip = gtk.VBox()
        #choose type strip
        box_tstrip = gtk.HBox()
        self.wood_strip = gtk.RadioButton(None, "Wood")
        self.wood_strip.set_active(True)
        self.brick_strip = gtk.RadioButton(self.wood_strip, "Brick")
        self.tile_strip = gtk.RadioButton(self.wood_strip, "Tile")
        self.wood_strip.connect("toggled", self.Set_Texture, 0)
        self.brick_strip.connect("toggled", self.Set_Texture, 1)
        self.tile_strip.connect("toggled", self.Set_Texture, 2)
        box_tstrip.pack_start(self.wood_strip, True, True, 0)
        box_tstrip.pack_start(self.brick_strip, True, True, 0)
        box_tstrip.pack_start(self.tile_strip, True, True, 0)


        box_cstrip.pack_start(box_tstrip, True, True, 0)
        box_gen_load.pack_start(box_cstrip, True, True, 0)
        box_gen.pack_start(box_gen_load, True, True, 0)

        #tile_effect
        self.tile_effect = gtk.CheckButton("Effect color tile")
        self.tile_effect.set_size_request(20,20)
        self.tile_effect.set_active(True)
        box_cstrip.pack_start(self.tile_effect, True, True, 0)

        #color
        label_color=gtk.Label("Choose color:")
        self.color=gtk.ColorButton(color=gtk.gdk.Color(0,0,0))
        self.color.set_size_request(100,35)
        box_cstrip.pack_start(self.color, True, False, 1)

        #space
        box_gen_load.pack_start(gtk.Label(""), True, True, 2)
        
        #choose laying
        box_laying = gtk.HBox()
        box_laying2 = gtk.HBox()
        pose_l=gtk.Label("Laying tipe: ")
        pose_l.set_size_request(120, 40)
        pose_v=gtk.Label("")
        pose_v.set_size_request(120, 40)
        self.brick_b = gtk.RadioButton(None, "Brick Bond")
        self.brick_b.connect("toggled", self.Set_Laying, 0)
        self.brick_b.set_active(True)
        self.set_tooltip(self.brick_b,"_ _ _ _ _\n _ _ _ _\n_ _ _ _ _\n _ _ _ _")
        self.brick_rx = gtk.RadioButton(self.brick_b, "Brick RandomX")
        self.brick_rx.connect("toggled", self.Set_Laying, 1)
        self.set_tooltip(self.brick_rx,"_ _ _ _ _\n _ _ _ _\n_ _ _ _ _\n _ _ _ _\nRandom X position")
        self.brick_rxy = gtk.RadioButton(self.brick_b, "Brick RandomXY")
        self.brick_rxy.connect("toggled", self.Set_Laying, 2)
        self.set_tooltip(self.brick_rxy,"_ _ _ _ _\n _ _ _ _\n_ _ _ _ _\n _ _ _ _\nRandom X position\nRandom Y height")
        self.basket = gtk.RadioButton(self.brick_b, "Basket 5/1")
        self.basket.connect("toggled", self.Set_Laying, 3)
        self.set_tooltip(self.basket,"_____|||||\n_____|||||\n_____|||||\n_____|||||\n_____|||||\n|||||_____\n|||||_____\n|||||_____\n|||||_____\n|||||_____")
        self.herringbone = gtk.RadioButton(self.brick_b, "Herringbone")
        self.herringbone.connect("toggled", self.Set_Laying, 4)
        her="/////\n"+"\\"+"\\"+"\\"+"\\"+"\\"
        self.set_tooltip(self.herringbone,her)
        self.tile = gtk.RadioButton(self.brick_b, "Tile")
        self.tile.connect("toggled", self.Set_Laying, 5)
        self.set_tooltip(self.tile,"_ _ _ _ _\n_ _ _ _ _\n_ _ _ _ _\n_ _ _ _ _")

        box_laying.pack_start(pose_l, True, True, 0)
        box_laying.pack_start(self.brick_b, True, True, 0)
        box_laying.pack_start(self.brick_rx, True, True, 0)
        box_laying.pack_start(self.brick_rxy, True, True, 0)
        box_laying2.pack_start(pose_v, False, False, 0)
        box_laying2.pack_start(self.basket, False, False, 0)
        box_laying2.pack_start(self.herringbone, False, False, 0)
        box_laying2.pack_start(self.tile, False, False, 5)

        box_gen.pack_start(box_laying, True, True, 0)
        box_gen.pack_start(box_laying2, True, True, 0)

        #space
        box_gen_load.pack_start(gtk.Label(""), True, True, 2)

        box_vload = gtk.VBox()
        #load dir strip
        self.load_dir = gtk.Button()
        image = gtk.Image()
        image.set_from_stock(gtk.STOCK_OPEN,gtk.ICON_SIZE_SMALL_TOOLBAR)
        self.load_dir.set_image(image)
        self.load_dir.set_label("Load strip folder")
        self.load_dir.set_size_request(100,35)
        self.load_dir.connect("clicked", self.Load_Strip)

        #randomize
        self.randomize = gtk.CheckButton("Randomize map")
        self.randomize.set_size_request(20,20)

        box_vload.pack_start(self.randomize, True, False, 1)
        box_vload.pack_start(self.load_dir, True, False, 1)

        box_gen_load.pack_start(box_vload, True, True, 0)

        #space
        box_gen_load.pack_start(gtk.Label(""), True, True, 0)

        #merge
        self.merge = gtk.CheckButton("Merge layers")
        self.merge.set_size_request(20,20)
        self.merge.set_active(True)
        box_gen.pack_start(self.merge, True, True, 3)

        #bump
        self.bump = gtk.CheckButton("Create bump map")
        self.bump.set_size_request(20,20)
        box_gen.pack_start(self.bump, True, True, 3)

        #progress bar
        self.pbar = gtk.ProgressBar()
        self.pbar.set_text("0 %")
        box_gen.pack_start(self.pbar, True, True, 5)
        
        #control button
        box_btn = gtk.HBox()
        self.exit = gtk.Button()
        img_exit = gtk.Image()
        img_exit.set_from_stock(gtk.STOCK_QUIT,gtk.ICON_SIZE_SMALL_TOOLBAR)
        self.exit.set_image(img_exit)
        self.exit.set_label("Quit")
        self.exit.set_size_request(100,35)
        self.exit.connect("clicked", self.destroy)

        self.exe = gtk.Button()
        img_exe = gtk.Image()
        img_exe.set_from_stock(gtk.STOCK_EXECUTE,gtk.ICON_SIZE_SMALL_TOOLBAR)
        self.exe.set_image(img_exe)
        self.exe.set_label("Run")
        self.exe.set_size_request(100,35)
        self.exe.connect("clicked", self.Generate_Texture)

        box_btn.pack_start(self.exit, True, True, 1)
        box_btn.pack_start(self.exe, True, True, 1)
        box_gen.pack_start(box_btn, True, True, 1)

        self.Generate_Load(self,"G")
        self.Set_Texture(self,0)
        self.win.show_all()

    def set_tooltip(self,chi,text):
        tooltips = gtk.Tooltips()
        tooltips.set_tip(chi, text)
    
    def Set_Basket_Width(self,widget):
        global laying
        if laying==3:
            self.striph.set_value(self.stripw.get_value()/5)

    def Set_Basket_Height(self,widget):
        global laying
        if laying==3:
            self.stripw.set_value(self.striph.get_value()*5)

    def Set_Laying(self,widget,data):
        global laying
        if data==0:
            laying=0
        elif data==1:
            laying=1
        elif data==2:
            laying=2
        elif data==3:
            laying=3
            self.striph.set_value(self.stripw.get_value()/5)
        elif data==4:
            laying=4
        elif data==5:
            laying=5

    def Set_Texture(self,widget,data):
        global texture_type
        if data==0:
            texture_type=0
            self.tile_effect.set_sensitive(False)
        elif data==1:
            texture_type=1
            self.tile_effect.set_sensitive(False)
        elif data==2:
            texture_type=2
            self.tile_effect.set_sensitive(True)

    def Load_Strip(self, widget):
        global dir_map        
        fin = gtk.FileChooserDialog("Load strip folder...",None,gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,gtk.STOCK_OPEN,gtk.RESPONSE_OK))
        fin.set_default_response(gtk.RESPONSE_OK)
        fin.set_position(gtk.WIN_POS_CENTER_ON_PARENT)
        response = fin.run()

        if response == gtk.RESPONSE_OK:
            dir_map_temp = fin.get_filenames()
            dir_map = dir_map_temp[0]

        elif response == gtk.RESPONSE_CANCEL:
            dir_map = ""
            fin.destroy()
        fin.destroy()
        

    def Generate_Load(self,widget,data):
        global dir_map        
        if data=="G":
            self.load_dir.set_sensitive(False)
            self.randomize.set_sensitive(False)
            self.wood_strip.set_sensitive(True)
            self.brick_strip.set_sensitive(True)
            self.tile_strip.set_sensitive(True)
            self.color.set_sensitive(True)
            dir_map = ""
        elif data=="L":
            self.load_dir.set_sensitive(True)
            self.randomize.set_sensitive(True)
            self.wood_strip.set_sensitive(False)
            self.brick_strip.set_sensitive(False)
            self.tile_strip.set_sensitive(False)
            self.color.set_sensitive(False)

    def Generate_Texture(self,widget):
        global laying
        global dir_map

        self.exit.set_sensitive(False)
        tex_w=int(self.texw.get_value())
        tex_h=int(self.texh.get_value())
        strip_w=int(self.stripw.get_value())
        strip_h=int(self.striph.get_value())
        rand_map=self.randomize.get_active()

        colore_base = self.color.get_color()
        
        #self.Show_msg(str(self.color.get_color()))
        #color_strip= gtk.gdk.color_parse( str(self.color.get_color()) )  
        color_strip=gtk.gdk.Color(colore_base.red,colore_base.green,colore_base.blue)
        bump_map=self.bump.get_active()
        merge_all=self.merge.get_active()

#***set progress*************************************
        self.pbar.set_text("0 %")
        self.pbar.set_fraction(0.0)
#***progress*************************************

        if self.gen_strip.get_active():
            dir_map=""
            tex_maps=""
        else:
            if dir_map=="":
                self.Show_msg("Load a folder!")
            else:
                tex_maps=os.listdir(dir_map)


        #create texture image
        tex_img=pdb.gimp_image_new(tex_w,tex_h,0)
        tex_img.disable_undo()
        
#        num_colonne=int(math.ceil(tex_w/strip_w))
#        num_righe=int(math.ceil(tex_h/strip_h))
        num_colonne=int(math.ceil(float(tex_w)/float(strip_w)))
        num_righe=int(math.ceil(float(tex_h)/float(strip_h)))

        if laying==0:
            self.Brick_Bond(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map)
        if laying==1:
            self.Brick_RandomX(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map)
        if laying==2:
            self.Brick_RandomXY(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map)
        if laying==3:
            dim_blocco=strip_w*2
            num_righe=int(math.ceil(float(tex_w)/float(dim_blocco)))
            num_colonne=int(math.ceil(float(tex_h)/float(dim_blocco)))
            self.Basket_5_1(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map)
        if laying==4:
            self.Herringbone(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map,tex_w,tex_h)
        if laying==5:
            self.Tile(num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map)
        #merge all layers
        if merge_all==1:
            pdb.gimp_image_merge_visible_layers(tex_img,1)
        tex_img.enable_undo()
        pdb.gimp_display_new(tex_img)


        if bump_map==1:
            #create bump
            tex_bump=pdb.gimp_image_new(tex_w,tex_h,0)
            tex_bump.disable_undo()
            #create new layer from visible to paste in bump map
            l_bump=pdb.gimp_layer_new_from_visible(tex_img,tex_bump,"")
            #add the new layer to the new image
            pdb.gimp_image_add_layer(tex_bump,l_bump,0)
            pdb.gimp_convert_grayscale(tex_bump)
            tex_bump.enable_undo()
            pdb.gimp_display_new(tex_bump)

        self.exit.set_sensitive(True)
#***progress*************************************
        self.pbar.set_text("100 %")
        self.pbar.set_fraction(1.0)
#***progress*************************************

#***Start Brick_Bond *****************************************************    
    def Brick_Bond(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map):
        count = 1
        percent=float(count)/(num_righe)
        fraz=0.0
        pos_x=0
        pos_y=0
        temp_y = pos_y
        for i in range(0,num_righe):
#***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            fraz=fraz+percent
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
#***progress*************************************
            if i % 2 == 0:
                pos_x=0
            else:
                pos_x=0-(strip_w/2)       
            for a in range(0,int(num_colonne+1)):
                if self.gen_strip.get_active():
                    if texture_type==0:
                        l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==1:
                        l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==2:
                        l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                else:
                    l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                #add the new layer to the new image
                pdb.gimp_image_add_layer(tex_img,l_temp,i)
                #scale layer 
                pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                #Set position of the strip
                temp_x = (pos_x+(strip_w*a))
                pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
            temp_y = (pos_y+(strip_h*(i+1)))
#***End Brick_Bond *****************************************************    

#***Start Brick_RandomX *****************************************************    
    def Brick_RandomX(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map):
        count = 1
        percent=float(count)/(num_righe)
        fraz=0.0
        pos_x=0
        pos_y=0
        temp_y = pos_y
        for i in range(0,num_righe):
#***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            fraz=fraz+percent
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
#***progress*************************************
            pos_x=0-random.randint(0,(strip_w/2))
            for a in range(0,int(num_colonne+1)):
                if self.gen_strip.get_active():
                    if texture_type==0:
                        l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==1:
                        l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==2:
                        l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                else:
                    l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                #add the new layer to the new image
                pdb.gimp_image_add_layer(tex_img,l_temp,i)
                #scale layer 
                pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                #Set position of the strip
                temp_x = (pos_x+(strip_w*a))
                pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
            temp_y = (pos_y+(strip_h*(i+1)))
#***End Brick_RandomX *****************************************************    
            
#***Start Brick_RandomXY *****************************************************    
    def Brick_RandomXY(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map):
        count = 1
        percent=float(count)/(num_righe)
        fraz=0.0
        pos_x=0
        pos_y=0
        pos_y=0-random.randint(0,(strip_h/2))
        temp_y = pos_y
        for i in range(0,num_righe*2):
#***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            fraz=fraz+percent
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
#***progress*************************************
            pos_x=0-random.randint(0,(strip_w/2))
            strip_h_t=random.randint((strip_h/2),strip_h)
            for a in range(0,int(num_colonne+1)):
                if self.gen_strip.get_active():
                    if texture_type==0:
                        l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==1:
                        l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==2:
                        l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                else:
                    l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                #add the new layer to the new image
                pdb.gimp_image_add_layer(tex_img,l_temp,i)
                #scale layer 
                pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                #Set position of the strip
                temp_x = (pos_x+(strip_w*a))
                pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
            temp_y = (temp_y+(strip_h_t))
#***End Brick_RandomXY *****************************************************    
            
#***Start Basket_5_1 **************************************************
    def Basket_5_1(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map):
        count = 1
#        percent=float(count)/(num_righe)
        percent=float(count)/((num_righe*num_colonne)*25)
        fraz=0.0
        pos_x=0
        pos_y=0
        pos_z=0
        temp_y=0
        dim_blocco=strip_w*2

        for i in range(0,num_righe):
            #***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            fraz=fraz+percent
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
            #***progress*************************************

            for a in range(0,num_colonne):
                #***progress*************************************
                while gtk.events_pending():
                    gtk.main_iteration()
                fraz=fraz+percent
                self.pbar.set_fraction(fraz)
                self.pbar.set_text(str(int(fraz * 100)) + "%")
                #***progress*************************************

                for b in range(0,5):#orizz1
                    #***progress*************************************
                    while gtk.events_pending():
                        gtk.main_iteration()
                    fraz=fraz+percent
                    self.pbar.set_fraction(fraz)
                    self.pbar.set_text(str(int(fraz * 100)) + "%")
                    #***progress*************************************
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,i)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Set position of the strip
                    temp_x = pos_x
                    temp_y = (pos_y+(strip_h*b))
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)

                for c in range(0,5):#vert2
                    #***progress*************************************
                    while gtk.events_pending():
                        gtk.main_iteration()
                    fraz=fraz+percent
                    self.pbar.set_fraction(fraz)
                    self.pbar.set_text(str(int(fraz * 100)) + "%")
                    #***progress*************************************
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,i)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Set position of the strip
                    temp_x = (pos_x + (strip_h*c)+strip_w)
                    temp_y = (pos_y)
                    #Rotation of the strip 90
                    pdb.plug_in_rotate(tex_img,l_temp,1,False)
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)

                for d in range(0,5):#vert3
                    #***progress*************************************
                    while gtk.events_pending():
                        gtk.main_iteration()
                    fraz=fraz+percent
                    self.pbar.set_fraction(fraz)
                    self.pbar.set_text(str(int(fraz * 100)) + "%")
                    #***progress*************************************
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,i)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Set position of the strip
                    #Set position of the strip
                    temp_x = (pos_x + (strip_h*d))
                    temp_y = pos_y+strip_w
                    #Rotation of the strip -90
                    pdb.plug_in_rotate(tex_img,l_temp,3,False)
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)

                for e in range(0,5):#orizz4
                    #***progress*************************************
                    while gtk.events_pending():
                        gtk.main_iteration()
                    fraz=fraz+percent
                    self.pbar.set_fraction(fraz)
                    self.pbar.set_text(str(int(fraz * 100)) + "%")
                    #***progress*************************************
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,i)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Set position of the strip
                    temp_x = (pos_x + (strip_w))
                    temp_y = (pos_y + strip_w + (strip_h*e))
                    #Rotation of the strip 180
                    pdb.plug_in_rotate(tex_img,l_temp,2,False)
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
                    
                pos_x = pos_x + dim_blocco
                
            pos_y = pos_y + dim_blocco
            pos_x = 0
#***End Basket_5_1 *****************************************************    

#***Start Herringbone **************************************************
    def Herringbone(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map,tex_w,tex_h):
        count = 1
        percent=float(count)/(num_righe)
        fraz=0.0
        pos_x=0
        pos_y=0
        pos_z=0
        temp_x=0
        temp_y=0
        alt_y=0
        self.pbar.set_text("")
        while (temp_x<=tex_w or alt_y==0):
            alt_y=0
            while pos_x+strip_w>=0 and pos_y<=tex_h:
                while gtk.events_pending():
                    gtk.main_iteration()
                self.pbar.pulse()
                a=0
                if temp_x<tex_w and temp_y<tex_h:
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,0)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Set position of the strip
                    temp_x = (pos_x+(strip_w*a))
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
                    a=a+1
                else:
                    pass
                temp_x=pos_x-strip_h
                temp_y=pos_y+strip_h
                pos_x=temp_x
                pos_y=temp_y
            else:
                temp_x=pos_x+strip_w
                pos_x=temp_x

            while pos_y+strip_w>=0 and alt_y==0:
                while gtk.events_pending():
                    gtk.main_iteration()
                self.pbar.pulse()
                if pos_y>tex_h and pos_x > tex_w:
                    alt_y=1
                else:
                    pass
                    
                if (temp_x-strip_h)<tex_w and temp_y<tex_h:
                    if self.gen_strip.get_active():
                        if texture_type==0:
                            l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==1:
                            l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                        if texture_type==2:
                            l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    else:
                        l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
                    #add the new layer to the new image
                    pdb.gimp_image_add_layer(tex_img,l_temp,0)
                    #scale layer 
                    pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                    #Rotation of the strip 90
                    pdb.plug_in_rotate(tex_img,l_temp,1,False)
                    #Set position of the strip
                    pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
                else:
                    pass
                temp_x=pos_x+strip_h
                temp_y=pos_y-strip_h
                pos_x=temp_x
                pos_y=temp_y
            else:
                temp_y=pos_y+(strip_w-strip_h)
                temp_x=pos_x+strip_h
                pos_x=temp_x
                pos_y=temp_y


#***Start Tile **************************************************
    def Tile(self,num_righe,num_colonne,dir_map,tex_maps,tex_img,strip_w,strip_h,color_strip,rand_map):
        global texture_type

        count = 1
        percent=float(count)/(num_righe)
        fraz=0.0
        pos_x=0
        pos_y=0
        temp_y = pos_y
        for i in range(0,num_righe):
#***progress*************************************
            while gtk.events_pending():
                gtk.main_iteration()
            fraz=fraz+percent
            self.pbar.set_fraction(fraz)
            self.pbar.set_text(str(int(fraz * 100)) + "%")
#***progress*************************************
            for a in range(0,int(num_colonne)):
                if self.gen_strip.get_active():
                    if texture_type==0:
                        l_temp=self.wood_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==1:
                        l_temp=self.brick_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                    if texture_type==2:
                        l_temp=self.tile_map(strip_w, strip_h, tex_img, rand_map, color_strip)
                else:
                    l_temp=self.load_img_map(dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h)
    
                #add the new layer to the new image
                pdb.gimp_image_add_layer(tex_img,l_temp,i)
                #scale layer 
                pdb.gimp_layer_scale(l_temp,strip_w,strip_h,False)
                #Set position of the strip
                temp_x = (pos_x+(strip_w*a))
                pdb.gimp_layer_set_offsets(l_temp,temp_x,temp_y)
            temp_y = (pos_y+(strip_h*(i+1)))
#***End Tile **************************************************
        
    def load_img_map(self,dir_map,tex_maps,tex_img,rand_map,strip_w,strip_h):
        #create layer from random map 
        img_random=os.path.join(dir_map,tex_maps[random.randint(0, (len(tex_maps)-1))])    
        img_temp=pdb.gimp_file_load(img_random,img_random)

        #random map
        if rand_map==1:
            transform=random.randint(0,2)
            if transform==1:
                pdb.gimp_image_flip(img_temp,0)
            if transform==2:
                pdb.gimp_image_flip(img_temp,1)
        l_temp=pdb.gimp_layer_new_from_visible(img_temp,tex_img,"")

        return l_temp

    def tile_map(self,strip_w, strip_h, tex_img, rand_map, color_strip):
        pdb.gimp_context_set_background((color_strip.red/256,color_strip.green/256,color_strip.blue/256))
        #base layer
        tex_temp=pdb.gimp_image_new(strip_w,strip_h,0)
        bklayer = gimp.Layer(tex_temp,"",strip_w,strip_h,RGB_IMAGE,100,NORMAL_MODE)
        tex_temp.add_layer(bklayer,0)
        pdb.gimp_edit_fill(bklayer,1)
        
        #effect layer
        effect=self.tile_effect.get_active()
        if effect==1:
            effect_layer = pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",70,5)
            tex_temp.add_layer(effect_layer,0)
            pdb.plug_in_plasma(tex_temp,effect_layer,random.randint(0,100),1)
            pdb.gimp_image_merge_visible_layers(tex_temp,1)
        
        pdb.gimp_context_set_foreground((255,255,230))
        grid=pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",100,0)
        tex_temp.add_layer(grid,0)
        pdb.gimp_selection_all(tex_temp)
        sel_shrink=0
        if strip_w<=100:
            sel_shrink=2
        if 100<strip_w<=200:
            sel_shrink=4
        if strip_w>200:
            sel_shrink=6
        pdb.gimp_selection_shrink(tex_temp,sel_shrink)
        pdb.gimp_selection_invert(tex_temp)
        pdb.gimp_edit_fill(grid,0)
        pdb.gimp_selection_none(tex_temp)
        pdb.plug_in_gauss(tex_temp,grid,5.0,5.0,0)

        pdb.gimp_image_merge_visible_layers(tex_temp,1)

        l_temp=pdb.gimp_layer_new_from_visible(tex_temp,tex_img,"")

        #destroy temp image
        pdb.gimp_image_delete(tex_temp)

        return l_temp
        
    def brick_map(self,strip_w, strip_h, tex_img, rand_map, color_strip):
        pdb.gimp_context_set_background((color_strip.red/256,color_strip.green/256,color_strip.blue/256))
        #base layer
        tex_temp=pdb.gimp_image_new(strip_w,strip_h,0)
        bklayer = gimp.Layer(tex_temp,"",strip_w,strip_h,RGB_IMAGE,100,NORMAL_MODE)
        tex_temp.add_layer(bklayer,0)
        pdb.gimp_edit_fill(bklayer,1)
        pdb.gimp_brightness_contrast(bklayer,random.randint(-100,0),0)
        
        #effect layer
        effect_layer = pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",70,3)
        tex_temp.add_layer(effect_layer,0)
        pdb.plug_in_solid_noise(tex_temp,effect_layer,0,0,1,2,random.random(),random.random())

        pdb.gimp_image_merge_visible_layers(tex_temp,1)

        #grid layer
        pdb.gimp_context_set_foreground((255,255,255))
        grid2=pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",100,0)
        tex_temp.add_layer(grid2,0)
        pdb.gimp_selection_all(tex_temp)
        pdb.gimp_selection_shrink(tex_temp,3)
        pdb.gimp_selection_invert(tex_temp)
        pdb.gimp_edit_fill(grid2,0)
        pdb.gimp_selection_none(tex_temp)

        grid=pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",100,0)
        tex_temp.add_layer(grid,0)
        pdb.gimp_selection_all(tex_temp)
        sel_shrink=0
        if strip_w<=100:
            sel_shrink=2
        if 100<strip_w<=200:
            sel_shrink=4
        if strip_w>200:
            sel_shrink=6
        pdb.gimp_selection_shrink(tex_temp,sel_shrink)
        pdb.gimp_selection_invert(tex_temp)
        pdb.gimp_edit_fill(grid,0)
        pdb.gimp_selection_none(tex_temp)
        pdb.plug_in_gauss(tex_temp,grid,5.0,5.0,0)
        pdb.plug_in_whirl_pinch(tex_temp,grid,random.randint(-180,180),random.random(),1.1)
        pdb.plug_in_hsv_noise(tex_temp,grid,random.randint(1,3),random.randint(10,20),random.randint(45,55),random.randint(15,25))

        pdb.gimp_image_merge_visible_layers(tex_temp,1)

        l_temp=pdb.gimp_layer_new_from_visible(tex_temp,tex_img,"")

    #    destroy temp image
        pdb.gimp_image_delete(tex_temp)

        return l_temp

    def wood_map(self,strip_w, strip_h, tex_img, rand_map, color_strip):
        pdb.gimp_context_set_background((color_strip.red/256,color_strip.green/256,color_strip.blue/256))
        #base layer
        tex_temp=pdb.gimp_image_new(strip_w,strip_h,0)
        bklayer = gimp.Layer(tex_temp,"",strip_w,strip_h,RGB_IMAGE,100,NORMAL_MODE)
        tex_temp.add_layer(bklayer,0)
        pdb.gimp_edit_fill(bklayer,1)
        pdb.plug_in_hsv_noise(tex_temp,bklayer,random.randint(2,3),25,random.randint(20,25),random.randint(50,55))
        pdb.plug_in_mblur(tex_temp,bklayer,0,15,0,0,0)
        pdb.plug_in_sharpen(tex_temp,bklayer,35)

        #bump layer
        l_copy=pdb.gimp_layer_copy(bklayer,False)
        tex_temp.add_layer(l_copy,0)
        pdb.gimp_desaturate(l_copy)
        pdb.plug_in_bump_map(tex_temp,bklayer,l_copy,245.0,50.0,25,0,0,0,0,1,0,0)
        pdb.gimp_image_remove_layer(tex_temp,l_copy)
        
        #grid layer
        grid=pdb.gimp_layer_new(tex_temp,strip_w,strip_h,1,"",100,0)
        tex_temp.add_layer(grid,0)
        pdb.gimp_selection_all(tex_temp)
        pdb.gimp_selection_shrink(tex_temp,1)
        pdb.gimp_selection_invert(tex_temp)
        pdb.gimp_edit_fill(grid,1)
        pdb.gimp_selection_none(tex_temp)
        pdb.gimp_brightness_contrast(grid,-75,0)

        l_temp=pdb.gimp_layer_new_from_visible(tex_temp,tex_img,"")

    #    destroy temp image
        pdb.gimp_image_delete(tex_temp)

        return l_temp


#            msg="Please insert a file name."
#            self.fine(msg)

    def Show_msg(self,msg):
        
        msg_box=gtk.MessageDialog(None, 0, gtk.MESSAGE_INFO, gtk.BUTTONS_CLOSE, msg)
        response = msg_box.run()
        if response == gtk.RESPONSE_CLOSE:
            msg_box.destroy()
        else:
            pass
        msg_box.destroy()
    
    def destroy(self, widget, data=None):
        self.win.destroy()
        return gtk.main_quit()
    
    def main(self): 
        gtk.main()

if __name__ == '__main__':

    register(
        "Gimp_Texture_Generator",
        N_("Texture Generator"),
        "Texture Generator",
        "Marco Crippa",
        "Marco Crippa",
        "2009",
        N_("Texture Generator"),
        "",
        [
#        (PF_SPINNER, "tex_w", "Texture Width:", 1200, (0, 9999999999, 1)),
#        (PF_SPINNER, "tex_h", "Texture Height:", 600, (0, 9999999999, 1)),
#        (PF_SPINNER, "strip_w", "Strip Width:", 0, (0, 9999999999, 1)),
#        (PF_SPINNER, "strip_h", "Strip Height:", 0, (0, 9999999999, 1)),
#        (PF_COLOR, "color_strip", "Color:", (100,50,0)),
#        (PF_RADIO, "laying",("Select the laying:"), 1,
#        ((("Brick Bond"),1),
#        (("Brick RandomX"),2),
#        (("Brick RandomXY"),3),
#        (("Basket 5/1"),4),
#        (("Herringbone"),5),
#        (("Tile"),6))),
#        (PF_BOOL,"rand_map", "Random flip map:", 0),
#        (PF_BOOL,"bump", "Bump map:", 0),
#        (PF_BOOL,"normal", "Normal map:", 0),
#        (PF_DIRNAME, "dir_map", "Dir map:", ""),
        ],
        [],
        T_G,
        menu="<Toolbox>/File/Save/"
    )

main()


