#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

#### copyright (c) 2013 Vesa Kivimäki
#### released under GNU General Public License v3
#### for license information see: https://www.gnu.org/licenses/gpl.html


import gimp, gimpplugin, math
from gimpenums import *
pdb = gimp.pdb
import gtk, gimpui, gimpcolor
from gimpshelf import shelf

PARNAME = "Selection-Bevel-Values"

def debugMessage(Message):
    dialog = gtk.MessageDialog(None, 0, gtk.MESSAGE_INFO, gtk.BUTTONS_OK, Message)
    dialog.run()
    dialog.hide()


class reuse_init(object):
        previewLayer = None

        def save_values(self, *values):
                data = []
                for v in values:
                     data.append(str(v))
                ndata = "|".join(data)
                if not gimp.parasite_find(PARNAME) == None:
                        gimp.parasite_detach(PARNAME)
                gimp.attach_new_parasite(PARNAME, 0, ndata)


        def restore_values(self):
                p = gimp.parasite_find(PARNAME)
                if p == None:
                        return None
                return p.data.split("|")




        def get_layer_pos(self, layer):
                i = 0
                while i < len(self.img.layers):
                        if layer == self.img.layers[i]:
                                return i
                else:
                        i += 1
                return -1


        def layer_exists(self, layer):
                return layer != None and layer in self.img.layers

        def removePreviews(self):
                if self.layer_exists(self.previewLayer):
                        self.img.remove_layer(self.previewLayer)
                        self.previewLayer = None
                gimp.displays_flush()

        def make_label(self, text):
                label = gtk.Label(text)
                label.set_use_underline(True)
                label.set_alignment(1.0, 0.5)
                label.show()
                return label

        def make_slider_and_spinner(self, init, min, max, step, page, digits):
                controls = {'adj':gtk.Adjustment(init, min, max, step, page), 'slider':gtk.HScale(), 'spinner':gtk.SpinButton()}
                controls['slider'].set_adjustment(controls['adj'])
                controls['slider'].set_draw_value(False)
                controls['spinner'].set_adjustment(controls['adj'])
                controls['spinner'].set_digits(digits)
                controls['slider'].show()
                controls['spinner'].show()
                return controls

        def make_blend_mode_box(self):
            return gimpui.IntComboBox((
              "Normal",        NORMAL_MODE,
              "Dissolve",      DISSOLVE_MODE,
              "Multiply",      MULTIPLY_MODE,
              "Divide",        DIVIDE_MODE,
              "Screen",        SCREEN_MODE,
              "Overlay",       OVERLAY_MODE,
              "Dodge",         DODGE_MODE,
              "Burn",          BURN_MODE,
              "Hard Light",    HARDLIGHT_MODE,
              "Soft Light",    SOFTLIGHT_MODE,
              "Grain Extract", GRAIN_EXTRACT_MODE,
              "Grain Merge",   GRAIN_MERGE_MODE,
              "Difference",    DIFFERENCE_MODE,
              "Addition",      ADDITION_MODE,
              "Subtract",      SUBTRACT_MODE,
              "Darken Only",   DARKEN_ONLY_MODE,
              "Lighten Only",  LIGHTEN_ONLY_MODE,
              "Hue",           HUE_MODE,
              "Saturation",    SATURATION_MODE,
              "Color",         COLOR_MODE,
              "Value",         VALUE_MODE
            ))

        def show_error_msg(self, msg):
                origMsgHandler = pdb.gimp_message_get_handler()
                pdb.gimp_message_set_handler(ERROR_CONSOLE)
                pdb.gimp_message(msg)
                pdb.gimp_message_set_handler(origMsgHandler)

        def stringToColor(self, string):
                colorlist = string[5:-1].split(", ")
                return gimpcolor.RGB(float(colorlist[0]), float(colorlist[1]), float(colorlist[2]), float(colorlist[3]))


class bevel(reuse_init):
        def __init__(self, runmode, img, drawable, lmode, up, inner, width, height, shape, prenoise, azimuth, elevation, depth, postblur, opacity, gloss, ialpha):
                self.img = img
                self.drawable = drawable
                self.shelfkey = 'bevel-plugin'

                self.activelayer = pdb.gimp_image_get_active_layer(img)

                if runmode == RUN_INTERACTIVE:
                      self.showDialog()
                elif runmode == RUN_NONINTERACTIVE:
                      if up == 0:
                          upb = False
                      if up == 1:
                          upb = True
                      if inner == 0:
                          innerb = False
                      if inner == 1:
                          innerb = True
                      if ialpha == 0:
                          ialphab = False
                      if ialpha == 1:
                          ialphab = True
                      fxlayer = self.makeLayer(img, drawable, lmode, upb, innerb, width, height, feather, shape, prenoise, preblur, azimuth, elevation, depth, postblur, opacity, gloss, ialphab )
                elif runmode == RUN_WITH_LAST_VALS:
                    d = self.restore_values()
                    if d == None:
                        self.showDialog()
                    else:
                        t_lmode = int(d[0])
                        if int(d[1]) == 0:
                            t_up = False
                        else:
                            t_up = True
                        if int(d[2]) == 0:
                            t_inner = False
                        else:
                            t_inner = True
                        width = int(d[3])
                        shape = int(d[4])
                        prenoise = float(d[5])
                        azimuth = float(d[6])
                        elevation = float(d[7])
                        depth = int(d[8])
                        postblur = float(d[9])
                        opacity = float(d[10])
                        gloss = float(d[11])
                        if int(d[12]) == 0:
                            t_ialpha = False
                        else:
                            t_ialpha = True
                        height = int(d[13])

                        fxlayer = self.makeLayer(img, drawable, t_lmode, t_up, t_inner, width, height, shape, prenoise, azimuth, elevation, depth, postblur, opacity, gloss, t_ialpha)


        def showDialog(self):
                self.dialog = gimpui.Dialog("Selection Bevel", "beveldialog")

                self.table = gtk.Table(16, 4, True)
                self.table.set_homogeneous(False)
                self.table.set_row_spacings(8)
                self.table.set_col_spacings(8)
                self.table.show()


                col = 0

#layer mode parameter
                col += 1

                self.lmode_label = self.make_label("_Layer mode:")
                self.table.attach(self.lmode_label, 0, 1, col, col+1)

                self.lmode_box = self.make_blend_mode_box()
                self.lmode_box.set_active(HARDLIGHT_MODE)
                self.lmode_label.set_mnemonic_widget(self.lmode_box)
                self.table.attach(self.lmode_box, 1, 4, col, col+1)
                self.lmode_box.show()

#direction parameter
                col += 1

                self.up_label = self.make_label("_Direction:")
                self.table.attach(self.up_label, 0, 1, col, col+1)

                self.up_radio = gtk.RadioButton(None, "_Up", True)
                self.down_radio = gtk.RadioButton(self.up_radio, "Do_wn", True)
                self.up_radio.set_active(True)
                self.down_radio.set_active(False)

                self.up_radio.show()
                self.down_radio.show()
                self.table.attach(self.up_radio, 1, 2, col, col+1)
                self.table.attach(self.down_radio, 2, 3, col, col+1)

#inner/outer parameter
                col += 1

                self.inner_label = self.make_label("_Bevel type:")
                self.table.attach(self.inner_label, 0, 1, col, col+1)

                self.inner_radio = gtk.RadioButton(None, "_Inner bevel", True)
                self.outer_radio = gtk.RadioButton(self.inner_radio, "Ou_ter bevel", True)
                self.inner_radio.set_active(True)
                self.outer_radio.set_active(False)

                self.inner_radio.show()
                self.outer_radio.show()
                self.table.attach(self.inner_radio, 1, 2, col, col+1)
                self.table.attach(self.outer_radio, 2, 3, col, col+1)




#width
                col += 1

                self.width_label = self.make_label("Bevel _width:")
                self.table.attach(self.width_label, 0, 1, col, col+1)

                self.width_spinner = self.make_slider_and_spinner(10.0, 1.0, 100.0, 0.5, 5.0, 1)
                self.width_spinner['adj'].set_value(10.0)

                self.width_label.set_mnemonic_widget(self.width_spinner['spinner'])
                self.table.attach(self.width_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.width_spinner['spinner'], 2, 3, col, col+1)

#height
                col += 1

                self.height_label = self.make_label("Bevel _height:")
                self.table.attach(self.height_label, 0, 1, col, col+1)

                self.height_spinner = self.make_slider_and_spinner(10.0, 1.0, 100.0, 0.5, 5.0, 1)
                self.height_spinner['adj'].set_value(10.0)

                self.height_label.set_mnemonic_widget(self.height_spinner['spinner'])
                self.table.attach(self.height_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.height_spinner['spinner'], 2, 3, col, col+1)



#shape
                col += 1

                self.shape_label = self.make_label("_Shape curve:")
                self.table.attach(self.shape_label, 0, 1, col, col+1)

                self.shape_spinner = self.make_slider_and_spinner(0.0, -64.0, 64.0, 1.0, 10.0, 0)
                self.shape_spinner['adj'].set_value(0.0)

                self.shape_label.set_mnemonic_widget(self.shape_spinner['spinner'])
                self.table.attach(self.shape_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.shape_spinner['spinner'], 2, 3, col, col+1)

#prenoise
                col += 1

                self.prenoise_label = self.make_label("Pre-emboss _noise:")
                self.table.attach(self.prenoise_label, 0, 1, col, col+1)

                self.prenoise_spinner = self.make_slider_and_spinner(0.0, 0.0, 25.0, 1.0, 10.0, 0)
                self.prenoise_spinner['adj'].set_value(0.0)

                self.prenoise_label.set_mnemonic_widget(self.prenoise_spinner['spinner'])
                self.table.attach(self.prenoise_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.prenoise_spinner['spinner'], 2, 3, col, col+1)




#azimuth
                col += 1

                self.azimuth_label = self.make_label("Light _angle:")
                self.table.attach(self.azimuth_label, 0, 1, col, col+1)

                self.azimuth_spinner = self.make_slider_and_spinner(135.0, 0.0, 360.0, 1.0, 10.0, 1)
                self.azimuth_spinner['adj'].set_value(135.0)

                self.azimuth_label.set_mnemonic_widget(self.azimuth_spinner['spinner'])
                self.table.attach(self.azimuth_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.azimuth_spinner['spinner'], 2, 3, col, col+1)

#elevation
                col += 1

                self.elevation_label = self.make_label("Light _elevation:")
                self.table.attach(self.elevation_label, 0, 1, col, col+1)

                self.elevation_spinner = self.make_slider_and_spinner(30.0, 0.0, 90.0, 1.0, 10.0, 1)
                self.elevation_spinner['adj'].set_value(30.0)

                self.elevation_label.set_mnemonic_widget(self.elevation_spinner['spinner'])
                self.table.attach(self.elevation_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.elevation_spinner['spinner'], 2, 3, col, col+1)

#depth
                col += 1

                self.depth_label = self.make_label("De_pth:")
                self.table.attach(self.depth_label, 0, 1, col, col+1)

                self.depth_spinner = self.make_slider_and_spinner(10.0, 1.0, 50.0, 1.0, 10.0, 0)
                self.depth_spinner['adj'].set_value(10.0)

                self.depth_label.set_mnemonic_widget(self.depth_spinner['spinner'])
                self.table.attach(self.depth_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.depth_spinner['spinner'], 2, 3, col, col+1)




#postblur
                col += 1

                self.postblur_label = self.make_label("Post-e_mboss blur:")
                self.table.attach(self.postblur_label, 0, 1, col, col+1)

                self.postblur_spinner = self.make_slider_and_spinner(1.0, 0.0, 50.0, 0.1, 1.0, 1)
                self.postblur_spinner['adj'].set_value(1.0)

                self.postblur_label.set_mnemonic_widget(self.postblur_spinner['spinner'])
                self.table.attach(self.postblur_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.postblur_spinner['spinner'], 2, 3, col, col+1)

#opacity
                col += 1

                self.opacity_label = self.make_label("Opac_ity:")
                self.table.attach(self.opacity_label, 0, 1, col, col+1)

                self.opacity_spinner = self.make_slider_and_spinner(100.0, 0.0, 100.0, 1.0, 10.0, 1)
                self.opacity_spinner['adj'].set_value(100.0)

                self.opacity_label.set_mnemonic_widget(self.opacity_spinner['spinner'])
                self.table.attach(self.opacity_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.opacity_spinner['spinner'], 2, 3, col, col+1)

#gloss
                col += 1

                self.gloss_label = self.make_label("_Gloss:")
                self.table.attach(self.gloss_label, 0, 1, col, col+1)

                self.gloss_spinner = self.make_slider_and_spinner(0.0, 0.0, 10.0, 1.0, 1.0, 0)
                self.gloss_spinner['adj'].set_value(0.0)

                self.gloss_label.set_mnemonic_widget(self.gloss_spinner['spinner'])
                self.table.attach(self.gloss_spinner['slider'], 1, 2, col, col+1)
                self.table.attach(self.gloss_spinner['spinner'], 2, 3, col, col+1)


#ialpha
                col += 1

                self.ialpha_check = gtk.CheckButton("Intersect with alpha")
                self.ialpha_check.show()
                self.ialpha_check.set_active(False)
                self.table.attach(self.ialpha_check, 1, 2, col, col+1)


#end parameters

#see if saved parameters are found, if so restore them

                d = self.restore_values()
                #debugMessage(str(d))
                if not d == None:
                        values =[
                                int(d[0]),
                                int(d[1]),
                                int(d[2]),
                                float(d[3]),
                                float(d[4]),
                                float(d[5]),
                                float(d[6]),
                                float(d[7]),
                                float(d[8]),
                                float(d[9]),
                                float(d[10]),
                                float(d[11]),
                                int(d[12]),
                                float(d[13])
                                ]

                        self.lmode_box.set_active(values[0])

                        if values[1] == 1:
                                self.up_radio.set_active(True)
                        elif values[1] == 0:
                                self.down_radio.set_active(True)

                        if values[2] == 1:
                                self.inner_radio.set_active(True)
                        elif values[2] == 0:
                                self.outer_radio.set_active(True)

                        self.width_spinner['adj'].set_value(values[3])
                        self.shape_spinner['adj'].set_value(values[4])
                        self.prenoise_spinner['adj'].set_value(values[5])

                        self.azimuth_spinner['adj'].set_value(values[6])
                        self.elevation_spinner['adj'].set_value(values[7])
                        self.depth_spinner['adj'].set_value(values[8])

                        self.postblur_spinner['adj'].set_value(values[9])
                        self.opacity_spinner['adj'].set_value(values[10])
                        self.gloss_spinner['adj'].set_value(values[11])

                        if values[12] == 1:
                            self.ialpha_check.set_active(True)
                        if values[12] == 0:
                            self.ialpha_check.set_active(False)

                        self.height_spinner['adj'].set_value(values[13])

#buttons

                self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
                self.dialog.vbox.hbox1.show()
                self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
                self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)

                reset_button = gtk.Button("Reset")
                reset_button.connect("clicked", self.resetbutton)
                reset_button.show()

                self.preview_button = gtk.Button("Preview")
                self.preview_button.connect("clicked", self.preview)
                self.preview_button.set_size_request(100, -1)
                self.preview_button.show()

 #               if gtk.alternative_dialog_button_order():
 #                       ok_button = self.dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
 #                       cancel_button = self.dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
 #                       self.dialog.action_area.add(reset_button)
 #                       self.dialog.action_area.add(self.preview_button)
 #               else:
                self.dialog.action_area.add(reset_button)
                self.dialog.action_area.add(self.preview_button)
                cancel_button = self.dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
                ok_button = self.dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)

                ok_button.connect("clicked", self.okbutton)
                cancel_button.connect("clicked", self.cancelbutton)
                self.dialog.show()
                self.dialog.run()
                self.removePreviews()

        def cancelbutton(self, widget):
                if self.layer_exists(self.previewLayer):
                        self.img.remove_layer(self.previewLayer)
                        self.previewLayer = None
        def okbutton(self, widget):
#                debugMessage("I got here")
                # remove old preview layer if it exists
                if self.layer_exists(self.previewLayer):
                        self.img.remove_layer(self.previewLayer)
                        self.previewLayer = None

                # get values of the settings
                lmode = self.lmode_box.get_active()
                up = self.up_radio.get_active()
                inner = self.inner_radio.get_active()

                width = self.width_spinner['adj'].get_value()
                height = self.height_spinner['adj'].get_value()
                shape = int(self.shape_spinner['adj'].get_value())
                prenoise = self.prenoise_spinner['adj'].get_value()

                azimuth = self.azimuth_spinner['adj'].get_value()
                elevation = self.elevation_spinner['adj'].get_value()
                depth = int(self.depth_spinner['adj'].get_value())

                postblur = self.postblur_spinner['adj'].get_value()
                opacity = self.opacity_spinner['adj'].get_value()
                gloss = self.gloss_spinner['adj'].get_value()

                ialpha = self.ialpha_check.get_active()

#save values to parasite
                if up:
                        upv = 1
                else:
                        upv = 0
                if inner:
                        innerv = 1
                else:
                        innerv = 0
                if ialpha:
                        ialphav = 1
                else:
                        ialphav = 0

                self.save_values(lmode, upv, innerv, width, shape, prenoise, azimuth, elevation, depth, postblur, opacity, gloss, ialphav, height)

                # then build new layer with current settings to actually impliment...

                fxlayer = self.makeLayer(self.img, self.drawable, lmode, up, inner, width, height, shape, prenoise, azimuth, elevation, depth, postblur, opacity, gloss, ialpha )




#resetbutton


        def resetbutton(self, widget):
                self.lmode_box.set_active(HARDLIGHT_MODE)
                self.up_radio.set_active(True)
                self.down_radio.set_active(False)
                self.inner_radio.set_active(True)
                self.outer_radio.set_active(False)

                self.width_spinner['adj'].set_value(10.0)
                self.height_spinner['adj'].set_value(10.0)
                self.shape_spinner['adj'].set_value(0.0)
                self.prenoise_spinner['adj'].set_value(0.0)

                self.azimuth_spinner['adj'].set_value(135.0)
                self.elevation_spinner['adj'].set_value(35.0)
                self.depth_spinner['adj'].set_value(20.0)

                self.postblur_spinner['adj'].set_value(3.0)
                self.opacity_spinner['adj'].set_value(100.0)
                self.gloss_spinner['adj'].set_value(0.0)

                self.ialpha_check.set_active(False)




        def preview(self, widget):
                ptxt = self.preview_button.get_label()

                if self.layer_exists(self.previewLayer):
                        self.img.remove_layer(self.previewLayer)
                        gimp.displays_flush()

                else:
                        lmode = self.lmode_box.get_active()
                        up = self.up_radio.get_active()
                        inner = self.inner_radio.get_active()

                        width = (self.width_spinner['adj'].get_value())
                        height = (self.height_spinner['adj'].get_value())
                        shape = int(self.shape_spinner['adj'].get_value())
                        prenoise = self.prenoise_spinner['adj'].get_value()

                        azimuth = self.azimuth_spinner['adj'].get_value()
                        elevation = self.elevation_spinner['adj'].get_value()
                        depth = int(self.depth_spinner['adj'].get_value())

                        postblur = self.postblur_spinner['adj'].get_value()
                        opacity = self.opacity_spinner['adj'].get_value()
                        gloss = self.gloss_spinner['adj'].get_value()

                        ialpha = self.ialpha_check.get_active()

                        self.previewLayer = self.makeLayer(self.img, self.drawable, lmode, up, inner, width, height, shape, prenoise,  azimuth, elevation, depth, postblur, opacity, gloss, ialpha )


                if ptxt == "Preview":
                        ptxt = "Undo Preview"
                else:
                        ptxt = "Preview"
                self.preview_button.set_label(ptxt)

#start effect

        def makeLayer(self, timg, tdrawable, lmode, up, inner, width, height, shape, prenoise,  azimuth, elevation, depth, postblur, opacity, gloss, ialpha):
                if timg.base_type is RGB:
                        ltype = RGBA_IMAGE
                else:
                        ltype = GRAYA_IMAGE

                if type(self.activelayer) == gimp.Layer:
                    pdb.gimp_image_set_active_layer(timg,self.activelayer)
                    activename = self.activelayer.name
                else:
                    activename = ""

                gimp.context_push()
                timg.undo_group_start()

        #create the bevel layer
                if inner:
                        lname = "Inner Bevel: " + activename
                else:
                        lname = "Outer Bevel: " + activename

                newlayer = gimp.Layer(timg, lname, timg.width, timg.height, ltype, opacity, lmode)
                timg.add_layer(newlayer, -1)

        #save current selection
                tmpchan2 = pdb.gimp_selection_save(timg)

        #intersect with alpha?
                if ialpha and (type(self.activelayer) == gimp.Layer):
                    pdb.gimp_image_select_item(timg, 2, self.activelayer)
                    tmpchan = pdb.gimp_selection_save(timg)
                    pdb.gimp_channel_combine_masks(tmpchan, tmpchan2, 3, 0, 0)
                    pdb.gimp_image_select_item(timg, 2, tmpchan)
                else:
                    tmpchan = pdb.gimp_selection_save(timg)

        #set fg and bg colours and fill the layer with black
                pdb.gimp_context_set_foreground((0,0,0))
                pdb.gimp_context_set_background((255,255,255))
                pdb.gimp_drawable_fill(newlayer, FOREGROUND_FILL)

        #fill the selection with white and apply blur
                pdb.gimp_edit_fill(newlayer, BACKGROUND_FILL)

        #at this point, select all...
                pdb.gimp_selection_all(timg)

        #blurs
        #if both are the same, do both at the same time...
                if (width == height):
                    pdb.plug_in_gauss_rle(timg, newlayer, width, 1, 1)
                else:                       #if not, blur separately
                    pdb.plug_in_gauss_rle(timg, newlayer, width, 1, 0)
                    pdb.plug_in_gauss_rle(timg, newlayer, height, 0, 1)

        #fix contours
                if inner:
                    pdb.gimp_curves_spline(newlayer,0,6, (0,0, 127,64, 255,255))
                else:
                    pdb.gimp_curves_spline(newlayer,0,6, (0,0, 128,192, 255,255))

        #apply shape curve
        #algorithm: 1st vertex = 96, 96        moved by -shape,   shape/2
        #           2nd vertex = 160,160                -shape/2, shape
        #

                if shape != 0:
                        shape2 = shape // 2
                        pdb.gimp_curves_spline(newlayer, 0, 8, (0,0, (96 - shape), (96 + shape2), (160 - shape2), (160 + shape),  255,255))


        #invert colours of whole layer if down
                if not up:
                        pdb.gimp_invert(newlayer)

        #prenoise
                if prenoise > 0:
                        pdb.plug_in_hsv_noise(timg, newlayer, (3 + (prenoise//5) ), 0, 0, prenoise)

        #emboss the selection
                pdb.plug_in_emboss(timg, newlayer, azimuth, elevation, depth, 1)

        #gloss
                if gloss > 0:
                    y1 = int(1.0 * gloss)
                    y2 = int(4.5 * gloss)
#                    y3 = int(9.6 * gloss)
                    y4 = int(3.2 * gloss)
                    y5 = int(5.0 * gloss)
                    y6 = int(0.4 * gloss)

                    pdb.gimp_curves_spline(newlayer, 0, 16, (0,0, 86,(86 + y1), 95,(95 + y2), 128,128, 156,(156 + y4), 191,(191 - y5), 223,(223 + y6), 255,255))

        #postblur
                if postblur > 0:
                        pdb.plug_in_gauss_rle(timg, newlayer, postblur, 1, 1)

                pdb.gimp_layer_set_lock_alpha(newlayer,False)





        #reload selection
                pdb.gimp_image_select_item(timg, 2, tmpchan)

        #clear excess
                if inner:
                        pdb.gimp_selection_invert(timg)
                if not pdb.gimp_selection_is_empty(timg):
                    pdb.gimp_edit_clear(newlayer)


        #reload original selection
                pdb.gimp_image_select_item(timg, 2, tmpchan2)


        #remove temp channels
                timg.remove_channel(tmpchan)
                timg.remove_channel(tmpchan2)

        #set active layer to what it was
                if type(self.activelayer) == gimp.Layer:
                    pdb.gimp_image_set_active_layer(timg, self.activelayer)
                else:
                    pdb.gimp_image_set_active_layer(timg, newlayer)


        #end undo group and finish plugin
                gimp.context_pop()
                timg.undo_group_end()
                pdb.gimp_displays_flush()

#                debugMessage("I got to end of makelayer")

                return newlayer



class pyBevel(gimpplugin.plugin):
        def start(self):
                gimp.main(self.init, self.quit, self.query, self._run)

        def init(self):
                pass

        def quit(self):
                pass

        def query(self):
                authorname = "dd"
                copyrightname = "dd"
                menu_location = "<Image>/Filters/Distorts/Bevel..."
                date = "2013"
                plug_description = "Selection bevel v2.3"
                plug_help = "Applies a bevel in the shape of the selection (or layer alpha channel, or both)"
                plug_params = [
                        (PDB_INT32, "run_mode", "Run mode"),
                        (PDB_IMAGE, "image", "Input image"),
                        (PDB_DRAWABLE, "drawable", "Input drawable"),
                        ####### 3 params above needed by all scripts using gimpplugin.plugin ######################

#bevel parameters - radio buttons
                        (PDB_INT32, "lmode", "Layer mode"),
                        (PDB_INT32, "up", "Direction"),
                        (PDB_INT32, "inner", "Type"),

#sliders
                        (PDB_FLOAT, "width", "Bevel width"),
                        (PDB_FLOAT, "height", "Bevel height"),
                        (PDB_FLOAT, "shape", "Shape"),
                        (PDB_FLOAT, "prenoise", "Pre-emboss noise"),

                        (PDB_FLOAT, "azimuth", "Light angle"),
                        (PDB_FLOAT, "elevation", "Light elevation"),
                        (PDB_FLOAT, "depth", "Depth"),

                        (PDB_FLOAT, "postblur", "Post-emboss blur"),
                        (PDB_FLOAT, "opacity", "Layer opacity"),
                        (PDB_FLOAT, "gloss", "Gloss"),

                        (PDB_INT32, "ialpha", "Intersect with alpha")
                        ]

                gimp.install_procedure("python_fu_bevel",
                        plug_description,
                        plug_help,
                        authorname,
                        copyrightname,
                        date,
                        menu_location,
                        "RGB*, GRAY*",
                        PLUGIN,
                        plug_params,
                        [])

        def python_fu_bevel(self, runmode, img, drawable, lmode=NORMAL_MODE, up=0, inner=0, width=1.0, height=1.0, shape=1.0, prenoise=1.0, azimuth=135.0, elevation=30.0, depth=20.0, postblur=1.0, opacity=100.0, gloss=0.0, ialpha=0):
                bevel(runmode, img, drawable, lmode, up, inner, width, height, shape, prenoise, azimuth, elevation, depth, postblur, opacity, gloss, ialpha)

if __name__ == '__main__':
        pyBevel().start()
