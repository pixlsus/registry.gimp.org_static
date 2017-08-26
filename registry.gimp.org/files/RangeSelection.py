#!/usr/bin/env python
#
# ==============================================================================
#                   RangeSelection.py V7 - (c) Berthold Hinz 2009
# ==============================================================================
#
# RANGE SELECTION FOR THE GIMP
#
# ==============================================================================
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# ==============================================================================
from gimpfu import *
from string import *

def RangeSelection(img,drw,channel,amount,area,width_of_range,border_l,border_r,feathering,sel_operation,addition_of_higlighting_layer,highlighting_color,mask,mask_mode, mask_format):
    pdb.gimp_image_undo_group_start(img)
    if pdb.gimp_drawable_is_layer_mask(drw)==1:
        m="The current drawable is a layer mask."+ "\n"+"The sript doesn't work on layer masks."+ "\n"+"\n"+ "Please activate the layer and run the script again!"
        gimp.message(str(m))
    else:
        selop={0:'add', 1:'subtract', 2:'replace', 3:'intersect'}
        sel_exists=1-pdb.gimp_selection_is_empty(img)
        if sel_exists==1 and sel_operation<>2:
            pdb.gimp_selection_save(img)
            stored_selection=pdb.gimp_image_get_active_channel(img)
        elif sel_operation<>2:
            m="There was no selection yet." + "\n"+ "\n"+"The chosen operation '" + str(selop[sel_operation])+ "' was ignored!"
            gimp.message(m)
        # deletes former selections
        pdb.gimp_selection_none(img)
        base_layer=drw
        name=drw.name
        channel_number={'R':0,'G':1,'B':2, 'S':1, 'V':2, 'L': 0, 'C':0, 'M':1, 'Y':2, 'Sr':1,'Sg':1,'Sb':1,'Sc':1,'Sm':1,'Sy':1, 'E':2}
        cl={0:'V',1:'L',2:'R',3:'G',4:'B',5:'C',6:'M',7:'Y',8:'S',9:'S',10:'S',11:'S',12:'S',13:'S',14:'S', 15:'V'}
        channel_letter=cl[channel]
        if "RGB".find(channel_letter)>-1:
            cmod="RGB"
        elif "HSV".find(channel_letter)>-1:
            cmod="HSV"
        elif "CMY".find(channel_letter)>-1:
            cmod="CMY"
        elif "LAB".find(channel_letter)>-1:
            cmod="LAB"
        itype=cmod+"_IMAGE"
        nr_base=pdb.gimp_image_get_layer_position(img,drw)

        # deletes former selections
        pdb.gimp_selection_none(img)

        # remembers initial background color
        bgc = gimp.get_background()

        if channel>8:
            # creates copy of drawable if channel for single colour saturation was selected
            drw=img.layers[0]
            layer_desat = drw.copy(True)
            layer_desat.mode = NORMAL_MODE
            layer_desat.name = "desaturated"
            layer_desat.opacity=100.0
            img.add_layer(layer_desat,-1)
            drw=layer_desat
            if channel<>9:
                pdb.gimp_hue_saturation(drw,1,0,0,-100)
            if channel<>14:
                pdb.gimp_hue_saturation(drw,2,0,0,-100)
            if channel<>10:
                pdb.gimp_hue_saturation(drw,3,0,0,-100)
            if channel<>12:
                pdb.gimp_hue_saturation(drw,4,0,0,-100)
            if channel<>11:
                pdb.gimp_hue_saturation(drw,5,0,0,-100)
            if channel<>13:
                pdb.gimp_hue_saturation(drw,6,0,0,-100)
            nr_base=nr_base+1
            if channel==15:
                pdb.plug_in_edge(img,drw,amount,1,0)

        # creates new layer for displaying selection
        DEC_img=pdb.plug_in_decompose(img, drw, cmod, 1)[0]
        width = drw.width
        height = drw.height
        layer_check = gimp.Layer(img, "check", width, height, RGB_IMAGE, 100, NORMAL_MODE)
        img.add_layer(layer_check, 0)
        pdb.gimp_edit_copy(DEC_img.layers[channel_number[channel_letter]])
        pdb.gimp_edit_paste(layer_check,0)
        pdb.gimp_floating_sel_anchor(img.layers[0])
        layer_check.mode = NORMAL_MODE
        layer_check.opacity=50.0
        drw=img.layers[0]
        pdb.gimp_image_delete(DEC_img)

     # calculates borders of range ('min' and 'max')
        if area=="highlights":
            min=255-255.0/100*width_of_range
            max=255
        elif area=="shadows":
            min=0
            max=255.0/100*width_of_range
        elif area=="midtones":
            min=127.5-(255.0/100*width_of_range/2.0)
            max=127.5+(255.0/100*width_of_range/2.0)
        else:
            min=border_l
            max=border_r
            if max<min:
                min=border_r
                max=border_l
        min=round(min)
        max=round(max)

        # creates checking layer name that contains chosen settings
        if channel==15: channel_letter='E'
        LName=name+" " + str(int(min))+";"+str(int(max))+" "+channel_letter
        if channel==9: LName=LName+"r"
        if channel==10: LName=LName+"g"
        if channel==11: LName=LName+"b"
        if channel==12: LName=LName+"c"
        if channel==13: LName=LName+"m"
        if channel==14: LName=LName+"y"
        if area<>"custom":
            if area=="highlights":
                LName=LName+" (" + str(width_of_range)+"% HL)"
            if area=="midtones":
                LName=LName+" (" + str(width_of_range)+"% MT)"
            if area=="shadows":
                LName=LName+" (" + str(width_of_range)+"% SH)"
        if channel==15:
            LName=LName + " ED"+str(int(amount))
        if feathering>0:
                LName=LName + " F"+str(int(feathering))
        if sel_exists==1:
            if sel_operation==0:
                LName=LName+" (add.)"
            if sel_operation==1:
                LName=LName+" (subtr.)"
            if sel_operation==2:
                LName=LName+" (repl.)"
            if sel_operation==3:
                LName=LName+" (inters.)"

        # creates copy of drawable if conversion to layer mask is activated
        if mask==True:
            drw=img.layers[nr_base+1]
            layer_copy = drw.copy(True)
            layer_copy.mode = NORMAL_MODE
            layer_copy.name = LName
            layer_copy.opacity=100.0
            img.add_layer(layer_copy,-1)

        # creates selection
        layer_number=0
        if mask==True:
            layer_number=layer_number+1
        if min>0 and max==255:
            pdb.plug_in_exchange (img,img.layers[layer_number],255,255,255,255,255,255,255-min,255-min,255-min)
            pdb.gimp_by_color_select(img.layers[layer_number],(255,255,255), 0, 0, True, True, feathering, False)
        if max<255 and min==0:
            pdb.gimp_invert(img.layers[layer_number])
            pdb.plug_in_exchange (img,img.layers[layer_number],255,255,255,255,255,255,max,max,max)
            pdb.gimp_by_color_select(img.layers[layer_number],(255,255,255), 0, 0, True, True, feathering, False)
            pdb.gimp_invert(img.layers[layer_number])

        if max<255 and min>0:
            pdb.gimp_selection_none(img)
            THR=int(max-min)/2
            AM=int(min+THR)
            CS=(AM,AM,AM)
            pdb.gimp_by_color_select(img.layers[layer_number],(AM,AM,AM), THR, 0, True, True, feathering, False)

        if sel_exists==1 and sel_operation<>2:
            if sel_operation<>1:
                # if mode is not 'subtract'
                pdb.gimp_selection_combine(stored_selection,sel_operation)
            else:
                # subtract new selection from old one
                pdb.gimp_selection_save(img)
                stored_new_selection=pdb.gimp_image_get_active_channel(img)
                pdb.gimp_selection_load(stored_selection)
                pdb.gimp_selection_combine(stored_new_selection,sel_operation)

        pdb.gimp_layer_add_alpha(img.layers[layer_number])
        if max<255 and min==0:
            pdb.gimp_invert(img.layers[layer_number])

        # erases unselected parts of the image
        pdb.gimp_selection_invert(img)
        pdb.plug_in_threshold_alpha(img,img.layers[layer_number],255)
        pdb.gimp_selection_invert(img)

        # highlights selection according chosen highlighting color
        gimp.set_background(highlighting_color)
        pdb.gimp_edit_fill(img.layers[layer_number],1)
        gimp.set_background(bgc)
        drw=layer_check

        # conversion to layer mask
        if mask==True:
            drw=base_layer
            layer_masked = drw.copy(True)
            layer_masked.mode = NORMAL_MODE
            layer_masked.name = "masked"
            layer_masked.opacity=100.0
            img.add_layer(layer_masked,-1)
            gimp.set_background(0,0,0)
            if mask_format==0:
                pdb.gimp_edit_fill(img.layers[0],1)
            pdb.gimp_edit_copy(layer_masked)
            if pdb.gimp_layer_get_mask(img.layers[1]) <>None:
                pdb.gimp_layer_remove_mask(img.layers[1],0)
            mask = pdb.gimp_layer_create_mask(base_layer, ADD_WHITE_MASK)
            pdb.gimp_layer_add_mask(img.layers[1],mask)
            pdb.gimp_edit_paste(mask,0)
            drw=mask
            pdb.gimp_floating_sel_anchor(img.layers[0])
            if mask_mode==0:
                pdb.gimp_invert(mask)
            if mask_format==1:
                pdb.gimp_levels_stretch(mask)
            pdb.gimp_image_remove_layer(img,img.layers[0])
            pdb.gimp_image_lower_layer(img,img.layers[0])
        img.layers[0].name =LName
        pdb.gimp_image_set_active_layer(img,img.layers[0])
        if addition_of_higlighting_layer==False:
            pdb.gimp_image_remove_layer(img,img.layers[0])

        if channel>8:
            pdb.gimp_image_remove_layer(img,layer_desat)

    pdb.gimp_image_undo_group_end(img)

register(
    "plug-in-range-selection",
    "",
    "This plug-in creates selections of ranges (of values, colors, saturation or edges). You can chose one of 16 different channels and set a percentage width of range to be selected for highlights, midtones or shadows of this channel." + "\n"+"Alternatively you can define the left and the right border for a custom range."+"\n"+"The created selection can be converted to a layer mask if desired." +"\n"+"After running the script there will be an adittional layer on top that highlights the selection. The name of this layer contains the chosen settings.",
    "Berthold Hinz",
    "public domain",
    "2009",
    "<Image>/Select/Adagio Range Selection",
    "RGB*, GRAY*",
        [(PF_OPTION,"channel",("Channel: "),0,["[V] Value","[L] Luminance","[R] Red","[G] Green","[B] Blue","[C] Cyan","[M] Magenta","[Y] Yellow","[S] Saturation","[Sr] Saturation red","[Sg] Saturation green","[Sb] Saturation blue","[Sc] Saturation cyan","[Sm] Saturation magenta","[Sy] Saturation yellow","[E] Edges"]),
        (PF_SLIDER,"amount","     If [E]: Amount of edge detection [ED]:",2,(1,50,1)),
        (PF_RADIO,"mode",("Mode:"),"highlights",(("Highlights [HL]","highlights"),("Midtones [MT]","midtones"),("Shadows [SH]","shadows"),("Custom","custom"))),
        (PF_SLIDER,"width_of_range","Width of range (%):",33,(1,100,1)),
        (PF_SLIDER,"border_l","      Left border (custom mode):",0,(0,254,1)),
        (PF_SLIDER,"border_r","      Right border (custom mode):",255,(1,255,1)),
        (PF_SLIDER,"feathering","Feathering [F]:",2,(0,50,1)),
        (PF_OPTION,"sel_operation",("Operation (if there's already a selection):"),2,["Add","Subtract","Replace","Intersect"]),
        (PF_TOGGLE,"addition_of_higlighting_layer","Add highlighting layer:", True),
        (PF_COLOR,"highlighting_color","      Highlighting color:",(29,255,210)),
        (PF_TOGGLE,"mask","Convert to layer mask (sel. will be deleted):",True),
        (PF_OPTION,"mask_mode",("      Layer mask mode:"),0,["White on black","Black on white"]),
        (PF_OPTION,"mask_format",("      Layer mask format:"),1,["B&W","Grayscale"])
    ],
    [],
    RangeSelection)
main()
