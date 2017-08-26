#!/usr/bin/env python

###################################################
##### Group Change Font - GIMP plugin
##### (c) Vesa Kivimäki 2012
##### released under GNU General Public License v2
###################################################

from gimpfu import *
import gtk

def debugMessage(Message):
    dialog = gtk.MessageDialog(None, 0, gtk.MESSAGE_INFO, gtk.BUTTONS_OK, Message)
    dialog.run()
    dialog.hide()

def py_group_chfont(img, tdraw, font, size, cfont, csize, rec):

### nested function to parse group layers ###
        def dogroup(group):
              ll = group.layers
              for l in ll:
                 
### recursion for nested groups ###
                 if (type(l) == gimp.GroupLayer):
                   if rec: 
                     dogroup(l)

### actual action ###
                 else:
                    if pdb.gimp_item_is_text_layer(l):
                       if cfont:
                         pdb.gimp_text_layer_set_font(l, font)
                       if csize:
                         pdb.gimp_text_layer_set_font_size(l, size, 0)
                                               


### main function ###

          

        pdb.gimp_image_undo_group_start(img)

### get active layer             
        draw = img.active_layer

### only do if active layer is a group
        if (type(draw) == gimp.GroupLayer):
              dogroup(draw)         
        
        else:
          debugMessage("This is not a layer group!")

        pdb.gimp_image_undo_group_end(img)

register(
        "py_group_chfont",
        "Change font and size of all text layers in a group",
        "Change font and size of all text layers in a group",
        "dd",
        "dd",
        "2012",
        "<Image>/Group/Change font...", 
        "RGB*, GRAY*",
        [
                (PF_FONT, "font", "Font", "Sans"),
                (PF_SPINNER, "size", "Font size", 10, (1, 8192, 1)),
                (PF_TOGGLE, "cfont", "Change font", True),
                (PF_TOGGLE, "csize", "Change size", False),
                (PF_TOGGLE, "rec", "Recurse nested groups", True)
        ],
        [],
        py_group_chfont)

main()
