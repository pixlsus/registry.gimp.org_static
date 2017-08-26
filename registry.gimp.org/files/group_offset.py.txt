#!/usr/bin/env python

###################################################
##### Group Offset - GIMP plugin
##### (c) Vesa Kivimäki 2012
##### released under GNU General Public License v2
###################################################

from gimpfu import *
import gtk

def debugMessage(Message):
    dialog = gtk.MessageDialog(None, 0, gtk.MESSAGE_INFO, gtk.BUTTONS_OK, Message)
    dialog.run()
    dialog.hide()

def py_group_offset(img, tdraw, xoffset, yoffset, edge, half):

### nested function to parse group layers ###
        def dogroup(group):
              ll = group.layers
              for l in ll:
                 
### recursion for nested groups ###
                 if (type(l) == gimp.GroupLayer):
                   dogroup(l)

### actual action ###
                 else:
                   if (edge == "wrap"):
                     pdb.gimp_drawable_offset(l, True, 0, xoffset, yoffset)
                   if (edge == "bg"):
                     pdb.gimp_drawable_offset(l, False, 0, xoffset, yoffset)
                   if (edge == "trans"):
                     pdb.gimp_drawable_offset(l, False, 1, xoffset, yoffset)
                                     


### main function ###
### if half is set, set offsets to 1/2 image size
        if half:
           xoffset = (img.width // 2)
           yoffset = (img.height // 2)
          

        pdb.gimp_image_undo_group_start(img)

### get active layer             
        draw = img.active_layer

        if (type(draw) == gimp.GroupLayer):
              dogroup(draw)         
        
        else:
          debugMessage("This is not a layer group!")

        pdb.gimp_image_undo_group_end(img)

register(
        "py_group_offset",
        "Apply offset layer to all layers in group",
        "Apply offset layer to all layers in group",
        "dd",
        "dd",
        "2012",
        "<Image>/Group/Offset...", 
        "RGB*, GRAY*",
        [
                (PF_SPINNER, "xoffset", "X Offset", 0, (-32768, 32767, 1)),
                (PF_SPINNER, "yoffset", "Y Offset", 0, (-32768, 32767, 1)),
                (PF_RADIO, "edge", "Edge behaviour", "wrap", (
                     ("Wrap around", "wrap"),
                     ("Fill with bg", "bg"),
                     ("Transparent", "trans"))),
                (PF_TOGGLE, "half", "Offset by 1/2 of image", False)
        ],
        [],
        py_group_offset)

main()
