#!/usr/bin/python
#
# Copyright (C)2009 Laurentiu Badea
#
# Author:   Laurentiu Badea        sourceforge.net/users/wotevah
# Created:  Sep 5, 2009
# $Date: 2009-11-08 21:20:40 -0800 (Sun, 08 Nov 2009) $
#
# License
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# Version 2, as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to:
# Free Software Foundation, Inc.
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

"""Image Watermark GIMP Plugin for GIMP 2.6

Imprints a non-intrusive faint text watermark sparsely repeated over the
entire image. The watermark is meant to be visible upon close inspection
without disturbing the viewing of the image.

The watermark visibility can be fine-tuned by changing the opacity of the
watermark layer.

Install in the plug-ins GIMP directory. Make sure the file is executable
or GIMP will ignore it.

Contains an image plugin that operates on an open image and a batch plugin
that requires the filenames for the source and modified images.

Running in command-line batch mode (parameters are in the registered order):
gimp -ib '(python-fu-batch-watermark RUN-NONINTERACTIVE "image.jpg" "image-wm.jpg" 12 "(C)2009" "Sans Bold" 24)(gimp-quit 1)'

"""

__version__ = "$Revision: 10 $"

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

WHITE = (255,255,255)

def watermark(img, opacity, text, fontname, fontsize):
    gimp.context_push()
    img.undo_group_start()

    # Create watermark pattern
    pdb.gimp_context_set_foreground(WHITE)
    txt_layer = pdb.gimp_text_fontname(img, None, 0, 0, text, 50, 1,
                                       fontsize, POINTS, fontname)
    pdb.script_fu_selection_to_pattern(img, txt_layer, "watermark", "watermark")
    pdb.gimp_image_remove_layer(img, txt_layer)
    pdb.gimp_context_set_pattern("watermark")

    pdb.gimp_progress_set_text("Creating watermark layer")
    img_type = (img.base_type is RGB) and RGBA_IMAGE or GRAYA_IMAGE
    wm_layer = gimp.Layer(img, "Watermark", img.width, img.height, img_type,
                          opacity, GRAIN_MERGE_MODE)
    wm_layer.fill(TRANSPARENT_FILL)
    img.add_layer(wm_layer, 0)
    pdb.gimp_edit_bucket_fill_full(wm_layer, PATTERN_BUCKET_FILL, NORMAL_MODE,
                                   100, 255, 0, 1, SELECT_CRITERION_COMPOSITE,
                                   0, 0)
    img.undo_group_end()
    gimp.context_pop()


register("python-fu-watermark",
         N_("Watermark a photograph"),
         "Adds a repeated faint text watermark",
         "Laurentiu Badea",
         "Laurentiu Badea",
         "2009",
         N_("Faint Text Watermark..."),
         "RGB*, GRAY*",
         [
          (PF_IMAGE,  "image",    "Input image",           None),
          (PF_SLIDER, "opacity",  _("Opacity"),            12, (0,100,1)),
          (PF_STRING, "text",     _("Watermark text"),     _(u"\xa92009")),
          (PF_FONT,   "fontname", _("Font"),               "Sans Bold"),
          (PF_SLIDER, "fontsize", _("Font Size (points)"), 24, (12,64,2)),
         ],
         [],
         watermark,
         menu="<Image>/Filters/Watermark",
         domain=("gimp20-python", gimp.locale_directory)
        )


def batch_watermark(srcfile, dstfile, opacity, text, fontname, fontsize):
    img = pdb.gimp_file_load(srcfile, srcfile)
    pdb.python_fu_watermark(img, opacity, text, fontname, fontsize)
    pdb.gimp_image_flatten(img)
    drawable = pdb.gimp_image_get_active_layer(img)
    pdb.gimp_file_save(img, drawable, dstfile, dstfile)
    pdb.gimp_image_delete(img)


register(
        "python-fu-batch-watermark",
        N_("Watermark a photograph in batch mode"),
        "Adds a repeated faint watermark in batch mode",
        "Laurentiu Badea",
        "Laurentiu Badea",
        "2009",
        N_("Batch Faint Text Watermark..."),
        "",
        [
          (PF_FILE,   "srcfile",  "Image filename",        None),
          (PF_FILE,   "dstfile",  "Output filename",       None),
          (PF_SLIDER, "opacity",  _("Opacity"),            12, (0,100,1)),
          (PF_STRING, "text",     _("Watermark text"),     _(u"\xa92009")),
          (PF_FONT,   "fontname", _("Font"),               "Sans Bold"),
          (PF_SLIDER, "fontsize", _("Font Size (points)"), 24, (12,64,2)),
        ],
        [],
        batch_watermark,
        menu="<Image>/Filters/Watermark",
        domain=("gimp20-python", gimp.locale_directory)
        )

main()
