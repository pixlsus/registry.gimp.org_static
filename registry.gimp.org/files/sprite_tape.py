#!/usr/bin/env python

from gimpfu import *

def python_sprite_tape(timg, tdrawable, cnt_hor=8, cnt_vert=8):
     width = timg.width
     height = timg.height
     timg.resize(width*cnt_hor,height*cnt_vert,0,0)
     for y in range(cnt_vert):
       for x in range(cnt_hor):
         layer_numb=x+(y*cnt_hor)
         if layer_numb>=len(timg.layers):
           break
         timg.layers[layer_numb].translate(x*width,y*height)
     timg.merge_visible_layers(0)

register(
        "python_fu_sprite_tape",
        "Make the tape of sprites from separate sprites in layers",
        "Make the tape of sprites from separate sprites in layers",
        "FedeX",
        "Pheodor Tsapanas a.k.a. FedeX",
        "2007",
        "<Image>/Python-Fu/Sprites/Sprite tape",
        "RGBA, RGB",
        [
                (PF_INT, "cnt_hor", "Sprites hor.",8 ),
                (PF_INT, "cnt_vert", "Sprites vert.", 8),
        ],
        [],
        python_sprite_tape)

main()
