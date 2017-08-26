#!/usr/bin/env python
# -*- coding: utf-8 -*-
#- flips x and y 8x8 pixel cells
#- Nitrofurano - 0806241812 - GPL2 licence
#- thanks to the code template from DanielPC - without it i couldn't do it
from gimpfu import *
def python_flipcells(img, layer):
   print img.width,"x",img.height,"\n"
   picture=layer.get_pixel_rgn(0,0,img.width,img.height,True,False);
   for y2 in range(picture.y,picture.y+picture.h,8):
     print y2
     for x2 in range(picture.x,picture.x+picture.w,8):
       for y1 in range(0,8,1):
         for x1 in range(0,4,1):
           x=x1+x2;y=y1+y2;xq=x2+7-x1;yq=y2+7-y1
           str1=picture[x,y];str2=picture[xq,yq];picture[x,y]=str2;picture[xq,yq]=str1
   layer.update(0,0,img.width,img.height)
register(
        "python_fu_flipcells",
        "flipcells - a very simple picture filter using PythonGimp",
        "",
        "Nitrofurano",
        "Nitrofurano",
        "2008",
        "<Image>/Filters/_FlipCells",
        "RGB*, GRAY*",
        [
        ], 
        [],
        python_flipcells)
main()
#- benchmark: 20 seconds with a 320x200 picture, in a core2duo (2 cores) at 1.85ghz


