#!/usr/bin/env python
# -*- coding: utf-8 -*-
import operator
from gimpfu import *
# print img.width,"x",img.height,"\n"
def python_filter(img, layer):
  region=layer.get_pixel_rgn(0,0,img.width,img.height,True,False);
  for y in range(0,img.height,1):
    yfact=operator.truediv(y,img.height)
    # print yfact
    gimp.progress_update(yfact) 
    for x in range(0,img.width/2,1):
      str1=region[x,y]
      str2=region[x+int(img.width/2),y]
      r1=ord(str1[0]);g1=ord(str1[1]);b1=ord(str1[2])
      r2=ord(str2[0]);g2=ord(str2[1]);b2=ord(str2[2])
      str1=chr(r2)+chr(g1)+chr(b1)
      str2=chr(r1)+chr(g2)+chr(b2)
      region[x,y]=str1
      region[x+int(img.width/2),y]=str2
  layer.update(0,0,img.width,img.height)
register(
  "python_fu_crosseye2anaglyph", ##??
  "crosseye2anaglyph",
  "","Nitrofurano","Nitrofurano","2008",
  "<Image>/Filters/crosseye2anaglyph", # indica onde vai ficar no menu
  "RGB*",
  [],[],python_filter)
main()

#- crosseye2anaglyph - Paulo Silva - GPL - 0807311048 (with some help from DanielPC's code template)



