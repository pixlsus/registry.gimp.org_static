#!/usr/bin/env python
# -*- coding: utf-8 -*-
#- HalftoneCluster - Nitrofurano - 0806241812 - GPL2 licence - (thanks DanielPC help)
from gimpfu import *
def python_filter(img,layer):
  region=layer.get_pixel_rgn(0,0,img.width,img.height,True,False);
  clust=[[0,6,8,14],[2,12,4,10],[8,14,0,6],[4,10,2,12]]
  clidx=[0x000000]*8 #- SyntaxError: invalid syntax (?)
  clidx[0]=0x000000;clidx[1]=0x0000FF;clidx[2]=0xFF0000;clidx[3]=0xFF00FF
  clidx[4]=0x00FF00;clidx[5]=0x00FFFF;clidx[6]=0xFFFF00;clidx[7]=0xFFFFFF
  for y1 in range(0,img.height,1):
    print int(100*y1/img.height)
    for x1 in range(0,img.width,1):
      str1=region[x1,y1];r=ord(str1[0]);g=ord(str1[1]);b=ord(str1[2])
      patgf=int(((clust[x1 % 4][y1 % 4]+1)*255)/16) #- mod=% ?
      rpat=0
      if r>patgf:rpat=1      gpat=0
      if g>patgf:gpat=1      bpat=0
      if b>patgf:bpat=1
      o4b=(bpat+(rpat*2)+(gpat*4)) % 8
      # ink(clidx[o4b]):dot(x,y)
      str2=chr(int(clidx[o4b]/65536))+chr(int(clidx[o4b]/256)% 256)+chr(clidx[o4b] % 256)
      region[x1,y1]=str2
  layer.update(0,0,img.width,img.height)
register(
  "python_fu_halftonecluster",
  "halftonecluster - a very simple picture filter using PythonGimp",
  "","Nitrofurano","Nitrofurano","2008",
  "<Image>/Filters/_HalftoneCluster",
  # "RGB*, GRAY*",
  "RGB*",
  [],[],
  python_filter)
main()
#- benchmark: 20 seconds with a 320x200 picture, in a core2duo (2 cores) at 1.85ghz
#- missing progress bar

