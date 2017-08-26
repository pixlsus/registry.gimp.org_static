#!/usr/bin/env python
# -*- coding: utf-8 -*-
#- Nitrofurano - 0806241812 - GPL2 licence - (thanks DanielPC help)
#- zx spectrum picture converter - sdlbasic version 0708072157 - from firstbasic version: 0201310133

import operator
from gimpfu import *

def python_filter(img,layer):
  region=layer.get_pixel_rgn(0,0,img.width,img.height,True,False);
  xed=img.width;yed=img.height

  clust=[[0]*8]*8
  r0=[[0]*8]*8
  g0=[[0]*8]*8
  b0=[[0]*8]*8

  plte=[0x000000]*16
  plte[ 0]=0x000000;plte[ 2]=0x0000B4;plte[ 1]=0xB40000;plte[ 3]=0xB400B4
  plte[ 4]=0x00B400;plte[ 6]=0x00B4B4;plte[ 5]=0xB4B400;plte[ 7]=0xB4B4B4
  plte[ 8]=0x000000;plte[10]=0x0000FF;plte[ 9]=0xFF0000;plte[11]=0xFF00FF
  plte[12]=0x00FF00;plte[14]=0x00FFFF;plte[13]=0xFFFF00;plte[15]=0xFFFFFF

  #'- "00,12,03,15,08,04,11,07,02,14,01,13,10,06,09,05."
  #'- "00,06,08,14,02,12,04,10,08,14,00,06,04,10,02,12."
  clust=[[0,6,8,14],[2,12,4,10],[8,14,0,6],[4,10,2,12]]
  hbedge=180 #- halfbright attr edge -zx32=218, zx32fs=153 , realthing=180?
  rgrsp=30;ggrsp=30;bgrsp=30 #- for r, g and b saturation levels

  for y1 in range(0,int(img.height/8),1):
   gimp.progress_update(operator.truediv(y1,int(img.height/8)))
   # print y1
   for x1 in range(0,int(img.width/8),1):
    for y2 in range(0,8,1):
      for x2 in range(0,8,1):
        y=y1*8+y2;x=x1*8+x2
        # zzz=point(x,y)
        str1=region[x,y]
        b0[x2][y2]=ord(str1[0])
        g0[x2][y2]=ord(str1[1])
        r0[x2][y2]=ord(str1[2])
    bi=0;ri=0;gi=0
    for y2 in range(0,8,1):
      for x2 in range(0,8,1):
        x=(x1*8)+x2;y=(y1*8)+y2
        bi=bi+b0[x2][y2];gi=gi+g0[x2][y2];ri=ri+r0[x2][y2]
        # next:next

    #b=bi/64
    #g=gi/64
    #r=ri/64
    b=operator.truediv(bi,64)
    g=operator.truediv(gi,64)
    r=operator.truediv(ri,64)

    xrreg=0
    hbrite=0
    if (r<hbedge) and(g<hbedge) and(b<hbedge):
      hbrite=1
    hbampl=255-(hbrite*(255-hbedge))

    # if b>(hbampl/2):
    if b>operator.truediv(hbampl,2):
      b=(hbampl-b)
      xrreg=xrreg|1

    # if r>(hbampl/2):
    if r>operator.truediv(hbampl,2):
      r=(hbampl-r)
      xrreg=xrreg|2

    # if g>(hbampl/2):
    if g>operator.truediv(hbampl,2):
      g=(hbampl-g)
      xrreg=xrreg|4

    # halbr=(r*rgrsp)/100
    # halbg=(g*ggrsp)/100
    # halbb=(b*bgrsp)/100
    halbr=operator.truediv((r*rgrsp),100)
    halbg=operator.truediv((g*ggrsp),100)
    halbb=operator.truediv((b*bgrsp),100)

    vlik=7
    if((r>halbb) and(g<=halbb)) or((b<=halbr) and(g<=halbr)):
      vlik=3
    if((g>halbb) and(r<=halbb)) or((b<=halbg) and(r<=halbg)):
      vlik=5
    if((g>halbr) and(b<=halbr)) or((r<=halbg) and(b<=halbg)):
      vlik=6
    if((r<=halbb) and(g<=halbb)):
      vlik=1
    if((b<=halbr) and(g<=halbr)):
      vlik=2
    if((b<=halbg) and(r<=halbg)):
      vlik=4

    brattr=1-hbrite
    ikattr=(vlik^xrreg) #- ^ is used for xor? very weird.... - from ansi-basic, ^ is ** ?
    paattr=xrreg

    if ikattr<paattr:
      tmpr=ikattr
      ikattr=paattr
      paattr=tmpr

    ikval=ikattr+((ikattr&6)/2)
    paval=paattr+((paattr&6)/2)

    # lumik=(ikval*255)/10
    lumik=operator.truediv((ikval*255),10)

    # lumpa=(paval*255)/10
    lumpa=operator.truediv((paval*255),10)

    if brattr<1:
      # lumik=(lumik*hbedge)/255
      lumik=operator.truediv((lumik*hbedge),255)

      # lumpa=(lumpa*hbedge)/255
      lumpa=operator.truediv((lumpa*hbedge),255)

    dflum=lumik-lumpa
    for y2 in range (0,8,1):
      for x2 in range (0,8,1):
        y=y1*8+y2
        x=x1*8+x2
        b=b0[x2][y2]
        g=g0[x2][y2]
        r=r0[x2][y2]
        vlue=(float(b+(r*3)+(g*6))/10)
        patgf=(float((clust[x2&3][y2&3]+1)*255)/16)
        varnd=(float(patgf*dflum)/255)+lumpa
        ik=ikattr+(8*brattr)
        if varnd>vlue:
          ik=paattr+(8*brattr)

        str2=chr((plte[ik]&0xFF0000)/65536)+chr((plte[ik]&0x00FF00)/256)+chr(plte[ik]&0x0000FF)
        region[x,y]=str2

  layer.update(0,0,img.width,img.height)
register(
  "python_fu_zxspectrum",
  "zxspectrum - a very simple picture filter using PythonGimp",
  "","Nitrofurano","Nitrofurano","2008",
  "<Image>/Filters/_ZxSpectrum",
  # "RGB*, GRAY*",
  "RGB*",
  [],[],
  python_filter)
main()

#finp$="8x5_proteusmag_i03p11.png":fout$=finp$+"_.bmp"
#'- sdlbasic version 0708072157
#'- from firstbasic version: 0201310133
#'- conversor tem erros - verificar - ciano é visto como preto (?)
#'- please be sure the picture source is a .png

#- benchmark core2duo 1.8ghz 480x360= 1 min

