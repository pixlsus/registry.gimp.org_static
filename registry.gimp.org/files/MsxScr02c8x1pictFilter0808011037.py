#!/usr/bin/env python
# -*- coding: utf-8 -*-

# finp$="8x5_proteusmag_i03p11.png":fout$=finp$+"_.bmp"
#- 0108091823
#---- teste1_0105052418_fileopening_msx1_8x1_conv.txt
#---- 010808 - primeira tentativa no firstbasic-dos

import operator
from gimpfu import *
from math import sqrt

def python_filter(img,layer):
  region=layer.get_pixel_rgn(0,0,img.width,img.height,True,False);
  xed=img.width;yed=img.height

  #- reads png file
  #xed=0:yed=0:open finp$ for input as #1:for i=1 to 16:n=readbyte(1):next:for i=1 to 2:n=readbyte(1):next
  #xed=readbyte(1)*256:xed=xed+readbyte(1):for i=1 to 2:n=readbyte(1):next:yed=readbyte(1)*256:yed=yed+readbyte(1)
  #close #1:setdisplay(xed,yed,32,1):paper(8^8-1):ink(0):pen(0):cls:Loadimage(finp$,1):pasteicon(0,0,1)

  # dim npaletr[16],npaletg[16],npaletb[16],clust[8,8],grpltlev[16],realpal[16]
  # dim rfromat[16],gfromat[16],bfromat[16],r0[8,8],g0[8,8],b0[8,8],plte[16]

  npaletr=[0]*16
  npaletg=[0]*16
  npaletb=[0]*16
  grpltlev=[0]*16
  realpal=[0]*16
  rfromat=[0]*16
  gfromat=[0]*16
  bfromat=[0]*16
  plte=[0]*16
  clust=[[0]*8]*8
  r0=[[0]*8]*8
  g0=[[0]*8]*8
  b0=[[0]*8]*8

  # '- defines the cluster for halftone
  # dmst$="00,06,08,14,02,12,04,10,08,14,00,06,04,10,02,12."
  # for y2=0 to 3:for x2=0 to 3:clust[x2,y2]=val(mid$(dmst$,((y2*4+x2)*3)+1,2)):next:next

  clust=[[0,6,8,14],[2,12,4,10],[8,14,0,6],[4,10,2,12]]

  # '- colour palette
  # dmst$="0,1,5,1,7,2,6,1,7,2,5,6,6,6,7,":for i=0 to 14:rr=val(mid$(dmst$,(i*2)+1,1)):npaletr[i]=int((rr*255)/7):next
  # dmst$="0,1,1,4,1,3,2,6,3,6,5,7,6,6,7,":for i=0 to 14:rg=val(mid$(dmst$,(i*2)+1,1)):npaletg[i]=int((rg*255)/7):next
  # dmst$="0,7,1,1,1,7,5,1,3,7,5,6,2,8,7,":for i=0 to 14:rb=val(mid$(dmst$,(i*2)+1,1)):npaletb[i]=int((rb*255)/7):next

  npaletr=[0,1,5,1,7,2,6,1,7,2,5,6,6,6,7]
  npaletg=[0,1,1,4,1,3,2,6,3,6,5,7,6,6,7]
  npaletb=[0,7,1,1,1,7,5,1,3,7,5,6,2,8,7]
  for i in range (0,14+1,1):
    npaletr[i]=int(float(npaletr[i]*255)/7)
    npaletg[i]=int(float(npaletg[i]*255)/7)
    npaletb[i]=int(float(npaletb[i]*255)/7)

  #'- defines grayscale from the palette
  #'dmst$="0000,0237,0314,0395,0400,0448,0504,0564,0600,0701,0714,0765,0778,0825,1000."
  #'for i=0 to 14:grd=val(mid$(dmst$,(i*5)+1,4)):grpltlev[i]=((grd*255)/1000):next
  #'for i=0 to 14:print grpltlev[i]:next

  #'- não funciona?
  for i in range (0,14+1,1):
    grpltlev[i]=int(float((npaletr[i]*30)+(npaletg[i]*59)+(npaletb[i]*11))/100)

  #for i=0 to 14:print grpltlev[i]:next

  for i in range (0,14+1,1):
    plte[i]=npaletr[i]*65536+npaletg[i]*256+npaletb[i]

  xmax=xed;ymax=yed

  xmaxo=int(xmax/8);ymaxo=int(ymax/8)
  xmaxoo=xmaxo*8;ymaxoo=ymaxo*8
  xsm0=(xmaxoo&255);ysm0=(ymaxoo&255)

  for i in range (0,14+1,1):
    rfromat[i]=npaletr[i];gfromat[i]=npaletg[i];bfromat[i]=npaletb[i]

  for y1 in range (0,ymaxo,1):
    #- progressbar - setcaption(str$(int((y1*100)/ymaxo))+"%")
    gimp.progress_update(operator.truediv(y1,ymaxo))
    for x1 in range (0,xmaxo,1):
      for y2 in range(0,8,1):
       for x2 in range(0,8,1):
        y=y1*8+y2;x=x1*8+x2;iy=(ymax-1)-y;ympos=(((iy*xmax)+x)*3)+55

        # zzz=point(x,y)
        # b0[x2,y2]=bitwiseand(zzz,255):zzz=int(zzz/256)
        # g0[x2,y2]=bitwiseand(zzz,255):zzz=int(zzz/256)
        # r0[x2,y2]=bitwiseand(zzz,255)
        # next:next

        str1=region[x,y]
        b0[x2][y2]=ord(str1[0])
        g0[x2][y2]=ord(str1[1])
        r0[x2][y2]=ord(str1[2])

        #r0[x2][y2]=ord(str1[0])
        #g0[x2][y2]=ord(str1[1])
        #b0[x2][y2]=ord(str1[2])

      for y2 in range(0,8,1):
        y=y1*8+y2
        #'- atribute reading and mounting
        bi=0;ri=0;gi=0
        for x2 in range(0,8,1):
          x=(x1*8)+x2;iy=(ymax-1)-y;bi=bi+b0[x2][y2];gi=gi+g0[x2][y2];ri=ri+r0[x2][y2]
        b=bi/8;g=gi/8;r=ri/8;dbuf=1000;paattr=0;ikattr=14;pa=0;ik=14
        lumik=1;lumpa=0
        for pa in range(0,13+1,1):
          for ik in range(pa+1,14+1,1):
            graypa=grpltlev[pa];grayik=grpltlev[ik]
            #'- lumik=grayik:lumpa=graypa
            grayrgb=(float((b*11)+(r*30)+(g*59))/100)
            if grayrgb>graypa and grayrgb<grayik:
              ikincid=float((grayrgb-graypa)*255)/(grayik-graypa)
              rfikinc=float((rfromat[ik]*ikincid)+(rfromat[pa]*(255-ikincid)))/255
              gfikinc=float((gfromat[ik]*ikincid)+(gfromat[pa]*(255-ikincid)))/255
              bfikinc=float((bfromat[ik]*ikincid)+(bfromat[pa]*(255-ikincid)))/255
              rdist=abs(rfikinc-r)
              gdist=abs(gfikinc-g)
              bdist=abs(bfikinc-b)
              # rgbdist=sqrt((rdist^2)+(gdist^2)+(bdist^2))
              rgbdist=sqrt((rdist**2)+(gdist**2)+(bdist**2))
              if rgbdist<=dbuf:
                dbuf=rgbdist;paattr=pa;ikattr=ik;lumik=grayik;lumpa=graypa
                #end if
              #next:next

          #'- memory block 4 reading as grayscale for screen 2
          dflum=lumik-lumpa;pkvar=0
          for x2 in range(0,8,1):
            x=x1*8+x2;yi=(ymaxoo-1)-y;b=b0[x2][y2];g=g0[x2][y2];r=r0[x2][y2]
            vlue=(float((b*11)+(r*30)+(g*59))/100)
            patgf1=x2%4;patgf2=y%4;patgf=(float((clust[patgf1][patgf2]+1)*255)/16)
            varnd=(float(patgf*dflum)/255)+lumpa
            ik=ikattr
            if varnd>vlue:
              ik=paattr

            # ink(plte[ik]):dot(x,y)
            str2=chr(plte[ik]&0x0000FF)+chr((plte[ik]&0x00FF00)/256)+chr((plte[ik]&0xFF0000)/65536)
            region[x,y]=str2


            #next:next:next:next

  #grab (1,0,0,xed,yed):saveimage(fout$,1)
  #shell("convert "+fout$+" "+finp$+"_.png"):shell("rm "+fout$)

  layer.update(0,0,img.width,img.height)
register(
  "python_fu_msx1scr2",
  "msx1scr2 - a very simple picture filter using PythonGimp",
  "","Nitrofurano","Nitrofurano","2008",
  "<Image>/Filters/_msx1scr2",
  # "RGB*, GRAY*",
  "RGB*",
  [],[],
  python_filter)
main()

#- benchmark - core2duo 1.85ghz 144x128 = 1 minute

