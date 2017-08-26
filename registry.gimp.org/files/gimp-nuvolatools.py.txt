#!/usr/bin/env python

import math
from gimpfu import *
import numpy as np
from scipy import ndimage
from scipy.misc.pilutil import imread
import mahotas
import pymorph
import pylab
import cv2, cv
import os
from skimage.morphology import medial_axis
from skimage.morphology import label
from skimage.morphology import skeletonize
from skimage.filter import threshold_otsu
import matplotlib.pyplot as plt
from skimage import filter
from skimage import io
import png
from Tkinter import *
import popen2
import os
import Image, tkSimpleDialog
import datetime
import time







############################# EXTRA PARAMETERS INPUT SECTION via Tkinter / tkSimpleDialog



def writedialogdatatoshm(dialogconfdata):

	print "data received",dialogconfdata
	stringconfdata = dialogconfdata[0]+","+dialogconfdata[1]+","+dialogconfdata[2]+","+dialogconfdata[3]+","+dialogconfdata[4]+","+dialogconfdata[5]+","+dialogconfdata[6]+","+dialogconfdata[7]+","+dialogconfdata[8]+","+dialogconfdata[9]+","+dialogconfdata[10]+","+dialogconfdata[11]+","+dialogconfdata[12]+","+dialogconfdata[13]+","+dialogconfdata[14]+","+dialogconfdata[15]
	print "writing data to nuvola.conf", stringconfdata
	a, b = popen2.popen2("cat > /dev/shm/nuvolatools.conf")
	b.write(stringconfdata)
	a.close()
	b.close()




def readdialogdatafromshm():

	a, b = popen2.popen2("cat /dev/shm/nuvolatools.conf")
	myconfdataraw = a.read()
	a.close()
	b.close()
	if (len(myconfdataraw.split(',')) < 16):
		wdialogconfdata = ("shapedetfillratio", "0.785398", "shapedetfillratio", "1", "shapedethwrange", "0", "shapedethwrange", "0","shapeselectex", "1","shapeinvertpic","0","radiusoperation","0","radiuspixel","0")
		writedialogdatatoshm(wdialogconfdata)
	
		a, b = popen2.popen2("cat /dev/shm/nuvolatools.conf")
		myconfdataraw = a.read()
		a.close()
		b.close()



	myconfdatasplit = myconfdataraw.split(',')
	return myconfdatasplit[1], myconfdatasplit[3], myconfdatasplit[5], myconfdatasplit[7],myconfdatasplit[9],myconfdatasplit[11],myconfdatasplit[13],myconfdatasplit[15]






class MyDialog(tkSimpleDialog.Dialog):

	def body(self, master):

		Label(master, text="Shape detector Fill Ratio (0~1):").grid(row=0, column=0, sticky=W)
	
		Label(master, text="Shape detector Height/Width Ratio:").grid(row=1, column=0, sticky=W)

		Label(master, text="Shape detector Fill Range (0~1):").grid(row=0, column=2, sticky=W)

                Label(master, text="Shape detector Height/Width Range:").grid(row=1, column=2, sticky=W)

		Label(master, text="Shape select(1)|exclude(0):").grid(row=2, column=0, sticky=W)
		Label(master, text="Shape invert_picture(1)|normal(0):").grid(row=2, column=2, sticky=W)

		Label(master, text="Nearest blob distance, no(0)|beyond(1)|within(2):").grid(row=3, column=0, sticky=W)
		Label(master, text="Distance from bounding box borders (px):").grid(row=3, column=2, sticky=W)

		self.e1 = Entry(master)
		self.e2 = Entry(master)
		self.e3 = Entry(master)
                self.e4 = Entry(master)
		self.e5 = Entry(master)
		self.e6 = Entry(master)
		self.e7 = Entry(master)
		self.e8 = Entry(master)

		self.e1.grid(row=0, column=1)
		self.e2.grid(row=1, column=1)
                self.e3.grid(row=0, column=3)
                self.e4.grid(row=1, column=3)
		self.e5.grid(row=2, column=1)
		self.e6.grid(row=2, column=3)
		self.e7.grid(row=3, column=1)
		self.e8.grid(row=3, column=3)

		mydialogdata = readdialogdatafromshm()
	
		self.e1.insert(INSERT, mydialogdata[0])			
		self.e2.insert(INSERT, mydialogdata[1])
		self.e3.insert(INSERT, mydialogdata[2])
		self.e4.insert(INSERT, mydialogdata[3])
		self.e5.insert(INSERT, mydialogdata[4])
		self.e6.insert(INSERT, mydialogdata[5])
		self.e7.insert(INSERT, mydialogdata[6])
		self.e8.insert(INSERT, mydialogdata[7])

		return self.e1 # initial focus







	def apply(self):
		shapedetfillratio = float(self.e1.get())
		shapedetfillrange = float(self.e2.get())
		shapedethwratio = float(self.e3.get())
		shapedethwrange = float(self.e4.get())
		shapeselectex = float(self.e5.get())
		shapeinvertpic = float(self.e6.get())
		radiusoperation = float(self.e7.get())
		radiuspixel = float(self.e8.get())

		print shapedetfillratio, shapedetfillrange, shapedethwratio, shapedethwrange, shapeselectex, shapeinvertpic
		self.result = shapedetfillratio, shapedetfillrange, shapedethwratio, shapedethwrange, shapeselectex, shapeinvertpic, radiusoperation, radiuspixel

		wdialogconfdata = ("shapedetfillratio", self.e1.get(), "shapedetfillrange", self.e2.get(), "shapedethwratio", self.e3.get(), "shapedethwrange", self.e4.get(),"shapeselectex",self.e5.get(),"shapeinvertpic",self.e6.get(),"radiusoperation",self.e7.get(),"radiuspixel",self.e8.get())
		writedialogdatatoshm(wdialogconfdata)





############################# END OF EXTRA PARAMETERS INPUT SECTION <<<<<<<<<<<<<<<<<<<<<










def imagepreprocesslabel(pplimg, pplmangaoptype, pplotsuon, pplsamplethreshold, pplshapeinvertpic):

	#### if whitepores = True, invert the picture
	if ((pplmangaoptype==1) or (pplshapeinvertpic)):
		imgneg = pymorph.neg(pplimg)
		pplimg = imgneg
	
	####conversion to bin via Otsu
	if (pplotsuon == 1):	
		thresh = threshold_otsu(pplimg)
		imgotsu = pplimg > thresh
		imgtosaveotsu = Image.fromarray(np.uint8(imgotsu*255))

	####conversion to bin via threshad
	if (pplotsuon == 0):
		imgneg = pymorph.neg(pplimg)
		imgbinneg = pymorph.threshad(imgneg, pplsamplethreshold, 255)
		imgbin = pymorph.neg(imgbinneg)
		imgotsu = imgbin
		imgtosaveotsu = Image.fromarray(np.uint8(imgotsu*255))
		imgotsuneg = imgbinneg
		


	####labeling the Otsu output via mahotas or threshad
	if (pplotsuon == 1):
		imgneg = pymorph.neg(pplimg)
		imgotsuneg = pymorph.neg(imgotsu)
		imglabel, labelnum = mahotas.label(imgotsuneg)
		imgbin = imgotsuneg 
		imgbinneg = imgotsu

	if (pplotsuon == 0):
		imglabel, labelnum = mahotas.label(imgbinneg)
		

	#print imglabel[974], labelnum
	# print other lines for debugging purposes

	return imglabel, labelnum, imgbin, imgbinneg, pplimg, imgneg, imgotsu, imgtosaveotsu, imgotsuneg









def makenparrayfromfile(fimgfilename, wsselectionBox, uniqueExtension):

	fimgpilopen = Image.open(fimgfilename)
	fimgcropped = fimgpilopen.crop(wsselectionBox)
	fimgcropped.save('/dev/shm/'+uniqueExtension+'nuvolatools-proctemppngtimgfilecrop.png','PNG')

	fimggray = imread('/dev/shm/'+uniqueExtension+'nuvolatools-proctemppngtimgfilecrop.png', flatten = True)
	#print dir(imggray)
    
	print "flattening pil image"

	pdb.gimp_progress_set_text('flattening pil image')
	fbimggray = np.uint8(fimggray)
	print "flattened"
	fimg = np.asarray(fbimggray)
	print "array created"
	return fimg









def measurefromlabel(mimglabel, tdrawable, mfancyproc):
	mimgblobdata = []
	endY = len(mimglabel)
        endX = len(mimglabel[0])
        indexY = 0
        indexX = 0
	msgCounter = 0
	while (indexY < endY):
                #print "processing line", indexY, "/", endY
		msgCounter = msgCounter + 1
		if (msgCounter == 50):
			msgCounter = 0
			pdb.gimp_progress_set_text("measuring speckles on line " + str(indexY) + " / " + str(endY))
                indexX = 0
		while (indexX < endX):
			pixelval = mimglabel[indexY][indexX]
			if (pixelval != 0):
				while (len(mimgblobdata) < pixelval):
					blobvals=[0,0,0,0,0,0,0,0]
					mimgblobdata.append([0,0,0,0,0,0,0,0])
				mimgblobdata[pixelval-1][0] = mimgblobdata[pixelval-1][0] + 1
				if (mfancyproc):
					if (indexY > mimgblobdata[pixelval-1][1]):
						mimgblobdata[pixelval-1][1] = indexY
					if (mimgblobdata[pixelval-1][2] == 0):
						mimgblobdata[pixelval-1][2] = indexY
					if (indexY < mimgblobdata[pixelval-1][2]):
						mimgblobdata[pixelval-1][2] = indexY
					if (indexX > mimgblobdata[pixelval-1][3]):
						mimgblobdata[pixelval-1][3] = indexX
					if (mimgblobdata[pixelval-1][4] == 0):
						mimgblobdata[pixelval-1][4] = indexX
					if (indexX < mimgblobdata[pixelval-1][4]):
						mimgblobdata[pixelval-1][4] = indexX
			#print "pixel val", pixelval, "X pixel", indexX
				
				
			indexX = indexX + 1
	
		indexY = indexY + 1
	return mimgblobdata



    



def makefromlabel(wimgbinneg, wimgspeckles, wimglabel, wimgblobdata, wminblobarea, wmaxblobarea, wfancyproc, wshapedetfillratio, wshapedetfillrange, wshapedethwratio, wshapedethwrange, wshapeselectex, wradiusoperation, wradiuspixel, wmangaoptype, uniqueExtension):
	endY = len(wimglabel)
	endX = len(wimglabel[0])
	indexY = 0
	indexX = 0
	indexfoundblobs = 0
	foundblobs = []
	msgCounter = 0
	endBlobdata = len(wimgblobdata)
	indexBlobdata = 0
	wshapedetfillmax = wshapedetfillratio + wshapedetfillrange/2.0
	wshapedetfillmin = wshapedetfillratio - wshapedetfillrange/2.0
	wshapedethwmax = wshapedethwratio + wshapedethwrange/2.0
	wshapedethwmin = wshapedethwratio - wshapedethwrange/2.0
	wradiuspixelindex = 0
	wimgspecklestemp = wimgspeckles # for debugging purposes
	
	
	print "shape match parameters:", wshapedetfillratio, wshapedetfillrange/2.0, wshapedethwratio, wshapedethwrange/2.0
	print "processing label for blobs"
	#pdb.gimp_message('Computing speckles removal... please wait')
	while (indexBlobdata < endBlobdata):
		#print "Blob data", wimgblobdata[indexBlobdata], "index", indexBlobdata, "limits", wmaxblobarea, wminblobarea
		if ((wimgblobdata[indexBlobdata][0] <= wmaxblobarea) and (wimgblobdata[indexBlobdata][0] >= wminblobarea)):
			print "found candidate blob #", indexBlobdata, "area", wimgblobdata[indexBlobdata][0]
			foundblobs.append(indexBlobdata)
			indexfoundblobs = indexfoundblobs + 1

		#### preparing data for the radius operation (shape matching operations)
		if (wradiusoperation > 0):
			print "coordinates and data", wimgblobdata[indexBlobdata]
			pointa = wimgblobdata[indexBlobdata][1], wimgblobdata[indexBlobdata][3]
			pointb = wimgblobdata[indexBlobdata][1], wimgblobdata[indexBlobdata][4]+(wimgblobdata[indexBlobdata][3]-wimgblobdata[indexBlobdata][4])/2
			pointc = wimgblobdata[indexBlobdata][1], wimgblobdata[indexBlobdata][4]
			pointd = wimgblobdata[indexBlobdata][2]+(wimgblobdata[indexBlobdata][1] - wimgblobdata[indexBlobdata][2])/2, wimgblobdata[indexBlobdata][4]
			pointe = wimgblobdata[indexBlobdata][2], wimgblobdata[indexBlobdata][4]
			pointf = wimgblobdata[indexBlobdata][2], wimgblobdata[indexBlobdata][4]+(wimgblobdata[indexBlobdata][3]-wimgblobdata[indexBlobdata][4])/2
			pointg = wimgblobdata[indexBlobdata][2], wimgblobdata[indexBlobdata][3]
			pointh = wimgblobdata[indexBlobdata][2]+(wimgblobdata[indexBlobdata][1] - wimgblobdata[indexBlobdata][2])/2, wimgblobdata[indexBlobdata][3]

			print "points: ", pointa, pointb, pointc, pointd, pointe, pointf, pointg, pointh

			wradiuspixelindex = 2
			while (wradiuspixelindex <= wradiuspixel):
				
				pointa = pointa[0] + 1, pointa[1] + 1
				pointb = pointb[0] + 1, pointb[1]
				pointc = pointc[0] + 1, pointc[1] - 1
				pointd = pointd[0], pointd[1] - 1
				pointe = pointe[0] - 1, pointe[1] - 1
				pointf = pointf[0] - 1, pointf[1]
				pointg = pointg[0] - 1, pointg[1] + 1
				pointh = pointh[0], pointh[1] + 1
				if (pointa[0]<=1):
					pointa = 1, pointa[1]
				if (pointa[0]>=endY-1):
					pointa = endY-2, pointa[1]
				if (pointb[0]<=1):
					pointb = 1, pointb[1]
				if (pointb[0]>=endY-1):
					pointb = endY-2, pointb[1]
				if (pointc[0]<=1):
					pointc = 1, pointc[1]
				if (pointc[0]>=endY-1):
					pointc = endY-2, pointc[1]
				if (pointd[0]<=1):
					pointd = 1, pointd[1]
				if (pointd[0]>=endY-1):
					pointd = endY-2, pointd[1]
				if (pointe[0]<=1):
					pointe = 1, pointe[1]
				if (pointe[0]>=endY-1):
					pointe = endY-2, pointe[1]
				if (pointf[0]<=1):
					pointf = 1, pointf[1]
				if (pointf[0]>=endY-1):
					pointf = endY-2, pointf[1]
				if (pointg[0]<=1):
					pointg = 1, pointg[1]
				if (pointg[0]>=endY-1):
					pointg = endY-2, pointg[1]
				if (pointh[0]<=1):
					pointh = 1, pointh[1]
				if (pointh[0]>=endY-1):
					pointh = endY-2, pointh[1]				


				if (pointa[1]<=1):
					pointa = pointa[0], 1
				if (pointa[1]>=endX-1):
					pointa = pointa[0], endX-2
				if (pointb[1]<=1):
					pointb = pointb[0], 1
				if (pointb[1]>=endX-1):
					pointb = pointb[0], endX-2
				if (pointc[1]<=1):
					pointc = pointc[0], 1
				if (pointc[1]>=endX-1):
					pointc = pointc[0], endX-2
				if (pointd[1]<=1):
					pointd = pointd[0], 1
				if (pointd[1]>=endX-1):
					pointd = pointd[0], endX-2
				if (pointe[1]<=1):
					pointe = pointe[0], 1
				if (pointe[1]>=endX-1):
					pointe = pointe[0], endX-2
				if (pointf[1]<=1):
					pointf = pointf[0], 1
				if (pointf[1]>=endX-1):
					pointf = pointf[0], endX-2
				if (pointg[1]<=1):
					pointg = pointg[0], 1
				if (pointg[1]>=endX-1):
					pointg = pointg[0], endX-2
				if (pointh[1]<=1):
					pointh = pointh[0], 1
				if (pointh[1]>=endX-1):
					pointh = pointh[0], endX-2			




				print "points new: ", pointa, pointb, pointc, pointd, pointe, pointf, pointg, pointh
				print "vals: ", wimgspeckles[pointa[0]][pointa[1]], wimgspeckles[pointb[0]][pointb[1]], wimgspeckles[pointc[0]][pointc[1]], wimgspeckles[pointd[0]][pointd[1]], wimgspeckles[pointe[0]][pointe[1]], wimgspeckles[pointf[0]][pointf[1]], wimgspeckles[pointg[0]][pointg[1]], wimgspeckles[pointh[0]][pointh[1]]

				if (wimgspeckles[pointa[0]][pointa[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointa[0]-1][pointa[1]-1] = 1
					wimgspecklestemp[pointa[0]+1][pointa[1]-1] = 1
					wimgspecklestemp[pointa[0]-1][pointa[1]+1] = 1
					wimgspecklestemp[pointa[0]+1][pointa[1]+1] = 1
					wimgspecklestemp[pointa[0]][pointa[1]] = 127
					break
				if (wimgspeckles[pointb[0]][pointb[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointb[0]-1][pointb[1]-1] = 1
					wimgspecklestemp[pointb[0]+1][pointb[1]-1] = 1
					wimgspecklestemp[pointb[0]-1][pointb[1]+1] = 1
					wimgspecklestemp[pointb[0]+1][pointb[1]+1] = 1
					wimgspecklestemp[pointb[0]][pointb[1]] = 127
					break
				if (wimgspeckles[pointc[0]][pointc[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointc[0]-1][pointc[1]-1] = 127
					wimgspecklestemp[pointc[0]+1][pointc[1]-1] = 127
					wimgspecklestemp[pointc[0]-1][pointc[1]+1] = 127
					wimgspecklestemp[pointc[0]+1][pointc[1]+1] = 127
					wimgspecklestemp[pointc[0]][pointc[1]] = 127
					break
				if (wimgspeckles[pointd[0]][pointd[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointd[0]-1][pointd[1]-1] = 127
					wimgspecklestemp[pointd[0]+1][pointd[1]-1] = 127
					wimgspecklestemp[pointd[0]-1][pointd[1]+1] = 127
					wimgspecklestemp[pointd[0]+1][pointd[1]+1] = 127
					wimgspecklestemp[pointd[0]][pointd[1]] = 127
					break
				if (wimgspeckles[pointe[0]][pointe[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointe[0]-1][pointe[1]-1] = 127
					wimgspecklestemp[pointe[0]+1][pointe[1]-1] = 127
					wimgspecklestemp[pointe[0]-1][pointe[1]+1] = 127
					wimgspecklestemp[pointe[0]+1][pointe[1]+1] = 127
					wimgspecklestemp[pointe[0]][pointe[1]] = 127
					break
				if (wimgspeckles[pointf[0]][pointf[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointf[0]-1][pointf[1]-1] = 127
					wimgspecklestemp[pointf[0]+1][pointf[1]-1] = 127
					wimgspecklestemp[pointf[0]-1][pointf[1]+1] = 127
					wimgspecklestemp[pointf[0]+1][pointf[1]+1] = 127
					wimgspecklestemp[pointf[0]][pointf[1]] = 127
					break
				if (wimgspeckles[pointg[0]][pointg[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointg[0]-1][pointg[1]-1] = 127
					wimgspecklestemp[pointg[0]+1][pointg[1]-1] = 127
					wimgspecklestemp[pointg[0]-1][pointg[1]+1] = 127
					wimgspecklestemp[pointg[0]+1][pointg[1]+1] = 127
					wimgspecklestemp[pointg[0]][pointg[1]] = 127
					break
				if (wimgspeckles[pointh[0]][pointh[1]] == 0):
					wimgblobdata[indexBlobdata][5] = wradiuspixelindex
					print "registering distance", wradiuspixelindex
					wimgspecklestemp[pointh[0]-1][pointh[1]-1] = 127
					wimgspecklestemp[pointh[0]+1][pointh[1]-1] = 127
					wimgspecklestemp[pointh[0]-1][pointh[1]+1] = 127
					wimgspecklestemp[pointh[0]+1][pointh[1]+1] = 127
					wimgspecklestemp[pointh[0]][pointh[1]] = 127
					break
				wimgspecklestemp[pointa[0]][pointa[1]] = 1
				wimgspecklestemp[pointb[0]][pointb[1]] = 1
				wimgspecklestemp[pointc[0]][pointc[1]] = 1
				wimgspecklestemp[pointd[0]][pointd[1]] = 1
				wimgspecklestemp[pointe[0]][pointe[1]] = 1
				wimgspecklestemp[pointf[0]][pointf[1]] = 1
				wimgspecklestemp[pointg[0]][pointg[1]] = 1
				wimgspecklestemp[pointh[0]][pointh[1]] = 1


				wradiuspixelindex = wradiuspixelindex + 1

		print "preprocessing blob distance data", wimgblobdata[indexBlobdata]
		indexBlobdata = indexBlobdata + 1	

	io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-pwimgspeckles.png', wimgspecklestemp)

	print "found blobs", foundblobs
	foundblobslen = len(foundblobs)
	while (indexY < endY):
		#print "processing line", indexY
		indexX = 0
		while (indexX < endX):
			#print indexY, endY, indexX, endX
			pixelval = wimglabel[indexY][indexX]
			wimgspeckles[indexY][indexX] = 0
			if (pixelval != 0):
				#print "pixel values", pixelval
				#print "pixel val", pixelval, "blob data len", len(wimgblobdata)
				if ((wimgblobdata[pixelval-1][0] < int(wmaxblobarea)) and (wimgblobdata[pixelval-1][0] > int(wminblobarea))):
					wimgspeckles[indexY][indexX] = 255
					if (wfancyproc) and (wmangaoptype == 2):
						### this section is a *simple* shape processor
						### the fill ratio for a circle is PI/4, use that to find precise circular shapes *and* anything else with this fill ratio..
						#note we add 1 to height and width because the area in our case includes the perimeter! It's not simply a subtraction!
						if (wshapeselectex):
							wimgspeckles[indexY][indexX] = 0 #first we clear the pixel if we are in selection mode else leave it set

						blobheight = 1+wimgblobdata[pixelval-1][1] - wimgblobdata[pixelval-1][2]
						blobwidth = 1+wimgblobdata[pixelval-1][3] - wimgblobdata[pixelval-1][4]
						blobsquarearea = blobheight * blobwidth
						blobfillratio = float(wimgblobdata[pixelval-1][0]) / float(blobsquarearea)
						blobsquarenessratio = float(blobheight)/float(blobwidth)
						### matching the shape
						if ((blobfillratio <= wshapedetfillmax) and (blobfillratio >= wshapedetfillmin)):
							print "MATCHED FILL RATIO", wshapedetfillmax, blobfillratio, wshapedetfillmin
							if ((blobsquarenessratio <= wshapedethwmax) and (blobsquarenessratio >= wshapedethwmin)):
								print "MATCHED H/W RATIO", wshapedethwmax, blobsquarenessratio, wshapedethwmin
								### on match set the pixel anyways
								wimgspeckles[indexY][indexX] = 255
								### if we are in exclusion mode the pixel is cleared, else we leave it set
								if (not wshapeselectex): 
									wimgspeckles[indexY][indexX] = 0
								print "SHAPE MATCHED! ^_^"
								if (wradiusoperation > 0):
									wimgspeckles[indexY][indexX] = 0
	
									if ((wradiusoperation == 1) and (wimgblobdata[pixelval-1][5] == 0)):
										### on match set the pixel anyways
										wimgspeckles[indexY][indexX] = 255
										print "MATCHED BEYOND RADIUS", wradiuspixel
									if ((wradiusoperation == 2) and (wimgblobdata[pixelval-1][5] > 0)):
										### on match set the pixel anyways
										wimgspeckles[indexY][indexX] = 255
										print "MATCHED WITHIN RADIUS",  wimgblobdata[pixelval-1][5], "/", wradiuspixel


						print "blob square area", blobsquarearea, "blob fill ratio", blobfillratio, "blobsquarenessratio", blobsquarenessratio, "blob area", wimgblobdata[pixelval-1][0]
						### actual processing begins here



					#print "found speck val", pixelval, "/", wimgblobdata[pixelval-1][0]
					msgCounter = msgCounter + 1
					if (msgCounter == 1000):
						msgCounter = 0
						pdb.gimp_progress_set_text('found speckle number: '+str(pixelval) + " / " + str(foundblobs[foundblobslen-1])+ " (estimated!)")

			#print indexX, indexY, pixelval, endX, endY
			indexX = indexX + 1
		indexY = indexY + 1
	foundblobs = 0
	return wimgspeckles









######################################################################################################
### end subfunctions
######################################################################################################


def python_nuvolatools(timg, tdrawable, mangaoptype=0, lowlevelnumber=0, highlevelnumber=255,
                     samplethreshold=90, minblobarea=1, maxblobarea=10, otsuon=0, fancyproc=0, cbrregistry=0, numberOfDilations=1):


####CBR Registry section
###note CBR registry is meant to become a very compact way of specifying multiple values (i.e. for multiple operations, even as binary values) and should be processed in this section to initialize the necessary variables. This has priority over everything else passed as arguments to the present function



####variables init

    width = tdrawable.width
    height = tdrawable.height
    swidth = tdrawable.mask_bounds[2] - tdrawable.mask_bounds[0]
    sheight = tdrawable.mask_bounds[3] - tdrawable.mask_bounds[1]
    soffsetX = tdrawable.mask_bounds[0]
    soffsetY = tdrawable.mask_bounds[1]
    sselectionBox = tdrawable.mask_bounds


    shapedetfillratio = 0.0
    shapedetfillrange = 0.0
    shapedethwratio = 0.0
    shapedethwrange = 0.0
    shapeselectex = 1
    shapeinvertpic = 0
    radiusoperation = 0
    radiuspixel = 1
    uniqueExtension = ""
    if ((cbrregistry & 16)== 16):
    	dateNow = datetime.datetime.now()
    	uniqueExtension = str(time.mktime(dateNow.timetuple()))


    if ((mangaoptype==2) or (mangaoptype==3) or (mangaoptype==4)):

    	if ((cbrregistry & 4)== 4):
    		extradata = readdialogdatafromshm()		

    	root = Tk()
    	if ((cbrregistry & 4)!= 4):
    		d = MyDialog(root)
	    	extradata = d.result
    		print "Extra parameters input result:", extradata

	print "EXTRA PARAMETERS RAW", extradata
    	shapedetfillratio = float(extradata[0])
    	shapedethwratio = float(extradata[1])
    	shapedetfillrange = float(extradata[2])
    	shapedethwrange = float(extradata[3])
    	shapeselectex = int(extradata[4])
    	shapeinvertpic = int(extradata[5])
   	radiusoperation = int(extradata[6])
   	radiuspixel = int(extradata[7])
    	#basically, invertpic means setting the whitepores operation... so:
    	# (shapeinvertpic) complements the whitepores condition ~_~
    		


    scropBox = (swidth,sheight,soffsetX,soffsetY)

####creating work image (currently the imgg is NOT USED)

    imgg = gimp.Image(width, height, GRAY)
    imgg.disable_undo()
    layer_imgg = gimp.Layer(imgg, "work layer", width, height, GRAY_IMAGE, 100, NORMAL_MODE)
    layer_imgg.add_alpha()
    imgg.add_layer(layer_imgg, 0)
    pdb.gimp_image_crop(imgg,swidth,sheight,soffsetX,soffsetY)
    ##pdb.gimp_edit_copy(tdrawable)
    ##imggpastelayer = pdb.gimp_edit_paste(layer_imgg, True)
    ##imgg.add_layer(imggpastelayer, 1)
    ##imggreturnlayer = pdb.gimp_image_flatten(imgg)

    wdrawable = tdrawable.copy()
    if (fancyproc):
    	timg.add_layer(wdrawable, 0)
    	wdrawable.add_alpha()
    	pdb.gimp_layer_set_name(wdrawable,"nuvola work layer")

#### FANCY OPERATIONS (work layer)

    if (fancyproc):
	### this is a preset known to work well for me ~_~ ...doing two passes! (I should make this optonal tho)
	## excluding the gaussian blur, useless for now.
	#pdb.plug_in_gauss_rle(timg, wdrawable, maxblurrad***, 1, 1)  #mablurrad will give error since this variable has been removed
    	pdb.gimp_levels(wdrawable, 0, 0, highlevelnumber, 1, 0, 255) #first let's minimize the background
    	pdb.plug_in_unsharp_mask(timg, wdrawable, 5, 0.5, 0) # only then, more crispness
    	#pdb.plug_in_unsharp_mask(timg, wdrawable, 5, 0.5, 0) ###maybe this is excessive, let's comment it
    	pdb.gimp_levels(wdrawable, 0, lowlevelnumber, 255, 1, 0, 255) #then, further darken the blacks
    	pdb.plug_in_antialias(timg, wdrawable) # a touch of antialias~





####conversion to numpy array
    #pdb.file_png_save(timg, tdrawable, "/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.png", "pngtimgfile.png", 0, 9, 1, 1, 1, 1, 1)


    #img = makenparrayfromfile("/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.png", sselectionBox, uniqueExtension)
 
    #pdb.file_gif_save(timg, tdrawable, "/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.gif", "pngtimgfile.gif", 0, 0, 0, 0)


    #img = makenparrayfromfile("/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.gif", sselectionBox, uniqueExtension)


    pdb.file_tiff_save(timg, tdrawable, "/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.tif", "pngtimgfile.tif", 1)

    print "calling nparrayfrom pngtimgfile"
    img = makenparrayfromfile("/dev/shm/"+uniqueExtension+"nuvolatools-pngtimgfile.tif", sselectionBox, uniqueExtension)


    #pdb.file_png_save(timg, wdrawable, "/dev/shm/"+uniqueExtension+"nuvolatools-wpngtimgfile.png", "wpngtimgfile.png", 0, 9, 1, 1, 1, 1, 1)


    #wimg = makenparrayfromfile("/dev/shm/"+uniqueExtension+"nuvolatools-wpngtimgfile.png", sselectionBox, uniqueExtension) <-- only if fancyproc
    if (fancyproc):
    	print "calling nparrayfrom wpngtimgfile"
    	pdb.file_tiff_save(timg, wdrawable, "/dev/shm/"+uniqueExtension+"nuvolatools-wpngtimgfile.tif", "wpngtimgfile.tif",1)
    	wimg = makenparrayfromfile("/dev/shm/"+uniqueExtension+"nuvolatools-wpngtimgfile.tif", sselectionBox, uniqueExtension)

#### thresholding and labeling


    imglabel, labelnum, imgbin, imgbinneg, img, imgneg, imgotsu, imgtosaveotsu, imgotsuneg = imagepreprocesslabel(img, mangaoptype, otsuon, samplethreshold, shapeinvertpic)

    if (fancyproc):
    	wimglabel, wlabelnum, wimgbin, wimgbinneg, wimg, wimgneg, wimgotsu, wimgtosaveotsu, wimgotsuneg = imagepreprocesslabel(wimg, mangaoptype, otsuon, samplethreshold, shapeinvertpic)


#### special for the shapedetector: we are actually using the work layer since it should give a much better shape separation 

    if ((mangaoptype==2) and (fancyproc)):
    	imglabel = wimglabel
    	labelnum = wlabelnum
    	imgbin = wimgbin
    	imgbinneg = wimgbinneg
    	img = wimg
    	imgneg = wimgneg
    	imgotsu = wimgotsu
    	imgtosaveotsu = wimgtosaveotsu
    	imgotsuneg = wimgotsuneg






####blob measurement via pymorph, parameter maxblobarea

    pdb.gimp_progress_set_text('Measuring specks sizes.. (may take some time)')
    
    imgblobdata = measurefromlabel(imglabel, tdrawable, fancyproc)
    print imgblobdata, len(imgblobdata)
    #len of data == number of labels, of course.

#### speckles operation start
    if ((mangaoptype==0) or (mangaoptype==1)):
	#print "making measurement from labels"
    	imgspeckles = img; #create a copy of the original nparray
	imgspecklesres = makefromlabel(imgbinneg, imgspeckles, imglabel, imgblobdata,minblobarea,maxblobarea,fancyproc, shapedetfillratio, shapedetfillrange, shapedethwratio, shapedethwrange, shapeselectex, radiusoperation, radiuspixel, mangaoptype, uniqueExtension)
    if (mangaoptype==2):
	imgspeckles = img; #create a copy of the original nparray
	### for now, fancyproc is needed true if we want to match specific shapes
    	fancyproc = True
	print "pre-call shape det parameters", shapedetfillratio, shapedetfillrange, shapedethwratio, shapedethwrange, shapeselectex, radiusoperation, radiuspixel
    	imgspecklesres = makefromlabel(imgbinneg, imgspeckles, imglabel, imgblobdata,minblobarea,maxblobarea,fancyproc, shapedetfillratio, shapedetfillrange, shapedethwratio, shapedethwrange, shapeselectex, radiusoperation, radiuspixel, mangaoptype, uniqueExtension)


#### Fancy operation: dilation 

    #if (mangaoptype==1):
    #	propagate_mode=1
    print "processing dilations"
    for i in range(numberOfDilations):
    	imgspecklesresdil = mahotas.dilate(imgspecklesres)
    	imgspecklesres = imgspecklesresdil



#### Fancy operation: thinning skeleton from mahotas when processing lineart

    print "thinning skeleton, this may take some minutes for very large pictures!"

    if ((fancyproc) and (mangaoptype == 3)):
    #if ((mangaoptype == 0)):
	minsegment=256
	minoverlap=64
    	#if (not fancyproc): # wimgbinneg is not declared if fancyproc is false, so for debugging purposes we assign it
	#	wimgbinneg = imgbinneg #preliminary results show a *less crisp* starting picture is MUCH desirable ~_~
	
	
    	wbinneglen=len(wimgbinneg)
    	wbinneglenintparts=wbinneglen/minsegment
	wbinneglenintrem=wbinneglen-wbinneglenintparts*minsegment
	wbinnegindex=0

    	#wimgbinneg=mahotas.erode(wimgbinneg)  ###this has a very positive effect of stabilizing certain aspects of the picture, however it's also altering it greatly. Commented for the time being.
    	#wimgbinneg=mahotas.erode(wimgbinneg)


    	print "processing segment at", wbinnegindex, "/",wbinneglen
    	imgthin=mahotas.thin(wimgbinneg[wbinnegindex:wbinnegindex+minsegment])
    	wbinnegindex=wbinnegindex+minsegment
	
	while (wbinnegindex<=wbinneglen):
    		pdb.gimp_progress_set_text("processing segment at "+str(wbinnegindex)+"/"+str(wbinneglen))
		wbinnegindex=wbinnegindex-minoverlap ###makes it 25% more computationally expensive, but pays with no artifacts
    		if (wbinnegindex+minsegment <= wbinneglen):
       			imgthinpart=mahotas.thin(wimgbinneg[wbinnegindex:wbinnegindex+minsegment])
    		if (wbinnegindex+minsegment > wbinneglen):
       			imgthinpart=mahotas.thin(wimgbinneg[wbinnegindex:])
    		#print imgthin[0]
    		#print imgthinpart[0]
    		imgthin=np.append(imgthin[0:wbinnegindex+minoverlap/2],imgthinpart[minoverlap/2:], axis=0)
    		wbinnegindex=wbinnegindex+minsegment


    	print "skeleton thinned"

    	io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgthin.png', imgthin*255)

	### a word of wisdom: this damn fails with "memory error" for VERY large pictures ~_~

    	###skel, distance = medial_axis(wimgbinneg, return_distance=True)

    	###print "medialaxis done"

    	###skeldist=skel*distance

    	###io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-medialaxis2.png', skeldist)




#### if whitepores = True or (shapeinvertpic), invert the picture
    if ((mangaoptype==1) or (shapeinvertpic)):
        imgnegres = pymorph.neg(imgspecklesres)
        imgspecklesres = imgnegres


####saving numpy intermediates
    #io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgtosaveotsu.png', imgtosaveotsu)
    #io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgbinneg.png', imgbinneg*255)
    io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgbin.png', imgbin*255)
    if (otsuon == 1):
    	io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgotsuneg.png',imgotsuneg*255)

    io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imglabel.png',imglabel)
    io.imsave('/dev/shm/'+uniqueExtension+'nuvolatools-imgspecklesres.png',imgspecklesres)


####creating spare GIMP layers
    layer_specks = gimp.Layer(timg, "negative specks", swidth, sheight, GRAY_IMAGE, 100, NORMAL_MODE)


####loading specks as layer
    layer_specks = pdb.gimp_file_load_layer(timg, '/dev/shm/'+uniqueExtension+'nuvolatools-imgspecklesres.png')
    #pdb.gimp_layer_add_alpha(layer_specks)
    timg.add_layer(layer_specks, 0)
    layer_specks.add_alpha()
 
    pdb.gimp_layer_set_name(layer_specks,"speckles remover")
    #### if whitepores = True or (shapeinvertpic), use the correct layer name
    if ((mangaoptype==1) or (shapeinvertpic)):
    	pdb.gimp_layer_set_name(layer_specks,"white pores filler")

    print tdrawable.mask_bounds, tdrawable.mask_bounds[2]-tdrawable.mask_bounds[0], tdrawable.mask_bounds[3]-tdrawable.mask_bounds[1], tdrawable.mask_bounds[0], tdrawable.mask_bounds[1]



#### removing background and publishing

    #layer_specks.resize(tdrawable.mask_bounds[2]-tdrawable.mask_bounds[0], tdrawable.mask_bounds[3]-tdrawable.mask_bounds[1], tdrawable.mask_bounds[0], tdrawable.mask_bounds[1])

    pdb.gimp_by_color_select(layer_specks, gimpcolor.RGB(0,0,0), 0, CHANNEL_OP_REPLACE, False, False, 0, False)

#### if whitepores = True, invert the selection
    if ((mangaoptype==1) or (shapeinvertpic)):
        pdb.gimp_by_color_select(layer_specks, gimpcolor.RGB(255,255,255), 0, CHANNEL_OP_REPLACE, False, False, 0, False)



    pdb.gimp_edit_clear(layer_specks)
    pdb.gimp_selection_none(timg) 
    #print dir(layer_specks)
    #print dir(pdb)
    ##layer_two = layer_one.copy()
    ##layer_two.mode = MULTIPLY_MODE
    ##layer_two.name = "Y Dots"
    ##timg.add_layer(layer_two, 0)


    imgg.flatten()

    ##bump_layer = imgg.active_layer

    layer_specks.translate(soffsetX, soffsetY)
####sending back layers

    #layer_one.image = imgtosaveotsu
    #timg.add_layer(layer_one, 0)

    if ((cbrregistry & 1)== 1):
    	wdrawable.add_alpha()
	pdb.gimp_selection_all(timg)
	print "matched cbr registry 1 -> clearing wdrawable"
	#pdb.gimp_drawable_delete(wdrawable)
    	pdb.gimp_edit_clear(wdrawable)
    	pdb.gimp_selection_none(timg) 

    if ((cbrregistry & 2)== 2):
	print "saving current picture in xcf format"
	timgfilename=pdb.gimp_image_get_filename(timg)
    	timgname=pdb.gimp_image_get_name(timg)
	print timgname, timgfilename
    	
	timgfilenames=timgfilename+str(cbrregistry)+"op"+str(mangaoptype)+".xcf"
	timgnames=timgname+str(cbrregistry)+"op"+str(mangaoptype)+".xcf"
	print timgnames,timgfilenames
    	pdb.gimp_xcf_save(2,timg,wdrawable,timgfilenames,timgnames)
	#pdb.file_xjt_save(timg,wdrawable,timgfilenames,timgnames,1,0,1,0) 
    	#pdb.gimp_file_save(timg,wdrawable,timgfilenames,timgnames)
	 

#### Final


    if ((cbrregistry & 8)== 8):
    	os.system("rm -v /dev/shm/"+uniqueExtension+"nuvolatools-*")
    	os.system("rm -f /dev/shm/"+uniqueExtension+"nuvolatools-*")

    gimp.delete(imgg)


register(
        "python_fu_nuvolatools",
        "Nuvola tools: an advanced toolset designed to speed-clean grayscale and manga/comics scans. Visit http://nuvolagroup.blogspot.com for help and tutorials and http://registry.gimp.org for updates.",
        "Nuvola Tools",
        "Nuvola Group",
        "Nuvola Group",
        "2012",
        "<Image>/Filters/Artistic/_Nuvola Tools...",
        "GRAY*",
        [
#		(PF_IMAGE, "timage",       "Input image", None),
#               (PF_DRAWABLE, "tdrawable", "Input drawable", None),
		(PF_OPTION, "mangaoptype", "Operation Type", 0, ("speckles", "whitepores", "remove specific shapes", "lineart (WIP)", "topological decomposition (WIP)")),
		(PF_SLIDER, "lowlevelnumber", "Black level", 0, [0, 255, 1]),
		(PF_SLIDER, "highlevelnumber", "White level", 255, [0, 255, 1]),
		(PF_SLIDER, "samplethreshold", "BW  Threshold", 90, [0, 255, 1]),
		(PF_INT, "minblobarea", "Min Speckle/Pore Area (px)", 1),
		(PF_INT, "maxblobarea", "Max Speckle/Pore Area (px)", 10),
		(PF_BOOL, "otsuon", "Automatic BW Threshold", 0),
		(PF_BOOL, "fancyproc", "Fancy processing", 0),
		(PF_INT, "cbrregistry", "CBR (currently unused)", 0),
		(PF_INT, "numberOfDilations", "Number of dilations", 1)
        ],
        [],
        python_nuvolatools)

main()


##### CBR assigned bit values: 1-clear the work layer // 2-save the resulting image as XCF // 4-for mangaoptype == 2 use the preset extra parameters without asking // 8-delete current session files in shared memory, useful for parallel usage // 16-parallel usage flag, saves files with an (almost) unique name


