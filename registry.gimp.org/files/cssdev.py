#!/usr/bin/env python
# -*- coding: utf-8 -*-
# TODO: nested layers für linked
# TODO: Layernamen kürzen
from gimpfu import *
import os, string, sys
import os.path

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

def python_cssdev(active_image, active_layer, 
	export_dir, export_hidden, export_layer, export_height, export_divs, 
	li_color, ho_color, div_color, default_font, default_size, center_content, 
	generate_image, image_format):
	
	#Fontface alternativ listen
	arial = [ "Arial","Helvetica","FreeSans","Nimbus Sans L","sans-serif" ]
	courier = [ "Courier New","Courier","FreeMono","Nimbus Mono L","monospace" ]
	georgia = [ "Georgia","Bitstream Charter","Century Schoolbook L","Times","serif" ]
	lucida = [ "Lucida Sans","Lucida Sans Unicode","Lucida Grande","Lucida","sans-serif" ]
	console = [ "Lucida Console","Monaco","DejaVu Sans Mono","Bitstream Vera Sans Mono","monospace" ]
	palatino = [ "Palatino","Palatino Linotype","Palladio","URW Palladio L","Book Antiqua","Times","serif" ]
	tahoma = [ "Tahoma","Geneva","DejaVu Sans Condensed","sans-serif" ]
	times = [ "Times New Roman","Times","Nimbus Roman No9 L","FreeSerif","serif" ]
	verdana  = [ "Verdana","Bitstream Vera Sans","DejaVu Sans","Geneva","sans-serif" ]
	ind_list = [""]
	ind_cnt = 0
	do_wrapper = 0
	do_position = "position:absolute; "
	do_layerbg = 1
	do_height = ""
	num_Layers = pdb.gimp_image_get_layers(active_image)[0]
	xpos = 30
	ypos = 30
	valid_layer = []
	layer_text = []
	fg_color = gimp.get_foreground() 
	bg_color = gimp.get_background()
	if os.sep == "\\":
		export_file = "\\gimp_cssdev.html"
	else:
		export_file = "/gimp_cssdev.html"
	
	# font-face bestimmen
	if default_font in times:
		this_font = "\"Times New Roman\",Times,\"Nimbus Roman No9 L\",\"FreeSerif\",serif"
	elif default_font in arial:
		this_font = "Arial,Helvetica,FreeSans,\"Nimbus Sans L\",sans-serif"
	elif default_font in courier:
		this_font = "\"Courier New\",Courier,FreeMono,\"Nimbus Mono L\",monospace"		
	elif default_font in georgia:
		this_font = "Georgia,\"Bitstream Charter\",\"Century Schoolbook L\",Times,serif"		
	elif default_font in lucida:
		this_font = "\"Lucida Sans\",\"Lucida Sans Unicode\",\"Lucida Grande\",Lucida,sans-serif"		
	elif default_font in console:
		this_font = "\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",\"Bitstream Vera Sans Mono\",monospace"		
	elif default_font in palatino:
		this_font = "Palatino,\"Palatino Linotype\",Palladio,\"URW Palladio L\",\"Book Antiqua\",Times,serif"			
	elif default_font in tahoma:
		this_font = "Tahoma,Geneva,\"DejaVu Sans Condensed\",sans-serif"	
	elif default_font in verdana:
		this_font = "Verdana,\"Bitstream Vera Sans\",\"DejaVu Sans\",Geneva,sans-serif"	
	else:
		this_font = default_font
	
	# pfad uns datei zusammenbauen
	this_filepath = export_dir + export_file
	
	#Einrückungsliste füllen
	for i in xrange(20):
		ind_list.append("%s\t" % ind_list[-1])
	#Center WrapperCSS
	if center_content and export_layer == "2":
		do_wrapper = 1
		do_position = ""
	#Bildeigenschaften
	this_title = pdb.gimp_image_get_name(active_image)
	this_wrapper = pdb.gimp_image_width(active_image)

	file = open(this_filepath, 'w')
	# Head
	file.write("%s<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" % ind_list[ind_cnt])
	file.write("%s<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n" % ind_list[ind_cnt])
	file.write("%s<head>\n" % ind_list[ind_cnt])
	ind_cnt += 1
	file.write("%s<title>%s</title>\n" % (ind_list[ind_cnt],this_title))
	file.write("%s<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta http-equiv=\"imagetoolbar\" content=\"no\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta name=\"robots\" content=\"INDEX,follow\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta name=\"Keywords\" content=\"\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta name=\"Description\" content=\"\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta name=\"revisit-after\" content=\"14 Days\" />\n" % ind_list[ind_cnt])
	file.write("%s<meta name=\"author\" content=\"CSSdev for GIMP, http://www.area42.de, http://my.opera.com/area42/blog/\" />\n" % ind_list[ind_cnt])
	# Styleblock
	file.write("%s<style type=\"text/css\" media=\"screen\">\n" % ind_list[ind_cnt])
	ind_cnt += 1
	# BodyCSS
	file.write("%shtml, body { background:rgb(%s,%s,%s); color:rgb(%s,%s,%s); font-family:%s; font-size:%ipx; }\n" % (ind_list[ind_cnt], bg_color[0],bg_color[1],bg_color[2], fg_color[0], fg_color[1], fg_color[2], this_font, default_size))
	file.write("%sa { color:rgb(%s,%s,%s) }\n" % (ind_list[ind_cnt], li_color[0],li_color[1],li_color[2]))
	file.write("%sa:hover { color:rgb(%s,%s,%s) }\n" % (ind_list[ind_cnt], ho_color[0],ho_color[1],ho_color[2]))
	file.write("%simg { border:none; }\n" % ind_list[ind_cnt])
	file.write("%sp { color:rgb(%s,%s,%s); font-family:%s; font-size:12px; }\n" % (ind_list[ind_cnt], fg_color[0], fg_color[1], fg_color[2], this_font))
	# WrapperCSS
	if do_wrapper:
		file.write("%s#wrapperCSSDEV { width:%spx; margin:0 auto; }\n" % (ind_list[ind_cnt], this_wrapper))
	#LayerCSS
	for this_layer in range(len(active_image.layers)):
		do_layerbg = 1
		layer_width = active_image.layers[this_layer].width
		layer_pos = active_image.layers[this_layer].offsets
		layer_name = active_image.layers[this_layer].name
		layer_opacity = active_image.layers[this_layer].opacity
		if export_height:
			do_height = "height:%spx; " % active_image.layers[this_layer].height
		try:
			layer_bg = active_image.layers[this_layer].get_pixel(xpos,ypos)
		except:
			do_layerbg = 0
		#create valid layer names
		layer_text.append(layer_name)
		if layer_name[0].isdigit():
			layer_name = "%s%s" % ("_",layer_name)
		for bad_char in [' ','/','\t','"','#',"'",',','.',':',';','[','\\',']','{','}','ä','Ä','ö','Ö','ü','Ü','ß','Ž','`','~']:
			layer_name = layer_name.replace(bad_char,'')
			layer_name = string.lower(layer_name)
		valid_layer.append(layer_name)
		num_Layers -= 1
		if export_hidden == "0" and not active_image.layers[this_layer].visible:
			continue
		file.write("%s#%s { left:%spx; top:%spx; width:%spx; %sz-index:%s; %s" % (ind_list[ind_cnt], layer_name, layer_pos[0], layer_pos[1], layer_width, do_height, num_Layers, do_position))
		if export_hidden == "4" and export_layer == "0" and not active_image.layers[this_layer].visible:
			file.write("visibility:hidden; ")
		elif export_hidden == "4" and export_layer == "2" and not active_image.layers[this_layer].visible:
			file.write("display:none; ")		
		if div_color and do_layerbg == 1:
			file.write("background:rgb(%s,%s,%s); " % (layer_bg[0], layer_bg[1], layer_bg[2]))
		if (layer_opacity != 100.0) and (layer_opacity > 0):
			layer_opacity = round(layer_opacity/100,3)
			ind_cnt += 1
			file.write("\n%sfilter:alpha(opacity=%f); -moz-opacity:%f; opacity:%f; khtml-opacity:%f; " % (ind_list[ind_cnt], layer_opacity*100, layer_opacity, layer_opacity, layer_opacity))
			ind_cnt -= 1
		file.write("}\n")
	ind_cnt -= 1
	file.write("%s</style>\n" % ind_list[ind_cnt])
	ind_cnt -= 1
	file.write("%s</head>\n\n" % ind_list[ind_cnt])	
	#XHTML
	file.write("%s<body>\n" % ind_list[ind_cnt])	
	ind_cnt += 1
	
	# WrapperDIV open
	if do_wrapper:	
		file.write("%s<div id=\"wrapperCSSDEV\">\n" % ind_list[ind_cnt])
		ind_cnt += 1

	# LayerDIV
	for this_layer in range(len(valid_layer)):
		file.write("%s<div id=\"%s\">\n" % (ind_list[ind_cnt], valid_layer[this_layer]))
		ind_cnt += 1
		file.write("%s%s\n" % (ind_list[ind_cnt], layer_text[this_layer]))
		ind_cnt -= 1
		file.write("%s</div>\n" % ind_list[ind_cnt])
				
	# WrapperDIV close
	if do_wrapper:	
		ind_cnt -= 1
		file.write("%s</div>\n" % ind_list[ind_cnt])	
		
	ind_cnt -= 1
	file.write("%s</body>\n\n" % ind_list[ind_cnt])
	file.write("%s</html>\n" % ind_list[ind_cnt])
	file.close()

register(
	"python-fu-cssdev",
	"Creating XHTML source and cascading stylesheet from an layered image. Presented by area42 - Agentur & Systempartner: www.area42.de.",
	"Creating XHTML source and cascading stylesheet from an layered image. Presented by area42 - Agentur & Systempartner: www.area42.de",
	"Eckhard M. Jaeger",
	"Eckhard M. Jaeger",
	"2008",
	"<Image>/Filters/Web/CSSdev...",
	"",
	[
		(PF_DIRNAME,	"export_dir", 		_("Export to"), ""),
		(PF_RADIO, 		"export_hidden", 	_("Export hidden Layers"), "0", (("No","0"), ("As visible","2"), ("As hidden in XHTML/ CSS","4"))),
		(PF_RADIO, 		"export_layer", 	_("Export Layers as"), "0", (("Absolute aligned wit z-Index","0"), ("As nested with margins","2"))),
		(PF_TOGGLE,		"export_height", 	_("Export Layer Height"), False),
		(PF_TOGGLE, 	"export_divs", 	_("Export Linked Layers as nested Div"), True),
		(PF_COLOR, 		"li_color", 		_("Link Color"), (0,0,192)),
		(PF_COLOR, 		"ho_color", 		_("Link Hover Color"), (0,0,255)),
		(PF_TOGGLE, 	"div_color", 		_("Caclculate Background Color of Layers"), True),
		(PF_FONT, 		"default_font", 	_("Default Webpage Font"), "Verdana"),
		(PF_SPINNER,	"default_size", 	_("Default Font Size (px)"), 12, (0,100,1)),
		(PF_TOGGLE, 	"center_content", _("Center Page in Browser (for nesting only)"), True),
		(PF_TOGGLE, 	"generate_image", _("Generate Images"), False),
		(PF_RADIO, 		"image_format", 	_("Image Format"), "gif", (("GIF","gif"), ("JPEG","jpg"), ("PNG","png"))),
	],
	[],
	python_cssdev,
	domain=("gimp20-python", gimp.locale_directory))

main()
