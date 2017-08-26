#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Screenshot Tearoff Effect
# Copyright (c) 2008 Edgar D'Souza
# Contact: edgar.b.dsouza@gmail.com

# ---------------------------------------------------------------------
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------------
#Version: 0.1 - initial release.
#	Known issues:
#	- hasn't been tested on anything except the development platform - my laptop :)
#
# GIMP Python info taken from: http://www.gimp.org/docs/python/index.html
#
# This script was developed and tested under the following environment:
# - Ubuntu Linux 7.10
# - Python 2.5.1
# - GIMP 2.4.2
# - gimp-python 2.4.2-0ubuntu0.7.10.1
# - libgimp2.0 2.4.2-0ubuntu0.7.10.1
# 
# Before invoking the plugin:
# 1. Capture a new screenshot, or open an existing image file.
# 2. Select the part to keep, making the selection a little larger than what
# 	 you want to keep. GIMP does appear to distress the selection "outward",
# 	 but verify that you're not losing image data that you want to keep. Since
# 	 a few more pixels than needed are probably not going to be fatal :) I'm
# 	 suggesting selecting a little more than you need to keep.

from gimpfu import *

#Helper function - writes debug messages to a log file. 
def log(msg, init_file=False):
	"""Opens a log file, writes the string in the msg argument, closes the file.
		The optional init_file argument, if True, (re)initializes the file.
	"""
	filename = '/tmp/gimp_plugin_tearoff.log'
	#Uncomment and customize the next line if you're trying to get it running on Windows...
	#filename = r"C:\gimp_plugin_tearoff.log"
	dumpstr = "tearoff: " + msg + '\n'
	if init_file:
		write_mode = 'w'
	else:
		write_mode = 'a'
	tmpfile = open(filename, write_mode)
	tmpfile.write(dumpstr)
	tmpfile.close()
	#Observation: the log file contains only one startup and invocation block, no matter
	#how many times I invoke the plugin on a single image window. This implies that it's
	#being torn down and re-initialized after every invocation. A bit expensive, perhaps,
	#but I suppose it enables better garbage collection...just a guess.

#The main plugin function that manipulates the image.
def tearoff(the_img, the_drawable,
			distort_threshold, distort_spread, distort_granularity, distort_smooth_level, 
			add_alpha, add_dropshadow,
			ds_offset_x, ds_offset_y, ds_blur_radius, ds_color, ds_opacity, ds_allow_resizing,
			crop
			):
	"""Applies the tearoff effect to the image.
	Parameters expected are those defined in the plugin_params_list below, plus:
		the_img: the open, currently active image (auto-passed by gimp-fu)
		the_drawable: drawable layer of the image (-do-)
	"""
	# 1A. Check that there is a valid selection.
	try:
		sel_empty = pdb.gimp_selection_is_empty(the_img)		#See "name weirdness" note below.
		log('pdb.gimp_selection_is_empty returned:' + str(sel_empty))
	except BaseException, error:
		log('Calling pdb.gimp_selection_is_empty: error: ' + str(error))
		
	if sel_empty:
		log("Error: called without a selection for the image!")
		gimp.message('This plugin needs a selection in the image; please select an area of the image and re-run the plugin.\n\nPlugin will now exit.')
		return False
	
	# 1B. Start an undo group on the image, so that all steps done in the plugin can be undone in
	# one user undo step (Ctrl-Z)
	try:
		pdb.gimp_image_undo_group_start(the_img)
	except BaseException, error:
		log('Calling pdb.gimp_image_undo_group_start: error: ' + str(error))
	
	# 2. Distress the selection, passing parameters obtained from UI. GIMP is smart
	#    - it only distresses the edge of the selection that has image area beyond it
	#    (i.e. the selection edge is NOT up against the edge of the image).
	try:
		pdb.script_fu_distress_selection(the_img, the_drawable,distort_threshold,
			distort_spread, distort_granularity, distort_smooth_level,
			True, True)	# The True values are to force smoothing horiz and vert; looks awful otherwise.
	except BaseException, error:
		log('Calling script_fu_distress_selection: error: ' + str(error))
	
	# 3A. Found that re-inverting the selection in code after cutting out
	# 	  image content doesn't work (whole layer is selected) so am saving
	# 	  the active selection here and will load it later.
	try:
		saved_selection_channel = pdb.gimp_selection_save(the_img)
	except BaseException, error:
		log('Calling gimp_selection_save: error: ' + str(error))
	
	# 3B. Invert the selection, so what is selected can be deleted.
	try:
		pdb.gimp_selection_invert(the_img)
	except BaseException, error:
		log('Calling gimp_selection_invert: error: ' + str(error))
		
	# 4. Check if image has alpha channel (transparency) or add it (GUI: Layer >
	#    Transparency > Add Alpha Channel)
	if not add_alpha:
		log('Parameter passed from UI said not to add alpha channel')
	else:
		try:
			if the_drawable.has_alpha > 0:
				pass
			else:
				pdb.gimp_layer_add_alpha(the_drawable)
		except BaseException, error:
			log('Checking/adding alpha channel: error: ' + str(error))
	
	# 5. Delete selection (leaving behind transparent/background-filled area).
	try:
		del_buf_name = pdb.gimp_edit_named_cut(the_drawable, "del_buf")
		pdb.gimp_buffer_delete(del_buf_name)
		#Couldn't find a 'delete selection' method... but this works?
		#Cutting to a named buffer, and then deleting it, avoids leaving the cut
		#image data on the clipboard, which ugly outcome you face when using just
		#plain gimp-edit-cut().
	except BaseException, error:
		log('Trying to delete (inverted) selected area: error: ' + str(error))

	# 6. Invert the selection again, selecting the part we want to keep.
	# 6A. Found that re-inverting the selection in code after cutting out
	# 	  image content doesn't work (whole layer is selected) so I load
	# 	  a selection that I saved earlier in step 3A.
	try:
		pdb.gimp_selection_none(the_img)
		pdb.gimp_selection_load(saved_selection_channel)
	except BaseException, error:
		log('Loading saved selection: error: ' + str(error))
		
	# 7. Call Filters > Light and Shadow > Drop Shadow
	if not add_dropshadow:
		log('Parameter passed from UI said not to add drop shadow')
	else:
		try:
			pdb.script_fu_drop_shadow(the_img, the_drawable, ds_offset_x, ds_offset_y,
									  ds_blur_radius, ds_color, ds_opacity, ds_allow_resizing)
		except BaseException, error:
			log('Calling script_fu_drop_shadow: error: ' + str(error))
		
		# 8. Extend selection by extra area added by drop shadow plugin.
		log('Trying to grow selection to include drop shadow...')
		try:
			#Should grow it uniformly to the largest non-negative offset of
			#drop-shadow.
			grow_size = max(ds_offset_x, ds_offset_y)
			if grow_size > 0:
				pdb.gimp_selection_grow(the_img, grow_size)
		except BaseException, error:
			log('Calling gimp_selection_grow: error: ' + str(error))
	
	# 9. Manually crop image if user has chosen to do so.
	# 	 (Doing an auto-crop seems to eat up too much into the shadow...)
	if not crop:
		log('User-supplied UI param said not to crop image.')
	else:
		log('Trying to crop image...')
		try:
			#Due to previous operations, we're sure we have a selection...
			has_sel, x1, y1, x2, y2 = pdb.gimp_selection_bounds(the_img)
			pdb.gimp_image_crop(the_img, x2-x1, y2-y1, x1, y1)
		except BaseException, error:
			log('Cropping image: error: ' + str(error))

	# 9B. End the undo group we started in step 1B.
	try:
		pdb.gimp_image_undo_group_end(the_img)
	except BaseException, error:
		log('Calling pdb.gimp_image_undo_group_end: error: ' + str(error))
	
#Register the plugin with GIMP.
#For help/more info on params, read http://www.gimp.org/docs/python/index.html
#Construct our list of params for register() in a verbose manner...
params_list = []
#Params that we will pass to the selection-distort procedure:
params_list.append( (PF_SPINNER, "distort_threshold", "(Selection) Distort - Threshold", 111, (1,255,1)) )
params_list.append( (PF_SPINNER, "distort_spread", "Distort - Spread", 8, (0,1000,1)) )
params_list.append( (PF_SPINNER, "distort_granularity", "Distort - Granularity (1 is low)", 4, (1,25,1)) )
params_list.append( (PF_SPINNER, "distort_smooth_level", "Distort - Smoothing Level", 5, (1,150,1)) )
#Parameters specific to this plugin:
params_list.append( (PF_BOOL, "add_alpha", "Delete to transparency (add an alpha channel to image)?", True) )
params_list.append( (PF_BOOL, "add_dropshadow", "Add drop shadow?", True) )
#Parameters for the drop shadow (if user chooses to apply it):
params_list.append( (PF_SPINNER, "ds_offset_x", "Drop Shadow - X axis offset (pixels)", 8, (-25,25,1)) )
params_list.append( (PF_SPINNER, "ds_offset_y", "Drop Shadow - Y axis offset (pixels)", 8, (-25,25,1)) )
params_list.append( (PF_SPINNER, "ds_blur_radius", "Drop Shadow - Blur Radius", 15, (0,1024,1)) )
params_list.append( (PF_COLOR, "ds_color", "Drop Shadow - Shadow Color", (0,0,0)) )
params_list.append( (PF_SLIDER, "ds_opacity", "Drop Shadow - Shadow Opacity", 80, (0,100,1)) )
params_list.append( (PF_TOGGLE, "ds_allow_resizing", "Drop Shadow - Allow resizing?", True) )
#Another parameter specific to this plugin:
params_list.append( (PF_TOGGLE, "crop", "Crop image down to (selection plus drop shadow, if applied)?", True) )

log("About to register", init_file=True)	#First invocation to log() in this script's run; empty out the log file.

register(
	"screenshot_tearoff",
	"Applies an irregular 'tear-off' effect to the image at any selection edge that is NOT at the boundary of the image; deletes the remainder of the image, optionally applies a drop shadow to the 'to-keep' area, and optionally crops the image.",
	"Select the part of the image that you want to keep, before invoking this plugin. You can adjust the selection distort/distress parameters, and choose whether to apply a drop shadow and whether to crop the image after that.",
	"Edgar D'Souza (edgar.b.dsouza@gmail.com)",
	"(c) 2008 Edgar D'Souza, licensed under GPL v3 or later",
	"2008-10-26",
	"<Image>/Image/Te_ar-off",
	"RGB*, GRAY*",
	params_list,
	[],
	tearoff
	)

log("Registration finished")

log("Calling main() to start the plugin running.")
main()
