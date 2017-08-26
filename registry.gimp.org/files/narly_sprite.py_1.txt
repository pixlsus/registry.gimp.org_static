#!/usr/bin/env python

from gimpfu import *
import re
import math
import json

COPYRIGHT1 = "Nephi Johnson"
COPYRIGHT2 = "Nephi Johnson"
COPYRIGHT_YEAR = "2012"



def make_frame_name(frame_num):
	return "Frame %d" % (frame_num) 

def _shift_frames_helper(img, start_frame_num, delta):
	for frame in get_frames(img):
		curr_frame_num = get_frame_num(frame)
		if curr_frame_num >= start_frame_num:
			frame.name = make_frame_name(curr_frame_num+delta) + " SHIFTTMP"

	for frame in get_frames(img):
		frame.name = frame.name.replace(" SHIFTTMP", "")

def copy_layer_no_data(img, layer):
	res = pdb.gimp_layer_new(
		img,
		layer.width,
		layer.height,
		layer.type,
		layer.name,
		100,		# opacity
		NORMAL_MODE
	)
	return res

narly_sprite_default_config = {
	"new_frame_copy_image_data": False,

	"always_show_prev_frame": False,
	"show_prev_frame_on_new": True,
	"prev_frame_alpha": 30.0,
}
def get_config_parasite(img):
	p = img.parasite_find("narly_sprite_config")
	if p is None:
		p = gimp.Parasite(
			"narly_sprite_config",
			1,	# 1 = Persistent
			json.dumps(narly_sprite_default_config)
		)
		img.parasite_attach(p)
	return p

def get_config(img):
	p = get_config_parasite(img)
	config = json.loads(p.data)
	return config

# TODO: this doesn't work, fix it!
def save_config(img, config):
	get_config_parasite(img)
	img.parasite_detach("narly_sprite_config")
	new_parasite = gimp.Parasite(
		"narly_sprite_config",
		1,	# 1 = Persistent
		json.dumps(config)
	)
	img.parasite_attach(new_parasite)

def shift_frames_up(img, start_frame_num):
	"""
	Shift frames "up" - number-wise a frame would go from
	being frame 4 to frame 3
	"""
	_shift_frames_helper(img, start_frame_num, -1)

def shift_frames_down(img, start_frame_num):
	"""
	Shift frames "down" - number-wise a frame would go from
	being frame 3 to frame 4
	"""
	_shift_frames_helper(img, start_frame_num, 1)

def get_frame_by_number(img, num):
	for frame in get_frames(img):
		if get_frame_num(frame) == num:
			return frame
	return None

def get_frames(img):
	res = []
	for layer in img.layers:
		if get_frame_num(layer) is not None:
			res.append(layer)
	return res

def make_frame_visible(img, frame_num, opacity=100.0):
	frames = get_frames(img)
	for frame in frames:
		curr_frame_num = get_frame_num(frame)
		if curr_frame_num == frame_num:
			pdb.gimp_image_undo_freeze(img)
			frame.opacity = opacity
			frame.visible = True
			pdb.gimp_image_undo_thaw(img)

def goto_frame(img, frame_num, layer_pos=0, set_active=True):
	"""
	Sets the desired frame folder to be visible and all
	other frame folders to not be visible.

	@returns whether or not it even found the frame you were
	looking for
	"""

	last_frame = get_last_frame_num(img)
	# means there's no frames left in the img
	if last_frame == -1:
		return

	frame_num = frame_num % (last_frame+1)

	found_frame = False
	for frame in get_frames(img):
		curr_frame_num = get_frame_num(frame)
		frame.opacity = 100.0
		if curr_frame_num == frame_num:
			frame.visible = True
			found_frame = True
			if set_active:
				if len(frame.children) > 0:
					pdb.gimp_image_set_active_layer(img, frame.children[layer_pos])
				else:
					pdb.gimp_image_set_active_layer(img, frame)
		else:
			frame.visible = False

	return found_frame

def get_last_frame_position(img):
	last_pos = -1
	curr_pos = 0
	for layer in img.layers:
		curr_frame_num = get_frame_num(layer)
		if curr_frame_num is not None:
			last_pos = pdb.gimp_image_get_layer_position(img, layer)

	return last_pos

def get_last_frame_num(img):
	max_frame_num = -1
	for layer in img.layers:
		curr_frame_num = get_frame_num(layer)
		if curr_frame_num is not None:
			if curr_frame_num > max_frame_num:
				max_frame_num = curr_frame_num

	return max_frame_num

def is_frame_root(layer):
	return pdb.gimp_item_is_group(layer) and layer.parent is None

def get_frame_root(layer):
	"""
	Assumes the layer is either a valid layer inside of a frame
	folder, or that it's a frame folder itself
	"""
	if pdb.gimp_item_is_group(layer):
		return layer
	
	return layer.parent

def get_frame_num(layer):
	if layer is None:
		return None

	match_string = None

	# it's a folder, so match off the folder's name
	if pdb.gimp_item_is_group(layer):
		# all frame folders must be at top level
		# (arbitrary, I know, but oh well)
		if layer.parent is not None:
			return None

		match_string = layer.name

	# it's a normal layer (not a folder), so check
	# to see if it's in a frame folder
	else:
		if layer.parent is None:
			return None
		if not pdb.gimp_item_is_group(layer.parent):
			return None

		match_string = layer.parent.name
	
	match = re.match(r"Frame (\d+)", match_string)

	if match is None:
		return None

	frame = int(match.groups()[0])
	return frame

def get_layers_in_frame(img, frame_num):
	res = []

	for layer in img.layers:
		layer_frame_num = get_frame_num(layer.name)
		if layer_frame_num == frame_num:
			res.append(layer)
	
	return res

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_export_flatten(img, layer, reverse, display_image=True):
	new_img = gimp.Image(img.width, img.height, img.base_type)

	if display_image:
		gimp.Display(new_img)
		gimp.displays_flush()

	frames = get_frames(img)

	if reverse:
		frames.reverse()
	
	pdb.gimp_image_undo_freeze(img)

	curr_frame = get_frame_num(layer)

	curr_count = 0
	for frame in frames:
		frame_num = get_frame_num(frame)
		goto_frame(img, frame_num, set_active=False)
		pdb.gimp_edit_copy_visible(img)
		new_layer = pdb.gimp_layer_new(
			new_img,
			new_img.width,
			new_img.height,
			new_img.base_type*2+1,
			make_frame_name(frame_num),
			100,	# opacity
			NORMAL_MODE
		)
		pdb.gimp_image_insert_layer(new_img, new_layer, None, len(new_img.layers))
		pasted_layer = pdb.gimp_edit_paste(new_layer, 0) # 0 = clear selection in the new image
		pdb.gimp_floating_sel_anchor(pasted_layer)
		curr_count += 1

		pdb.gimp_progress_update(float(curr_count) / len(frames))
	
	# make the current frame visible again
	if curr_frame is not None:
		goto_frame(img, curr_frame)

	# set focus back to the active layer
	pdb.gimp_image_set_active_layer(img, layer)
	
	pdb.gimp_image_undo_thaw(img)
	
	return new_img

register(
	"python_fu_narly_sprite_convert_frames_to_layers",	# unique name for plugin
	"Narly Sprite Export Flatten",		# short name
	"Flatten all of the frames into individual layers",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Export/Flatten",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[
		(PF_TOGGLE, "reverse", "Reverse Frame Order", False)
	],	# input params,
	[],	# output params,
	narly_sprite_export_flatten	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_toggle_visibility_all_current_layer(img, layer):
	"""
	Hides or shows the current layer in all frames.
	"""
	this_frame_num = get_frame_num(layer)

	# can't perform this operation if a frame layer isn't currently selected
	# (reading minds will be implemented in v 9.0)
	if this_frame_num is None:
		return
	
	# a frame layer needs to be selected, not the frame folder layer
	if is_frame_root(layer):
		return

	# don't store these actions in the undo history
	pdb.gimp_image_undo_freeze(img)

	frame_pos = pdb.gimp_image_get_layer_position(img, layer)

	# toggle the visibility - this is what we'll set all of the other
	# frames to as well
	layer.visible = not layer.visible

	frames = get_frames(img)
	for frame in frames:
		curr_frame_num = get_frame_num(frame)
		# sanity check - make sure that there's enough frame layers in this frame
		if len(frame.children) >= frame_pos:
			frame.children[frame_pos].visible = layer.visible

	# make sure we keep the currently selected layer the active one (not sure if
	# toggling the visibility on other layers changes that)
	pdb.gimp_image_set_active_layer(img, layer)

	# resume normal undo recording
	pdb.gimp_image_undo_thaw(img)

register(
	"python_fu_narly_sprite_toggle_visibility_all_current_layer",	# unique name for plugin
	"Narly Sprite Toggle Frame Layer Visibility",	# short name
	"Narly Sprite Toggle Frame Layer Visibility",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Frames/Toggle Visibility",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[
	],	# input params,
	[],	# output params,
	narly_sprite_toggle_visibility_all_current_layer	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_copy_layer_to_all_frames(img, layer):
	"""
	Copies the current layer to all frames. If the current layer is in a
	frame, it will try to copy it to the same position. Otherwise, it
	is added at the last position.
	"""
	dont_copy_to = get_frame_num(layer)
	frame_pos = -1
	if dont_copy_to is not None:
		frame_pos = pdb.gimp_image_get_layer_position(img, layer)

	pdb.gimp_undo_push_group_start(img)

	frames = get_frames(img)
	curr_count = 0
	for frame in frames:
		frame_num = get_frame_num(frame)
		if frame_num == dont_copy_to:
			continue
		copied_layer = layer.copy()
		copied_layer.name = layer.name
		pos_to_insert_at = frame_pos if frame_pos != -1 else len(frame.children)
		pdb.gimp_image_insert_layer(img, copied_layer, frame, pos_to_insert_at)

		curr_count += 1
		pdb.gimp_progress_update(float(curr_count) / len(frames))
	
	# restore focus back to the original layer
	pdb.gimp_image_set_active_layer(img, layer)

	pdb.gimp_undo_push_group_end(img)

register(
	"python_fu_narly_sprite_copy_layer_to_all_frames",	# unique name for plugin
	"Narly Sprite Copy Layer to All Frames",		# short name
	"Narly Sprite Copy Layer to All Frames",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Layer to all Frames",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[
	],	# input params,
	[],	# output params,
	narly_sprite_copy_layer_to_all_frames	# actual function
)


# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

HORIZONTAL = 0
GRID = 1
def narly_sprite_export_sprite_sheet(img, layer, sheet_type):
	frames = get_frames(img)

	if sheet_type == HORIZONTAL:
		new_img = gimp.Image(img.width * len(frames), img.height, img.base_type)
		gimp.Display(new_img)
		gimp.displays_flush()

		pdb.gimp_image_undo_freeze(img)
		frames = get_frames(img)
		curr_count = 0
		for frame in frames:
			frame_num = get_frame_num(frame)
			goto_frame(img, frame_num)
			pdb.gimp_edit_copy_visible(img)
			new_layer = pdb.gimp_layer_new(
				new_img,
				img.width,
				img.height,
				new_img.base_type*2+1,
				make_frame_name(frame_num),
				100,	# opacity
				NORMAL_MODE
			)
			pdb.gimp_image_insert_layer(new_img, new_layer, None, len(new_img.layers))
			pasted_layer = pdb.gimp_edit_paste(new_layer, 0) # 0 = clear selection in the new image
			pdb.gimp_floating_sel_anchor(pasted_layer)

			pdb.gimp_layer_set_offsets(new_layer, frame_num*img.width, 0)

			curr_count += 1
			pdb.gimp_progress_update(float(curr_count)/len(frames))

		pdb.gimp_image_undo_thaw(img)
	
	elif sheet_type == GRID:
		# determine the total number of rows and columns in the sprite sheet
		total_area = img.width * img.height * len(frames)
		square = math.sqrt(total_area)
		num_rows = square / img.height
		num_cols = square / img.width

		ceil_rows = math.ceil(num_rows) * img.height * img.width * math.floor(num_cols)
		ceil_cols = math.floor(num_rows) * img.height * img.width * math.ceil(num_cols)
		both_ceil = math.ceil(num_rows) * img.height * img.width * math.ceil(num_cols)

		if ceil_rows >= total_area and ceil_cols >= total_area:
			if ceil_rows < ceil_cols:
				num_rows = math.ceil(num_rows)
				num_cols = math.floor(num_cols)
			else:
				num_rows = math.floor(num_rows)
				num_cols = math.ceil(num_cols)
		if ceil_rows >= total_area:
			num_rows = math.ceil(num_rows)
			num_cols = math.floor(num_cols)
		if ceil_cols >= total_area:
			num_rows = math.floor(num_rows)
			num_cols = math.ceil(num_cols)
		elif both_ceil >= total_area:
			num_rows = math.ceil(num_rows)
			num_cols = math.ceil(num_cols)

		num_cols = int(num_cols)
		num_rows = int(num_rows)

		new_img = gimp.Image(img.width*num_cols, img.height*num_rows, img.base_type)
		gimp.Display(new_img)
		gimp.displays_flush()

		pdb.gimp_image_undo_freeze(img)
		frames = get_frames(img)
		curr_count = 0
		for frame in frames:
			frame_num = get_frame_num(frame)

			goto_frame(img, frame_num)
			pdb.gimp_edit_copy_visible(img)
			new_layer = pdb.gimp_layer_new(
				new_img,
				img.width,
				img.height,
				new_img.base_type*2+1,
				make_frame_name(frame_num),
				100,	# opacity
				NORMAL_MODE
			)
			pdb.gimp_image_insert_layer(new_img, new_layer, None, len(new_img.layers))
			pasted_layer = pdb.gimp_edit_paste(new_layer, 0) # 0 = clear selection in the new image
			pdb.gimp_floating_sel_anchor(pasted_layer)
			
			frame_col = frame_num % num_cols
			frame_row = int((frame_num - frame_col) / num_cols)
			pdb.gimp_layer_set_offsets(new_layer, frame_col*img.width, frame_row*img.height)

			curr_count += 1
			pdb.gimp_progress_update(float(curr_count)/len(frames))

		pdb.gimp_image_undo_thaw(img)
	
	# if we were in a valid frame, make that frame visible again
	curr_frame_num = get_frame_num(layer)
	if curr_frame_num is not None:
		pdb.gimp_image_undo_freeze(img)
		goto_frame(img, curr_frame_num)
		pdb.gimp_image_undo_thaw(img)

	# make the current layer the active layer again
	pdb.gimp_image_set_active_layer(img, layer)

register(
	"python_fu_narly_sprite_export_sprite_sheet",	# unique name for plugin
	"Narly Sprite Export Sprite Sheet",		# short name
	"Narly Sprite Export Sprite Sheet",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Export/Sprite Sheet",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[
		(PF_RADIO, "sheet_type", "Sprite Sheet Type", True,
			(
				("Horizontal", HORIZONTAL),
				("Grid", GRID),
			)
		),
	],	# input params,
	[],	# output params,
	narly_sprite_export_sprite_sheet	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_play_animation(img, layer):
	return
	# last_frame_num = get_last_frame_num(img)
# 
	# anim_window = AnimationWindow(img)
	# gtk.main()

register(
	"python_fu_narly_sprite_play_animation",	# unique name for plugin
	"Narly Sprite Play Animation",		# short name
	"Narly Sprite Play Animation",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Play Animation",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_play_animation	# actual function
)

# from gobject import timeout_add
# 
# class AnimationWindow(gtk.Window):
	# def __init__ (self, img, *args):
		# self.img = img
		# self._currFrameNum = 0
		# self._frameDelay = 100
# 
		# # Create the dialog
		# win = gtk.Window.__init__(self, *args)
# 
		# # Obey the window manager quit signal:
		# self.connect("destroy", gtk.main_quit)
# 
		# # Make the UI
		# self.set_border_width(10)
		# vbox = gtk.VBox(spacing=10, homogeneous=False)
		# self.add(vbox)
		# label = gtk.Label("Narly Sprite Animator")
		# vbox.add(label)
		# label.show()
# 
		# table = gtk.Table(rows=2, columns=2, homogeneous=False)
		# table.set_col_spacings(10)
		# vbox.add(table)
# 
		# # Delay Changer
		# label = gtk.Label("Delay")
		# label.set_alignment(xalign=0.0, yalign=1.0)
		# table.attach(label, 0, 1, 0, 1, xoptions=gtk.FILL, yoptions=0)
		# label.show()
		# delay_adj = gtk.Adjustment(value=100, lower=0, upper=5000, step_incr=10)
		# delay_adj.connect("value_changed", self.delay_changed_cb)
		# delay_input = gtk.SpinButton(delay_adj, climb_rate=10, digits=0)
		# table.attach(delay_input, 1, 2, 0, 1)
		# delay_input.show()
# 
		# table.show()
# 
		# hbox = gtk.HBox(spacing=20)
		# play_btn = gtk.Button(stock=gtk.STOCK_MEDIA_PLAY)
		# play_btn.set_use_stock(True)
		# hbox.add(play_btn)
		# play_btn.connect("clicked", self.play_animation)
		# play_btn.show()
# 
		# vbox.add(hbox)
		# hbox.show()
# 
		# # Make the dialog button box
		# hbox = gtk.HBox(spacing=20)
# 
		# btn = gtk.Button("Close")
		# hbox.add(btn)
		# btn.show()
		# btn.connect("clicked", gtk.main_quit)
# 
		# vbox.add(hbox)
		# hbox.show()
		# vbox.show()
		# self.show()
# 
		# timeout_add(300, self.update, self)	
		# return win
# 
	# def play_animation(self):
		# pass
# 
	# def delay_changed_cb(self, val):
		# self._frameDelay = val
# 
	# def update(self, *args):
		# pdb.gimp_displays_flush()

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_delete_frame(img, layer):
	curr_frame_num = get_frame_num(layer)
	if curr_frame_num is None:
		return
	
	curr_frame_pos = 0
	if not is_frame_root(layer):
		curr_frame_pos = pdb.gimp_image_get_layer_position(img, layer)
	
	pdb.gimp_undo_push_group_start(img)

	frame_root = get_frame_root(layer)
	pdb.gimp_image_remove_layer(img, frame_root)
	shift_frames_up(img, curr_frame_num+1)
	pdb.gimp_image_undo_freeze(img)
	if not goto_frame(img, curr_frame_num, curr_frame_pos):
		goto_frame(img, curr_frame_num-1, curr_frame_pos)
	pdb.gimp_image_undo_thaw(img)

	pdb.gimp_undo_push_group_end(img)

register(
	"python_fu_narly_sprite_del_frame",	# unique name for plugin
	"Narly Sprite Delete Frame",		# short name
	"Narly Sprite Delete Frame",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Frames/Delete Frame",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_delete_frame	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_new_frame(img, layer):
	last_frame_num = get_last_frame_num(img)
	curr_frame_num = get_frame_num(layer)
	if curr_frame_num == -1:
		frame_num_to_copy = last_frame_num
	else:
		frame_num_to_copy = curr_frame_num
	
	config = get_config(img)

	# means that we're currently in a valid frame, so
	# insert a new frame after this one, copying all the layers
	# and shifting all the subsequent frames down
	if frame_num_to_copy is not None:
		pdb.gimp_undo_push_group_start(img)

		frame_root = get_frame_by_number(img, frame_num_to_copy)
		curr_frame_position = pdb.gimp_image_get_layer_position(img, frame_root)

		new_frame_num = frame_num_to_copy+1
		new_frame_pos = curr_frame_position+1

		# shift down any frames after the current one so we leave an
		# opening for the new frame
		shift_frames_down(img, new_frame_num)

		# make the new frame's folder and add it to the image
		new_frame_root = pdb.gimp_layer_group_new(img)
		new_frame_root.name = make_frame_name(new_frame_num)
		pdb.gimp_image_insert_layer(img, new_frame_root, None, new_frame_pos)

		# copy any layers in the current frame to the
		# new frame
		for frame_layer in frame_root.children:
			new_layer = None
			if config["new_frame_copy_image_data"]:
				new_layer = frame_layer.copy()
				new_layer.name = frame_layer.name
			else:
				new_layer = copy_layer_no_data(img, frame_layer)
			pdb.gimp_image_insert_layer(img, new_layer, new_frame_root, len(new_frame_root.children))

		curr_pos_in_frame = 0
		if not is_frame_root(layer) and curr_frame_num is not None:
			curr_pos_in_frame = pdb.gimp_image_get_layer_position(img, layer)

		goto_frame(img, new_frame_num, curr_pos_in_frame)

		pdb.gimp_undo_push_group_end(img)

		if config["always_show_prev_frame"] or config["show_prev_frame_on_new"]:
			frame_root.opacity = config["prev_frame_alpha"]
			frame_root.visible = True

		return
	
	pdb.gimp_undo_push_group_start(img)

	# making it here means that we're not currently in a valid frame, so
	# just make the frame folder, add a frame layer, and be done with it
	# (it automatically adds the frame at the end)
	last_frame_num = get_last_frame_num(img)
	new_frame_root = pdb.gimp_layer_group_new(img)
	new_frame_root.name = make_frame_name(last_frame_num+1)
	pdb.gimp_image_insert_layer(img, new_frame_root, None, get_last_frame_position(img)+1)

	# add a blank new layer
	blank_layer = pdb.gimp_layer_new(
		img,
		img.width,
		img.height,
		img.base_type*2+1,	# RBGA_IMAGE,etc - always include the alpha TODO: change this?
		"Layer 1",	# layer name
		100,	# opacity
		NORMAL_MODE	# layer combination mode
	)
	pdb.gimp_image_insert_layer(img, blank_layer, new_frame_root, 0)

	pdb.gimp_image_undo_freeze(img)
	goto_frame(img, last_frame_num+1)
	pdb.gimp_image_undo_thaw(img)

	pdb.gimp_undo_push_group_end(img)

register(
	"python_fu_narly_sprite_new_frame",	# unique name for plugin
	"Narly Sprite New Frame",		# short name
	"Narly Sprite New Frame",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Frames/New Frame",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_new_frame	# actual function
	#menu="<Image>/Sprite/Frames"
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def get_min_max_coords(layer):
	layer_offsets = layer.offsets
	offset_x = layer_offsets[0]
	offset_y = layer_offsets[1]
	min_x = layer.width
	min_y = layer.height
	max_x = 0
	max_y = 0

	curr_x = 0
	curr_y = 0
	while curr_y < layer.height:
		curr_x = 0
		found_pixel = False
		# find the first pixel from the left
		while curr_x < min_x:
			pixel_val = layer.get_pixel(curr_x, curr_y)
			# found our first pixel from the left
			if pixel_val[3] != 0:
				min_x = curr_x
				found_pixel = True
				break
			curr_x += 1

		curr_x = layer.width-1
		# now go from right to left until we find our first pixel
		while curr_x > max_x:
			pixel_val = layer.get_pixel(curr_x, curr_y)
			# found our first pixel from the left that isn't alpha=0
			if pixel_val[3] != 0:
				max_x = curr_x
				found_pixel = True
				break
			curr_x -= 1

		if found_pixel:
			if curr_y < min_y:
				min_y = curr_y

			if curr_y > max_y:
				max_y = curr_y

		curr_y += 1
	
	curr_x = min_x
	while curr_x < max_x:
		curr_y = layer.height-1
		# now go from right to left until we find our first pixel
		while curr_y > max_y:
			pixel_val = layer.get_pixel(curr_x, curr_y)
			# found our first pixel from the left that isn't alpha=0
			if pixel_val[3] != 0:
				max_y = curr_y
				found_pixel = True
				break
			curr_y -= 1

		curr_x += 1
	
	return (min_x, min_y, max_x, max_y)

def narly_sprite_trim(img, layer):
	pdb.gimp_undo_push_group_start(img)
	
	min_x = img.width-1
	min_y = img.height-1
	max_x = 0
	max_y = 0

	frames = get_frames(img)

	curr_count = 0
	for frame in frames:
		fminx,fminy,fmaxx,fmaxy = get_min_max_coords(frame)
		if fminx < min_x:
			min_x = fminx
		if fminy < min_y:
			min_y = fminy
		if fmaxx > max_x:
			max_x = fmaxx
		if fmaxy > max_y:
			max_y = fmaxy

		curr_count += 1

		pdb.gimp_progress_update(float(curr_count)/ len(frames))

	pdb.gimp_image_crop(img, max_x - min_x+1, max_y - min_y+1, min_x, min_y)

	pdb.gimp_undo_push_group_end(img)

register(
	"python_fu_narly_sprite_trim",	# unique name for plugin
	"Narly Sprite Trim",		# short name
	"Narly Sprite Trim",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Tools/Trim Sprite",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_trim	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_prev_frame(img, layer):
	curr_frame_num = get_frame_num(layer)
	if curr_frame_num is None:
		return
	
	# don't know what's happening here
	if curr_frame_num < 0:
		return
	
	curr_pos_in_frame = 0
	if not is_frame_root(layer):
		curr_pos_in_frame = pdb.gimp_image_get_layer_position(img, layer)

	pdb.gimp_image_undo_freeze(img)
	goto_frame(img, curr_frame_num-1, curr_pos_in_frame)
	pdb.gimp_image_undo_thaw(img)

	config = get_config(img)
	if curr_frame_num > 2 and config["always_show_prev_frame"]:
		make_frame_visible(img, curr_frame_num-2, config["prev_frame_alpha"])

register(
	"python_fu_narly_sprite_prev_frame",	# unique name for plugin
	"Narly Sprite Prev Frame",		# short name
	"Narly Sprite Prev Frame",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Frames/Prev Frame",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_prev_frame	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_next_frame(img, layer):
	curr_frame_num = get_frame_num(layer)
	if curr_frame_num is None:
		return
	
	last_frame_num = get_last_frame_num(img)

	# don't know what the heck is going on here
	if curr_frame_num > last_frame_num:
		return
	
	curr_pos_in_frame = 0
	if not is_frame_root(layer):
		curr_pos_in_frame = pdb.gimp_image_get_layer_position(img, layer)
	
	pdb.gimp_image_undo_freeze(img)
	goto_frame(img, curr_frame_num+1, curr_pos_in_frame)
	pdb.gimp_image_undo_thaw(img)

	config = get_config(img)
	if config["always_show_prev_frame"]:
		make_frame_visible(img, curr_frame_num, config["prev_frame_alpha"])

register(
	"python_fu_narly_sprite_next_frame",	# unique name for plugin
	"Narly Sprite Next Frame",		# short name
	"Narly Sprite Next Frame",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Frames/Next Frame",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_next_frame	# actual function
)

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_settings(img, layer):
	import gtk
	class NarlySettingsDialog(gtk.Window):
		def __init__(self, img, *args):
			self.img = img
			self.config = get_config(img)

			win = gtk.Window.__init__(self, *args)
			self.connect("destroy", gtk.main_quit)
			self.set_border_width(10)

			# add the main vbox and the title label
			vbox = gtk.VBox(spacing=10, homogeneous=False)
			self.add(vbox)
			label = gtk.Label("Narly Sprite Settings")
			vbox.add(label)
			label.show()

			# for new_frame_copy_image_data
			new_frame_copy = gtk.CheckButton("Copy Pixels to New Frame")
			new_frame_copy.set_active(self.config["new_frame_copy_image_data"])
			new_frame_copy.connect("toggled", self.new_frame_copy_toggled, new_frame_copy)
			vbox.add(new_frame_copy)
			new_frame_copy.show()

			sep = gtk.HSeparator()
			vbox.add(sep)
			sep.show()

			# for new_frame_copy_image_data
			always_show_prev_frame = gtk.CheckButton("Always Show Prev Frame")
			always_show_prev_frame.set_active(self.config["always_show_prev_frame"])
			always_show_prev_frame.connect("toggled", self.always_show_prev_frame_toggled, always_show_prev_frame)
			vbox.add(always_show_prev_frame)
			always_show_prev_frame.show()

			show_prev_frame_on_new = gtk.CheckButton("Show Prev Frame on New")
			show_prev_frame_on_new.set_active(self.config["show_prev_frame_on_new"])
			show_prev_frame_on_new.connect("toggled", self.show_prev_frame_on_new_toggled, show_prev_frame_on_new)
			vbox.add(show_prev_frame_on_new)
			show_prev_frame_on_new.show()

			hbox = gtk.HBox()
			label = gtk.Label("Prev Frame Alpha")
			hbox.add(label)
			label.show()
			adj = gtk.Adjustment(
				value=self.config["prev_frame_alpha"],
				lower=0.0,
				upper=100.0,
				step_incr=1.0,
				page_incr=10.0,
			)
			prev_frame_alpha = gtk.SpinButton(adjustment=adj, climb_rate=0.5, digits=2)
			adj.connect("value_changed", self.prev_frame_alpha_changed, prev_frame_alpha)
			hbox.add(prev_frame_alpha)
			prev_frame_alpha.show()
			vbox.add(hbox)
			hbox.show()

			# for always_show_prev_frame

			# for show_prev_frame_on_new

			# for prev_frame_alpha

			# add the exit buttons
			hbox = gtk.HBox(spacing=20)
			close_btn = gtk.Button("OK")
			hbox.add(close_btn)
			close_btn.show()
			close_btn.connect("clicked", self.ok_btn_clicked)
			vbox.add(hbox)
			hbox.show()
			vbox.show()

			self.show()

		def prev_frame_alpha_changed(self, widget, spin_btn):
			self.config["prev_frame_alpha"] = spin_btn.get_value()
			save_config(self.img, self.config)

		def show_prev_frame_on_new_toggled(self, widget, check_btn):
			self.config["show_prev_frame_on_new"] = not not widget.get_active()
			save_config(self.img, self.config)

		def always_show_prev_frame_toggled(self, widget, check_btn):
			self.config["always_show_prev_frame"] = not not widget.get_active()
			save_config(self.img, self.config)

		def new_frame_copy_toggled(self, widget, check_btn):
			self.config["new_frame_copy_image_data"] = not not widget.get_active()
			save_config(self.img, self.config)

		def ok_btn_clicked(self, *args):
			save_config(self.img, self.config)
			gtk.main_quit()
	
	settings_dialog = NarlySettingsDialog(img)
	gtk.main()

register(
	"python_fu_narly_sprite_settings",	# unique name for plugin
	"Narly Sprite Settings",		# short name
	"Narly Sprite Settings",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"<Image>/Sprite/Narly Settings",	# what to call it in the menu
	"*",	# used when creating a new image (blank), else, use "*" for all existing image types
	[],	# input params,
	[],	# output params,
	narly_sprite_settings	# actual function
)
	

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

def narly_sprite_create(width, height, image_type):
	img = gimp.Image(width, height, image_type)

	narly_sprite_new_frame(img, None)

	gimp.Display(img)
	gimp.displays_flush()

register(
	"python_fu_narly_sprite_new",	# unique name for plugin
	"Narly Sprite",		# short name
	"Narly Sprite",	# long name
	COPYRIGHT1,
	COPYRIGHT2,
	COPYRIGHT_YEAR,	# copyright year
	"New Sprite",	# what to call it in the menu
	"",	# used when creating a new image (blank), else, use "*"
	[
		(PF_INT16, "width", "Width for the sprite", 64),
		(PF_INT16, "height", "Height for the sprite", 64),
		(PF_RADIO, "image_type", "Image Type", True,
			(("RGB", RGB),
			("Grayscale", GRAY),
			("Indexed", INDEXED))
		),
	],	# input params,
	[],	# output params,
	narly_sprite_create,	# actual function
	menu="<Image>/File/Create"
)

if __name__ == "__main__":
	main()
