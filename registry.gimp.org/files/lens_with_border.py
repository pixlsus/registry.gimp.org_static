#!/usr/bin/env python

from gimpfu import *

def lens_with_border(image, zoom, shadow_color, shadow_size, shadow_opaqe, shadow_x, shadow_y, target_x, target_y, rand, randfarbe):
	# make undo group (so undo can done with 1 click)
	pdb.gimp_undo_push_group_start(image)
	# push context (prevent changing actuall settings)
	pdb.gimp_context_push()
	
	width = image.width
	height = image.height
	zx = zoom
	zy = zoom
	border = 20
	
	# calc new image size, keep ratio, use x, or y if is given
	if target_y == 0:
		f = (target_x / width)
		pic_y = height * f
		pic_x = target_x
	else:
		f = (target_y / height)
		pic_x = width * f
		pic_y = target_y

	# get the current selection
	selection = pdb.gimp_image_get_selection(image)
	# get actuall layer
	layer = pdb.gimp_image_get_active_layer(image)
	
	# copy layer for (contains lens now)
	pdb.gimp_edit_copy(layer)
	# delete selection (so other steps dont get disturbed)
	pdb.gimp_selection_none(image)
	
	# resize the image
	pdb.gimp_image_scale(image, pic_x, pic_y)

	# paste the lens as new layer (is not scaled so has higher resolution than the image)
	float = pdb.gimp_edit_paste(layer, TRUE)
	pdb.gimp_floating_sel_to_layer(float)
	# scale lens to desired size
	pdb.gimp_layer_scale(float, zx, zy, TRUE)
	# fit lens layer to size
	pdb.gimp_layer_resize(float, zx + border * 2, zy + border * 2, border, border)
	# set layer name to "lens"
	pdb.gimp_item_set_name(float, "lens")
	
	# set lens position so we can make a selection for "drop shadow"
	# (maybe there is a better way do to this)
	float.set_offsets(0, 0)
	offx = border
	offy = border
	
	# make selection for drop_shadow
	pdb.gimp_image_select_ellipse(image, 2, offx, offy, zx, zy)
	# drop shadow
	pdb.script_fu_drop_shadow(image, float, shadow_x, shadow_y, shadow_size, shadow_color, shadow_opaqe, FALSE)
	# get all layers (shadow, lens, image)
	layers = image.layers
	# combine shadow and lens
	lupe = pdb.gimp_image_merge_down(image, layers[0], 0)
	# select image for next step
	drw = pdb.gimp_image_active_drawable(image)

	# get brush
	num_brushes, brush_list = pdb.gimp_brushes_list("lensborder")
	# if it not exists, generate it
	if num_brushes <> 1:
		copy_name = pdb.gimp_brush_duplicate("2. Hardness 100")
		actual_name = pdb.gimp_brush_rename(copy_name, "lensborder")
	# set brush size
	pdb.gimp_brush_set_radius("lensborder", rand)
	# make our brush active
	pdb.gimp_context_set_brush("lensborder")
	# set brush color
	pdb.gimp_context_set_foreground(randfarbe)
	# paint border
	pdb.gimp_edit_stroke(drw)
	
	# restore settings and finish undo group
	pdb.gimp_context_pop()
	pdb.gimp_undo_push_group_end(image)

register(
	"python_lens_with_border",
	"Lens with border",
	"Make (by user) a circle selection, the selection will zoomed and get a border + dropshadow, image is resized. (Creates a brush 'lensborder')",
	"iconberg",
	"iconberg",
	"2014/01",
	"Lens with border",
	"*",
	[
		(PF_IMAGE, "image", "Image", None),
		(PF_FLOAT, "zoom", "Final Lens size", 190),
		(PF_COLOR, "shadow_color", "Shadow color:", (0, 0, 0)),
		(PF_INT, "shadow_size", "Shadow size:", 12),
		(PF_INT, "shadow_opaqe", "Shadow opaque:", 20),
		(PF_INT, "shadow_x", "Shadow offset x:", 12),
		(PF_INT, "shadow_y", "Shadow offset y:", 12),
		(PF_FLOAT, "target_x", "Image resize x:", 600),
		(PF_FLOAT, "target_y", "Image resize y:", 0),
		(PF_FLOAT, "rand", "Border size:", 6),
		(PF_COLOR, "randfarbe", "Border size color:", (198, 198, 198))
	],
	[],
	lens_with_border, menu="<Image>/Filters/Distorts")
main()