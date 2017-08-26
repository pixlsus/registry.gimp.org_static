#!/usr/bin/python
# -*- coding: utf-8 -*-

#-------------------------------------------------------------------------------
# Script to randomly mix cells of a rectangular selection.
# Copyright (c) 2009 Perjéssy Lóránt
# lostlont@freemail.hu

#-------------------------------------------------------------------------------
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#-------------------------------------------------------------------------------
# Version 1.0
#
# Developed and tested on:
# - openSUSE 11.1 (x86-64)
# - GIMP 2.6.2
# - Python 2.6
#
# Also tested on:
# - Slackware 12.1.0
# - GIMP 2.4.5
# - Python 2.5.2

import random
from gimpfu import *

#-------------------------------------------------------------------------------
def mix_cells(image, layer, cols, rows, new_layer):
#-------------------------------------------------------------------------------
  # 1. We must have a selection.
  if pdb.gimp_selection_is_empty(image):
    gimp.message('There is no selection on the image; please create a selection before running the plugin.')
    return False

  # 2. Start undo group.
  pdb.gimp_image_undo_group_start(image)

  # 3. New layer.
  new = layer.copy(True)
  new.name = layer.name + " [mixed]"
  image.add_layer(new, 0)

  # 4. Rectangulate, resize and save selection.
  s, x, y, r, b = pdb.gimp_selection_bounds(image)
  w = r - x
  h = b - y
  w -= w % cols
  h -= h % rows
  pdb.gimp_rect_select(image, x, y, w, h, CHANNEL_OP_REPLACE, False, 0)
  sel = pdb.gimp_selection_save(image)

  # 5. Clear selection.
  pdb.gimp_edit_clear(new_layer and new or layer)

  # 6. Create array of cell indices.
  cell_count = cols * rows
  cells = range(cell_count)
  random.shuffle(cells)

  cell_w = w / cols
  cell_h = h / rows

  # 7. Copy cells one by one.
  for i in range(cell_count):
    j = cells[i]
    old_col = i % cols           # Original cell column index.
    old_row = i / cols           # Original cell row index.
    new_col = j % cols           # Translated cell column index.
    new_row = j / cols           # Translated cell row index.
    old_x = x + old_col * cell_w # Original cell x position.
    old_y = y + old_row * cell_h # Original cell y position.
    new_x = x + new_col * cell_w # Translated cell x position.
    new_y = y + new_row * cell_h # Translated cell y position.

    # 7.a Select original cell.
    pdb.gimp_rect_select(
      image,
      old_x,
      old_y,
      cell_w,
      cell_h,
      CHANNEL_OP_REPLACE,
      False,
      0)
    # 7.b Copy cell.
    pdb.gimp_edit_named_copy(new_layer and layer or new, "mix_cell")
    # 7.c Paste cell as floating selection.
    pasted = pdb.gimp_edit_named_paste(new_layer and new or layer, "mix_cell", False)
    # 7.d Translate cell to new position.
    pasted.set_offsets(new_x, new_y)
    # 7.e Anchor floating selection.
    pdb.gimp_floating_sel_anchor(pasted)
    # 7.f Delete cell buffer.
    pdb.gimp_buffer_delete("mix_cell")

    j += 1

  if not new_layer:
    image.remove_layer(new)

  # 8. Reload selection and finish undo group.
  pdb.gimp_selection_load(sel)
  pdb.gimp_image_undo_group_end(image)

#-------------------------------------------------------------------------------
register(
  "python_fu_mix_cells",
  "Randomly mix cells of a rectangular selection",
  "Randomly mix a table consisting of the specified columns and rows of the selection.",
  "Perjéssy Lóránt",
  "(c) 2009 Perjéssy Lóránt",
  "2009",
  "<Image>/Python-Fu/Mix cells",
  "",
  [
    (PF_SPINNER, "cols", "Columns", 3, (1, 100, 1)),
    (PF_SPINNER, "rows", "Rows", 3, (1, 100, 1)),
    (PF_TOGGLE, "new_layer", "Create on new layer", True)
  ],
  [],
  mix_cells)

#-------------------------------------------------------------------------------
main()
