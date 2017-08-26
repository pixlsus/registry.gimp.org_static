#!/usr/bin/env python

from gimpfu import *
import pygtk
pygtk.require('2.0')
import gtk, gobject, time

# create an output function that redirects to gimp's Error Console
def gprint( text ):
   pdb.gimp_message(text)
   return 

# See analyze_select_help.txt for program overview.

class color_analyzer:
   def __init__(self, image):
      # transmit error messages to gimp console
      pdb.gimp_message_set_handler( ERROR_CONSOLE )
      gprint('color_analyzer initializing')
      self.image = image
      # Following Analyze Mode variables are interrelated, change one
      # change all accordingly
      self.PARTIAL_EXCLUDE = 0
      self.PARTIAL_AS_FULL = 1
      self.PARTIAL_AS_PARTIAL = 2
      self.analyze_opt_vals = [self.PARTIAL_EXCLUDE, self.PARTIAL_AS_FULL,
                               self.PARTIAL_AS_PARTIAL]
      self.analyze_opt_lbls = ['Exclude', 'Full Value', 'Partial Value']
      self.reset_vars()
   def reset_vars(self):
      # These are vars that bld_results_header() or bld_results_msg()
      # expect analyze_rgn_tasklet() to have left for them.
      self.image_name = self.image.name
      self.drawable = pdb.gimp_image_get_active_drawable(self.image)
      self.drawable_name = self.drawable.name
      self.has_alpha = self.drawable.has_alpha
      self.is_rgb = self.drawable.is_rgb
      if self.is_rgb:
         self.n_chns = 3
      else:
         self.n_chns = 1
      self.num_pixels = 0
      self.does_intersect = False
      self.sel_bounds = (0,) * 4
      self.min_vals = [256] * 3
      self.max_vals = [-1] * 3
      self.mid_vals = [127.5] * 3
      self.delta_vals = [0.0] * 3
      self.thrshld_vals = [0.0] * 3
      # self.num_alpha_part = 0
      # self.num_sel_part = 0
   def analyze_rgn_tasklet(self):
      # Read pixels in current selection or whole drawable to find
      # Max & Min RGB values and also Mid values and thresholds which
      # define that color range. Builds histogram of numbers of pixels
      # which have each R, G, & B val, also a similar histogram of
      # each unique RGB combination.  Works on Greyscale too.
      # This is time consuming (I blame Python) so we must update a
      # progress bar to reassure the user.
      # Therefore routine is setup to be called as a GTK Idle Function
      # and it periodically yields control so GTK can update the dialog
      # window and the user can even quit in the middle.
      start_time = time.time()
      self.reset_vars()
      image = self.image
      drawable = self.drawable
      has_alpha = self.has_alpha
      n_chns = self.n_chns
      each_channel = range(n_chns)
      NUM_PX_SLICE = 100000  # Num pixels to process before yielding control.
      # Get boundry box of the intersection of this drawable and the image's selection area.
      # If no selection is currently specified, the intersection is considered to be the
      # whole drawable.
      self.does_intersect, sel_x1, sel_y1, sel_width, sel_height \
         = pdb.gimp_drawable_mask_intersect(drawable)
      if not self.does_intersect:
         # Mostly stopping processing but need self.sel_bounds
         # relative to drawable coords for results msg header.
         # (problem is drawable_mask_intersect() sets any
         # negative x1, y1 to 0)
         non_empty, x1, y1, x2, y2 = pdb.gimp_selection_bounds(image)
         offset_x, offset_y = pdb.gimp_drawable_offsets(drawable)
         self.sel_bounds = (x1 - offset_x, y1 - offset_y,
                            x2 - x1, y2 - y1)
      else:
         self.sel_bounds = (sel_x1, sel_y1, sel_width, sel_height)
         sel_x2 = sel_x1 + sel_width - 1
         sel_y2 = sel_y1 + sel_height - 1
         srcRgn = drawable.get_pixel_rgn(sel_x1, sel_y1,
                                         sel_width, sel_height,
                                         False, False)
         src_pixels = bytearray(srcRgn[:,:])
         bpp = srcRgn.bpp
         # To simplify code, all drawable types are ultimately handled as if
         # they have an alpha channel and a selection mask, where either is
         # not actually there we supply default values of full opacity (255)
         has_sel = not pdb.gimp_selection_is_empty(image)
         if has_sel:
            offset_x, offset_y = pdb.gimp_drawable_offsets(drawable)
            sel_mask = image.selection    #returns a channel
            selRgn = sel_mask.get_pixel_rgn(sel_x1 + offset_x,
                                            sel_y1 + offset_y,
                                            sel_width, sel_height,
                                            False, False)
            sel_pixels = bytearray(selRgn[:,:])
            s = 0
         sel, alpha = 255, 255                     # default: fully selected and opaque
         p_lim = sel_width * sel_height * bpp
         NOT_PARTIAL_EXCLUDE = self.analyze_partial_mode != self.PARTIAL_EXCLUDE
         # Setting up function ptr here saves execing IF stmnt in pixel loop
         if self.PARTIAL_AS_FULL == self.analyze_partial_mode:
            do_partial_pixel = self.do_pixel_full_val
         elif self.PARTIAL_AS_PARTIAL == self.analyze_partial_mode:
            do_partial_pixel = self.do_pixel_partial_val
         # loop thru each pixel checking if selected and adding to RGB
         # & unique color histograms.  We use an outer loop to break work
         # into slices so we can periodically return control to gtk.main
         # so progress bar can update.
         slice_size = bpp * NUM_PX_SLICE     # must be a multiple of bytes per pixel
         slc = 0
         # Show user info about drawable we're about to work on
         msg = self.bld_results_header() + '\n\nResults Pending...'
         self.result_label.set_markup('<tt>' + msg + '</tt>')
         self.progressbar.set_text('Working...')
         p = 0
         while p < p_lim:      # for each Slice of slice_size
            slc_lim = min(p + slice_size, p_lim)
            self.progressbar.set_fraction(p / float(p_lim - 1))
            # while gtk.events_pending():   # Allow progress bar to update.
            #    gtk.main_iteration()
            yield True              # Allow gtk.main to get control back
            while p < slc_lim:      # for each pixel within slice
               if has_sel:
                  sel = sel_pixels[s]
                  s += 1
               if has_alpha:
                  alpha = src_pixels[p + n_chns]
               if sel == 255 == alpha:                # pixel fully selected and opaque
                  self.num_pixels += 1
                  self.do_pixel_full_val(src_pixels[p:p+n_chns])
               elif NOT_PARTIAL_EXCLUDE and sel and alpha:
                  # pixel at least partially selected and opaque
                  self.num_pixels += 1
                  # if alpha != 255:
                  #    self.num_alpha_part += 1
                  # if sel != 255:
                  #    self.num_sel_part += 1
                  # analyze each color channel of this  RGB or Grey pixel
                  do_partial_pixel(src_pixels[p:p+n_chns], alpha, sel)
               p += bpp
         if self.num_pixels > 0: # Maybe no pixels were selected.
            # summarize results from histogram and unique colours
            for c in each_channel: # for each color channel
               self.delta_vals[c] = (self.max_vals[c] - self.min_vals[c]) / 2.0
               self.mid_vals[c] = self.min_vals[c] + self.delta_vals[c]
            # msg = 'num partial alphas found: {:d}'.format(self.num_alpha_part)
            # msg += '\nnum partial sels found: {:d}'.format(self.num_sel_part)
            # gprint(msg)
      end_time = time.time() - start_time
      mins = int(end_time)/60
      secs = end_time % 60
      self.progressbar.set_fraction(1.0)
      self.progressbar.set_text('Analysis Completed in {} min {:.2f} sec'.format(mins, secs))
      msg = self.bld_results_msg()
      self.result_label.set_markup('<tt>' + msg + '</tt>')
      yield False    # Returning False means Idle Time Function will be removed.
   def do_pixel_full_val(self, RGBorG, alpha=255, sel=255):
      # Due to use of function ptrs, must have default defs for alpha, sel
      # though they're not used here.
      # RGBorG is a bytearray at this point.
      for c, val in enumerate(RGBorG):    # for each color channel in pixel
         if val < self.min_vals[c]:
            self.min_vals[c] = val
         if self.max_vals[c] < val:
            self.max_vals[c] = val
   def do_pixel_partial_val(self, RGBorG, alpha, sel):
      # RGBorG is a bytearray at this point.
      mult = sel * alpha
      for c, val in enumerate(RGBorG):    # for each color channel in pixel
         val *= mult                      # "pro-rate" val according to what percentage it is
         val /= 65025                     # selected and opaque
         if val < self.min_vals[c]:
            self.min_vals[c] = val
         if self.max_vals[c] < val:
            self.max_vals[c] = val
   def bld_results_header(self):
      sel_bounds = self.sel_bounds
      if self.is_rgb:
         d_type = "RGB"
      else:
         d_type = "Greyscale"
      if self.has_alpha:
         alpha = " w/ Alpha"
      else:
         alpha = ' '
      COL1_W, COL2_W = 26, 15
      msg = 'Selection bounds box:'.ljust(COL1_W) + \
            'Image Name:'.ljust(COL2_W) + self.image_name
      msg += '\n' + ' X:{:7,}   Y:{:7,}'.format(
               sel_bounds[0], sel_bounds[1]).ljust(COL1_W) + \
               'Drawable Name:'.ljust(COL2_W) + self.drawable_name
      msg += '\n' + ' W:{:7,}   H:{:7,}'.format(
               sel_bounds[2], sel_bounds[3]).ljust(COL1_W) + \
               'Drawable Type:'.ljust(COL2_W) + d_type + alpha
      return msg
   def bld_results_msg(self):
      msg = self.bld_results_header()
      if not self.does_intersect:
         msg += '\n\nDrawable not within Selection Bounds.'
         return msg
      if self.num_pixels == 0:
         msg += '\n\nNo pixels were selected.'
         return msg
      each_channel = range(self.n_chns)
      if self.is_rgb:
         type_label = "RGB Vals:"
         # RGBorG_colhead = ('<u>Red</u>', '<u>Green</u>', '<u>Blue</u>')
      else:
         type_label = "Grey Vals:"
         # RGBorG_colhead = ('<u>Grey</u>',)
      COL1_W = 18
      msg += '\n\nNum pixels selected: {:,}'.format(self.num_pixels)
      msg += ('\nMin ' + type_label).ljust(COL1_W)
      for c in each_channel:
         msg += '{:8d}'.format(self.min_vals[c])
      msg += ('\nMax ' + type_label).ljust(COL1_W)
      for c in each_channel:
         msg += '{:8d}'.format(self.max_vals[c])
      msg += ('\nMid-pt ' + type_label).ljust(COL1_W + 2)
      for c in each_channel:
         msg += '{:8.1f}'.format(self.mid_vals[c])
      msg += '\nThreshold(s):'.ljust(COL1_W + 2)
      for c in each_channel:
         msg += '{:8.1f}'.format(self.delta_vals[c])
      return msg
   def build_dialog(self):
      HOMOGENEOUS = True
      NOT_HOMOGENEOUS = False
      EXPAND = True
      NOT_EXPAND = False
      FILL = True
      NOT_FILL = False
      V_SPACE = 3
      V_PAD = 3
      H_PAD = 3
      # Create a Frame to pack other containers and widgets into.
      # A frame can only directly contain one widget or other container.
      frame = gtk.Frame('RGB or Grey Analysis Results')
      vbox = gtk.VBox(NOT_HOMOGENEOUS, 0)
      frame.add(vbox)      
      # Add other child containers and widgets to define
      # the 'Analysis' section.
      self.result_label = gtk.Label('')
      self.result_label.set_selectable(True)
      self.result_label.set_text(
            'Click [Analyze] to examine whole active layer' +
            '\nor current selection')
      self.result_label.set_alignment(0, 0)   #Left just. & Vert top
      self.result_label.set_padding(H_PAD, 0)
      vbox.pack_start(self.result_label, NOT_EXPAND, NOT_FILL, V_PAD)
      # Build row of 'Analyze' option btns
      vbox.pack_start(gtk.HSeparator(), EXPAND, FILL, 0)
      hbox = gtk.HBox(NOT_HOMOGENEOUS, 0)
      vbox.pack_start(hbox, NOT_EXPAND, NOT_FILL, V_PAD)
      lbl = gtk.Label('Partially Selected/\nTransparent Areas:')
      hbox.pack_start(lbl, NOT_EXPAND, NOT_FILL, H_PAD)
      rbtn, rbtn_actv = None, None
      for val, text in zip(self.analyze_opt_vals, self.analyze_opt_lbls):
         rbtn = gtk.RadioButton(rbtn, text)
         rbtn.connect('toggled', self.a_mode_callback, val)
         hbox.pack_start(rbtn, NOT_EXPAND, NOT_FILL, H_PAD)
         if not rbtn_actv:       # first rbtn we created will be
            rbtn_actv = rbtn     # the active one.
      rbtn.set_active(True)      # get toggled msg sent out
      rbtn_actv.set_active(True) # so callback() inits vars
      # Create [Analyze] button.
      bbox = gtk.HButtonBox()
      bbox.set_layout(gtk.BUTTONBOX_END)
      bbox.set_spacing(0)
      hbox.pack_end(bbox, NOT_EXPAND, NOT_FILL, H_PAD)
      self.anlys_btn = gtk.Button('Analyze')
      self.anlys_btn.connect('clicked', self.anlys_callback, 'Analyze')
      bbox.add(self.anlys_btn)
      # Add progress bar as bottom row w/in Analysis frame.
      self.progressbar = gtk.ProgressBar()
      vbox.pack_start(self.progressbar, NOT_EXPAND, NOT_FILL, 0)
      return frame
   def anlys_callback(self, widget, data):
      # Callback for [Analyze] btn: We call our Analyze routine
      # as an Idle Time Function so progress bar will update and
      # user can quit during processing.
      analyze_tasklet = self.analyze_rgn_tasklet()
      gobject.idle_add(analyze_tasklet.next)
   def a_mode_callback(self, widget, data):
      # Callback for Analysis Partial Mode Radio btns:
      if widget.get_active():
         self.analyze_partial_mode = data

class analyzer_selector(color_analyzer):
   def __init__(self, image):
      color_analyzer.__init__(self, image)
      # Following context variables are interrelated, change one
      # change all accordingly
      self.my_context = [False] * 4
      self.context_opt_lbl = ['Feather Edges', 'Antialiasing',
                              'Select Transparent', 'Sample Merged']
      self.context_funcs = [pdb.gimp_context_set_feather,
                            pdb.gimp_context_set_antialias,
                            pdb.gimp_context_set_sample_transparent,
                            pdb.gimp_context_set_sample_merged]
      self.context_feather_idx = 0
      self.undo_wasnt_enabled = False
      self.undo_enable_ok = False
   def build_dialog(self):
      # Our parent creates widgets in a Analyze Color Range frame,
      # we add to that a dialog window we build.
      analysis_frame = color_analyzer.build_dialog(self)
      HOMOGENEOUS = True
      NOT_HOMOGENEOUS = False
      EXPAND = True
      NOT_EXPAND = False
      FILL = True
      NOT_FILL = False
      WINDOW_BORDER = 5
      V_SPACE = 3
      H_PAD = 3
      TABLE_X_SPACE = 3
      TABLE_Y_SPACE = 3
      TBL_BTN_SEC_H_SPACE = 30   # Between right of table and Btn section
      row_labels = ('Mid Vals:' ,'Thresholds:')
      color_labels = ('  Red' ,' Green', '  Blue')
      empty_labels= ('',) * 3
      spinner_labels = color_labels, empty_labels
      self.mid_val_thresh_adj_lst = []
      self.range_thresh_adj_lst = []
      thresh_adj_lst = [self.mid_val_thresh_adj_lst,
                        self.range_thresh_adj_lst]
      self.diag_window = gtk.Window(gtk.WINDOW_TOPLEVEL)
      self.diag_window.set_title('Analyze & Select by Color Range')
      self.diag_window.set_border_width(WINDOW_BORDER)
      self.diag_window.connect('destroy', self.delete_event)
      # Here we just set a handler for delete_event that immediately
      # exits GTK.
      # self.diag_window.connect('delete_event', self.delete_event)
      uber_box = gtk.VBox(NOT_HOMOGENEOUS, V_SPACE)
      self.diag_window.add(uber_box)
      uber_box.pack_start(analysis_frame, NOT_EXPAND, NOT_FILL, 0)
      # ==== Build the 'Specify Selection' section.
      tooltips = gtk.Tooltips()
      frame = gtk.Frame('Specify color range to select')
      # frame.set_label_align(0.0, 1.0)
      uber_box.pack_start(frame, NOT_EXPAND, NOT_FILL, 0)
      vbox = gtk.VBox(NOT_HOMOGENEOUS, V_SPACE)
      frame.add(vbox)
      # Build row of Selection Mode options
      hbox = gtk.HBox(NOT_HOMOGENEOUS, 0)
      vbox.pack_start(hbox, NOT_EXPAND, NOT_FILL, 0)
      lbl = gtk.Label('Selection Mode: ')
      hbox.pack_start(lbl, NOT_EXPAND, NOT_FILL, H_PAD)
      rbtn_lbl = ['Replace', 'Add', 'Subtract', 'Intersect']
      rbtn_data = [CHANNEL_OP_REPLACE, CHANNEL_OP_ADD,
                   CHANNEL_OP_SUBTRACT, CHANNEL_OP_INTERSECT]
      rbtn = None
      for c in range(4):
         rbtn = gtk.RadioButton(rbtn, rbtn_lbl[c])
         rbtn.connect('toggled', self.sel_mode_callback, rbtn_data[c])
         hbox.pack_start(rbtn, NOT_EXPAND, NOT_FILL, H_PAD)
         if c == 0:
            rbtn_actv = rbtn
      rbtn.set_active(True)      # get toggled msg sent out
      rbtn_actv.set_active(True) # so callback() inits vars
      # Build row of Selection Context Options
      hbox = gtk.HBox(NOT_HOMOGENEOUS, 0)
      vbox.pack_start(hbox, NOT_EXPAND, NOT_FILL, 0)
      context_opt_lbl = self.context_opt_lbl
      for c in range(4):
         chk_btn = gtk.CheckButton(context_opt_lbl[c])
         chk_btn.connect("toggled", self.sel_ctxt_callback, c)
         hbox.pack_start(chk_btn, NOT_EXPAND, NOT_FILL, H_PAD)
      # Build 'Feather Radius' threshold widgets
      hbox = gtk.HBox(NOT_HOMOGENEOUS, 0)
      vbox.pack_start(hbox, NOT_EXPAND, NOT_FILL, 0)
      lbl = gtk.Label('Feather Radius')
      thresh_adj = gtk.Adjustment(pdb.gimp_context_get_feather_radius()[0],
                                  0, 100, .1, 1)
      spinner = gtk.SpinButton(thresh_adj, 1, 1)
      spinner.set_wrap(True)
      spinner.set_numeric(True)
      spinner.set_snap_to_ticks(True)
      self.f_radius_thresh_adj = thresh_adj
      self.f_radius_spinner = spinner
      hbox.pack_start(spinner, NOT_EXPAND, NOT_FILL, H_PAD)
      hbox.pack_start(lbl, NOT_EXPAND, NOT_FILL, 0)
      self.f_radius_box = hbox
      # Build Table for  Mid Pt. Color and Threshold widgets
      hbox = gtk.HBox(NOT_HOMOGENEOUS, TBL_BTN_SEC_H_SPACE)
      vbox.pack_start(hbox, NOT_EXPAND, NOT_FILL, V_SPACE)
      table = gtk.Table(len(row_labels),
                        1 + (len(spinner_labels[0]) * 2),
                        NOT_HOMOGENEOUS)
      table.set_row_spacings(TABLE_Y_SPACE)
      table.set_col_spacings(TABLE_X_SPACE)
      hbox.pack_start(table, NOT_EXPAND, NOT_FILL, H_PAD)
      row = 0
      # R, G & B threshold widgets
      for r_lbl in row_labels:
         col = 0
         lbl = gtk.Label(r_lbl)
         table.attach(lbl, col, col + 1, row, row + 1,
                      gtk.FILL, gtk.FILL) # fill so justify method works
         lbl.set_alignment(0, .5)   #Left just. & Vert center
         col += 1
         for spin_lbl in spinner_labels[row]:
            if '' != spin_lbl:
               lbl = gtk.Label()
               lbl.set_markup('<tt>' + spin_lbl + '</tt>')
               table.attach(lbl, col, col + 1, row, row + 1, 0, 0)
            col += 1
            thresh_adj = gtk.Adjustment(0, 0, 255, 0.5)
            spinner = gtk.SpinButton(thresh_adj, 0.5, 1)
            spinner.set_wrap(True)
            spinner.set_numeric(True)
            spinner.set_snap_to_ticks(True)
            thresh_adj_lst[row] += [thresh_adj]
            table.attach(spinner, col, col + 1, row, row + 1, 0, 0)
            col += 1
         row += 1
      # Add [Select] btn to lower right corner of Select Frame
      vbox = gtk.VBox(NOT_HOMOGENEOUS, 0)
      hbox.pack_end(vbox, NOT_EXPAND, NOT_FILL, H_PAD)
      bbox = gtk.HButtonBox()
      bbox.set_layout(gtk.BUTTONBOX_END)
      bbox.set_spacing(0)
      vbox.pack_end(bbox, NOT_EXPAND, NOT_FILL, 0)
      btn = gtk.Button('Select')
      btn.connect('clicked', self.select_callback)
      bbox.add(btn)
      # Add [Load Vals] btn above Select] btn
      bbox = gtk.HButtonBox()
      bbox.set_layout(gtk.BUTTONBOX_END)
      bbox.set_spacing(0)
      vbox.pack_end(bbox, NOT_EXPAND, NOT_FILL, 0)
      btn = gtk.Button('Load Vals')
      btn.connect('clicked', self.load_callback)
      bbox.add(btn)
      tooltips.set_tip(btn, 'Load Mid Vals & Thresholds\nfrom Analysis')
      # Build last section, [Close] btn
      bbox = gtk.HButtonBox()
      bbox.set_layout(gtk.BUTTONBOX_END)
      bbox.set_spacing(0)
      uber_box.pack_end(bbox, NOT_EXPAND, NOT_FILL, 0)
      btn = gtk.Button('Close')
      btn.connect('clicked', self.delete_event)
      bbox.add(btn)
      self.diag_window.show_all()
      self.f_radius_box.hide()
      return uber_box
   def delete_event(self, widget, event=None, data=None):
      gtk.main_quit()
      return False
   def load_callback(self, widget):
      # Callback for [Load] btn:
      # We copy vals from Analysis into our color selection
      # threshold widgets.
      if self.is_rgb:
         for c in range(3):
            self.mid_val_thresh_adj_lst[c].set_value(self.mid_vals[c])
            self.range_thresh_adj_lst[c].set_value(self.delta_vals[c])
      else:
         for c in range(3):
            self.mid_val_thresh_adj_lst[c].set_value(self.mid_vals[0])
            self.range_thresh_adj_lst[c].set_value(self.delta_vals[0])
   def select_callback(self, widget, data=None):
      # Callback for [Select] color range btn:
      image = self.image
      drawable = pdb.gimp_image_get_active_drawable(image)
      RGB_chans = (0, 1, 2)
      color_1, color_2 = [], []
      for c in RGB_chans:
         mid_clr_real = self.mid_val_thresh_adj_lst[c].value
         threshold_real = self.range_thresh_adj_lst[c].value
         color_1 += [int(mid_clr_real - threshold_real)]   # Append to list
         color_2 += [int(mid_clr_real + threshold_real)]   # Append to list
      color_1 = tuple(color_1)
      color_2 = tuple(color_2)
      self.set_my_context()   # Set our 'feathered' etc. options
      self.try_undo_group_start(image)
      self.select_color_range(image, self.select_mode, drawable, color_1, color_2)
      self.try_undo_group_end(image)
      pdb.gimp_context_set_defaults()
      gimp.displays_flush()
   def select_color_range(self, image, operation, drawable, color_1, color_2):
      # Select composite RGB by a range between two colors in RGB space.
      # What we're aiming for a version of gimp_image_select_color()
      # that allows separate thresholds for each color channel.
      # Implementation on a pixel by pixel level thru Python would
      # run too slowly (and would be a lot of work!). So instead we
      # accomplish the task by making multiple calls to
      # gimp_image_select_color()
      # 
      # To make a color selection with different thresholds for each
      # color channel, we'll get the intersection of the separate
      # selections from R, G, & B channels.
      # We'll first have to convert our 'colors as end points' to
      # a mid point color with thresholds that we can pass to
      # gimp_image_select_color(). B/c _select_color() only accepts
      # an int val for color target, a mid pt of say 132.5 will be
      # rounded down. That means if the difference between end color
      # vals is odd, the mid point will shift down so we'll also have
      # to bump up the threshold so the max val will still be selected.
      # This of course means our min val selected will be 1 too low
      # so we'll have to subtract out a second selection for just that
      # min val.
      #
      # Note on feathering and antialias:
      # feather(sel1) intersect feather(sel1)
      # != feather( sel1 intersect sel2 )
      # so we do turn off feathering and antialiasing while
      # constructing the RGB selection and then apply them to the
      # final result.
      #
      # BTW, this routine works on grayscale drawables too.
      #
      # Convert our two end point colors into a mid pt clr and
      # thresholds to use with gimp_image_select_color()
      mid_color, min_color, thresholds = [], [], []
      # Vals from each channel will be appended to above lists.
      for c1, c2 in zip(color_1, color_2):   # Each channel
         min_c = min(int(c1), int(c2))
         delta = abs(int(c2) - int(c1))
         thresh = delta / 2            # integer div
         if delta % 2:                 # if odd (i.e .5 mid pt)
            min_c -= 1
            thresh += 1
            mid_color += [min_c + thresh]
         else:
            mid_color += [min_c + thresh]
            min_c = -1
         # will need to subtract selection for min
         min_color += [min_c]     # Append this chan
         thresholds += [thresh]
      mid_color = tuple(mid_color)
      min_color = tuple(min_color)
      antialias = pdb.gimp_context_get_antialias()
      if antialias:
         pdb.gimp_context_set_antialias(False)
      feather = pdb.gimp_context_get_feather()
      if feather:
         pdb.gimp_context_set_feather(False)
      feather_or_antialias = feather or antialias
      # Determine whether our 1st channel operation has to be a
      # REPLACE or an INTERSECT, the next two have to be INTERSECT
      # of course. Save the current selection if necessary.
      active_changed = False
      if CHANNEL_OP_INTERSECT == operation and not feather_or_antialias:
         chan_ops = [CHANNEL_OP_INTERSECT] * 3
      else:
         chan_ops = [CHANNEL_OP_REPLACE] + [CHANNEL_OP_INTERSECT] * 2
      starting_sel = None
      if CHANNEL_OP_REPLACE != operation:
         if CHANNEL_OP_INTERSECT != operation or feather_or_antialias:
            # We save the current selection if the mode is ADD or SUBTRACT
            # or if mode is INTERSECT but feather or antialias is True
            starting_sel = pdb.gimp_selection_save(image)
            active_changed = True
      # Create the intersection of the range selections for
      # each RGB chan
      sel_criteria = [SELECT_CRITERION_R,
                      SELECT_CRITERION_G,
                      SELECT_CRITERION_B]
      if pdb.gimp_drawable_is_gray(drawable):
         each_channel = range(1)
      else:
         each_channel = range(3)
      for c in each_channel:
         pdb.gimp_context_set_sample_criterion(sel_criteria[c])
         pdb.gimp_context_set_sample_threshold_int(thresholds[c])
         pdb.gimp_image_select_color(image, chan_ops[c],
                                     drawable, mid_color)
         if -1 < min_color[c]:
            pdb.gimp_context_set_sample_threshold_int(0)
            pdb.gimp_image_select_color(image, CHANNEL_OP_SUBTRACT,
                                        drawable, min_color)
      # Current selection is the intersection of each RGB chan
      # selection; so integrate that with the old selection
      # if necessary.
      if antialias:
         cur_sel = pdb.gimp_selection_save(image)
         pdb.gimp_selection_none(image)
         pdb.plug_in_antialias(image, cur_sel)
         pdb.gimp_image_select_item(image, CHANNEL_OP_REPLACE,
                                    cur_sel)
         active_changed = True
      if feather:
         radius_x, radius_y = pdb.gimp_context_get_feather_radius()
         pdb.gimp_selection_feather(image, radius_x)
      if starting_sel is not None:
         if CHANNEL_OP_ADD == operation or CHANNEL_OP_INTERSECT == operation:
            pdb.gimp_image_select_item(image, operation, starting_sel)
         elif CHANNEL_OP_SUBTRACT == operation:
            sel_rgb = pdb.gimp_selection_save(image)
            active_changed = True
            pdb.gimp_image_select_item(image, CHANNEL_OP_REPLACE,
                                       starting_sel)
            pdb.gimp_image_select_item(image, CHANNEL_OP_SUBTRACT,
                                       sel_rgb)
            pdb.gimp_image_remove_channel(image, sel_rgb)
         pdb.gimp_image_remove_channel(image, starting_sel)
      if active_changed:
         if pdb.gimp_item_is_channel(drawable):
            pdb.gimp_image_set_active_channel(image, drawable)
         else:
            pdb.gimp_image_set_active_layer(image, drawable)
      if antialias:
         pdb.gimp_context_set_antialias(True)
      if feather:
         pdb.gimp_context_set_feather(True)
   def try_undo_group_start(self, image):
      # Another plugin could have disabled Undo's
      self.undo_wasnt_enabled = not pdb.gimp_image_undo_is_enabled(image)
      if self.undo_wasnt_enabled:
         self.undo_enable_ok = pdb.gimp_image_undo_enable(image)
      else:
         self.undo_enable_ok = True
      if self.undo_enable_ok:
         pdb.gimp_image_undo_group_start(image)
   def try_undo_group_end(self, image):
      if self.undo_enable_ok:
         pdb.gimp_image_undo_group_end(image)
   def set_my_context(self):
      for f in range(4):
         self.context_funcs[f](self.my_context[f])
      if pdb.gimp_context_get_feather():
         radius = self.f_radius_thresh_adj.value
         pdb.gimp_context_set_feather_radius(radius, radius)
   def sel_mode_callback(self, widget, data):
      # Callback for Selection Mode Radio btns:
      if widget.get_active():
         self.select_mode = data
   def sel_ctxt_callback(self, widget, idx = 0):
      # Callback for Selection Context Check btns:
      self.my_context[idx] = widget.get_active()
      if idx == self.context_feather_idx:
         if self.my_context[idx]:
            self.f_radius_box.show()
         else:
            self.f_radius_box.hide()

def color_range_selector(image, drawable) :
   analyzer = analyzer_selector(image)
   analyzer.build_dialog()
   gtk.main()
   return

# This is the plugin registration function
register(
    "python_fu_analyze_select_color_range",    
    "Analyze & Select by Color Range",   
    "This tool finds Max & Min RGB vals within current selection " +
    "and then allows you to select all pixels in current layer " +
    "within that range; allows different thresholds for R, B & G",
    "Donald Myron", 
    "Donald Myron", 
    "Febuary 2014",
    "<Image>/Colors/Analyze & Select Color Range", 
    "RGB*, GRAY*", 
    [], 
    [],
    color_range_selector,
    )

main()