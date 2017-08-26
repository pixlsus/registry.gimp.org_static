#!/usr/bin/env python

'''
plugin-trace.py
Trace plugin for Gimp

Author:
lloyd konneker, lkk, bootch at nc.rr.com

Version:
0.1 12/2009 Initial version.

License:

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

The GNU Public License is available at
http://www.gnu.org/copyleft/gpl.html


Note, under menu Edge-Detect as the best alternative?  Artistic?

Note that there is no handling of undo.
All action is on separate image.

TBD below means To Be Done, a future enhancement

'''
from gimpfu import *
import subprocess
import os




'''
Functions that:
Transform plugin-trace.py parameters to autotrace parameters.
Each function understands relations (various aritys ie one-to-many)
of plugin-trace.py parameters to autotrace params.
Also understands the encoding by PyGimp widgets.

Plugin-trace.py simplifies the user interface of autotrace.
The simplifications were discovered by experiment.
Without the simplification, the user must do their own experiments.
Here, I target users who want a simpler user interface,
without the full power of autotrace, but also without the long learning curve for autotrace.

One of the few reasonable (but still confusing)  discussions of autotrace
parameters is by Nathan Willis at linux.com.

Returns a string for one or more args to autotrace.
Note trailing spaces important in returned arg string.

Defaults for plugin-trace.py parameters are specified at registration.
autotrace defaults many parameters that plugin-trace.py omits.
'''

def colorcountArgs(colorcountParam):
  '''
  Color count must be reduced from 1-256.
  Value 0 means 256 to autotrace.  It does NOT mean: don't reduce colors!!!.
  IE autotrace uses 8-bit color and must reduce color to that range.
  I decided not to futz with recoding 256 to zero.
  The user only sees 1-255.
  1 doesn't always make sense (outlines with 1 color?), but works.
  '''
  return "--color-count " + str(colorcountParam) + " "


def tracetypeArgs(tracetypeParam):
  '''
  User choice of type of tracing (outline and centerline).
  !!! Note that preserve-width is NOT a parameter of centerline tracing, as some docs suggest:
  --centerline --preserve-width --width-weight-factor 8.0 " # trace varying width centerline
  '''
  if tracetypeParam == 1:
    return "--centerline " # trace thin centerline
  else:
    return " "  # autotrace defaults to trace outline


def despeckleArgs(despeckleParam):
  '''
  Note autotrace enforces range 1-20 on despeckle-level, 0.0-8.0 on despeckle-tightness.
  
  Note despeckle-level is NOT a radius, but the power of 2 for the speckle size:
  1 gives some size block (say 2) and 2 gives twice as many pixels (say 4)
  and 3 gives twice again (say 8).
  See despeckle.c: despeckle() in autotrace package.
  
  !!! Most importantly, despeckle-level and despeckle-tightness are coupled.
  Level stipulates the size of feature.
  Level and tightness stipulate the color difference of features.
  Color difference to despeckle = 256/(1+ tightness*level.)
  Note below that level increases but tightness increases then decreases
  because it multiplies level.
  
  Tightness 0 means that every color difference can be despeckled,
  ie a black speckle can go to white (difference of 256).
  '''
  if despeckleParam == 0:   # Less: 2**3 = 8 pixels = 3 pixel diameter
    return "--despeckle-level 3 --despeckle-tightness 0.0 "
  elif despeckleParam == 1: # Moderate: 2**5 = 32 pixels = 6 pixel diameter
    return "--despeckle-level 5 --despeckle-tightness 2.0 "  
  else:                     # Most: 2**8 = 256 pixels = 16 pixel diameter
    return "--despeckle-level 8 --despeckle-tightness 0.0 "


def smoothingArgs(smoothingParam):
  '''
  Simplify the parameters for corners and curve fitting.
  Best explanation for corners (cusps) and curves is actually in potrace paper.
  Autotrace is similar.
  
  Always remove adjacent corners.
  I assume that NOT is rarely useful.
  '''
  params = " --remove-adjacent-corners "
  
  angles = [130, 100, 80, 40, 40]
  params = params + "--corner-threshold " + str(angles[smoothingParam]) + " "
  
  # An extreme allowance for spline distance from actual curve (error-threshold) gives very smooth splines
  # In pixels, TBD should be a percentage of resolution?
  if smoothingParam == 4:   # Smoothest
    params += "--error-threshold 32 "
  return params


'''
These are unadulterated autotrace parameters.
Experiments show that these are mostly useless or hard to use.
'''
def cornersArgs(cornerthresholdParam):
  '''
  Always remove adjacent corners.
  I assume that the alternative is only useful for rare cases.
  TBD understand the other parameters and use them if defaults are not appropriate.
  '''
  return "--corner-threshold " + str(cornerthresholdParam) + " --remove-adjacent-corners "


def fittingArgs(fittingParam):
  '''
  The args that affect fitting of splines.
  
  This is the param that determines when a fitted spline is close enough.
  A float, in pixel units, of the max orthogonal distance between fitted spline and original curve.
  TBD other parameters for this.
  '''
  return "--error-threshold " + str(fittingParam) + " "


def iterationArgs(iterationParam):
  '''
  Smoothing iterations before spline fitting.
  Smoothing is done in the pixel domain??
  '''
  return "--filter-iterations " + str(iterationParam) + " "




def plugin_main(image, drawable, colorcountParam,
     tracetypeParam, despeckleParam, smoothingParam, pathParam):  # , fittingParam, iterationParam):
  # Copy so the save operations doesn't affect the original
  tempimage = pdb.gimp_image_duplicate(image)
  if not tempimage:
    raise RuntimeError
    
  # Use temp file names from gimp, it reflects the user's choices in gimp.rc
  temppngfilename = pdb.gimp_temp_name("png")
  tempsvgfilename = pdb.gimp_temp_name("svg")
  # Save in temporary.  Note: empty user entered file name
  tempdrawable = pdb.gimp_image_get_active_drawable(tempimage)

  # !!! Note no run-mode first parameter, and user entered filename is empty string
  pdb.gimp_progress_set_text ("Saving a copy")
  pdb.gimp_file_save(tempimage, tempdrawable, temppngfilename, "")
  
  '''
  Open the temporary svg file.  
  1) to catch possible os errors early
  2) to redirect stdout of the command to this file descriptor
  '''
  try:
    outfile = open(tempsvgfilename, 'w')
  except:
    raise RuntimeError, "Could not open temporary svg file: " + tempsvgfilename
    
  '''
  Cat command string for autotrace.
  Autotrace out defaults to stdout, we later connect stdout to outfile
  (instead of using an arg to autotrace -output-file foo).
  !!! Note spaces are important, between args and before file name
  '''
  command = "/usr/bin/autotrace " \
    + colorcountArgs(colorcountParam) \
    + tracetypeArgs(tracetypeParam) \
    + despeckleArgs(despeckleParam) \
    + smoothingArgs(smoothingParam) \
    + "-output-format svg " + temppngfilename
  print command, tempsvgfilename
  
  '''
  Raw parameters, for unsimplified UI
    + iterationArgs(iterationParam) \
    + cornersArgs(cornerthresholdParam) \
    + fittingArgs(fittingParam) \
  Omitted parameters:
    background color etc.  They all have defaults in autotrace.
  '''
  
  '''
  Invoke autotrace.
  Child process proceeds independently. Then wait for its completion, so we can kill it if it hangs.
  autotrace can hang if user enters parameters too aggressive (eg many colors, fine grained tracing)
  shell=False : we don't need shell (filename globbing or redirection) but args are a sequence, not a string
  shell=True: use a string
  '''
  pdb.gimp_progress_set_text ("Tracing")
  pdb.gimp_progress_pulse()
  child = subprocess.Popen(command, shell=True, stdout=outfile)
  child.communicate() # wait for child to terminate
  
  '''
  Limitation of gimp: seems to hang on importing thousands of paths from large SVG.  See Bugzilla 604175.
  So rather than hang Gimp, preclude user from continuing if SVG file too large
  AND user wants to import paths.
  '''
  if os.path.getsize(tempsvgfilename) > 600100 and pathParam == 1:  
    # Magic number, wag for file size as of 2009, my computer, etc.
    pdb.gimp_message("Trace plugin error: SVG file has too many individual paths to import.")
    return  # Leaves files in gimp/tmp, to get cleaned later
    
  resolutionx, resolutiony = pdb.gimp_image_get_resolution(tempimage) # Same resolution as original
  
  # Reload the transformed image.
  # !!! omit the leading parameter: run_mode.  PyGimp substitutes NON-INTERACTIVE
  # filename, rawname, resolution, 0,0 =retain width, height, [0,1,2] = [No, individual, merged paths]
  # file_svg_load does its own progress, saying "Rendering", but it is slow so progress_set_text our own
  pdb.gimp_progress_set_text ("Rendering")
  try:  # Missing file is usually autotrace failure of unknown reason
    svgimage = pdb.file_svg_load(tempsvgfilename, "", resolutionx, 0,0, pathParam)
  except RuntimeError:
    pdb.gimp_message("The underlying Autotrace program failed for unknown reasons, possibly not enough resources.\
      Try reducing colors, lower quality, or more despeckling.")
  
  # TBD user options for output: image, layer, or just paths?  Low priority, user can do it manually
  # TBD user option to copy paths back to original?
  # TBD step to fill in gaps between colors?
  
  # For testing, print to console
  vectors = pdb.gimp_image_get_vectors(svgimage)
  print "File size: ", os.path.getsize(tempsvgfilename)," Paths: ", len(vectors[1])  
  
  # cleanup
  os.remove(temppngfilename)  # delete the temporary png file
  # Don't delete the svg file, user might want it???
  # os.remove(tempsvgfilename)  # delete the temporary svg file
  
  # Note the new image is dirty in Gimp and the user will be asked to save before closing,
  # but the name of the image is still the name of the tempsvgfilename and the extension is .svg
  # which Gimp does not currently support saving: the user will be asked for another extension.
  
  gimp.Display(svgimage)
  gimp.displays_flush()
  
  

        
    
register(
        "python_fu_trace",
        "Create new image with fewer colors and outline or centerline paths. Requires separate autotrace program be installed.",
        "Calls separate autotrace package to trace edges or centerlines, reducing colors.  Uses temporary files.  Often leaves gaps between colors.  A sequence of filters: despeckle, quantize, trace vectors, render vectors.",
        "Lloyd Konneker (bootch nc.rr.com)",
        "Copyright 2009 Lloyd Konneker",
        "2009",
        "<Image>/Filters/Edge-Detect/_Trace...", # menuitem with accelerator key
        "*", # image types
        [ (PF_SLIDER, "colorcountParam",    "Reduce color count to:", 4, (1, 255, 1)),
          (PF_OPTION, "tracetypeParam",    "Trace:",0,["Outlines", "Centerlines"]),
          (PF_OPTION, "despeckleParam",    "Despeckling:",1,["Speckly","Some Despeckling","More Despeckling"]),
          (PF_OPTION, "smoothingParam",    "Smoothing:",2,["Jaggiest","Jaggy","Smooth","Smoother", "Smoothest"]),
          (PF_OPTION, "pathParam",         "Create paths:",0,["None","Many, Individual","One, Merged"]),
          # (PF_SLIDER, "fittingParam",       "Fitting:", 2, (1, 255, 1)),
          # (PF_INT,    "iterationParam",    "Iterations:", 4)
          # (PF_INT,    "cornerthresholdParam",    "Corner angle threshold:", 100)
          # (PF_INT,    "despeckleParam", "Despeckle radius:", 4)
        ],
        [],
        plugin_main,
        # menu="<Image>/Filters", # really the menupath less menuitem. 
        # Enables plugin regardless of image open, passes less params
        # domain=("gimp20-python", gimp.locale_directory))
        )


main()
 

