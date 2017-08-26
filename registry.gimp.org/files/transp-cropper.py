#!/usr/bin/env python

#       Gimp 2 plugin to crop an image to remove all transparency.
#  
#       Copyright (C) 2005 J R Hunt  
# 
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
# 
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
# 
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



#       Crop an image to remove all transparency. Typically this is used after
#       rotating or correcting the perspective of an image, but it can be used
#       on any layer where the opaque pixels form a single convex region with no
#       holes.
#         
#       The script includes optimisation routines to find the best size and
#       positioning for the cropping rectangle.  The aspect ratio can be specified
#       in advance, or can be optimised to yield the rectangle with the largest
#       possible area.
#         
#       This script requires SciPy: http://www.scipy.org/ . (For Mandriva this is
#       available as a contributed package.),

#       This script was tested with Gimp 2.2.8 in Mandriva 2005 LE with
#       SciPy 0.3.2.

#       This program was made by John Hunt in the hope it will be useful.
#       email: jrh72 A T cantab D O T net


#   Change history:
#
#   2005-12-26  v1.0    JRH                                      Written
#   2009-01-02  v1.1    Darrell Enns <darrell@darrellenns.com>   Updated for Gimp 2.6



from __future__ import division
import string
from math import floor, sqrt
from gimpfu import *
from gimpenums import *
from scipy.optimize import fmin, fminbound
import sys

ASPECT_PRESERVE = 0
ASPECT_ANY = 1
ASPECT_FIX = 2

ASPECT_MAXFUN = 25
RECT_MAXFUN = 100
progress = 0

#####################################################################    

def python_transcrop(timg, tdrawable, aspectType, aspectX, aspectY, optimise, crop):

    # set the environment we need                        
    #   nothing yet
    pdb.gimp_image_undo_group_start(timg)
    try:
        python_transcrop_body(timg, tdrawable, aspectType, aspectX, aspectY, optimise, crop)
    finally:
        pdb.gimp_image_undo_group_end(timg)
        # restore the environment
        #   nothing yet


#####################################################################
   
def python_transcrop_body(timg, tdrawable, aspectType, aspectX, aspectY, optimise, crop):

    global progress
    
    # check and calculate params
    if aspectType == ASPECT_PRESERVE:
        tan = timg.height / timg.width
    elif aspectType == ASPECT_ANY:
        optimise = True
        tan = 1.0 
    else:   # ASPECT_FIX
        if aspectX <= 0 or aspectY <= 0:
            pdb.gimp_message("Width and Height must be positive numbers")
            return
        tan = aspectY / aspectX

    if timg.active_layer == None:
        pdb.gimp_message("No active layer")
        return
        

    # find the bounds and centre for the opaque region
    selectChan, x1, y1, x2, y2 = findBounds(timg)
    x0 = floor((x1 + x2) * 0.5)
    y0 = floor((y1 + y2) * 0.5)
    print 'initial (x, y) = (%g, %g), tan = %g' % (x0, y0, tan)

    # Optimise the size and position, and optionally the aspect ratio
    if optimise:       

        if aspectType == ASPECT_ANY:
            # optimise the rectangle's aspect ratio over range 0.25 to 4.0
            msg = "Optimising aspect ratio..."
            print msg
            gimp.progress_init(msg)
            progress = 0
            tan = fminbound(objFunction1, 0.25, 4.0, (selectChan, x0, y0), maxfun=ASPECT_MAXFUN, xtol=1e-3, disp=1)
            print 'optimised aspect = %g' % (tan)

        msg = "Optimising size and position..."
        print msg
        gimp.progress_init(msg)
        progress = 0
        x0, y0, dx, dy = optMaxRect(selectChan, x0, y0, tan, True)

    else:    
        # just find the rectangle at current centre
        dx, dy = maxRectAtPos(selectChan, x0, y0, tan)

    print 'final (x, y) = (%g, %g), (width, height) = (%g, %g), tan = %g, height/width = %g' % (x0, y0, dx * 2, dy * 2, tan, dy/dx)
        
    # make selection of the rectangle
    pdb.gimp_rect_select(timg, x0-dx, y0-dy, dx * 2, dy * 2, CHANNEL_OP_REPLACE, 0, 0)

    # tidy up
    pdb.gimp_image_remove_channel(timg, selectChan)

    # crop?
    if crop:
        pdb.gimp_selection_none(timg)
        pdb.gimp_image_crop(timg, dx * 2, dy * 2, x0-dx, y0-dy)
        pdb.gimp_image_flatten(timg)

    
    
#####################################################################

# Optimise size and position of rectangle at fixed aspect ratio

def optMaxRect(selectChan, x0, y0, tan, showProgress = False):
    scale = max(selectChan.height, selectChan.width)
    opt = fmin(objFunction2, [x0/scale, y0/scale], (selectChan, tan, scale, showProgress), maxfun = RECT_MAXFUN, disp=showProgress)
    x, y = floor(opt[0] * scale), floor(opt[1] * scale)
    dx, dy = maxRectAtPos(selectChan, x, y, tan)
    return x, y, dx, dy
    
#####################################################################

# Objective function for optimising aspect ratio (tan).
# At each aspect ratio we run a sub-optimisation to get the optimum size and
# position for the rectangle. This results in a relatively robust optimisation
# of both aspect and size/position. The apparently simpler option of optimising
# aspect and position as a 3-variable optimisation does not work, because
# it gets stuck at a local minimum caused by the discrete nature of the
# underlying system.
# Returns minus area of rectangle

def objFunction1(tan, selectChan, x0, y0):
    global progress
    
    x, y, dx, dy = optMaxRect(selectChan, x0, y0, tan)
    
    progress += 1
    gimp.progress_update(progress / ASPECT_MAXFUN)

    print 'objFunction1 n=%d, tan=%g, area=%g' % (progress, tan, dx * dy * 4)

    return - dx * dy * 4

#####################################################################

# Objective function for optimising position of rectangle at fixed aspect ratio (tan).
# Returns minus area of rectangle

def objFunction2(p, selectChan, tan, scale, showProgress):
    global progress
    
    dx, dy = maxRectAtPos(selectChan, p[0]*scale, p[1]*scale, tan)

    if showProgress:
        progress += 1
        gimp.progress_update(progress / RECT_MAXFUN) 
        print 'objFunction2 n=%d, (x, y) = (%g, %g), area = %g' % (progress, p[0]*scale, p[1]*scale, dx * dy * 4)
        
    return - dx * dy * 4

#####################################################################

# Find max rectangle that can be drawn centred at (x0, y0) with aspect ratio tan.
# Returns half width and height of rectangle

def maxRectAtPos(selectChan, x0, y0, tan):
    xsign = [1, 1, -1, -1]
    ysign = [1, -1, 1, -1]
    r = None

    cos = 1 / sqrt(1 + tan * tan)
    sin = cos * tan

    x0 = floor(x0)
    y0 = floor(y0)
    
    if not isOpaque(selectChan, x0, y0):
        return 0, 0

    for i in range(4):
        x, y = findEdge(selectChan, x0, y0, cos * xsign[i], sin * ysign[i])
        thisr = sqrt((x-x0)**2 + (y-y0)**2)
        
        if r == None or thisr < r:
            r = thisr
            dx = abs(x - x0)
            dy = abs(y - y0)

    return dx, dy   # integers because findEdge returned integers

#####################################################################

# Find edge opaque pixel starting at (x0, y0) in direction (cos, sin)
# Use cos, sin not tan to avoid ambiguity in direction

def findEdge(selectChan, x0, y0, cos, sin):

    # cannot have both cos and sin zero
    assert cos != 0 or sin != 0, 'cos and sin cannot both be zero'

    # check that start point is visible 
    assert isOpaque(selectChan, x0, y0), 'pixel at (%g, %g) is not opaque' % (x0, y0)

    # choose a large radius to search over, the diagonal is always enough
    radius = sqrt(selectChan.width **2 + selectChan.height **2)

    # check that end point is not opaque
    assert not isOpaqueByRadialPos(selectChan, x0, y0, cos, sin, 1.0, radius)[0], 'pixel at end point is opaque'

    # do binary search for edge point. 
    
    lo = 0.0
    hi = 1.0
    lox, loy = coords(x0, y0, cos, sin, lo, radius)
    hix, hiy = coords(x0, y0, cos, sin, hi, radius)
    n = 20
    while max(abs(hix-lox), abs(hiy-loy)) >= 0.4 and n > 0:
        mid = (lo + hi) * 0.5
        isOpaquePixel, midx, midy = isOpaqueByRadialPos(selectChan, x0, y0, cos, sin, mid, radius)

        if isOpaquePixel:
            lo, lox, loy = mid, midx, midy
        else:
            hi, hix, hiy = mid, midx, midy
        
        n -= 1
        if n==0: print 'binary search failed to converge'
            
    # Rounding shifts right x to just outside the visible are, this is consistent
    # with the bounding box being just outside the right of a selection. Similarly for
    # bottom y.
    lox = round(lox)
    loy = round(loy)
    
    return lox, loy
    
#####################################################################

# Is point at specified radial position opaque?
# 0 <= r <= 1

def isOpaqueByRadialPos(selectChan, x0, y0, cos, sin, r, radius):
    assert r >=0 and r <= 1, 'r not in range'
    x, y = coords(x0, y0, cos, sin, r, radius)
    return isOpaque(selectChan, x, y), x, y

#####################################################################

# x, y coords of radial point

def coords(x0, y0, cos, sin, r, radius):
    # does not round results, as that stops findEdge from converging
    return x0 + r * radius * cos, y0 + r * radius * sin
    
#####################################################################

# Is point at specified x,y opaque?

def isOpaque(selectChan, x, y):
    
    if x < 0 or x >= selectChan.width or y < 0 or y >= selectChan.height:
        return False

    num, pixel = pdb.gimp_drawable_get_pixel(selectChan, x, y)
    assert num==1, 'mask does not have 1 channel'

    return pixel[0] != 0

#####################################################################

# Find the bounding rectangle for the opaque region of the active layer

def findBounds(timg):
    current_layer = timg.active_layer
    assert current_layer != None, 'current_layer is null'

    # duplicate the layer and set to image size
    new_layer = pdb.gimp_layer_copy(current_layer, True)
    assert new_layer != None, 'new_layer is null'
    pdb.gimp_image_add_layer(timg, new_layer, -1)
    pdb.gimp_layer_resize_to_image_size(new_layer)
    pdb.gimp_drawable_set_visible(new_layer, False)

    # don't preserve transp
    pdb.gimp_layer_set_preserve_trans(new_layer, False)

    # apply the layer mask if any
    if pdb.gimp_layer_get_mask(new_layer) > -1:
        pdb.gimp_layer_remove_mask(new_layer, MASK_APPLY)

    # threshold alpha - all pixels now either fully transparent or fully opaque
    pdb.plug_in_threshold_alpha(timg, new_layer, 254)

    # make a selection from the alpha
    pdb.gimp_selection_layer_alpha(new_layer)

    # find the bounding box for the selection
    hasSel, x1, y1, x2, y2 = pdb.gimp_selection_bounds(timg)
    assert hasSel != 0, 'no selection for bounding box'

    # save the selection, we will need it again
    selectChan = pdb.gimp_selection_save(timg)
    
    # tidy up
    pdb.gimp_selection_none(timg)
    pdb.gimp_image_remove_layer(timg, new_layer)

    return selectChan, x1, y1, x2, y2

    
#####################################################################    


register(
        "python_fu_transcrop",
        "Crop an image to remove all transparency, typically use after rotate or perspective.",
        "Crop an image to remove all transparency. Typically this is used after rotating or correcting the perspective of an image, but "
            "it can be used on any layer where the opaque pixels form a single convex region with no holes.\n\n"
            "The script includes optimisation routines to find the best size and positioning for the cropping rectangle.  The aspect ratio "
            "can be specified in advance, or can be optimised to yield the rectangle with the largest possible area.\n\n"
            "This script requires SciPy: http://www.scipy.org/ . (For Mandriva this is available as a contributed package.)",
        "John Hunt <jrh72 A T cantab D O T net>",
        "John Hunt",
        "2005",
        "<Image>/Python-Fu/Selection/Transparency Crop",
        "",
        [
        (PF_RADIO, "aspectType", "Aspect Ratio", ASPECT_PRESERVE,
                  (("As image", ASPECT_PRESERVE), ("From values below", ASPECT_FIX), ("Any (always optimises, slow)", ASPECT_ANY))),
        (PF_FLOAT, "aspectX", "Aspect Width", 4),
        (PF_FLOAT, "aspectY", "Aspect Height", 3),
        (PF_TOGGLE, "optimise", "Optimise size and postion", FALSE),
        (PF_TOGGLE, "crop", "Crop and flatten", FALSE),
        ],
        [],
        python_transcrop)

#####################################################################    

main()

#####################################################################    
