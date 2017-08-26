; Tuckbox Generator Script for the Gimp
; copyright 2013, Stephen Brown
; v1.0 - 10OCT13 
; v1.1 - 17OCT13 added guides to cutout definition
; v1.2 - 17OCT13 added rounding to draw functions and removed antialiasing
; v1.3 - 17OCT13 changed shape of side tabs to make more flexible and fix bugs
; v1.4 - 17OCT13 added combination layer to single piece box

; This program is intended to help make tuckboxes used to store cards for board
; games. It is flexible enough to create boxes for any size cards, with or without
; sleeves. It uses the dimensions of the stack of cards to create the box geometry
; in the gimp and also creates the layers and layer masks necessary to add art
; to the box without having to resize the art to exactly the same size as the side.


;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;===============================================================================
; main function ================================================================
;===============================================================================

(define (script-fu-tuckbox inType inWidth inHeight inDepth) (let*
    ( ; define variables that are in the local scope for this entire script
        (PI 3.14159)
        (empty '()) ; the empty list
        (ppi 200) ; pixels per inch setting for image
        (brushName "tuckBrush")
        (lineWidth 1) ; px
        (brushRadius (max (/ lineWidth 2) 0.5)) ; pixels, min 0.5
        (inForeground (car (gimp-context-get-foreground)))
        (inBackground (car (gimp-context-get-background)))
    )
    
  ;-----------------------------------------------------------------------------
    
    ; a tuckbox can be thought of as a list of cutouts and a cutout can be thought
    ; of mainly as a list of faces.  This function just does something to every 
    ; item in a list and returns true when complete.
    (define (forEach itemList doThis)
        (cond
            ((null? itemList) TRUE)
            (else (and (doThis (car itemList)) (forEach (cdr itemList) doThis)))))
            
  ;-----------------------------------------------------------------------------  
             
    ; This function contains all the logic of how to draw the tuckbox in the gimp.
    ; a cutout looks like (list width height outline masks)
        ; outline looks like (list feature1 feature2 ...)
        ; masks looks like (list mask1 mask2 ...)
        ; a mask looks like (list name (list feature1 feature2 ...)))
    (define (drawCutout cutout)
        (let* ( ; define the local variables that are specific to a cutout
                (imageWidth (car cutout))
                (imageHeight (cadr cutout))
                (outline (caddr cutout))
                (masks (cadddr cutout))
                (guides (car (cdr (cdr (cdr (cdr cutout))))))
                (image (car (gimp-image-new imageWidth imageHeight RGB))) )
            
        ; this function creates a new layer in the image and returns a reference
        ; to the newly created layer
        (define (makeRGBALayer name position)
            (let ( (newlayer (car (gimp-layer-new image imageWidth imageHeight RGBA-IMAGE name 100 NORMAL))) )
                ;(print (string-append "starting makeRGBALayer..." name))
                (gimp-drawable-fill newlayer TRANSPARENT-FILL)
                (gimp-image-add-layer image newlayer position)
                (gimp-image-get-layer-by-name image name)))
        
        ; this function selects a polygon area to limit a feature between two angles,
        ; startAngle and endAngle in radians
        (define (selectAnglePolygon centerX centerY bigRadius startAngle endAngle selectMode)
            (let* ( (angleMax (/ PI 4))
                    (angleRange (- endAngle startAngle))
                    (angleSteps (ceiling (/ angleRange angleMax)))
                    (deltaAngle (/ angleRange angleSteps))
                    (vLength (* 2 (+ angleSteps 2)))
                    (vPoints (make-vector vLength)) )
                
            (define (fillVector a aMax da counter)
                (if (<= a (+ aMax 0.1))
                    (begin
                        (vector-set! vPoints (* 2 counter) (+ centerX (* bigRadius (cos a)))) ; x coordinate
                        (vector-set! vPoints (+ (* 2 counter) 1) (- centerY (* bigRadius (sin a)))) ; y coordinate                
                        (fillVector (+ a da) aMax da (+ counter 1)))))
                                
            (vector-set! vPoints 0 centerX)
            (vector-set! vPoints 1 centerY)
            ; execute loop from startAngle to endAngle in steps of deltaAngle
            (fillVector startAngle endAngle deltaAngle 1)
            (gimp-image-select-polygon image selectMode vLength vPoints)))
                    
        ; this function contains all the logic of how to actually draw the outline features
        ; precondition: outlineLayer must be created already
        ; a feature looks like: (list typesym (vector parameter1 parameter2 ...))
        (define (drawFeature feature)
            (let (  (featureType (car feature))
                    (parameters (cadr feature))  )
            
            ; draw a quarter circle given the center, the radius, and the region
            (define (drawQuarterCircle params)
                (let ( (centerX (vector-ref params 0))
                       (centerY (vector-ref params 1))
                       (radius (vector-ref params 2))
                       (region (vector-ref params 3)))
                    (cond
                        ((= region 1) (drawArc (vector centerX centerY radius radius 0 (* PI 0.5))))
                        ((= region 2) (drawArc (vector centerX centerY radius radius (* PI 0.5) PI)))
                        ((= region 3) (drawArc (vector centerX centerY radius radius PI (* PI 1.5))))
                        ((= region 4) (drawArc (vector centerX centerY radius radius (* PI 1.5) (* PI 2))))
                        (else (print "region is out of bounds in drawQuarterCircle")))))          
            
            ; draw an arc given the center, x and y radius, and start and end angles
            (define (drawArc params)
                (let ( (arcLayer (car (makeRGBALayer "Arc" 0))) 
                       (centerX (round (vector-ref params 0)))
                       (centerY (round (vector-ref params 1)))
                       (radX (round (vector-ref params 2)))
                       (radY (round (vector-ref params 3)))
                       (startAngle (vector-ref params 4))
                       (endAngle (vector-ref params 5)))
                       
                ; select an ellipse of the outer dimensions
                (gimp-image-select-ellipse image CHANNEL-OP-REPLACE 
                    (- centerX (+ radX brushRadius)) ;top left X
                    (- centerY (+ radY brushRadius)) ;top left Y 
                    (* 2 (+ radX brushRadius)) ; width 
                    (* 2 (+ radY brushRadius))) ; height
                (gimp-edit-fill arcLayer FOREGROUND-FILL)
                
                ; shrink the selection by the brush diameter and erase
                (gimp-selection-shrink image (max 1 (* 2 brushRadius)))
                (gimp-edit-clear arcLayer)

                ; select a polygon from the center through the two angles
                (selectAnglePolygon centerX centerY 
                    (* 2 (max radX radY)) ; bigRadius
                    startAngle endAngle CHANNEL-OP-REPLACE)
                (gimp-selection-invert image)
                (gimp-edit-clear arcLayer)
                
                ; merge the layer down onto the outline layer
                (gimp-image-merge-down image arcLayer CLIP-TO-BOTTOM-LAYER)
                (gimp-selection-all image))); select all    
                
            ; draw a solid line between two points
            ; precondition: brush must be set before function is called
            ; precondition: line will be drawn in the current foreground color
            ; layer is a reference to the drawable where the line will be drawn
            ; start and end are the x,y coordinates of the start and end points 
            ; in pixels relative to the origin of the layer
            (define (drawSolidLine vPoints)
                (if (= (vector-length vPoints) 4)
                    ;(gimp-paintbrush layer 0 4 vPoints PAINT-CONSTANT 0)                    
                    (gimp-pencil outlineLayer 4 vPoints)
                    (print "vPoints not length 4 in drawSolidLine")))
               
            ; draw a dashed line between two points
            ; precondition: brush must be set before function is called
            ; precondition: line will be drawn in the current foreground color
            ; layer is a reference to the drawable where the line will be drawn
            ; start is a list of the x,y coordinates of the start point in px relative to the origin of the layer
            ; end is a list of the x,y coordinates of the end point in px relative to the origin of the layer
            (define (drawDashedLine vPoints)
                (let* ( (startX (vector-ref vPoints 0))
                        (startY (vector-ref vPoints 1))
                        (endX (vector-ref vPoints 2))
                        (endY (vector-ref vPoints 3))
                        (dashCeiling 5) ; mm, upper bound for dash length
                        (lineLength (sqrt (+ (*  (- endX startX) (- endX startX))
                                             (*  (- endY startY) (- endY startY)))))
                        (lineUnit (list  (/ (- endX startX) lineLength)
                                         (/ (- endY startY) lineLength)))
                        (dashguess (ceiling (/ lineLength (* dashCeiling (/ ppi 25.4)))))
                        ; calculate the number of dash segments, must be odd
                        (dashes (if (odd? dashguess) dashguess (+ dashguess 1)))
                        (dashLength (/ lineLength dashes)) ) ; calculated dash length                        
                
                (define (drawDashes x xmax dx counter)
                    (if (< x (- xmax 0.1))
                        (begin
                            (if (even? counter) 
                                ;(drawSegment x (+ x dx)))
                                (drawSolidLine (vector ; draw segment between x and x + dx
                                    (+ startX (* x (car lineUnit))) ; startX
                                    (+ startY (* x (cadr lineUnit))); startY
                                    (+ startX (* (+ x dx) (car lineUnit))); endX
                                    (+ startY (* (+ x dx) (cadr lineUnit)))))); endY
                            (drawDashes (+ x dx) xmax dx (+ counter 1)))))
                
                ; execute loop from 0 to lineLength in steps of dashLength
                (drawDashes 0 lineLength dashLength 1)))
               
            ;logic of how to draw a feature on the image
            ; features that create a new layer and merge it down (drawArc) must
            ; reassign the outlineLayer variable after the merge because its reference changes
            (cond
                ((eq? featureType 'line) (drawSolidLine parameters))                                             
                ((eq? featureType 'dashed) (drawDashedLine parameters))
                ((eq? featureType 'fillet) (begin (drawQuarterCircle parameters) 
                    (set! outlineLayer (car (gimp-image-get-layer-by-name image "Outline")))))
                ((eq? featureType 'arc) (begin (drawArc parameters) 
                    (set! outlineLayer (car (gimp-image-get-layer-by-name image "Outline")))))
                (else (print "feature type not recognized in drawFeature")))))

        ; this function contains all the logic of how to draw a layer with a mask
        ; in the gimp so that the box art can be added easily
        (define (drawMask maskedLayer)
            (let* ( (visibleColor '(255 255 255))
                    (hiddenColor '(0 0 0))
                    (layerName (car maskedLayer))
                    (visibleRegions (cadr maskedLayer))
                    (layer (car (makeRGBALayer layerName 1)))
                    (maskDrawable (car (gimp-layer-create-mask layer ADD-BLACK-MASK))) )
                    
            (define (drawRegion region)
                (let ( (regionType (car region))
                       (parameters (cadr region)))
                
                ; draw or clear a solid rectangle defined by the top left and bottom right
                ; x and y coordinates
                (define (drawRect vPoints addOrSub)
                    (let ( (topLeftX (round (vector-ref vPoints 0)))
                           (topLeftY (round (vector-ref vPoints 1)))
                           (bottomRightX (round (vector-ref vPoints 2)))
                           (bottomRightY (round (vector-ref vPoints 3))) )
                            
                    (gimp-image-select-rectangle image CHANNEL-OP-REPLACE topLeftX topLeftY
                        (- bottomRightX topLeftX) ; width
                        (- bottomRightY topLeftY)) ; height
                    (if (= addOrSub 0)
                        (gimp-edit-fill maskDrawable BACKGROUND-FILL)
                        (gimp-edit-fill maskDrawable FOREGROUND-FILL))
                    (gimp-selection-all image))); select all
                
                ; draw an arc given the center, x and y radius, and start and end angles
                (define (drawPie params addOrSub)
                    (let ( (centerX (round (vector-ref params 0)))
                           (centerY (round (vector-ref params 1)))
                           (radX (round (vector-ref params 2)))
                           (radY (round (vector-ref params 3)))
                           (startAngle (vector-ref params 4))
                           (endAngle (vector-ref params 5)))
                           
                    (gimp-image-select-ellipse image CHANNEL-OP-REPLACE 
                        (- centerX radX) ;top left X
                        (- centerY radY) ;top left Y 
                        (* 2 radX) ; width 
                        (* 2 radY)) ; height
                    (selectAnglePolygon centerX centerY 
                        (* 2 (max radX radY)) ; bigRadius
                        startAngle endAngle CHANNEL-OP-INTERSECT)
                    (if (= addOrSub 0)
                        (gimp-edit-fill maskDrawable BACKGROUND-FILL)
                        (gimp-edit-fill maskDrawable FOREGROUND-FILL))
                    (gimp-selection-all image)))
                
                ; draw a solid quarter circle given the center, the radius, and the region
                (define (drawQuarterPie params addOrSub)
                    (let ( (centerX (vector-ref params 0))
                           (centerY (vector-ref params 1))
                           (radius (vector-ref params 2))
                           (region (vector-ref params 3)))
                    (cond
                        ((= region 1) (drawPie (vector centerX centerY radius radius 0 (* PI 0.5)) addOrSub))
                        ((= region 2) (drawPie (vector centerX centerY radius radius (* PI 0.5) PI) addOrSub))
                        ((= region 3) (drawPie (vector centerX centerY radius radius PI (* PI 1.5)) addOrSub))
                        ((= region 4) (drawPie (vector centerX centerY radius radius (* PI 1.5) (* PI 2)) addOrSub))
                        (else (print "region is out of bounds in drawQuarterPie")))))
                     
                 ; draw a filled polygon given the coordinates of the corners
                 (define (drawPolygon vPoints addOrSub)
                    (gimp-image-select-polygon image CHANNEL-OP-REPLACE (vector-length vPoints) vPoints)
                    (if (= addOrSub 0)
                        (gimp-edit-fill maskDrawable BACKGROUND-FILL)
                        (gimp-edit-fill maskDrawable FOREGROUND-FILL))
                    (gimp-selection-all image)); select all
                        
                ; this is the body of drawRegion       
                (cond
                    ((eq? regionType 'rect) (drawRect parameters 1))
                    ((eq? regionType 'rectclear) (drawRect parameters 0))
                    ((eq? regionType 'quartpie) (drawQuarterPie parameters 1))
                    ((eq? regionType 'quartpieclear) (drawQuarterPie parameters 0))
                    ((eq? regionType 'pie) (drawPie parameters 1))
                    ((eq? regionType 'pieclear) (drawPie parameters 0))
                    ((eq? regionType 'poly) (drawPolygon parameters 1))  
                    ((eq? regionType 'polyclear) (drawPolygon parameters 0))                               
                    (else (print "region type not recognized in drawRegion")))))
            
            ; this is the body of drawMask
            (print (string-append "starting drawMask for layer " layerName))
            (gimp-context-set-foreground visibleColor)
            (gimp-context-set-background hiddenColor)
            (gimp-layer-add-mask layer maskDrawable)
            (forEach visibleRegions drawRegion)))

        ; this function draws either a vertical or horizontal guide on the image
        ; at the specified position
        (define (drawGuide guide)
            (let ( (type (car guide))
                   (pos (cadr guide)) )
            (if (eq? type 'horizontal)
                (gimp-image-add-hguide image pos)
                (gimp-image-add-vguide image pos))))
                
        ; this is the logic for drawing a cutout in the gimp.  It should first draw
        ; the outline on its own layer and then it should create a new layer with a
        ; layer mask for each side that will have its own art so that the art can be
        ; added without the need to match the image size to the side exactly
        (print "starting drawCutout...")
        (define outlineLayer (car (makeRGBALayer "Outline" 0)))
        (forEach outline drawFeature) ; draw all the features of the outline
        (forEach masks drawMask) ; draw all the layer masks
        (forEach guides drawGuide) ; draw all the image guides
        (gimp-context-set-foreground inForeground)
        (gimp-image-set-resolution image ppi ppi)
        (gimp-image-clean-all image)
        (gimp-display-new image)))                         

  ;-----------------------------------------------------------------------------

    ; Create a list of cutouts to represent the tuckbox based on the type
    ; a cutout looks like (list width height outline masks)
        ; outline looks like (list feature1 feature2 ...)
        ; masks looks like (list mask1 mask2 ...)
        ; a mask looks like (list name (list feature1 feature2 ...)))
    (define (makeTuckbox type)
        (let* ( ; define the local variables that are specific to defining the tuckbox
            (ppmm (/ ppi 25.4)) ; pixels per millimeter
            (width (* inWidth ppmm))
            (height (* inHeight ppmm))
            (depth (* inDepth ppmm))
            (mediaThickness (* 0.25 ppmm)) ; px, thickness of cardstock 
            (sideTabFillet (* 2 ppmm)) ; px, rounded over corners on opposide side of fold tabs
            (sideTabLength (min (* 15 ppmm) (/ width 2))) ; px, maximum length of side folding tabs, must be shorter than width / 2
            (sideTabOffset (* 2 mediaThickness)) ; px
            ;(sideTabRadius (min (* 15 ppmm) (- depth sideTabFillet) sideTabLength))
            (foldGap (* 0.5 ppmm)) ; px, distance between glued ends and corners of box
            (foldOverlap (* 2 ppmm)) ; px
            (foldBack (* 10 ppmm)) ; px
            (foldOffset (* 1.0 ppmm)) ; px, angled clearance by folds
            (bigFoldOffset (* 4.0 ppmm)) ; px, angled clearance by folds
            (tuckOffset (* 0.25 ppmm)) ; px, angled clearance by tuck ends
            (tuckHeightMax (* 32.0 ppmm)) ; px
            (tuckLock (* 10 ppmm)) ; px
            (tuckLockAngle (* 15 (/ PI 180))) ; radians
            (tuckReinforcement (* 8 ppmm))
            (tuckHalfWidth (/ (- width (* 2 tuckOffset)) 2))
            (tuckHeight (min tuckHeightMax tuckHalfWidth))
            (wideTuckRadius tuckHeightMax)
            (thumbRadius (* 9 ppmm)) ; px, cutout radius for thumb
            (thumbDepth (* 7 ppmm)) ) ; px, cutout depth for thumb

        ; this function solves for the coordinates of the line tangent to the
        ; tuck radius for when the tuck is wide
        (define (solveWideTuck R a b)
            (let* ( (T (sqrt (- (+ (* a a) (* b b)) (* R R))))
                    (Qa (+ (/ (* a a) (* b b)) 1))
                    (Qb (/ (* -2 a R R) (* b b)))
                    (Qc (- (/ (* R R R R) (* b b)) (* R R)))
                    (x (/ (- (- 0 Qb) (sqrt (- (* Qb Qb) (* 4 Qa Qc)))) (* 2 Qa))) )    
            (vector
                x ;x
                (/ (- (* R R) (* a x)) b) ;y
                (acos (/ (- a x) T)))));theta
                
        ; this function solves for the geometry of the side tab
        (define (solveSideTab R th h)
            (let* (
                (cth (cos th))
                (sth (sin th))
                (S (/ (- h (- R (* R sth))) cth)) )
            (vector
                (* S sth) ; x
                (* S cth); y
                (+ (* S sth) (* R cth)) ; xc
                (- h R)))) ; yc

        ;.......................................................................
            
        (define (describeOnePiece)
            (let* ( (x0 brushRadius)           
                    (x1 (+ x0 depth))
                    (x2 (+ x1 width))
                    (x3 (+ x2 depth))
                    (x4 (- x1 sideTabLength))
                    (x5 (+ x2 sideTabLength))
                    (y0 brushRadius)
                    (y1 (+ y0 height))
                    (y2 (+ y1 depth mediaThickness))
                    (y3 (+ y2 height))
                    (y4 (+ y3 depth)) 
                    (y5 (+ y4 tuckHeight))
                    (y6 (+ y3 sideTabLength))
                    (wideSoln (if (< tuckHeight tuckHalfWidth) 
                        (solveWideTuck wideTuckRadius tuckHalfWidth tuckReinforcement)
                        (vector 0 0 0)))
                    (tabSoln (solveSideTab sideTabFillet tuckLockAngle (- sideTabLength (/ tuckLock 2)))) )
            
            (define outline
                (let (
                (baseOutline (list ; of features
                    (list 'line (vector x0 (+ y0 sideTabOffset) x0 (- y1 foldOffset)))
                    (list 'line (vector x0 (- y1 foldOffset) x1 y1))
                    (list 'line (vector x1 y1 x0 (+ y1 bigFoldOffset)))
                    (list 'line (vector x0 (+ y1 bigFoldOffset) x0 (- y2 foldOffset)))
                    (list 'line (vector x0 (- y2 foldOffset) x1 y2))
                    (list 'line (vector x1 y2 x0 y2))
                    (list 'line (vector x0 y2 x0 y3))
                    (list 'line (vector x1 y3 (+ x1 tuckOffset) y4))
                    (list 'line (vector (+ x1 tuckOffset) y4 (+ x1 tuckOffset tuckLock) y4))
                    (list 'line (vector (- x2 tuckOffset tuckLock) y4 (- x2 tuckOffset) y4))
                    (list 'line (vector (- x2 tuckOffset) y4 x2 y3))
                    (list 'line (vector x3 y3 x3 y2))
                    (list 'line (vector x3 y2 x2 y2))
                    (list 'line (vector x2 y2 x3 (- y2 foldOffset)))
                    (list 'line (vector x3 (- y2 foldOffset) x3 (+ y1 bigFoldOffset)))
                    (list 'line (vector x3 (+ y1 bigFoldOffset) x2 y1))
                    (list 'line (vector x2 y1 x3 (- y1 foldOffset)))
                    (list 'line (vector x3 (- y1 foldOffset) x3 (+ y0 sideTabOffset)))
                    (list 'line (vector x0 (+ y0 sideTabOffset) x1 (+ y0 sideTabOffset)))
                    (list 'line (vector x1 (+ y0 sideTabOffset) x1 y0))
                    (list 'line (vector x1 y0 x2 y0))
                    (list 'line (vector x2 y0 x2 (+ y0 sideTabOffset)))
                    (list 'line (vector x2 (+ y0 sideTabOffset) x3 (+ y0 sideTabOffset))) 
                    (list 'arc (vector (/ (+ x1 x2) 2) (- thumbDepth thumbRadius) thumbRadius thumbRadius PI (* 2 PI)))
                    (list 'dashed (vector x1 (+ y0 sideTabOffset) x1 y1))
                    (list 'dashed (vector x1 y1 x1 y2))
                    (list 'dashed (vector x1 y2 x1 (- y3 foldBack)))
                    (list 'dashed (vector x2 (+ y0 sideTabOffset) x2 y1))
                    (list 'dashed (vector x2 y1 x2 y2))
                    (list 'dashed (vector x2 y2 x2 (- y3 foldBack)))
                    (list 'dashed (vector x1 y1 x2 y1))
                    (list 'dashed (vector x1 y2 x2 y2))
                    (list 'dashed (vector x1 y3 x2 y3))
                    (list 'dashed (vector (+ x1 tuckOffset tuckLock) y4 (- x2 tuckOffset tuckLock) y4))
                    (list 'dashed (vector x0 y3 x1 y3))
                    (list 'dashed (vector x1 (- y3 (- foldBack (* 1.5 ppmm))) x2 (- y3 (- foldBack (* 1.5 ppmm)))))
                    (list 'dashed (vector x2 y3 x3 y3))))
                  
                (sideTabsOutline (let (
                        (xp (vector-ref tabSoln 0))
                        (yp (vector-ref tabSoln 1))
                        (xc (vector-ref tabSoln 2))
                        (yc (vector-ref tabSoln 3)) )
                    (list
                        ;left side
                        (list 'line (vector x0 y3 x0 (+ y3 (/ tuckLock 2))))
                        (list 'line (vector x0 (+ y3 (/ tuckLock 2)) (+ x0 xp) (+ y3 (/ tuckLock 2) yp)))
                        (list 'arc (vector (+ x0 xc) (+ y3 (/ tuckLock 2) yc) sideTabFillet sideTabFillet (+ PI tuckLockAngle) (* PI 1.5)))
                        (list 'line (vector (+ x0 xc) y6 (- x1 sideTabFillet) y6))
                        (list 'fillet (vector (- x1 sideTabFillet) (- y6 sideTabFillet) sideTabFillet 4))
                        (list 'line (vector x1 (- y6 sideTabFillet) x1 (- y3 foldBack)))
                        ;right side
                        (list 'line (vector x2 (- y3 foldBack) x2 (- y6 sideTabFillet)))
                        (list 'fillet (vector (+ x2 sideTabFillet) (- y6 sideTabFillet) sideTabFillet 3))
                        (list 'line (vector (+ x2 sideTabFillet) y6 (- x3 xc) y6))
                        (list 'arc (vector (- x3 xc) (+ y3 (/ tuckLock 2) yc) sideTabFillet sideTabFillet (* PI 1.5) (- (* 2 PI) tuckLockAngle)))
                        (list 'line (vector (- x3 xp) (+ y3 (/ tuckLock 2) yp) x3 (+ y3 (/ tuckLock 2))))
                        (list 'line (vector x3 y3 x3 (+ y3 (/ tuckLock 2)))))))

                (tuckOutline
                    (if (>= tuckHeight tuckHalfWidth)
                        ; narrow tuck
                        (list 
                            (list 'arc (vector (/ (+ x1 x2) 2) y4 tuckHeight tuckHeight PI (* 2 PI))))
                        ; wide tuck
                        (let ( (dx (vector-ref wideSoln 0))
                               (dy (vector-ref wideSoln 1))
                               (dA (vector-ref wideSoln 2))
                               (centerAngle (* PI 1.5)) )
                        (list
                            (list 'line (vector (+ x1 tuckOffset) y4 (+ x1 tuckOffset) (+ y4 tuckReinforcement)))
                            (list 'line (vector (+ x1 tuckOffset) (+ y4 tuckReinforcement) 
                                                (- (/ (+ x1 x2) 2) dx) (+ y4 dy)))
                            (list 'arc (vector (/ (+ x1 x2) 2) y4 wideTuckRadius wideTuckRadius 
                                                (- centerAngle dA) (+ centerAngle dA)))
                            (list 'line (vector (+ (/ (+ x1 x2) 2) dx) (+ y4 dy)
                                                (- x2 tuckOffset) (+ y4 tuckReinforcement)))
                            (list 'line (vector (- x2 tuckOffset) y4 (- x2 tuckOffset) (+ y4 tuckReinforcement))))))))
       
                ; this is how to make the entire outline
                (append baseOutline tuckOutline sideTabsOutline)))
            
            (define masks 
                (let (
                (tuckMask 
                    (if (>= tuckHeight tuckHalfWidth)
                        (list (list 'pie (vector (/ (+ x1 x2) 2) y4 tuckHeight tuckHeight PI (* 2 PI))))
                        ; create the mask for the wide tuck
                        (let ( (dx (vector-ref wideSoln 0))
                               (dy (vector-ref wideSoln 1))
                               (dA (vector-ref wideSoln 2))
                               (centerAngle (* PI 1.5)) )
                        (list 
                            (list 'poly (vector (+ x1 tuckOffset) y4 (+ x1 tuckOffset) (+ y4 tuckReinforcement)
                                                (- (/ (+ x1 x2) 2) dx) (+ y4 dy)
                                                (+ (/ (+ x1 x2) 2) dx) (+ y4 dy)
                                                (- x2 tuckOffset) (+ y4 tuckReinforcement) (- x2 tuckOffset) y4))
                            (list 'pie (vector (/ (+ x1 x2) 2) y4 wideTuckRadius wideTuckRadius 
                                                (- centerAngle dA) (+ centerAngle dA)))))))
                (topMask (list 
                    (list 'poly (vector x1 y3 (+ x1 tuckOffset) y4 (- x2 tuckOffset) y4 x2 y3))))
                (leftMask (let (
                        (xp (vector-ref tabSoln 0))
                        (yp (vector-ref tabSoln 1))
                        (xc (vector-ref tabSoln 2))
                        (yc (vector-ref tabSoln 3)) )
                    (list 
                        (list 'rect (vector x0 y2 x1 y3))
                        (list 'poly (vector x0 y3 x0 (+ y3 (/ tuckLock 2)) (+ x0 xp) (+ y3 (/ tuckLock 2) yp)
                                            x1 (+ y3 (/ tuckLock 2) yp) x1 y3))
                        (list 'pie (vector (+ x0 xc) (+ y3 (/ tuckLock 2) yc) 
                                    sideTabFillet sideTabFillet (+ PI tuckLockAngle) (* PI 1.5)))
                        (list 'rect (vector (+ x0 xc) (- y6 sideTabFillet) (- x1 sideTabFillet) y6))
                        (list 'quartpie (vector (- x1 sideTabFillet) (- y6 sideTabFillet) sideTabFillet 4)))))
                (rightMask (let (
                        (xp (vector-ref tabSoln 0))
                        (yp (vector-ref tabSoln 1))
                        (xc (vector-ref tabSoln 2))
                        (yc (vector-ref tabSoln 3)) )
                    (list 
                        (list 'rect (vector x2 y2 x3 y3))
                        (list 'poly (vector x2 y3 x2 (+ y3 (/ tuckLock 2) yp) (- x3 xp) (+ y3 (/ tuckLock 2) yp)
                                            x3 (+ y3 (/ tuckLock 2)) x3 y3))
                        (list 'quartpie (vector (+ x2 sideTabFillet) (- y6 sideTabFillet) sideTabFillet 3))
                        (list 'rect (vector (+ x2 sideTabFillet) (- y6 sideTabFillet) (- x3 xc) y6))
                        (list 'pie (vector (- x3 xc) (+ y3 (/ tuckLock 2) yc) sideTabFillet sideTabFillet
                                            (* PI 1.5) (- (* 2 PI) tuckLockAngle))))))
                (backMask (list 
                    (list 'rect (vector x1 y2 x2 y3))))
                (bottomMask (list 
                    (list 'rect (vector (- x1 foldOverlap) y1 (+ x2 foldOverlap) y2))))
                (frontMask (list ; of selections
                    (list 'rect (vector x1 y0 x2 y1))
                    (list 'rect (vector (- x1 foldOverlap) (+ y0 sideTabOffset) x1 y1))
                    (list 'rect (vector x2 (+ y0 sideTabOffset) (+ x2 foldOverlap) y1))
                    (list 'pieclear (vector (/ (+ x1 x2) 2) (- thumbDepth thumbRadius) 
                        thumbRadius thumbRadius PI (* 2 PI))))) )
            
                (list ; of masked layers
                    (list "Combination" (append bottomMask backMask leftMask rightMask topMask))
                    (list "Tuck" tuckMask)
                    (list "Top" topMask)
                    (list "Left" leftMask)
                    (list "Right" rightMask)
                    (list "Back" backMask)
                    (list "Bottom" bottomMask)
                    (list "Front" frontMask))))
                        
            (define guides (list
                (list 'vertical (/ (+ x1 x2) 2))
                (list 'horizontal (/ (+ y2 y3) 2))
                (list 'horizontal (/ (+ y0 y1) 2))))
                
            (print "starting describeOnePiece...")
            (list ; of cutouts
                (list ; width height outline masks guides)
                    (+ x3 brushRadius 1) ; width
                    (+ y5 brushRadius 1) ; height
                    outline masks guides))))
                    
        ;.......................................................................
            
        (define (describeTwoPiece)
        
            (define (makeCutout1)
                (let* ( (x0 brushRadius)           
                        (x1 (+ x0 depth))
                        (x2 (+ x1 width))
                        (x3 (+ x2 depth))
                        (y0 brushRadius)
                        (y1 (+ y0 tuckHeight))
                        (y2 (+ y1 depth))
                        (y3 (+ y2 height))
                        (y4 (+ y3 depth)) 
                        (y5 (- y2 sideTabLength))
                        (y6 (+ y3 sideTabLength))
                        (wideSoln (if (< tuckHeight tuckHalfWidth) 
                            (solveWideTuck wideTuckRadius tuckHalfWidth tuckReinforcement)
                            (vector 0 0 0)))
                        (tabSoln (solveSideTab sideTabFillet tuckLockAngle (- sideTabLength (/ tuckLock 2)))) )
                
                (define outline 
                    (let (
                    (baseOutline (list ; of features
                        (list 'line (vector x0 y2 x0 y3))
                        (list 'line (vector x0 y3 (+ x0 foldGap) y6))
                        (list 'line (vector (+ x0 foldOffset) y6 (- x1 bigFoldOffset) y6))
                        (list 'line (vector (- x1 bigFoldOffset) y6 x1 y3))
                        (list 'line (vector x1 y3 (+ x1 foldOffset) y4))
                        (list 'line (vector (+ x1 foldOffset) y4 (- x2 foldOffset) y4))
                        (list 'line (vector (- x2 foldOffset) y4 x2 y3))
                        (list 'line (vector x2 y3 x3 (- y3 foldOffset)))
                        (list 'line (vector x3 (- y3 foldOffset) x3 (+ y2 sideTabOffset)))
                        (list 'line (vector x3 (+ y2 sideTabOffset) x2 (+ y2 sideTabOffset)))
                        (list 'line (vector x2 y2 x2 (+ y2 foldBack)))
                        (list 'line (vector x2 y2 (- x2 tuckOffset) y1))
                        (list 'line (vector (- x2 tuckOffset) y1 (- x2 tuckOffset tuckLock) y1))
                        (list 'line (vector (+ x1 tuckOffset) y1 (+ x1 tuckOffset tuckLock) y1))
                        (list 'line (vector (+ x1 tuckOffset) y1 x1 y2))
                        (list 'dashed (vector x0 y2 x1 y2))
                        (list 'dashed (vector x0 y3 x1 y3))
                        (list 'dashed (vector x1 y2 x2 y2))
                        (list 'dashed (vector x1 y3 x2 y3))
                        (list 'dashed (vector x1 (+ y2 foldBack) x1 y3))
                        (list 'dashed (vector x2 (+ y2 foldBack) x2 y3))
                        (list 'dashed (vector x1 (+ y2 (- foldBack (* 1.5 ppmm))) x2 (+ y2 (- foldBack (* 1.5 ppmm)))))
                        (list 'dashed (vector (+ x1 tuckOffset tuckLock) y1 (- x2 tuckOffset tuckLock) y1))))
                     
                    (sideTabOutline (let (
                            (xp (vector-ref tabSoln 0))
                            (yp (vector-ref tabSoln 1))
                            (xc (vector-ref tabSoln 2))
                            (yc (vector-ref tabSoln 3)) )
                        (list
                            (list 'line (vector x0 y2 x0 (- y2 (/ tuckLock 2))))
                            (list 'line (vector x0 (- y2 (/ tuckLock 2)) (+ x0 xp) (- y2 (/ tuckLock 2) yp)))
                            (list 'arc (vector (+ x0 xc) (- y2 (/ tuckLock 2) yc) sideTabFillet sideTabFillet (* PI 0.5) (- PI tuckLockAngle)))
                            (list 'line (vector (+ x0 xc) y5 (- x1 sideTabFillet) y5))
                            (list 'fillet (vector (- x1 sideTabFillet) (+ y5 sideTabFillet) sideTabFillet 1))
                            (list 'line (vector x1 (+ y2 foldBack) x1 (+ y5 sideTabFillet))))))
                        
                    (tuckOutline
                        (if (>= tuckHeight tuckHalfWidth)
                        ; narrow tuck
                        (list 
                            (list 'arc (vector (/ (+ x1 x2) 2) y1 tuckHeight tuckHeight  0 PI)))
                        ; wide tuck
                        (let ( (dx (vector-ref wideSoln 0))
                               (dy (vector-ref wideSoln 1))
                               (dA (vector-ref wideSoln 2))
                               (centerAngle (* PI 0.5)) )
                        (list
                            (list 'line (vector (+ x1 tuckOffset) y1 (+ x1 tuckOffset) (- y1 tuckReinforcement)))
                            (list 'line (vector (+ x1 tuckOffset) (- y1 tuckReinforcement) 
                                                (- (/ (+ x1 x2) 2) dx) (- y1 dy)))
                            (list 'arc (vector (/ (+ x1 x2) 2) y1 wideTuckRadius wideTuckRadius 
                                                (- centerAngle dA) (+ centerAngle dA)))
                            (list 'line (vector (+ (/ (+ x1 x2) 2) dx) (- y1 dy)
                                                (- x2 tuckOffset) (- y1 tuckReinforcement)))
                            (list 'line (vector (- x2 tuckOffset) y1 (- x2 tuckOffset) (- y1 tuckReinforcement))))))))

                    ; this is how to make the entire outline      
                    (append baseOutline tuckOutline sideTabOutline)))
                
                (define masks
                    (let (
                    (tuckMask 
                        (if (>= tuckHeight tuckHalfWidth)
                            (list (list 'pie (vector (/ (+ x1 x2) 2) y1 tuckHeight tuckHeight 0 PI)))
                            ; create the mask for the wide tuck
                            (let ( (dx (vector-ref wideSoln 0))
                                   (dy (vector-ref wideSoln 1))
                                   (dA (vector-ref wideSoln 2))
                                   (centerAngle (* PI 0.5)) )
                            (list 
                                (list 'poly (vector (+ x1 tuckOffset) y1 (+ x1 tuckOffset) (- y1 tuckReinforcement)
                                                    (- (/ (+ x1 x2) 2) dx) (- y1 dy)
                                                    (+ (/ (+ x1 x2) 2) dx) (- y1 dy)
                                                    (- x2 tuckOffset) (- y1 tuckReinforcement) (- x2 tuckOffset) y1))
                                (list 'pie (vector (/ (+ x1 x2) 2) y1 wideTuckRadius wideTuckRadius 
                                                    (- centerAngle dA) (+ centerAngle dA)))))))
                    (topMask (list 
                        (list 'poly (vector x1 y2 (+ x1 tuckOffset) y1 (- x2 tuckOffset) y1 x2 y2))))
                    (rightMask (let (
                            (xp (vector-ref tabSoln 0))
                            (yp (vector-ref tabSoln 1))
                            (xc (vector-ref tabSoln 2))
                            (yc (vector-ref tabSoln 3)) )
                        (list 
                            (list 'rect (vector x0 y2 x1 (+ y3 foldOverlap)))
                            (list 'poly (vector x0 y2 x0 (- y2 (/ tuckLock 2)) (+ x0 xp) (- y2 (/ tuckLock 2) yp)
                                                x1 (- y2 (/ tuckLock 2) yp) x1 y2))
                            (list 'pie (vector (+ x0 xc) (- y2 (/ tuckLock 2) yc) sideTabFillet sideTabFillet
                                                (* PI 0.5) (- PI tuckLockAngle)))
                            (list 'rect (vector (+ x0 xc) y5 (- x1 sideTabFillet) (- y2 (/ tuckLock 2) yc)))
                            (list 'quartpie (vector (- x1 sideTabFillet) (+ y5 sideTabFillet) sideTabFillet 1)))))

                    (backMask (list 
                        (list 'rect (vector x1 y2 x2 y3))
                        (list 'rect (vector x2 (+ y2 sideTabOffset) (+ x2 foldOverlap) y3))
                        (list 'rect (vector x1 y3 x2 (+ y3 foldOverlap))))) )
                                                    
                    (list ; of masked layers
                        (list "Tuck" tuckMask)
                        (list "Top" topMask)
                        (list "Right" rightMask)
                        (list "Back" backMask))))
                        
                (define guides (list
                    (list 'vertical (/ (+ x1 x2) 2))
                    (list 'horizontal (/ (+ y2 y3) 2))))
                    
                (list ; width height outline masks guides)
                    (+ x3 brushRadius 1) ; width
                    (+ y4 brushRadius 1) ; height
                    outline masks guides)))
                
            (define (makeCutout2)
                (let* ( (x0 brushRadius)           
                        (x1 (+ x0 depth))
                        (x2 (+ x1 width))
                        (x3 (+ x2 depth))
                        (y0 brushRadius)
                        (y1 (+ y0 sideTabLength))
                        (y2 (+ y1 height))
                        (y3 (+ y2 sideTabLength))
                        (y4 (+ y2 depth))
                        (thumbAngle (acos (/ (- thumbRadius thumbDepth) thumbRadius)))
                        (tabSoln (solveSideTab sideTabFillet tuckLockAngle (- sideTabLength (/ tuckLock 2)))) )
                
                (define outline (let (
                    (baseOutline (list ; of features
                        (list 'line (vector x1 y1 x2 y1))
                        (list 'line (vector x2 y1 x2 (+ y1 sideTabOffset)))
                        (list 'line (vector x2 (+ y1 sideTabOffset) x3 (+ y1 sideTabOffset)))
                        (list 'arc (vector (/ (+ x1 x2) 2) (- y1 (- thumbRadius thumbDepth)) thumbRadius thumbRadius 
                                            (- (* PI 1.5) thumbAngle) (+ (* PI 1.5) thumbAngle)))
                        (list 'line (vector x0 y2 (+ x0 bigFoldOffset) y3))
                        (list 'line (vector (+ x0 bigFoldOffset) y3 (- x1 foldOffset) y3))
                        (list 'line (vector (- x1 foldOffset) y3 x1 y2))
                        (list 'line (vector x1 y2 x1 y4))
                        (list 'line (vector x1 y4 x2 y4))
                        (list 'line (vector x2 y4 x2 y2))
                        (list 'line (vector x2 y2 x3 (- y2 foldOffset)))
                        (list 'line (vector x3 (- y2 foldOffset) x3 (+ y1 sideTabOffset)))
                        (list 'dashed (vector x0 y1 x1 y1))
                        (list 'dashed (vector x0 y2 x1 y2))
                        (list 'dashed (vector x1 y1 x1 y2))
                        (list 'dashed (vector x1 y2 x2 y2))
                        (list 'dashed (vector x2 (+ y1 sideTabOffset) x2 y2))))
                        
                    (sideTabOutline (let (
                            (xp (vector-ref tabSoln 0))
                            (yp (vector-ref tabSoln 1))
                            (xc (vector-ref tabSoln 2))
                            (yc (vector-ref tabSoln 3)) )
                        (list
                            (list 'line (vector x0 (+ y0 sideTabFillet) x0 y2))
                            (list 'fillet (vector (+ x0 sideTabFillet) (+ y0 sideTabFillet) sideTabFillet 2))
                            (list 'line (vector (+ x0 sideTabFillet) y0 (- x1 xc) y0))
                            (list 'arc (vector (- x1 xc) (- y1 (/ tuckLock 2) yc) sideTabFillet sideTabFillet tuckLockAngle (* PI 0.5)))
                            (list 'line (vector (- x1 xp) (- y1 (/ tuckLock 2) yp) x1 (- y1 (/ tuckLock 2))))
                            (list 'line (vector x1 (- y1 (/ tuckLock 2)) x1 y1))))))
                
                    ; this is how to make the entire outline      
                    (append baseOutline sideTabOutline)))
       
                (define masks (let (
                        (bottomMask (list 
                            (list 'rect (vector x1 y2 x2 y4))))
                        (leftMask (let (
                                (xp (vector-ref tabSoln 0))
                                (yp (vector-ref tabSoln 1))
                                (xc (vector-ref tabSoln 2))
                                (yc (vector-ref tabSoln 3)) )
                            (list
                                (list 'rect (vector x0 y1 x1 (+ y2 foldOverlap)))
                                (list 'poly (vector x0 y1 x0 (- y1 (/ tuckLock 2) yp) (- x1 xp) (- y1 (/ tuckLock 2) yp)
                                                x1 (- y1 (/ tuckLock 2)) x1 y1))
                                (list 'quartpie (vector (+ x0 sideTabFillet) (+ y0 sideTabFillet) sideTabFillet 2))
                                (list 'rect (vector (+ x0 sideTabFillet) y0 (- x1 xc) (- y1 (/ tuckLock 2) yc)))
                                (list 'pie (vector (- x1 xc) (- y1 (/ tuckLock 2) yc) sideTabFillet sideTabFillet
                                                    tuckLockAngle (* PI 0.5))))))
                        (frontMask (list 
                            (list 'rect (vector x1 y1 x2 y2))
                            (list 'rect (vector x2 (+ y1 sideTabOffset) (+ x2 foldOverlap) y2))
                            (list 'pieclear (vector (/ (+ x1 x2) 2) (- y1 (- thumbRadius thumbDepth)) thumbRadius thumbRadius PI (* 2 PI))))))
                    
                    (list ; of masked layers
                        (list "Bottom" bottomMask)
                        (list "Left" leftMask)
                        (list "Front" frontMask))))
                    
                (define guides (list
                    (list 'vertical (/ (+ x1 x2) 2))
                    (list 'horizontal (/ (+ y1 y2) 2))))
                    
                (list ; width height outline masks guides)
                    (+ x3 brushRadius 1) ; width
                    (+ y4 brushRadius 1) ; height
                    outline masks guides)))
                
            (print "starting describeTwoPiece...")
            (list (makeCutout1) (makeCutout2)))

        ;.......................................................................
            
        ; this is the body of makeTuckBox
        (print "starting makeTuckBox...")
        (cond
            ((= type 0) (describeOnePiece))
            ((= type 1) (describeTwoPiece)))))
  
  ;-----------------------------------------------------------------------------
                
    ; create a brush that will be editable so I can adjust the line width
    ; the brush should be deleted before the script ends so that I don't pollute
    ; the system with a bunch of new brushes
    (define (prepareContext)
        (print "preparing context...")
        (define newBrush (gimp-brush-duplicate "2. Hardness 075"))
        ;(define newBrush (gimp-brush-duplicate "1. Pixel"))
        (gimp-brush-rename (car newBrush) brushName)
        (gimp-context-set-brush brushName)
        (gimp-brush-set-radius brushName brushRadius)
        ;(gimp-brush-set-hardness brushName 0.5)
        (gimp-context-set-brush-size 1.0)
        (gimp-context-set-feather FALSE)
        ;(gimp-context-set-feather-radius 0 0)
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-paint-method "gimp-paintbrush"))
                  
    (define (restoreContext)
        (print "restoring context...")
        (gimp-brush-delete brushName)
        (gimp-context-set-foreground inForeground)
        (gimp-context-set-background inBackground))

  ;-----------------------------------------------------------------------------

    ; this is the body of the main function:
    (prepareContext) ; set brush characteristics and colors
    (forEach (makeTuckbox inType) drawCutout) ;call the function to draw the tuckbox
    (restoreContext) ; clean up and restore state to before script was run
    
)) ; end of let scope and main function

;===============================================================================
; register the script with script-fu ===========================================
;===============================================================================
(script-fu-register
    "script-fu-tuckbox"                        ;func name
    "Tuck Box"                                  ;menu label
    "Creates new image(s) used to create a tuck box for a deck of\
    cards. Enter the dimensions of the inside of the box and the\
    type of box you wish to create and it will create a new image,\
    draw the outline of the box in the current foreground color\
    and create layers with masks for the sides of the box so that\
    art can be easily added."              ;description
    "Stephen Brown"                             ;author
    "copyright 2013, Stephen Brown\
     licensed under the GNU GPL"        ;copyright notice
    "September 22, 2013"                          ;date created
    ""                     ;image type that the script works on
    SF-OPTION      "Box Type"      '("One Piece" "Two Piece")   ;an option variable
    SF-VALUE       "Box Inside Width (mm)" "90.5"
    SF-VALUE       "Box Inside Height (mm)" "59.5"
    SF-VALUE       "Box Inside Depth (mm)" "18.8"
)
(script-fu-menu-register "script-fu-tuckbox" "<Image>/File/Create")
