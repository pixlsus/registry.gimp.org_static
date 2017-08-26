;LSE v2.1r1
;
;LSE (Lightsaber effect) - lightsaber effect creation script;
;
;Version history:
;==================================================================
;ver. 0.3 (September 2009)
; - working script without layer merging;
;==================================================================
;ver. 0.6 (October 2009)
; - add layers merging and achieved final result;
;==================================================================
;ver. 0.8 (November 2009)
; - sabsize variable extended to 350 (for very big photos);
;==================================================================
;ver. 0.9 (November 2009)
; - first public release;
;==================================================================
;ver 1.0 (December 2009)
; - "sun" mode;
; - script rebuild and optimization;
; - better glow quality by addition gauss blur;
; - support of undo/rebo;
;==================================================================
;vet 1.0r1 (February 2010)
; - optioanal undo/rebo support.
; - bugfix for final layers merging.
;==================================================================
;ver. 1.0r2 (September 30th 2010)
; - using a separate image for undo stack support.
;==================================================================
;ver. 2.0 (April 18th 2011)
; - faster core (experimental);
; - separate colors for glow core and main glow;
; - color profiles;
;==================================================================
;ver. 2.1 (November 27th 2011)
; - new core now enabled by default;
; - relative settings for glow size;
; - optimization and code cleanup;
;==================================================================
;ver. 2.1r1 (August 1st 2012)
; - much simplier structure of color profiles;
; - GIMP 2.8 native version;
;==================================================================

;Color profile list
(define lse-presets
  (list
    (list
      "User colors"
      (quote 'nil)
    )
    (list
      "Core color => Glow color" 
      (quote (set! sabColor coreColor))
    )
    (list
      "Glow color => Core color"
      (quote (set! coreColor sabColor))
    )
    (list
      "Classic blue"
      (quote (begin (set! coreColor '(42 170 255)) (set! sabColor '(42 156 255))))
    )
    (list
      "Classic green"
      (quote (begin (set! coreColor '(30 255 252)) (set! sabColor '(30 255 35))))
    )
    (list
      "Classic red"
      (quote (begin (set! coreColor '(255 41 102)) (set! sabColor '(255 41 41))))
    )
    (list
      "New blue"
      (quote (begin (set! coreColor '(30 173 255)) (set! sabColor '(30 120 255))))
    )
    (list
      "New green"
      (quote (begin (set! coreColor '(152 255 30)) (set! sabColor '(41 255 30))))
    )
    (list
      "New red"
      (quote (begin (set! coreColor '(255 95 70)) (set! sabColor '(255 30 30))))
    )
    (list
      "Purple"
      (quote (begin (set! coreColor '(210 30 255)) (set! sabColor '(184 30 255))))
    )
    (list 
      "Orange"
      (quote (begin (set! coreColor '(255 170 35)) (set! sabColor '(255 144 35))))
    )
    (list
      "Silver"
      (quote (begin (set! coreColor '(159 172 195)) (set! sabColor coreColor)))
    )
    (list
      "Emerald"
      (quote (begin (set! coreColor '(120 174 94)) (set! sabColor '(94 174 96))))
    )
  )
)

;lse-get-presets
;Extract list of color profiles
;Has no arguments
;RETURNS:
;LIST - list with names of profiles;
(define (lse-get-presets)
  (define presets-source lse-presets)
  (define presets-dlist '())
  (while (not (null? presets-source))
    (define preset (car presets-source))
    (set! presets-dlist (append presets-dlist (list (car preset))))
    (set! presets-source (cdr presets-source))
  )
  presets-dlist
)

;script-fu-lse
;Main function
;LIST OF ARGUMENTS:
;IMAGE - processed image;
;LAYER - processed layer;
;BOOLEAN - new core activation flag;
;INTEGER - number of color profile;
;FLOAT - core glow size;
;FLOAT - main glow size;
;COLOR - core glow color;
;COLOR - main glow color;
;BOOLEAN - "sky" mode swich;
(define (script-fu-lse image layer core_new color_preset coreSize sabSize coreColor sabColor sky_switch)

  (define used-preset)

  (if (= (car (gimp-drawable-has-alpha layer)) FALSE)
    (begin
      (gimp-message "Selected layer doesn't contain alpha channel.\nChoose the other one.")
      (quit)
    )
  )

  (let* (
	(imh (car (gimp-image-height image)))
	(imw (car (gimp-image-width image)))
	(b_side (if (> imh imw) imh imw))
	(sep-image (car (gimp-image-new imw imh 0)))
	(r_blade)
	(r_over)
	(rel_c_size (* (/ b_side 100) coreSize))
	(rel_b_size (* (/ b_side 100) sabSize))
	(current_preset)
	)

	(gimp-image-undo-group-start image)
	(gimp-image-undo-disable sep-image)

	;Profiles processing
	(set! current_preset (list-ref lse-presets color_preset))
	(eval (cadr current_preset))
	(set! used-preset (car current_preset))

	(if (= core_new FALSE)
	  (set! r_blade (lse-oldcore sep-image layer rel_c_size rel_b_size coreColor sabColor TRUE))
	  (set! r_blade (lse-newcore sep-image layer rel_c_size rel_b_size coreColor sabColor TRUE))
	)

	(set! r_blade (car (gimp-layer-new-from-drawable r_blade image)))
	(gimp-image-insert-layer image r_blade -1 -1)
	(gimp-item-set-name r_blade "LSE Saber")
	(gimp-layer-set-mode r_blade 4)
	(gimp-brightness-contrast r_blade 0 25)
	(set! r_over 
	  (car
	    (gimp-layer-copy r_blade TRUE)
	  )
	)
	(gimp-image-insert-layer image r_over -1 -1)
	(gimp-item-set-name r_over "LSE Overlay")

	(if (= sky_switch TRUE)
	  (begin 
	    (gimp-layer-set-mode r_over 0)
	    (gimp-layer-set-opacity r_over 23)
	  )
	  (gimp-layer-set-mode r_over 5)
	)
	(gimp-image-undo-group-end image)
	(gimp-image-delete sep-image)

	;Refresh image
	(gimp-displays-flush)
  )
)

(script-fu-register
"script-fu-lse"
"<Image>/Filters/RSS/L_SE"
"Light saber creation from blade layer"
"Nepochatov Stanislav"
"Free license"
"June 17th 2011"
"RGBA"
SF-IMAGE	"Image"					0
SF-DRAWABLE	"Layer"					0
SF-TOGGLE	"Use new core"				TRUE
SF-OPTION	"Set color profile"			(lse-get-presets)
SF-ADJUSTMENT	"Core glow size (%)"			'(4 0.2 25 1 3 1 0)
SF-ADJUSTMENT	"Main glow size (%)"			'(15 0.8 50 5 10 1 0)
SF-COLOR		"Core color"				'(80 150 255)
SF-COLOR		"Main color"				'(30 78 255)
SF-TOGGLE	"\"Sky\" mode"				FALSE
)

;lse-oldcore
;Old core procedure
;LIST OF ARGUMENTS:
;IMAGE - processed image;
;LAYER - processed layer;
;FLOAT - core glow size;
;FLOAT - main glow size;
;COLOR - core glow color;
;COLOR - main glow color;
;BOOLEAN - additional blur switch;
(define (lse-oldcore input_image layer coreSize sabSize coreColor sabColor pass)

  ;Declaring variables
  (let* (
	(off_count 4)
	(blade)
	(big_glow)
	(big_glow_2)
	(big_glow_3)
	(soft_blade)
	(blade_glow)
	(blade_glow_2)
	(big_size)
	)

	;Begin process

	(set! blade (car (gimp-layer-new-from-drawable layer input_image)))
	(gimp-item-set-visible layer FALSE)
	(gimp-image-insert-layer input_image blade -1 -1)
	(gimp-item-set-name blade "Blade_FX")
	(set! soft_blade 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-insert-layer input_image soft_blade -1 -1)
	(gimp-item-set-name soft_blade "Soft_Blade")
	(plug-in-gauss-rle2 1 input_image soft_blade sabSize sabSize)
	(set! big_glow 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-insert-layer input_image big_glow -1 -1)
	(gimp-item-set-name big_glow "Big_Glow")
	(plug-in-colorify 1 input_image big_glow sabColor)
	(set! blade_glow 
	  (car 
	    (gimp-layer-copy soft_blade TRUE)
	  )
	)
	(set! big_size (* 3 sabSize))
	(plug-in-gauss-rle2 1 input_image big_glow big_size big_size)
	(set! big_glow_2 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(set! big_glow_3 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(gimp-image-insert-layer input_image big_glow_2 -1 -1)
	(gimp-image-insert-layer input_image big_glow_3 -1 -1)
	(gimp-item-set-name big_glow_2 "Big_Glow_2")
	(gimp-item-set-name big_glow_3 "Big_Glow_3")
	(gimp-image-insert-layer input_image blade_glow -1 -1)
	(gimp-item-set-name blade_glow "Blade_Glow")
	(plug-in-colorify 1 input_image blade_glow coreColor)
	(set! blade_glow_2 
	  (car 
	    (gimp-layer-copy blade_glow TRUE)
	  )
	)
	(gimp-image-insert-layer input_image blade_glow_2 -1 -1)
	(gimp-item-set-name blade_glow_2 "Blade_Glow_2")
	(while (>= off_count 0)
	  (gimp-image-raise-item input_image soft_blade)
	  (gimp-image-raise-item input_image blade)
	  (set! off_count (- off_count 1))
	)
	(plug-in-gauss-rle2 1 input_image blade coreSize coreSize)

	;Layers final merge
	(set! blade 
	  (car 
	    (gimp-image-merge-down input_image soft_blade 0)
	  )
	)
	(set! blade_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow_2 0)
	  )
	)
	(set! big_glow_2 
	  (car 
	    (gimp-image-merge-down input_image big_glow_3 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image big_glow_2 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow 0)
	  )
	)
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)
	(set! blade (car (gimp-image-merge-visible-layers input_image 0)))
	
	;Returning layer
	blade
  )
)

;lse-newcore
;New core procedure
;LIST OF ARGUMENTS:
;IMAGE - processed image;
;LAYER - processed layer;
;FLOAT - core glow size;
;FLOAT - main glow size;
;COLOR - core glow color;
;COLOR - main glow color;
;BOOLEAN - additional blur switch;
(define (lse-newcore input_image layer coreSize sabSize coreColor sabColor pass)
  (let* (
	(input_fore (car (gimp-context-get-foreground)))
	(input_back (car (gimp-context-get-background)))
	(imh (car (gimp-image-height input_image)))
	(imw (car (gimp-image-width input_image)))
	(orig_layer (car (gimp-layer-new-from-drawable layer input_image)))
	(blade)
	(big_glow)
	(soft_blade)
	(blade_glow)
	(blade_buffer)
	)

	(gimp-item-set-visible layer FALSE)

	(gimp-image-insert-layer input_image orig_layer -1 -1)
	(gimp-image-select-item input_image 0 orig_layer)
	(set! blade_buffer (car (gimp-selection-save input_image)))
	(gimp-image-remove-layer input_image orig_layer)
	(set! blade (car (gimp-layer-new input_image imw imh 1 "Blade_FX" 100 0)))
	(gimp-image-insert-layer input_image blade -1 -1)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-selection-feather input_image coreSize)
	(gimp-edit-fill blade 3)
	(gimp-edit-fill blade 0)
	(gimp-selection-none input_image)
	(gimp-image-select-item input_image 0 blade_buffer)
	(set! soft_blade (car (gimp-layer-new input_image imw imh 1 "Soft_Blade" 100 0)))
	(gimp-image-insert-layer input_image soft_blade -1 -1)
	(gimp-selection-feather input_image sabSize)
	(gimp-edit-fill soft_blade 3)
	(gimp-edit-fill soft_blade 0)
	(gimp-selection-none input_image)
	(set! big_glow (car (gimp-layer-new input_image imw imh 1 "Big_Glow" 100 0)))
	(gimp-image-insert-layer input_image big_glow -1 -1)
	(gimp-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image (* sabSize 3))
	(gimp-context-set-foreground sabColor)
	(gimp-context-set-background sabColor)
	(gimp-edit-fill big_glow 3)
	(gimp-edit-fill big_glow 0)
	(set! blade_glow (car (gimp-layer-new input_image imw imh 1 "Blade_Glow" 100 0)))
	(gimp-image-insert-layer input_image blade_glow -1 -1)
	(gimp-selection-none input_image)
	(gimp-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image sabSize)
	(gimp-context-set-foreground coreColor)
	(gimp-context-set-background coreColor)
	(gimp-edit-fill blade_glow 3)
	(gimp-edit-fill blade_glow 0)
	(gimp-selection-none input_image)
	(gimp-item-set-visible blade FALSE)
	(gimp-item-set-visible soft_blade FALSE)

	(set! big_glow (car (gimp-image-merge-visible-layers input_image 0)))
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)

	(gimp-item-set-visible blade TRUE)
	(gimp-item-set-visible soft_blade TRUE)
	(gimp-layer-set-opacity soft_blade 65)
	(gimp-image-raise-item input_image soft_blade)
	(gimp-image-raise-item input_image blade)
	(set! blade (car (gimp-image-merge-down input_image blade 0)))
	(set! blade (car (gimp-image-merge-down input_image soft_blade 0)))
	(gimp-context-set-background input_back)
	(gimp-context-set-foreground input_fore)

	;Returning layer
	blade
  )
)