(script-fu-register "vintage-look"
	"<Image>/Script-Fu/vinage-look"

"
This script-fu for The Gimp is a attempt to simulate a vintage look.
Last version can be found at:
http://www.mmip.net/gimp-script-fu

This Script is based on the tutorial from CrazyMurdock1 (Vintage look in Gimp)
"

	"Michael Maier info[at]mmip.net >" 
	"(c) Michael Maier. This is GPL Free Software." 	
	"March 3, 2008" 
	""	
	
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
	SF-ADJUSTMENT "Cyan" '(17 0 100 1 1 0 0)
	SF-ADJUSTMENT "Magenta" '(20 0 100 1 1 0 0)
	SF-ADJUSTMENT "Yellow" '(59 0 100 1 1 0 0)
)



(define
	(vintage-look
		img	
		drw	
		VarCyan
		VarMagenta
		VarYellow
	)
	
	(let* (
		(drawable-width (car (gimp-drawable-width drw)))
		(drawable-height (car (gimp-drawable-height drw)))
		(new-image (car (gimp-image-new drawable-width drawable-height RGB)))
		(original-layer (car (gimp-layer-new new-image
                                           drawable-width drawable-height
                                           RGB-IMAGE "Original"
                                           100 NORMAL-MODE)))
		(cyan-layer 0)
		(magenta-layer 0)
		(yellow-layer 0)
		)
		
		(gimp-undo-push-group-start img)
		(gimp-image-add-layer new-image original-layer 0)
	
		;Yellow Layer
		(set! yellow-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
		(gimp-image-add-layer img yellow-layer -1)
		(gimp-drawable-set-name yellow-layer "yellow")
		(gimp-context-set-background '(251 242 163) )
		(gimp-drawable-fill yellow-layer BACKGROUND-FILL)
		(gimp-layer-set-opacity yellow-layer VarYellow)
		(gimp-layer-set-mode yellow-layer MULTIPLY-MODE)
	
		;Magenta Layer
		(set! magenta-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
		(gimp-image-add-layer img magenta-layer -1)
		(gimp-drawable-set-name magenta-layer "magenta")
		(gimp-context-set-background '(232 101 179) )
		(gimp-drawable-fill magenta-layer BACKGROUND-FILL)
		(gimp-layer-set-opacity magenta-layer VarMagenta)
		(gimp-layer-set-mode magenta-layer SCREEN-MODE)
	
		; Cyan Layer 
		(set! cyan-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
		(gimp-image-add-layer img cyan-layer -1)
		(gimp-drawable-set-name cyan-layer "cyan")
		(gimp-context-set-background '(9 73 233) )
		(gimp-drawable-fill cyan-layer BACKGROUND-FILL)
		(gimp-layer-set-opacity cyan-layer VarCyan)
		(gimp-layer-set-mode cyan-layer SCREEN-MODE)
	
		; combine layers
		(gimp-image-flatten img)
		(gimp-displays-flush)
	
		(gimp-undo-push-group-end img)
	)
)

