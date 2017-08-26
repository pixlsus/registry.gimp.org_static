;based on a script from John Laubach <laubach_john@hotmail.com>
;revisited by Rod ;http://www.gimpchat.com/viewtopic.php?f=9&t=395&p=19368#p19368



(define (gimpchat)
  (plug-in-web-browser "http://www.gimpchat.com/")
 )
 
 (define (gimptalk)
  (plug-in-web-browser "http://www.gimptalk.com/")
)

(define (flickr-gimpusers-group)
  (plug-in-web-browser "http://www.flickr.com/groups/gimpusers/discuss/")
)


(define (Meet-the-gimp)
  (plug-in-web-browser "http://meetthegimp.org/getting-the-old-shows/")
)

(define (GimpTrick)
  (plug-in-web-browser "http://www.youtube.com/user/GIMPtricks")
)

(define (CreativeCommons)
  (plug-in-web-browser "http://search.creativecommons.org/")
 )
 
(script-fu-register "gimpchat"
   _"_GimpChat"
   _"most active & friendly GIMP forum"
    "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)

(script-fu-menu-register "gimpchat"
                         "<Image>/Help/Help Forum/")


(script-fu-register "gimptalk"
   _"_GimpTalk"
   _"another friendly gimp forum"
    "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)


(script-fu-menu-register "gimptalk"
                         "<Image>/Help/Help Forum/")


(script-fu-register "flickr-gimpusers-group"
   _"_Gimpusers on Flickr"
   _"Gimpusers group on Flickr"
     "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)

(script-fu-menu-register "flickr-gimpusers-group"
                         "<Image>/Help/Help Forum/")



(script-fu-register "Meet-the-gimp"
   _"_ MeetTheGimp"
   _"Most large collection of excellent video-tutorial"
   "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)

(script-fu-menu-register "Meet-the-gimp"
                         "<Image>/Help/Video Tutorial/")
						 
(script-fu-register "GimpTrick"
   _"_GimpTrick"
   _"Excellent video-tutorial"
   "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)

(script-fu-menu-register "GimpTrick"
                         "<Image>/Help/Video Tutorial/")
						 
(script-fu-register "CreativeCommons"
   _"_Browse CreativeCommons "
   _"search images licenced as Creative Commons"
   "PhotoComix <photocomix@gmail.com>"
    "PhotoComix <photocomix@gmail.com>"
    "2011"
    ""
)

(script-fu-menu-register "CreativeCommons"
                         "<Image>/Image/")


