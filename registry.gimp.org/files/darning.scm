;   Copyright 2013 Alexandr Kalenuk (akalenuk@gmail.com)
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;  You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.

(define interval (lambda (a b) 
  (if (= a b) 
    (list a)
    (append (list a) (interval (+ a 1) b)))))
    
(define (script-fu-darning image drawable)
  (let* ((bounds (gimp-selection-bounds image))
	 (has-selection (nth 0 bounds)) 
	 (x1 (nth 1 bounds)) 
	 (y1 (nth 2 bounds)) 
	 (x2 (- (nth 3 bounds) 1)) 
	 (y2 (- (nth 4 bounds) 1))
         (w (- (car (gimp-image-width image)) 1))
         (h (- (car (gimp-image-height image)) 1)))
    
    (if (= has-selection TRUE) (let* (
        (p1 (gimp-drawable-get-pixel drawable 0 0))
        (channels (car p1))
        (col_0 (nth 1 p1))
        (x1->x2 (interval x1 x2))
        (y1->y2 (interval y1 y2))
        (channels-enum (interval 0 (- channels 1))))

      (gimp-message "Darning...")
      (gimp-image-undo-disable image)
  
      (map (lambda (i) (let* (
          (col_l (nth 1 (gimp-drawable-get-pixel drawable x1 i)))
          (col_r (nth 1 (gimp-drawable-get-pixel drawable x2 i)))
          (ti (if (= y1 0) 0 (/ 1 (+ (/ (- i y1) (- y2 y1)) 0.00001))))
          (fi (if (= y2 h) 0 (/ 1 (+ (/ (- y2 i) (- y2 y1)) 0.00001)))))
        (map (lambda (j) (let* (
            (col_t (nth 1 (gimp-drawable-get-pixel drawable j y1)))
            (col_b (nth 1 (gimp-drawable-get-pixel drawable j y2)))
            (tj (if (= x1 0) 0 (/ 1 (- (/ (- j x1) (- x2 x1)) 0.00002))))
            (fj (if (= x2 w) 0 (/ 1 (- (/ (- x2 j) (- x2 x1)) 0.00002)))))
          (map (lambda (k) (let* (
              (col_l_k (aref col_l k))
              (col_r_k (aref col_r k))
              (col_t_k (aref col_t k))
              (col_b_k (aref col_b k))
              (chan_k (/ (+ (* col_l_k tj) (* col_r_k fj) (* col_t_k ti) (* col_b_k fi)) (+ tj fj ti fi))))
            (aset col_0 k chan_k)
          )) channels-enum)
          (gimp-drawable-set-pixel drawable j i channels col_0)
        )) x1->x2)
      )) y1->y2)
      (gimp-drawable-update drawable x1 y1 (+ (- x2 x1 ) 1) (+ (- y2 y1 ) 1))
      (gimp-image-undo-enable image)
      (gimp-displays-flush)
      (gimp-message "Darning done!")
    )
      (gimp-message "Selection is empty"))  
  )
)

(script-fu-register "script-fu-darning"
  _"_Darning"
  _"Darn the current selection"
  "A Kalenuk <akalenuk@gmail.com>"
  "A Kalenuk"
  "2013"
  "RGB* GRAY*"
  SF-IMAGE       "Image"          0
  SF-DRAWABLE    "Drawable"       0
)

(script-fu-menu-register "script-fu-darning"
                         "<Image>/Image")
