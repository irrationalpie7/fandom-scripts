;nyquist plug-in
;version 4
;type tool analyze
;name "Add Label Plus"
;release 3.0.2
;author "Laura Vonessen"
;copyright "Released under terms of the GNU General Public License version 2"

;; This plug-in adds a label at the specified time, with the specified text.
;; Draws heavily from the "Regular Interval Labels" plugin: https://github.com/audacity/audacity/blob/master/plug-ins/equalabel.ny

;control mode "Create label relative to" choice "Selection Start,Selection End,Project Start" "Selection Start"
;control anchor "Anchor" time "" 0 0 nil
;control anchormode "Treat anchor as" choice "Label Start,Label End" "Label Start"
;control anchorpos "Use anchor as a +/- offset" choice "Positive,Negative" "Positive"
;control duration "Duration" time "" 1 0 nil
;control text "Label text" string "" "label"

;; Import Audacity commands as LISP functions
(aud-import-commands)

(defun make-label (&aux labels)
  (let ((selstart (get '*selection* 'start))
        (seldiff (- (get '*selection* 'end) (get '*selection* 'start))))
    ;; set start and end based on endpoint
    (case anchorpos
      (1  ;Negative
        (setf anchor (- 0 anchor)))
      (t  ;Positive/no-op
        ))
    (case anchormode
      (0  ;Label Start
        (setf start anchor)
        (setf end (+ anchor duration)))
      (1  ;Label End
        (setf start (- anchor duration))
        (setf end anchor)))
    ;; shift start and end relative to correct frame of reference
    (case mode
      (1  ;Selection End
          (setf start (+ start seldiff))
          (setf end (+ end seldiff)))
      (2  ;Project Start
          (setf start (- start selstart))
          (setf end (- end selstart)))
      (t  ;Selection Start -- no action required.
          ))
    (setf negstart (- 0 selstart))
    (if (< start negstart)
      (setf start negstart))
    (if (< end negstart)
      (setf end negstart))
    (if (= start end)
      (push (list start text) labels)
      (push (list start end text) labels))))

;; Apply a function "process" to
;; the first selected track only.
(if (= (get '*track* 'index) 1)
  (make-label)
  "")