;nyquist plug-in
;version 4
;type tool
;name "Extend selection"
;release 3.0.2
;author "Laura Vonessen"
;copyright "Released under terms of the GNU General Public License version 2"

;; This plug-in sets the selection from either the focused or first currently
;; selected track, plus the next track(s) until N tracks are selected.

;control mode "Select relative to" choice "Selected,Focused"
;control numtracks "Number of tracks to select:" int-text "" 2 1 nil

;; Import Audacity commands as LISP functions
(aud-import-commands)

(defun get-focus-track ()
  ;; Return index of track that has focus.
  (let ((info (aud-get-info "Tracks")) hasfocus)
    (dotimes (i (length info) (throw 'err "No track focus"))
      (setf hasfocus (second (assoc 'FOCUSED (nth i info))))
      (when (= hasfocus 1)
        (return i)))))

(defun get-first-selected-track ()
  ;; Return index of the first selected track.
  (let ((info (aud-get-info "Tracks")) isselected)
    (dotimes (i (length info) (throw 'err "No track selected"))
      (setf isselected (second (assoc 'SELECTED (nth i info))))
      (when (= isselected 1)
        (return i)))))

(defun select-next ()
  ;; mode is 0 for selected, 1 for focused
  (let ((trackindex (if (= mode 0) (get-first-selected-track) (get-focus-track))))
    (aud-SelectTracks :track trackindex
                      :trackcount numtracks
                      :mode "Set")
    (aud-do "SetTrack:Focused=1 Selected=1"))
  "")

(catch 'err (select-next))
