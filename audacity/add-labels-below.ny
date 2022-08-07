;nyquist plug-in
;version 4
;type tool
;name "Add Labels Below"
;release 3.0.2
;author "Laura Vonessen"
;copyright "Released under terms of the GNU General Public License version 2"

;; This plug-in creates a label track and moves it directly below the track
;; that currently has focus. Afterwards, sets focus back to the original track.
;; Inspired by the "Move track here" plugin: https://forum.audacityteam.org/viewtopic.php?f=69&t=105172


;; Import Audacity commands as LISP functions
(aud-import-commands)


(defun select-last ()
  ;; Select the final track and set focus on it.
  ;; Previous plug-in subtracted one from the number of
  ;; tracks, but that seemed to ignore the new track so
  ;; I changed it
  (let ((last-track (get '*project* 'tracks)))
    (aud-SelectTracks :track last-track
                      :trackcount 1
                      :mode "Set")
    (aud-do "SetTrack:Focused=1 Selected=1")))

(defun get-focus-track ()
  ;; Return index of track that has focus.
  (let ((info (aud-get-info "Tracks"))
        hasfocus)
    (dotimes (i (length info) (throw 'err "No track focus"))
      (setf hasfocus (second (assoc 'FOCUSED (nth i info))))
      (when (= hasfocus 1)
        (return (1+ i))))))

(defun move ()
  (let ((ft (get-focus-track)))
    (aud-do "NewLabelTrack:")
    (select-last)
    ;; Again, number of tracks doesn't seem to include the
    ;; new label track
    (do ((i (get '*project* 'tracks) (1- i)))
        ((<= i ft))
      (aud-do "TrackMoveUp:"))
    (aud-SelectTracks :track (- ft 1)
                      :trackcount 1
                      :mode "Set")
    (aud-do "SetTrack:Focused=1 Selected=1"))
  "")

(catch 'err (move))
