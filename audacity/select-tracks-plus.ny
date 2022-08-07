;nyquist plug-in
;version 4
;type tool
;name "Select Tracks Plus"
;release 3.0.2
;author "Laura Vonessen"
;copyright "Released under terms of the GNU General Public License version 2"

;; This plug-in expands the included "Select Tracks" Scriptable to provide more relative options.

;control text "Note: Negative indices count back from end of project/selection"
;control Track "Track index" int-text "" 0 nil nil
;control TrackMode "Select track relative to" choice "Project tracks,Selection tracks" "Project tracks"
;control text "Note: A negative number of tracks will *end* the selection with the track"
;control text "index specified above. The number of tracks will be truncated towards 0"
;control TrackCount "Number of tracks" int-text "" -100 nil nil
;control CountMode "Number of tracks" choice "Number,Percent of selected tracks,Percent of project tracks" "Number"
;control Mode "Mode" choice "Set,Add,Remove" "Set"

;; Import Audacity commands as LISP functions
(aud-import-commands)

(defun format-t (&rest msg)
  (setf (nth 1 msg) (strcat (nth 1 msg) "~%"))
  (apply 'format msg))

(defun get-all-track-indices (&aux track-indices)
  ;; Returns list of indices of project tracks
  (let ((num-tracks (length (aud-get-info "Tracks"))))
    (dotimes (i num-tracks) (push i track-indices)))
  ; This is a load-bearing sort! DO NOT REMOVE
  (sort track-indices '<))

(defun get-selected-track-indices (&aux track-indices)
  ;; Returns list of indices of selected tracks
  (let ((info (aud-get-info "Tracks")))
    (dotimes (i (length info))
      (if (= (second (assoc 'SELECTED (nth i info))) 1)
        (push i track-indices))))
  ; This is possibly also a load-bearing sort! DO NOT REMOVE
  (sort track-indices '<))

(defun do-selection (indices mode)
  ;; for each index in indices, select that track with mode
  (dolist (i indices)
    (format-t T "Selecting track ~a with mode ~a" i mode)
    (aud-SelectTracks :track i
                      :trackcount 1
                      :mode mode)))

(defun truncate-window (start end tracks &aux truncated-tracks)
  (setf start (max start 0))
  (setf end (min end (length tracks)))
  (setf count (- end start))
  (dotimes (i count)
    (push (nth (+ i start) tracks) truncated-tracks))
  truncated-tracks)

(defun select-tracks ()
  ;; Select tracks
  ; Convert track count from percent, if relevant
  (case CountMode
    (1  ; Percent of selected tracks
      (setf track-count (truncate (/ (* TrackCount (length (get-selected-track-indices))) 100))))
    (2  ; Percent of project tracks
      (setf track-count (truncate (/ (* TrackCount (length (get-all-track-indices))) 100))))
    (t  ; Number/no-op
      (setf track-count (truncate TrackCount))))
  (format-t T "Initial track count after converting percents/truncating: ~a" track-count)
  ; Get set of relevant track indices
  (case TrackMode
    (1  ; Selection tracks
      (setf track-indices (get-selected-track-indices)))
    (t  ; Project tracks
      (setf track-indices (get-all-track-indices))))
  (format-t T "Initial num indices: ~a" (length track-indices))
  ; Compute track window.
  (if (>= Track 0)
    (setf track Track)
    (setf track (+ (length track-indices) Track)))
  (format-t T "Initial track (inclusive): ~a" track)
  (if (>= track-count 0)
    (psetq track-start track track-end (+ track track-count))
    ; since we want to include track, add 1 to shift the window up.
    (psetq track-start (+ 1 track track-count) track-end (+ 1 track)))
  (format-t T "Initial track window: (~a, ~a)" track-start track-end)
  ; Slice the list of relevant indices, ignoring any tracks outside that range.
  (setf track-indices (truncate-window track-start track-end track-indices))
  (format-t T "Num indices after truncating window: ~a" (length track-indices))
  (if (> (length track-indices) 0)
    (format-t T "First index after truncating: ~a" (nth 0 track-indices)))
  ; All right, at this point, track-indices is the canonical list of tracks to ... do something to.
  (case Mode ;Set,Add,Remove
    (1  ;Add
      (do-selection track-indices "Add"))
    (2  ;Remove
      (do-selection track-indices "Remove"))
    (t  ;Set
      (format-t T "Clearing selected tracks...")
      (aud-SelectTracks :trackcount 0 :mode "Set")
      (do-selection track-indices "Add"))))

(catch 'err (select-tracks))