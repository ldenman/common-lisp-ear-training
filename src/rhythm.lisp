(in-package :ld-music)

;; A list of pairs of note to rhythm
(defun make-rhythmic-notes (notes rhythm-list)
  (pairup notes rhythm-list))

;; convert rhythm value (ex. 1 2 4 8 16)) to a scaled duration based on bpm. The
;; scaling and rounding so that we send positive integer time value in the notes->midi-messages fn.
(defun rhythm->duration-scaled (r bpm)
  (let ((res (case r
	       (1 (*  4 bpm))
	       (2 (*  2 bpm))
	       (4 (*  1 bpm ))
	       (8 (*  0.5 bpm))
	       (16 (* 0.25 bpm )))))
    (round  res)))

(defun beat-length (beat bpm)
  (/ 60 (/ bpm beat)))
(defun rhythm->seconds (r bpm)
    (let ((res (case r
	       (1 4)
	       (2 2)
	       (4 1)
	       (8 0.5)
	       (16 0.25))))
    (beat-length res bpm)))

(defun rhythm-values (r)
    (let ((res (case r
	       (1 4)
	       (2 2)
	       (4 1)
	       (8 0.5)
	       (16 0.25))))
    res))
