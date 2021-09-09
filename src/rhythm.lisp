(in-package :ld-music)

(defun rhythm-values (r)
    (let ((res (case r
	       (1 4)
	       (2 2)
	       (4 1)
	       (8 0.5)
	       (16 0.25))))
    res))

;; convert rhythm value (ex. 1 2 4 8 16)) to a scaled duration based on bpm. The
;; scaling and rounding so that we send positive integer time value in the notes->midi-messages fn.
(defun rhythm->duration-scaled (r bpm)
  (round (* (rhythm-values r) bpm)))

(defun beat-length (beat bpm)
  (/ 60 (/ bpm beat)))

(defun rhythm->seconds (r bpm)
  (beat-length (rhythm-values r) bpm))

(defun measure-beats (measure)
  (reduce #'+ (mapcar (lambda (beat)
			(rhythm-values beat))
		      measure)))

(defun make-measure (&optional (result '()))
  (if (= 4 (measure-beats result))
      result
      (if (> (measure-beats result) 4)
	  (make-measure (butlast result))
	  (make-measure
	   (append result
		   (list 
		    (random-element '(2 4))))))))

(defun make-measures (n)
  (if (> n 0)
      (cons (make-measure) (make-measures (- n 1)))))

;;;; Rhythmic Notes constructor
(defun make-rhythmic-notes (notes rhythm-list)
  (pairup notes rhythm-list))

;;;; Rhythmic Notes selector
(defun select-rhythm (notes/rhythms)
  (cdr notes/rhythms))
(defun select-note (notes/rhythms)
  (car notes/rhythms))

;; Transform list of rhythmic notes to midi messages
;; Output suitable for writing to midi file via MIDI lib
(defun rhythmic-notes->midi-messages (rhythmic-notes bpm)
  (let ((result '())
	(time 0))
    (dolist (item rhythmic-notes)
      (let ((rhythm (select-rhythm item))
	    (note (select-note item)))
	(push (note->midi-message note time (+ time (rhythm->duration-scaled rhythm bpm))) result)
	(incf time (rhythm->duration-scaled rhythm bpm))))
    (reverse result)))

;; Transform list of rhythmic notes to PORTMIDI midi events
(defun rhythmic-notes->pm-events (rhythmic-notes bpm &optional (*midi-channel* 0))
  (let ((result '())
	(time 0))
    (dolist (item rhythmic-notes)
      (let ((rhythm (select-rhythm item))
	    (note (select-note item)))
	(push (make-event note time (+ time (rhythm->seconds rhythm bpm)) 80) result)
	(incf time (rhythm->seconds rhythm bpm))))
    (reverse result)))
