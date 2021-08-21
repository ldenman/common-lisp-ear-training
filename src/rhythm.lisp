(in-package :ld-music)

;; A list of pairs of note to rhythm
(defun make-rhythmic-notes (notes rhythm-list)
  (pairup notes rhythm-list))

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
		    (random-element '(4 8))))))))

(defun make-measures (n)
  (if (> n 0)
      (cons (make-measure) (make-measures (- n 1)))))
