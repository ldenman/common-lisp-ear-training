(in-package :ld-music)

;; Scale Functions

;;;; SCALE TEMPLATES ;;;;
(defun make-scale-template (steps solfege)
  (if steps (pairup steps solfege)))

(defun chromatic-scale-template ()
  (make-scale-template '(h h h h h h h h h h h h) '(do (di ra) re (ri me) mi fa (fi se) so (si le) la (li te) ti)))
(defun major-scale-template () (make-scale-template '(w w h w w w h) '(do re mi fa so la ti) ))
(defun minor-scale-template () (make-scale-template '(w h w w h w w) '(do re me fa so le te)))
(defun dorian-scale-template () (make-scale-template '(w h w w w h w) '(do re me fa so la ti)))
(defun phrygian-scale-template () (make-scale-template '(h w w w h w) '(do ra me fa so le te)))
;;;; END SCALE TEMPLATES ;;;;

;;;; SCALE BUILDING ;;;;

;;external
(defun make-scale (scale-root &optional (template (major-scale-template)))
  (list
   (cons 'type 'scale)
   (cons 'notes (assign-relative-octaves (build-scale scale-root template)))
   (cons 'template template)))

;(setf (cdr (assoc 'notes (make-scale 'c4))) (assign-relative-octaves (attr 'notes (make-scale 'c4))))
;;internal
(defun make-scale-from-template (p1 p2 scale-template)
  (midi-notes-from-scale (note-range p1 p2 (midi-notes)) scale-template scale-template))
;;internal
(defun build-scale-up (from-note-pos pattern)
  (midi-notes-from-scale (subseq (midi-notes) from-note-pos 88) pattern pattern))
;;internal
(defun build-scale-down (from-note-pos pattern)
   (assign-solfege 
    (midi-notes-from-scale-down-helper (reverse (subseq (midi-notes) 0 (+ 1 from-note-pos)))
				      (reverse pattern)
				      (reverse pattern))
    (prepend-tail (reverse (mapcdr pattern)))))
;;internal
(defun assign-solfege (scale scale-template)
  (loop for note in scale
	for solfege in (grow scale scale-template)
	collect (progn
		  (attr= solfege 'solfege note)
		  note)))
(defun assign-relative-octaves (notes &optional (count 0))
  (if notes
      (progn 
      (if (eq (note-solfege (car notes)) 'do)
	  (progn
	   (attr= (+ 1 count) 'relative-octave (car notes))
	   (assign-relative-octaves (cdr notes) (+ 1 count)))
	  (progn
	   (attr=  count 'relative-octave (car notes))
	   (assign-relative-octaves (cdr notes) count)))
      notes)))


;;internal
(defun midi-notes-from-scale (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (let ((newnote (make-note (note-name (car midi-notes)) (note-value (car midi-notes)) (cdr (car scale)))))
	(if (eq 'w (car (car scale)))
	    (cons newnote (midi-notes-from-scale (cdr (cdr midi-notes)) original-scale (cdr scale)))
	    (cons newnote (midi-notes-from-scale (cdr midi-notes) original-scale (cdr scale)))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale midi-notes original-scale original-scale))))

;;internal
(defun midi-notes-from-scale-down-helper (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (if (eq 'h (car (car scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr midi-notes) original-scale (cdr scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr (cdr midi-notes)) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale-down-helper midi-notes original-scale original-scale))))

;;internal
(defun build-scale (start-note pattern)
  (let ((pos (note-name-position start-note)))
    (append (reverse (build-scale-down pos pattern))
	    (rest (build-scale-up pos pattern)))))
;;;; END SCALE BUILDING ;;;;

;; external
(defun scale-notes (scale)
  (_notes scale))

;; external
(defun note-range (n1 n2 notes)
  (let* ((eqfn (lambda (note other-note) (eq note (note-attr other-note 'name))))
	 (p1 (position n1 notes :test eqfn ))
	 (p2 (position n2 notes :test eqfn)))
    (subseq notes p1 (+ 1 p2))))

;; external - deprecating
(defun random-scale (template)
  (let* ((letters '(A B C D E F G C# D# F# G# A#))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template (intern (format nil "~A~d" random-letter 3))
			      (intern (format nil "~A~d" random-letter 4))
			      template)))
;; external
(defun random-scale2 (template &optional range)
  (let* ((letters '(A B C D E F G C# D# F# G# A#))
	 (random-letter (nth (random (length letters)) letters))
	 (scale (make-scale (intern (format nil "~A~d" random-letter 4)) template)))
    (if range
	(scale-range3 scale (intern (format nil "~A~d" random-letter (car range))) (intern (format nil "~A~d" random-letter (cdr range))))
	scale)))

(defun random-major-scale () (random-scale (major-scale-template)))
(defun random-major-scale2 () (random-scale2 (major-scale-template)))
(defun random-chromatic-scale () (random-scale2 (chromatic-scale-template )))

;; external
(defun scale-range (p1 p2 scale-data)
  (let ((newscale (make-scale p1)))
    (attr= (note-range p1 p2 (attr 'notes scale-data))
	   'notes
	   newscale)
    newscale))
(defun scale-range3 (scale-data p1 p2)
  (let ((newscale (make-scale p1)))
    (attr= (note-range p1 p2 (attr 'notes scale-data))
	   'notes
	   newscale)
    newscale))

;; external
(defun with-scale-helper (scale my-fn)
  (funcall my-fn scale))
(defmacro with-scale (scale &body body)
  `(with-scale-helper ,scale (lambda (*current-scale*) ,@body )))

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y scale) (loop for x from 1 to y collect (random-note scale)))

;;;; Octave Functions ;;;;
(defun scale-octave-range-helper (o1 o2 notes)
  (if notes
      (if (and (>= (note-relative-octave (car notes)) o1)
	       (<=  (note-relative-octave (car notes)) o2))
	  (cons (car notes)
		(scale-octave-range-helper o1 o2 (cdr notes)))
	  (scale-octave-range-helper o1 o2 (cdr notes)))))

(defun scale-octave-range (o1 o2 notes)
  (if notes
      (if (note-solfege-equalp (car notes) 'do)
	  (scale-octave-range-helper o1 o2 notes)
	  (scale-octave-range o1 o2 (cdr notes)))))

(defun scale-octave-range2 (o1 o2 notes)
  (if notes
      (if (note-solfege-equalp (car notes) 'do)
	  (scale-octave-range-helper o1 o2 notes)
	  (scale-octave-range o1 o2 (cdr notes)))))
