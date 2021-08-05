(in-package :ld-music)

;; Scale Functions
(defun make-scale-template (steps solfege)
  (if steps (pairup steps solfege)))

(defun chromatic-scale-template ()
  (make-scale-template '(h h h h h h h h h h h h) '(do (di ra) re (ri me) mi fa (fi se) so (si le) la (li te) ti do)))
(defun major-scale-template () (make-scale-template '(w w h w w w h) '(do re mi fa so la ti) ))
(defun minor-scale-template () (make-scale-template '(w h w w h w w) '(do re me fa so le te)))
(defun dorian-scale-template () (make-scale-template '(w h w w w h w) '(do re me fa so la ti)))
(defun phrygian-scale-template () (make-scale-template '(h w w w h w) '(do ra me fa so le te)))

(defun scale-range (n1 n2 scale)
  (let* ((eqfn (lambda (note other-note) (eq note (note-attr other-note 'name))))
	 (p1 (position n1 scale :test eqfn ))
	 (p2 (position n2 scale :test eqfn)))
    (subseq scale p1 (+ 1 p2))))

(defun make-scale-from-template (p1 p2 scale-template)
  (midi-notes-from-scale (scale-range p1 p2 (midi-notes)) scale-template scale-template))

(defun build-scale-up (from-note-pos pattern)
  (midi-notes-from-scale (subseq (midi-notes) from-note-pos 88) pattern pattern))

(defun build-scale-down (from-note-pos pattern)
  (assign-solfege
   (midi-notes-from-scale-down-helper (reverse (subseq (midi-notes) 0 (+ 1 from-note-pos)))
				      (reverse pattern)
				      (reverse pattern))
   (prepend-tail (reverse (mapcdr pattern)))))

(defun assign-solfege (scale scale-template)
  (loop for note in scale
	for solfege in (grow scale scale-template)
	collect (progn
		  (attr= solfege 'solfege note)
		  note)))

(defun build-scale (start-note pattern &optional (notes (midi-notes)))
  (let ((pos (note-name-position start-note)))
    (append (reverse (build-scale-down pos pattern))
	    (rest (build-scale-up pos pattern)))))

(defun random-scale (template)
  (let* ((letters '(A B C D E F G C# D# F# G# A#))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template (intern (format nil "~A~d" random-letter 3))
			      (intern (format nil "~A~d" random-letter 4))
			      template)))


(defun random-major-scale () (random-scale (major-scale-template)))

(defun scale-notes (scale)
  (attr 'notes scale))

(defun make-scale (scale-root &optional (template (major-scale-template)))
  (list
   (cons 'notes (build-scale scale-root template))
   (cons 'template template)))

(defun scale-range2 (p1 p2 scale-data)
  (let ((newscale (make-scale p1)))
    (setf (cdr (assoc 'scale newscale))
	  (scale-range p1 p2 (scale-notes scale-data)))
    newscale))

(defun scale-octaves (scale &optional (count 0))
  (if scale
      (if (eq (note-solfege (car scale)) 'do)
	  (cons
	   (cons (car scale) (+ 1 count))
	   (scale-octaves (cdr scale) (+ 1 count)))
	  (cons
	   (cons (car scale) count)
	   (scale-octaves (cdr scale) count)))))

(defun midi-notes-from-scale (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (let ((newnote (make-note (note-name (car midi-notes)) (note-value (car midi-notes)) (cdr (car scale)))))
      (if (eq 'w (car (car scale)))
	  (cons newnote (midi-notes-from-scale (cdr (cdr midi-notes)) original-scale (cdr scale)))
	  (cons newnote (midi-notes-from-scale (cdr midi-notes) original-scale (cdr scale)))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale midi-notes original-scale original-scale))))

(defun midi-notes-from-scale-down-helper (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (if (eq 'h (car (car scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr midi-notes) original-scale (cdr scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr (cdr midi-notes)) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale-down-helper midi-notes original-scale original-scale))))

(defun with-scale-helper (scale my-fn)
  (funcall my-fn scale))
(defmacro with-scale (scale &body body)
  `(with-scale-helper ,scale (lambda (*current-scale*) ,@body )))

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y scale) (loop for x from 1 to y collect (random-note scale)))

(defun solfege-chord (l scale)
  (let ((chord (mapcar (lambda (x)
			 (find-solfege x scale) ) l)))
    (chord-play chord)))

(defun find-solfege (solfege lis)
  (if lis
      (if (listp (note-solfege (car lis)))
	  (if (position solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis)))

	  (if (eq solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis))))))
