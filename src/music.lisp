(in-package :ld-music)

;; External maker functions

;(pm-initialize)

;; Finder functions
(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

(defun find-solfege (solfege lis)
  (if lis
      (if (listp (note-solfege (car lis)))
	  (if (position solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis)))

	  (if (eq solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis))))))

(defun find-note (name &optional (scale (midi-notes)))
  (find-if (lambda (note) (eq (note-name note) name)) scale))

(defun find-chord2 (octave romand-num chord-list)
  (find-if
   (lambda (y)
     (= (note-octave2 (chord-tone-note (car (cdr y))) (scale-notes (make-scale 'c4))) octave))
   (find-all-if (lambda (chord-tones) (eq romand-num (car chord-tones))) chord-list )))

(defun find-chord (octave romand-num chord-list)
  (find-if
   (lambda (y)
     (eq octave (cdr (assoc 'octave (chord-tone-note (car (cdr y)))))))
   (find-all-if (lambda (chord-tones) (eq romand-num (car chord-tones))) chord-list )))

;; Scale Functions
(defun make-scale-template (steps solfege)
  (if steps (pairup steps solfege)))

(defun chromatic-scale-template ()
  (make-scale-template '(h h h h h h h h h h h h) '(do (di ra) re (ri me) mi fa (fi se) so (si le) la (li te) ti do)))
(defun major-scale-template () (make-scale-template '(w w h w w w h) '(do re mi fa so la ti do) ))
(defun minor-scale-template () (make-scale-template '(w h w w h w w) '(do re me fa so le te do)))
(defun dorian-scale-template () (make-scale-template '(w h w w w h) '(do re me fa so la ti do)))
(defun phrygian-scale-template () (make-scale-template '(h w w w h w) '(do ra me fa so le te do)))

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

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y scale) (loop for x from 1 to y collect (random-note scale)))
(defun play-random (scale) (note-play (car (random-note scale))))

;; Boiler plate
;; Note Data boiler plate
(defun note-name-position (note-name &optional (scale (midi-notes)))
  (position note-name scale :test (lambda (x y) (equal x (note-name y)))))

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

;; (defun midi-notes-from-scale-down (midi-notes scale)
;;   (midi-notes-from-scale-down-helper midi-notes scale scale))

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

(defun make-chord-tone (note degree)
  (if note
      (list
       (cons 'type 'chord-tone)
       (cons 'note note)
       (cons 'degree degree))))
(defun chord-tone-note (chord-tone)(attr 'note chord-tone))
(defun chord-degree (chord-tone) (attr 'degree chord-tone))
(defun chord-notes (chord) (mapcar #'chord-tone-note chord))

;; CHORDS functions
(defun chord-builder (l)
  (if l

      (let ((chord-tones
	      (list (make-chord-tone (car l) 1)
		    (make-chord-tone (nth 1 (cdr l)) 3)
		    (make-chord-tone (nth 3 (cdr l)) 5)
		    (make-chord-tone (nth 5 (cdr l)) 7)
		    (make-chord-tone (nth 7 (cdr l)) 9)
		    (make-chord-tone (nth 9 (cdr l)) 11)
		    (make-chord-tone (nth 11 (cdr l)) 13))))
	(cons
	 (remove nil chord-tones)
	 (chord-builder (cdr l))))))

(defun make-chords (start-note &optional (filter-fn #'triads) (template (major-scale-template)))
  (chord-roman-numerals (funcall filter-fn (chord-builder (build-scale start-note (major-scale-template)))))) 

(defun make-scale-chords (scale)
  (list (cons 'scale scale)
	(cons 'chords (chord-builder (scale-notes scale)))
	(cons 'roman-numeral-chords (chord-roman-numerals (chord-builder (scale-notes scale))))))

(defun scale-chords (scale-chord-data) (attr 'chords scale-chord-data))
(defun chord-sequence-chords (chord-sequence) (mapcdr chord-sequence))
(defun chord-root (chord)
  (find-if (lambda (chord-tone) (= 1 (attr 'degree chord-tone))) chord))

(defun chord-sequence-play (chord-sequence)
  (dolist (chord (chord-sequence-chords chord-sequence))
    (chord-play chord)))

(defun chord-butroot (chord) (chord-remove-degree chord 1))
(defun chord-butfifth (chord) (chord-remove-degree chord 5))
(defun chord-drop-root (chord) 
  (if (note-octave-down (chord-tone-note (chord-root chord)) (make-scale 'c4))
      (setf
       (cdr (assoc 'note (chord-root chord)))
       (note-octave-down (chord-tone-note (chord-root chord)) (make-scale 'c4))))
  chord)
(defun chord-invert-upper (chord)
  (append (list (chord-root chord))
	  (chord-over-3 (chord-butroot chord)
			(make-scale 'c4))))

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

(defun note-octave2 (note scale)
  (cdr (find-if (lambda (n)
	     (note-equal-p note (car n))) (scale-octaves scale))))

;; (chord-sequence-play
;;  (chord-sequence '(I  II- III- IV V VI- VII (octave . 3) I) 
;; 		 (sevenths (scale-chords (make-scale-chords (make-scale 'C4))))
;; 		 2
;; 		 ))

;; (chord-sequence-play
;;  (chord-sequence '(II- V I I II- V I (octave . 4) I) 
;; 		 (mapcar #'chord-drop-root (mapcar #'chord-drop-root (sevenths (scale-chords (make-scale-chords (make-scale 'C4))))))
;; 		 2
;; 		 ))



;; (chord-play2 (car (mapcar #'chord-drop-root (triads (scale-chords (make-scale-chords (scale-range2 'C2 'C4 (make-scale 'c4))))))))

;; (chord-play (car (mapcar #'chord-drop-root (triads (scale-chords (make-scale-chords (scale-range2 'C4 'G5 (make-scale 'c4))))))))

(defun chord-tone-degree (chord-tone) (attr 'degree chord-tone))
(defun chord-remove-degree (chord degree)
  (remove-if (lambda (chord-tone) (= degree (chord-tone-degree chord-tone))) chord))

(defun chord-take (n listofchords)
  (mapcar (lambda (l) (take n l) ) (remove-if (lambda (chord) (< (length chord) n)) listofchords)))
(defun triads (myl) (chord-take 3 myl))
(defun sevenths (myl) (chord-take 4 myl))
(defun ninths (myl) (chord-take 5 myl))
(defun elevenths (myl) (chord-take 6 myl))
(defun thirteenths (myl) (chord-take 7 myl))

(defun chord-invert (chord scale)
  (attr= (note-octave-up (chord-tone-note (car chord)) scale) 'note (car chord))
  (append (rest chord) (list (car chord))))
(defun chord-over-3 (root-position-chord scale)
  (chord-invert root-position-chord scale))
(defun chord-over-5 (root-position-chord scale)
  (chord-invert (chord-invert root-position-chord scale) scale))

(defun major-solfege-chords ()
  '((do . I)
    (re . II-)
    (mi . III-)
    (fa . IV)
    (so . V)
    (la . VI-)
    (ti . VII)))

(defun chord-roman-numerals (chord-list)
  (mapcar (lambda (chord)
	    (cons
	     (cdr (find (note-solfege (car (chord-notes chord)))
			(major-solfege-chords) :test (lambda (note-solfege solfege->romnum) (eq note-solfege (car solfege->romnum)))))
	     chord)

	    ) chord-list))

(defun chord-sequence (chord-sequence chords &optional (octave 4))
  (if chord-sequence
      (if (and (listp (car chord-sequence)) (eq 'octave (car (car chord-sequence))))
	  (chord-sequence (cdr chord-sequence) chords (cdr (car chord-sequence)))
	  (cons (find-chord2 octave (car chord-sequence) (chord-roman-numerals chords))
		(chord-sequence (cdr chord-sequence) chords octave)))))

(defun chord-play (chord)
  ;(note-play (note-octave-down (car listofchords)))
  (dolist (note (chord-notes chord))
    (note-play note))
  (sleep 1))

(defun play-chords (chords)
  (dolist (chord chords)
    (chord-play chord)
    (sleep 1)))

;;FIXME
;;(defun modes (scale) (mapcar (lambda (i) (rotate-n (cdr i) scale)) (map-idx scale)))
(defun with-scale-helper (scale my-fn)
  (funcall my-fn scale))
(defmacro with-scale (scale &body body)
  `(with-scale-helper ,scale (lambda (*current-scale*) ,@body )))

(defun make-note (name value solfege)
  (list
   (cons 'type 'note)
   (cons 'name name)
   (cons 'value value)
   (cons 'solfege solfege)
   (cons 'octave (note-octave name ))))

;; Note selector functions
(defun note-attr (note attr) (cdr (assoc attr note)))
(defun note-name (note) (note-attr note 'name))
(defun note-value (note) (note-attr note 'value))
(defun note-solfege (note) (note-attr note 'solfege))
(defun note-equal-p (x y)
  (and (equal (note-value x)
	      (note-value y))
       (equal (note-name x)
	      (note-name y))))
(defun note-idx (note &optional (scale (midi-notes)))
  (position note scale :test #'note-equal-p))

(defun note-octave-up (note scale)
  (let* ((other-note (nth (+ 12 (note-idx note)) (midi-notes))))
    (when other-note
      (attr= (note-solfege note) 'solfege other-note)
      other-note)))
(defun note-octave-down (note scale)
  (if (>= (- (note-idx note) 12) 0)
      (let* ((other-note (nth (- (note-idx note) 12) (midi-notes))))
	(when other-note
	  (attr= (note-solfege note) 'solfege other-note)
	  other-note))))

(defun note-octave (note-name)
  (parse-integer (car (multiple-value-list (cl-ppcre:scan-to-strings "\\d" (symbol-name note-name))))))

;;MIDI and Play functions
;; TODO - don't use globals
(defun solfege-chord (l scale)
  (let ((chord (mapcar (lambda (x)
			 (find-solfege x scale) ) l)))
    (chord-play chord)))

(defun arp (l scale)
  (dolist
      (x (mapcar (lambda (x) (find-solfege x scale)) l))
    (note-play x) (sleep 0.5)))

(defun rarp (l scale)
  (let ((m (reverse (mapcar (lambda (x) (find-solfege x scale)) l))))
    (dolist (x m)
      (note-play x) (sleep 0.5))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun play-tonic (scale) (note-play (car scale)))
(defun play-subdominant (scale) (note-play (nth 3 scale)))
(defun play-dominant (scale) (note-play (nth 4 scale)))

(defun play-tonic-subdominant-dominant (scale)
  (progn
    (note-play (note-octave-down (car scale)))
    (solfege-chord '(DO MI SO) scale)

    (solfege-chord '(FA LA DO) scale)

    (solfege-chord '(SO TI RE) scale)

    (play-tonic scale)))

(defun smoke-test ()
  (note-play (make-note 'C4 72 nil)))
