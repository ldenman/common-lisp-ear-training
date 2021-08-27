(in-package :ld-music)

(defun find-chord (octave romand-num chord-list scale)
  (find-if
   (lambda (y)
     (= (relative-octave (find-note-in-octave (chord-tone-note (car (cdr y))) (scale-notes scale)))
	octave))
   (find-all-if (lambda (chord-tones) (eq romand-num (car chord-tones))) chord-list )))

(defun make-chord-tone (note degree)
  (if note
      (list
       (cons 'type 'chord-tone)
       (cons 'note note)
       (cons 'degree degree))))
(defun chord-tone-note (chord-tone) (attr 'note chord-tone))
(defun chord-degree (chord-tone) (attr 'degree chord-tone))
(defun chord-notes (chord) (mapcar #'chord-tone-note chord))

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
  (chord-roman-numerals
   (funcall filter-fn (chord-builder (build-scale start-note (major-scale-template)))))) 

(defun make-scale-chords (scale)
  (list (cons 'scale scale)
	(cons 'chords (chord-builder (scale-notes scale)))
	(cons 'roman-numeral-chords (chord-roman-numerals (chord-builder (scale-notes scale))))))

;; CHORDS functions
(defun scale-chords (scale-chord-data) (attr 'chords scale-chord-data))
(defun chord-sequence-chords (chord-sequence) (mapcdr chord-sequence))
(defun chord-root (chord)
  (find-if (lambda (chord-tone) (= 1 (attr 'degree chord-tone))) chord))

(defun chord-butroot (chord) (chord-remove-degree chord 1))
(defun chord-butfifth (chord) (chord-remove-degree chord 5))
(defun chord-drop-root (chord scale) 
  (if (note-octave-down (chord-tone-note (chord-root chord)) scale)
      (setf
       (cdr (assoc 'note (chord-root chord)))
       (note-octave-down (chord-tone-note (chord-root chord)) scale)))
  chord)
(defun chord-invert-upper (chord)
  (append (list (chord-root chord))
	  (chord-over-3 (chord-butroot chord)
			(make-scale 'c4))))

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

(defun chord-sequence (chord-sequence chords scale &optional (octave 4))
  (if chord-sequence
      (if (and (listp (car chord-sequence)) (eq 'octave (car (car chord-sequence))))
	  (chord-sequence (cdr chord-sequence) chords scale octave)
	  (cons (find-chord octave (car chord-sequence) (chord-roman-numerals chords) scale)
		(chord-sequence (cdr chord-sequence) chords scale octave)))))

(defun scale-chord-filter (chord-data fn &rest args)
  (let ((chords (funcall (apply fn args) (attr 'chords chord-data))))
    (attr= chords 'chords chord-data)
    (attr= (chord-roman-numerals chords) 'roman-numeral-chords chord-data)
    chord-data))

(defun rebuild-chords ()
  (lambda (chord-data)
    (attr 'chords (make-scale-chords (attr 'scale chord-data)))))

(defun octave-filter (octave)
  (lambda (chords)
    (remove-if-not
     (lambda (chord)
       (= octave (note-attr (chord-tone-note (car chord)) 'octave)))
     chords)))

(defun chord-filter (fn)
  (lambda (chord-data) (mapcar fn chord-data)))

(defun chord-type-filter (fn)
  (lambda (chord-data) (funcall fn chord-data)))

(defun chord-seq (chord-data seq &optional (octave 4))
  (chord-sequence seq (attr 'chords chord-data) (attr 'scale chord-data) octave))
