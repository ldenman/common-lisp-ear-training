(in-package :ld-music)


;; Data directed programming
(defvar *db* '())
(setf *db* nil)
(defun dbput (type name fn)
  (push (cons (cons type name) fn) *db*))
(defun dbget (type op)
  (cdr (assoc (cons type op) *db* :test #'equal)))

(defun operate (op object)
  (let ((proc (dbget (whattype object) op)))
    (if (not (null proc))
	(funcall proc (contents object))
	(error "undefined operator"))))

(defun play2 (obj)
  (operate 'play obj))

;(dbput 'note 'play 'note-play)
;(dbput 'chord 'play 'play-chord-notes-nonasync)


;(play2 (make-note 'c4 72 nil))
;(play2 (car (attr 'chords (make-scale-chords (make-scale 'c4)))))

;(real-part (attach-type 'lake 1))

;; constructors
(defun make-chord-tone (note degree)
  (if note
      (list
       (cons 'type 'chord-tone)
       (cons 'note note)
       (cons 'degree degree))))


(defun chord-builder (notes)
  (if notes
      (let ((chord-tones
	      (list (make-chord-tone (nth 0 notes) 1)
		    (make-chord-tone (nth 2 notes) 3)
		    (make-chord-tone (nth 4 notes) 5)
		    (make-chord-tone (nth 6 notes) 7)
		    (make-chord-tone (nth 8 notes) 9)
		    (make-chord-tone (nth 10 notes) 11)
		    (make-chord-tone (nth 12 notes) 13))))
	(cons
	 (make-chord (remove nil chord-tones) )
	 (chord-builder (cdr notes))))))

(defun make-chord (chord-tones)
  (list
   (cons 'type 'chord)
   (cons 'chord-tones chord-tones)))

(defun make-scale-chords (scale &optional (template (major-scale-template)))
  (let ((chords (chord-builder (scale-notes scale))))
    (list
     (cons 'type 'scale-chords)
     (cons 'scale scale)
     (cons 'chords chords)
     (cons 'template template)
     (cons 'roman-numeral-chords (chord-roman-numerals chords)))))

(defun make-chord-sequence (chord-sequence chord-item &optional (octave 4))
  (tag 'chord-sequence
       (chord-sequence chord-sequence chord-item octave)))

 ;;;; Converts a list of roman numerals into chords
(defun chord-sequence-helper (chord-sequence chord-data &optional (octave 4))
  (if chord-sequence
      (if (and (listp (car chord-sequence))
	       (eq 'octave (car (car chord-sequence))))
	  (chord-sequence-helper (cdr chord-sequence) chord-data (cdr (car chord-sequence)))
	  (cons (find-chord2 octave (car chord-sequence) chord-data)
		(chord-sequence-helper (cdr chord-sequence) chord-data octave)))))

;;;; Scale chord sequence
(defun chord-sequence (seq item &optional (octave 4))
  (cond ((scale? item)
	 (chord-sequence-helper seq (make-scale-chords item) octave))
	((scale-chords? item)
	 (chord-sequence-helper seq item octave))))
;(chord-sequence '(I) (scale? (make-scale 'c4)))
;; selectors

;;;; scale chord
(defun scale-chords (scale-chord-data) (attr 'chords scale-chord-data))

;;;; chords
(defun chord-tones (chord)
  (attr 'chord-tones chord))
(defun chord-degree (chord-tone) (attr 'degree chord-tone))
(defun chord-notes (chord) (mapcar #'chord-tone-note (chord-tones chord)))
(defun chord-solfege (chord)
  (mapcar #'note-solfege (chord-notes chord)))

;;;; chord tones
(defun chord-tone-note (chord-tone) (attr 'note chord-tone))
(defun chord-tone-degree (chord-tone) (attr 'degree chord-tone))

;;;; roman numeral chord sequence
(defun chord-sequence-romans (chord-sequence) (mapcar #'car chord-sequence))
(defun chord-sequence-chords (chord-sequence) (mapcdr chord-sequence))
(defun select-romand-numeral (roman-numeral->chord)
  (car roman-numeral->chord))
(defun select-chord (roman-numeral->chord)
  (cdr roman-numeral->chord))

;; functions


(defun find-chord2 (octave romand-num chord-data)
  (find-if
   (lambda (roman-num-data) (= (note-relative-octave
				(car (chord-notes
				      (select-chord roman-num-data)))) octave))
   (find-all-if
    (lambda (chord-tones)
      (eq romand-num (car chord-tones)))
    (attr 'roman-numeral-chords chord-data))))

;; Chords functions

;;;; destructive
(defun chord-modify-chord-tones (chord chord-tones)
  (attr= chord-tones 'chord-tones chord)
  chord)

(defun chord-length (chord)
  (length (chord-tones chord)))

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

(defun chord-remove-degree (chord degree)
  (remove-if (lambda (chord-tone) (= degree (chord-tone-degree chord-tone))) chord))

(defun c-take (n chord)
  (let ((chord-tones (take n (chord-tones chord))))
    (chord-modify-chord-tones chord chord-tones)
    chord))

(defun chord-triad (chord)
  (c-take 3 chord))
(defun chord-seventh (chord) (c-take 4 chord))
(defun chord-ninth (chord) (c-take 5 chord))
(defun chord-eleventh (chord) (c-take 6 chord))
(defun chord-thirteenth (chord) (c-take 7 chord))

(defun chord-invert (chord scale)
  (attr= (note-octave-up (chord-tone-note (car chord)) scale) 'note (car chord))
  (append (rest chord) (list (car chord))))
 (defun chord-over-3 (root-position-chord scale)
  (chord-invert root-position-chord scale))
(defun chord-over-5 (root-position-chord scale)
  (chord-invert (chord-invert root-position-chord scale) scale))

(defun find-roman-numeral-by-solfege (chord)
  (cdr (find (note-solfege
	 (car (chord-notes chord)))
	(major-solfege-chords)
	:test (lambda (note-solfege other) (eq note-solfege (car other))))))

;; aggregrate chord functions
(defun chord-take (n listofchords)
  (mapcar (lambda (chord)
	    (chord-modify-chord-tones chord
				      (take n (chord-tones chord))))
	    (remove-if (lambda (chord) (< (chord-length chord) n)) listofchords)))
(defun triads (myl) (chord-take 3 myl))
(defun sevenths (myl) (chord-take 4 myl))
(defun ninths (myl) (chord-take 5 myl))
(defun elevenths (myl) (chord-take 6 myl))
(defun thirteenths (myl) (chord-take 7 myl))

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
	    (let ((roman-numeral (find-roman-numeral-by-solfege chord)))
	      (cons roman-numeral chord)))
	  chord-list))

(defun scale-chord-filter (chord-data fn &rest args)
  (let ((chords (funcall (apply fn args) (attr 'chords chord-data))))
    (attr= chords 'chords chord-data)
    (attr= (chord-roman-numerals chords) 'roman-numeral-chords chord-data)
    chord-data))

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
