(in-package :ld-music)

; Background
; This project is my exploration in representing musical information with LISP.
; What kind of musical information is there to represent? Notes, Scales, and Chords.

;;;; Notes - the most basic unit of musical pitch information.
;;;;;; These are currently represented as integers (MIDI) and
;;;;;; symbolically as moveable-do solfege like (DO RE MI) and
;;;;;; absolute name "C#".

;;;; Scales - collections of notes based on scale patterns (major, minor, etc)
;;;; Chords - collections of notes usually derived from a scale

;;;; This program deals only with the notes on a standard 88 key piano.
;;;; Limiting the notes to 88 seems to be practical at this time.

;; The initial and most fundamental data we have is a list of MIDI INTEGERS (27..108)
;;;; (midi-integers)
;;;; => (21 22 23 24 25 26 27 28 29 ... 108)

;;  Then there is the #'midi-note-octave list of absolute note names and octave
;;;(midi-note-octave)
;;;; => (A0 |A#0| B0 C0 |C#0| D0 |D#0| ... C7)

;;;; (first (midi-note-octave)) ; A0
;;;; (last (midi-note-octave))  ; (C7)
;;;; (length (midi-note-octave)); 88

;; The  #'midi-notes function pairs up the MIDI integers and the absolute note names
;;;; (first (midi-notes)) (A0 . 21)
;;;; (last (midi-notes)) ((C7 . 108))
;;;; (length (midi-notes))   ; 88

;; At this point, we have a basic representation of all notes on the keyboard.
;; The next step is to build scales.

;; The #'make-scale-template function is used to make scale templates.
;; Scale templates are used to realize scales from the patterns they define.
;; The major scale uses a pattern of "W W H W W W H" where W is 2 semitones and H is 1 semitone.

;; To define a major scale template, set the pattern and the solfege syllables:
;; (make-scale-template '(w w h w w w h) '(do re mi fa so la ti do))
;;   => ((W . DO) (W . RE) (H . MI) (W . FA) (W . SO) (W . LA) (H . TI))

;; And then to realize the scale, use the #'make-scale-from-template
;; This function looks at all the notes available and reduces the result to only the notes found according to the scale pattern.
;; The function signature requires a starting note and ending note used to return a range of notes.

;; The following creates a C major scale from C4 to C5:

;; (let ((major-scale-template
;; 	(make-scale-template '(w w h w w w h)
;; 			     '(do re mi fa so la ti do))))
;;   (make-scale-from-template 'C4 'C5 major-scale-template))
;;=> (((C4 . 72) . DO) ((D4 . 74) . RE) ((E4 . 76) . MI) ((F4 . 77) . FA) ((G4 . 79) . SO) ((A5 . 81) . LA) ((B5 . 83) . TI) ((C5 . 84) . DO))

;; At this point we have a list representing the C major scale from  C4 to C5.
;; Each item in the list is a NOTE -- a pairing of SOLFEGENAME with a pair of NOTENAME and MIDI-INT
;; The functions #'note-name, #'note-value, #'note-solfege are used to get note data
;; (note-name '((C4 . 72) . DO))   ;=> C4
;; (note-value '((C4 . 72) . DO))  ;=> 72
;; (note-solfege '((C4 . 72) . DO));=> DO

;; --CHORDS--
;; The next step would be to build up chords.
;; The C Major scale notes are C D E F G A B. To make chords, you combine every other note in scale:
;; The triads in C major are "CEG" "DFA" "EGB" "FAC" "GBD" "ACE" "BDF".
;; The seventh chords in C major are "CEGA" "DFAG" "EGBD" "FACE" "GBDF" "ACEG" "BDFA".
;; Use the #'chord-builder function to get a list of chords back.
;; #'chord-builder takes a scale and generates a list of chords up the the 13th (remember, a chord is just a list of notes)
 
;; (defun test-chord-builder ()
;;   (let* ((c-major-scale
;; 	   (make-scale-from-template 'C2 'C7
;; 				     (make-scale-template '(w w h w w w h)
;; 							  '(do re mi fa so la ti do)))))
;;     (take-octaves 2 (chord-builder c-major-scale))))

;; (nth 0 (test-chord-builder))
;; => (((C2 . 48) . DO) ((E2 . 52) . MI) ((G2 . 55) . SO) ((B3 . 59) . TI) ((D3 . 62) . RE) ((F3 . 65) . FA) ((A4 . 69) . LA))
;; (nth 1 (test-chord-builder))
;; => (((D2 . 50) . RE) ((F2 . 53) . FA) ((A3 . 57) . LA) ((C3 . 60) . DO) ((E3 . 64) . MI) ((G3 . 67) . SO) ((B4 . 71) . TI))

;; --TRIADS AND SEVENTHS--
;; The #'triads and #'sevenths functions take a list of chords and reduce each chord to a specific number of notes, 3 and 4 respectively.
;; The #'chord-take function takes an integer and list of chords 
;; (car (triads (test-chord-builder)))
;;=> (((C2 . 48) . DO) ((E2 . 52) . MI) ((G2 . 55) . SO))

;; (car (sevenths (test-chord-builder)))
;;=> (((C2 . 48) . DO) ((E2 . 52) . MI) ((G2 . 55) . SO) ((B3 . 59) . TI)) 

;; (car (chord-take 2 (test-chord-builder)))
;; => (((C2 . 48) . DO) ((E2 . 52) . MI))


;; For exTo generate the triads in the C major scale 

(defun chord-range (p1 p2 chords)
  (let ((i (position p1 chords :test (lambda (x y) (eq x (note-name (car y))))))
	(e (position p2 chords :test (lambda (x y) (eq x (note-name (car y)))))))
    (subseq chords i (+ 1 e))))

(defun take-octaves (n list)
  (take (+ 1 (* 7 n)) list))

;; DATA FORMATS

;; NOTE -- a pairing of SOLFEGENAME with a pair of NOTENAME and MIDI-INT
;;; '((NOTENAME . MIDI-INT) . SOLFEGENAME)
;;; For example: '((C4 . 77) . DO)

;; SCALE -- a list of NOTES
;;; '(((C4 . 77) . DO))

;; SCALE TEMPLATE -- a list of scale steps and solfege used to realize scales
;;;((W . DO) (W . RE) (H . MI) (W . FA) (W . SO) (W . LA) (H . TI))

;; INTERNAL MAKER FUNCTIONS
;;; (make-scale-template '(w w h w w w h) '(do re mi fa so la ti do))
;;; (make-note2 'C4) -> '((C4 . 72) . NIL) -> '((NOTENAME . NOTEVALUE) . SOLFEGENAME)
;;; (make-scale-from-template 'C4 'C5 (major-scale))
;;; (make-scale scale-steps solfege-list) 

;; External maker functions

;; TODO - don't use globals
(defvar *midi-out3* nil)
(setf *midi-out3* nil)

;; TODO - don't use globals
(defun pm-reload ()
  (if *midi-out3*
      (progn
	(let ((oldid *midi-out3*))
	  (setf *midi-out3* nil)
	  (pm:close-midi oldid))))
  (pm:list-devices)
  (pm:terminate)
  (pm:initialize)
  (setf *midi-out3* (pm:open-output 2 1024 0)))
(pm-reload)
;; TODO - don't use globals
(defun pm-terminate ()
  (if *midi-out3*
      (progn
	(let ((oldid *midi-out3*))
	  (setf *midi-out3* nil)
	  (pm:close-midi oldid))))
  (pm:list-devices)
  (pm:terminate))

;; Helpers

(defun pairup (l1 l2)
  (if (and l1 l2)
  (cons
   (cons (car l1) (car l2))
   (pairup (cdr l1) (cdr l2)))))

(defun take (n l)
  (subseq l 0 n))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)


(defun any? (i l)
   (if l
       (if (eq i (car l))
 	  (cons i (any? i (cdr l)))
 	  (any? i (cdr l)))))

;; ROTATE SCALES
;; (defun rotate (scale) (append (cdr scale) (list (car scale))))
;; (defun rotate-n (n scale)
;;   (if (> n 0)
;;       (rotate-n (- n 1) (rotate scale))
;;       scale))

(defun map-idx (s)
  (let ((idx 0))
    (mapcar
     (lambda (i)
       (let ((r (cons i idx)))
	 (setf idx (+ 1 idx))
	 r)  )
     s)))

(defun find-solfege1 (los lis)
  (if (and los lis)
      (if (not (listp (cdr (car lis))))
	  (if (eq (car los) (cdr (car lis)))
	      (cons (car lis) (find-solfege1 (cdr los) (cdr lis)))
	      (find-solfege1 los (cdr lis)))
	  (if (any? (car los) (cdr (car lis)))
	      (cons (car lis) (find-solfege1 (cdr los) (cdr lis)))
	      (find-solfege1 los (cdr lis))))))

;; Scale Functions
(defun make-scale-template (steps solfege)
  (if steps (pairup steps solfege)))

(defun chromatic-scale-template ()
  (make-scale-template '(h h h h h h h h h h h h) '(do (di ra) (re) (ri me) mi fa (fi se) so (si le) la (li te) ti do)))
(defun major-scale-template () (make-scale-template '(w w h w w w h) '(do re mi fa so la ti do)))
(defun minor-scale-template () (make-scale-template '(w h w w h w w) '(do re me fa so le te do)))
(defun dorian-scale-template () (make-scale-template '(w h w w w h) '(do re me fa so la ti do)))
(defun phrygian-scale-template () (make-scale-template '(h w w w h w) '(do ra me fa so le te do)))

(defun scaleints (scale)
  (mapcar (lambda (i) (if (eq 'w (car i))
			  (cons 2 (cdr i))
			  (cons 1 (cdr i)))) scale))

(defun midi-note-octave ()
  '(A0 A#0 B0 C0 C#0 D0 D#0 E0    F0    F#0    G0    G#0 A1    A#1    B1    C1    C#1    D1    D#1    E1    F1    F#1    G1    G#1 A2    A#2    B2    C2    C#2    D2    D#2    E2    F2    F#2    G2    G#2 A3    A#3    B3    C3    C#3    D3    D#3    E3    F3    F#3    G3    G#3  A4    A#4    B4    C4    C#4    D4    D#4    E4    F4    F#4    G4    G#4 A5    A#5    B5    C5    C#5    D5    D#5    E5    F5    F#5    G5    G#5 A6    A#6    B6    C6    C#6    D6    D#6    E6    F6    F#6    G6    G#6  A7    A#7    B7    C7))

;; Boiler plate
(defun midi-integers ()
  (loop for x from 0 to 87 collect (+ 21 x)))

(defun midi-notes ()
  (pairup (midi-note-octave) (midi-integers)))

(defun midi-notes-from-scale (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (if (eq 2 (car (car (scaleints scale))))
	  (cons (cons (car midi-notes) (cdr (car (scaleints scale))))
		(midi-notes-from-scale (cdr (cdr midi-notes)) original-scale (cdr scale)))
	  (cons (cons (car midi-notes) (cdr (car (scaleints scale))))
		(midi-notes-from-scale (cdr midi-notes) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale midi-notes original-scale original-scale))))

(defun scale-range (n1 n2 scale)
  (let* ((eqfn (lambda (x y) (eq x (car y))))
	 (p1 (position n1 scale :test eqfn ))
	(p2 (position n2 scale :test eqfn)))
    (subseq scale p1 (+ 1 p2))))

(defun make-scale-from-template (p1 p2 scale-template)
  (midi-notes-from-scale (scale-range p1 p2 (midi-notes)) scale-template scale-template))

;;FIXME
;;(defun modes (scale) (mapcar (lambda (i) (rotate-n (cdr i) scale)) (map-idx scale)))

(defvar *current-scale* nil)
(defun set-scale (scale)
  (setf *current-scale* scale))

;; Note selector functions
(defun note-name (note) (car (car note)))
(defun note-value (note) (cdr (car note)))
(defun note-solfege (note) (cdr note))

;; Note functions
(defun prev-note (n l)
  (if (and n l)
      (if (equal n
		 (car (car (cdr l))))
	  (car l)
	  (prev-note n (cdr l)))))
(defun prev-notes (n note)
  (if (> n 0)
      (prev-notes (- n 1) (prev-note note (midi-notes)))
      note))
(defun next-note (n l)
  (if (and n l)
      (if (equal n
	      (car (car l)))
	  (car (cdr l))
	  (next-note n (cdr l)))))
(defun next-notes (n note)
  (if (> n 0)
      (next-notes (- n 1) (next-note note (midi-notes)))
      note))
(defun octave-down (note) (prev-notes 12 note))
(defun octave-up (note) (next-notes 12 note))

;;MIDI functions
;; (defun play-note (note &optional (on-time 0) off-time (velocity 80))
;;   "Play a note."
;;   (let ((value (note-value note))
;; 	(off-time (or off-time (+ on-time 1))))
;;     (schedule on-time #'note-play note velocity)
;;     (schedule off-time #'note-off note)))

;; TODO - don't use globals
(defun note-play (note &optional (velocity 80))
  (pm:write-short-midi *midi-out3* 0 (pm:note-on 0 (note-value note) 80)))
(defun note-off (note)
  (princ (note-value note))
  (pm:write-short-midi *midi-out3* 0 (pm:note-off 0 (note-value note) 0)))

;(play-note '((c4 . 72)) 0 0.01)

(pm-reload)
(pm:list-devices)
*midi-out3*
;; (progn
;; (note-play '((c4 . 72)))
;; (note-off '((c4 . 72))))
;; TODO - don't use globals
(defun note-play-sleep (note)
  (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value note) 80))
  (sleep 0.25)
  (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value note) 0)))

;; TODO - don't use globals
(defun play-scale (scale)
  (dolist (n scale)
    (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value n) 80))
    (sleep 0.1)
    (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value n) 0))
    (sleep 0.3)))

;(play-scale (make-scale-from-template 'C0 'C7 (major-scale-template)))

(defun solfege-chord (l) (dolist (note (find-solfege1 l *current-scale*)) (note-play-sleep note)))

(defun arp (l) (dolist
		   (x (find-solfege1 l *current-scale*))
		 (note-play (car x)) (sleep 0.5)))
(defun darp (l) (find-solfege1 l *current-scale*))
(defun rarp (l)
  (let ((m (reverse (find-solfege1 l *current-scale*))))
    (dolist
	(x m)
      (note-play (car x)) (sleep 0.5))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun play-tonic (scale) (note-play (car (car scale))))
(defun play-subdominant (scale) (note-play (car (nth 3 scale))))
(defun play-dominant (scale) (note-play (car (nth 4 scale))))

(defun play-tonic-subdominant-dominant (scale)
  (progn
    (note-play (octave-down (car (car scale))))
    (solfege-chord '(DO MI SO))
    (sleep 0.5)
    (solfege-chord '(FA LA DO))
    (sleep 0.5)
    (solfege-chord '(SO TI RE))
    (sleep 0.5)
    (play-tonic scale)))

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y) (loop for x from 1 to y collect (random-note *current-scale*)))
(defun play-random (scale) (note-play (car (random-note scale))))

(defun set-random-scale ()
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (set-scale (make-scale-from-template (intern (format nil "~A~d" random-letter 3))
					(intern (format nil "~A~d" random-letter 4))
					(major-scale-template)))))

(defun quick-test ()
  (pm-reload)
  (note-play '((C4 . 72))))
;;(quick-test)
;;(pm-reload)

(defun chord-builder (l)
  (if l
      (cons (list (car l) ; 1
		  (nth 1 (cdr l)) ;3
		  (nth 3 (cdr l)) ; 5
		  (nth 5 (cdr l)) ; 7
		  (nth 7 (cdr l)) ; 9 
		  (nth 9 (cdr l)) ; 11
		  (nth 11 (cdr l)) ; 13
		  )
	    (chord-builder (cdr l)))))

(defun modes2 (scale)
  (chord-builder scale))
(defun chord-take (n listofchords)
  (mapcar (lambda (l) (take n l) ) listofchords))
(defun triads (myl)
  (chord-take 3 myl))
(defun sevenths (myl)
  (chord-take 4 myl))

(defun chord-play (listofchords)
  (dolist (n listofchords)
    (note-play n)
    (sleep 1)))

;; (mapcar #'chord-play (take 8 (triads (modes2 (make-scale-from-template 'C2 'B5 (major-scale-template))))))

;; (loop while *playing* do
;; (mapcar (lambda (n) 
;; 	  (dolist (i  (take (+ 1 (random 3)) n))
;; 	    (note-play i)
;; 	    (note-off i))
;; 	  (sleep 1))
;; 	(nshuffle
;; 	 (take 8 (chord-builder (make-scale-from-template 'C2 'B5 (major-scale-template)))))))

;; (setf *playing* nil)
