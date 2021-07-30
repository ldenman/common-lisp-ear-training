(in-package :ld-music)

; Background
; This project is my exploration in representing musical information with LISP.
; What kind of musical information is there to represent? To start... Notes, Scales, and Chords.

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

;; The  #'midi-notes function turns the MIDI integers and absolute note names into the NOTE data structure
;;;; (first (midi-notes)); ((TYPE . NOTE) (NAME . A0) (VALUE . 21) (SOLFEGE))  
;;;; (last (midi-notes)) ; (((TYPE . NOTE) (NAME . C7) (VALUE . 108) (SOLFEGE))) 
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

;=>(((TYPE NOTE) (NAME C4) (VALUE 72) (SOLFEGE DO))
  ; ((TYPE NOTE) (NAME D4) (VALUE 74) (SOLFEGE RE))
  ; ((TYPE NOTE) (NAME E4) (VALUE 76) (SOLFEGE MI))
  ; ((TYPE NOTE) (NAME F4) (VALUE 77) (SOLFEGE FA))
  ; ((TYPE NOTE) (NAME G4) (VALUE 79) (SOLFEGE SO))
  ; ((TYPE NOTE) (NAME A5) (VALUE 81) (SOLFEGE LA))
  ; ((TYPE NOTE) (NAME B5) (VALUE 83) (SOLFEGE TI))
  ; ((TYPE NOTE) (NAME C5) (VALUE 84) (SOLFEGE DO)))

;; At this point we have a list representing the C major scale from  C4 to C5.
;; Each item in the list is a NOTE -- a pairing of SOLFEGENAME with a pair of NOTENAME and MIDI-INT
;; The functions #'note-name, #'note-value, #'note-solfege are used to get note data
;; (let ((note (make-note 'C4 72 'DO)))
;;   (list (note-name note) 
;; 	(note-value note)  
;; 	(note-solfege note)
;;   ));=> (C4 72 DO)

;; --CHORDS--
;; The next step would be to build up chords.
;; The C Major scale notes are C D E F G A B. To make chords, you combine every other note in scale:
;; The triads in C major are "CEG" "DFA" "EGB" "FAC" "GBD" "ACE" "BDF".
;; The seventh chords in C major are "CEGA" "DFAG" "EGBD" "FACE" "GBDF" "ACEG" "BDFA".
;; Use the #'chord-builder function to get a list of chords back.
;; #'chord-builder takes a scale and generates a list of chords up the the 13th (remember, a chord is just a list of notes)
 
;; (defun test-chord-builder ()
;;   (let* ((c-major-scale
;; 	   (make-scale-from-template 'C2 'C4
;; 				     (make-scale-template '(w w h w w w h)
;; 							  '(do re mi fa so la ti do)))))
;;     (chord-builder c-major-scale)))

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

;; TODO - don't use globals
(defun pm-terminate ()
  (if *midi-out3*
      (progn
	(let ((oldid *midi-out3*))
	  (setf *midi-out3* nil)
	  (pm:close-midi oldid))))
  (pm:list-devices)
  (pm:terminate))

(defun pm-initialize ()
  (pm-reload))

(pm-initialize)

;; Helpers


(defun random-element (l)
  (nth (random (length l)) l))

(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

;; Grow list2 to same size as list1
(defun grow (l1 l2 &optional (idx 0))
  (if (not (= (length l1) (length l2)))
      (grow l1 (append l2 (list (nth idx l2))) (+ 1 idx))
      l2))

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

(defun prepend-tail (lis)
  (append (last lis) (butlast lis)))

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


(defun find-solfege (solfege lis)
  (if lis
      (if (eq solfege (note-solfege (car lis)))
	  (car lis)
	  (find-solfege solfege (cdr lis)))))

;; Scale Functions
(defun make-scale-template (steps solfege)
  (if steps (pairup steps solfege)))

(defun chromatic-scale-template ()
  (make-scale-template '(h h h h h h h h h h h h) '(do (di ra) (re) (ri me) mi fa (fi se) so (si le) la (li te) ti do)))
(defun major-scale-template () (make-scale-template '(w w h w w w h) '(do re mi fa so la ti do)))
(defun minor-scale-template () (make-scale-template '(w h w w h w w) '(do re me fa so le te do)))
(defun dorian-scale-template () (make-scale-template '(w h w w w h) '(do re me fa so la ti do)))
(defun phrygian-scale-template () (make-scale-template '(h w w w h w) '(do ra me fa so le te do)))

(defun midi-note-octave ()
  '(A0 A#0 B0 C0 C#0 D0 D#0 E0    F0    F#0    G0    G#0 A1    A#1    B1    C1    C#1    D1    D#1    E1    F1    F#1    G1    G#1 A2    A#2    B2    C2    C#2    D2    D#2    E2    F2    F#2    G2    G#2 A3    A#3    B3    C3    C#3    D3    D#3    E3    F3    F#3    G3    G#3  A4    A#4    B4    C4    C#4    D4    D#4    E4    F4    F#4    G4    G#4 A5    A#5    B5    C5    C#5    D5    D#5    E5    F5    F#5    G5    G#5 A6    A#6    B6    C6    C#6    D6    D#6    E6    F6    F#6    G6    G#6  A7    A#7    B7    C7))

(defun take-octaves (n list)
  (take (+ 1 (* 7 n)) list))

;; Boiler plate

;; Note Data boiler plate

(defun midi-instruments () '(
(1 . "Acoustic Grand Piano")
(2 . "Bright Acoustic Piano")
(3 . "Electric Grand Piano")
(4 . "Honky-tonk Piano")
(5 . "Electric Piano 1")
(6 . "Electric Piano 2")
(7 . "Harpsichord")
(8 . "Clavinet")

(9 . "Celesta")
(10 . "Glockenspiel")
(11 . "Music Box")
(12 . "Vibraphone")
(13 . "Marimba")
(14 . "Xylophone")
(15 . "Tubular Bells")
(16 . "Dulcimer")

(17 . "Drawbar Organ")
(18 . "Percussive Organ")
(19 . "Rock Organ")
(20 . "Church Organ")
(21 . "Reed Organ")
(22 . "Accordion")
(23 . "Harmonica")
(24 . "Tango Accordion")

(25 . "Acoustic Guitar (nylon)")
(26 . "Acoustic Guitar (steel)")
(27 . "Electric Guitar (jazz)")
(28 . "Electric Guitar (clean)")
(29 . "Electric Guitar (muted)")
(30 . "Overdriven Guitar")
(31 . "Distortion Guitar")
(32 . "Guitar harmonics")


(33 . "Acoustic Bass")
(34 . "Electric Bass (finger)")
(35 . "Electric Bass (pick)")
(36 . "Fretless Bass")
(37 . "Slap Bass 1")
(38 . "Slap Bass 2")
(39 . "Synth Bass 1")
(40 . "Synth Bass 2")

(41 . "Violin")
(42 . "Viola")
(43 . "Cello")
(44 . "Contrabass")
(45 . "Tremolo Strings")
(46 . "Pizzicato Strings")
(47 . "Orchestral Harp")
(48 . "Timpani")


(49 . "String Ensemble 1")
(50 . "String Ensemble 2")
(51 . "Synth Strings 1")
(52 . "Synth Strings 2")
(53 . "Choir Aahs")
(54 . "Voice Oohs")
(55 . "Synth Voice")
(56 . "Orchestra Hit")


(57 . "Trumpet")
(58 . "Trombone")
(59 . "Tuba")
(60 . "Muted Trumpet")
(61 . "French Horn")
(62 . "Brass Section")
(63 . "Synth Brass 1")
(64 . "Synth Brass 2")


(65 . "Soprano Sax")
(66 . "Alto Sax")
(67 . "Tenor Sax")
(68 . "Baritone Sax")
(69 . "Oboe")
(70 . "English Horn")
(71 . "Bassoon")
(72 . "Clarinet")


(73 . "Piccolo")
(74 . "Flute")
(75 . "Recorder")
(76 . "Pan Flute")
(77 . "Blown Bottle")
(78 . "Shakuhachi")
(79 . "Whistle")
(80 . "Ocarina")


(81 . "Lead 1 (square)")
(82 . "Lead 2 (sawtooth)")
(83 . "Lead 3 (calliope)")
(84 . "Lead 4 (chiff)")
(85 . "Lead 5 (charang)")
(86 . "Lead 6 (voice)")
(87 . "Lead 7 (fifths)")
(88 . "Lead 8 (bass + lead)")


(89 . "Pad 1 (new age)")
(90 . "Pad 2 (warm)")
(91 . "Pad 3 (polysynth)")
(92 . "Pad 4 (choir)")
(93 . "Pad 5 (bowed)")
(94 . "Pad 6 (metallic)")
(95 . "Pad 7 (halo)")
(96 . "Pad 8 (sweep)")


(97 . "FX 1 (rain)")
(98 . "FX 2 (soundtrack)")
(99 . "FX 3 (crystal)")
(100 . "FX 4 (atmosphere)")
(101 . "FX 5 (brightness)")
(102 . "FX 6 (goblins)")
(103 . "FX 7 (echoes)")
(104 . "FX 8 (sci-fi)")


(105 . "Sitar")
(106 . "Banjo")
(107 . "Shamisen")
(108 . "Koto")
(109 . "Kalimba")
(110 . "Bag pipe")
(111 . "Fiddle")
(112 . "Shanai")


(113 . "Tinkle Bell")
(114 . "Agogo")
(115 . "Steel Drums")
(116 . "Woodblock")
(117 . "Taiko Drum")
(118 . "Melodic Tom")
(119 . "Synth Drum")


(120 . "Reverse Cymbal")
(121 . "Guitar Fret Noise")
(122 . "Breath Noise")
(123 . "Seashore")
(124 . "Bird Tweet")
(125 . "Telephone Ring")
(126 . "Helicopter")
(127 . "Applause")
(  128 . "Gunshot")
			     ))

(defun make-message (status data1 data2)
  "=> an integer representing a MIDI message
Combines the integers `status`, `data1` and `data2` to a MIDI message."
  (let ((d2 (boole boole-and (ash data2 16) #xFF0000))
	(d1 (boole boole-and (ash data1 8) #xFF00))
	(st (boole boole-and status #xFF)))
    (boole boole-ior d2
	   (boole boole-ior d1 st))))

(defun make-message* (upper lower data1 data2) ;internal
  "=> a MIDI message as an integer
Works like `make-message` but combines `upper` and `lower` to the status byte."
  (let ((status (boole boole-ior
		       (boole boole-and (ash upper 4) #xF0)
		       (boole boole-and lower #xF))))
    (make-message status data1 data2)))


(defun program-change (program &optional (channel 1) (stream *midi-out3*))
  (pm:write-short-midi stream 0 (make-message* #0xC channel program  0))
  program)

(defun midi-integers ()
  (loop for x from 0 to 87 collect (+ 21 x)))

(defun midi-notes ()
  (loop for octave in (midi-note-octave)
	for integer in (midi-integers)
	collect (make-note octave integer nil)))

(defun note-name-position (note-name &optional (scale (midi-notes)))
  (position note-name scale :test (lambda (x y) (equal x (note-name y)))))

(defun find-note (name &optional (scale (midi-notes)))
  (find-if (lambda (note)
	     (eq (note-name note) 'C4)) scale)))

(defun midi-notes-from-scale2 (midi-notes original-scale scale)
  (assign-solfege
   (if (and midi-notes scale)
      (if (eq 'w (car (car scale)))
	  (cons (car midi-notes) (midi-notes-from-scale2 (cdr (cdr midi-notes)) original-scale (cdr scale)))
	  (cons (car midi-notes) (midi-notes-from-scale2 (cdr midi-notes) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale2 midi-notes original-scale original-scale)))
   original-scale))

(defun midi-notes-from-scale (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (let ((newnote (make-note (note-name (car midi-notes)) (note-value (car midi-notes)) (cdr (car scale)))))
      (if (eq 'w (car (car scale)))
	  (cons newnote (midi-notes-from-scale (cdr (cdr midi-notes)) original-scale (cdr scale)))
	  (cons newnote (midi-notes-from-scale (cdr midi-notes) original-scale (cdr scale)))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale midi-notes original-scale original-scale))))

;(make-scale-from-template 'C3 'C5 (major-scale-template))

(defun midi-notes-from-scale-down-helper (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (if (eq 'h (car (car scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr midi-notes) original-scale (cdr scale)))
	  (cons (car midi-notes)
		(midi-notes-from-scale-down-helper (cdr (cdr midi-notes)) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale-down-helper midi-notes original-scale original-scale))))

(defun midi-notes-from-scale-down (midi-notes scale)
  (assign-solfege (midi-notes-from-scale-down-helper midi-notes scale scale)
		  (prepend-tail (reverse (mapcar #'cdr (major-scale-template))))))

(defun note-p (item)
 (and (listp item) (assoc 'type item)))

(defun scale-range (n1 n2 scale)
  (let* ((eqfn (lambda (x y) (eq x (note-attr y 'name))))
	 (p1 (position n1 scale :test eqfn ))
	 (p2 (position n2 scale :test eqfn)))
    (subseq scale p1 (+ 1 p2))))

(defun make-scale-from-template (p1 p2 scale-template)
  (midi-notes-from-scale (scale-range p1 p2 (midi-notes)) scale-template scale-template))

(defun build-scale-up (from-note-pos)
  (midi-notes-from-scale (subseq (midi-notes) from-note-pos 88) (major-scale-template) (major-scale-template)))

(defun build-scale-down (from-note-pos)
  (midi-notes-from-scale-down (reverse (subseq (midi-notes) 0 (+ 1 from-note-pos)))
			      (reverse (major-scale-template))))

(defun assign-solfege (scale scale-template)
  (loop for x in scale
	for y in (grow scale scale-template)
	collect (progn
		  (setf (cdr (assoc 'solfege x)) y)
		  x)))

(defun build-scale (start-note pattern &optional (notes (midi-notes)))
  (let ((pos (note-name-position start-note)))
    (append (reverse (build-scale-down pos))
	    (rest (build-scale-up pos)))))

;; CHORDS functions
(defun chord-builder (l)
  (if l
      (cons
       (remove nil (list (car l) ; 1
		  (nth 1 (cdr l)) ;3
		  (nth 3 (cdr l)) ; 5
		  (nth 5 (cdr l)) ; 7
		  (nth 7 (cdr l)) ; 9 
		  (nth 9 (cdr l)) ; 11
		  (nth 11 (cdr l)) ; 13
		  ))
	    (chord-builder (cdr l)))))

(defun chord-take (n listofchords)
  (mapcar (lambda (l) (take n l) ) (remove-if (lambda (chord) (< (length chord) n)) listofchords)))
(defun triads (myl)
  (chord-take 3 myl))
(defun sevenths (myl)
  (chord-take 4 myl))
(defun ninths (myl)
  (chord-take 5 myl))


(defun chord-invert (chord scale)
  (append (rest chord) (list (note-octave-up (car chord) scale))))
;; Second inversion
(defun chord-over-5 (root-position-chord scale)
  (chord-invert (chord-invert root-position-chord scale) scale))
;;First inversion
(defun chord-over-3 (root-position-chord scale)
  (chord-invert root-position-chord scale))


(defun major-solfege-chords ()
  '((do . I)
    (re . II-)
    (mi . III-)
    (fa . IV)
    (so . V)
    (la . VI-)
    (ti . VII)))

(defun chord-roman-numerals (chord-list)
  (mapcar (lambda (n)
	    (cons 
	     (cdr (find (note-solfege (car n)) (major-solfege-chords) :test (lambda (x y) (eq x (car y)))))
	     n
	     ))
	  chord-list))

(defun find-chord (octave romand-num chord-list)
  (find-if
   (lambda (y)
     (eq octave (cdr (assoc 'octave (car (cdr y))))))
   (find-all-if (lambda (chord-tones) (eq romand-num (car chord-tones))) chord-list )))

(defun scale-chords (scale)
  (chord-roman-numerals (chord-builder scale)))

(defun chord-sequence (chord-sequence chords &optional (octave 4))
  (if chord-sequence
      (if (and (listp (car chord-sequence)) (eq 'octave (car (car chord-sequence))))
	  (chord-sequence (cdr chord-sequence) chords (cdr (car chord-sequence)))
	  (cons (find-chord octave (car chord-sequence) chords)
		(chord-sequence (cdr chord-sequence) chords octave)))))

(defun chords (scale chord-type-fn)
  (funcall chord-type-fn (chord-builder scale)))

(chords (make-scale 'c4) #'ninths)

;(chords (build-scale 'C4 (major-scale-template)) #'triads)

(defun chord-play (listofchords)
  ;(note-play (note-octave-down (car listofchords)))
  (dolist (n listofchords)
    (note-play n))
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
(defun note-idx (note)
  (position note (midi-notes) :test #'note-equal-p))

(defun note-octave-up (note scale)
  (let* ((other-note (nth (+ 12 (note-idx note)) (midi-notes))))
    (if other-note
	other-note)))
(defun note-octave-down (note scale)
  (let* ((other-note (nth (- (note-idx note) 12) (midi-notes))))
    (if other-note
	other-note)))

(defun note-octave (note-name)
  (parse-integer (car (multiple-value-list (cl-ppcre:scan-to-strings "\\d" (symbol-name note-name))))))

;;MIDI and Play functions
;; TODO - don't use globals

(defun panic (&optional (channel 1))
  "Stop all notes on a channel of a midi stream."
  (loop for x in (midi-notes) do (note-off x)))

(defun note-play (note &optional (velocity 80) (channel 1))
  (pm:write-short-midi *midi-out3* 0 (pm:note-on channel (note-value note) 80)))
(defun note-off (note &optional (channel 1))
  (princ (note-value note))
  (pm:write-short-midi *midi-out3* 0 (pm:note-off channel (note-value note) 0)))

(defun note-play-sleep (note)
  (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value note) 80))
  (sleep 0.25)
  (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value note) 0)))

(defun play-scale (scale)
  (dolist (n scale)
    (note-play n)
    (sleep 1)))

(defun solfege-chord (l scale)
  (let ((chord (mapcar (lambda (x)
			 (find-solfege x scale) ) l)))

    (chord-play chord)))

;(with-scale (scale-range 'C4 'G5 (make-scale 'C4))
;  (solfege-chord '(Do mi so) *current-scale*)
;  (solfege-chord '(re fa la) *current-scale*)
;  (solfege-chord '(mi so ti) *current-scale*)
;  (arp '(do mi so) *current-scale*)
;  (rarp '(do mi so) *current-scale*))

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

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y scale) (loop for x from 1 to y collect (random-note scale)))

(defun play-random (scale) (note-play (car (random-note scale))))

(defun random-scale (template)
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template (intern (format nil "~A~d" random-letter 3))
			      (intern (format nil "~A~d" random-letter 4))
			      template)))

(defun random-major-scale ()
  (random-scale (major-scale-template)))

(defun quick-test ()
  (pm-reload)
  (note-play (make-note 'C4 72 nil)))

;;(quick-test)


;; (defun inversion-test ()
;;   (chord-play (car (triads (chord-builder (scale-range 'C3 'G5 (make-scale 'c4))))))

;;   (chord-play (chord-over-3 (car (triads (chord-builder (scale-range 'C3 'G5 (make-scale 'c4)))))  (make-scale 'c4)))

;;   (chord-play (chord-over-5 (car (triads (chord-builder (scale-range 'C3 'G5 (make-scale 'c4))))) (make-scale 'c4)))

;;   (chord-play (car (triads (chord-builder (scale-range 'C4 'G5 (make-scale 'c4))))))

;;   )



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

;; TESTS

;; (with-scale (random-major-scale)
;;   (play-scale *current-scale*))

;; (with-scale (random-major-scale)
;;   (play-tonic-subdominant-dominant  *current-scale*))

;; (with-scale (random-major-scale)
;;   (play-tonic *current-scale*)
;;   (sleep 0.5)
;;   (play-subdominant *current-scale*)
;;   (sleep 0.5)
;;   (play-dominant *current-scale*)
;;   (sleep 0.5)
;;   (play-tonic *current-scale*))

;; (with-scale (random-major-scale)
;;   (solfege-chord '(DO MI SO) *current-scale*))

;; (with-scale (random-major-scale)
;;   (play-tonic-subdominant-dominant *current-scale*))

;; (with-scale (random-major-scale)
;;   (chord-builder *current-scale*))

;; (mapcar #'chord-play (triads (chord-builder (build-scale 'C4 (major-scale-template)))))
;; (mapcar #'chord-play (subseq (triads (chord-builder (build-scale 'C4 (major-scale-template)))) 16 24))

;; (with-scale (build-scale 'C4 (major-scale-template))
;;   (play-chords (sevenths (chord-sequence '(I IV V I)
;; 					 (scale-range 'C2 'G3 *current-scale*)))))

;; (with-scale (build-scale 'C4 (major-scale-template))
;;   (let* ((chord-list (take-octaves 2 (chord-builder (scale-range 'A2 'C7 *current-scale*))))
;; 	 (chords (chord-roman-numerals (triads chord-list)))
;; 	 (chord-sequence '(I VI- II- V III- VI- II- V I)))

;;     (play-chords (mapcar (lambda (rn)
;; 			   (find-chord rn chords))
;; 			 chord-sequence))))

(defun scale-helper (mydo template)
  (build-scale mydo template))

(defun make-scale (first-note &optional (template (major-scale-template)))
  (scale-helper first-note template))

;; (with-scale (make-scale 'a#4 (major-scale-template))
;;   *current-scale*)



(defun play-scale-chord-seq (chord-seq chords)
  (dolist (chord chords)
    (chord-play chord)))

(defun make-chords (start-note &optional (filter-fn #'triads) (template (major-scale-template)))
  (chord-roman-numerals (funcall filter-fn (chord-builder (build-scale start-note (major-scale-template))))))

;; (let ((chords (make-chords 'C4 #'sevenths))
;;       (chord-seq  '((octave . 2)
;; 		    I
;; 		    VI-
;; 		    II-
;; 		    (octave . 1)
;; 		    V
;; 		    (octave . 2)
;; 		    III-
;; 		    VI-
;; 		    II-
;; 		    (octave . 1)
;; 		    V
;; 		    (octave . 2)
;; 		    I
;; 		    )))
      
;;       (play-chords (mapcar #'cdr (chord-sequence chord-seq chords))))


;; (let ((chords (chord-roman-numerals
;; 	       (triads (chord-builder (build-scale 'B4 (major-scale-template))))))
;;       (chord-seq  '((octave . 2)
;; 		    I
;; 		    VI-
;; 		    II-
;; 		    (octave . 1)
;; 		    V
;; 		    (octave . 2)
;; 		    III-
;; 		    VI-
;; 		    II-
;; 		    (octave . 1)
;; 		    V
;; 		    (octave . 2)
;; 		    I
;; 		    )))
  
;;   (play-chords (mapcar #'cdr (chord-sequence chord-seq chords)))
  
;;   )
