(in-package :ld-music)
;; Background
;; This project is my exploration in representing musical information with LISP.
;; What kind of musical information is there to represent? To start... Notes, Scales, and Chords.

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

;;=> (((TYPE . NOTE) (NAME . C4) (VALUE . 72) (SOLFEGE . DO) (OCTAVE . 4))
;;  ((TYPE . NOTE) (NAME . D4) (VALUE . 74) (SOLFEGE . RE) (OCTAVE . 4))
;;  ((TYPE . NOTE) (NAME . E4) (VALUE . 76) (SOLFEGE . MI) (OCTAVE . 4))
;;  ((TYPE . NOTE) (NAME . F4) (VALUE . 77) (SOLFEGE . FA) (OCTAVE . 4))
;;  ((TYPE . NOTE) (NAME . G4) (VALUE . 79) (SOLFEGE . SO) (OCTAVE . 4))
;;  ((TYPE . NOTE) (NAME . A5) (VALUE . 81) (SOLFEGE . LA) (OCTAVE . 5))
;;  ((TYPE . NOTE) (NAME . B5) (VALUE . 83) (SOLFEGE . TI) (OCTAVE . 5))
;;  ((TYPE . NOTE) (NAME . C5) (VALUE . 84) (SOLFEGE . DO) (OCTAVE . 5)))

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

;;=> (nth 0 (test-chord-builder))
;; (((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . C2) (VALUE . 48) (SOLFEGE . DO) (OCTAVE . 2))
;;   (DEGREE . 1))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . E2) (VALUE . 52) (SOLFEGE . MI) (OCTAVE . 2))
;;   (DEGREE . 3))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . G2) (VALUE . 55) (SOLFEGE . SO) (OCTAVE . 2))
;;   (DEGREE . 5))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . B3) (VALUE . 59) (SOLFEGE . TI) (OCTAVE . 3))
;;   (DEGREE . 7))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . D3) (VALUE . 62) (SOLFEGE . RE) (OCTAVE . 3))
;;   (DEGREE . 9))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . F3) (VALUE . 65) (SOLFEGE . FA) (OCTAVE . 3))
;;   (DEGREE . 11))
;;  ((TYPE . CHORD-TONE)
;;   (NOTE (TYPE . NOTE) (NAME . A4) (VALUE . 69) (SOLFEGE . LA) (OCTAVE . 4))
;;   (DEGREE . 13)))


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



;(with-scale (scale-range 'C4 'G5 (make-scale 'C4))
;  (solfege-chord '(Do mi so) *current-scale*)
;  (solfege-chord '(re fa la) *current-scale*)
;  (solfege-chord '(mi so ti) *current-scale*)
;  (arp '(do mi so) *current-scale*)
;  (rarp '(do mi so) *current-scale*))



;; (chord-sequence-play
;;  (chord-sequence-chords
;;   (chord-sequence
;;    '((octave . 3) I (octave . 3) VI- (octave . 3)  II- (octave . 2) V (octave . 3) I)
;;    (chords (make-scale 'C4) #'sevenths))))

;; (chords (make-scale 'C4) #'sevenths)

;; (chord-play (chord-invert (car (chords (scale-range 'c3 'G5 (make-scale 'c4)))) (make-scale 'c4)))

;; (chord-invert (chord-remove-degree (chord-upper (car (cdr (chords (scale-range 'c3 'G5 (make-scale 'c4)) #'sevenths)))) 5) (make-scale 'c4))


;; (-> (make-scale-chords (make-scale 'C2))
;;     (scale-chord-filter #'chord-type-filter #'sevenths)
;;     (scale-chord-filter #'chord-filter #'chord-butfifth)
;;     (chord-seq '(II-
;; 		 (octave . 2)
;; 		 V
;; 		 (octave . 3)
;; 		 I
;; 		 (octave . 3)
;; 		 VI-
;; 		 (octave . 3)
;; 		 II-
;; 		 (octave . 2)
;; 		 V
;; 		 (octave . 3)
;; 		 I
;; 		 I
;; 		 ) 3)

;;       #'chord-seq-play)
