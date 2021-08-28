(in-package :ld-music)

(defun mapnotes (scale fn)
  (mapcar fn (attr2 scale 'notes)))
(defun maplis (l fn)
  (mapcar fn l))

(deftest test-midi-notes ()
  (check
    (= 88 (length (midi-notes)))))

(deftest test-scale-range ()
  (let ((result  (->(make-scale  'c4)
		   (scale-range3 'c4 'c5))))
    (check
      (= 8 (-> (attr2 result 'notes) (length)))
      (= 4 (-> (car (attr2 result 'notes)) (attr2 'octave)))
      (= 5 (attr2 (car (last (attr2 result 'notes))) 'octave)))))

(deftest test-chromatic-scale-solfege ()
  (check
    (equal '(LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO (DI RA) RE (RI ME) MI FA (FI SE) SO (SI LE) LA (LI TE) TI DO)
	   (-> (make-scale 'c4 (chromatic-scale-template))
	     (mapnotes #'note-solfege)))))

(deftest test-major-scale-solfege ()
  (check
    (equal
     '(LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI
       DO RE MI FA SO LA TI DO)
     (-> (make-scale 'c4 (major-scale-template))
       (mapnotes #'note-solfege)))))

(deftest test-minor-scale-solfege ()
  (check
    (equal
     '(TE DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE
       DO RE ME FA SO LE TE DO)
     (-> (make-scale 'c4 (minor-scale-template))
       (mapnotes #'note-solfege)))))

(deftest test-find-solfege ()
  (let ((solfege 'do)
	(l (list (make-note 'c4 72 'do))))
    (check (equal 'do (note-solfege (find-solfege solfege l))))))

(deftest test-find-solfege2 ()
  (let* ((notes (attr 'notes (make-scale 'c4)))
	 (found-note (find-solfege2 'do notes 5)))

    ;; verify note found in relative octave 
    (check (= 5 (note-relative-octave found-note)))
    ;; verify note found by solfege
    (check (equal 'do (note-solfege found-note)))))

(deftest test-find-solfege2-chromatic ()
  (let* ((relative-octave 5)
	 (notes (attr 'notes (make-scale 'c4 (chromatic-scale-template))))
	 (found-note (find-solfege2 'di notes relative-octave))
	 (enharmonic-note (find-solfege2 'ra notes relative-octave)))

    ;; verify note found in octave
    (check (= 5 (note-relative-octave found-note)))
    ;; verify note found by solfege
    (check (equal '(di ra) (note-solfege found-note)))
    ;; verify note found by enharmonic solfege
    (check (equal '(di ra) (note-solfege enharmonic-note)))))

(deftest test-scale-octave-range ()
  ;; verify scale-octave-range returns notes within range
  (check (equal '(4 5)
	   (-> (make-scale 'c4)
	     (lambda (scale)
	       (scale-octave-range '4 '5 (attr 'notes scale)))
	     (lambda (notes)
	       (find-all-if (lambda (n) (equal 'do (note-solfege n))) notes))
	     (maplis #'note-relative-octave)))))

;;; TEST NOTE RESOLUTIONS
(defun test-resolve-note-helper (note notes)
    (-> (resolve-note
	 note
	 notes)
      (maplis #'note-solfege)))

(defun checker-fn (a b)
  (eval `(check (equal (quote ,a) (quote ,b)))))

(defun check-note-resolutions (note->resolutions &optional (scale (make-scale 'c4)))
  (let ((notes (attr 'notes scale)))
    (let ((result t))
      (dolist (n->r note->resolutions)
	(unless (checker-fn
		 (second n->r)
		 (test-resolve-note-helper (find-solfege2 (first n->r) notes) notes))
	  (setf result 'f)))
      result)))

(deftest test-resolve-notes-major-scale ()
  ;; verify diatonic notes can resolve to DO
  (check-note-resolutions '((do  (do))
			    (re  (re do))
			    (mi  (mi re do))
			    (fa  (fa mi re do))
			    (so  (so la ti do))
			    (la  (la ti do))
			    (ti  (ti do)))))

(deftest test-resolve--chromatic-scale ()
  ;; verify chromatic notes can resolve to DO
  (check-note-resolutions '(( DO (do))
			    ( DI ((DI RA) DO))
			    ( RA ((DI RA) DO))
			    ( RE (re do) )
			    ( RI ((ri me) re do) )
			    ( ME ((ri me) re do) )
			    ( MI (mi re do) )
			    ( FA (fa mi re do) )
			    ( FI ((FI SE) FA MI RE DO) )
			    ( SO (so la ti do) )
			    ( SI ((SI LE) LA TI DO) )
			    ( LA (la ti do) )
			    ( LI ((LI TE) TI DO) )
			    ( TE ((LI TE) TI DO) )
			    ( TI (ti do)))
			  (make-scale 'c4 (chromatic-scale-template))))
;;;; END TEST NOTE RESOLUTION ;;;;


;;;; TEST DIATONIC CHORDS ;;;;
; seventh chords
(deftest test-seventh-chords ()
  (let ((chords   (-> (make-scale 'c4)
		    (scale-range3 'c4 'b6) 
		    (make-scale-chords)
		    (scale-chords)
		    (sevenths))))

    ;; verify 7ths are returned
    (check (equal '((DO MI SO TI)
		    (RE FA LA DO)
		    (MI SO TI RE)
		    (FA LA DO MI)
		    (SO TI RE FA)
		    (LA DO MI SO)
		    (TI RE FA LA)
		    (DO MI SO TI))
		  (mapcar #'chord-solfege chords)))))

; triads
(deftest test-triad-chords ()
  (let ((chords   (->
     	            (make-scale 'c4)
		    (scale-range3 'c4 'g5) 
		    (make-scale-chords)
		    (scale-chords)
		    (triads))))

    ;; verify triads are returned
    (check (equal '((DO MI SO)
		    (RE FA LA)
		    (MI SO TI)
		    (FA LA DO)
		    (SO TI RE)
		    (LA DO MI)
		    (TI RE FA)
		    (DO MI SO))
		  (mapcar #'chord-solfege chords)))))

;;;; TEST DIATONIC CHORDS ;;;;
(deftest test-chord-sequence ()
  (let* ((chord-data (make-scale-chords (make-scale 'c4)))
	 (sequence '(I II- III- IV V VI- VII I))
	 (chords (chord-sequence2 sequence chord-data)))
    ;; verify default octave
    (check
      (= 3 (-> chords
	     #'chord-sequence-chords
	     #'car
	     #'chord-notes
	     #'car
	     #'note-octave))

      ;; verify chord sequence stored
      (equal sequence (chord-sequence-romans chords))

    ;; verify solfege realized from roman numeral pattern
      (equal '((DO MI SO)
		    (RE FA LA)
		    (MI SO TI)
		    (FA LA DO)
		    (SO TI RE)
		    (LA DO MI)
		    (TI RE FA)
		    (DO MI SO))
		  (mapcar #'chord-solfege (triads (chord-sequence-chords chords)))))))



;; (deftest test-find-chord ())
;; (deftest test-chord-builder ())
;; (deftest test-chord-butroot ())
;; (deftest test-chord-butfifth ())
;; (deftest test-chord-drop-root ())
;; (deftest test-chord-invert-upper ())
;; (deftest test-make-scale-chords ())
;; (deftest test-chord-invert ())
;; (deftest test-chord-roman-numerals ())
;; (deftest test-scale-chord-filter ())  
;; (deftest test-chord-octave-filter ())
