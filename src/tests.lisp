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
     '(LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO RE MI FA SO LA TI DO)
     (-> (make-scale 'c4 (major-scale-template))
       (mapnotes #'note-solfege)))))

(deftest test-minor-scale-solfege ()
  (check
    (equal
     '(TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO RE ME FA SO LE TE DO)
     (-> (make-scale 'c4 (minor-scale-template))
       (mapnotes #'note-solfege)))))

(deftest test-find-solfege ()
  (let ((solfege 'do)
	(l (list (make-note 'c4 72 'do))))
    (check
      (equal 'do
	     (note-solfege (find-solfege solfege l))))))

(deftest test-find-solfege2 ()
  (let* ((solfege 'do)
	 (notes (attr 'notes (make-scale 'c4)))
	 (found-note (find-solfege2 solfege notes 5)))
    (check (= 5 (note-relative-octave found-note)))
    (check (equal 'do (note-solfege found-note)))))

(deftest test-find-solfege2-chromatic ()
  (let* ((solfege 'di)
	 (notes (attr 'notes (make-scale 'c4 (chromatic-scale-template))))
	 (found-note (find-solfege2 solfege notes 5)))
    (check found-note)
    (check (= 5 (note-relative-octave found-note)))
    (check (equal '(di ra) (note-solfege found-note)))))

(deftest test-scale-octave-range ()
  (check
    (equal '(4 5)
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

(defun check-note-resolutions (assertions &optional (scale (make-scale 'c4)))
  (let ((notes (attr 'notes scale)))
    (let ((result t))
      (dolist (assertion assertions)
	(unless (checker-fn
		   (second assertion)
		   (test-resolve-note-helper (find-solfege2 (car assertion) notes) notes))
	  (setf result 'f)))
      result)))

(deftest test-resolve-notes-major-scale ()
  (check-note-resolutions '((do  (do))
			    (re  (re do))
			    (mi  (mi re do))
			    (fa  (fa mi re do))
			    (so  (so la ti do))
			    (la  (la ti do))
			    (ti  (ti do)))))

(deftest test-resolve--chromatic-scale ()
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
  (let ((chords   (->
     	            (make-scale 'c4)
		    (scale-range3 'c4 'b6) 
		    (make-scale-chords)
		    (scale-chords)
		    (sevenths)
		    (maplis #'chord-notes))))
    (check (equal '((DO MI SO TI)
		    (RE FA LA DO)
		    (MI SO TI RE)
		    (FA LA DO MI)
		    (SO TI RE FA)
		    (LA DO MI SO)
		    (TI RE FA LA)
		    (DO MI SO TI))
		  (mapcar (lambda (n) (mapcar #'note-solfege n)) chords)))))

; triads
(deftest test-triad-chords ()
  (let ((chords   (->
     	            (make-scale 'c4)
		    (scale-range3 'c4 'g5) 
		    (make-scale-chords)
		    (scale-chords)
		    (triads)
		    (maplis #'chord-notes))))
    (check (equal '((DO MI SO)
		    (RE FA LA)
		    (MI SO TI)
		    (FA LA DO)
		    (SO TI RE)
		    (LA DO MI)
		    (TI RE FA)
		    (DO MI SO))
		  (mapcar (lambda (n) (mapcar #'note-solfege n)) chords)))))

;;;; TEST DIATONIC CHORDS ;;;;
