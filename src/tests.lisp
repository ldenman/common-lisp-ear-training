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
    (equalp '(4 5)
	    (-> (make-scale 'c4)
	      (lambda (scale)
		(scale-octave-range '4 '5 (attr 'notes scale)))
	      (lambda (notes)
		(find-all-if (lambda (n) (equal 'do (note-solfege n))) notes))
	      (maplis #'note-relative-octave)))))

(defun test-resolve-note-helper (note notes)
    (-> (resolve-note
	 note
	 notes)
      (maplis #'note-solfege)))

(deftest test-resolve-notes-major-scale ()
  (let* ((notes (scale-notes (make-scale 'c4)))
	 (DO (find-solfege2  'do notes))
	 (RE (find-solfege2  're notes))
	 (MI (find-solfege2  'mi notes))
	 (FA (find-solfege2  'fa notes))
	 (SO (find-solfege2  'so notes))
	 (LA (find-solfege2  'la notes))
	 (TI (find-solfege2  'ti notes)))
    (check
      (equal '(do) (test-resolve-note-helper DO notes)))
      (equal '(re do) (test-resolve-note-helper RE notes))
      (equal '(mi re do) (test-resolve-note-helper MI notes))
      (equal '(fa mi re do) (test-resolve-note-helper fa notes))
      (equal '(so la ti do) (test-resolve-note-helper SO notes))
      (equal '(la ti do) (test-resolve-note-helper LA notes))
      (equal '(ti do) (test-resolve-note-helper TI notes))))

(deftest test-resolve-notes-chromatic-scale ()
  (let* ((notes (scale-notes (make-scale 'c4 (chromatic-scale-template))))
	 (DO (find-solfege2  'do notes))
	 (DI (find-solfege2  'di notes))
	 (RA (find-solfege2  'ra notes))
	 (RE (find-solfege2  're notes))
	 (MI (find-solfege2  'mi notes))
	 (FA (find-solfege2  'fa notes))
	 (SO (find-solfege2  'so notes))
	 (LA (find-solfege2  'la notes))
	 (TI (find-solfege2  'ti notes)))

    (check 
      (equal '(do) (test-resolve-note-helper DO notes))
      (equal '(re do) (test-resolve-note-helper RE notes))
      (equal '(mi re do) (test-resolve-note-helper MI notes))
      (equal '(fa mi re do) (test-resolve-note-helper fa notes))
      (equal '(so la ti do) (test-resolve-note-helper SO notes))
      (equal '(la ti do) (test-resolve-note-helper LA notes))
      (equal '(ti do) (test-resolve-note-helper TI notes))
      (equal '((DI RA) DO)
	     (test-resolve-note-helper DI notes))
      (equal '((DI RA) DO)
	     (test-resolve-note-helper RA notes)))))

(reset-tests)
(run-tests)

