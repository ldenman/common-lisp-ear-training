(in-package :ld-music)

(defun random-notes ()
  (let* ((scale (make-scale 'c4))
	 (notes (note-range 'c2 'c3 (scale-notes scale))))

    (dolist (l (loop
		 for x from 0 to 100
		 collect (random-element notes) )
	       )
      (princ (note-solfege l))
      (finish-output)
      (note-play l)
      (sleep 2))))

(defun random-chromatic ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (note-range 'c2 'c3 (scale-notes scale))))

    (dolist (note-list (loop
			 for x from 0 to 1000
			 collect (resolve-note (random-element notes) notes)))
      (dolist (note note-list)
	(princ (note-solfege note))
	(finish-output)
	(note-play note)
	(write-line "")
	(sleep 0.25)
;	(note-play note)
	))))

(defun sing-do ()
  (let* ((scale (random-major-scale2)))
    (play-tonic-subdominant-dominant3 scale)
    (sleep 2)
    (note-play (car (scale-octave-range 3 4 (scale-notes scale))))
					;    (note-play (car scale))
    ))

(defun random-chromatic2 ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (note-range 'c2 'c3 (scale-notes scale)))
	 (counter 0))

    (dolist (note (loop
		    for x from 0 to 1000
		    collect (random-element notes)))

      (if (= 0 (mod counter 4))

	  (chord-sequence-play
	   (chord-sequence '(I IV V I)
			   (triads (chord-builder
				    (note-range 'c2 'g5 (scale-notes (make-scale 'c4 (major-scale-template)))))))
	   0.5)
	  )

      (note-play note)
      (sleep 1)
      (note-play note)
      (sleep 0.25)
      (dolist (n (resolve-note note notes))
	(princ (note-solfege n))
	(finish-output)
	(note-play n)
	(write-line "")
	(sleep 0.25)
;	(note-play note)
	)

      (note-play (car notes))
      (note-play note)
      (sleep 1)
      (setf counter (+ 1 counter))

))

  )

(defun random-chromatic3 ()


  (let*
((scale (random-chromatic-scale))
	 (notes (scale-octave-range 2 3 (scale-notes scale)))
	 (last-idx (position (car (last notes)) (scale-notes scale)))
	 (next-do (nth (+ 1 last-idx) (scale-notes scale)))
	 (notes2 (append notes (list next-do)))
	 (counter 0))

    (dolist (note (loop
		    for x from 0 to 1000
		    collect (random-element notes2)))

      ;; (if (= 0 (mod counter 4))
      ;; 	  (play-tonic-subdominant-dominant3 scale))

      (dolist (n (resolve-note note notes2))
	(princ (note-solfege n))
	(finish-output)
	(note-play n)
	(write-line "")
	(sleep 1)
					;	(note-play note)
	)

      ;; (note-play (car notes))
      ;; (note-play note)
      (sleep 1)
      (setf counter (+ 1 counter))

))

  )

