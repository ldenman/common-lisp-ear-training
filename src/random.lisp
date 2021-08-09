(in-package :ld-music)

(defun find-prev-do-helper (idx scale)
  (if (eq (note-solfege (nth idx scale)) 'do)
      (nth idx scale)
      (find-prev-do-helper (- idx 1) scale)))

(defun find-prev-do (note scale)
  (position
   (find-prev-do-helper (note-idx note scale) scale)
   scale))

(defun resolve-down (note scale)
  (let* ((prev-do-pos (find-prev-do note scale)))
    (append 
     (remove-if-not
      (lambda (note)
	(position (note-solfege note) (mapcdr (major-scale-template))))
      (subseq scale prev-do-pos  (note-idx note scale)))
     (list note))))

(defun resolve-note (note scale)
  (let ((down-resolve (resolve-down note scale))
	(up-resolve  (note-to-do note scale)) )
     (if (> (length down-resolve) (length up-resolve))
	 up-resolve
	 (reverse down-resolve))))

(defun note-to-do (note scale)
  (remove-after-do
   (append (list (find-note (note-name note) scale))
	   (remove-if-not
	    (lambda (note)
	      (position (note-solfege note) (mapcdr (major-scale-template))))
	    (rest (subseq scale (note-idx (find-note (note-name note) scale) scale)))))))

(defun remove-after-do (scale)
  (if scale
      (if (eq (note-solfege (car scale)) 'do)
	  (cons (car scale) (remove-after-do '()))
	  (cons (car scale)
		(remove-after-do (cdr scale))))))

(defun random-notes ()
  (let* ((scale (make-scale 'c4))
	 (notes (scale-range 'c2 'c3 (scale-notes scale))))

    (dolist (l (loop
		 for x from 0 to 100
		 collect (random-element notes) )
	       )
      (princ (note-solfege l))
      (finish-output)
      (note-play l)
      (sleep 2)
      )))



(defun random-chromatic ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (scale-range 'c2 'c3 (scale-notes scale))))

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
	)
)))


;(pm-reload 2)
					;(random-chromatic2)
					;(random-chromatic)
;(smoke-test)

;; (dolist (note (mapcar (lambda (solfege)
;; 			 (find-solfege solfege
;; 				       (scale-range 'c3 'c4 (scale-notes (make-scale 'c4 (chromatic-scale-template))))))
;; 		       '(do ti do di do re ra do mi me do fa mi do so se do)))
;;   (note-play note)
;;   (sleep 1)
;;   )

(defun scale-octave-range-helper (o1 o2 scale)
  (if scale
      (if (and (>= (cdr (car scale)) o1) (<=  (cdr (car scale)) o2))
	  (cons (car (car scale))
		(scale-octave-range-helper o1 o2 (cdr scale)))
	  (scale-octave-range-helper o1 o2 (cdr scale)))))

(defun scale-octave-range (o1 o2 notes)
  (if notes
      (if (eq 'do (note-solfege (car notes)))
	  (scale-octave-range-helper o1 o2 (scale-octaves notes))
	  (scale-octave-range o1 o2 (cdr notes)))))

(defun play-tonic-subdominant-dominant2 (scale)
  (chord-sequence-play
   (chord-sequence '(I IV V I) (triads (chord-builder (attr 'notes scale))) scale 4 ) 0.5))

(defun play-tonic-subdominant-dominant3 (scale)
  (chord-sequence-play
   (chord-sequence '(I IV V I) (triads (chord-builder (attr 'notes scale))) scale 4 ) 0.5))

(defun sing-do ()
  (let* ((scale (random-major-scale2)))
    (play-tonic-subdominant-dominant3 scale)
    (sleep 2)
    (note-play (car (attr 'notes (scale-octave-range 3 4 scale))))
					;    (note-play (car scale))
    ))

(defun random-chromatic2 ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (scale-range 'c2 'c3 (scale-notes scale)))
	 (counter 0))

    (dolist (note (loop
		    for x from 0 to 1000
		    collect (random-element notes)))

      (if (= 0 (mod counter 4))

	  (chord-sequence-play
	   (chord-sequence '(I IV V I)
			   (triads (chord-builder
				    (scale-range 'c2 'g5 (attr 'notes (make-scale 'c4 (major-scale-template)))))))
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

(defun random-chromatic ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (scale-range 'c2 'c3 (scale-notes scale))))

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
	)
)))


;(pm-reload 2)
					;(random-chromatic2)
					;(random-chromatic)
;(smoke-test)

;; (dolist (note (mapcar (lambda (solfege)
;; 			 (find-solfege solfege
;; 				       (scale-range 'c3 'c4 (scale-notes (make-scale 'c4 (chromatic-scale-template))))))
;; 		       '(do ti do di do re ra do mi me do fa mi do so se do)))
;;   (note-play note)
;;   (sleep 1)
;;   )
