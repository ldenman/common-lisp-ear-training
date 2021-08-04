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
   (remove-if-not
	    (lambda (note)
	      (position (note-solfege note) (mapcdr (major-scale-template))))
	     (subseq scale prev-do-pos  (note-idx note scale)))))

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

(defun random-chromatic2 ()
  (let* ((scale (make-scale 'c4 (chromatic-scale-template)))
	 (notes (scale-range 'c2 'c3 (scale-notes scale)))
	 (counter 0))

    (dolist (note (loop
		    for x from 0 to 1000
		    collect (random-element notes)))

      (write-line "")
      (finish-output)

      (if (evenp counter)

	  (chord-sequence-play
	   (chord-sequence '(I IV V I)
			   (triads (chord-builder
				    (scale-range 'c2 'g5 (attr 'notes (make-scale 'c4 (major-scale-template)))))))
	   0.5)
	  )
      (setf counter (+ 1 counter))
      (note-play note)
      (sleep 1)
      (princ (note-solfege note))
      (sleep 1)
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
	(sleep 1)
	)
      (sleep 2)
      (write-line ""))))

;(pm-reload 2)
;(random-chromatic2)

;; (dolist (note (mapcar (lambda (solfege)
;; 			 (find-solfege solfege
;; 				       (scale-range 'c3 'c4 (scale-notes (make-scale 'c4 (chromatic-scale-template))))))
;; 		       '(do ti do di do re ra do mi me do fa mi do so se do)))
;;   (note-play note)
;;   (sleep 1)
;;   )
