(in-package :ld-music)

(defun make-level (name scale solfege)
  (list
   (cons 'name name)
   (cons 'scale scale)
   (cons 'solfege solfege)))

(defun find-level (name levels)
  (find-if
   (lambda (level)
     (string= name (attr 'name level))) levels))

(defun output-option (level length count)
  (list
   (cons 'level level)
   (cons 'length length)
   (cons 'count count)))

(defun levels ()
  (mapcar (lambda (x) (attr 'level x)) outputs))

(defun make-level-1 (name solfege)
  (make-level (format nil "l1-~a" name) (scale-range2 'c2 'c6 (make-scale 'c4)) solfege))
(defun make-level-2 (name solfege)
  (make-level (format nil "l2-~a" name) (scale-range2 'c2 'c6 (make-scale 'c4)) solfege))

(setf outputs
      (list
       (output-option
	(make-level  "do" (scale-range2 'c1 'c7 (make-scale 'c4)) '(do))
	50 1)
       (output-option
	(make-level-1 "do-re" '(re))
	50 1)
       (output-option
	(make-level-1 "do-mi" '(mi))
	50 1)
       (output-option
	(make-level-1 "do-fa" '(fa))
	50 1)
       (output-option
	(make-level-1 "do-so" '(so))
	50 1)
       (output-option
	(make-level-1 "do-la" '(la))
	50 1)
       (output-option
	(make-level-1 "do-ti" '(ti))
	50 1)

       (output-option
	(make-level-2 "do-mi-so"  '(mi so))
	50 1)
       (output-option
	(make-level-2 "re-fa-la"  '(re fa la))
	50 1)
       (output-option
	(make-level-2 "mi-so-ti"  '(mi so ti) )
	50 1)
       (output-option
	(make-level-2 "fa-la"  '(fa la))
	50 1)
       (output-option (make-level-2 "so-ti-re"  '(so ti re))
		      50 1)
       (output-option (make-level-2 "la-do-mi"  '(la do mi))
		      50 1)
       (output-option (make-level-2 "ti-re-fa"  '(ti re fa))
		      50 1)

       (output-option
	(make-level "re-mi-fa-so-la-ti-three-octave" (scale-range2 'c2 'c5 (make-scale 'c4)) '(re mi fa so la ti))
	10 5)
       (output-option (make-level "ra-re-mi-fa-so-la-ti-three-octave"
				  (scale-range2 'c2 'c5 (make-scale 'c4 (chromatic-scale-template)))
				  '(re ra mi fa so la ti) )

		      10 5)
       (output-option (make-level "ra-re-me-mi-fa-so-la-ti-three-octave"
				  (scale-range2 'c2 'c5 (make-scale 'c4 (chromatic-scale-template)))
				  '(ra re me mi fa so la ti) )
		      10 5)
       (output-option (make-level "ra-re-me-mi-fa-fi-so-la-li-ti-three-octave"
				  (scale-range2 'c2 'c5 (make-scale 'c4 (chromatic-scale-template)))
				  '(ra re me mi fa fi so la li ti) )
		      10 5)
       (output-option (make-level "do-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(do))
		      10 5)
       (output-option 
	(make-level "re-two-octaves"
		    (scale-range2 'c2 'c4 (make-scale 'c4)) '(re) )
	10 5)

       (output-option  (make-level "re-mi-two-octaves"
				   (scale-range2 'c2 'c4 (make-scale 'c4)) '(re mi) )
		       10 5)
       (output-option (make-level "re-mi-fa-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4)) '(re mi fa))
		      10 5)
       (output-option (make-level "re-mi-fa-so-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4)) '(re mi fa so))
		      10 5)
       (output-option (make-level "re-mi-fa-so-la-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4)) '(re mi fa so la))
		      10 5)
       (output-option (make-level "re-mi-fa-so-la-ti-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4)) '(re mi fa so la ti))
		      10 5)
       (output-option 
	(make-level "do-di-two-octaves"
		    (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(do di) )
	10 5)

       (output-option  (make-level "re-ra-two-octaves"
				   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(re ra) )
		       10 5)

       (output-option  (make-level "mi-me-two-octaves"
				   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(mi me) )
		       10 5)

       (output-option  (make-level "fa-fi-two-octaves"
				   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(fa fi) )
		       10 5)

       (output-option  (make-level "la-li-two-octaves"
				   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(la li) )
		       10 5)
       (output-option (make-level "te-ti-two-octaves"
				  (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template))) '(te ti) )
		      10 5)
       (output-option 
	(make-level "re-ra-mi-fa-so-la-ti-two-octaves"
		    (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template)))
		    '(re ra mi fa so la ti) )
	20 5)

     (output-option  (make-level "ra-re-mi-fa-fi-so-la-ti-two-octaves"
		   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template)))
				 '(re ra mi fa fi so si la ti) )
		     20 5)

     (output-option  (make-level "ra-re-me-mi-fa-fi-so-si-la-li-ti-two-octaves"
		   (scale-range2 'c2 'c4 (make-scale 'c4 (chromatic-scale-template)))
				 '(ra re me mi fa fi so si la li ti) )
		     20 5)))

(defun render-all-videos ()
  (dolist (output outputs)
    (let ((level (attr 'level output)))
      (render-examples level (attr 'count output)
		       (attr 'length output)))))

(defun render-output (output)
  (let ((level (attr 'level output)))
    (render-examples level (attr 'count output)
		     (attr 'length output))))

(defun render-some-videos (output)
  (dolist (output (take n outputs))
    (let ((level (attr 'level output)))
      (render-examples level (attr 'count output)
		       (attr 'length output)))))

;(render-some-videos 1) 

;; make list of note resolutions
(defun resolve-notes (level notes)
  (remove-if #'null (append
		     (list (find-if (lambda (x) (eq (note-solfege (car notes)) 'do)) notes))
		     (remove-if #'null (if notes
					   (append
					    (if (not (eq (note-solfege (car notes)) 'do))
						(resolve-note (car notes)
							      (attr 'notes (attr 'scale level))))
					    (resolve-notes level (cdr notes))))))))

(defun do-try (level length seed result)
  (find-if
   (lambda (note)
     (and
      (note-solfege-member-p note (attr 'solfege level))
      (not (note-equal-p note (car (last result))))))

   (shuffle (attr 'notes (attr 'scale level))
	    (if (eq 't seed)
		(sb-ext:seed-random-state seed)
		(sb-ext:seed-random-state (+ length seed))))))

(defun generate-notes-helper (level length seed result)
  (if (>= length 0)
      (let ((try (do-try level length seed result)))
	(if (eq try (car (last result)))
	    (generate-notes-helper level (- length 1) seed result)
	    (generate-notes-helper level (- length 1) seed (append result (list try)))))
      result))

(defun note-solfege-filter (notes solfege)
  (dolist (note notes)
    (if (listp (note-solfege note))
	(if (member solfege (note-solfege note))
	    (progn
	      (attr= solfege 'solfege note)
	      note)))
  notes)      )

(defun solfege-filter (notes losolfege)
  (dolist (solfege losolfege)
    (note-solfege-filter notes solfege))
  notes)

(defun generate-notes (level length &optional (seed t))
  (let ((result (generate-notes-helper level length seed '())))
    (if (and 	(every-p (remove 'do (attr 'solfege level))
			 (solfege-in-notes result))
		(> (length result) length))
	(remove-if #'null result)


	(generate-notes-helper level length seed result))))

(defun generate-notes2 (level length &optional (seed t))
  (solfege-filter
   (let ((result (generate-notes-helper level length seed '())))
     (if (and 	(every-p (remove 'do (attr 'solfege level))
			 (solfege-in-notes result))
		(> (length result) length))
	 (remove-if #'null result)
	 (generate-notes-helper level length seed result)))
   (attr 'solfege level)))

(defun note-solfege-member-p (note lofsolfege)
  (if (listp (note-solfege note))
      (some (lambda (s) (member s lofsolfege)) (note-solfege note))
      (member (note-solfege note) lofsolfege)))

(defun solfege-in-notes (notes)
  (let ((solfege-all '()))
    (dolist (note notes)
      (if (listp (note-solfege note))
	  (setf solfege-all (append solfege-all (note-solfege note)))
	  (setf solfege-all (append solfege-all (list (note-solfege note))))))
    solfege-all))

(defun generate-examples-helper2 (level length s)
  (if (>= length (length (attr 'solfege level)))
      (let* ((notes (generate-notes2 level length s)))
	(if (every-p (remove 'do (attr 'solfege level))
			 (solfege-in-notes notes))
	    (resolve-notes
	     level
	     notes)))))

(defun generate-example (level length s)
  (let ((examples (generate-examples-helper2 level length s)))
    (if examples
	(remove-if #'null examples)
	(generate-example level (+ 1 length) s))))

(defun render-level (level name length seed)
    (let ((x (generate-example level length seed)))
      (if x
	  (progn
	  (prepare-remotion! x)
	  (render-video2 name)))))

(defun render-examples (level c length) 
  (dotimes (n c)
    (render-level level (format nil "~a-~d" (attr 'name level) n) length n)))







