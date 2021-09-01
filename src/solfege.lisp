(in-package :ld-music)
;; Functions having to do with solfege

;; Features
(defun solfanote-find (notes solfege octave)
  (let ((note (car notes)))
    (if (and (>= (note-relative-octave note) octave)
	     (note-solfege-equalp note solfege))
	note)))

(defun solfachord-helper (notes solfege-list &optional (start-octave 4))
  "return list of notes by solfege, relative to a starting octave, stepping incrementally by octave."
  (if (and (not (null notes)) solfege-list)
      (if solfege-list
	  (if-let (note (solfanote-find notes (car solfege-list) start-octave))
	    (cons (car notes) (solfachord-helper (cdr notes) (cdr solfege-list) (note-relative-octave (car notes))))
	    (solfachord-helper (cdr notes) solfege-list start-octave)))))

(defun solfanotes (scale solfege-list &optional (start-octave 4))
  (solfachord-helper (scale-notes scale) solfege-list start-octave))

;; (play-list-of-notes (solfanotes (make-scale 'c4 (chromatic-scale-template))
;; 				'(do mi so ti re fi la) 4))

;; (play-list-of-notes (solfanotes (make-scale 'c4 (chromatic-scale-template))
;; 				'(do mi so ti re fa la) 4))


;;;; Solfege-in-notes

(defun solfege-in-notes (notes)
  (let ((solfege-all '()))
    (dolist (note notes)
      (if (listp (note-solfege note))
	  (setf solfege-all (append solfege-all (note-solfege note)))
	  (setf solfege-all (append solfege-all (list (note-solfege note))))))
    solfege-all))

;;; note solfege-filter
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

(defun find-solfege (solfege lis)
  "DEPRECATING. find note by solfege in lis of notes. returns the first note found in list order."
  (if lis
      (if (listp (note-solfege (car lis)))
	  (if (position solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis)))

	  (if (eq solfege (note-solfege (car lis)))
	      (car lis)
	      (find-solfege solfege (cdr lis))))))

(defun find-solfege2 (solfege notes &optional (octave 4))
  "return note by SOLFEGE in NOTES relative to OCTAVE"
  (if notes
      (let ((note (car notes)))
	(if (and (note-solfege-equalp note solfege)
		 (= (note-relative-octave note) octave))
	    note
	    (find-solfege2 solfege (cdr notes) octave)))))

(defun solfege->notes (scale solfege-list &optional (octave 4))
  (if solfege-list
      (cond ((listp (car solfege-list))
	     (solfege->notes scale (cdr solfege-list) (cdr (car solfege-list))))
	    (t (cons (find-solfege2 (car solfege-list) (scale-notes scale) octave)
		     (solfege->notes scale (cdr solfege-list) octave))))))
;;;; SOLFEGE RELATED ;;;;

(defun find-prev-do-helper (idx scale)
  (if (eq (note-solfege (nth idx scale)) 'do)
      (nth idx scale)
      (find-prev-do-helper (- idx 1) scale)))

(defun find-prev-do (note scale)
  (position
   (find-prev-do-helper (note-idx note scale) scale)
   scale))

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


;;;; Note Resolutions ;;;;
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
