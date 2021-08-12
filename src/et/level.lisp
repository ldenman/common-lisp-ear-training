(in-package :ld-music)

(defun make-level (name scale solfege seed)
  (acons 'name name
	 (list
	  (cons 'scale scale)
	  (cons 'seed seed)
	  (cons 'solfege solfege))))

(defun generate-notes (level length)
  (let ((result '()))
    (dotimes (n length)
      (setf result
	    (append result
		    (list (find-if
			   (lambda (note)
			     (member (note-solfege note)
				     (attr 'solfege level)))
			   (shuffle
			    (attr 'notes
				  (attr 'scale level))
			    (sb-ext:seed-random-state
			     (+ n (attr 'seed level)))))))))
    result))

(defun generate-levels (levels length)
  (mapcar
   (lambda (level)
     (mapcar (lambda (note)
	       (resolve-note note (attr 'notes
					(attr 'scale level))))
	     (generate-notes level length))
     )levels))

(generate-levels levels 10)

(defun contains-p (x l)
  (if (member x l) t))

(defun lcontains-p (lx l)
      (every (lambda (solfege) (contains-p solfege l)) lx))

(defun good-matches (level length &optional (n 0 ) (limit 100))
  (if (< n limit)
      (if (lcontains-p
	   (attr 'solfege level)
	   (mapcar #'note-solfege (generate-notes level length)))
	  (append
	   (good-matches level length (+ n 1) limit)
	   (generate-notes level n)
)
	  (good-matches level length (+ n 1) limit))))

(car (remove-if #'null (good-matches (second levels) 10)))

(defun foo (level)
(let ((guesses (loop for x from 0 to 1000
		  collect (generate-notes level 10))))
  (remove-if-not (lambda (guess)
		   (lcontains-p
		    (attr 'solfege level)
		    (mapcar #'note-solfege guess))) guesses)))

(foo (fifth levels))

(length (good-matches (second levels) 10))

(contains-p 1 '(1))
(lcontains-p '(1) '(1))
(contains-p 'foo '(foo))
(lcontains-p '(1 2 3) '(1 2 3))

(defvar levels '())
(setf levels
      (list
       (make-level '1
		   (scale-range2 'c3 'c4 (make-scale 'c4)) '(do) 1)
       (make-level '2
		   (scale-range2 'c3 'c4 (make-scale 'c4)) '(do re) 1)
       (make-level '3
		   (scale-range2 'c3 'c4 (make-scale 'c4)) '(do re mi) 1)
       (make-level '4
		   (scale-range2 'c3 'c4 (make-scale 'c4)) '(do re mi fa) 100)
       (make-level '5
		   (scale-range2 'c3 'c4 (make-scale 'c4)) '(do re mi fa so) 1)))
