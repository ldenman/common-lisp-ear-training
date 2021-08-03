(in-package :ld-music)

(defun note-name-position (note-name &optional (scale (midi-notes)))
  (position note-name scale :test (lambda (x y) (equal x (note-name y)))))

(defun note-octave2 (note scale)
  (cdr (find-if (lambda (n)
	     (note-equal-p note (car n))) (scale-octaves scale))))

;; Note selector functions
(defun note-attr (note attr) (cdr (assoc attr note)))
(defun note-name (note) (note-attr note 'name))
(defun note-value (note) (note-attr note 'value))
(defun note-solfege (note) (note-attr note 'solfege))
(defun note-equal-p (x y)
  (and (equal (note-value x)
	      (note-value y))
       (equal (note-name x)
	      (note-name y))))
(defun note-idx (note &optional (scale (midi-notes)))
  (position note scale :test #'note-equal-p))

(defun note-octave-up (note scale)
  (let* ((other-note (nth (+ 12 (note-idx note)) (midi-notes))))
    (when other-note
      (attr= (note-solfege note) 'solfege other-note)
      other-note)))
(defun note-octave-down (note scale)
  (if (>= (- (note-idx note) 12) 0)
      (let* ((other-note (nth (- (note-idx note) 12) (midi-notes))))
	(when other-note
	  (attr= (note-solfege note) 'solfege other-note)
	  other-note))))

(defun note-octave (note-name)
  (parse-integer (car (multiple-value-list (cl-ppcre:scan-to-strings "\\d" (symbol-name note-name))))))

(defun find-note (name &optional (scale (midi-notes)))
  (find-if (lambda (note) (eq (note-name note) name)) scale))

(defun make-note (name value solfege)
  (list
   (cons 'type 'note)
   (cons 'name name)
   (cons 'value value)
   (cons 'solfege solfege)
   (cons 'octave (note-octave name ))))
