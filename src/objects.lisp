(in-package :org.oop.ld-music)

;; <note>  :: <integer>
;; <chord> ::

;; <atom>         :: = <literal atom> | <numeral> | -<numeral>
;; <literal atom> :: = <atom letter>
;;                :: = <literal atom><atom letter>
;;                :: = <literal atom><digit>
;; <numeral>      :: = <digit> | <numeral><digit>
;; <atom letter>  :: = A | B | C ... | Z
;; <digit>        :: = 0 | 1 | 2 ... | 9

(defgeneric mediant (obj)
  (:method (obj)))
(defmethod mediant ((obj minor-key))
  obj)
(defmethod mediant ((obj major-key))
  obj)

;; (defun make-tonic () 0 )

;; (defun sharpen (n)
;;   (+ 1 n))
;; (defun flatten (n)
;;  (- n 1))
;; (defun dominant (tonic)
;;   (+ 7 tonic))
;; (defun subdominant (tonic)
;;   (+ 5 tonic))
;; (defun supertonic (tonic)
;;   (+ 12 tonic))

;; (defun make-interval (n1 n2)
;;   (cons n1 n2 ))
;; (defun 5th (tonic)
;;   (make-interval tonic (dominant tonic)))
;; (defun 4th (tonic)
;;   (make-interval tonic (subdominant tonic)))
;; (defun octave (tonic)
;;   (cons tonic (supertonic tonic)))

;; (defun add-accidental (note accidental)
;;   (cons note accidental))

(defclass key ()
  ((name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (scale :accessor scale :initarg :scale)))
(defclass major-key (key) ())
(defclass minor-key (key) ())

(defun tonic (key)
  (car (scale key)))

;(defun make-key (key-name key-type key-scale) (tag 'key (cons (cons key-name key-type) key-scale)))
(defun select-accidental (note) (cdr note))

(defun note (scale)
  (lambda (degree)
    (if (atom degree)
	(nth-degree degree scale)
	(cons (nth-degree (car degree) scale) (select-accidental degree)))))

(defun nth-degree (n scale) (nth (- n 1) scale))
(defun accidental (i accidental) (cons i accidental))

(defun keys (alphabet deglis &optional (n 0))
  (if (< n (length alphabet))
      (cons alphabet (keys (mapcar (note alphabet) deglis) deglis  (1+ n)))))

(defun major-keys ()
  (let ((alphabet '(c d e f g a b)))
    (remove-duplicates (mapcar (lambda (scale) (make-instance 'major-key :name (car scale)
				      :mode 'major
				      :scale scale))
	    (append
	     (keys alphabet (list 5 6 7 1 2 3 (accidental 4 'sharp)))
	     (keys alphabet (list 4 5 6 (accidental 7 'flat) 1 2 3)))) :test #'equalp)))

(defun rotate-n (n l)
  (if (> n 0)
      (rotate-n (1- n) (append (cdr l) (list (car l))))
      l))

(defun relative-minor-key (major-key)
  (make-instance 'minor-key
   :mode 'minor
   :name (nth-degree 6 (scale major-key))
   :scale (rotate-n 5 (scale major-key))))

(defun minor-keys ()
  (mapcar #'relative-minor-key (major-keys)))

(name (car (major-keys)))
(mode (car (major-keys)))
(scale (car (major-keys)))
(minor-keys)

(defmethod print-object ((obj key) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
                     (mode mode)
		     (scale scale))
        obj
      (format stream "~a, mode: ~a, scale: ~a" name mode scale))))
