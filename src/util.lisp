(in-package :ld-music)

;; Helpers
(defun fdbug (code)
  (dbug code)
)
(defmacro dbug (code)
  `(progn (princ (format nil "~A" ,code))
	  ,code))

(defun mapcdr (seq) (mapcar #'cdr seq))
(defun attr2 (alist item) (cdr (assoc item alist)))
(defun attr (item alist) (cdr (assoc item alist)))
(defun attr= (value item alist) (setf (cdr (assoc item alist)) value))
(defun random-element (l) (nth (random (length l)) l))
(defun take (n l) (subseq l 0 n))
(defun prepend-tail (lis) (append (last lis) (butlast lis)))

(defun attrs (item &rest attrlist)
  (if (cdr attrlist)
      (apply #'attrs (attr (car attrlist) item) (cdr attrlist) )
      (attr (car attrlist) item)))

;; Grow list2 to same size as list1
(defun grow (l1 l2 &optional (idx 0))
  (if (not (= (length l1) (length l2)))
      (grow l1 (append l2 (list (nth idx l2))) (+ 1 idx))
      l2))

(defun pairup (l1 l2)
  (if (and l1 l2)
      (cons
       (cons (car l1) (car l2))
       (pairup (cdr l1) (cdr l2)))))

(defun shuffle (sequence &optional (seed (make-random-state t)))
  (let ((s (copy-list sequence)))
    (loop for i from (length s) downto 2
       do (rotatef (elt s (random i seed))
                   (elt s (1- i))))
    s))

(defun any? (i l)
  (if l
      (if (eq i (car l))
 	  (cons i (any? i (cdr l)))
 	  (any? i (cdr l)))))

(defun lcontains-p (lx l)
  (some (lambda (x) (member x l)) lx ))

(defun every-p (lx l)
  (every (lambda (x) (member x l)) lx ))

(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

;; ROTATE SCALES
;; (defun rotate (scale) (append (cdr scale) (list (car scale))))
;; (defun rotate-n (n scale)
;;   (if (> n 0)
;;       (rotate-n (- n 1) (rotate scale))
;;       scale))

(defun map-idx (s)
  (let ((idx 0))
    (mapcar
     (lambda (i)
       (let ((r (cons i idx)))
	 (setf idx (+ 1 idx))
	 r)  )
     s)))

;; Higher level functions
;; (mapcar (car-eq 1) '((1) (2)));=> (T NIL)
(defun car-eq (item other)
  (eq (car item) other))

(defun car-fn (fn args)
  (lambda (item)
    (funcall fn item args)))

(defun flatten (structure)
  (cond ((null structure) nil)
	((atom structure) (list structure))
	(t (mapcan #'flatten structure))))

;; Split sequence by #'pred
(defun split-seq (pred seq)
  (let ((l1)
	(l2))
    (dolist (s seq)
      (if (funcall pred s)
	  (setf l1 (append l1 (list s)))
	  (setf l2 (append l2 (list s)))))
    (cons l1 l2)))
