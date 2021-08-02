(in-package :ld-music)

;; Helpers
(defun mapcdr (seq) (mapcar #'cdr seq))
(defun attr (item alist) (cdr (assoc item alist)))
(defun attr= (value item alist) (setf (cdr (assoc item alist)) value))
(defun random-element (l) (nth (random (length l)) l))
(defun take (n l) (subseq l 0 n))
(defun prepend-tail (lis) (append (last lis) (butlast lis)))

(defun attrs (item &rest attrlist)
  (if (cdr attrlist)
      (apply #'attrs (attr (car attrlist) item) (cdr attrlist) )
      (attr (car attrlist) item)))
(defun -> (item &rest fns)
  (if (cdr fns)
      (apply #'-> (funcall (car fns) item) (cdr fns))
      (funcall (car fns) item)))

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

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun any? (i l)
  (if l
      (if (eq i (car l))
 	  (cons i (any? i (cdr l)))
 	  (any? i (cdr l)))))

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
