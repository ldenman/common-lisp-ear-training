(in-package :ld-music)

(defun little-sequence (&optional (division 60))
  (midi-seq-format-1
   (make-rhythmic-notes
    (take 15 (scale-notes (scale-range 'c3 'c5 (make-scale 'c4))))		   
    (grow (take 15 (scale-notes (scale-range 'c3 'c5 (make-scale 'c4))))
	  '(1 2 2 4 4 4 4 8 8 8 8 8 8 8 ))) division))

;(write-midi-file-format-1 "output2.midi" (little-sequence))
;(write-midi-file-format-1 "output1.midi" (little-sequence 120) 120)

(defun seq (solfege-melody rhythm &optional (scale (scale-range2 'c3 'c5 (make-scale 'c3))) (bpm 60))
  (rhythmic-notes->pm-events
   (make-rhythmic-notes
    (solfege->notes
     (scale-range 'c3 'c5 (make-scale 'c3))
     '(do do do re mi mi re mi fa so))
    '(4 4 8 8 4 8 8 8 8 2))
   bpm))

(defun row-row-row-your-boat ()
  (play-events
   (seq '(do do do re mi
	  mi re mi fa so
	  do do do so so so 
	  mi mi mi do do do
	  so fa mi re do)
	'(4 4 8 8 4
	  8 8 8 8 2
	  8 8 8 8 8 8
	  8 8 8 8 8 8
	  4 8 8 8 1))))

(row-row-row-your-boat)

(defun parse-seq-item (item)
  (let* ((solfege (symbol-name item))
	(y (cl-ppcre:scan-to-strings "(\\d)" solfege)))
    (list
     (cons 'solfege solfege)
     (cons 'rhythm y))))

(parse-seq-item 'do3:3)
