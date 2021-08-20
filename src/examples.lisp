(in-package :ld-music)
(defun little-sequence (&optional (division 60))
  (midi-seq-format-1
   (make-rhythmic-notes
    (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))		   
    (grow (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))
	  '(1 2 2 4 4 4 4 8 8 8 8 8 8 8 ))) division))

;(write-midi-file-format-1 "output2.midi" (little-sequence))
;(write-midi-file-format-1 "output1.midi" (little-sequence 120) 120)

(defun mary-had-a ()
  (play-events
   (rhythmic-notes->pm-events
    (make-rhythmic-notes
     (solfege->notes
      (scale-range2 'c3 'c5 (make-scale 'c3))
      '(do do do re mi mi re mi fa so))
      '(4 4 8 8 4 8 8 8 8 2))
    60)))

