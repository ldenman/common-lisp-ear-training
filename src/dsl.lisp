(in-package :ld-music)

(defun song (start-note solfege rhythm)
  (make-rhythmic-notes
   (solfege->notes
    (scale-range2 'c3 'c5 (make-scale 'c3))
    solfege)
   rhythm))

(defun play-song (song)
  (play-events (rhythmic-notes->pm-events song 60)))

; (pm-reload 2)
(play-song
 (song 'c4
       '(do re mi
	 fa so la
	 do mi re
	 do)
       '(4 4 4
	 8 8 8
	 4 4 4
	 1)))
