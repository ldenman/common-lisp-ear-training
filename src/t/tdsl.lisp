(in-package :ld-music)

(deftest test-melodic-dsl ()
  (let* ((events (solfadsl '(do 4 re mi fa so la ti do))))
 
    (check
      ;; The generated events have the correct on-time values at 60BPM
      (equal '(0 1 2 3 4 5 6 7)
		  (mapcar
		   (lambda (event)
		     (attr 'on-time event))
		   events))

      ;; The generated events have the correct solfege values
      (equal '(do re mi fa so la ti do)
		  (mapcar
		   (lambda (event)
		     (note-solfege (attr 'note event)))
		   events)))))

