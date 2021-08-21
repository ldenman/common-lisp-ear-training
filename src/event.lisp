(in-package :ld-music)

(defun make-event (note on-time off-time velocity)
  (list
   (cons 'note note)
   (cons 'on-time on-time)
   (cons 'off-time off-time)
   (cons 'velocity velocity)))

(defun play-event (event)
  (ensure-midi)
  (schedule-note (attr 'note event)
		 (attr 'on-time event)
		 (attr 'off-time event)
		 (attr 'velocity event)))

(defun play-events (events)
  "Play multiple notes."
  (ensure-midi)
  (dolist (event events)
    (play-event event)
    ;; workaround pm memory error/hosterror
    (sleep 0.001)))
