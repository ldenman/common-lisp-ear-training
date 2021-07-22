;; (defun schedule (time fn &rest args)
;;   "Schedule a function to be called after some amount of seconds."
;;   (schedule-timer
;;    (make-timer
;;     (lambda ()
;;       (apply fn args))
;;     :thread t)
;;    time))

;; (defun make-bpm (bpm)
;;   bpm
;;   ) 

;; (defun beat-duration (bpm)
;;   ( / 60 bpm))


;; (defun beat-fn (lonotes)
;;   (if *beating*
;;       (progn
;;       (sleep (beat-duration 120))
;;       (schedule-timer (make-timer (beat-fn lonotes) :thread t) (beat-duration 120)))
;;       ))
;; (defvar *beating* nil)
;; (setf *beating* t)
;; (schedule-timer (make-timer (beat-fn '(((c4 . 72)))) :thread t) (beat-duration 120))

;; (defun make-sequencer (bpm)
;;   (list
;;    (make-timer
;;     (lambda ()
      
;;       )
;;     :thread t)
;;    time)
;;   )

;; (make-sequencer (make-bpm 120))
