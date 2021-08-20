(in-package :ld-music)

(defun read-in-frame-file (infile)
  (with-open-file (stream infile)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun parse-midi-to-json ()
  (run-program "/usr/bin/bash" '("--login" "-c" "node src/api/parseMidiToJson.js") :directory "/home/lake/src/remotion-midi-piano-vizualiser/"))

;(ql:quickload :uiop)

(defun render-video ()
  (uiop:launch-program "yarn run build --concurrency 16" :directory "/home/lake/src/remotion-midi-piano-vizualiser/" :output :interactive))

(defun render-video2 (name)
  (uiop:run-program (format nil "yarn run prepare --concurrency 16 && npx remotion render src/index.tsx PianoComposition ~s.mp4 --concurrency 16 " name)
		       :directory "/home/lake/src/remotion-midi-piano-vizualiser/" :output :interactive))

;(render-video)

(defvar *remotion-servers* '())
(defun preview-video ()
  (let ((proc (uiop:launch-program "npm start" :directory "/home/lake/src/remotion-midi-piano-vizualiser/" :output :interactive)))
    (setf *remotion-servers* (append *remotion-servers* (list proc)))
    proc))

;; (preview-video) 
;; (car (last *remotion-servers*))
;; (mapcar #'uiop:close-streams *remotion-servers*)
;; (mapcar #'kill-process *remotion-servers*)
;; (uiop:terminate-process (car *remotion-servers*) :urgent t )
;; (last (mapcar #'uiop:process-info-pid *remotion-servers*))(104942)

(defun kill-process (process-info)
  (uiop:run-program (format nil "/usr/bin/kill ~a" (uiop:process-info-pid process-info) )))

(defun print-entry (key value)
  (format t "~A => ~A~%" key value))

(defun write-solfege-frames (outfile string)
  (with-open-file (stream outfile
			  :direction :output
			  :if-exists :supersede)
    (format stream string)))

(defun find-solfege-by-midi-value (value notes &optional (enharmonic-switch nil))
  (let ((x (find-if (lambda (note) (equal value (write-to-string (note-value note)))) notes)))
    (if x
	(if (listp (note-solfege x))
	    (if enharmonic-switch
		(symbol-name (car (cdr (note-solfege x))))
		(symbol-name (car (note-solfege x))))	
	    (symbol-name (note-solfege x)))

	"HUH")))

(defun update-json (json notes)
  (let ((newht (make-hash-table))
	(myht (gethash "activeFramePerNote" json)))
    (loop for k being each hash-key of myht using (hash-value v)
	  do (setf (gethash (find-solfege-by-midi-value k notes t) newht) (append (gethash (find-solfege-by-midi-value k notes t) newht) v))
    (setf (gethash "activeFramePerNote" json) newht))
    json))

(defun decode-json (infile)
  (let* ((frame-json (read-in-frame-file infile))
	 (decoded-json (with-input-from-string (s (car frame-json))
			 (yason:parse s))))
    decoded-json))

(defun prepare-remotion! (notes &optional (outfile "~/src/remotion-midi-piano-vizualiser/input.mid") )
      (write-midi-file-format-0 outfile (midi-seq-format-0 notes))
      (parse-midi-to-json)

      (write-solfege-frames
       "/home/lake/src/remotion-midi-piano-vizualiser/src/api/solfege.json"
       (with-output-to-string (*standard-output*)
	 (yason:encode (update-json (decode-json "~/src/remotion-midi-piano-vizualiser/src/api/midi.json") notes)))))



;(test)




