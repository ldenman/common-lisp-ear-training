(in-package :ld-music)

(deftest test-make-note ()
  (let ((note (make-note 'c4 72 'do)))

    (check
      (equal 'note (note-attr note 'type))
      (= (note-attr note 'octave) 4)
      (equal 'do (note-solfege note))
      (= 72 (note-value note)) 
      (equal 'c4 (note-name note)))))
