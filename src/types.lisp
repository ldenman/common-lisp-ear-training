(in-package :ld-music)

(defun chord-sequence? (item) (equal (tagname item) 'chord-sequence))
(defun cadence-sequence? (item) (equal (tagname item) 'cadence-sequence))
(defun note? (item) (_typep item 'note))
(defun chord-tone? (item) (equal (whichtype item) 'chord-tone))
(defun chord? (item) (equal (whichtype item) 'chord))
(defun scale? (item) (equal (whichtype item) 'scale))
(defun scale-chords? (item) (equal (whichtype item) 'scale-chords))


