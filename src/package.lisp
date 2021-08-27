(defpackage :ld-music
  (:use :cl :sb-ext :arrow-macros) ;; being sbcl dependant rn
  (:export
   :with-scale
   :make-scale
   :major-scale-template
   :c4
   :note
   :notes
   :note-solfege
   :note-value
)
  )
