(defsystem #:ld-music
  :description "Playing with music stuff"
  :author "Lake Denman"
  :serial t
  :depends-on (#:portmidi #:cl-ppcre)
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "readme")
		 (:file "util")
		 (:file "midi")
		 (:file "music")
		 (:file "games")
		 ))))
