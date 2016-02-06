;;;; cepl.sdl2.asd

(asdf:defsystem #:cepl.sdl2
  :description "SDL2 host for cepl"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (#:cepl #:sdl2)
  :serial t
  :components ((:file "package")
               (:file "cepl.sdl2")))
