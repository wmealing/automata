;;;; automata.asd

(asdf:defsystem #:automata
  :description "Describe automata here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "automata")))


(asdf:defsystem #:automata/tests
  :depends-on (:automata :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
