;;;; -*- Mode: Lisp -*-

;;;; gillispie.asd

(asdf:defsystem "GILLISPIE"
  :description
  "A (simple) Common Lisp implementation of Gillespie's algorithm."

  :author "Marco Antoniotti"
  :license "BSD"

  :components ((:file "gillispie-pkg")
               (:file "sampling"
                :depends-on ("gillispie-pkg"))
               (:file "exponential"
                :depends-on ("gillispie-pkg"))
               (:file "gillispie"
                :depends-on ("sampling"
                             "exponential"))
               )
  )


;;;; end of file -- gillispie.asdf
