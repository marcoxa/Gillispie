;;;; -*- Mode: Lisp -*-

;;;; gillespie-pkg.lisp


(defpackage "IT.UNIMIB.DISCO.MA.GILLISPIE"
  (:use "CL")

  (:documentation
   "A Common lisp implementation of implemention of Gillespie's
Stochastic Simulation procedure(s) for biochemical systems.")

  (:nicknames "GILLISPIE" "GILLESPIE")

  (:export
   "MODEL"
   "IS-MODEL"
   "MODELP"

   "MAKE-MODEL"
   )


  (:export
   "REACTION"
   "IS-REACTION"
   "REACTIONP"

   "MAKE-REACTION"
   )


  (:export
   "SIMULATE"
   )
  )

;;;; end of file -- gillespie-pkg.lisp
