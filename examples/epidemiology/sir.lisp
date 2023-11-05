;;;; -*- Mode: Lisp -*-

;;;; sir.lisp
;;;; The traditional SIR model originating from Kermack-McKendrick
;;;; theory (cfr., https://en.wikipedia.org/wiki/Kermack%E2%80%93McKendrick_theory).

(use-package "GILLISPIE")

(defparameter *N* 1000)
(defparameter *beta* 2.0)
(defparameter *gamma* 0.5)

(defparameter *sir-model*
  (make-model "SIR" '((s . 999) (i . 1) (r . 0))))


(defparameter *s->i*
  (make-reaction "S->I"
                 *sir-model*
                 '(s i)
                 '(-1 1)
                 '(/ (* *beta* s i) *N*)
                 ))


(defparameter *i->r*
  (make-reaction "I->R"
                 *sir-model*
                 '(i r)
                 '(-1 1)
                 '(* i *gamma*)
                 ))
                 

;;;; end of file -- sir. lisp
