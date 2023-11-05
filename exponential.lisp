;;;; -*- Mode: Lisp -*-

;;;; exponential.lisp

(in-package "GILLISPIE")

(defstruct (distribution (:constructor nil)))

(defstruct (exponential (:include distribution)
                        ;; (:constructor %make-exponential)
                        )
  (rate 1.0 :type float :read-only t)
  )


(defun exponentialf (rate ; I.e., \lambda.
                    &aux (r (random 1.0)))
  (declare (type float rate)
           (type (float 0.0 1.0) r))
  (* (/ rate)  (- (log r))))

;;;; end of file -- exponential.lisp
