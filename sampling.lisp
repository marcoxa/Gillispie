;;;; -*- Mode: Lisp -*-

;;;; sampling.lisp

(in-package "GILLISPIE")

#+less-optimized
(defun sample (seq weights)
  (loop with ws of type (float 0.0) = (sum weights)
        with r of-type (float 0.0) = (* ws (random 1.0))
        with c of-type (float 0.0) = 0.0
        for e in seq
        and w of-type (float 0.0) in weights

        ;; do (format t ">>> ~s ~s ~s (~s)~%" e w c r)
                   
        if (>= (+ c w) r)
          return e ; (values e r)
        else
          do (incf c w)
        end
        finally ; Just in case...
          ;; (format t ">>> Last!~%")
          ;; (return (values e r))
          (return e)
        )
  )


(defun sample-ll (seq weights)
  (declare (type list seq weights))
  (loop with ws of-type (float 0.0) = (sum weights)
        with r of-type (float 0.0) = (* ws (random 1.0))
        with c of-type (float 0.0) = 0.0
        for e in seq
        and w of-type (float 0.0) in weights

        ;; do (format t ">>> ~s ~s ~s (~s)~%" e w c r)
                   
        when (>= (incf c w) r)
          return e ; (values e r)

        finally ; Just in case...
          ;; (format t ">>> Last!~%")
          ;; (return (values e r))
          (return e)
        )
  )


(defun sample-lv (seq weights)
  (declare (type list seq)
           (type vector weights))
  (loop with ws of-type (float 0.0) = (sum weights)
        with r of-type (float 0.0) = (* ws (random 1.0))
        with c of-type (float 0.0) = 0.0
        for e in seq
        and w of-type (float 0.0) across weights

        ;; do (format t ">>> ~s ~s ~s (~s)~%" e w c r)
                   
        when (>= (incf c w) r)
          return e ; (values e r)

        finally ; Just in case...
          ;; (format t ">>> Last!~%")
          ;; (return (values e r))
          (return e)
        )
  )


(defun sample-vl (seq weights)
  (declare (type vector seq)
           (type list weights))
  (loop with ws of-type (float 0.0) = (sum weights)
        with r of-type (float 0.0) = (* ws (random 1.0))
        with c of-type (float 0.0) = 0.0
        for e across seq
        and w of-type (float 0.0) in weights

        ;; do (format t ">>> ~s ~s ~s (~s)~%" e w c r)
                   
        when (>= (incf c w) r)
          return e ; (values e r)

        finally ; Just in case...
          ;; (format t ">>> Last!~%")
          ;; (return (values e r))
          (return e)
        )
  )


(defun sample-vv (seq weights)
  (declare (type vector seq weights))
  (loop with ws of-type (float 0.0) = (sum weights)
        with r of-type (float 0.0) = (* ws (random 1.0))
        with c of-type (float 0.0) = 0.0
        for e across seq
        and w of-type (float 0.0) across weights

        ;; do (format t ">>> ~s ~s ~s (~s)~%" e w c r)
                   
        when (>= (incf c w) r)
          return e ; (values e r)

        finally ; Just in case...
          ;; (format t ">>> Last!~%")
          ;; (return (values e r))
          (return e)
        )
  )


(defun sample (seq weights)
  (declare (type sequence seq weights))
  (etypecase seq
    (list
     (etypecase weights
       (list (sample-ll (the list seq) (the list weights)))
       (vector (sample-lv (the list seq) (the vector weights)))
       ))
    (vector
     (etypecase weights
       (list (sample-vl (the vector seq) (the list weights)))
       (vector (sample-vv (the vector seq) (the vector weights)))
       ))
    ))
            

;;;; end of file -- sampling.lisp
