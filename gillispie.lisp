;;;; -*- Mode: Lisp -*-

;;;; gillispie-pkg.lisp

(in-package "GILLISPIE")


;;; Model
;;; -----

(defstruct (model
            (:constructor
             %new-model (name
                         id
                         reactions
                         initial-state
                         state
                         state-changes
                         t0
                         ))
            )

  (name "Model" :type string :read-only t)
  (id 'some-model :type symbol :read-only t)

  reactions
  initial-state
  state
  state-changes
  (t0 0.0 :type float)
  (times #() :type (vector float))
  )


(defun is-model (x) (model-p x))


(defun make-model (name initial
                        &key
                        (id (gentemp name))
                        (reactions ())
                        &allow-other-keys)
  (assert (typep initial '(or sequence hash-table)))

  ;; See under "Utilities" for MAKE-DICT and COPY-DICT.
  
  (let ((initial-state
         ;; No need to ETYPECASE given the ASSERT above.
         (typecase initial
           (sequence
            (if (every #'symbolp initial)
                (make-dict (mapcar (lambda (v) (cons v 0)) initial))
                ;; Else assume it it a list of pairs (i.e., conses).
                (make-dict initial)))
           (hash-table initial)))
        )
    (%new-model name
                id
                reactions
                initial-state
                (copy-dict initial-state)
                ()
                0.0)))


(defmethod add-specie ((m model) (v symbol) &optional (init 0))
  (assert (>= init 0))
  (unless (nth-value 1 (gethash v (model-initial-state m)))
    (setf (gethash v (model-initial-state m)) init
          (gethash v (model-state m)) init))
  )


(defmethod set-specie ((m model) (v symbol) &optional (init 0))
  (assert (>= init 0))
  (assert (nth-value 1 (gethash v (model-initial-state m))))
  (setf (gethash v (model-state m)) init)
  )


(defmethod get-specie ((m model) (v symbol) &key (time :last))
  (ecase time
    (:last (gethash v (model-state m)))
    (:initial (gethash v (model-initial-state m)))))


(defun dump-current-state (m &optional (stream t))
  (declare (type model m))
  (format stream
          "~&GILLESPIE: current state~%~:{~A~4T : ~S~%~}~%"
          (loop for v being the hash-key of (model-state m)
                  using (hash-value i)
                collect (list v i))))


;;; We have too many tangled references between MODEL and REACTION,
;;; hence we need a minimal PRINT-OBJECT for both.

(defmethod print-object ((m model) stream)
  (print-unreadable-object (m stream :identity t)
    (format stream
            "MODEL ~S (~A) with ~D species, ~D reaction at time ~F."
            (model-name m)
            (model-id m)
            (hash-table-count (model-initial-state m))
            (length (model-reactions m))
            (if (plusp (length (model-times m)))
                (aref (model-times m)
                      (1- (fill-pointer (model-times m))))
                (model-t0 m)))))


(defmethod describe-object ((m model) stream)
  (format stream
          "Model ~S (~S) is a ~S.~%"
          (model-name m)
          (model-id m)
          (type-of m))

  (format stream
          "~@[Reactions:~{ ~A~}~%~]"
          (mapcar #'reaction-name (model-reactions m)))

  (format stream
          "Initial State~%~:{~A~4T : ~S~%~}~%"
          (loop for v being the hash-key of (model-initial-state m)
                  using (hash-value i)
                collect (list v i)))

  (format stream
          "~&Current time: ~F~%"
          (if (plusp (length (model-times m)))
              (aref (model-times m)
                    (1- (fill-pointer (model-times m))))
              (model-t0 m)))

  (format stream
          "~&Current State~%~:{~A~4T : ~S~%~}~%"
          (loop for v being the hash-key of (model-state m)
                  using (hash-value i)
                collect (list v i)))
  )


;;; Reaction
;;; --------

(defstruct (reaction
            (:constructor
             %new-reaction (name
                            id
                            model
                            reactants
                            form
                            propensity
                            stoichiometry))
            )
  (name "Reaction"
        :type string
        :read-only t)
  (id nil
      :type symbol
      :read-only t)
  (model nil
         :type (or null model)
         :read-only t)
  (reactants ()
             :type list ; This is usually very short.
             :read-only t)
  (form ()
        :type list ; The propensity form; saved for documentation.
        :read-only t)
  (propensity #'(lambda (&rest vs) 42)
              :type function
              :read-only t)
  (stoichiometry ()
                 :type list ; An a-list
                 :read-only t)
  )


(defun make-reaction (name
                      model
                      vars
                      stoich
                      form
                      &key (id (gentemp name))
                      )
  (declare (type model model))
  (let ((r (%new-reaction name
                          id
                          model
                          vars
                          form
                          (compile
                           nil
                           `(lambda ,vars
                              (declare (ignorable ,@vars))
                              ;; Technicality: some reactions may
                              ;; involve more variables in the
                              ;; stoichiometry than in the propensity
                              ;; calculation; hence the `ignorable'.

                              ,form))
                          (pairlis vars stoich)))
        )
    (declare (type (or null model) model)
             (type reaction r))
    ;; The model in the reaction is now correct.
    (add-reaction model r)
    ))


(defun add-reaction (model r)
  (declare (type (or null model) model)
           (type reaction r))
  (when (and model (not (eq model (reaction-model r))))
    (error "GILLISPIE: tyring to add reaction ~A to model ~A,~@
            but the reaction is in model ~A."
           (reaction-name r)
           model
           (reaction-model r)))
  (when model
    (setf (model-reactions model)
          (nconc (model-reactions model) (list r))))
  r
  )


(defmethod print-object ((r reaction) stream)
  (print-unreadable-object (r stream :identity t)
    (format stream "REACTION ~S (~A)"
            (reaction-name r)
            (reaction-id r)))
  )


(defmethod describe-object ((r reaction) stream)
  (format stream
          "Reaction ~S (~S) is a ~S~%"
          (reaction-name r)
          (reaction-id r)
          (type-of r))
  (format stream
          "~@[~4Tin model ~A~%~]"
          (model-name (reaction-model r)))
  (format stream
          "Reactants:~{ ~A~}~%"
          (reaction-reactants r))
  (format stream
          "Stoichiometry:~%~:{~A~4T : ~2D~%~}"
          (mapcar (lambda (pair)
                    (list (car pair) (cdr pair)))
                  (reaction-stoichiometry r)))
  (format stream
          "Propensity: ~A~%"
          (reaction-form r))
  )


(defun happen (reaction)
  (declare (type reaction reaction))
  (let ((m  (reaction-model reaction))
        (vs (reaction-reactants reaction))
        (s  (reaction-stoichiometry reaction))
        )
    (declare (type model model)
             (type list vs)
             (type list s) ; An a-list.
             )
    
    (dolist (v vs)
      ;; Assume the stoichiometry is non null
      (incf (gethash v (model-state m))
            (the fixnum (rest (assoc v s :test #'eq))))
      )

    (push s (model-state-changes m))
    reaction
    ))


(defun current-propensity (reaction)
  (declare (type reaction reaction))
  (let* ((m  (reaction-model reaction))
         (vs (reaction-reactants reaction))
         ;; (s  (reaction-stoichiometry reaction))
         (p  (reaction-propensity reaction))
         (current-state (model-state m))
         )
    (declare (type model model)
             (type list vs)
             (type function p)
             (hash-table current-state)
             )
    
    (apply p (mapcar #'(lambda (v) (gethash v current-state)) vs))
    ))


;;; Simulation engine
;;; -----------------

(defun reset-model (m)
  "Resets a model to the initial state."
  (declare (type model m))
  (setf (model-state-changes m) nil
        (model-times m) #()
        (model-state m) (copy-dict (model-initial-state m)))
  m)
  

;;; simulate

(defun simulate (model &key (start 0.0) (finish 15.0))
  (declare (type model model))
  (let ((times (make-array 1
                           :initial-element 0.0
                           :element-type 'float
                           :fill-pointer t
                           :adjustable t))
        (rates ())
        (reactions (model-reactions model))
        (transition nil)
        (dt 0.0)
        )
    (declare (type (vector float) times)
             (type list rates reactions)
             (type reaction (or null transition))
             (type float dt))

    (loop for current-time = (aref times (1- (fill-pointer times)))

          while (<= current-time finish)

          do (setf rates (mapcar #'current-propensity reactions))

             #+nil
             (format t
                     "~A~%GILLISPIE: propensities at time ~F~%~
                      ~:{~A ~F~%~}"
                     (make-string 72 :initial-element #\_)
                     current-time
                     (mapcar #'list
                             (mapcar #'reaction-name reactions)
                             rates))
            
          when (every #'zerop rates) do
              (loop-finish)
            end
            
          do (setf transition (sample reactions rates))
             
             #+nil
             (format t
                     "GILLISPIE: chosen ~S~2%" transition)

             #+nil
             (dump-current-state model)

             (happen transition)
             (setf dt (random (exponentialf (sum rates))))

             (vector-push-extend (+ dt current-time) times 10)

          finally
            (setf (model-times model) times)
            #+nil
            (return (values times
                            (model-state-changes model)
                            (model-initial-state model)))
            (return model)
            )
    ))


;;; Utilities
;;; =========

(defun make-dict (init-pairs)
  (declare (type list init-pairs)) ; An A-LIST.
  (loop with htd of-type hash-table = (make-hash-table :test #'eq)
        for (v . i) in init-pairs
        do (setf (gethash v htd) i)
        finally (return htd)))


(defun copy-dict (ht)
  (declare (type hash-table ht))
  (let ((ht-copy (make-hash-table :test (hash-table-test ht))))
    (declare (type hash-table ht-copy))
    (maphash #'(lambda (v s)
                 (setf (gethash v ht-copy) s))
             ht)
    ht-copy))


(defun htkeys (ht)
  (declare (type hash-table ht))
  (loop for k being the hash-key of ht collect k))


(defun htvalues (ht)
  (declare (type hash-table ht))
  (loop for v being the hash-values of ht collect v))


(defun htkvs (ht)
  (declare (type hash-table ht))
  (loop for k being the hash-keys of ht using (hash-value v)
        collect (cons k v)))


(defun sum (seq)
  (declare (type list seq))
  (reduce #'+ seq :initial-value 0.0))


(defun model-trace (m)
  (declare (type model m))

  (loop with state = (copy-dict (model-initial-state m))
        for time across (model-times m)
        for state-change in (model-state-changes m)
      
        do (loop for (v . s) in state-change
                 do (incf (gethash v state) s))

        collect (cons time (htkvs state))))


(defparameter *gillispie-debug* nil)

(defun msg (where format-control &rest format-args)
  (when *gillispie-debug*
    (appy #'format where format-control format-args)))

;;;; end of file -- gillispie.lisp
