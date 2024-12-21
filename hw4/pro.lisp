(defun prolog_prove (axioms query)
  "Prove a query using the given axioms with depth-first search and resolution"
  (labels 
      ((variable-p (x)
         "Check if a symbol is a variable (starts with uppercase)"
         (and (stringp x) 
              (not (string= x ""))
              (upper-case-p (char x 0))))
       
       (substitute-bindings (bindings term)
         "Replace variables in term with their bound values"
         (cond 
           ((variable-p term) 
            (let ((binding (assoc term bindings :test #'string=)))
              (if binding 
                  (substitute-bindings bindings (cdr binding))
                  term)))
           ((atom term) term)
           (t (mapcar (lambda (subterm) 
                       (substitute-bindings bindings subterm)) 
                     term))))
       
       (unify (term1 term2 &optional bindings)
         "Unify two terms, returning updated bindings or nil if unification fails"
         (let ((t1 (substitute-bindings bindings term1))
               (t2 (substitute-bindings bindings term2)))
           (cond 
             ((variable-p t1) 
              (if (and (variable-p t2) (string= t1 t2))
                  bindings
                  (cons (cons t1 t2) bindings)))
             
             ((variable-p t2)
              (cons (cons t2 t1) bindings))
             
             ((and (atom t1) (atom t2))
              (if (string= t1 t2) 
                  bindings
                  nil))
             
             ((and (listp t1) (listp t2))
              (if (= (length t1) (length t2))
                  (let ((new-bindings (unify (first t1) (first t2) bindings)))
                    (if new-bindings
                        (reduce (lambda (curr-bindings pair)
                                (and curr-bindings 
                                     (unify (car pair) (cadr pair) curr-bindings)))
                              (mapcar #'list (rest t1) (rest t2))
                              :initial-value new-bindings)
                        nil))
                  nil))
             (t nil))))
       
       (prove-all (goals axioms bindings)
         "Prove a list of goals using the given axioms and bindings"
         (if (null goals)
             (list bindings)
             (loop for solution in (prove-goal (first goals) axioms bindings)
                   append (prove-all (mapcar (lambda (g) 
                                             (substitute-bindings solution g))
                                           (rest goals))
                                   axioms
                                   solution))))
       
       (prove-goal (goal axioms bindings)
         "Prove a single goal using the axioms"
         (loop for axiom in axioms
               for renamed-axiom = (rename-variables axiom)
               when (cond 
                      ;; Simple fact
                      ((= (length renamed-axiom) 1)
                       (let ((new-bindings (unify goal (first renamed-axiom) bindings)))
                         (when new-bindings 
                           (list new-bindings))))
                      ;; Rule with body
                      ((and (= (length renamed-axiom) 3)
                            (string= (second renamed-axiom) "<"))
                       (let ((head (first renamed-axiom))
                             (body (third renamed-axiom)))
                         (let ((new-bindings (unify goal head bindings)))
                           (when new-bindings
                             (if (listp body)
                                 (prove-all (list body) axioms new-bindings)
                                 (prove-all (list (list body)) axioms new-bindings)))))))
               append it))
       
       (rename-variables (axiom)
         "Rename variables in an axiom to avoid conflicts"
         (let ((counter 0))
           (labels ((rename (term)
                     (cond
                       ((variable-p term)
                        (incf counter)
                        (format nil "~a_~d" term counter))
                       ((atom term) term)
                       (t (mapcar #'rename term)))))
             (rename axiom)))))
    
    ;; Main proving logic
    (let ((results (prove-all query axioms nil)))
      (when results
        (mapcar (lambda (bindings)
                  (remove-if-not (lambda (binding)
                                  (some (lambda (term)
                                        (search (car binding) (format nil "~a" term)))
                                      query))
                                bindings))
                results)))))

(defun run-tests ()
  (let* ((axioms '(
                   (("father" "jim" "jill"))
                   (("mother" "mary" "jill"))
                   (("father" "samm" "jim"))
                   (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                   (("ancestor" "X" "Y") "<" ("parent" "X" "Z") ("ancestor" "Z" "Y"))
                   (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                   (("parent" "X" "Y") "<" ("father" "X" "Y"))))
         (query1 '(("ancestor" "X" "jill")))
         (query2 '(("ancestor" "X" "jill") ("mother" "X" "bob")))
         (result1 (prolog_prove axioms query1))
         (result2 (prolog_prove axioms query2)))
    
    (format t "Result of query1: ~a~%" result1)
    (format t "Result of query2: ~a~%" result2)
    (format t "All tests completed!~%")))

(run-tests)