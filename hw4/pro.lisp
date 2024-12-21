(defun is-variable? (term)
  "Check if a term is a variable (capitalized string)."
  (and (stringp term) (char= (char term 0) #\X)))

(defun unify (query axiom substitutions)
  "Unifies a query with an axiom, returning updated substitutions or NIL if unification fails."
  (cond
    ((null query) substitutions)
    ((null axiom) nil)
    ((equal query axiom) substitutions)
    ((is-variable? (car query))
     (let ((existing (assoc (car query) substitutions :test #'equal)))
       (if existing
           (unify (cdr query) (cdr axiom) substitutions)
           (unify (cdr query) (cdr axiom)
                  (cons (cons (car query) (car axiom)) substitutions)))))
    ((is-variable? (car axiom))
     (unify (cdr query) (cdr axiom)
            (cons (cons (car axiom) (car query)) substitutions)))
    ((equal (car query) (car axiom))
     (unify (cdr query) (cdr axiom) substitutions))
    (t nil)))

(defun apply-substitutions (clause substitutions)
  "Apply substitutions to a clause."
  (mapcar
   (lambda (term)
     (if (is-variable? term)
         (let ((sub (assoc term substitutions :test #'equal)))
           (if sub (cdr sub) term))
         term))
   clause))

(defun resolve (axioms query substitutions)
  "Resolve a query using axioms and existing substitutions."
  (if (null query)
      (list substitutions)
      (let ((results nil)
            (current-query (apply-substitutions (car query) substitutions)))
        (dolist (axiom axioms)
          (cond
            ;; If the axiom is a fact
            ((and (listp (car axiom)) (not (member '< axiom)))
             (let ((new-sub (unify current-query (car axiom) substitutions)))
               (when new-sub
                 (setf results (nconc results (resolve axioms (cdr query) new-sub))))))
            ;; If the axiom is a rule
            ((and (listp (car axiom)) (member '< axiom))
             (let* ((head (apply-substitutions (car axiom) substitutions))
                    (body (cdr (member '< axiom))))
               (let ((new-sub (unify current-query head substitutions)))
                 (when new-sub
                   (setf results (nconc results (resolve axioms (append body (cdr query)) new-sub)))))))))
        results)))

(defun prolog_prove (axioms query)
  "Prove a query using the given axioms and return the result."
  (let ((solutions (resolve axioms query nil)))
    (if (null solutions)
        nil
        (remove-duplicates
         (mapcar (lambda (sub)
                   (mapcar (lambda (pair) (list (car pair) (cdr pair)))
                           sub))
                 solutions)
         :test #'equal))))


(let ((axioms '(
                (("father" "jim" "jill"))
                (("mother" "mary" "jill"))
                (("father" "samm" "jim"))
                (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                (("parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query1 '(("father" "X" "jill")))
      (query2 '(("parent" "X" "jill"))))
  (format t "~%Testing father query: ~A" (prolog_prove axioms query1))
  (format t "~%Testing parent query: ~A" (prolog_prove axioms query2)))