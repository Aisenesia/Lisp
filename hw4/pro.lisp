(defun return-matching-rules (query axioms &optional list)
  "Find matching rules for goal in axioms"
  (let ((ret-list (if (null list) '() list)))
    (dolist (axiom axioms)
      (progn
        (format t "DEBUG: query: ~A Matching axiom: ~A, list: ~A~%" query axiom ret-list)
        (cond
          ;; Case 1: Direct predicate match (unchanged)
          ((equal (car query) (car axiom))
           (progn
             (format t "DEBUG: Direct match: ~A~%" axiom)
             (cond
               ((and (not (variable-p (cadr axiom)))
                    (not (variable-p (cadr query))))
               (when (equal (cadr axiom) (cadr query))
                 (push axiom ret-list)))
               ((or (variable-p (cadr axiom))
                    (variable-p (cadr query)))
                (let ((match-found t))
                  (when (and (not (variable-p (caddr axiom)))
                           (not (variable-p (caddr query)))
                           (not (equal (caddr axiom) (caddr query))))
                    (setf match-found nil))
                  (when match-found
                    (push axiom ret-list)))))))
         
          ;; Case 2: Rule match with "<" - transform query and recurse
          ((and (listp axiom)
                (listp (car axiom))
                (equal (caar axiom) (car query))
                (member "<" (cdr axiom) :test #'equal))
           (progn
             (format t "DEBUG: Found rule with head: ~A and body: ~A~%" (car axiom) (cddr axiom))
             ;; For each body predicate, create a new query by substituting variables
             (dolist (body-pred (cddr axiom))
               (let* ((head-vars (cdar axiom))  ; Variables in the head
                      (query-vals (cdr query))   ; Values in the query
                      ;; Create new query by replacing variables in body predicate
                      (new-query (substitute-vars body-pred head-vars query-vals)))
                 (format t "DEBUG: Created new query: ~A~%" new-query)
                 (let ((sub-matches (return-matching-rules new-query axioms)))
                   (setf ret-list (append sub-matches ret-list))))))))))
    ret-list))

(defun substitute-vars (predicate head-vars query-vals)
  "Substitute variables in predicate with values from query"
  (if (listp predicate)
      (mapcar #'(lambda (term)
                  (let ((pos (position term head-vars :test #'equal)))
                    (if pos
                        (nth pos query-vals)
                        term)))
              predicate)
      predicate))
    
(defun variable-p (term)
  "Check if a symbol is a variable (starts with uppercase)"
  (and (stringp term)
       (alpha-char-p (char term 0))
       (upper-case-p (char term 0))))

;; Test the implementation
(defun testspr ()
  (let ((axioms '(("father" "jim" "jill")
                  ("mother" "mary" "jill")
                  ("mother" "mary" "zoe")
                  ("father" "samm" "jim")
                  (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                  (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                  (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                  (("parent" "X" "Y") "<" ("father" "X" "Y"))))
        (query1 '("ancestor" "Z" "jill"))
        (query2 '("parent" "X" "jill"))
        (query3 '("ancestor" "X" "zoe")))
    (format t "~%Testing father query: ~A~%" (return-matching-rules query1 axioms))))
    ;(format t "~%Testing parent query: ~A~%" (return-matching-rules query2 axioms))
    ;(format t "~%Testing ancestor query: ~A~%" (return-matching-rules query3 axioms))))

(testspr)