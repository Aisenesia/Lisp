(defparameter *max-depth* 5 "Maximum recursion depth")
;; The depth limit is used to prevent infinite recursion and cycles in complex queries,
;; cycle detection is kind of broken so..

(defun substitute-vars (predicate head-vars query-vals)
  "Substitute variables in predicate with values from query"
  (if (listp predicate) ; If predicate is a list, map over it
      (mapcar #'(lambda (term) ; For each term in the predicate
                  (let ((pos (position term head-vars :test #'equal))) ; Find the position of the term in the head-vars
                    (if pos
                        (nth pos query-vals) ; If the term is a variable, 
                        ;                      substitute it with the corresponding value from query-vals
                        term))) ; Otherwise, return the term as is
              predicate)
      predicate))
    
(defun variable-p (term)
  "Check if a symbol is a variable (starts with uppercase)"
  (and (stringp term) ; Check if term is a string, and the first character is an uppercase letter
       (alpha-char-p (char term 0))
       (upper-case-p (char term 0))))

(defun prolog_prove (query axioms &optional (depth 0) (visited nil))
  "Find matching rules for goal in axioms with depth limit"
  ; (format t "~%DEBUG: Processing query ~A at depth ~A" query depth) ; Debug output
  
  (when (>= depth *max-depth*)
    (format t "~%WARNING: Max depth ~A reached for query ~A~%" depth query)
    (return-from prolog_prove nil))
  
  (let ((ret-list '())
        (query-key (format nil "~A-~A" (car query) (caddr query)))) ; Generate a unique key for the query to detect cycles
    
    (when (member query-key visited :test #'equal)
      (format t "~%WARNING: Cycle detected for ~A~%" query-key)
      (return-from prolog_prove nil)) ; break
    
    (dolist (axiom axioms) ; for each axiom in the list of axioms
      (cond
        ;; Direct fact match
        ((and (not (listp (car axiom))) ; If the axiom is not a list
              (equal (length query) (length axiom)) ; and the query and axiom have the same length
              (every #'(lambda (q a) ; and every element in the query and axiom are equal or the query element is a variable
                         (or (variable-p q) (equal q a)))
                     query axiom))
         (unless (member axiom ret-list :test #'equal)
           (push axiom ret-list))) ; Add the axiom to the return list since we can conclude its a direct fact match
        
        ;; Rule match
        ;; A rule match occurs when the head of the axiom matches the query's key. 
        ((and (listp (car axiom)) ; If the axiom is a list
              (equal (caar axiom) (car query))) ; and the head of the axiom matches the query's key
         (let* ((head-vars (cdar axiom)) 
                (query-vals (cdr query)) 
                (body-preds (cddr axiom))) ; Extract the head variables, query values and body predicates
            ; (format t "~%DEBUG: Matching rule ~A with query ~A" axiom query)
           (dolist (body-pred body-preds) ; For each body predicate in the body predicates
             (unless (equal body-pred "<") ; if not <
               (let ((new-query (substitute-vars body-pred head-vars query-vals))) ; change the variables in the body predicate with the query values
                 ;(format t "~%DEBUG: Generated subquery ~A" new-query)
                 (let ((sub-matches (prolog_prove ; recursively call prolog_prove with the new query
                                    new-query 
                                    axioms 
                                    (1+ depth)
                                    (cons query-key visited)))) ; add the query key to the visited list
                   (dolist (match sub-matches)
                     (unless (member match ret-list :test #'equal)
                       (push match ret-list)))))))))))
    ; (format t "~%DEBUG: Returning matches ~A for query ~A" ret-list query)
    ret-list)) ; return the list of matches

(defun run-tests ()
  (let ((axioms '(("father" "jim" "jill")
                  ("mother" "mary" "jill")
                  ("father" "samm" "jim")
                  (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                  (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                  (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                  (("parent" "X" "Y") "<" ("father" "X" "Y")))))
    
    (format t "~%Test 1 - Direct fact query:")
    (print (prolog_prove '("father" "jim" "X") axioms))
    
    (format t "~%Test 2 - Parent rule query:")
    (print (prolog_prove '("parent" "X" "jill") axioms))
      
    
    (format t "~%Test 3 - Ancestor query with depth limit:")
    (print (prolog_prove '("ancestor" "X" "jill") axioms))))

(run-tests)
