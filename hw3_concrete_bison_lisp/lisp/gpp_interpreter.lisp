;;;; GPP Interpreter - A Lisp-like language interpreter

;;; Data structures
(defstruct fraction
  numerator
  denominator)

(defvar *variable-table* (make-hash-table :test #'equal))

;;; Helper functions
(defun split-sequence (delimiter sequence)
  "Split a sequence by a delimiter."
  (loop with result = nil
        for element in sequence
        for current = (list element)
        if (eql element delimiter)
        do (progn
             (push (reverse current) result)
             (setf current nil))
        else
        do (push element current)
        finally (return (reverse (cons (reverse current) result)))))

(defun parse-fraction (str)
  "Parse a fraction string in format 'NfD' where N is numerator and D is denominator"
  (let* ((parts (split-sequence #\f str))
         (num (parse-integer (first parts)))
         (den (parse-integer (second parts))))
    (make-fraction :numerator num :denominator den)))

(defun fraction-to-string (frac)
  (format nil "~Af~A" 
          (fraction-numerator frac) 
          (fraction-denominator frac)))

(defun add-fractions (f1 f2)
  (let* ((n1 (fraction-numerator f1))
         (d1 (fraction-denominator f1))
         (n2 (fraction-numerator f2))
         (d2 (fraction-denominator f2))
         (new-den (* d1 d2))
         (new-num (+ (* n1 d2) (* n2 d1))))
    (make-fraction :numerator new-num :denominator new-den)))

(defun subtract-fractions (f1 f2)
  (let* ((n1 (fraction-numerator f1))
         (d1 (fraction-denominator f1))
         (n2 (fraction-numerator f2))
         (d2 (fraction-denominator f2))
         (new-den (* d1 d2))
         (new-num (- (* n1 d2) (* n2 d1))))
    (make-fraction :numerator new-num :denominator new-den)))

(defun multiply-fractions (f1 f2)
  (make-fraction 
   :numerator (* (fraction-numerator f1) (fraction-numerator f2))
   :denominator (* (fraction-denominator f1) (fraction-denominator f2))))

(defun divide-fractions (f1 f2)
  (make-fraction 
   :numerator (* (fraction-numerator f1) (fraction-denominator f2))
   :denominator (* (fraction-denominator f1) (fraction-numerator f2))))

(defun compare-fractions (f1 f2)
  (let ((n1 (* (fraction-numerator f1) (fraction-denominator f2)))
        (n2 (* (fraction-numerator f2) (fraction-denominator f1))))
    (- n1 n2)))

;;; Main evaluator
(defun gpp-eval (expr &optional env)
  (cond
    ;; Self-evaluating expressions
    ((numberp expr) expr)
    ((stringp expr) expr)
    ((eq expr 'true) t)
    ((eq expr 'false) nil)
    ((eq expr 'nil) nil)
    
    ;; Variable references
    ((symbolp expr) (or (gethash expr *variable-table*) 0))
    
    ;; Special forms
    ((listp expr)
     (let ((op (first expr)))
       (cond
         ;; Arithmetic operations, currentl
         ((eq op '+) (+ (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op '-) (- (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op '*) (* (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op '/) (if (zerop (gpp-eval (third expr)))
                        0
                        (/ (gpp-eval (second expr)) (gpp-eval (third expr)))))
         
         ;; Variable assignment
         ((eq op 'set) 
          (setf (gethash (second expr) *variable-table*) 
                (gpp-eval (third expr))))
         
         ;; Control structures
         ((eq op 'if)
          (if (gpp-eval (second expr))
              (gpp-eval (third expr))
              (if (> (length expr) 3)
                  (gpp-eval (fourth expr))
                  0)))
         
         ;; Boolean operations
         ((eq op 'and) 
          (and (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op 'or)
          (or (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op 'not)
          (not (gpp-eval (second expr))))
         ((eq op 'equal)
          (= (gpp-eval (second expr)) (gpp-eval (third expr))))
         ((eq op 'less)
          (< (gpp-eval (second expr)) (gpp-eval (third expr))))
         
         ;; I/O operations
         ((eq op 'print)
          (format t "~A~%" (gpp-eval (second expr)))
          t)
         
         ;; List operations
         ((eq op 'list)
          (loop for i from 1 below (length expr)
                collect (gpp-eval (nth i expr))))
         ((eq op 'append)
          (append (list (gpp-eval (second expr)))
                  (gpp-eval (third expr))))
         ((eq op 'concat)
          (append (gpp-eval (second expr))
                  (gpp-eval (third expr))))
         
         ;; Default
         (t (format t "Unknown operator: ~A~%" op))))))
  ;; Remove the default return value of t
  )

;;; REPL
(defun gpp-repl ()
  (loop
    (format t "gpp> ")
    (force-output)
    (handler-case
        (let ((input (read)))
          (if (eq input 'exit)
              (return-from gpp-repl)
              (gpp-eval input)))
      (error (condition)
        (format t "Error: ~A~%" condition)))))

;;; Start the interpreter
(defun start-gpp ()
  (format t "GPP Interpreter~%")
  (format t "Type 'exit' to quit~%")
  (gpp-repl))

(start-gpp)