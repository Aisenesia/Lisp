(ql:quickload "cl-ppcre")
;(load "env.lisp")

(defun line-type (line)
  "Determine the type of the C code line."
  (format t "Processing line: ~A - " line)
  (cond 
        ;; Match "if" statements
        ((cl-ppcre:scan "^\\s*if\\s*\\(" line) 
         (progn (format t "Matched if  ") 'if-statement))
         
        ;; Match "for" loops
        ((cl-ppcre:scan "^\\s*for\\s*\\(" line) 
         (progn (format t "Matched for  ") 'for-loop))
         
        ;; Match "while" loops
        ((cl-ppcre:scan "^\\s*while\\s*\\(" line) 
         (progn (format t "Matched while  ") 'while-loop))
         
        ;; Match assignments with optional type declarations (e.g., "int x = 5;")
        ((cl-ppcre:scan "=" line)
         (progn (format t "Matched assignment  ") 'assignment))
         
        ;; Match "return" statements
        ((cl-ppcre:scan "^\\s*return\\s*" line) 
         (progn (format t "Matched return  ") 'return-statement))
         
        ;; Match function calls
        ((cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*;$" line) 
         (progn (format t "Matched function-call  ") 'function-call))
         
        ;; Match function definitions
        ((cl-ppcre:scan "^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" line) 
         (progn (format t "Matched function-definition  ") 'function-definition))
         
        ;; Match block start
        ((cl-ppcre:scan "^\\s*\\{" line) 
         (progn (format t "Matched block-start  ") 'block-start))
         
        ;; Match block end
        ((cl-ppcre:scan "^\\s*\\}" line) 
         (progn (format t "Matched block-end  ") 'block-end))
         
        ;; Unknown line
        (t (progn (format t "Unknown line  ") 'unknown))))

(defun conversion-foo (line-type)
  "Return the appropriate conversion function based on the line type."
  (format t "Determining conversion function for line type: ~A~%" line-type)
  (case line-type
    ('if-statement #'convert-if)
    ('for-loop #'convert-for)
    ('while-loop #'convert-while)
    ('assignment #'convert-assignment)
    ('return-statement #'convert-return)
    ('function-call #'convert-function-call)
    ('function-definition #'convert-function-definition)
    ('block-start #'convert-block-start)
    ('block-end #'convert-block-end)
    (t #'convert-unknown)))

(defun convert (line conversion-fn)
  "Convert a line of C code to Lisp using the appropriate conversion function."
  (format t "Converting line: ~A using function: ~A~%" line conversion-fn)
  (funcall conversion-fn line))

;; Conversion main functions

(defun convert-lines (lines)
  "Recursively process each line and convert it to Lisp."
  (if (null lines)
      nil
      (let* ((line (car lines))
             (line-type (line-type line))
             (conversion-fn (conversion-foo line-type))
             (converted-line (convert line conversion-fn)))
        (cons converted-line (convert-lines (cdr lines))))))

;; Conversion functions

;; if (a > b) { -> (if (> a b))
(defun convert-if (line)
  "Convert an 'if' statement from C to Lisp."
  (let ((extracted (split-if-into-parameters line)))
    (print extracted)
    (format nil "(if ~A" (c-to-lisp-arithmetic (first extracted)))))
;(convert-if "if (a > b) {")

(defun convert-for (line)
  "Convert a 'for' loop from C to Lisp."
  (format nil "(loop for ~A~%" line))

(defun convert-while (line)
  "Convert a 'while' loop from C to Lisp."
  (format nil "(loop while ~A~%" line))


  (defun extract-assignment (line)
    (cl-ppcre:register-groups-bind (type var-name value)
        ("^\\s*(\\w+\\s+)?(\\w+)\\s*=\\s*(.+);?$" line)
      (if (and var-name value)
          (progn
            (format t "Matched assignment~%")
            (when type
              (format t "Type: ~a~%" type))
            (format t "Variable name: ~a~%" var-name)
            (format t "Assigned value: ~a~%" (cl-ppcre:regex-replace ";" value ""))
            (list var-name (cl-ppcre:regex-replace ";" value "")))  ;; Return the captured groups with ";" removed from value
          (format t "No match~%"))))
        
;; int a = 5; -> (setf a 5), a = 5; -> (setf a 5)
(defun convert-assignment (line)
  "Converts a variable assignment from C to Lisp."
  ;; Assuming line can be 'int x = 5;' or 'x = 5;'
  (let ((extracted (extract-assignment line)))
  (format nil "(setf ~A ~A)" (first extracted) (second extracted))))
        ;; Example usage:


        

(defun convert-return (line)
  "Convert a 'return' statement from C to Lisp."
  (format nil "~A~%" (cl-ppcre:regex-replace ";" (extract-return line) "")))

(defun convert-function-call (line)
  "Convert a function call from C to Lisp."
  (format nil "(~A~%" line))

(defun convert-function-definition (line)
  "Convert a function definition from C to Lisp."
  (let ((parts (split-function-into-parameters line)))
    (format nil "(defun ~A (~{~A~^ ~})~%" (first parts) (rest parts))))

;; Example usage:
;(convert-function-definition "int main(int a, int b) {")

(defun convert-block-start (line)
  "Convert a block start from C to Lisp."
  (format nil "(progn~%"))

(defun convert-block-end (line)
  "Convert a block end from C to Lisp."
  (format nil ")~%"))

(defun convert-unknown (line)
  "Convert an unknown line from C to Lisp."
  (format nil ";; Unknown line: ~A~%" line))
  

;; String manipulation functions

(defun split-string (str &optional (delimiter " "))
  "Split a string by a specified delimiter (default is space)."
  (let ((start 0)
        (result '()))
    (loop for i from 0 below (length str)
          do (when (or (char= (char str i) (char delimiter 0))
                       (= i (1- (length str))))
               (when (> i start)
                 (push (subseq str start i) result))
               (setf start (1+ i))))
    ;; Handle the last substring if the string does not end with a delimiter
    (when (> (length str) start)
      (push (subseq str start) result))
    (nreverse result)))

(defun extract-param-names (params)
  "Extract the parameter names from a C-style function parameter list, e.g., 'int a, int b'."
  (mapcar #'second 
          (mapcar (lambda (param)
                    (cl-ppcre:split #\Space param))
                  (cl-ppcre:split #\, params))))

(defun extract-return (line)
  "Extract the return type and function name from a C-style function definition, e.g., 'int main(int a, int b)'."
  (let ((parts (split-string line)))
    (c-to-lisp-arithmetic (second parts))))

;; Example usage:
;(extract-return "return 0;")

;ExAMPLE USAGE
;(split-if-into-parameters "if (a > b) {")

(defun c-to-lisp-operator (operator)
  "Convert a C operator to the equivalent Lisp operator."
  (case operator
    ("==" 'equal)
    ("!=" 'not-equal)
    (">" 'greater)
    ("<" 'less)
    (">=" 'greater-equal)
    ("<=" 'less-equal)
    ("&&" 'and)
    ("||" 'or)
    ("!" 'not)
    ("+" '+)
    ("-" '-)
    ("*" '*)
    ("/" '/)
    ("%" 'mod)
    (t operator)))

(defun c-to-lisp-arithmetic(expression)
  "Convert a C arithmetic expression to Lisp. ex: 'a + b' to '(+ a b)', or 'a > b' to '(> a b)', or 'a' to '(a)'."
  (let ((tokens (cl-ppcre:split "\\s+" expression)))
    (cond
      ;; If there is only one token, return it surrounded by parentheses.
      ((= (length tokens) 1)
       (format nil "(~A)" (first tokens)))

      ;; Otherwise, handle it as an operator with two operands.
      (t
       (format nil "(~A ~A ~A)"
               (c-to-lisp-operator (second tokens)) ; The operator.
               (first tokens)                      ; First operand.
               (third tokens))))))                 ; Second operand.

;;example usage
;(c-to-lisp-arithmetic "a > b")

(defun split-param-helper (params)
  "Recursive helper to split the parameters, discarding types such as int, float, void* etc."
  (if (string= params "")
      nil
      (let* ((start (position #\, params))
             (param (if start
                        (subseq params 0 start)
                        params))
             (rest (if start
                       (subseq params (if (char= (char params (1+ start)) #\Space)
                                          (+ start 2)
                                          (1+ start)))
                       ""))
             (param-name (second (cl-ppcre:split #\Space param))))
        (cons param-name (split-param-helper rest)))))

(defun split-function-into-parameters (str)
  "Split a single C line such as 'int main(int a, int b)' into the function name and parameter names. Returns a list with the function name as the first element and the parameter names as the subsequent elements."
  (let ((parts '()))
    (cl-ppcre:register-groups-bind (function-name params)
        ("^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" str)
      (push function-name parts)
      (when (not (string= params ""))
        (setf parts (append parts (split-param-helper params)))))
        (print parts)
    parts))

;("^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" str), correct regex for if statement

(defun split-if-into-parameters (str)
  (let ((parts '()))
    (cl-ppcre:register-groups-bind (condition)
        ("^\\s*if\\s*\\(([^)]*)\\)\\s*\\{" str)
      ;(push "if" parts)
      (when (not (string= condition ""))
        (push condition parts)))
    (nreverse parts)))
    
;; File I/O functions

(defun read-file (filename)
  "Read the input line by line."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun write-file (filename content)
  "Write the converted Lisp code to the output file."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (line content)
      (write-line line stream))))

;; Main function
(defun main (input-file output-file)
  "Main function to read, convert, and write the file."
  (let* ((lines (read-file input-file))
         (converted-lines (convert-lines lines)))
    (write-file output-file converted-lines)))

;; Example usage:
;(main "input.c" "output.lisp")
;(split-string "int main (int a) {}")
;(split-function-into-parameters "int main(int a, int b) {")
;(split-if-into-parameters "if(a > b) {")

(defun m ()
  ;(clear)
  (main "input.c" "output.lisp"))