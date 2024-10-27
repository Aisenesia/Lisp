(ql:quickload "cl-ppcre")

;; Function to determine the type of a C code line
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

    ;; Match "printf" statements
    ((cl-ppcre:scan "^\\s*printf\\s*\\(" line) 
     (progn (format t "Matched printf  ") 'print-statement))

    ;; Match function calls
    ((cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*;\\s*$" line)
     (progn (format t "Matched function-call  ") 'function-call))

    ;; Match function definitions
    ((cl-ppcre:scan "^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" line) 
     (progn (format t "Matched function-definition  ") 'function-definition))

    ;; Match function declarations
    ((cl-ppcre:scan "^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*;\\s*$" line) 
     (progn (format t "Matched function-declaration  ") 'function-declaration))

    ;; Match block start
    ((cl-ppcre:scan "^\\s*\\{" line) 
     (progn (format t "Matched block-start  ") 'block-start))

    ;; Match block end
    ((cl-ppcre:scan "^\\s*\\}" line) 
     (progn (format t "Matched block-end  ") 'block-end))

    ;; Unknown line
    (t (progn (format t "Unknown line  ") 'unknown))))

;; Function to return the appropriate conversion function based on the line type
(defun conversion-foo (line-type)
  "Return the appropriate conversion function based on the line type."
  (format t "Determining conversion function for line type: ~A~%" line-type)
  (case line-type
    ('if-statement #'convert-if)
    ('for-loop #'convert-for)
    ('while-loop #'convert-while)
    ('assignment #'convert-assignment)
    ('return-statement #'convert-return)
    ('print-statement #'convert-print)
    ('function-call #'convert-function-call)
    ('function-definition #'convert-function-definition)
    ('function-declaration #'convert-function-declaration)
    ('block-start #'convert-block-start)
    ('block-end #'convert-block-end)
    (t #'convert-unknown)))

;; Function to convert a line of C code to Lisp using the appropriate conversion function
(defun convert (line conversion-fn)
  "Convert a line of C code to Lisp using the appropriate conversion function."
  (format t "Converting line: ~A using function: ~A~%" line conversion-fn)
  (funcall conversion-fn line))

;; Function to recursively process each line and convert it to Lisp
(defun convert-lines (lines)
  "Recursively process each line and convert it to Lisp."
  (if (null lines)
      nil
      (let* ((line (car lines))
             (line-type (line-type line))
             (conversion-fn (conversion-foo line-type))
             (converted-line (convert line conversion-fn)))
        (cons converted-line (convert-lines (cdr lines))))))


;; Function to split an 'if' statement into its components
(defun extract-if (str)
  (let ((parts '()))
    (cl-ppcre:register-groups-bind (condition)
        ("^\\s*if\\s*\\(([^)]*)\\)\\s*\\{" str)
      (when (not (string= condition ""))
        (push condition parts)))
    (nreverse parts)))
  ;; example usage
  ;; (print (extract-if "if (a > b) {"))

;; Function to convert an 'if' statement from C to Lisp
(defun convert-if (line)
  "Convert an 'if' statement from C to Lisp."
  (let ((extracted (extract-if line)))
    (print extracted)
    (format nil "(if ~A(progn " (c-to-lisp-arithmetic (first extracted)))))

;; Function to extract the components of a 'for' loop from C code
(defun extract-for (line)
  "Extract the components of a 'for' loop from C code."
  ;; For the line "for (int i = 0; i < 10; i++) {" it should return ("i" "0" "10")
  (cl-ppcre:register-groups-bind (init cond incr)
      ("^\\s*for\\s*\\(([^;]+);([^;]+);([^)]+)\\)\\s*\\{" line)
    (if (and init cond incr)
        (let* ((var-name (second (cl-ppcre:split "\\s+" init)))
               (start (second (cl-ppcre:split "=" init)))
               (end (second (cl-ppcre:split "<" cond))))
          (format t "Extracted for loop components: var-name: ~A, start: ~A, end: ~A~%" var-name start end)
          (list var-name start end))
        (format t "No match for 'for' loop~%"))))

;; Function to convert a 'for' loop from C to Lisp
(defun convert-for (line)
  "Convert a 'for' loop from C to Lisp."
  (let ((extracted (extract-for line)))
    (if extracted
        (format nil "(loop for ~A from ~A to ~A by ~A do (progn"
                (first extracted) (second extracted) (third extracted) 1)
        (format t "No match for 'for' loop conversion~%"))))

;; Example usage
;; (print (convert-for "for (i = 0; i < 10; i++) {"))

;; Function to extract the components of a 'while' loop from C code
(defun extract-while (line)
  "Extract the components of a 'while' loop from C code."
  (cl-ppcre:register-groups-bind (condition)
      ("^\\s*while\\s*\\(([^)]*)\\)\\s*\\{" line)
    (if condition
        (format t "Extracted while loop components: condition: ~A~%" condition)
        (format t "No match for 'while' loop~%"))))
;; Example usage
;; (print (extract-while "while (i < 10) {"))

;; Function to convert a 'while' loop from C to Lisp
(defun convert-while (line)
  "Convert a 'while' loop from C to Lisp."
  (format nil "(loop while ~A do (progn ~%" (c-to-lisp-arithmetic (extract-while line))))

;; Function to extract the components of a variable assignment from C code
(defun extract-assignment (line)
  (cl-ppcre:register-groups-bind (type var-name value)
      ("^\\s*(\\w+\\s+)?(\\w+)\\s*=\\s*(.+);?$" line)
    (if (and var-name value)
        (progn            
          (list var-name (cl-ppcre:regex-replace ";" value "")))  ;; Return the captured groups filtering ";" out.
        (format t "No match~%"))))

;; Function to convert a variable assignment from C to Lisp
(defun convert-assignment (line)
  "Converts a variable assignment from C to Lisp."
  ;; Line can be 'int x = 5;' or 'x = 5;' or 'int result = sum(x, y);'
  (let ((extracted (extract-assignment line)))
    (let ((value (second extracted)))
      (if (cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*$" value)
          (let ((function-name (cl-ppcre:register-groups-bind (fname args)
                                ("^\\s*(\\w+)\\s*\\((.*)\\)\\s*$" value)
                              fname))
                (arguments (cl-ppcre:register-groups-bind (fname args)
                                ("^\\s*(\\w+)\\s*\\((.*)\\)\\s*$" value)
                              args)))
            (if (string= arguments "")
                (format nil "(setf ~A (~A))" (first extracted) function-name) ; Handle function calls that have no arguments
                (format nil "(setf ~A (~A ~{~A~^ ~}))" (first extracted) function-name (cl-ppcre:split "\\s*,\\s*" arguments)))) ; Handle function calls that have arguments
          (format nil "(setf ~A ~A)" (first extracted) value))))) ; Handle simple assignments

;; Example usage
;; (print (convert-assignment "int result = sum(x, y);"))


;; Function to convert a 'printf' statement from C to Lisp
(defun convert-print (line)
  "Convert a 'printf' statement from C to Lisp."
  (cl-ppcre:register-groups-bind (format-string params)
      ("^\\s*printf\\s*\\((\"[^\"]*\")(.*)\\);?\\s*$" line)
    (let* ((clean-format-string (cl-ppcre:regex-replace-all "%\\w" "~A" format-string))
           (clean-params (cl-ppcre:split "\\s*,\\s*" params)))
      (format nil "(format t \"~A\" ~{~A~})" clean-format-string clean-params))))

;; Function to convert a function call from C to Lisp
(defun convert-function-call (line)
  "Convert a function call from C to Lisp."
  (cl-ppcre:register-groups-bind (function-name params)
      ("^\\s*(\\w+)\\s*\\(([^)]*)\\)\\s*;\\s*$" line) 
    (let ((args (cl-ppcre:split "\\s*,\\s*" params))) ; split by comma
      (format nil "(~A ~{~A~})" function-name args)))) 

;; Recursive helper to split the parameters, discarding types such as int, float, void* etc.
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

;; Function to split a C line into the function name and parameter names
(defun extract-function (str)
  "Split a single C line such as 'int main(int a, int b)' into the function name and parameter names. Returns a list with the function name as the first element and the parameter names as the subsequent elements."
  (cl-ppcre:register-groups-bind (function-name params)
      ("^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" str)
    (cons function-name
          (if (string= params "")
              '() ; base case
              (split-param-helper params))))) ; recursively create the list

;; Function to convert a function definition from C to Lisp
(defun convert-function-definition (line)
  "Convert a function definition from C to Lisp."
  (let ((parts (extract-function line)))
    (format nil "(defun ~A (~{~A~^ ~})~%(progn" (first parts) (rest parts))))

;; Function to convert a function declaration from C to Lisp
(defun convert-function-declaration (line)
  "Convert a function declaration from C to Lisp."
  (cl-ppcre:register-groups-bind (return-type function-name params)
      ("^\\s*(\\w+)\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*;\\s*$" line)
    (let ((param-names (split-param-helper params)))
      (format nil ";(declaim (ftype (function (~{~A~^ ~}) ~A) ~A))"
              param-names (c-to-lisp-type-names return-type) function-name))))
;; Example usage
;; (print (convert-function-declaration "int sum(int a, int b);"))

;; Function to convert a block start from C to Lisp
(defun convert-block-start (line)
  "Convert a block start from C to Lisp."
  (format nil "(progn~%"))

;; Function to convert a block end from C to Lisp
(defun convert-block-end (line)
  "Convert a block end from C to Lisp."
  (format nil "))~%"))

;; Function to convert an unknown line from C to Lisp
(defun convert-unknown (line)
  "Convert an unknown line from C to Lisp."
  (format nil ";; Unknown line: ~A~%" line))

;; Function to extract the parameter names from a C-style function parameter list

;; Example usage
;; (print (extract-param-names "int a, int b"))
;; vs extract-function
;; (print (extract-function "int a, int b"))
;; Function to extract the return value from a C-style return statement
(defun extract-return (line)
  "Extract the return value from a C-style return statement, e.g., 'return 5;'."
  (cl-ppcre:register-groups-bind (return-value)
      ("^\\s*return\\s+([^;]+);?\\s*$" line)
    (if return-value
        (cl-ppcre:regex-replace ";" (c-to-lisp-arithmetic return-value) "")
        (format t "No match for return statement~%"))))

;; Function to convert a 'return' statement from C to Lisp
(defun convert-return (line)
  "Convert a 'return' statement from C to Lisp."
  (format nil "~A~%" (extract-return line))) ; Remove the semicolon from extracted
;; Example usage
;; (print (convert-return "return a + b;"))

;; Function to convert a C operator to the equivalent Lisp operator
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

;; Function to convert a C type to the equivalent Lisp type, tried to do this for function declarations
(defun c-to-lisp-type-names (type)
  "Convert a C type to the equivalent Lisp type."
  (case (intern (string-upcase type) :keyword)
    (:INT 'integer)
    (:FLOAT 'float)
    (:DOUBLE 'double-float)
    (:CHAR 'character)
    (:VOID 'void)
    (t "type")))

;; Function to convert a C arithmetic expression to Lisp
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
;; Example usage
;; (print (c-to-lisp-arithmetic "a + b"))



;; Function to read the input line by line
(defun read-file (filename)
  "Read the input line by line."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; Function to write the converted Lisp code to the output file
(defun write-file (filename content)
  "Write the converted Lisp code to the output file."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (line content)
      (write-line line stream))))

;; Main function to read, convert, and write the file
(defun main (input-file output-file)
  "Main function to read, convert, and write the file."
  (let* ((lines (read-file input-file))
         (converted-lines (convert-lines lines)))
    (write-file output-file converted-lines)))

(defun m ()
  (main "input.c" "output.lisp"))