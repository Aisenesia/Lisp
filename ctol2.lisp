(ql:quickload "cl-ppcre")

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

(defun convert (line conversion-fn)
  "Convert a line of C code to Lisp using the appropriate conversion function."
  (format t "Converting line: ~A using function: ~A~%" line conversion-fn)
  (funcall conversion-fn line))

(defun convert-lines (lines)
  "Recursively process each line and convert it to Lisp."
  (if (null lines)
      nil
      (let* ((line (car lines))
             (line-type (line-type line))
             (conversion-fn (conversion-foo line-type))
             (converted-line (convert line conversion-fn)))
        (cons converted-line (convert-lines (cdr lines))))))

(defun convert-if (line)
  "Convert an 'if' statement from C to Lisp."
  (let ((extracted (split-if-into-parameters line)))
    (print extracted)
    (format nil "(if ~A(progn " (c-to-lisp-arithmetic (first extracted)))))

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
          (list var-name (cl-ppcre:regex-replace ";" value "")))  ;; Return the captured groups with ";" removed from value
        (format t "No match~%"))))

(defun convert-assignment (line)
  "Converts a variable assignment from C to Lisp."
  ;; Assuming line can be 'int x = 5;' or 'x = 5;'
  (let ((extracted (extract-assignment line)))
    (let ((value (cl-ppcre:regex-replace-all "\\s+" (second extracted) "")))
      (if (cl-ppcre:scan "^\\w+\\s*\\(.*\\)$" value)
            (if (cl-ppcre:scan "^\\w+\\s*\\(\\s*\\)\\s*$" value)
              (format nil "(setf ~A (~A))" (first extracted) (cl-ppcre:regex-replace-all "\\s*\\(\\s*\\)\\s*" value "")) ; Handle function calls that have no arguments
              (format nil "(setf ~A (~A))" (first extracted) value)) ; Handle function calls that have arguments
          (format nil "(setf ~A ~A)" (first extracted) value))))) ; Handle simple assignments

(defun convert-return (line)
  "Convert a 'return' statement from C to Lisp."
  (format nil "~A~%" (cl-ppcre:regex-replace ";" (extract-return line) ""))) ; Remove the semicolon from extracted
  ;Example usage
  
    (defun convert-print (line)
    "Convert a 'printf' statement from C to Lisp."
    (cl-ppcre:register-groups-bind (format-string params)
        ("^\\s*printf\\s*\\((\"[^\"]*\")(.*)\\);?\\s*$" line)
      (let* ((clean-format-string (cl-ppcre:regex-replace-all "%\\w" "~A" format-string))
             (clean-params (cl-ppcre:split "\\s*,\\s*" params)))
        (format nil "(format t \"~A\" ~{~A~})" clean-format-string clean-params))))
  ;Example usage
  ;(print (convert-print "printf(\"%d\\n\", i);"))



(defun convert-function-call (line)
  "Convert a function call from C to Lisp."
  (cl-ppcre:register-groups-bind (function-name params)
      ("^\\s*(\\w+)\\s*\\(([^)]*)\\)\\s*;\\s*$" line) 
    (let ((args (cl-ppcre:split "\\s*,\\s*" params))) ; split by comma
      (format nil "(~A ~{~A~})" function-name args)))) 

(defun convert-function-definition (line)
  "Convert a function definition from C to Lisp."
  (let ((parts (split-function-into-parameters line)))
    (format nil "(defun ~A (~{~A~^ ~})~%(progn" (first parts) (rest parts))))

    ;example usage
(defun convert-function-declaration (line)
  "Convert a function declaration from C to Lisp."
  (cl-ppcre:register-groups-bind (return-type function-name params)
      ("^\\s*(\\w+)\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*;\\s*$" line)
    (let ((param-names (extract-param-names params)))
      (format nil "(declaim (ftype (function (~{~A~^ ~}) ~A) ~A))"
              param-names (c-to-lisp-type-names return-type) function-name))))


(defun convert-block-start (line)
  "Convert a block start from C to Lisp."
  (format nil "(progn~%"))

(defun convert-block-end (line)
  "Convert a block end from C to Lisp."
  (format nil "))~%"))

(defun convert-unknown (line)
  "Convert an unknown line from C to Lisp."
  (format nil ";; Unknown line: ~A~%" line))

(defun extract-param-names (params)
  "Extract the parameter names from a C-style function parameter list, e.g., 'int a, int b'."
  (mapcar #'second 
          (mapcar (lambda (param)
                    (cl-ppcre:split #\Space param))
                  (cl-ppcre:split #\, params))))

(defun extract-return (line)
  "Extract the return value from a C-style return statement, e.g., 'return 5;'."
  (cl-ppcre:register-groups-bind (return-value)
      ("^\\s*return\\s+([^;]+);?\\s*$" line)
    (if return-value
        (c-to-lisp-arithmetic return-value)
        (format t "No match for return statement~%"))))

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

(defun c-to-lisp-type-names (type)
  "Convert a C type to the equivalent Lisp type."
  (case (intern (string-upcase type) :keyword)
    (:INT 'integer)
    (:FLOAT 'float)
    (:DOUBLE 'double-float)
    (:CHAR 'character)
    (:VOID 'void)
    (t "type")))

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
  (cl-ppcre:register-groups-bind (function-name params)
      ("^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" str)
    (cons function-name
          (if (string= params "")
              '() ; base case
              (split-param-helper params))))) ; recursively create the list

(defun split-if-into-parameters (str)
  (let ((parts '()))
    (cl-ppcre:register-groups-bind (condition)
        ("^\\s*if\\s*\\(([^)]*)\\)\\s*\\{" str)
      (when (not (string= condition ""))
        (push condition parts)))
    (nreverse parts)))

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

(defun main (input-file output-file)
  "Main function to read, convert, and write the file."
  (let* ((lines (read-file input-file))
         (converted-lines (convert-lines lines)))
    (write-file output-file converted-lines)))

(defun m ()
  (main "input.c" "output.lisp"))