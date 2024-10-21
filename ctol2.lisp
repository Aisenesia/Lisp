(ql:quickload "cl-ppcre")

(defun line-type (line)
  "Determine the type of the C code line."
  (format t "Processing line: ~A~%" line)
  (cond 
        ;; Match "if" statements
        ((cl-ppcre:scan "^\\s*if\\s*\\(" line) 
         (progn (format t "Matched if~%") 'if))
         
        ;; Match "for" loops
        ((cl-ppcre:scan "^\\s*for\\s*\\(" line) 
         (progn (format t "Matched for~%") 'for))
         
        ;; Match "while" loops
        ((cl-ppcre:scan "^\\s*while\\s*\\(" line) 
         (progn (format t "Matched while~%") 'while))
         
        ;; Match assignments with optional type declarations (e.g., "int x = 5;")
        ((cl-ppcre:scan "^\\s*(\\w+\\s+)?\\w+\\s*=\\s*[^;]+;?$" line) 
         (progn (format t "Matched assignment~%") 'assignment))
         
        ;; Match "return" statements
        ((cl-ppcre:scan "^\\s*return\\s*" line) 
         (progn (format t "Matched return~%") 'return))
         
        ;; Match function calls
        ((cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*;$" line) 
         (progn (format t "Matched function-call~%") 'function-call))
         
        ;; Match function definitions
        ((cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*\\{" line) 
         (progn (format t "Matched function-definition~%") 'function-definition))
         
        ;; Match block start
        ((cl-ppcre:scan "^\\s*\\{" line) 
         (progn (format t "Matched block-start~%") 'block-start))
         
        ;; Match block end
        ((cl-ppcre:scan "^\\s*\\}" line) 
         (progn (format t "Matched block-end~%") 'block-end))
         
        ;; Unknown line
        (t (progn (format t "Unknown line~%") 'unknown))))


;; Conversion main functions

(defun conversion-foo (line-type) ;; returns a function
  "Return the appropriate conversion function based on the line type."
  (case line-type
    ('if #'convert-if)
    ('for #'convert-for)
    ('while #'convert-while)
    ('assignment #'convert-assignment)
    ('return #'convert-return)
    ('function-call #'convert-function-call)
    ('function-definition #'convert-function-definition)
    ('block-start #'convert-block-start)
    ('block-end #'convert-block-end)
    (t #'convert-unknown)))

(defun convert (line conversion-fn) ;; takes the function and sends the line to the function
  "Convert a line of C code to Lisp using the appropriate conversion function."
  (funcall conversion-fn line))

;; Conversion functions


(defun convert-if (line)
  "Convert an 'if' statement from C to Lisp."
  (format nil "(if ~A~%" line))

(defun convert-for (line)
  "Convert a 'for' loop from C to Lisp."
  (format nil "(loop for ~A~%" line))

(defun convert-while (line)
  "Convert a 'while' loop from C to Lisp."
  (format nil "(loop while ~A~%" line))

(defun convert-assignment (line)
  "Convert an assignment from C to Lisp."
  (let ((parts (split-string line)))
    (format nil "(setf ~A ~A~%" (second parts) (fourth parts))))

(defun convert-return (line)
  "Convert a 'return' statement from C to Lisp."
  (format nil "(return-from ~A~%" line))

(defun convert-function-call (line)
  "Convert a function call from C to Lisp."
  (format nil "(~A~%" line))

(defun convert-function-definition (line)
  "Convert a function definition from C to Lisp."
  (let ((parts (nreverse (split-string-into-parameters line))))
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

(ql:quickload "cl-ppcre")

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
    (when (> (length str) (1- start))
      (push (subseq str start) result))
    (nreverse result)))

(defun extract-param-names (params)
  "Extract the parameter names from a C-style function parameter list, e.g., 'int a, int b'."
  (mapcar #'second 
          (mapcar (lambda (param)
                    (cl-ppcre:split #\Space param))
                  (cl-ppcre:split #\, params))))


(defun split-string-into-parameters (str)
  "Split a single C line such as 'int main(int a, int b)' into the function name and parameter names. Returns a list with the function name as the first element and the parameter names as the subsequent elements."
  (let ((parts '()))
    (cl-ppcre:register-groups-bind (function-name params)
        ("^\\s*\\w+\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*\\{" str)
      (push function-name parts)
      (when (not (string= params ""))
        (setf parts (append parts (split-param-helper params)))))
        (print parts)
    (nreverse parts)))


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

"Recursive helper to split the parameters"
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


;; Conversion recursive function

(defun convert-lines (lines)
  "Recursively process each line and convert it to Lisp."
  (if (null lines)
      nil
      (let* ((line (car lines))
             (line-type (line-type line))
             (conversion-fn (conversion-foo line-type))
             (converted-line (convert line conversion-fn)))
        (cons converted-line (convert-lines (cdr lines)))))) ;; construct list with recursive append, uses cdr to exclude first element (car)


;; Main function
(defun main (input-file output-file)
  "Main function to read, convert, and write the file."
  (let* ((lines (read-file input-file))
         (converted-lines (convert-lines lines)))
    (write-file output-file converted-lines)))

;; Example usage:
;(main "input.c" "output.lisp")
;(split-string "int main (int a) {}")
(split-string-into-parameters "int main(int a, int b) {")