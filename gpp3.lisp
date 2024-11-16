;; GPP Language Lexer - Version 2
;; This lexer tokenizes input for the GPP language with proper token classification

(defpackage :gpp-lexer
  (:use :common-lisp)
  (:export :gppinterpreter :classify-input))

(in-package :gpp-lexer)

(defparameter *keywords*
  '(("deffun" . "KW_DEFFUN")
    ("if" . "KW_IF")
    ("equal" . "KW_EQUAL")
    ("and" . "KW_AND") 
    ("or" . "KW_OR")
    ("not" . "KW_NOT")
    ("less" . "KW_LESS")
    ("nil" . "KW_NIL")
    ("list" . "KW_LIST")
    ("append" . "KW_APPEND")
    ("concat" . "KW_CONCAT")
    ("set" . "KW_SET")
    ("for" . "KW_FOR")
    ("exit" . "KW_EXIT")
    ("load" . "KW_LOAD")
    ("disp" . "KW_DISP")
    ("true" . "KW_TRUE")
    ("false" . "KW_FALSE")))

(defparameter *operators*
  '(("+" . "OP_PLUS")
    ("-" . "OP_MINUS")
    ("/" . "OP_DIV")
    ("*" . "OP_MULT")
    ("(" . "OP_OP")
    (")" . "OP_CP")
    ("," . "OP_COMMA")))

(defun is-keyword (token)
  "Check if token is a keyword"
  (cdr (assoc token *keywords* :test #'string-equal)))

(defun is-operator (token)
  "Check if token is an operator"
  (cdr (assoc token *operators* :test #'string=)))

(defun is-integer (token)
  "Check if token is an integer"
  (and (not (zerop (length token)))
       (every #'digit-char-p token)))

(defun is-identifier (token)
  "Check if token is a valid identifier"
  (and (not (zerop (length token)))
       (alpha-char-p (char token 0))
       (every #'(lambda (c) 
                  (or (alphanumericp c)
                      (char= c #\_)))
             token)))

(defun tokenize (input)
  "Split input string into tokens"
  (let ((tokens '())
        (current-token "")
        (i 0)
        (len (length input)))
    (flet ((push-token ()
             (unless (string= current-token "")
               (push (string-trim " " current-token) tokens)
               (setf current-token ""))))
      (loop while (< i len) do
        (let ((char (char input i)))
          (cond
            ;; Handle comments
            ((and (< i (1- len))
                  (char= char #\;)
                  (char= (char input (1+ i)) #\;))
             (push-token)
             (push ";;" tokens)
             (loop while (and (< i len) 
                            (not (char= (char input i) #\Newline)))
                   do (incf i))
             (when (< i len) (incf i)))
            
            ;; Handle operators
            ((find (string char) *operators* :key #'car :test #'string=)
             (push-token)
             (push (string char) tokens))
            
            ;; Handle whitespace
            ((or (char= char #\Space)
                 (char= char #\Tab)
                 (char= char #\Newline))
             (push-token))
            
            ;; Accumulate other characters
            (t (setf current-token 
                     (concatenate 'string current-token (string char)))))
          (incf i)))
      
      ;; Push final token if exists
      (push-token)
      (nreverse tokens))))

(defun categorize-token (token)
  "Categorize a single token and return its classification"
  (cond
    ((string= token ";;") "COMMENT")
    ((is-keyword token) (is-keyword token))
    ((is-operator token) (is-operator token))
    ((is-integer token) (format nil "VALUEI:~A" token))
    ((is-identifier token) (format nil "IDENTIFIER:~A" token))
    (t (format nil "UNKNOWN:~A" token))))

(defun classify-input (input)
  "Classify all tokens in the input string"
  (mapcar #'categorize-token (tokenize input)))

(defun gppinterpreter (&optional filename)
  "Start the GPP interpreter"
  (if filename
      ;; File mode
      (with-open-file (stream filename :if-does-not-exist nil)
        (if (null stream)
            (format t "Error: Could not open file ~A~%" filename)
            (loop for line = (read-line stream nil nil)
                  while line
                  do (handle-line line))))
      ;; Interactive mode
      (loop
        (format t "~%gpp> ")
        (force-output)
        (let ((line (read-line nil nil)))
          (when (null line) (return))
          (handle-line line)))))

(defun handle-line (line)
  "Process a single line of input"
  (handler-case
      (dolist (token (classify-input line))
        (format t "~A~%" token))
    (error (e)
      (format t "SYNTAX_ERROR: ~A~%" e))))

;; Example usage
;; (gppinterpreter "gp.g")