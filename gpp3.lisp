;; GPP Meta-Language Lexer

(defparameter *keywords*
  '(("and" . "KW_AND")
    ("or" . "KW_OR")
    ("not" . "KW_NOT")
    ("equal" . "KW_EQUAL")
    ("less" . "KW_LESS")
    ("nil" . "KW_NIL")
    ("list" . "KW_LIST")
    ("append" . "KW_APPEND")
    ("concat" . "KW_CONCAT")
    ("set" . "KW_SET")
    ("deffun" . "KW_DEFFUN")
    ("for" . "KW_FOR")
    ("if" . "KW_IF")
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
    ;; create pairs to map operators to their tokens, can be matched with assoc
    ;; can access tokens via cdr

(defun is-keyword (token)
  "Check if token is a keyword"
  (cdr (assoc token *keywords* :test #'string-equal)))

(defun is-operator (token)
  "Check if token is an operator"
  (cdr (assoc token *operators* :test #'string=))) ;; check if token is an operator

(defun is-integer (token)
  "Check if token is an integer"
  (and (not (zerop (length token))) ;; ensure the token is not an empty string
       (every #'digit-char-p token))) ;; ensure every character in the token is a digit

(defun is-fraction (token)
  "Check if token is a fraction."
  (let ((dot-count 0))  ;; initialize a variable to count the number of dots
    (and (not (zerop (length token)))  ;; ensure the token is not an empty string
         (digit-char-p (char token 0))  ;; ensure the first character is a digit
         (every #'(lambda (c)  ;; check every character in the token
                    (cond
                      ((digit-char-p c) t)  ;; if the character is a digit, return true
                      ((char= c #\f) (incf dot-count) t)  ;; if the character is a dot, increment dot-count and return true
                      (t nil)))  ;; for any other character, return false
               token)
         (= dot-count 1)  ;; ensure there is exactly one dot in the token
         (not (char= (char token (1- (length token))) #\f)))))  ;; ensure the token does not end with a dot

(defun is-identifier (token)
  "Check if token is a valid identifier"
  (and (not (zerop (length token)))
       (alpha-char-p (char token 0)) ;; first character must be a letter
       (every #'(lambda (c) ;; rest must be alphanumeric or underscore
                  (or (alphanumericp c)
                      (char= c #\_)))
             token)))

(defun whitespace-p (char)
  "Check if character is whitespace"
  (member char '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun tokenize (input)
  "Split input string into tokens, properly handling whitespace"
  (let ((tokens '())
        (current-token "")
        (i 0)
        (len (length input)))
    (flet ((push-token () ;; helper function to push a token to the list of tokens, while pushing it, it clears the current token
             (let ((trimmed-token (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) current-token))) ;; clear whitespace
               (unless (string= trimmed-token "") ;; if token is not empty, push it to the list of tokens
                 (push trimmed-token tokens) ;; push token to tokens
                 (setf current-token ""))))) ;; clear current token
      (loop while (< i len) do
        (let ((char (char input i)))
          (cond
            ;; handle comments
            ;; dfa start here
            ((and (< i (1- len))
                  (char= char #\;) ;; if char[i] and char[i+1] are semicolons
                  (char= (char input (1+ i)) #\;))
             (push-token) ;; push current token as ;;, a comment
             (push ";;" tokens)
             (loop while ;; consume until newline or end of input since comments are ignored
                (and (< i len) 
                     (not (char= (char input i) #\Newline))) ;; while char is not newline and i < len
                   do (incf i)) ;; i++
             (when (< i len) (incf i))) ;; i++
            
            ;; handle operators
            ((find (string char) *operators* :key #'car :test #'string=) ;; if char is an operator
             (push-token) ;; push current token
             (push (string char) tokens)) ;; push the operator
            
            ;; handle whitespace
            ((whitespace-p char)
             (push-token))
            
            ;; accumulate other characters
            (t (setf current-token ;; use t to hold the current token
                     (concatenate 'string current-token (string char)))))
          (incf i)))
      
      ;; push final token if it exists
      (push-token)
      (remove-if #'(lambda (token) ;; remove empty and whitespace tokens
                     (or (string= token "") 
                         (every #'whitespace-p token)))
                 (nreverse tokens)))))



(defun categorize-token (token)
  "Categorize a single token and return its classification"
  (cond
    ((string= token ";;") "COMMENT")
    ((is-keyword token) (is-keyword token))
    ((is-operator token) (is-operator token))
    ((is-fraction token) (format nil "VALUEF:~A" token))
    ((is-integer token) (format nil "VALUEI:~A" token))
    ((is-identifier token) (format nil "IDENTIFIER:~A" token))
    (t (error "Unrecognized token - ~A" token)))) ;; raise error

(defun classify-input (input)
  "Classify all tokens in the input string"
  (remove-if #'null ;; remove nil tokens
             (mapcar #'categorize-token (tokenize input)))) ;; tokrnize input and categorize each token
          ;; then remove nil tokens and return the list of tokens

(defun handle-line (line)
  "Process a single line of input"
  (handler-case
      (dolist (token (classify-input line))
        (if token  ; only print non-nil tokens
          (format t "~A~%" token)
          (format t "UNKNOWN: ~A~%" token)
          ))
    (error (e)
      (format t "SYNTAX_ERROR: ~A~%" e))))

(defun gppinterpreter (&optional filename)
  "Start the GPP interpreter"
  (if filename
      ;; file mode
      (with-open-file (stream filename :if-does-not-exist nil)
        (if (null stream)
            (format t "Error: Could not open file ~A~%" filename)
            (loop for line = (read-line stream nil nil)
                  while line
                  do (handle-line line))))
      ;; interpreter mode
    (progn
    (format t "Welcome to the G++ interpreter.~%Type (quit) to exit.~%")
     (loop
        (format t "~%gpp> ")
        (force-output)
        (let ((line (read-line nil nil)))
          (when (or (null line) (string= line "(quit)"))
            (return "Exiting G++ interpreter."))
          (when (not (string= line ""))
            (handle-line line)))))))
          