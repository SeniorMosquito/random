; TODOs
; pretty print output with colors, bolded first letter, arrows between chars?
; each word can only be found once - watch out when implementing location memory
; location memory - remember where a word has been found
; filter words that are too long
; handle unicode chars
; profiling
; performance improvements
; other solutions (trie, sqlite db?)
; diagonal
; printing while running
; sorting output (will require collect instead of print, prevent direct printing)


(defun random-character ()
  (character (+ (random 26) 97)))

(defparameter *field-size* 4)
(defparameter *field* (make-array (list *field-size* *field-size*)))

(defun init-field ()
  (loop
     for i
     from 0
     below (array-total-size *field*)
     do (setf
	 (row-major-aref *field* i)
	 (random-character))))

(defun print-field ()
    (loop for row from 0 below *field-size* do 
       (progn
	 (loop for col from 0 below *field-size* do
	      (format t "~c " (aref *field* row col)))
	 (format t "~%"))))



(defun string-starts-with (str1 str2)
  (or
   (string-equal str1 str2)
   (>= (string/= str1 str2) (length str1))))

(defparameter *test-words* `("berg" "bergbau" "hans" "bergen" "semmel" "xul" "jo"))
(defun words-starting-with (seq)
  (remove-if-not (lambda (x) (string-starts-with seq x)) *test-words*))

(defun get-neighbour-positions (pos)
  (remove-if (lambda (x) (or
			  (< x 0)
			  (>= x (array-total-size *field*))))
	     (list
	      (- pos 1)
	      (+ pos 1)
	      (- pos *field-size*)
	      (+ pos *field-size*))))

(defun get-neighbour-chars (pos)
  (map `list `get-character-at-pos (get-neighbour-positions pos)))

(defun get-character-at-pos (pos)
  (row-major-aref *field* pos))

(defun flo ()
    (loop for start-char-pos 
       from 0 
       below (array-total-size *field*) 
       do
	 (let ((visited-pos (list start-char-pos)) (current-string (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)))
	   (check-pos start-char-pos current-string visited-pos))))

(defun find-matching-word (words word)
	   (find word words :test `string-equal))

(defconstant +min-word-size+ 3)

(defun check-pos (pos current-string visited-pos)
;  (format t "checking ~a~%" pos)
  (vector-push-extend (get-character-at-pos pos) current-string)
  (let ((remaining-words (words-starting-with current-string)))
    (if (> (length remaining-words) 0)
	(progn
	  (if (and
	       (>= (length current-string) +min-word-size+)
	       (find-matching-word remaining-words current-string))
	      (progn (format t "found: ~a~%" current-string) (force-output t)))
	  ;(format t "~a~5t~a~%" current-string remaining-words)
	  (loop for neighbour in (get-neighbour-positions pos) do
	       (if (not (find neighbour visited-pos))
		       (check-pos neighbour current-string visited-pos)))
	  ))
    (vector-pop current-string)))

(defun load-dict ()
  (defparameter *test-words* `(""))
  (let ((in (open "/usr/share/dict/words" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do (nconc *test-words* (list line)))
      (close in))))