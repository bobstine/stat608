#|
;;   Put the block comment symbols bar-sharp before this line, save the file,
;;   fire up the inferior lisp process (M-x run-lisp), load the file
;;   and you are off and running.  Remove the symbol to get highlighting back.

;;;;  Code for parsing scanned forms

;;;;
;;;;  25 Aug 02  - Copy into lisp format file
;;;;  28 Aug 00  - Add keyed arguments.
;;;;

;--- Read data from file and split into lists.  Prompts for file.
;    Adjust the column defns in parse-line for current fields.
;    Column locations are zero origin as determined by emacs C-x = (or M-x col)
;    NOTE: make sure in mac file format else off by char.

(load "analysis")  ; this file

(def grades (read-grades "scan.sdf" :name 0 :ssn 29 :ques 38 :nques 45))
(format t "Number grade lines read: ~d~%" (length grades))
(format t "First record...~% ~a ~%" (first grades))

(def names     (mapcar #'first  grades))
(def ssn       (mapcar #'second grades))
(def origQues  (mapcar #'third  grades))

; --- Put a 4 in those idiots who left the key blank
; (setf idiots
;      (which (mapcar #'(lambda (ans) (equal 'd (first ans)))
;		     origQues)))

;--- If the solution key is not the first item, then insert one
;    as done in the next lines. 
; (def ques
;      (mapcar #'(lambda (items) (push 'a items))
;             ques))

;--- Check for equal numbers for each

(format t "Lengths  ~% ~a ~% should all be the same~%"
        (mapcar #'length origques))
(which (/= 45 (mapcar #'length origques)))

;--- Tabulate scores (result of reading is list of lists, with
;        each in format (name ssn (list of char grades)))  The keys
;        show solutions for each version, with * denoting question not
;        scored.  The first key item identifies the key itself.
;
;    These utility functions handle rotations of the keys.
;

(defun convert-ques (q)
  (let ((shift (position (first q) '(A x D C B)))   ) ; adjust to get rotation (rotate to top)
    (if (zerop shift) ; leave alone
	q
      (cons 'a (rotate-choices (rest q) shift)))
    ))

(defun rotate-choices (choices k)
  (mapcar #'(lambda (c) (let ((p (position c '(A B C D E)) ))
			  (if p ; its a letter, so shift it
			      (elt '(A B C D E) (mod (+ p k) 5))
			    '*)))
	  choices))

; Convert all of the tests as if used scan form A

(def ques (mapcar #'convert-ques origQues))

(let ((k
       '(a e d d a e a d e e       ;  1-  key is first item
	 b c a e c c d d d a       ; 11- 
	 b d c a b c d c a a       ; 21-
	 a a d b e d c e d a       ; 31-
         a d a e c )               ; 41-
     ))
  (def keys (list k))
  )

(format t "Solution keys to be used are ... ~a~%" (mapcar #'first ques))

;--- Get scores
(def score (mapcar #'calc-score ques))

(def nCorrect (mapcar #'first  score))
(def possible (mapcar #'second score))
(def items    (mapcar #'third  score))

(def total (* 100 (/ nCorrect possible)))

;--- Check up on the idiots
(select nCorrect idiots)

(defun set-key (q k)  (setf (select q 0) k))
(mapcar #'set-key 
	(select origQues idiots)
	'(b b  a a))

(mapcar #'first
	(mapcar #'calc-score 
		(mapcar #'convert-ques
			(select origQues idiots))))

;--- Report on total scores

(format t "Avg total of ~d is ~5,1f with SD ~6,1f~%"
        (length total) (mean total) (standard-deviation total))

(boxplot total)
(def h  (histogram total))
(send h :add-lines (kernel-dens total))


;--- Comparison across exams for a given question

(mapcar #'(lambda (i) (summarize-question i '(a b c)))  
	(iseq 2 42))

;--- Item analysis for each exam

(def exam       'a)
(def key        (assoc exam keys))
(def examCodes  (mapcar #'first items))
(def use        (which (mapcar #'(lambda (x) (eq exam x)) examCodes)))
(format t "Avg score for exam ~a is ~5,2f (SD = ~6,2f)~%"
        exam (mean (select total use)) 
        (standard-deviation (select total use)))

(def binaryVar  (transpose (select items use)))
(def freqs      (mapcar #'freq (transpose (select ques use))))
(mapcar #'(lambda (i k fr) 
            (format t "Ques ~2d (~6,2f): ~a  ~5,1f% correct, choices ~{~3d ~}~%"
                    (1+ i) (corr (select binaryVar i) (select total use))
                    k
                    (let ((pos (position k '(a b c d e)))  )
                      (if pos (* 100 (/ (select fr pos) (sum fr))) -1))
                    fr))
        (iseq (length (assoc exam keys))) ; 0 so numbered right
        (mapcar #'(lambda (k) (if (listp k) (first k) k)) key)
        freqs)


;--- Export the results for JMP in name order.  May need to fix names
;    to avoid missing columns in the output.  Watch for *'s in output.

(def index (order names))
(def exam  (mapcar #'first origques))
(def lastNames (mapcar #'up-to-blank names))
(def firstNames (mapcar #'(lambda (s) ; pull out name
                          (let ((p (position #\_ s))  )
                            (if p (up-to-blank (subseq s (1+ p))) s)))
                      names))
(def firstNames (mapcar #'(lambda(s)  ; fill in something for blank names 
			   (if (zerop (length s))
			       "blank"
			     s))
			firstNames))

(with-open-file (f "forjmp.txt" :direction :output)
                (format f "LastName\tFirstName\tExam\tPennID\tScore\tnCorrect~%")
                (dolist (r (transpose (list (select lastNames  index)
                                            (select firstNames index)
                                            (select exam       index)
                                            (select ssn        index)
                                            (select total      index)
					    (select nCorrect   index)) ))
		  (apply #'format f "~15a\t~15a\t~a\t~10a\t~5,2f\t~d~%" r)
		  ))

;; --- This code writes a file with the total score first, then the item selected
;;     as the answer, with item as a letter. Below explodes as dummies.

(with-open-file (f "forregr.txt" :direction :output)
		(format f "total ")
		(mapcar #'(lambda (i) (format f "Q~d " i))
			(iseq 2 (length (first ques)))  )
		(format f "~%")
		(setf count 0)
		(dolist (q ques) 
		  (format f "~d " (select nCorrect count))
		  (incf count)
		  (mapcar #'(lambda (a) (format f "~a " a)) (rest q))
		  (format f "~%")
		  ))

; Write out as dummy vars

(defun as-dummy (c)
  (let ((z (repeat 0 5))
	(p (position c '(a b c d e)))   )
    (when p
      (setf (elt z p) 1))  
    z))

(with-open-file (f "forregr.txt" :direction :output)
		(format f "total ")
		(mapcar #'(lambda (i c) 
			    (let ((sym (repeat 'D 5))
				  (p   (position c '(A B C D E)))  )
			      (when p
				(setf (select sym p) 'C))
			      (format f "~a~da ~a~db ~a~dc ~a~dd ~a~de " 
				      (elt sym 0) i (elt sym 1) i (elt sym 2) i
				      (elt sym 3) i (elt sym 4) i)))
			(iseq 2 (length (first ques)))
			(rest (first keys)))
		(format f "~%")
		(setf count 0)
		(dolist (q ques) 
		  (format f "~d " (select nCorrect count))
		  (incf count)
		  (mapcar #'(lambda (a) (apply #'format f "~d ~d ~d ~d ~d " (as-dummy a))) (rest q))
		  (format f "~%")
		  ))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun READ-GRADES (filename &key name ssn ques nQues) ; last are positions
  ; Returns as  list of three cols... period, date, value.
  (setf lines ()) ; global empty list holding scanned strings
  (let ((priorLine ()) 
        (data ())
        (line ()) 
        (obs ())    )
    (format t "file is ~a~%" name)
    (with-open-file (fileStream filename :direction :input)
                    (loop (setf line (read-line fileStream nil 'eof))
                          (when (eq line 'eof)
                                (format t "Read completed.~%")
                                (return))
                          (when (or (eq line nil) (< (length line) 2))
                                (format t "Read completed with empty line.~%")
                                (return))
                          (push line lines)
                          (push (parse-line line name ssn ques nQues) data)
                          (setf priorLine line))   )
    (setf data (reverse data))
    ))

(defun PARSE-LINE (line name ssn ques nQues)
  ; Input is a string, noting that they will repeat first ten items
  (let ((name (subst-blanks (string-trim " " (subseq line name (+ name 19)))))
        (ssn  (subseq line ssn (+ ssn 9)))
        (ques (coerce (subst-blanks (subseq line ques (+ ques nQues))) 'list)) )
    (def save line)
    (if (zerop (length name)) (setf name "BLANK_NAME"))
    (format t "~a ~a ~{ ~a~}~%" name ssn ques)
    (list name ssn (mapcar #'as-symbol ques))
    ))


(defun SUBST-BLANKS (str)
  ; Replaces blanks by underscores
  (dotimes (i (length str))
           (when (char= #\space (elt str i))
                 (setf (elt str i) #\_ ))
           )
  str)


(defun UP-TO-BLANK (str)
  (let ((p (position #\_ str))  )
    (if p (subseq str 0 p) str)
    ))


          
(defun AS-SYMBOL (x)
  ; Converts char number into a symbol a,b,c,...
  (case x
    (#\0  'x)
    (#\1  'a)
    (#\2  'b)
    (#\3  'c)
    (#\4  'd)
    (#\5  'e)
    (#\_  'y)  ))



(defun CALC-SCORE (s)
     (let ((res     ())     ; skip first and 
           (keyList (rest (assoc (first s) keys)))   )
       ; (format t "Key length ~d~%" (length keyList))
       (dotimes (i (length keyList))
                (let* ((key  (elt keyList i))    
                       (pick (elt s (1+ i)))  )  ; skip choice
                  (push (cond
                          ((listp key)
                           (if (member pick key) 1 0))
                          ((eq '* key)          '*)
                          ((eq pick key)         1)
                          (     t                0)  )
                        res)))
       (let ((valid (remove-if-not #'numberp res))  )
         (list (sum valid)
               (length valid)
               (cons (first s) (reverse res)))
         )))

(defun SUMMARIZE-QUESTION (ques keys)
  ; Note the first item is the code for q 1, so need a 0-based index.
  ; Cannot do question one since its the alpha key
  (format t "Summary for question #~d~%" ques)
  (let ((ki (mapcar #'first ITEMS))  )
    (mapcar #'(lambda (key)
                (let* ((use (select ITEMS (which 
                                           (mapcar #'(lambda (x) (eq key x))
                                                   ki))))
                       (sum (sum (mapcar #'(lambda (x) (select x (1- ques)))
                                         use)))  )
                  (format t "~3a  ~5,0f% (n = ~d)~%" key 
                          (* 100 (/ sum (length use))) (length use))))
	    keys)))

(defun FREQ (x)
  ; Number a,b,c,  in list x
  (mapcar #'(lambda (sym) (count-if #'(lambda (y) (eq y sym)) x))
          '(a b c d e)))

(defun CORR (x y)
  (let* ((use  (which
                (mapcar #'(lambda (a b) (and a b))
                        (mapcar #'numberp x) (mapcar #'numberp y))))
         (xUse (select x use))
         (yUse (select y use))  )
    (if use
        (let ((xDev (- xUse (mean xUse)))
              (yDev (- yUse (mean yUse)))
              (sdx  (standard-deviation xUse))
              (sdy  (standard-deviation yUse))  )
          (if (< 0 (min sdx sdy))
              (/ (/ (inner-product xDev yDev) (1- (length x)))
                 (* sdx sdy))
              0))
        0)))
          

(defun UP-TO-BLANK (str)
  (let ((p (position #\_ str))  )
    (if p (subseq str 0 p) str)
    ))


               

#|
  (parse-line 
   "SHAFFER CORINNE D   550279827422324133354321434552421535432334412441113251")

  (score-grades '(a b c) '(#\1 #\2 #\4))

  (freq '(a a b c a c d))
|#



