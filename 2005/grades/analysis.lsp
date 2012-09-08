;;   Put the block comment symbols around this part to load the defs


;;;;  Code for parsing scanned forms

;;;;
;;;;  25 Aug 02  - Copy into lisp format file
;;;;  28 Aug 00  - Add keyed arguments.
;;;;

;--- Read data from file and split into lists.  Prompts for file.
;    Adjust the column defns in parse-line for current fields.
;    Column locations are zero origin as determined by emacs C-x =
;    NOTE: make sure in mac file format else off by char.

(load "analysis")  ; this file

(def grades (read-grades "scan.dat" :name 0 :ssn 29 :ques 45 :nques 45))
(format t "Number grade lines read: ~d~%" (length grades))
(format t "First record...~% ~a ~%" (first grades))

(def names (mapcar #'first  grades))
(def ssn   (mapcar #'second grades))
(def ques  (mapcar #'third  grades))

;--- If the solution key is not the first item, then insert one
;    as done in the next two lines
(def ques
     (mapcar #'(lambda (items) (push 'a items))
             ques))
(format t "Lengths  ~% ~a ~% should all be the same~%"
        (mapcar #'length ques))

;--- Tabulate scores (result of reading is list of lists, with
;        each in format (name ssn (list of char grades)))  The keys
;        show solutions for each version, with * denoting question not
;        scored.  The first key item identifies the key itself.
                             
(def keys
     (list
       '(a  ; dummy first key since this exam did not have one
         a c c e d b   d   e   e   d         ;  1-
         a d c d c a (c e) e   e   e         ; 11-
         c d e b d a   a   d   e   a         ; 21-
         a a e e e c   b   c (d e) d         ; 31-
         d c b b c )                         ; 41-
      ))
(format t "Solution keys for each students are ... ~a~%" (mapcar #'first ques))

;--- Get scores
(def score (mapcar #'calc-score ques))

(def nCorrect (mapcar #'first  score))
(def possible (mapcar #'second score))
(def items    (mapcar #'third  score))


;--- Report on total scores

(setf total (* 100 (/ nCorrect possible)))
(format t "Avg total of ~d is ~5,1f with SD ~6,1f~%"
        (length total) (mean total) (standard-deviation total))

(boxplot total)
(def h  (histogram total))
(send h :add-lines (kernel-dens total))


;--- Comparison across exams for a given question

(summarize-question 2)


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
                    i (corr (select binaryVar i) (select total use))
                    k
                    (let ((pos (position k '(a b c d e)))  )
                      (if pos (* 100 (/ (select fr pos) (sum fr))) -1))
                    fr))
        (iseq 0 (1- (length (assoc exam keys)))) ; 0 so numbered right
        (mapcar #'(lambda (k) (if (listp k) (first k) k)) key)
        freqs)


;--- Export the results for JMP in name order.  May need to fix names
;    to avoid missing columns in the output.  Watch for *'s in output.

(def index (order names))
(def exam  (mapcar #'first ques))
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
                (format f "LastName\tFirstName\tExam\tSSN\tScore\tnCorrect~%")
                (dolist (r (transpose (list (select lastNames  index)
                                            (select firstNames index)
                                            (select exam       index)
                                            (select ssn        index)
                                            (select total      index)
					    (select nCorrect   index)) ))
		  (apply #'format f "~15a\t~15a\t~a\t~10a\t~5,2f\t~d~%" r)
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
       (format t "Key length ~d~%" (length keyList))
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

(defun SUMMARIZE-QUESTION (ques &optional (keys '(a b c d e)))
  ; Note the first item is the code for q 1, so need a 0-based index.
  ; Cannot do question one since its the alpha key
  (format t "Summary for question #~d~%" ques)
  (let ((keys (mapcar #'first ITEMS))  )
    (mapcar #'(lambda (key)
                (let* ((use (select ITEMS (which 
                                           (mapcar #'(lambda (x) (eq key x))
                                                   keys))))
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



