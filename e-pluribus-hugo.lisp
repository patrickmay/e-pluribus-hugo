;;;; This is an implementation of the E Pluribus Hugo (EPH) proposal for
;;;; the 2015 WSFS Business Meeting.  EPH modifies the nomination rules
;;;; to use Single Divisible Vote with Least Popular Elimination (SDV-LPE).
;;;;
;;;; The 1984 ballots are available as test data.

(in-package :common-lisp-user)

(defpackage :org.softwarematters.e-pluribus-hugo
  (:nicknames :e-pluribus-hugo :eph)
  (:use :common-lisp))

(in-package :e-pluribus-hugo)

(defparameter *category-alist*
  '(("1" . "Novel") ("2" . "Novella") ("3" . "Novelette") ("4" . "Short story")
    ("5" . "Non-Fiction Book") ("6" . "Dramatic Presentation")
    ("7" . "Professional Editor") ("8" . "Professional Artist")
    ("9" . "Semiprozine") ("10" . "Fanzine") ("11" . "Fan Writer")
    ("12" . "Fan Artist") ("13" . "John W. Campbell Award (Not a Hugo)")))

(defun categories ()
  (mapcar #'car *category-alist*))

(defun category->description (category)
  "Return the full description of the CATEGORY."
  (cdr (assoc category *category-alist* :test 'string=)))

(defparameter *fields-alist*
  '(('voter . (0 5))
    ('category . (5 8))
    ('title . (27 74))
    ('author . (77 112))))

(defun parse-fields (line)
  "Get the voter ID, category, title, and author from a ballot file line."
  (let ((fields (list)))
    (dolist (field *fields-alist* (nreverse fields))
      (let ((start (first (cdr field)))
            (end (second (cdr field))))
        (push (string-trim " "  (subseq line start end)) fields)))))

(defun alphanumeric-p (char)
  "Return t if CHAR is alphanumeric (assumes contiguous values)."
  (or (and (char>= char #\a) (char<= char #\z))
      (and (char>= char #\A) (char<= char #\Z))
      (and (char>= char #\0) (char<= char #\9))))

(defun canonical-string (string)
  "Remove all non-alphanumeric characters from STRING and set the
  remaining to lowercase."
  (string-downcase (remove-if-not #'alphanumeric-p string)))

(defclass nominated-work ()
  ((raw-author :initarg :raw-author
               :reader raw-author
               :type string)
   (raw-title :initarg :raw-title
              :reader raw-title
              :type string)
   (canonical-title :initarg :canonical-title
                    :reader canonical-title
                    :type string))
  (:documentation
   "The title and author of a nominated work."))

(defun make-nominated-work (author title)
  "Construct a nominated work instance."
  (make-instance 'nominated-work
                 :raw-author author
                 :raw-title title
                 :canonical-title (canonical-string title)))

(defclass category-ballot ()
  ((category :initarg :category
             :reader category
             :type string)
   (nominated-works :initarg :nominated-works
                    :accessor nominated-works
                    :type list
                    :initform (list)))
  (:documentation
   "The list of nominated works for a category on a voter's ballot."))

(defun make-category-ballot (category)
  "Construct a category ballot instance."
  (make-instance 'category-ballot :category category))

(defclass ballot ()
  ((voter :initarg :voter
          :reader voter
          :type string)
   (ballot-categories :initarg :ballot-categories
                      :accessor ballot-categories
                      :type hash-table
                      :initform (make-hash-table :test 'equal)))
  (:documentation
   "A voter's Hugo ballot."))

(defun make-ballot (voter)
  "Construct a ballot instance."
  (make-instance 'ballot :voter voter))

(defgeneric add-nominated-work (ballot category nominated-work)
  (:documentation "Add a nominated work to a ballot."))

(defmethod add-nominated-work ((ballot ballot) category nominated-work)
  "Add a nominated work to a ballot."
  (let ((category-ballot (gethash category (ballot-categories ballot)
                                  (make-category-ballot category))))
    (push nominated-work (nominated-works category-ballot))
    (setf (gethash category (ballot-categories ballot)) category-ballot)))

(defgeneric category-nominated-works (ballot category)
  (:documentation "Return the list of nominated works in the CATEGORY."))

(defmethod category-nominated-works ((ballot ballot) category)
  (let ((category-ballot (gethash category (ballot-categories ballot))))
    (unless (null category-ballot)
      (nominated-works category-ballot))))

(defun category-ballots (ballots category)
  "Return a list of category-ballot objects for the CATEGORY."
  (remove-if #'null (mapcar (lambda (ballot)
                              (gethash category (ballot-categories ballot)))
                            ballots)))

(defun category-votes (ballots category)
  "Return a list of lists with each inner list containing the title of a
  nominated work and the number of ballots on which it appeared."
  (let ((votes (list)))
    (dolist (ballot (category-ballots ballots category) votes)
      (dolist (work (nominated-works ballot))
        (let ((vote (find-if (lambda (vote)
                               (string= (car vote) (canonical-title work)))
                             votes)))
          (if (null vote)
              (push (list (canonical-title work) 1) votes)
              (incf (second vote))))))))

(defun current-rule-results (ballots)
  "Return the list of nominated works in each category that result from
  applying the current rules to the BALLOTS."
  (dolist (category (categories))
    (let ((sorted-results (sort (category-votes ballots category)
                                #'> :key #'second)))
      (format t "~A~%" (category->description category))
      (dolist (result sorted-results)
        (when (>= (second result) (second (sixth sorted-results)))
          (format t "~A:  ~A~%" (first result) (second result)))))))

(defun find-ballot (voter ballots)
  "Find the ballot for VOTER."
  (find-if (lambda (ballot)
             (string= voter (voter ballot)))
           ballots))

(defun file->ballots (filename)
  "Convert the raw data into ballots."
  (with-open-file (file filename)
    (do ((ballots (list))
         (line (read-line file nil 'eof) (read-line file nil 'eof)))
        ((eq line 'eof) ballots)
      (let* ((fields (parse-fields line))
             (voter (first fields))
             (category (second fields))
             (title (third fields))
             (author (fourth fields))
             (nominated-work (make-nominated-work author title))
             (ballot (find-ballot voter ballots)))
        (when (null ballot)
          (setf ballot (make-ballot voter))
          (push ballot ballots))
        (add-nominated-work ballot category nominated-work)))))

(defconstant +1984-data-file+ "/Users/Patrick/projects/HugoVotesim/catsort.txt")

(defparameter *ballots* (file->ballots +1984-data-file+))

;; To generate the results under the current rules, evaluate
;;   (current-rule-results *ballots*)

(defun category-titles (ballots category)
  "Return a list of all unique titles (or authors) in the CATEGORY."
  (remove-duplicates
   (reduce #'append
           (mapcar (lambda (category-ballot)
                     (mapcar #'canonical-title
                             (nominated-works category-ballot)))
                   (category-ballots ballots category)))
   :test 'string=))

(defun ballots->title-lists (ballots category)
  "Return a list of lists containing the titles (or authors) on each
  ballot in the CATEGORY."
  (mapcar (lambda (category-ballot)
            (mapcar #'canonical-title
                    (nominated-works category-ballot)))
          (category-ballots ballots category)))

(defun score (contenders title-lists)
  "Calculate the points and ballot counts for each title or author in
  CONTENDERS."
  (let ((scores (list)))
    (dolist (title contenders scores)
      (let ((points 0)
            (ballots 0))
        (dolist (title-list title-lists)
          (when (find title title-list :test 'string=)
            (incf points (/ 1 (length title-list)))
            (incf ballots)))
        (push (list title points ballots) scores)))))

(defun selection-phase (scores)
  "Return a list of the two or more titles or authors with the lowest points."
  (let* ((ordered-by-points (sort (copy-seq scores) #'< :key #'second))
         (cutoff (second (second ordered-by-points)))
         (selected (remove-if (lambda (score)
                                (> (second score) cutoff))
                              scores)))
    (mapcar #'first selected)))

(defun elimination-phase (titles scores)
  "Return a list of one or more titles or authors on the fewest ballots."
  (let* ((titles-scores
          (remove-if-not (lambda (score)
                           (find (first score) titles :test 'string=))
                         scores))
         (ordered-by-ballots (sort (copy-seq titles-scores) #'< :key #'third))
         (cutoff (third (first ordered-by-ballots)))
         (eliminated (remove-if (lambda (score)
                                  (> (third score) cutoff))
                                titles-scores)))
    (mapcar #'first eliminated)))

(defun eliminate (eliminated title-lists)
  "Remove the ELIMINATED title or authors from the TITLE-LISTS."
  (dotimes (i (length title-lists))
    (setf (nth i title-lists)
          (set-difference (nth i title-lists) eliminated :test 'string=)))
  (remove-if #'null title-lists))

(defmacro while (test &body body)
  "A little syntactic sugar around DO."
  `(do () ((not ,test)) ,@body))

(defun fraction->string (fraction)
  "Reduce FRACTION and return it as a string."
  (let* ((numerator (numerator fraction))
         (denominator (denominator fraction))
         (quotient (floor (/ numerator denominator)))
         (remainder (mod numerator denominator)))
    (cond ((and (> remainder 0) (> quotient 0))
           (format nil "~A ~A" quotient (/ remainder denominator)))
          ((> remainder 0)
           (format nil "~A" fraction))
          ((> quotient 0)
           (format nil "~A" quotient))
          (t ""))))

(defun category-eph-results (category scores)
  "Print the E Pluribus Hugo results for CATEGORY."
  (format t "~%~A~%" (category->description category))
  (let ((ordered-results (sort (copy-seq scores) #'> :key #'second)))
    (dolist (result ordered-results)
      (format t "~A: ~A points, ~A ballots~%"
              (first result)
              (fraction->string (second result))
              (third result)))))

(defconstant +maximum-results+ 5)

(defun eph-rule-results (ballots)
  "Return the list of nominated works in each category that result from
  applying the E Pluribus Hugo rules (SDV-LPE, selection by points,
  elimination by ballot count) to the BALLOTS."
  ;; TODO:  in write up, write this first then the functions
  (dolist (category (categories))
    (let ((contenders (category-titles ballots category))
          (title-lists (ballots->title-lists ballots category)))
      (while (> (length contenders) +maximum-results+)
        (let* ((scores (score contenders title-lists))
               (least-popular (selection-phase scores))
               (eliminated (elimination-phase least-popular scores)))
          (setf contenders (set-difference contenders eliminated))
          (setf title-lists (eliminate eliminated title-lists))
          (unless (> (length contenders) +maximum-results+)
            (category-eph-results category
                                  (score contenders title-lists))))))))

;; To generate the results under the E Pluribus Hugo rules, evaluate
;;   (eph-rule-results *ballots*)

