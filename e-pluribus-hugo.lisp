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
  '((1 . "Novel") (2 . "Novella") (3 . "Novelette") (4 . "Short story")
    (5 . "Non-Fiction Book") (6 . "Dramatic Presentation")
    (7 . "Professional Editor") (8 . "Professional Artist")
    (9 . "Semiprozine") (10 . "Fanzine") (11 . "Fan Writer")
    (12 . "Fan Artist") (13 . "John W. Campbell Award (Not a Hugo)")))

(defun categories ()
  (mapcar #'car *category-alist*))

(defun category-description (category)
  "Return the full description of the CATEGORY."
  (cdr (assoc category *category-alist*)))

;; Each voter has one ballot.  Each ballot has multiple categories.
;; There is a list of nominated works or authors for each category on a
;; ballot.
;;
;; An individual ballot is modeled as an association of the voter ID
;; and a hash table with the categories as the keys.  Each value in
;; the hash table is a list of titles or authors.  The full set of
;; ballots is simply a list of individual ballots.

(defun make-ballot (voter)
  "Create an association list element representing a ballot."
  (cons voter (make-hash-table)))

(defun add-nomination (ballot category title)
  "Add a nominated work (title or author) to the BALLOT.
  No work can be listed more than once on the same ballot."
  (let ((nominations (cdr ballot)))
    (if (null (gethash category nominations))
        (setf (gethash category nominations) (list title))
        (pushnew title (gethash category nominations) :test 'string=))))

;; The field names and column ranges in the sample data file.
(defparameter *fields-alist*
  '(('voter . (0 5))
    ('category . (5 8))
    ('title . (27 74))))

(defun parse-fields (line)
  "Get the voter ID, category, and title from a ballot file LINE."
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

(defun find-ballot (voter ballots)
  "Find the ballot for VOTER."
  (assoc voter ballots :test 'string=))

(defun file->ballots (filename)
  "Convert the raw data into ballots."
  (with-open-file (file filename)
    (do ((ballots (list))
         (line (read-line file nil 'eof) (read-line file nil 'eof)))
        ((eq line 'eof) ballots)
      (let* ((fields (parse-fields line))
             (voter (first fields))
             (category (parse-integer (second fields)))
             (title (canonical-string (third fields)))
             (ballot (find-ballot voter ballots)))
        (when (null ballot)
          (setf ballot (make-ballot voter))
          (push ballot ballots))
        (add-nomination ballot category title)))))

(defconstant +1984-data-file+
  "/Users/Patrick/projects/e-pluribus-hugo/catsort.txt")

(defparameter *ballots* (file->ballots +1984-data-file+))

(defun category-ballots (ballots category)
  "Return a list of lists containing the votes for works in CATEGORY
  across all BALLOTS."
  (let ((category-ballots (list)))
    (dolist (ballot ballots category-ballots)
      (let ((works (gethash category (cdr ballot))))
        (unless (null works)
          (push works category-ballots))))))

(defun category-votes (ballots category)
  "Return a list of lists with each inner list containing the title of a
  nominated work and the number of ballots on which it appeared."
  (let ((votes (list)))
    (dolist (ballot (category-ballots ballots category) votes)
      (dolist (work ballot)
        (let ((vote (find-if (lambda (vote)
                               (string= (car vote) work))
                             votes)))
          (if (null vote)
              (push (list work 1) votes)
              (incf (second vote))))))))

(defun current-rule-results (ballots)
  "Return the list of nominated works in each category that result from
  applying the current rules to the BALLOTS."
  (dolist (category (categories))
    (let ((sorted-results (sort (category-votes ballots category)
                                #'> :key #'second)))
      (format t "~A~%" (category-description category))
      (dolist (result sorted-results)
        (when (>= (second result) (second (sixth sorted-results)))
          (format t "~A:  ~A~%" (first result) (second result)))))))

;; To generate the results under the current rules, evaluate
;;   (current-rule-results *ballots*)


(defun category-titles (ballots category)
  "Return a list of all unique titles (or authors) in the CATEGORY."
  (remove-duplicates
   (reduce #'append (category-ballots ballots category))
   :test 'string=))

(defun score (contenders category-ballots)
  "Calculate the points and ballot counts for each title or author in
  CONTENDERS."
  (let ((scores (list)))
    (dolist (title contenders scores)
      (let ((points 0)
            (ballots 0))
        (dolist (ballot category-ballots)
          (when (find title ballot :test 'string=)
            (incf points (/ 1 (length ballot)))
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
  (let* ((selected-scores
          (remove-if-not (lambda (score)
                           (find (first score) titles :test 'string=))
                         scores))
         (ordered-by-ballots
          (sort (copy-seq selected-scores) #'< :key #'third))
         (cutoff (third (first ordered-by-ballots)))
         (eliminated (remove-if (lambda (score)
                                  (> (third score) cutoff))
                                selected-scores)))
    (mapcar #'first eliminated)))

(defun eliminate (eliminated category-ballots)
  "Remove the ELIMINATED title or authors from the CATEGORY-BALLOTS."
  (dotimes (i (length category-ballots))
    (setf (nth i category-ballots)
          (set-difference (nth i category-ballots) eliminated :test 'string=)))
  (remove-if #'null category-ballots))

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
  (format t "~%~A~%" (category-description category))
  (let ((ordered-results (sort (copy-seq scores) #'> :key #'second)))
    (dolist (result ordered-results)
      (format t "~A: ~A points, ~A ballots~%"
              (first result)
              (fraction->string (second result))
              (third result)))))

(defmacro while (test &body body)
  "A little syntactic sugar around DO."
  `(do () ((not ,test)) ,@body))

(defconstant +maximum-results+ 5)

(defun eph-rule-results (ballots)
  "Return the list of nominated works in each category that result from
  applying the E Pluribus Hugo rules (SDV-LPE, selection by points,
  elimination by ballot count) to the BALLOTS."
  (dolist (category (categories))
    (let ((contenders (category-titles ballots category))
          (category-ballots (category-ballots ballots category)))
      (while (> (length contenders) +maximum-results+)
        (let* ((scores (score contenders category-ballots))
               (least-popular (selection-phase scores))
               (eliminated (elimination-phase least-popular scores)))
          (setf contenders (set-difference contenders eliminated))
          (setf category-ballots (eliminate eliminated category-ballots))
          (unless (> (length contenders) +maximum-results+)
            (category-eph-results category
                                  (score contenders category-ballots))))))))

;; To generate the results under the E Pluribus Hugo rules, evaluate
;; (eph-rule-results *ballots*)
