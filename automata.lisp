;;;; automata.lisp


(ql:quickload "str")
(ql:quickload "cl-arrows")

(defpackage :automata
  (:use :cl :cl-arrows)
  (:export :main :read-db :parse-lines :filter-lines :sw-bus
   :select-by-function
   ))

(in-package #:automata)

(defun read-db (filename)
  (str:from-file filename))

(defun split-lines (data)
  "Gets a long string, separate on newline"
  (str:lines data))

(defun sw-bus (li)
  (str:starts-with? "!_" li))

(defun filter-lines (data-lines)
  "remove any lines that start with !_"
  (remove-if #'sw-bus data-lines))

(defun make-entry ( name file start end )
  (list :func-name name :file file :start start :end end))

(defun get-val (s)
  (parse-integer
   (first (last (str:split ":" s)))))

(defun fix-val (v)
  (- (get-val v) 1)
  )

(defun parse-lines (d)
  "Parse the line into more usable terms"
  (mapcar (lambda (arg)
            (let ((sl (str:split #\Tab arg)))
              (make-entry (first sl)
                          (second sl)
                          (fix-val (fifth sl))
                          (get-val (seventh sl))
                          )
              )
            )d))


(defun select-by-function (func-name db)
  (format t "running~%")
  (remove-if-not
   #'(lambda (entry) (equal (getf entry :func-name) func-name)) db))


(defun load-database ()
  (-<> "/Users/wmealing/Projects/automata/tests/tags"
       (read-db <>)
       (split-lines <>)
       (filter-lines <>)
       (parse-lines <>)
       )
  )

(defun print-function-match-debug (match)
  (format t "=========================== ~%" )
  (format t "Function      : ~a ~%" (getf (first d) :func-name))
  (format t "Starts at line: ~a ~%" (getf (first d) :start))
  (format t "Ends at line  : ~a ~%" (getf (first d) :end))
  (format t "=========================== ~%" )
  )

(defun extract-function (filename line-start line-end)
  (let (( contents (str:lines (str:from-file filename)))
        )
    (subseq contents line-start line-end)
    )
  )

(defun make-function-printable(contents)
  (join #\Newline contents)  )

(defun main ()
;;;  (print (load-database ))
  (setf d (select-by-function "another_one" (load-database)))
  (print-function-match-debug d)
  (format t "~a ~%" (make-function-printable (extract-function "tests/test.c" 6 10 )))
  T
  )
