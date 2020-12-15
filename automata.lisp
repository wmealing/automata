;;;; automata.lisp


(ql:quickload "str")
(ql:quickload "cl-arrows")

(defpackage :automata
  (:use :cl :cl-arrows)
  (:export :main :read-db :parse-lines :filter-lines :sw-bus))

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

(defun parse-lines (d)
  "Parse the line into more usable terms"
  (mapcar (lambda (arg)
            (str:split #\Tab  arg  )) d))

(parse-lines '("ONE" "TWO	DOG" "THREE"))
(str:split #\Tab "ONE	TWO	THREE")

(defun main ()
  (-<> "/Users/wmealing/Projects/automata/tests/tags"
       (read-db <>)
       (split-lines <>)
       (filter-lines <>)
       (parse-lines <>)
       (print <>))
  T
  )

(defun test-remove-if ()
  (filter-lines '("ONE" "TWO" "THREE" "!_FOUR"))
  )

(print (test-remove-if ))
