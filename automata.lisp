;; automata.lisp

(ql:quickload "str")
(ql:quickload "cl-arrows")

(defpackage :automata
  (:use :cl :cl-arrows)
  (:export :main
           :parse-lines :filter-lines :sw-bus
           :reload :select-by-function :find-func-in-all-tags
           :🆑))

(in-package #:automata)

(defun read-db (filename)
  (str:from-file filename))

(defun split-lines (data)
  "Gets a long string, separate on newline"
  (str:lines data))

(defun sw-bus (li)
  "Starts with bang underscore"
  (str:starts-with? "!_" li))

(defun filter-lines (data-lines)
  "remove any lines that start with !_"
  (remove-if #'sw-bus data-lines))

(defun make-entry ( name file start end )
  "Turns parameters into a map /vector ?"
  (list :func-name name :file file :start start :end end))

(defun get-val (s)
  (parse-integer
   (first (last (str:split ":" s)))))

(defun fix-val (v)
  (- (get-val v) 1)
  )

(defun parse-lines (dir line)
  "Parse the line into more usable terms"
  (mapcar (lambda (arg)
            (let* ((sl (str:split #\Tab arg))
                   (func-name (first sl))
                   (file-name (second sl))q
                   (start (fix-val (fifth sl)))
                   (end (get-val (seventh sl)))
                   (full-path (str:join "" (list dir file-name)))
                  )
              (make-entry func-name full-path start end)
              )
            )line))

(defun select-by-function (func-name db)
  (remove-if-not
   #'(lambda (entry) (equal (getf entry :func-name) func-name)) db))


(defun load-database (tagfile)
  (let ((dir (directory-namestring tagfile)))
  (-<> tagfile
       (read-db <>)
       (split-lines <>)
       (filter-lines <>)
       (parse-lines dir <>)
       )))

(defun extract-function (file-name line-start line-end)
  (subseq
   (str:lines (str:from-file file-name)) line-start line-end))

(defun make-function-printable(contents)
  (str:join #\Newline contents))

(defun get-releases-tags-files()
  "Returns a list of 'tags files for releases "
  (directory "./code/*/tags"))

(defun find-func-in-all-tags (function)
  (let ( (tags (get-releases-tags-files) ) )
    (mapcar (lambda (tagfile)
              (->> tagfile
                   (load-database)
                   (select-by-function function)
                   (first) ;; why ?
                   )
              ) tags
                )))

(defun print-function (f)
  (let* (
         (file-name (getf f :file))
         (line-start (getf f :start))
         (line-end (getf f :end))
         (func-data (extract-function file-name line-start line-end) )
         )
    (format t "== START SOURCE: ~a ==" file-name )
    (format t "~%~a~%" (str:join #\Newline func-data))
    (format t "== END SOURCE ==~%~%")
    nil
    ))

(defun print-all-functions (f)
  (format t "Looking for function: -= ~a =- ~%" "main")
  (mapcar #'print-function (find-func-in-all-tags "main"))
  )

(defun main ()
  "The main function"
  (print-all-functions "main")
  )


(defun build-tags ()
  (-<> (directory "./code/*")
       (print <>)
   ))

(defun reload()
  (ql:quickload "automata")
  (automata:main))
