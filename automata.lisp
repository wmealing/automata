;; automata.lisp

(ql:quickload "str")
(ql:quickload "cl-arrows")
(ql:quickload "fiveam")
(ql:quickload "gtwiwtg")

(defpackage :automata
  (:use :cl :cl-arrows :gtwiwtg)
  (:export :main :line-matches :find-in-tags-file :line-to-structure
           :run-tests :get-releases-tags-files
           :get-all-tag-files :reload
           :select-by-function :find-func-in-all-tags))

(in-package #:automata)

(defun starts-with-bang-underscore(li)
  "Starts with bang underscore"
  (str:starts-with? "!_" li))

;; FIXME: this may need to be improved to match 'actual' function srather than
;; just the first element.

(defun line-matches (line func-name)
  "Takes a line from ctags file and tests to see if the line starts with..."
  (let ((elements (str:split #\Tab line)))
    (if (not (str:starts-with? "!_" (nth 0 elements)))
        (if (string= (nth 0 elements) func-name)
            T)
        nil)
    )
  )

(defun split-kv(kvpair)
  (parse-integer (second (str:split  ":" kvpair)))
  )

;;; this function is NOT robust.
(defun line-to-structure (line tagfile)
  "Takes a line and returns it back as a well formatted data structure"
  (print tagfile)
  (if (not (starts-with-bang-underscore line))
      (let* ((elements (str:split #\Tab line))
             (func-name (first elements))
             (tagfile-base (directory-namestring tagfile))
             (file-name (second elements))
             (file-path (merge-pathnames (pathname file-name) tagfile-base))
             (start     (split-kv (fifth elements)) )
             (end       (split-kv (seventh elements))))
        (list :file file-path :func func-name :start start :end end))
      ))

(defun find-in-tags-file (tagfile func-name)
  "Find file in the tagfile this way because tagfiles can be huge."
  (with-open-file (stream tagfile)
    (loop for line = (read-line stream nil)
          while line do
            (if (eq T (line-matches line func-name))
                (progn
                  (return (line-to-structure line tagfile ))
                  )
                )
          )))

(defun extract-function (file-name line-start line-end)
  (subseq
   (str:lines (str:from-file file-name)) line-start line-end))

(defun make-function-printable(contents)
  (str:join #\Newline contents))

(defun get-all-tag-files()
  "Returns a list of 'tags files for releases "
  (directory "./code/*/tags"))

(defun find-func-in-all-tags (function)
  (progn
    (format t "Looking for function: ~a~%" function )
    (mapcar (lambda (tagfile)
            (find-in-tags-file tagfile function)) (get-all-tag-files) )
    )
  )


(defun print-function (f)
  (if (not (eq f nil) )
      (let* (
             (file-name (getf f :file))
             (line-start (- (getf f :start) 1)) ;; indexing change.
             (line-end  (getf f :end) ) ;; include the ending brace ?
             (func-data (extract-function file-name line-start line-end))
             )
        (progn
          (format t "~%== START SOURCE: ~a Lines: ~a-~a==" file-name line-start line-end )
          (format t "~%~a~%" (str:join #\Newline func-data))
          (format t "== END SOURCE ==~%~%")
          )
        )
      (format t  "Function to print is nil? : ~a~% " f)
      ))

(defun print-all-functions (f)
  (format t "Looking for function: -= ~a =- ~%" f)
  (mapcar #'print-function (find-func-in-all-tags f)))

(defun main ()
  "The main function"
  (print-all-functions "main")
;;  (find-func-in-all-tags "main")
  )

(defun reload ()
  (ql:quickload "automata")
  (ql:quickload 'automata/tests))

(defun run-tests ()
  (automata-tests::test-automata))
