;; automata.lisp

(ql:quickload "str")
(ql:quickload "cl-arrows")

(defpackage :automata
  (:use :cl :cl-arrows )
  (:export :main :line-matches :find-in-tags-file :line-to-structure
           :run-tests :get-releases-tags-files :format-function
   :get-all-tag-files :return-all-matches :extract-function :wrap-in-p
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
  (format t "SPLIT-KV ~a ~%" kvpair)
  (let ((key-value (str:split  ":" kvpair)))
    (parse-integer (second key-value))
    )
  )

;;; this function is NOT robust.
(defun line-to-structure (line tagfile)
  "Takes a line and returns it back as a well formatted data structure"
  (if (not (starts-with-bang-underscore line))
      (let* ((elements (str:split #\Tab line))
             (func-name (first elements))
             (tagfile-base (directory-namestring tagfile))
             (file-name (second elements))
             (file-path (merge-pathnames (pathname file-name) tagfile-base))
             (start     (split-kv (fifth elements)) )
             (end       (split-kv (seventh elements))))
        (format t "ELEMENTS~a~%"  elements)
        (format t "FUNC_NAME: ~a~%" func-name)
        (format t "file-path: ~a~%" file-path)
        (format t "start: ~a~%" start)
        (format t "end: ~a~%" end)
        `(:file ,file-path :func ,func-name :start ,start :end ,end))))


(defun get-release-from-tagfile (tagfile)
  "Release file is in the same dir as tagfile, grab contents of that"
  (let* (    (tagfile-base (directory-namestring tagfile))
             (file-path (merge-pathnames (pathname "release") tagfile-base))
             )
    (str:from-file file-path)))


(defun find-in-tags-file (tagfile func-name)
  "Find file in the tagfile this way because tagfiles can be huge."

  (list :release (get-release-from-tagfile tagfile) :match-list
        (with-open-file (stream tagfile)
          (loop for line = (read-line stream nil)
                while line
                when (eq T (line-matches line func-name))
                  collect (line-to-structure line tagfile)
                )
          )
        )
  )

(defun extract-function (file-name line-start line-end)
  (if (and file-name line-start line-end)
      (subseq
       ;; line-start - -1, because indexing here starts at 0.
       (str:lines (str:from-file file-name)) (- line-start 1) line-end))
  )

(defun extract-source (m)
  "Given a set struct of file start and end, extract that source and return it"
  (format t "EXTRACTING SOURCE ~a~%" m)
  (let* (
         (file-path (getf m :file ))
         (start-pos (getf m :start ))
         (end-pos   (getf m :end ))
         )
    (if (and file-path start-pos end-pos)
        (extract-function file-path start-pos end-pos )
        )
    ))

(defun make-function-printable(contents)
  (str:join #\Newline contents))

(defun get-all-tag-files()
  "Returns a list of 'tags files for releases "
  (directory "./code/*/tags"))

(defun find-func-in-all-tags (func-name)
  (mapcar (lambda (tagfile)
            (find-in-tags-file tagfile func-name)) (get-all-tag-files)))

(defun format-function (match-data)
  (if match-data
      (list :release (getf match-data :release )
            :match-list
            (loop for el in (getf match-data :match-list )
                  collect (extract-source el)
                  ))))


(defun return-all-matches (func-name)
  (mapcar #'format-function (find-func-in-all-tags func-name))
  )

(defun main ()
  (print (find-func-in-all-tags "main"))
  nil)
