;; automata.lisp

(ql:quickload "str")
(ql:quickload "split-sequence")
;; (ql:quickload "trivia")


(defpackage :automata
  (:use :cl)
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
  (let ((elements (split-sequence:split-sequence #\Tab line)))
    (if (not (str:starts-with? "!_" (elt elements 0)))
        (if (string= (elt elements 0) func-name)
            T)
        nil)
    )
  )

(defun split-kv(kvpair)
  "Splits a string in :"
  (let ((key-value (str:split ":" kvpair)))
    (if key-value
        (parse-integer (second key-value))
        )))


(defun find-v-kv (line key)
  "Find a value by key in a line"

  (loop for e in (split-sequence:SPLIT-SEQUENCE #\Tab line)
        do (format t "Iterating over: ~a ~%" e )
        when (str:starts-with? (str:concat key ":") e)
          return (values (split-kv e))
        ))



(defun find-v-kv-orig (line key)
  "Find a value by key in a line"
  (if line
      (mapc (lambda (element)
              (format t "KEY: ~a~%" key)
              (format t "ELEMENT: ~a~%" element)
              (let ((newprefix (str:concat key ":")))
                (if (str:starts-with? newprefix element)
                    (split-kv element)
                    ))
              )
            (split-sequence:SPLIT-SEQUENCE #\Space line)
            )))


;;; this function is NOT robust.
(defun line-to-structure (line tagfile)
  "Takes a line and returns it back as a well formatted data structure"
  (format t "LINE: ~a~%" line)
  (unless (starts-with-bang-underscore line)
    (let* ((elements (str:split #\Tab line))
           (func-name (first elements))
           (tagfile-base (directory-namestring tagfile))
           (file-name (second elements))
           (file-path (merge-pathnames (pathname file-name) tagfile-base))
           (start (find-v-kv line "line")) ;; this is broken
           (end (find-v-kv line "end")) ;; this is broken
           )

      `(:file ,file-path :func ,func-name :start ,start :end ,end))))


(defun get-release-from-tagfile (tagfile)
  "Release file is in the same dir as tagfile, grab contents of that"
  (let* (    (tagfile-base (directory-namestring tagfile))
             (file-path (merge-pathnames (pathname "release") tagfile-base))
             )

    (str:from-file file-path)))

;; this is much, much faster.
;;



(defun find-in-tags-file (tagfile func-name)
  "Find file in the tagfile this way because tagfiles can be huge."
  (let* (
         (all-output
           (uiop:run-program (list "readtags" "-t" (namestring tagfile)  "-p" "-ne" "-" func-name) :output :string))
         (output-list (str:split #\Newline all-output :omit-nulls t))
         )

    `(:release ,(get-release-from-tagfile tagfile) :match-list
               ,(loop for line in output-list
                      collect (line-to-structure line tagfile)
                      ))
    ))

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
