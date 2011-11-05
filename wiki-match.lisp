(declaim (optimize (safety 3) (debug 3) (speed 0)))
(defvar *load* (mapc #'ql:quickload '("split-sequence" "cl-ppcre" "alexandria")))
;; (cl:in-package :cl)
(defpackage :wiki-match
  (:use :common-lisp :split-sequence :cl-ppcre :alexandria))
(in-package :wiki-match)

(defclass article ()
  ((content
    :initarg :content
    :accessor content)
   (id
    :initarg :id
    :accessor id)
   (words
    :initarg :words
    :accessor words)
   (tfidf
    :initform (make-hash-table :test #'equalp)
    :accessor tfidf)))
(defclass articles ()
  ((amount
    :initform 0
    :accessor amount-of)
   (datas
    :initform (make-hash-table :test #'equalp)
    :accessor datas)
   (words
    :initform (make-hash-table :test #'equalp)
    :accessor words-of)
   (idf
    :initform (make-hash-table :test #'equalp)
    :accessor idf-of)))

(defvar *wikipedias* (make-instance 'articles))
(defvar *pages* (make-instance 'articles))
(defvar *advertisements* (make-instance 'articles))
(defun database-clear ()
  (setf *wikipedias* (make-instance 'articles))
  (setf *pages* (make-instance 'articles))
  (setf *advertisements* (make-instance 'articles)))

(defun hash-incf (key hash)
  (if (eq nil (gethash key hash))
      (setf (gethash key hash) 1)
      (incf (gethash key hash))))

(defun hash-decf (key hash)
  (if (eq 1 (gethash key hash))
      (remhash key hash)
      (decf (gethash key hash))))

(defun statistics-words (list)
  (let ((h (make-hash-table :test #'equalp)))
    (loop for i in list
       do (hash-incf i h))
    h))

(defgeneric calculate (type))
(defmethod calculate ((type (eql 'wikipedia))) (calculate *wikipedias*))
(defmethod calculate ((type (eql 'page))) (calculate *pages*))
(defmethod calculate ((type (eql 'advertisement))) (calculate *advertisements*))
(defmethod calculate ((type articles))
  (loop for k being the hash-key  in (words-of type) using (hash-value v)
     do (setf (gethash k (idf-of type)) (log (/ (amount-of type) v))))
  (loop for v being the hash-value of (datas type)
       do (loop for word being the hash-key of (words v) using (hash-value times)
	     do (setf (gethash word (tfidf v)) (* times (gethash word (idf-of type)))))))
(defmethod calculate ((type (eql 'all)))
  (loop for i in '(wikipedia page advertisement)
       do (calculate i)))

(defgeneric new (type id content &optional words))
(defmethod new ((type (eql 'wikipedia)) id content &optional words)(new *wikipedias* id content words))
(defmethod new ((type (eql 'page)) id content &optional words)(new *pages* id content words))
(defmethod new ((type (eql 'advertisement)) id content &optional words)(new *advertisements* id content words))
(defmethod new (type id content &optional words)
  (let ((words-list (or words (statistics-words (stem-words (extract-words (remove-token content)))))))
    (when (gethash id (datas type))
      (loop with h = (words-of type) for i being the hash-key of h
	 do (hash-decf i h))
      (decf (amount-of type)))
    (setf (gethash id (datas type)) (make-instance 'article :content content :id id :words words-list))
    (loop with h = (words-of type) for i being the hash-key of words-list
       do (hash-incf i h))
    (incf (amount-of type))))

(defun remove-token (string)
  (regex-replace-all "\\[[^]]*\\]|\\.(?![a-zA-Z0-9])|[\"()]" string ""))
(defun extract-words(string)
  "Extract words from string.The words is separated by spaces"
  (split-sequence #\space string :remove-empty-subseqs t))

(defun stem-words(list)
  (let ((string
	 (with-output-to-string (string)
	   (loop for i in list
	      do (write-string i string)
	      do (write-char #\Newline string)))))
    #+ :ccl
    (split-sequence #\Newline
		    (with-output-to-string (out)
		      (ccl:run-program "~/.bin/stemwords" nil :input (make-string-input-stream string) :output out))
		    :remove-empty-subseqs t)))

(defun read-file-to-string (pathname)
  (with-output-to-string (string)
    (with-open-file (in pathname)
      (loop for i = (read-line in nil nil)
	 while i
	 do (write-string i string)
	 do (write-char #\Space string)  ))
    string))