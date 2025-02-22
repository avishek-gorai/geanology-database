;;; package.lisp

;;; Copyright (c) 2025 Avishek Gorai <avishekgorai@myyahoo.com>


(defpackage geanology-database
  (:use common-lisp)
  (:export *family*
           father
           mother
           parents
           children
           siblings)
  (:documentation "Contains a geanological database and functions to extract all sorts of information from it."))
