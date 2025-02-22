;;; geanology-database.lisp -- Information extraction form geanological database.
;;; Copyright (C) 2024, 2025 by Avishek Gorai <avishekgorai@myyahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package geanology-database)

(defvar *family*
  (quote ((colin nil nil)
          (deirdre nil nil)
          (arthur nil nil)
          (kate nil nil)
          (frank nil nil)
          (linda nil nil)
          (suzanne colin deirdre)
          (bruce arthur kate)
          (charles arthur kate)
          (david arthur kate)
          (ellen arthur kate)
          (george frank linda)
          (hillary frank linda)
          (andre nil nil)
          (tamara bruce suzanne)
          (vincent bruce suzanne)
          (wanda nil nil)
          (ivan george ellen)
          (julie george ellen)
          (marie george ellen)
          (nigel andre hillary)
          (frederick nil tamara)
          (zelda vincent wanda)
          (joshua ivan wanda)
          (quentin nil nil)
          (robert quentin julie)
          (olivia nigel marie)
          (peter nigel marie)
          (erica nil nil)
          (yvette robert zelda)
          (diane peter erica)))
  "The geanological database.")


(defun father (person)
  "Returns the given person's father name."
  (second (assoc person family)))


(defun mother (person)
  "Returns given person's mother name."
  (third (assoc person family)))


(defun parents (person)
  "Returns given person's parents."
  (remove-if (function null)
             (rest (assoc person family))))


(defun children (person)
  "Returns the given person's children name."
  (if (null person) nil
      (mapcar (function first)
              (remove-if-not
               (function (lambda (entry) (member person (rest entry))))
               family))))


(defun siblings (person)
  "Returns a list of the person's siblings, including genetic half siblings."
  (let* ((person-parents (rest (assoc person family))))
    (mapcar (function first) (remove-if-not (function (lambda (entry)
                     (equal (rest entry) person-parents))) family))))
