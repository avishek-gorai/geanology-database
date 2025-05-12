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
  "Returns father's name."
  (second (assoc person *family*)))


(defun mother (person)
  "Returns mother's name."
  (third (assoc person *family*)))


(defun parents (person)
  "Returns a set of parents."
  (remove-if (function null)
             (list (father person) (mother person))))


(defun children (person)
  "Returns a set of children."
  (labels ((children-recursive (person family-tree)
             (let ((first-entry (first family-tree)))
               (cond ((null family-tree) nil)
                     ((member person (rest first-entry))
                      (cons (first first-entry)
                            (children-recursive person (rest family-tree))))
                     (t (children-recursive person (rest family-tree)))))))
    (children-recursive person *family*)))


(defun siblings (person)
  "Returns a set of siblings, including genetic half-siblings."
  (labels ((siblings-recursive (person family-tree)
             (let ((first-entry (first family-tree))
                   (mother (mother person))
                   (father (father person)))
               (if (not (and (null mother) (null father)))
                   (cond ((null family-tree) nil)
                         ((or (equal mother (third first-entry))
                              (equal father (second first-entry)))
                          (cons (first first-entry)
                                (siblings-recursive person (rest family-tree))))
                         (t (siblings-recursive person (rest family-tree))))))))
    (remove-if (function (lambda (element)
                 (equal element person)))
               (siblings-recursive person *family*))))


(defun mapunion (function list)
  (reduce (function union) (mapcar function list)))

(defun grandparents (person)
  "Returns a set of grandparents."
  (mapunion (function parents) (parents person)))

(defun cousin (person)
  "Returns a set of biological first cousins."
  (if (not (null (parents person)))
      (mapunion (function children)
                (mapunion (function siblings) (parents person)))))

(defun descended-from (person-1 person-2)
  "Returns T if person-1 is descended from person-2."
  (and (not (null person-1)) (not (null person-2))
       (or (member person-2 (parents person-1))
           (descended-from (mother person-1) person-2)
           (descended-from (father person-1) person-2))))

(defun ancestors (person)
  "Returns the set of ancestors."
  (if (not (null person))
      (append (parents person)
              (ancestors (mother person))
              (ancestors (father person)))))
