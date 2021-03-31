;;; functions.el -*- lexical-binding: t; -*-

(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))
