;;; functions.el -*- lexical-binding: t; -*-

(defun choose-random-banner (files)
  (make-banner-path (random-choice files)))

(defun make-banner-path (file)
  (substitute-in-file-name
   (concat "$HOME/.config/doom/banners/" file)))

(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))
