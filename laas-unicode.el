;;; laas-unicode.el --- Laas LaTeX macro snippet output to Unicode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Liam Hupfer
;;
;; Author: Liam Hupfer <liam@hpfr.net>
;; Maintainer: Liam Hupfer <liam@hpfr.net>
;; Created: February 18, 2022
;; Modified: February 18, 2022
;; Keywords: convenience extensions tex
;; Homepage: https://github.com/tecosaur/LaTeX-auto-activating-snippets
;; Package-Requires: ((emacs "25.1") (math-symbol-lists "1.3") (laas "1.0") (aas "1.1")
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `laas-unicode' employs the `math-symbol-lists' package to provide `laas'
;; users the option to replace LaTeX macros with their Unicode equivalents.
;;
;;; Code:

(require 'math-symbol-lists)
(require 'laas)

(defconst laas-unicode-list
  (let (result)
    (dolist (el (append math-symbol-list-subscripts
                        math-symbol-list-superscripts
                        math-symbol-list-basic
                        math-symbol-list-extended
                        ;; a few extra LaTeX aliases and \not\in
                        '(("mathrel" "\\not\\in" 8713 "∉")
                          ("mathrel" "\\impliedby" 10232 "⟸")
                          ("mathrel" "\\implies" 10233 "⟹")
                          ("mathrel" "\\iff" 10234 "⟺")))
                result)
      (when-let ((tex (nth 1 el))
                 (char (nth 2 el)))
        ;; use the first found symbol, since many greek letters have a mathematical italic version
        (unless (assoc tex result)
          (push (cons tex (char-to-string char)) result)))))

  "List of mappings between LaTeX macros and Unicode characters as strings.

Compiled from `math-symbol-lists'.")

(defun laas-unicode-rewrite ()
  "Convert LaTeX macro snippet expansion to Unicode if a mapping exists"
  (when-let ((sym (alist-get aas-transient-snippet-expansion laas-unicode-list nil nil #'equal)))
    (setq aas-transient-snippet-expansion sym)))

(provide 'laas-unicode)
;;; laas-unicode.el ends here
