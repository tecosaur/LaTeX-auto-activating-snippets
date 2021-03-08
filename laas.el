;;; laas.el --- A bundle of as-you-type LaTeX snippets -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Yoav Marco, TEC
;;
;; Authors: Yoav Marco <https://github.com/yoavm448> TEC <https://github.com/tecosaur>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: September 22, 2020
;; Modified: February 13, 2021
;; Version: 0.2
;; Keywords: tools, tex
;; Homepage: https://github.com/tecosaur/LaTeX-auto-activating-snippets
;; Package-Requires: ((emacs "26.3") (auctex "11.88") (aas "0.2") (yasnippet "0.14"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Make use of the auto-activating-snippets engine to provide an expansive
;; collection of LaTeX snippets. Primaraly covering: operators, symbols,
;; accents, subscripts, and a few fraction forms.
;;
;;; Code:

(require 'aas)
(require 'texmathp)
(require 'yasnippet)

(defgroup laas nil
  "Snippet expansions mid-typing."
  :prefix "laas-"
  :group 'aas)

(defun laas-current-snippet-insert-post-space-if-wanted ()
  "Insert a space at point, if it seems warranted."
  (when (and (stringp aas-transient-snippet-expansion)
             (= ?\\ (aref aas-transient-snippet-expansion 0))
             (not (memq (char-after) '(?\) ?\]))))
    (insert " ")))

(defun laas-insert-script (s)
  "Add a subscript with a text of S (string).

Rely on `aas-transient-snippet-condition-result' to contain the
result of `aas-auto-script-condition' which gives the info
whether to extend an existing subscript (e.g a_1 -> a_{1n}) or
insert a new subscript (e.g a -> a_1)."
  (interactive (list (this-command-keys)))
  (pcase aas-transient-snippet-condition-result
    ;; new subscript after a letter
    ('one-sub
     (insert "_" s))
    ;; continuing a digit sub/superscript
    ('extended-sub
     (backward-char)
     (insert "{")
     (forward-char)
     (insert s "}"))))

(defun laas-mathp ()
  "Determine whether point is within a LaTeX maths block."
  (pcase major-mode
    ('latex-mode (texmathp))
    ('org-mode (laas-org-mathp))
    (_ (message "LaTeX auto-activated snippets does not currently support %s" major-mode)
       nil)))

(declare-function org-element-at-point "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-context "org-element")

(defun laas-org-mathp ()
  "Determine whether the point is within a LaTeX fragment or environment."
  (let ((element (org-element-at-point)))
    (or (eq (org-element-type element) 'latex-environment)
        (eq (org-element-type (org-element-context element)) 'latex-fragment))))

(defun laas-auto-script-condition ()
  "Condition used for auto-sub/superscript snippets."
  (cond ((or (bobp) (= (1- (point)) (point-min)))
         nil)
        ((and (or (= (char-before (1- (point))) ?_)
                  (= (char-before (1- (point))) ?^))
              (/= (char-before) ?{))
         'extended-sub)
        ((and
          ;; Before is some indexable char
          (or (<= ?a (char-before) ?z)
              (<= ?A (char-before) ?Z))
          ;; Before that is not
          (not (or (<= ?a (char-before (1- (point))) ?z)
                   (<= ?A (char-before (1- (point))) ?Z)))
          ;; Inside math
          (laas-mathp))
         'one-sub)))

(defun laas-identify-adjacent-tex-object (&optional point)
  "Return the starting position of the left-adjacent TeX object from POINT."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((memq (char-before) '(?\) ?\]))
      (backward-sexp)
      (point))
     ((= (char-before) ?})
      (cl-loop do (backward-sexp)
               while (= (char-before) ?}))
      ;; try to catch the marco if the braces belong to one
      (when (looking-back "\\\\[A-Za-z@*]+" (line-beginning-position))
        (goto-char (match-beginning 0)))
      (when (memq (char-before) '(?_ ?^ ?.))
        (backward-char)
        (goto-char (laas-identify-adjacent-tex-object))) ; yay recursion
      (point))
     ((or (<= ?a (char-before) ?z)
          (<= ?A (char-before) ?Z)
          (<= ?0 (char-before) ?9))
      (backward-word)
      (when (= (char-before) ?\\) (backward-char))
      (when (memq (char-before) '(?_ ?^ ?.))
        (backward-char)
        (goto-char (laas-identify-adjacent-tex-object))) ; yay recursion
      (point)))))

(defun laas-wrap-previous-object (tex-command)
  "Wrap previous TeX object in TEX-COMMAND."
  (interactive)
  (let ((start (laas-identify-adjacent-tex-object)))
    (insert "}")
    (when (aref aas-transient-snippet-key (1- (length aas-transient-snippet-key)))
      (insert " "))
    (save-excursion
      (goto-char start)
      (insert (concat "\\" tex-command "{")))))

(defun laas-object-on-left-condition ()
  "Return t if there is a TeX object imidiately to the left."
  ;; TODO use `laas-identify-adjacent-tex-object'
  (and (or (<= ?a (char-before) ?z)
           (<= ?A (char-before) ?Z)
           (<= ?0 (char-before) ?9)
           (memq (char-before) '(?\) ?\] ?})))
       (laas-mathp)))

;; HACK Smartparens runs after us on the global `post-self-insert-hook' and
;;      thinks that since a { was inserted after a self-insert event, it
;;      should insert the matching } - even though we took care of that.
;;      laas--shut-up-smartparens removes
(defun laas--restore-smartparens-hook ()
  "Restore `sp--post-self-insert-hook-handler' to `post-self-insert-hook'.

Remove ourselves, `laas--restore-smartparens-hook', as well, so
it is restored only once."
  (remove-hook 'post-self-insert-hook #'laas--restore-smartparens-hook)
  (add-hook 'post-self-insert-hook #'sp--post-self-insert-hook-handler))

(declare-function sp--post-self-insert-hook-handler "smartparens")
(defun laas--shut-up-smartparens ()
  "Remove Smartparens' hook temporarily from `post-self-insert-hook'."
  (when (memq #'sp--post-self-insert-hook-handler
              (default-value 'post-self-insert-hook))
    (remove-hook 'post-self-insert-hook #'sp--post-self-insert-hook-handler)
    ;; push rather than add-hook so it doesn't run right after this very own
    ;; hook, but next time
    (unless (memq #'laas--restore-smartparens-hook
                  (default-value 'post-self-insert-hook))
      (push #'laas--restore-smartparens-hook (default-value 'post-self-insert-hook)))))

(defun laas-smart-fraction ()
  "Expansion function used for auto-subscript snippets."
  (interactive)
  (let* ((tex-obj (laas-identify-adjacent-tex-object))
         (start (save-excursion
                  ;; if bracketed, delete outermost brackets
                  (if (memq (char-before) '(?\) ?\]))
                      (progn
                        (backward-delete-char 1)
                        (goto-char tex-obj)
                        (delete-char 1))
                    (goto-char tex-obj))
                  (point)))
         (end (point))
         (content (buffer-substring-no-properties start end)))
    (yas-expand-snippet (format "\\frac{%s}{$1}$0" content)
                        start end))
  (laas--shut-up-smartparens))

(defvar laas-basic-snippets
  '(:cond laas-mathp
    "!="    "\\neq"
    "!>"    "\\mapsto"
    "**"    "\\cdot"
    "+-"    "\\pm"
    "-+"    "\\mp"
    "->"    "\\to"
    "..."   "\\dots"
    "<<"    "\\ll"
    "<="    "\\leq"
    "<>"    "\\diamond"
    "=<"    "\\impliedby"
    "=="    "&="
    "=>"    "\\implies"
    ">="    "\\geq"
    ">>"    "\\gg"
    "AA"    "\\forall"
    "EE"    "\\exists"
    "cb"    "^3"
    "iff"   "\\iff"
    "inn"   "\\in"
    "notin" "\\not\\in"
    "sr"    "^2"
    "xx"    "\\times"
    "|->"   "\\mapsto"
    "||"    "\\mid"
    "~="    "\\approx"
    "~~"    "\\sim"
    ;; "...\\)a" 	"...\\) a"
    ;; "//" 	"\\frac{}{}"
    ;; "a" "+b 	a + b"
    ;; "a" "^ 	a^"
    ;; "a+" 	"a +"
    ;; "a^11" 	"a^{11}"
    ;; "case" 	"cases env."
    ;; "part" 	"\\frac{\\partial }{\\partial }"
    ;; "pmat" 	"pmatrix"
    ;; "set" 	"\\{ \\}"
    ;; "sq" 	"\\sqrt{}"
    ;; "st" 	"\\text{s.t.}"
    ;;"\\\\\\"\\" 	"\\setminus"

    "arccos" "\\arccos"
    "arccot" "\\arccot"
    "arccot" "\\arccot"
    "arccsc" "\\arccsc"
    "arcsec" "\\arcsec"
    "arcsin" "\\arcsin"
    "arctan" "\\arctan"
    "cos"    "\\cos"
    "cot"    "\\cot"
    "csc"    "\\csc"
    "exp"    "\\exp"
    "ln"     "\\ln"
    "log"    "\\log"
    "perp"   "\\perp"
    "sin"    "\\sin"
    "star"   "\\star"
    "gcd"    "\\gcd"
    "min"    "\\min"
    "max"    "\\max"

    "CC" "\\CC"
    "FF" "\\FF"
    "HH" "\\HH"
    "NN" "\\NN"
    "PP" "\\PP"
    "QQ" "\\QQ"
    "RR" "\\RR"
    "ZZ" "\\ZZ"

    ";a"  "\\alpha"
    ";A"  "\\forall"        ";;A" "\\aleph"
    ";b"  "\\beta"
    ";;;c" "\\cos"
    ";;;C" "\\arccos"
    ";d"  "\\delta"         ";;d" "\\partial"
    ";D"  "\\Delta"         ";;D" "\\nabla"
    ";e"  "\\epsilon"       ";;e" "\\varepsilon"   ";;;e" "\\exp"
    ";E"  "\\exists"                               ";;;E" "\\ln"
    ";f"  "\\phi"           ";;f" "\\varphi"
    ";F"  "\\Phi"
    ";g"  "\\gamma"                                ";;;g" "\\lg"
    ";G"  "\\Gamma"                                ";;;G" "10^{?}"
    ";h"  "\\eta"           ";;h" "\\hbar"
    ";i"  "\\in"            ";;i" "\\imath"
    ";I"  "\\iota"          ";;I" "\\Im"
    ";;j" "\\jmath"
    ";k"  "\\kappa"
    ";l"  "\\lambda"        ";;l" "\\ell"          ";;;l" "\\log"
    ";L"  "\\Lambda"
    ";m"  "\\mu"
    ";n"  "\\nu"                                   ";;;n" "\\ln"
    ";N"  "\\nabla"                                ";;;N" "\\exp"
    ";o"  "\\omega"
    ";O"  "\\Omega"         ";;O" "\\mho"
    ";p"  "\\pi"            ";;p" "\\varpi"
    ";P"  "\\Pi"
    ";q"  "\\theta"         ";;q" "\\vartheta"
    ";Q"  "\\Theta"
    ";r"  "\\rho"           ";;r" "\\varrho"
    ";;R" "\\Re"
    ";s"  "\\sigma"         ";;s" "\\varsigma"    ";;;s" "\\sin"
    ";S"  "\\Sigma"                               ";;;S" "\\arcsin"
    ";t"  "\\tau"                                 ";;;t" "\\tan"
    ";;;T" "\\arctan"
    ";u"  "\\upsilon"
    ";U"  "\\Upsilon"
    ";v"  "\\vee"
    ";V"  "\\Phi"
    ";w"  "\\xi"
    ";W"  "\\Xi"
    ";x"  "\\chi"
    ";y"  "\\psi"
    ";Y"  "\\Psi"
    ";z"  "\\zeta"
    ";0"  "\\emptyset"
    ";8"  "\\infty"
    ";!"  "\\neg"
    ";^"  "\\uparrow"
    ";&"  "\\wedge"
    ";~"  "\\approx"        ";;~" "\\simeq"
    ";_"  "\\downarrow"
    ";+"  "\\cup"
    ";-"  "\\leftrightarrow"";;-" "\\longleftrightarrow"
    ";*"  "\\times"
    ";/"  "\\not"
    ";|"  "\\mapsto"        ";;|" "\\longmapsto"
    ";\\" "\\setminus"
    ";="  "\\Leftrightarrow"";;=" "\\Longleftrightarrow"
    ";(" "\\langle"
    ";)" "\\rangle"
    ";[" "\\Leftarrow"     ";;[" "\\Longleftarrow"
    ";]" "\\Rightarrow"    ";;]" "\\Longrightarrow"
    ";{"  "\\subset"
    ";}"  "\\supset"
    ";<"  "\\leftarrow"    ";;<" "\\longleftarrow"  ";;;<" "\\min"
    ";>"  "\\rightarrow"   ";;>" "\\longrightarrow" ";;;>" "\\max"
    ";'"  "\\prime"
    ";."  "\\cdot")
  "Basic snippets. Expand only inside maths.")

(defvar laas-subscript-snippets
  `(:cond laas-auto-script-condition
    ,@(cl-loop for (key exp) in '(("ii"  laas-insert-script)
                                  ("ip1" "_{i+1}")
                                  ("im1" "_{i-1}")
                                  ("jj"  laas-insert-script)
                                  ("jp1" "_{j+1}")
                                  ("jm1" "_{j-1}")
                                  ("nn"  laas-insert-script)
                                  ("np1" "_{n+1}")
                                  ("nm1" "_{n-1}")
                                  ("kk"  laas-insert-script)
                                  ("kp1" "_{k+1}")
                                  ("km1" "_{k-1}")
                                  ("0"   laas-insert-script)
                                  ("1"   laas-insert-script)
                                  ("2"   laas-insert-script)
                                  ("3"   laas-insert-script)
                                  ("4"   laas-insert-script)
                                  ("5"   laas-insert-script)
                                  ("6"   laas-insert-script)
                                  ("7"   laas-insert-script)
                                  ("8"   laas-insert-script)
                                  ("9"   laas-insert-script))
               if (symbolp exp)
               collect :expansion-desc
               and collect (format "X_%s, or X_{Y%s} if a subscript was typed already"
                               (substring key -1) (substring key -1))
               collect key collect exp))
  "Automatic subscripts! Expand In math and after a single letter.")

(defvar laas-frac-snippet
  '(:cond laas-object-on-left-condition
    :expansion-desc "Wrap object on the left with \\frac{}{}, leave `point' in the denuminator."
    "/" laas-smart-fraction)
  "Frac snippet. Expand in maths when there's something to frac on on the left.")


(defvar laas-accent-snippets
  `(:cond laas-object-on-left-condition
    ,@(cl-loop for (key exp) in '((". " "dot")
                                  (".. " "dot")
                                  (",." "vec")
                                  (".," "vec")
                                  ("~ " "tilde")
                                  ("hat" "hat")
                                  ("bar" "overline"))
               collect :expansion-desc
               collect (format "Wrap in \\%s{}" exp)
               collect key
               ;; re-bind exp so its not changed in the next iteration
               collect (let ((expp exp)) (lambda () (interactive)
                                           (laas-wrap-previous-object expp)))))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defun laas--no-backslash-before-point? ()
  "Check that the char before `point' is not a backslash."
  (/= (char-before) ?\\))


(apply #'aas-set-snippets 'laas-mode laas-basic-snippets)
(apply #'aas-set-snippets 'laas-mode laas-subscript-snippets)
(apply #'aas-set-snippets 'laas-mode laas-frac-snippet)
(apply #'aas-set-snippets 'laas-mode laas-accent-snippets)

(defcustom laas-enable-auto-space t
  "If non-nil, hook intelligent space insertion onto snippet expansion."
  :type 'boolean
  :group 'laas)

;;;###autoload
(define-minor-mode laas-mode
  "Minor mode for enabling a ton of auto-activating LaTeX snippets."
  :init-value nil
  (if laas-mode
      (progn
        (aas-mode +1)
        (aas-activate-keymap 'laas-mode)
        (add-hook 'aas-global-condition-hook
                  #'laas--no-backslash-before-point?
                  nil 'local)
        (when laas-enable-auto-space
          (add-hook 'aas-post-snippet-expand-hook
                    #'laas-current-snippet-insert-post-space-if-wanted
                    nil 'local)))
    (aas-deactivate-keymap 'laas-mode)
    (remove-hook 'aas-global-condition-hook #'laas--no-backslash-before-point?
                 'local)
    (remove-hook 'aas-post-snippet-expand-hook
                 #'laas-current-snippet-insert-post-space-if-wanted
                 'local)))

(provide 'laas)
;;; laas.el ends here
