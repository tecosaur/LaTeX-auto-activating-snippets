;;; laas.el --- A bundle of as-you-type LaTeX snippets -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Yoav Marco, TEC
;;
;; Authors: Yoav Marco <https://github.com/yoavm448> TEC <https://github.com/tecosaur>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: September 22, 2020
;; Modified: April 17, 2021
;; Version: 1.0
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
  (cond
   ((derived-mode-p 'latex-mode) (texmathp))
   ((derived-mode-p 'org-mode) (laas-org-mathp))
   (t (message "LaTeX-auto-activated snippets does not currently support math in any of %s"
               (aas--modes-to-activate major-mode))
      nil)))

(declare-function org-element-at-point "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-context "org-element")

(defun laas-org-mathp ()
  "Determine whether the point is within a LaTeX fragment or environment."
  (or (org-inside-LaTeX-fragment-p)
      (eq (org-element-type (org-element-at-point)) 'latex-environment)))

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
      (when (eq (char-before) ?\\) (backward-char))
      (when (memq (char-before) '(?_ ?^ ?.))
        (backward-char)
        (goto-char (laas-identify-adjacent-tex-object))) ; yay recursion
      (point)))))

(defun laas-wrap-previous-object (tex-cmd)
  "Wrap previous TeX object in TEX-COMMAND.
TEX-cmd can be a string like \"textbf\", a cons like
(\"{\\textstyle\" . \"}\"), or a function that would be called
and is expected to return a string or cons."
  (interactive)
  (let ((start (laas-identify-adjacent-tex-object))
        left right)
    (when (functionp tex-cmd)
      (setq tex-cmd (funcall tex-cmd)))
    ;; parse tex-cmd to left/right wrappings
    (cond
     ((stringp tex-cmd)
      (setq left (concat "\\" tex-cmd "{")
            right "}"))
     ((consp tex-cmd)
      (setq left (car tex-cmd)
            right (cdr tex-cmd)))
     (t
      (message "Wrong type of `tex-cmd' given to `laas-wrap-previous-object'.")))
    ;; wrap
    (when left
      (insert right)
      (save-excursion
        (goto-char start)
        (insert left)))))

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

(defun laas-frac-cond ()
  (cond ((and (= (char-before) ?/) (laas-mathp)) 'standalone-frac)
        ((laas-object-on-left-condition) 'wrapping-frac)))

(defun laas-smart-fraction ()
  "Expansion function used for auto-subscript snippets."
  (interactive)
  (pcase aas-transient-snippet-condition-result
    ('standalone-frac
     (delete-char -1) ;; delete the previous / that isn't part of the key
     (yas-expand-snippet "\\frac{$1}{$2}$0"))
    ('wrapping-frac
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
                           start end))))
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
    "|="    "\\models"
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
    "eqv"    "\\equiv" ; == is taken and =- too common

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
    ";+"  "\\cup"           ";;+" "\\oplus"
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
  '(:cond laas-frac-cond
    :expansion-desc "See the docs of `laas-frac-snippet'"
    "/" laas-smart-fraction)
  "Frac snippet.
Expand to a template frac after //, or wrap the object before point if it isn't /.

ab/ => \\frac{ab}{}
// => \\frac{}{}")

(defun laas-latex-accent-cond ()
  "Return non-nil if also non-math latex accents can be expanded"
  (or (derived-mode-p 'latex-mode)
      (laas-mathp)))

(defun laas-accent--rm () (interactive)   (laas-wrap-previous-object (if (laas-mathp) "mathrm" "textrm")))
(defun laas-accent--it () (interactive)   (laas-wrap-previous-object (if (laas-mathp) "mathit" "textit")))
(defun laas-accent--bf () (interactive)   (laas-wrap-previous-object (if (laas-mathp) "mathbf" "textbf")))
(defun laas-accent--emph () (interactive) (laas-wrap-previous-object (if (laas-mathp) "mathem" "emph")))
(defun laas-accent--tt () (interactive)   (laas-wrap-previous-object (if (laas-mathp) "mathtt" "texttt")))
(defun laas-accent--sf () (interactive)   (laas-wrap-previous-object (if (laas-mathp) "mathsf" "textsf")))
(defvar laas-accent-snippets
  `(;; work in both normal latex text and math
    :cond laas-latex-accent-cond
    :expansion-desc "Wrap in \\mathrm{} or \\textrm{}"     "'r" laas-accent--rm
    :expansion-desc "Wrap in \\mathit{} or \\textit{}"     "'i" laas-accent--it
    :expansion-desc "Wrap in \\mathbf{} or \\textbf{}"     "'b" laas-accent--bf
    :expansion-desc "Wrap in \\mathemph{} or \\textemph{}" "'e" laas-accent--emph
    :expansion-desc "Wrap in \\mathtt{} or \\texttt{}"     "'y" laas-accent--tt
    :expansion-desc "Wrap in \\mathsf{} or \\textsf{}"     "'f" laas-accent--sf
    ;; only normal latex text, no math
    :cond (lambda () (and (derived-mode-p 'latex-mode) (not (laas-mathp))))
    :expansion-desc "Wrap in \\textsl"
    "'l" (lambda () (interactive) (laas-wrap-previous-object "textsl"))
    ;; only math
    :cond laas-object-on-left-condition
    ,@(cl-loop for (key . exp)
               in '(("'." . "dot")
                    ("':" . "ddot")
                    ("'~" . "tilde")
                    ("'N" . "widetilde")
                    ("'^" . "hat")
                    ("'H" . "widehat")
                    ("'-" . "bar")
                    ("'T" . "overline")
                    ("'_" . "underline")
                    ("'{" . "overbrace")
                    ("'}" . "underbrace")
                    ("'>" . "vec")
                    ("'/" . "grave")
                    ("'\"". "acute")
                    ("'v" . "check")
                    ("'u" . "breve")
                    ("'m" . "mbox")
                    ("'c" . "mathcal")
                    ("'0" . ("{\\textstyle " . "}"))
                    ("'1" . ("{\\displaystyle " . "}"))
                    ("'2" . ("{\\scriptstyle " . "}"))
                    ("'3" . ("{\\scriptscriptstyle " . "}"))
                    ;; now going outside cdlatex
                    ("'q" . "sqrt")
                    ;; "influenced" by Gilles Castel
                    (".. " . ("\\dot{" . "} "))
                    (",." . "vec")
                    (".," . "vec")
                    ("~ " . ("\\tilde{" . "} "))
                    ("hat" . "hat")
                    ("bar" . "overline"))
               collect :expansion-desc
               collect (concat "Wrap in "
                               (if (consp exp)
                                   (concat (car exp) (cdr exp))
                                 (format "\\%s{}" exp)))
               collect key
               ;; re-bind exp so its not changed in the next iteration
               collect (let ((expp exp)) (lambda () (interactive)
                                           (laas-wrap-previous-object expp)))))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defun laas--no-backslash-before-point? ()
  "Check that the char preceding the snippet key is not backslash."
  ;; conditions are already run right at the start of the snippet key, no need
  ;; to move point
  (not (eq (char-before) ?\\)))


(apply #'aas-set-snippets 'laas-mode laas-basic-snippets)
(apply #'aas-set-snippets 'laas-mode laas-subscript-snippets)
(apply #'aas-set-snippets 'laas-mode laas-frac-snippet)
(apply #'aas-set-snippets 'laas-mode laas-accent-snippets)

(defcustom laas-enable-auto-space t
  "If non-nil, hook intelligent space insertion onto snippet expansion."
  :type 'boolean
  :group 'laas)

(defcustom laas-use-unicode nil
  "If non-nil, output Unicode symbols instead of LaTeX macros via `laas-unicode'."
  :type 'boolean
  :group 'laas)

(declare-function laas-unicode-rewrite "laas-unicode")
;;;###autoload
(define-minor-mode laas-mode
  "Minor mode for enabling a ton of auto-activating LaTeX snippets."
  :init-value nil
  :group 'laas
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
                    nil 'local))
        (when laas-use-unicode
          (unless (boundp 'laas-unicode-rewrite)
            (require 'laas-unicode))
          (add-hook 'aas-pre-snippet-expand-hook
                    #'laas-unicode-rewrite nil 'local)))
    (aas-deactivate-keymap 'laas-mode)
    (remove-hook 'aas-global-condition-hook #'laas--no-backslash-before-point?
                 'local)
    (remove-hook 'aas-post-snippet-expand-hook
                 #'laas-current-snippet-insert-post-space-if-wanted
                 'local)
    (remove-hook 'aas-pre-snippet-expand-hook
                 #'laas-unicode-rewrite
                 'local)))

(provide 'laas)
;;; laas.el ends here
