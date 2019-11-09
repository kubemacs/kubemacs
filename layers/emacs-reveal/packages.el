;;; packages.el --- emacs-reveal layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Hippie Hacker <hh@nom>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `emacs-reveal-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `emacs-reveal/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `emacs-reveal/pre-init-PACKAGE' and/or
;;   `emacs-reveal/post-init-PACKAGE' to customize the package as it is loaded.

(defun emacs-reveal/init-emacs-reveal ()
  (use-package emacs-reveal)
  )
(defun emacs-reveal/init-org-re-reveal-ref ()
  (use-package org-re-reveal-ref)
  )
(defun emacs-reveal/init-oer-reveal ()
  (use-package oer-reveal)
  )

;;; Code:

(defconst emacs-reveal-packages
  '(
    (emacs-reveal
     :location "~/.emacs.d/layers/emacs-reveal/local/emacs-reveal")
    org-re-reveal-ref
    oer-reveal
    )
  "The list of Lisp packages required by the emacs-reveal layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here
