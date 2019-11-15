;;; packages.el --- org-capture layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Hippie Hacker <hh@Surface-Book>
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
;; added to `org-capture-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-capture/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-capture/pre-init-PACKAGE' and/or
;;   `org-capture/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defun org-capture/init-org-protocol ()
  (use-package org-protocol)
  )
(defun org-capture/init-org-protocol-capture-html ()
  (use-package org-protocol-capture-html)
  )
(defun org-capture/init-org-capture-pop-frame ()
  (use-package org-capture-pop-frame)
  ;;(require 'org-capture)
  )

(defconst org-capture-packages
  '(
    (org-capture-pop-frame)
    (org-protocol
     :ensure t
     :location built-in)
    (org-protocol-capture-html
     :ensure t
     :location (recipe
                :fetcher github
                :repo "alphapapa/org-protocol-capture-html"
                :commit "23a1336")
     :config (progn
               (message "org-protocol-capture-html LOADED")
               )
     ))
  "The list of Lisp packages required by the org-capture layer.

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


  (setq-default org-capture-templates
        '(
;; oc-H
;; javascript:location.href = 'org-protocol://capture-html?template=w&url=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title || "[untitled page]") + '&body=' + encodeURIComponent(function () {var html = ""; if (typeof document.getSelection != "undefined") {var sel = document.getSelection(); if (sel.rangeCount) {var container = document.createElement("div"); for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());} html = container.innerHTML;}} else if (typeof document.selection != "undefined") {if (document.selection.type == "Text") {html = document.selection.createRange().htmlText;}} var relToAbs = function (href) {var a = document.createElement("a"); a.href = href; var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash; a.remove(); return abs;}; var elementTypes = [['a', 'href'], ['img', 'src']]; var div = document.createElement('div'); div.innerHTML = html; elementTypes.map(function(elementType) {var elements = div.getElementsByTagName(elementType[0]); for (var i = 0; i < elements.length; i++) {elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));}}); return div.innerHTML;}());
          ("w" "Default template" entry
           (file+headline "~/org/capture.org" "Notes")
           "* %^{Title}

  Source: %u, %c

  %i" :empty-lines 1)
          ("t" "TODO"
           entry (file+headline "~/org/TODO.org" "Inbox")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
           )
          ;; oc-p (paragraph)
          ;; javascript:location.href='org-protocol://capture?template=p&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())
          ("p" "Paragraph" entry
           (file+headline "~/ii/org/agenda/notes.org" "Inbox")
           "* [[%:link][%:description]] %^G
Time: %u
 #+BEGIN_QUOTE
%:initial
#+END_QUOTE


%?")
          ;; oc-L
          ;; javascript:location.href='org-protocol://capture?template=L&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'?body='+encodeURIComponent(window.getSelection())
          ("L" "Capture a Link" entry
           (file+headline "~/ii/org/agenda/notes.org" "Inbox")
           "* %? [[%:link][%:description]]
Captured On: %U")
          ;; o-sl (org-store-link)... insert later with ,il
          ;; javascript:location.href='org-protocol://store-link?url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title);
          )
        )

;;; packages.el ends here
