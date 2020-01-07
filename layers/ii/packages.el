;;; packages.el --- ii layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Zach Mandeville <zz@sharing.io>
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
;; added to `ii-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ii/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ii/pre-init-PACKAGE' and/or
;;   `ii/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defun ii/init-ob-shell ()
  (use-package ob-shell)
  ;;(require 'ob-shell)
  )
(defun ii/init-feature-mode ()
  (use-package feature-mode))
(defun ii/init-ob-tmate ()
  (use-package ob-tmate))
(defun ii/init-ob-javascript ()
  (use-package ob-javascript))
(defun ii/init-ob-sql-mode ()
  (use-package ob-sql-mode))
(defun ii/init-ox-gfm ()
  (use-package ox-gfm))
(defun ii/init-s ()
  (use-package ob-sql-mode))
(defun ii/init-xclip ()
  (use-package xclip))
(defun ii/init-org-checklist ()
  (use-package org-checklist))
(defun ii/init-ob-go ()
  (use-package ob-go))
(defun ii/init-ob-async ()
  (use-package ob-async))
(defun ii/init-kubernetes ()
  (use-package kubernetes))
(defun ii/init-kubernetes-evil ()
  (use-package kubernetes-evil))

(defun ii/init-osc52e ()
  )
(defun ii/post-init-osc52e ()
  (require 'osc52e)
  (osc52-set-cut-function)
  )
(setq ob-async-pre-execute-src-block-hook nil)
(defun ii/post-init-ob-async ()
  (message "ii/post-init-ob-async YAHOOOOOOOO!!")
  ;; lsp go-mode-hook should go into post-init-go-mode...
  ;; but for some reason ^^ isn't called
  ;; FIXME
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
               (require 'org)
               (require 'ob-shell)
               (require 'ob-javascript)
               ;;(require 'ob-sql-mode)
               (add-to-list 'org-babel-load-languages '(shell . t))
               (add-to-list 'org-babel-load-languages '(javascript . t))
               ;;(add-to-list 'org-babel-load-languages '(sql-mode . t))
               )))
;; (defun ii/pre-init-lsp-mode ()
;;   (spacemacs|use-package-add-hook lsp-mode
;;     :post-config (progn
;;                    )))
(defun ii/post-init-go-mode ()
  (message "ii/post-init-go-mode YAHOOOOOOOO!!")
  )
(defun ii/post-init-lsp-mode ()
  (message "ii/post-init-lsp-mode YAHOOOOOOOO!!")
   (setq
   go-backend #'lsp
   lsp-navigation 'both
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-alignment 'frame
                                        ; lsp-ui-doc-border 'white
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit t
   lsp-ui-doc-delay 0.2
   lsp-ui-doc-include-signature nil
   lsp-ui-sideline-show-symbol t
   lsp-ui-remap-xref-keybindings t
   lsp-ui-sideline-enable t
   lsp-prefer-flymake nil
   lsp-print-io t
   )
  )
(defun ii/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (progn
                   (add-to-list 'org-babel-load-languages
                                '(go . t))
                   ;;(add-to-list 'org-babel-load-languages
                   ;;             '(sh . t))
                   (add-to-list 'org-babel-load-languages
                                '(javascript . t))
                   (add-to-list 'org-babel-load-languages
                                '(shell . t))
                   (add-to-list 'org-babel-load-languages
                                '(sql-mode . t))
                   (add-to-list 'org-babel-load-languages
                                '(emacs-lisp . t))
                   )))
(defun ii/post-init-org ()
  (require 'ob-shell)
  (require 'ob-javascript)
  (require 'cal-iso)
  )
(defun ii/post-init-yasnippet ()
  ;; TODO do this within let for local var
  (spacemacs|use-package-add-hook yasnippet
    :post-config (progn
                   (add-to-list 'yas-snippet-dirs (expand-file-name
                                                   "snippets"
                                                   (configuration-layer/get-layer-local-dir
                                                    'ii))
                                t)
                   (yas-load-directory (expand-file-name
                                        "snippets"
                                        (configuration-layer/get-layer-local-dir
                                         'ii)))
  )))

(defconst ii-packages
  `(
    ;; async
    ;; closql
    ;; command-log-mode
    ;; dash
    ;; demo-it
    ;; ein ;; https://github.com/millejoh/emacs-ipython-notebook
    ;; emms
    ;; emacsql-sqlite
    ;; evil-vimish-fold
    ;; fancy-narrow
    feature-mode
    ;;(forge :location "/usr/local/share/emacs/site-lisp/forge"
    ;;       :afer magit)
    ;; ghub
    ;;go-playground
    ;;go-dlv
    ;;gorepl-mode ;; go
    ;;graphql
    ;;graphql-mode
    ;; (graphql-mode :location (recipe
    ;;                          :fetcher github
    ;;                          :repo "davazp/graphql-mode"
    ;;                          :commit "301a218"
    ;;                          ))
    ;; groovy-mode
    ;; jupyter
    ;; ob-async
    ;; (ob-javascript
    ;;           :location (recipe
    ;;                      :fetcher github
    ;;                      :repo "zweifisch/ob-javascript"))
    (ob-javascript
     :location ,(concat (configuration-layer/get-layer-local-dir 'ii) "ob-javascript")
     )
    (kubernetes
      :ensure t
      :commands (kubernetes-overview))

    ;; If you want to pull in the Evil compatibility package.
    (kubernetes-evil
      :ensure t
      :after kubernetes)
    (ob-tmate :ensure t
              :location (recipe
                         :fetcher github
                         :repo "ii/ob-tmate"))
    (ob-go :ensure t
           :location (recipe
                      :fetcher github
                      :repo "pope/ob-go"))
    (ob-sql-mode :ensure t)
    (ob-shell :ensure t
              :location built-in)
    ;; FIXME: likely a way to ask for this layer dir directly
    (osc52e :ensure t
             :location ,(concat (configuration-layer/get-layer-local-dir 'ii) "osc52e")
    ;;osc52e-package-dir
       )
            ;;:location ,(expand-file-name (concat (car dotspacemacs-configuration-layer-path) "ii/osc52e")))
            ;; :location (recipe
            ;;            :fetcher git
            ;;            :url "https://gist.github.com/49eabc1978fe3d6dedb3ca5674a16ece.git"
            ;;            ))
    ;;(org-checklist :ensure t)
    (org-checklist :ensure t
                   :location built-in)
    (ob-async :ensure t
              :location (recipe
                         :fetcher github
                         :repo "astahlman/ob-async"))
    ;; This should go as a layer dependency for org via

    ;; (org :variables
    ;;      org-enable-github-support t
    ;;      org-enable-bootstrap-support t
    ;;      org-enable-reveal-js-support t
    ;;      )
    ;;  (ox-gfm :ensure t)
    ;; oer-reveal
    ;; (org-protocol-capture-html :location (recipe
    ;;                                       :fetcher github
    ;;                                       :repo "alphapapa/org-protocol-capture-html"
    ;;                                       :commit "23a1336c"))
    ;; org-re-reveal-ref
    ;; (emacs-reveal :location (recipe
    ;;                          :fetcher gitlab
    ;;                          :repo "oer/emacs-reveal"
    ;;                          :commit "d0aa1f9d"))
    ;;ob-go
    ;; org-protocol ;; https://orgmode.org/worg/org-contrib/org-protocol.html
    ;; http://tech.memoryimprintstudio.com/org-capture-from-external-applications/
    ;; https://github.com/sprig/org-capture-extension
    ;;ob-tmux
    ;; org-babel-eval-in-repl
    ;; org-tree-slide
    ;; org-mu4e
    ;; org-pdfview
    ;; ox-reveal
    ;; pdf-tools ;; https://github.com/politza/pdf-tools
    ;; pdf-view
    s
    ;; scad-mode
    ;; slime
    ;; transcribe
    ;; togetherly
    ;; vimish-fold
    xclip
    ;; (yasnippet :location (recipe
    ;;                       :fetcher github
    ;;                       :repo "joaotavora/yasnippet"
    ;;                       :branch "0.13.0"))
    ;; :commit "89eb7ab"))
    ;;                      :branch "0.12.2"))
    ;; for tmate and over ssh cut-and-paste
    ;; https://gist.github.com/49eabc1978fe3d6dedb3ca5674a16ece.git
    ;; sakura is waiting on vte
    ;; https://bugs.launchpad.net/sakura/+bug/1769575
    ;; I'm pretty sure the lib vte issue is stale
    ;; https://bugzilla.gnome.org/show_bug.cgi?id=795774
    ;; available in minitty since 2.6.1
    ;; https://github.com/mintty/mintty/issues/258
    ;; http://mintty.github.io/ (Default tty on Cygwin etc)
    ;; I created a ticket to add support to vte
    ;; https://gitlab.gnome.org/GNOME/vte/issues/125
    ;; this would in turn enable support on many
    ;; default linux/gnome terminals
    ;; for now, you probably want to use xterm
    ;;(osc52e :location (recipe
    ;;                   :fetcher git
    ;;                   :url "https://gist.github.com/49eabc1978fe3d6dedb3ca5674a16ece.git"
    ;;                   :ensure t
    ;;                   ))
    ;; for jupyter
    ;; websocket
    ;; simple-httpd
    ;; emacs-websocket
    ;; company-mode
    ;; markdown-mode
    ;; (zmq :ensure t)
    yasnippet
    )
  "The list of Lisp packages required by the ii layer.

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
