;;; lsp-treemacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-metals-treeview" "lsp-metals-treeview.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-metals-treeview.el

(autoload 'lsp-metals-treeview-enable "lsp-metals-treeview" "\
Enable Metals treeview extension - send capability
to Metals to indicate we want treeview messages and wire up notification
handlers.

\(fn ENABLE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-metals-treeview" '("lsp-metals-tree")))

;;;***

;;;### (autoloads nil "lsp-treemacs" "lsp-treemacs.el" (0 0 0 0))
;;; Generated autoloads from lsp-treemacs.el

(autoload 'lsp-treemacs-errors-list "lsp-treemacs" "\
Display error list." t nil)

(autoload 'lsp-treemacs-symbols "lsp-treemacs" "\
Show symbols view." t nil)

(autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "\
Display error list." t nil)

(autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs" '("lsp-treemacs-")))

;;;***

;;;### (autoloads nil nil ("lsp-treemacs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-treemacs-autoloads.el ends here
