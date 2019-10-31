;;; js-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-doc" "js-doc.el" (0 0 0 0))
;;; Generated autoloads from js-doc.el

(autoload 'js-doc-insert-file-doc "js-doc" "\
Insert specified-style comment top of the file" t nil)

(autoload 'js-doc-insert-function-doc "js-doc" "\
Insert JsDoc style comment of the function
The comment style can be custimized via `customize-group js-doc'" t nil)

(autoload 'js-doc-insert-function-doc-snippet "js-doc" "\
Insert JsDoc style comment of the function with yasnippet." t nil)

(autoload 'js-doc-insert-tag "js-doc" "\
Insert a JsDoc tag interactively." t nil)

(autoload 'js-doc-describe-tag "js-doc" "\
Describe the JsDoc tag" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-doc" '("js-doc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-doc-autoloads.el ends here
