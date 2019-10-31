;;; godoctor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "godoctor" "godoctor.el" (0 0 0 0))
;;; Generated autoloads from godoctor.el

(autoload 'godoctor-rename "godoctor" "\


\(fn &optional DRY-RUN)" t nil)

(autoload 'godoctor-rename-dry-run "godoctor" nil t nil)

(autoload 'godoctor-extract "godoctor" "\


\(fn &optional DRY-RUN)" t nil)

(autoload 'godoctor-extract-dry-run "godoctor" nil t nil)

(autoload 'godoctor-toggle "godoctor" "\


\(fn &optional DRY-RUN)" t nil)

(autoload 'godoctor-toggle-dry-run "godoctor" nil t nil)

(autoload 'godoctor-godoc "godoctor" "\


\(fn &optional DRY-RUN)" t nil)

(autoload 'godoctor-godoc-dry-run "godoctor" nil t nil)

(autoload 'godoctor-set-scope "godoctor" "\
Set the scope for the godoctor, prompting the user to edit the previous scope.

The scope restricts analysis to the specified packages.
Its value is a comma-separated list of patterns of these forms:
	golang.org/x/tools/cmd/guru     # a single package
	golang.org/x/tools/...          # all packages beneath dir
	...                             # the entire workspace.

A pattern preceded by '-' is negative, so the scope
	encoding/...,-encoding/xml
matches all encoding packages except encoding/xml." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "godoctor" '("godoctor-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; godoctor-autoloads.el ends here
