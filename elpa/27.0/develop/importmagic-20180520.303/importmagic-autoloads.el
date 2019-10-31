;;; importmagic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "importmagic" "importmagic.el" (0 0 0 0))
;;; Generated autoloads from importmagic.el

(autoload 'importmagic-mode "importmagic" "\
A mode that lets you autoimport unresolved Python symbols.

If called interactively, enable Importmagic mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "importmagic" '("importmagic-")))

;;;***

;;;### (autoloads nil nil ("importmagic-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; importmagic-autoloads.el ends here
