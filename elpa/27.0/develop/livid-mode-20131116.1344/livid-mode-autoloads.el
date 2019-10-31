;;; livid-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "livid-mode" "livid-mode.el" (0 0 0 0))
;;; Generated autoloads from livid-mode.el

(autoload 'livid-mode "livid-mode" "\
Minor mode for automatic evaluation of a JavaScript buffer on every change

If called interactively, enable Livid mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "livid-mode" '("livid-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; livid-mode-autoloads.el ends here
