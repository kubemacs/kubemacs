;;; py-isort-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "py-isort" "py-isort.el" (0 0 0 0))
;;; Generated autoloads from py-isort.el

(autoload 'py-isort-region "py-isort" "\
Uses the \"isort\" tool to reformat the current region." t nil)

(autoload 'py-isort-buffer "py-isort" "\
Uses the \"isort\" tool to reformat the current buffer." t nil)

(autoload 'py-isort-before-save "py-isort" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "py-isort" '("py-isort-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; py-isort-autoloads.el ends here
