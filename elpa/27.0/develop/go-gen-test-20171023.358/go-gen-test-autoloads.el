;;; go-gen-test-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-gen-test" "go-gen-test.el" (0 0 0 0))
;;; Generated autoloads from go-gen-test.el

(autoload 'go-gen-test-dwim "go-gen-test" "\
(go-gen-test-dwim &optional START END)
Generate tests for functions you want to.
If you call this function while region is active it extracts
functions defined between START and END and generate tests for it.
Else it generates tests for exported or all functions.
You can customize this behavior with `go-gen-test-default-functions'.

\(fn &optional START END)" t nil)

(autoload 'go-gen-test-all "go-gen-test" "\
(go-gen-test-all)
Generate tests for all functions." t nil)

(autoload 'go-gen-test-exported "go-gen-test" "\
(go-gen-test-exported)
Generate tests for all exported functions." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-gen-test" '("go-gen-test-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-gen-test-autoloads.el ends here
