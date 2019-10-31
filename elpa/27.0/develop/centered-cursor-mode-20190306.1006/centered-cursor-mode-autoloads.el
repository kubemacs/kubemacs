;;; centered-cursor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "centered-cursor-mode" "centered-cursor-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centered-cursor-mode.el

(autoload 'ccm-visible-text-lines "centered-cursor-mode" "\
Visible text lines" nil nil)

(autoload 'centered-cursor-mode "centered-cursor-mode" "\
Makes the cursor stay vertically in a defined
position (usually centered).

If called interactively, enable Centered-Cursor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centered-cursor-mode" '("animate-first-start-p" "ccm-" "global-centered-cursor-mode" "recenter-sequence")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; centered-cursor-mode-autoloads.el ends here
