;;; clean-aindent-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clean-aindent-mode" "clean-aindent-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from clean-aindent-mode.el

(defvar clean-aindent-mode nil "\
Non-nil if Clean-Aindent mode is enabled.
See the `clean-aindent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `clean-aindent-mode'.")

(custom-autoload 'clean-aindent-mode "clean-aindent-mode" nil)

(autoload 'clean-aindent-mode "clean-aindent-mode" "\
Activates clean auto indent for function 'newline-and-indent' and
back-space unindent for M-DEL (meta-backspace).

If called interactively, enable Clean-Aindent mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

clean-aindent mode is a global minor mode.

1. Extension of 'newline-and-indent' that keeps track of the last
auto-indent operation and, if it is abandoned, would take care to
trim down the unused white space characters.

2. Simple indent, if activated, where cursor is aligned with
indent of the lines above.

3. Backspace Unindent. Extension of M-backspace. When cursor is
in the indentation space of a line, or at the first character and
you press M-backspace it will move the entire line to be aligned
to the line above or any other that is with indentation smaller
than the current.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clean-aindent-mode" '("clean-aindent-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clean-aindent-mode-autoloads.el ends here
