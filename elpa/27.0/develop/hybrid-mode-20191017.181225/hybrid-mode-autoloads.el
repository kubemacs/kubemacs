;;; hybrid-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hybrid-mode" "hybrid-mode.el" (0 0 0 0))
;;; Generated autoloads from hybrid-mode.el

(defvar hybrid-style-default-state (spacemacs|dotspacemacs-backward-compatibility hybrid-mode-default-state normal) "\
Value of `evil-default-state' for hybrid-mode.")

(custom-autoload 'hybrid-style-default-state "hybrid-mode" t)

(defvar hybrid-style-enable-hjkl-bindings (spacemacs|dotspacemacs-backward-compatibility hybrid-mode-enable-hjkl-bindings nil) "\
If non-nil then packages configuration should enable hjkl navigation.")

(custom-autoload 'hybrid-style-enable-hjkl-bindings "hybrid-mode" t)

(defvar hybrid-style-enable-evilified-state (spacemacs|dotspacemacs-backward-compatibility hybrid-mode-enable-evilified-state t) "\
If non-nil then evilified states is enabled in buffer supporting it.")

(custom-autoload 'hybrid-style-enable-evilified-state "hybrid-mode" t)

(defvar hybrid-style-use-evil-search-module (spacemacs|dotspacemacs-backward-compatibility hybrid-mode-use-evil-search-module nil) "\
If non-nil then use evil own search module which is closer to Vim search
behavior (for instance it support C-r pasting).")

(custom-autoload 'hybrid-style-use-evil-search-module "hybrid-mode" t)

(defvar hybrid-mode nil "\
Non-nil if Hybrid mode is enabled.
See the `hybrid-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hybrid-mode'.")

(custom-autoload 'hybrid-mode "hybrid-mode" nil)

(autoload 'hybrid-mode "hybrid-mode" "\
Global minor mode to replace insert state by hybrid state.

If called interactively, enable Hybrid mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hybrid-mode" '("disable-hybrid-editing-style" "enable-hybrid-editing-style" "evil-insert-state-p" "hybrid-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hybrid-mode-autoloads.el ends here
