(setq
 auto-completion-enable-sort-by-usage t
 auto-completion-enable-help-tooltip t
 auto-completion-private-snippets-directory (expand-file-name (concat iimacs-dir "snippets"))
 auto-completion-enable-snippets-in-popup t
 dotspacemacs-enable-server t
 dotspacemacs-persistent-server t
 dotspacemacs-line-numbers '(; :visual t
                             ;; :relative nil
                             :disabled-for-modes dired-mode
                             ;; doc-view-mode
                             ;; markdown-mode
                             ;; org-mode
                             ;; pdf-view-mode
                             ;; text-mode
                             :size-limit-kb 5000)
 org-babel-python-command "python3"
 org-confirm-babel-evaluate nil
 org-enable-github-support t
 org-enable-bootstrap-support t
 org-enable-reveal-js-support t
 org-enable-sticky-header t
 org-enable-epub-support t
 spaceline-org-clock-p t
 osc52e-package-dir (concat (configuration-layer/get-layer-local-dir 'ii) "osc52e")
 python-shell-interpreter "python3"
 ii-tmate-configured nil
 helm-mode-handle-completion-in-region nil
 )

;; setting 'safe-local-variable properties
(put 'ii 'safe-local-variable (lambda (_) t))
(put 'org-babel-tmate-session-prefix 'safe-local-variable #'stringp)
(put 'github-username 'safe-local-variable #'stringp)
(put 'github-user 'safe-local-variable #'stringp)
(put 'org-babel-tmate-default-window-name 'safe-local-variable #'stringp)
(put 'org-confirm-babel-evaluate 'safe-local-variable #'booleanp)
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (_) t))
(put 'org-use-property-inheritance 'safe-local-variable (lambda (_) t))
(put 'org-src-preserve-indentation 'safe-local-variable (lambda (_) t))
(put 'org-file-dir 'safe-local-variable (lambda (_) t))
(put 'eval 'safe-local-variable (lambda (_) t))

;; ensure go binaries are available
(setenv "PATH" (concat user-home-directory "go/bin:" (getenv "PATH")))

;; before-local-var-hacks
(add-hook 'before-hack-local-variables-hook 'ii/before-local-var-hacks)

;; ensure current line is highlighted everywhere
(global-visual-line-mode 1)

;; older versions of org require this for s> templating to work correctly
(when (version<= "9.2" (org-version)) (require 'org-tempo))
