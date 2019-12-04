(setq
 org-enable-github-support t
 org-enable-bootstrap-support t
 org-enable-reveal-js-support t
 auto-completion-enable-sort-by-usage t
 auto-completion-enable-help-tooltip t
 auto-completion-private-snippets-directory (expand-file-name (concat iimacs-dir "snippets"))
 auto-completion-enable-snippets-in-popup t
 osc52e-package-dir (concat (configuration-layer/get-layer-local-dir 'ii) "osc52e")
 python-shell-interpreter "python3"
 org-babel-python-command "python3"
 )
