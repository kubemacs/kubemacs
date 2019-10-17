;; This is the folder containing the spacemacs repository
(setq spacemacs-start-directory (expand-file-name "~/.emacs.d/spacemacs/"))
;; This is your spacemacs config file
(setq dotspacemacs-filepath (expand-file-name "~/.emacs.d/.spacemacs"))
;; This folder will be where other config / cache related items reside
;; (setenv "SPACEMACSDIR" "~/.emacs.d/")
;; Loading spacemacs
(setq debug-on-signal t)
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(defconst emacs-start-time (current-time))
(setq org-confirm-babel-evaluate nil)
(setq dotspacemacs-enable-server t)
;; https://stackoverflow.com/questions/19806176/in-emacs-how-do-i-make-a-local-variable-safe-to-be-set-in-a-file-for-all-possibl
(put 'org-babel-tmate-session-prefix 'safe-local-variable #'stringp)
(put 'github-username 'safe-local-variable #'stringp)
(put 'github-user 'safe-local-variable #'stringp)
(put 'org-babel-tmate-default-window-name 'safe-local-variable #'stringp)
(put 'org-confirm-babel-evaluate 'safe-local-variable #'booleanp)
;; (put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (_) t))
(put 'org-use-property-inheritance 'safe-local-variable (lambda (_) t))
(put 'org-file-dir 'safe-local-variable (lambda (_) t))
(put 'eval 'safe-local-variable (lambda (_) t))
(load-file (concat spacemacs-start-directory "init.el"))
