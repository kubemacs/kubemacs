;; This is the folder containing the spacemacs repository
(setq spacemacs-start-directory (expand-file-name "~/.emacs.d/spacemacs/"))
;; This is your spacemacs config file
(setq dotspacemacs-filepath (expand-file-name "~/.emacs.d/.spacemacs"))
;; This folder will be where other config / cache related items reside
;; (setenv "SPACEMACSDIR" "~/.emacs.d/")
;; Loading spacemacs
(load-file (concat spacemacs-start-directory "init.el"))
