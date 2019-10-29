;; This is the folder containing the spacemacs repository as a submodule
(setq spacemacs-start-directory (expand-file-name "~/.emacs.d/spacemacs/"))

;; This is your spacemacs config file moved to within this repo
;; Instead of referencing ~/.emacs.d/spacemacs/private
;; Add the following so that folder is managed by your personal config
;; by adding the following to dotspacemacs/init
;; configuration-layer-private-layer-directory (expand-file-name "~/.emacs.d/layers")
(setq dotspacemacs-filepath (expand-file-name "~/.emacs.d/.spacemacs"))

;; This folder will be where other config / cache related items reside
;; (setenv "SPACEMACSDIR" "~/.emacs.d/")
;; Loading spacemacs
;;(setq debug-on-signal t)
;;(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; (defconst emacs-start-time (current-time))
;;(setq dotspacemacs-enable-server t)
;; https://stackoverflow.com/questions/19806176/in-emacs-how-do-i-make-a-local-variable-safe-to-be-set-in-a-file-for-all-possibl
(load-file (concat spacemacs-start-directory "init.el"))
;; fix for strange load state bug when using dumper
;; (define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)

