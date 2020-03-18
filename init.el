;;(setq package-check-signature nil)
;; This folder should be ~/.emacs.d OR within your EMACS_LOAD_PATH
;; Set iimacs-dir to the folder containing this file
(setq iimacs-dir (file-name-directory load-file-name))
;; (setenv "SPACEMACSDIR" iimacs-dir)
(message "iimacs: LOADING")
;; your .spacemacs will reside here
;(setq dotspacemacs-filepath (concat iimacs-dir ".spacemacs-" user-login-name)) ;; per user?
(setq dotspacemacs-filepath (concat "~/.iimacs")) ;; per user?
;; we used several emacs server/daemons and they try to 'steal' the recentf from each other
(setq recentf-save-file (concat user-emacs-directory ".cache/recentf." server-name )) ;; per server
;;(setq (concat "~/.emacs.d/.spacemacs-" user-login-name)) ;; per user?
;; This is some minor advice to add our layers folder and ii layer itself
;; by running these two fnuctions just before the configurationd and loading of layers

;; Calling this functions ensures the layers folder containing ii layer is in the path
(defun ii/add-spacemacs-layers-dir (&optional refresh-index)
  ; TODO Append to Path rather than overwrite it
  (setq dotspacemacs-configuration-layer-path
	(list(concat iimacs-dir "layers/"))
	))
(advice-add 'configuration-layer/discover-layers :before #'ii/add-spacemacs-layers-dir)

;; Advice choose-banner, as it's Nice to have control of our own banners
(defun ii/reset-spacemacs-banner-directory ()
  (setq spacemacs-banner-directory (expand-file-name (concat iimacs-dir "banners/")))
  (setq spacemacs-banner-official-png (expand-file-name (concat spacemacs-banner-directory "img/spacemacs.png")))
  (setq spacemacs-badge-official-png (expand-file-name (concat spacemacs-banner-directory "img/spacemacs-badge.png")))
  (setq spacemacs-purple-heart-png (expand-file-name (concat spacemacs-banner-directory "img/heart.png"))
	 ))
(advice-add 'spacemacs-buffer//choose-banner :before #'ii/reset-spacemacs-banner-directory)

                                        ;(defadvice package-initialize (before spacemacs-buffer//get-banner-path)
; (message "UPDATING BANNERS")
; (setq spacemacs-banner-directory (expand-file-name (concat iimacs-dir "banners/"))
;       ))
  ;; (expand-file-name (concat spacemacs-banner-directory "img/spacemacs-badge.png"))
  ;; "Purple heart emoji.")
  ;; (expand-file-name (concat spacemacs-banner-directory "img/heart.png"))
;; It needs to be called before the configuration-layer/discover-layers funcion is loaded
(advice-add 'configuration-layer/discover-layers :before #'ii/add-spacemacs-layers-dir)

;; Calling this function ensures the ii layer is appended to the list
(defun ii/add-spacemacs-layers ()
  ;; TODO only add if it doesn't already exist
  (setq-default dotspacemacs-configuration-layers (append dotspacemacs-configuration-layers '(ii)))
  )
;; It needs to be called before the configuration-layer/discover-layers funcion is loaded
(advice-add 'dotspacemacs/layers :after #'ii/add-spacemacs-layers)

;; This ensures local variable customizations are not saved to .emacs.d/.spacemacs
;; but instead separately to ~/.emacs.d/customizations
(setq custom-file (concat iimacs-dir "customizations"))

;; This is the folder containing the spacemacs repository as a submodule
;; From here we are just locating spacemacs source and running it's normal init.el
(setq spacemacs-start-directory (concat iimacs-dir "spacemacs/"))
(load-file (concat spacemacs-start-directory "init.el"))
;; TODO fix for strange load state bug when using dumper
;; (define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
(message "iimacs: LOADED")

;; Would be good to try and detect if we are in OSX and within an app bundle
;; OR get tmate to compile to a static binary 
;; OR have the emacs bundle set this before emacs loads since it is specific to the OSX app bundle
;; TODO figure out how to ensure subprocesses have DYLD_LIBRARY_PATH set so tmate and other
;; embedded into the EMacs.app binaries work
;; (if (string= system-type "darwin")
;;     (progn
;;       (getenv "DYLD_LIBRARY_PATH")
;;       (setenv "DYLD_LIBRARY_PATH" (expand-file-name (concat iimacs-dir "../../lib/")))
;;       )
;;     )
;; https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
;(evil-leader/set-key "q q" 'spacemacs/frame-killer)
