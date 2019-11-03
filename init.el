;; This folder should be ~/.emacs.d OR within your EMACS_LOAD_PATH
;; Set iimacs-dir to the folder containing this file
(setq iimacs-dir (file-name-directory user-init-file))
;; (setenv "SPACEMACSDIR" iimacs-dir)

;; your .spacemacs will reside here
(setq dotspacemacs-filepath (concat iimacs-dir ".spacemacs"))

;; This is some minor advice to add our layers folder and ii layer itself
;; by running these two fnuctions just before the configurationd and loading of layers

;; Calling this functions ensures the layers folder containing ii layer is in the path
(defun ii/add-spacemacs-layers-dir (&optional refresh-index)
  ; TODO Append to Path rather than overwrite it
  (setq dotspacemacs-configuration-layer-path (list
						(concat iimacs-dir "layers/")
						))
  )
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
;; fix for strange load state bug when using dumper
;; (define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)

