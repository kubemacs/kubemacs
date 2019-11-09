(setq org-confirm-babel-evaluate nil)
(put 'org-babel-tmate-session-prefix 'safe-local-variable #'stringp)
(put 'github-username 'safe-local-variable #'stringp)
(put 'github-user 'safe-local-variable #'stringp)
(put 'org-babel-tmate-default-window-name 'safe-local-variable #'stringp)
(put 'org-confirm-babel-evaluate 'safe-local-variable #'booleanp)
;; (put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (_) t))
(put 'org-use-property-inheritance 'safe-local-variable (lambda (_) t))
(put 'org-file-dir 'safe-local-variable (lambda (_) t))
(put 'eval 'safe-local-variable (lambda (_) t))
(setenv "PATH" (concat user-home-directory "go/bin:" (getenv "PATH")))
(setq dotspacemacs-enable-server t)
(setq dotspacemacs-persistent-server t)
(defun runs-and-exits-zero (program &rest args)
  "Run PROGRAM with ARGS and return the exit code."
  (with-temp-buffer
    (if (= 0 (apply 'call-process program nil (current-buffer) nil args))
        'true
      ))
  )

(defun xclip-working ()
  "Quick Check to see if X is working."
  (if (getenv "DISPLAY")
      ;; this xset test is a bit flakey
      ;; (if (runs-and-exits-zero "xset" "q")
      ;; Using xclip to set an invalid selection is as lightly intrusive
      ;; check I could come up with, and not overwriting anything
      ;; however it seems to hang
      ;; (if (runs-and-exits-zero "xclip" "-selection" "unused")
      ;;     'true)
      'true
    ;; )
    )
  )

(defun create-target-script (filename command)
  "Create a temporary script to create/connect to target tmate window"
  (message "Creating a script file in tmp")
  (with-current-buffer (find-file-noselect filename)
    (erase-buffer)
    (insert-for-yank
     (concat "\n#!/bin/sh\n\n" command))
    (save-buffer)
    (set-file-modes filename #o755)
    )
  )

(defun populate-terminal-clipboard ()
  "Populate the osc52 clipboard via terminal with the start-tmate-sh"
  ;; TODO
  (message "Unable to set X Clipboard to contain the start-tmate-sh")
  (create-target-script tmate-sh start-tmate-command)
  ;; (gui-select-text tmate-sh)
  (setq current-tmate-sh tmate-sh) ;; since tmate-sh is buffer-local..
  ;;(setq current-tmate-ssh (concat "export IISOCK=" socket " ; rm -f $IISOCK ; ssh -tAX " ssh-user-host " -L $IISOCK:$IISOCK " tmate-sh))
  (setq current-tmate-ssh (concat "ssh -tAX " ssh-user-host " " tmate-sh))
  (with-current-buffer (get-buffer-create "start-tmate-sh" )
    (insert-for-yank "You will need to copy this manually:\n\n" )
    (insert-for-yank
     (concat "\nTo open on another host, forward your iisocket by pasting:\n\n" current-tmate-ssh
             "\n\nOR open another terminal on the same host and paste:\n\n" current-tmate-sh)
     ))
  )

(defun populate-x-clipboard ()
  "Populate the X clipboard with the start-tmate-sh"
  (message "Setting X Clipboard to contain the start-tmate-sh")
  (xclip-mode 1)
  ;; (gui-select-text (concat "rm -fi " socket "; ssh -tAX " ssh-user "@" ssh-host " -L " socket ":" socket " " start-tmate-over-ssh-command))
  (create-target-script tmate-sh start-tmate-command)
  ;; (gui-select-text tmate-sh)
  ;; If we work to detect if it's a remote host, this might make sense
  ;; but for now we mostly connect to the pair box
  ;; (setq current-tmate-ssh (concat "export II=" socket " ; rm -f $II ; ssh -tAX " ssh-user-host " -L $II:$II " tmate-sh))
  (setq current-tmate-sh tmate-sh) ;; since tmate-sh is buffer-local..
  (setq current-tmate-ssh (concat "ssh -tAX " ssh-user-host " " tmate-sh))
  (gui-select-text current-tmate-ssh)
                                        ; (gui-select-text start-tmate-command)
  (xclip-mode 0)
  (with-current-buffer (get-buffer-create "start-tmate-sh")
    (insert-for-yank "The following has been populated to your local X clipboard:\n")
    (insert-for-yank
     ;; we can use the global current-tmate-sh
     (concat "\nTo open on another host, forward your iisocket by pasting:\n\n" current-tmate-ssh
             "\n\nOR open another terminal on the same host and paste:\n\n" current-tmate-sh)
     ))
  ;; and unset it when done
  (setq current-tmate-ssh nil)
  (setq current-tmate-sh nil)
  )
 (defun ssh-find-agent ()
  (interactive)
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string (concat "\
          . " (configuration-layer/get-layer-local-dir 'ii) "ssh-find-agent.sh ;\
          ssh-find-agent | grep ssh- | tail -1 \
          | tail -1 | awk '{print $2}' | awk -F= '{print $2}' \
          | tr --delete '\n'"))
  (message (getenv "SSH_AUTH_SOCK"))
  ))
(with-eval-after-load "org"
  ;; (add-to-list 'org-src-lang-modes '("go-mode" . sql))
  (add-to-list 'org-structure-template-alist
		           `("g" . "src go")))
;(add-to-list 'org-structure-template-alist
;		         `("ii"
;		           . "src ii-mode"))

;; https://www.wisdomandwonder.com/article/10630/how-fast-can-you-tangle-in-org-mode
;; (setq help/default-gc-cons-threshold gc-cons-threshold)
;; (defun help/set-gc-cons-threshold (&optional multiplier notify)
;;   "Set `gc-cons-threshold' either to its default value or a
;;    `multiplier' thereof."
;;   (let* ((new-multiplier (or multiplier 1))
;;          (new-threshold (* help/default-gc-cons-threshold
;;                            new-multiplier)))
;;     (setq gc-cons-threshold new-threshold)
;;     (when notify (message "Setting `gc-cons-threshold' to %s"
;;                           new-threshold))))

;; (defun help/double-gc-cons-threshold () "Double `gc-cons-threshold'." (help/set-gc-cons-threshold 2))
;; (add-hook 'org-babel-pre-tangle-hook #'help/double-gc-cons-threshold)
;; (add-hook 'org-babel-post-tangle-hook #'help/set-gc-cons-threshold)

;; (setq-default
;;  time-stamp-zone "Pacific/Auckland"
;;  ;; https://www.emacswiki.org/emacs/TimeStamp
;;  time-stamp-pattern "10/#+UPDATED: needs time-local formatted regexp"
;;  )

(when (version<= "9.2" (org-version)) (require 'org-tempo))
