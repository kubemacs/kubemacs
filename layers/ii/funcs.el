;; Stephen's weekly time tracker
(defun iso-week-to-time (year week day)
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week day year)))))
    (encode-time 0 0 0 d m y)))

(defun ii-timesheet ()
  "Create a timesheet buffer and insert skel"
  (interactive)
  (require 'cal-iso)
  (switch-to-buffer (get-buffer-create "*ii-timesheet*"))
  (ii-timesheet-skel)
  )

(define-skeleton ii-timesheet-skel
  "Prompt the week and year before generating ii timesheet for the user."
  ""
  (text-mode)

  ;; > "#+TITLE: Timesheet: Week " (setq v1 (skeleton-read "Timesheet Week? "))
  > "#+TITLE: Timesheet: Week " (setq v1 (skeleton-read "Timesheet Week? "))
  ;; ", " (setq v2 (skeleton-read "Timesheet Year? "))
  ", " (setq v2 "2020")
  " (" (getenv "USER") ")" \n
  > "#+AUTHOR: " (getenv "USER") \n
  > " " \n
  > "Please refer to the instructions in ii-timesheet.org as required." \n
  > " " \n
  > "* Week Summary" \n
  > " " _ \n
  > "#+BEGIN: clocktable :scope file :block thisweek :maxlevel 2 :emphasise t :tags t :formula %" \n
  > "#+END" \n
  > " " \n

  > "* " (format-time-string "%B %e, %Y" (iso-week-to-time (string-to-number v2) (string-to-number v1) 1)) \n
  > "** Task X" \n
  > "* " (format-time-string "%B %e, %Y" (iso-week-to-time (string-to-number v2) (string-to-number v1) 2)) \n
  > "** Task X" \n
  > "* " (format-time-string "%B %e, %Y" (iso-week-to-time (string-to-number v2) (string-to-number v1) 3)) \n
  > "** Task X" \n
  > "* " (format-time-string "%B %e, %Y" (iso-week-to-time (string-to-number v2) (string-to-number v1) 4)) \n
  > "** Task X" \n
  > "* " (format-time-string "%B %e, %Y" (iso-week-to-time (string-to-number v2) (string-to-number v1) 5)) \n
  > "** Task X" \n
  > " " \n
  (org-mode)
  (save-buffer)
  )

;;; This section is for tmate / copy / paste for creating/using the right eye
;; ensure a process can run, discard output
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

(defun ii/populate-clipboard-with-tmate-connect-command()
  "Populate the clipboard with the correct command to connect to tmate"
  (message "Trying to populate clipboard")
  (let ((attach-command (if ;; incluster
                            (file-exists-p "/var/run/secrets/kubernetes.io/serviceaccount/namespace")
                            ;; use kubectl
                            (concat "kubectl exec -n "
                                    (with-temp-buffer
                                      (insert-file-contents
                                       "/var/run/secrets/kubernetes.io/serviceaccount/namespace")
                                      (buffer-string))
                                    " -ti " system-name
                                    " attach " (file-name-base load-file-name))
                          ;; out of cluster, use tmate directly
                          (concat "tmate -S " socket " attach"))
                        ))
    (gui-select-text attach-command)
    (osc52-interprogram-cut-function attach-command)
                          )
  )
(defun populate-terminal-clipboard ()
  "Populate the osc52 clipboard via terminal with the start-tmate-sh"
  ;; TODO
  (message "Unable to set X Clipboard to contain the start-tmate-sh")
  ;; (create-target-script tmate-sh start-tmate-command)
  ;; (gui-select-text tmate-sh)
  (if (string= (getenv "KUBERNETES_PORT_443_TCP_PROTO") "tcp")
      (setq current-tmate-sh (concat "kubectl exec -ti " system-name " attach " (file-name-base load-file-name)))
    (progn
      (setq current-tmate-sh tmate-sh) ;; since tmate-sh is buffer-local..
      (if (string= (getenv "CLOUD_SHELL") "true")
          (setq current-tmate-ssh (concat "gcloud alpha cloud-shell ssh --ssh-flag=-t --command=" tmate-sh))
        (if (string= system-name "sharing.io")
            (setq current-tmate-ssh (concat "ssh -tAX " ssh-user-host " " tmate-sh))
            (setq current-tmate-ssh tmate-sh)
          )
        )
      )
  )
  ;;(setq current-tmate-ssh (concat "export IISOCK=" socket " ; rm -f $IISOCK ; ssh -tAX " ssh-user-host " -L $IISOCK:$IISOCK " tmate-sh))
  (if (string= (getenv "KUBERNETES_PORT_443_TCP_PROTO") "tcp")
      (setq current-tmate-ssh (concat "kubectl exec -ti " system-name " attach " (file-name-base load-file-name)))
    (if (string= (getenv "CLOUD_SHELL") "true")
        (setq current-tmate-ssh (concat "gcloud alpha cloud-shell ssh --ssh-flag=-t --command=" tmate-sh))
      (if (string= system-name "sharing.io")
          (setq current-tmate-ssh (concat "ssh -tAX " ssh-user-host " " tmate-sh))
        (setq current-tmate-ssh tmate-sh))
      )
    )
  (message "Trying to set via osc52")
  (osc52-interprogram-cut-function current-tmate-ssh)
  (with-current-buffer (get-buffer-create "start-tmate-sh" )
    (erase-buffer)
     (insert-for-yank "You may need to copy this manually:\n\n" )
     (if (string= (getenv "KUBERNETES_PORT_443_TCP_PROTO") "tcp")
         (insert-for-yank (concat "\nConnect to this in cluster tmate via:\n\n" current-tmate-sh))
       (insert-for-yank
        (concat "\nTo open on another host, forward your iisocket by pasting:\n\n" current-tmate-ssh
              "\n\nOR open another terminal on the same host and paste:\n\n" current-tmate-sh)
      )
     )
  )
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
  (if (string= ssh-host "")
      (progn
        (gui-select-text current-tmate-sh)
        (with-current-buffer (get-buffer-create "start-tmate-sh")
          (insert-for-yank "The following has been populated to your local X clipboard:\n")
          (insert-for-yank
           ;; we can use the global current-tmate-sh
           (concat "Open another terminal on the same host and paste:\n\n" current-tmate-sh)
           ))
        )
    (progn
      (gui-select-text current-tmate-ssh)
      (with-current-buffer (get-buffer-create "start-tmate-ssh")
        (insert-for-yank "The following has been populated to your local X clipboard:\n")
        (insert-for-yank
         ;; we can use the global current-tmate-sh
         (concat "Open another terminal on the your emacs host and paste:\n\n" current-tmate-ssh)
         ))
      )
      )
                                        ; (gui-select-text start-tmate-command)
  (xclip-mode 0)
  ;; and unset it when done
  (setq current-tmate-ssh nil)
  (setq current-tmate-sh nil)
  )
 (defun ssh-find-agent ()
  (interactive)
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "find /tmp /run/host/tmp/ -type s -regex '.*/ssh-.*/agent..*$' 2> /dev/null | tail -n 1 | tr -d '\n'"))
  (message (getenv "SSH_AUTH_SOCK"))
  )
(with-eval-after-load "org"
  ;; (add-to-list 'org-src-lang-modes '("go-mode" . sql))
  (add-to-list 'org-structure-template-alist
               `("g" . "src go")))
;(add-to-list 'org-structure-template-alist
;            `("ii"
;              . "src ii-mode"))

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
;; https://emacs.stackexchange.com/questions/33892/replace-element-of-alist-using-equal-even-if-key-does-not-exist

;; This section is for setting org code block defaults that are based on the current user and file
;; alist-set is used to override the existing settings
(defun alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;; Some local variable defaults that set our database connections
;; note the UID being dynamic, so we can have a dedicated port per person
(defun ii/sql-org-hacks()
  (message "START: ii/sql-org-hacks")
  (set (make-local-variable 'sql-sqlite-program)
       (executable-find "sqlite3"))
  (set (make-local-variable 'sql-server)
       (if (getenv "PGHOST")
           (getenv "PGHOST")
         (if (file-exists-p "/var/run/secrets/kubernetes.io/serviceaccount/namespace")
             "postgres"
             "localhost"
           )))
  (set (make-local-variable 'sql-port)
       (if (getenv "PGPORT")
           (string-to-number (getenv "PGPORT"))
         5432))
  (set (make-local-variable 'sql-user)
       (if (getenv "PGUSER")
           (getenv "PGUSER")
         "apisnoop"))
  (set (make-local-variable 'sql-database)
       (if (getenv "PGDATABASE")
           (getenv "PGDATABASE")
         "apisnoop"))
  (set (make-local-variable 'sql-product)
       '(quote postgres))
  (set (make-local-variable 'sql-connection-alist)
       (list
        (list 'raiinbow
              (list 'sql-product '(quote sqlite))
              (list 'sql-database "raiinbow.sqlite")
              )
        (list 'apisnoop
              (list 'sql-product '(quote postgres))
              (list 'sql-user (getenv "PGUSER"))
              (list 'sql-database (getenv "PGDATABASE"))
              (list 'sql-port sql-port)
              (list 'sql-server (getenv "PGHOST")))
        ;; (list 'apisnoop
        ;;       (list 'sql-product '(quote postgres))
        ;;       (list 'sql-user "apisnoop")
        ;;       (list 'sql-database "apisnoop")
        ;;       (list 'sql-port (+ (* (user-uid) 10) 1))
        ;;       (list 'sql-server "localhost"))
        ;; setting these allows for the connection to be
        ;; created on the fly
        (list 'none
              (list 'sql-product '(quote postgres))
              (list 'sql-user sql-user)
              (list 'sql-database sql-database)
              (list 'sql-port sql-port)
              (list 'sql-server sql-server))))
  (message "END: ii/sql-org-hacks")
  )

;; This sets the local-variables for tmate so that they are specific to the user
;; and org-file name
(defun ii/tmate-org-hacks()
  (message "START: ii/tmate-org-hacks")
  ;; (set (make-local-variable 'ssh-user)
  ;;      user-login-name)
  ;; ;; set this in the org file or ENV
  ;; (set (make-local-variable 'ssh-host)
  ;;      (if (eq (system-name) "sharing.io")
  ;;          (concat "sharing.io")
  ;;        (concat "")
  ;;        ))
  ;; (set (make-local-variable 'ssh-user-host)
  ;;      (concat ssh-user "@" ssh-host))
  ;; (set (make-local-variable 'user-buffer)
  ;;      (concat user-login-name "." (file-name-base load-file-name)))
  ;; (set (make-local-variable 'tmate-sh)
  ;;        (if (and (getenv "TMPDIR") (file-directory-p (getenv "TMPDIR")))
  ;;            (concat (getenv "TMPDIR") user-buffer ".target.sh")
  ;;          (concat "/tmp/" user-buffer ".target.sh")
  ;;          )
  ;;        )
  ;; (set (make-local-variable 'socket)
  ;;      (if (and (getenv "TMPDIR") (file-directory-p (getenv "TMPDIR")))
  ;;          (concat (getenv "TMPDIR") user-buffer ".target.iisocket")
  ;;        (concat "/tmp/" user-buffer ".target.iisocket")
  ;;        ))
  ;; (set (make-local-variable 'socket-param)
  ;;      (concat ":sockets " socket))
  ;; (set (make-local-variable 'copy-tmate-to-ui)
  ;;      ;; TODO make this use osc52 or termux as necessary
  ;;      "; (echo \\$TMATE_CONNECT | xclip -i -sel p -f | xclip -i -sel c ) 2>/dev/null "
  ;;      )
  ;; (set (make-local-variable 'start-tmate-command)
  ;;      (concat
  ;;       "tmate -S "
  ;;       socket
  ;;       " new-session -A -s "
  ;;       user-login-name
  ;;       " -n main "
  ;;       "\"tmate wait tmate-ready "
  ;;       "&& TMATE_CONNECT=\\$("
  ;;       "tmate display -p '#{tmate_ssh} # "
  ;;       user-buffer
  ;;       ".target # "
  ;;       ;; would like this to be shorter
  ;;       (concat
  ;;        (format-time-string "%Y-%m-%d %T")
  ;;        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
  ;;       " # #{tmate_web} ') "
  ;;       "; echo \\$TMATE_CONNECT "
  ;;       copy-tmate-to-ui
  ;;       "; echo Share the above with your friends and hit enter when done. "
  ;;       ;; "; read "
  ;;       "; bash --login\""
  ;;       )
  ;;      )
  ;; at some point we can bring back working on remote hosts
  ;; (set (make-local-variable 'start-tmate-over-ssh-command)
  ;;      (concat
  ;;       "tmate -S "
  ;;       socket
  ;;       " new-session -A -s "
  ;;       user-login-name
  ;;       " -n main "
  ;;       "\"tmate wait tmate-ready "
  ;;       "\\&\\& TMATE_CONNECT=\\$\\("
  ;;       "tmate display -p '#{tmate_ssh} # "
  ;;       user-buffer
  ;;       ".target # "
  ;;       (concat
  ;;        (format-time-string "%Y-%m-%d %T")
  ;;        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
  ;;       " #{tmate_web} '\\) "
  ;;       "; echo \\$TMATE_CONNECT "
  ;;       copy-tmate-to-ui
  ;;       "; echo Share the above with your friends and hit enter when done. "
  ;;       ;; "; read "
  ;;       "; bash --login\""
  ;;       )
  ;;      )
  (message "END: ii/tmate-org-hacks")
  )

;; This uses alist-set to override the default code block parameters
(defun ii/before-local-org-hacks()
  (message "BEGIN ii/before-local-org-hacks")
  (ii/tmate-org-hacks)
  (ii/sql-org-hacks)
  (make-local-variable 'org-babel-default-header-args)
  (setq org-babel-default-header-args
        (alist-set :noweb "yes"
        (alist-set :noweb-ref "(org-entry-get nil \"ITEM\")"
        (alist-set :comments "org"
        (alist-set :exports "both"
        (alist-set :eval "never-export"
        (alist-set :results "replace code"
                   org-babel-default-header-args)))))))
  (make-local-variable 'org-babel-default-header-args:tmate)
  (setq org-babel-default-header-args:tmate
        (alist-set :exports "code"
        (alist-set :session (concat user-login-name ":main")
        (alist-set :window user-login-name
        ;; (alist-set :socket socket
                   org-babel-default-header-args:tmate)
        ;; )
        )))
  (make-local-variable 'org-babel-default-header-args:tmate)
  (setq org-babel-default-header-args:tmux
        (alist-set :exports "code"
        (alist-set :session (concat user-login-name ":main")
        (alist-set :window user-login-name
        (alist-set :socket socket
                   org-babel-default-header-args:tmate)))))
  (make-local-variable 'org-babel-default-header-args:sql-mode)
  (setq org-babel-default-header-args:sql-mode
        (alist-set :results "replace code"
        (alist-set :product "postgres"
        (alist-set :comments "org"
        (alist-set :wrap "SRC example"
                   org-babel-default-header-args:sql-mode)))))
  ;; (make-local-variable 'org-babel-default-header-args:emacs-lisp)
  ;; (setq org-babel-default-header-args:emacs-lisp
  ;;       (alist-set :results "replace code"
  ;;                  org-babel-default-header-args:emacs-lisp))
  ;; (make-local-variable 'org-babel-default-header-args:elisp)
  ;; (setq org-babel-default-header-args:elisp
  ;;       (alist-set :results "replace code"
  ;;                  org-babel-default-header-args:elisp))
  (make-local-variable 'org-babel-default-header-args:shell)
  (setq org-babel-default-header-args:shell
        (alist-set :results "output code verbatim replace"
                   (alist-set :wrap "example"
                              org-babel-default-header-args:shell)))
  (make-local-variable 'org-babel-default-header-args:bash)
  (setq org-babel-default-header-args:bash
        (alist-set :results "output code verbatim replace"
        (alist-set :wrap "example"
                   org-babel-default-header-args:bash)))
  (make-local-variable 'org-babel-default-header-args:sh)
  (setq org-babel-default-header-args:sh
        (alist-set :results "output code verbatim replace"
        (alist-set :wrap "example"
                   org-babel-default-header-args:sh)))
  ;; (make-local-variable 'org-babel-default-header-args:json)
  ;; (setq org-babel-default-header-args:json
  ;;       (alist-set :results "output code verbatim replace"
  ;;       (alist-set :wrap "src EXAMPLE"
  ;;                  org-babel-default-header-args:json)))
  ;; (make-local-variable 'org-babel-default-header-args:yaml)
  ;; (setq org-babel-default-header-args:yaml
  ;;       (alist-set :results "output code verbatim replace"
  ;;       (alist-set :wrap "src EXAMPLE"
  ;;                  org-babel-default-header-args:yaml)))
  ;;   (message "BEGIN ii/before-local-org-hacks")
  ;;   )
  ;; ;; Setup tmate socket etc
  ;; (defun ii/after-local-var-hacks()
  ;;   (message "BEGIN: ii/after-local-var-hacks")
  ;; (message tmate-sh-sh)
  ;; For testing / setting DISPLAY to something else
  ;; (getenv "DISPLAY")
  ;; (setenv "DISPLAY" ":0")
  ;; As we start on other OSes, we'll need to copy this differently
  ;; does this org require a right eye?
  ;; local var for that
  ;; (if (xclip-working)
  ;;     (populate-x-clipboard)
  ;;   (populate-terminal-clipboard)
  ;;   )
  ;; (switch-to-buffer "start-tmate-sh")
  ;; (y-or-n-p "Have you Pasted?")
  (message "END ii/before-local-org-hacks")
  ;; (switch-to-buffer ii-org-buffer)
  )

(defun ii/advice:org-babel-exp-src-block (obde)
  "Disable the execute advice when we export src blocks"
  (advice-remove 'org-babel-execute-src-block
                 #'ii/advice:org-babel-execute-src-block)
  (funcall obde)
  (advice-add 'org-babel-execute-src-block
               :before #'ii/advice:org-babel-execute-src-block)
  )
(defun ii/advice:org-babel-execute-src-block (&optional arg info params)
  "if ii-mate not set and this is a tmate src block"
  (interactive)
  ;; only run if this is a tmate block
  (if (string= "tmate" (car (org-babel-get-src-block-info t)))
      (let* (
             (org-session (alist-get :session (nth 2 (org-babel-get-src-block-info t))))
             ;; (org-session (cdr (assq :session params)))
             (terminal (alist-get :terminal (nth 2 (org-babel-get-src-block-info t))))
             ;; (terminal (cdr (assq :terminal params)))
             ;; (socket (cdr (assq :socket params)))
             (socket (alist-get :socket (nth 2 (org-babel-get-src-block-info t))))
             (session-dir (cdr (assq :dir params)))
             (session-x (message "terminal: %S, socket: %S, org-session: %S" terminal socket org-session))
             (session-name (ob-tmate--tmate-session org-session))
             (session-window (ob-tmate--tmate-window org-session))
             (session-socket (if socket
                                 (expand-file-name socket)
                               (ob-tmate--tmate-socket org-session)
                       ))
             (session-y (message "session-socket: %S" session-socket))
            ;; (dir
            ;;  (file-truename
            ;;   (alist-get :dir (nth 2 (org-babel-get-src-block-info t))
            ;;              (file-name-directory buffer-file-name))))
            ;; (target-name
            ;;  (file-name-base load-file-name))
            )
        (progn
          (message "about to trying to start ii-tmate-process")
          ;; (make-local-variable 'ii-tmate-process)
          (make-local-variable 'ii-tmate-configured)
          ;; ensure a tmate server has been started
          (unless ii-tmate-process
            (progn
              (setq ii-tmate-process
                    (start-process-shell-command
                     (concat session-name "-tmate-process")
                     (concat "**" session-name "-tmate-process**")
                     (concat "tmate"
                             ;; " -F -v"
                             " -d -v"
                             " -S " session-socket
                             " new-session"
                             " -s " session-name
                             " -n " "init"; session-window
                             " -c " session-dir
                             "read X")
                     ))
              ;; (call-process-shell-command (concat "tmate -S " socket
              ;;                                     " wait tmate-ready") nil "wait ready")
              ;; (call-process-shell-command (concat "tmate -S " socket
              ;;                                     " set-hook -g client-attached"
              ;;                                     " 'run-shell "
              ;;                                     "\"tmate new-window osc52-tmate.sh\"'") nil "client-atteched")
                     ))
          ;; popup asking user to paste connection command into another terminal
          ;; (switch-to-buffer "**tmate-process**")
          ;; (unless t ; ii-tmate-configured
          ;;   (progn
          ;;     (if (string= system-type "darwin")
          ;;         (message "We used to start tmate/iterm here, now we do it elsewhere")
          ;;         ;; (iterm-new-window-send-string
          ;;         ;;  (concat
          ;;         ;;   "tmate -S " socket ; Wait for tmate to be ready
          ;;         ;;   " wait tmate-ready "
          ;;         ;;   "; tmate -S " socket ; copy ssh/url to clipboard
          ;;         ;;   " display -p '#{tmate_ssh } # #{tmate_web} | pbcopy' "
          ;;         ;;   "; tmate -S " socket ; create a new session
          ;;         ;;   " new-session " ; OR
          ;;         ;;   "|| tmate -S " socket ; attach
          ;;         ;;   " at" ; TODO get -CC working for iTerm
          ;;         ;;   ))
          ;;       (progn
          ;;         (ii/populate-clipboard-with-tmate-connect-command)
          ;;         (setq ii-org-buffer (current-buffer))
          ;;         ;; (if (xclip-working)
          ;;         ;;     (populate-x-clipboard)
          ;;         ;;   (populate-terminal-clipboard)
          ;;         ;;   )
          ;;         ;; (switch-to-buffer "start-tmate-sh")
          ;;         ;; (y-or-n-p "Have you Pasted?")
          ;;         ;; (switch-to-buffer ii-org-buffer)
          ;;         ;; (y-or-n-p "A command has been copied to your local OS. Have you pasted it into a terminal?")
          ;;         )
          ;;       )
          ;;     (setq ii-tmate-configured t)
          ;;     ))
          )
      )
    ))

;; This is the function intended to be run as a before-hack-local-variables-hook
(defun ii/before-local-var-hacks()
  (message "BEGIN: ii/before-local-var-hacks")
  (if (string-equal mode-name "Org")
      (if (alist-get 'ii file-local-variables-alist)
          (ii/before-local-org-hacks)
        )
  )
  (message "END: ii/before-local-var-hacks")
  (message "ii org-mode hacks applied")
  )

