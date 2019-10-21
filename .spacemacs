;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-private-snippets-directory (expand-file-name "~/.emacs.d/snippets")
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     deft ;; https://jblevins.org/projects/deft/
     emacs-lisp
     emoji ;; sudo apt-get install ttf-ancient-fonts
     ;; ~SPC a E~ ~SPC i e~
     git
     helm
     lsp
     markdown
     (mu4e :variables
           mu4e-enable-mode-line nil
           mu4e-enable-notifications nil
           mu4e-drafts-folder "/[Gmail].Drafts"
           mu4e-sent-folder   "/[Gmail].Sent Mail"
           mu4e-trash-folder  "/[Gmail].Bin"
           mu4e-maildir-shortcuts
           '( ("/INBOX"               . ?i)
              ("/[Gmail].Sent Mail"   . ?s)
              ("/[Gmail].Bin"       . ?t)
              ("/[Gmail].All Mail"    . ?a))
           )
     multiple-cursors
     neotree
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-hugo-support t
          org-enable-org-journal-support t
          org-projectile-file "TODO.org"
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-babel-tmate-session-prefix ""
          org-babel-python-command "python3"
          org-babel-tmate-default-window-name "main"
          org-confirm-babel-evaluate nil
          org-use-property-inheritance t
          )
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     sql
     syntax-checking
     copy-as-format ;; ~SPC x f g~ copy-as-format-markdown
     docker
     dash ;; https://zealdocs.org/download.html
     ;;forge
     (go :variables
         godoc-at-point-function 'godoc-gogetdoc
         go-tab-width 4
         go-use-gometalinter t)
     html
     ;;;; impatient-mode
     javascript
     lua
     (lsp :variables
          lsp-navigation 'both
          lsp-ui-doc-enable t
          lsp-ui-doc-position 'top
          lsp-ui-doc-alignment 'frame
                                        ; lsp-ui-doc-border 'white
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-use-webkit t
          lsp-ui-doc-delay 0.2
          lsp-ui-doc-include-signature nil 
          lsp-ui-sideline-show-symbol t
          lsp-ui-remap-xref-keybindings t
          lsp-ui-sideline-enable t
          lsp-prefer-flymake nil
          lsp-print-io t
          )
     nginx
     pandoc
     pdf
     python
     ruby
     ;; speed-reading
     ;; search-engine
     shell-scripts
     terraform
     treemacs
     typography
     ;; #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.0.0/
     ;; (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
     ;;async-shell
     xkcd ;; ~SPC a x~
     version-control
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(

                                      async
                                      closql
                                      command-log-mode
                                      dash
                                      demo-it
                                      ein ;; https://github.com/millejoh/emacs-ipython-notebook
                                      emms
                                      emacsql-sqlite
                                      evil-vimish-fold
                                      fancy-narrow
                                      feature-mode
                                      ;;(forge :location "/usr/local/share/emacs/site-lisp/forge"
                                      ;;       :afer magit)
                                      ghub
                                      go-playground
                                      go-dlv
                                      gorepl-mode ;; go
                                      graphql
                                      graphql-mode
                                      ;; (graphql-mode :location (recipe
                                      ;;                          :fetcher github
                                      ;;                          :repo "davazp/graphql-mode"
                                      ;;                          :commit "301a218"
                                      ;;                          ))
                                      groovy-mode
                                      jupyter
                                      ob-async
                                      (ob-tmate :ensure t
                                                :location (recipe
                                                           :fetcher github
                                                           :repo "ii/ob-tmate"))
                                      org-pdfview
                                      (ob-sql-mode :ensure t)
                                      oer-reveal
                                      (org-protocol-capture-html :location (recipe
                                                                            :fetcher github
                                                                            :repo "alphapapa/org-protocol-capture-html"
                                                                            :commit "23a1336c"))
                                      org-re-reveal-ref
                                      (emacs-reveal :location (recipe
                                                               :fetcher gitlab
                                                               :repo "oer/emacs-reveal"
                                                               :commit "d0aa1f9d"))
                                      ob-go
                                      ;; org-protocol ;; https://orgmode.org/worg/org-contrib/org-protocol.html
                                      ;; http://tech.memoryimprintstudio.com/org-capture-from-external-applications/
                                      ;; https://github.com/sprig/org-capture-extension
                                      ob-tmux
                                      org-babel-eval-in-repl
                                      org-tree-slide
                                      ;; org-mu4e
                                      org-pdfview
                                      ox-reveal
                                      ;; pdf-tools ;; https://github.com/politza/pdf-tools
                                      ;; pdf-view
                                      s
                                      scad-mode
                                      slime
                                      transcribe
                                      togetherly
                                      vimish-fold
                                      xclip
                                      (yasnippet :location (recipe
                                                            :fetcher github
                                                            :repo "joaotavora/yasnippet"
                                                            :branch "0.13.0"))
                                                            ;; :commit "89eb7ab"))
                                      ;;                      :branch "0.12.2"))
                                      ;; for tmate and over ssh cut-and-paste
                                                 ;; https://gist.github.com/49eabc1978fe3d6dedb3ca5674a16ece.git
                                      ;; sakura is waiting on vte
                                      ;; https://bugs.launchpad.net/sakura/+bug/1769575
                                      ;; I'm pretty sure the lib vte issue is stale
                                      ;; https://bugzilla.gnome.org/show_bug.cgi?id=795774
                                      ;; available in minitty since 2.6.1
                                      ;; https://github.com/mintty/mintty/issues/258
                                      ;; http://mintty.github.io/ (Default tty on Cygwin etc)
                                      ;; I created a ticket to add support to vte
                                      ;; https://gitlab.gnome.org/GNOME/vte/issues/125
                                      ;; this would in turn enable support on many
                                      ;; default linux/gnome terminals
                                      ;; for now, you probably want to use xterm
                                      ;;(osc52e :location (recipe
                                      ;;                   :fetcher git
                                      ;;                   :url "https://gist.github.com/49eabc1978fe3d6dedb3ca5674a16ece.git"
                                      ;;                   :ensure t
                                      ;;                   ))
                                      ;; for jupyter
                                      websocket
                                      ;; simple-httpd
                                      ;; emacs-websocket
                                      ;; company-mode
                                      ;; markdown-mode
                                      (zmq :ensure t)
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper t

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa t

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   ;; Can be a string path to PNG
   ;; spacemacs-banner-directory (expand-file-name (concat dotspacemacs-directory "banners/"))
   dotspacemacs-startup-banner 4

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
   (require 'ein)
   (require 'togetherly)
   (require 'org-checklist)
   (require 'ox-publish)
   (require 'ob-ein)
   (require 'ob-sql-mode)
   (require 'ob-tmate)
   (require 'zmq)
   (load (expand-file-name "~/.emacs.d/osc52e/osc52e.el"))
   (when (version<= "9.2" (org-version))
     (require 'org-tempo))
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (require 'ein)
  (require 'togetherly)
  (require 'org-checklist)
  (require 'ox-publish)
  (require 'ob-ein)
  (require 'ob-sql-mode)
  (require 'ob-tmate)
  (require 'zmq)
  (load (expand-file-name "~/.emacs.d/osc52e/osc52e.el"))
  (when (version<= "9.2" (org-version))
    (require 'org-tempo))
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
  ;; https://www.wisdomandwonder.com/article/10630/how-fast-can-you-tangle-in-org-mode
  (setq help/default-gc-cons-threshold gc-cons-threshold)
  (defun help/set-gc-cons-threshold (&optional multiplier notify)
    "Set `gc-cons-threshold' either to its default value or a
   `multiplier' thereof."
    (let* ((new-multiplier (or multiplier 1))
           (new-threshold (* help/default-gc-cons-threshold
                             new-multiplier)))
      (setq gc-cons-threshold new-threshold)
      (when notify (message "Setting `gc-cons-threshold' to %s"
                            new-threshold))))
  (defun help/double-gc-cons-threshold () "Double `gc-cons-threshold'." (help/set-gc-cons-threshold 2))
  (add-hook 'org-babel-pre-tangle-hook #'help/double-gc-cons-threshold)
  (add-hook 'org-babel-post-tangle-hook #'help/set-gc-cons-threshold)
  ;; FIXME: workaround
  ;; https://github.com/syl20bnr/spacemacs/issues/11798
  ;; (when (version<= "9.2" (org-version))
  ;;   (require 'org-tempo))
  ;; (require 'ob-tmate)
  ;; (require 'ob-sql-mode)
  ;; (setq-default
  ;;  time-stamp-zone "Pacific/Auckland"
  ;;  ;; https://www.emacswiki.org/emacs/TimeStamp
  ;;  time-stamp-pattern "10/#+UPDATED: needs time-local formatted regexp"
  ;;  )
  ;; (defun togetherly-server-start-now ()
  ;;   "Start a Togetherly server with this buffer."
  ;;   (interactive)
  ;;   (cond ((null togetherly--server)
  ;;          (let* ((addr "127.0.0.1")
  ;;                 (server-port togetherly-port)
  ;;                 (server-name user-login-name)
  ;;                 (server-proc (make-network-process
  ;;                               :name "togetherly-server" :server t
  ;;                               :service server-port :noquery t :host addr
  ;;                               :sentinel 'togetherly--server-sentinel-function
  ;;                               :filter 'togetherly--server-filter-function))
  ;;                 (rcolor (car togetherly-region-colors))
  ;;                 (pcolor (car togetherly-cursor-colors)))
  ;;            (setq togetherly-region-colors   (cdr togetherly-region-colors)
  ;;                  togetherly-cursor-colors   (cdr togetherly-cursor-colors)
  ;;                  togetherly--server         `(,server-proc ,server-name ,rcolor . ,pcolor)
  ;;                  togetherly--server-buffer  (current-buffer)
  ;;                  togetherly--server-clients nil
  ;;                  togetherly--server-timer-object
  ;;                  (run-with-timer nil togetherly-cursor-sync-rate
  ;;                                  'togetherly--server-broadcast-cursor-positions))
  ;;            (set (make-local-variable 'header-line-format)
  ;;                 (concat " " (propertize server-name 'face `(:background ,pcolor)))))
  ;;          (add-hook 'before-change-functions 'togetherly--server-before-change nil t)
  ;;          (add-hook 'after-change-functions 'togetherly--server-after-change nil t)
  ;;          (add-hook 'kill-buffer-query-functions 'togetherly--server-kill-buffer-query)
  ;;          (populate-x-togetherly) ;; go ahead and create the tmate paste for the togetherly
  ;;          )
  ;;         ((y-or-n-p "Togetherly server already started. Migrate to this buffer ? ")
  ;;          (set (make-local-variable 'header-line-format)
  ;;               (buffer-local-value 'header-line-format togetherly--server-buffer))
  ;;          (add-hook 'before-change-functions 'togetherly--server-before-change nil t)
  ;;          (add-hook 'after-change-functions 'togetherly--server-after-change nil t)
  ;;          (with-current-buffer togetherly--server-buffer
  ;;            (remove-hook 'before-change-functions 'togetherly--server-before-change t)
  ;;            (remove-hook 'after-change-functions 'togetherly--server-after-change t)
  ;;            (kill-local-variable 'header-line-format))
  ;;          (setq togetherly--server-buffer (current-buffer))
  ;;          (togetherly--server-broadcast `(welcome ,(togetherly--buffer-string) . ,major-mode))
  ;;          )
  ;;         (t
  ;;          (message "Togetherly: Canceled."))))
  ;; (defun populate-x-togetherly ()
  ;;   "Populate the clipboard with the command for a together client"
  ;;   (interactive)
  ;;   (message "Setting X Clipboard to contain the start-tmate command")
  ;;   (xclip-mode 1)
  ;;   (gui-select-text start-tmate-for-togetherly-client)
  ;;   )
  ;; (defun yas/org-very-safe-expand ()
  ;;   (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (make-variable-buffer-local 'yas/trigger-key)
  ;;             (setq yas/trigger-key [tab])
  ;;             (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  ;;             (define-key yas/keymap [tab] 'yas/next-field)))
  ;; info:org#Conflicts for org 9 and very recent yas
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (setq-local yas/trigger-key [tab])
  ;;             (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
  ;;  (defun yas/org-very-safe-expand ()
  ;;    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  ;;  (yas/expand)
  ;; (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  ;;(make-variable-buffer-local 'yas/trigger-key)
  ;;(setq yas/trigger-key [tab])
  ;;(define-key yas/keymap [tab] 'yas/next-field)
  )
;; ‘yasnippet.el’
;;      The way Org mode binds the ‘<TAB>’ key (binding to ‘[tab]’ instead
;;      of ‘"\t"’) overrules YASnippet’s access to this key.  The following
;;      code fixed this problem:

;;           (add-hook 'org-mode-hook
;;                     (lambda ()
;;                       (setq-local yas/trigger-key [tab])
;;                       (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

;;      The latest version of YASnippet does not play well with Org mode.
;;      If the above code does not fix the conflict, start by defining the
;;      following function:

;;           (defun yas/org-very-safe-expand ()
;;             (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;;      Then, tell Org mode to use that function:

;;           (add-hook 'org-mode-hook
;;                     (lambda ()
;;                       (make-variable-buffer-local 'yas/trigger-key)
;;                       (setq yas/trigger-key [tab])
;;                       (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
;;                       (define-key yas/keymap [tab] 'yas/next-field)))

