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

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(sql
     clojure
     rust
     typescript
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
     ;; epub
     ;; elfeed
     git
     helm
     ;; ietf
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
          org-journal-time-format "")
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; slack
     emoji ;; sudo apt-get install ttf-ancient-fonts
     ;; ~SPC a E~ ~SPC i e~
     copy-as-format ;; ~SPC x f g~ copy-as-format-markdown
     docker
     dash ;; https://zealdocs.org/download.html
     (go :variables
         godoc-at-point-function 'godoc-gogetdoc
         go-tab-width 4
         go-use-gometalinter t)
     html
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
     themes-megapack
     yaml
     ;; #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.0.0/
     ;; (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
     ;;async-shell
     xkcd ;; ~SPC a x~
     version-control
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ;; ob-async version in elpa
                                      ;; is too old, get master
                                      ;; ob-async,, but async is fine
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
                                      (forge :location "/usr/local/share/emacs/site-lisp/forge"
                                             :afer magit)
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
                                      ;; coped these into org layer
                                      ;;(org :location "/usr/local/share/emacs/site-lisp/org-9.2.2")
                                      (ob-tmate :location "/usr/local/share/emacs/site-lisp/ob-tmate")
                                      (ob-async :location "/usr/local/share/emacs/site-lisp/ob-async")
                                      (impatient-mode :location "/usr/local/share/emacs/site-lisp/impatient-mode")
                                      ;; https://gitlab.com/oer/oer-reveal
                                      ob-sql-mode
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
                                                            :commit "89eb7ab"))
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
                                      (osc52e :location "/usr/local/share/emacs/site-lisp/osc52e")
                                      ;; for jupyter
                                      websocket
                                      ;; simple-httpd
                                      ;; emacs-websocket
                                      ;; company-mode
                                      ;; markdown-mode
                                      )

   ;; A list of packages that cannot be updatedj.j
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
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

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
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
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

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 11 ;; 'official

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

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         adwaita
                         rebecca)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 0.75)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; https://wiki.debian.org/Fonts#Font_lists
   ;; https://github.com/hbin/top-programming-fonts
   ;; "Source Code Pro"
   ;; "Inconsolata"
   ;; "terminus"
   ;; "IBM 3270"
   ;; "DejaVu Sans Mono" ;; Very nice
   ;; "FiraCode"
   ;; "Menlo"
   ;; "Monaco"
   ;; "SourceCodeVariable"
   ;; -unknown-DejaVu Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1
   ;; "mononoki"
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 30
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
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   ;; dotspacemacs-line-numbers nil
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

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
  ;; our org-capture-templates
  ;; (setq org-capture-templates '(
  ;;                               ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
  ;;                                "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
  ;;                               ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
  ;;                                "* %? [[%:link][%:description]] \nCaptured On: %U")
  ;;                               ))
  (setq deft-directory (expand-file-name "~/ii/org/"))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq browse-url-generic-program "chromium-browser")
  (setq
   org-capture-templates
   (quote(
          ("w" "Default template"
           entry (file+headline "~/org/capture.org" "Notes")
           "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1)
          ("t" "TODO"
           entry (file+headline "~/org/TODO.org" "Inbox")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
           )
          ("p" "Protocol"
           entry (file+headline "~/ii/org/agenda/notes.org" "Inbox")
           ;; "* [[%:link][%:description]] %^G\nSource: %u, %c\n #+BEGIN_QUOTE\n%initial\n#+END_QUOTE\n\n\n%?"
           "* [[%:link][%:description]] %^G\nTime: %U\n #+BEGIN_QUOTE\n%:initial\n#+END_QUOTE\n\n\n%?"
           )
          ("L" "Protocol Link"
           entry (file+headline "~/ii/org/agenda/notes.org" "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ;; ... more templates here ...
          )))
  ;; our go stuff
  (setq go-format-before-save t)
  (setq gofmt-command "goimports")
  (setq quelpa-checkout-melpa-p nil)
  ;; would probably be nicer to have these somewhere later
  (let*
      (
       (site-lisp "/usr/local/share/emacs/site-lisp/")
       (elpa-path (concat site-lisp "elpa/" (symbol-value 'emacs-version) "/develop/"))
       )
    ;; (add-to-list 'load-path (concat elpa-path "togetherly-20170426.616"))
    ;; (load (concat elpa-path "togetherly-20170426.616/togetherly.el"))
    ;; (add-to-list 'load-path (concat elpa-path "async-20190503.656"))
    ;; (load (concat elpa-path "async-20190503.656/async.el"))
    ;; (add-to-list 'load-path (concat elpa-path "dash-20190424.1804"))
    ;; (load (concat elpa-path "dash-20190424.1804/dash.el"))
    ;; (load (concat site-lisp "ob-tmate/ob-tmate.el"))
    ;; (load (concat site-lisp "ob-async/ob-async.el"))
    ;; (add-to-list 'load-path "~/.emacs.d/private/local/emacs-zmq/")
    ;; (load (concat site-lisp "ob-async/ob-async.el"))
    ;; (load (concat site-lisp "ob-async/ob-async.el"))
    ;; (add-to-list 'load-path "~/.emacs.d/private/local/emacs-zmq/")
    ;; (load "~/.emacs.d/private/local/ob-tmate.el/ob-tmate.el")
    ;; (load "~/.emacs.d/private/local/ob-tmux.el/ob-tmux.el")
    ;; (require 'togetherly)
    )
  ;; this needs to be defined early
  ;; from http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
  ;; function to return first name of email recipients
  ;; used by yasnippet
  ;; inspired by
  ;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
  (defun bjm/mu4e-get-names-for-yasnippet ()
    "Return comma separated string of names for an email"
    (interactive)
    (let ((email-name "") str email-string email-list email-name2 tmpname)
      (save-excursion
        (goto-char (point-min))
        ;; first line in email could be some hidden line containing NO to field
        (setq str (buffer-substring-no-properties (point-min) (point-max))))
      ;; take name from TO field - match series of names
      (when (string-match "^To: \"?\\(.+\\)" str)
        (setq email-string (match-string 1 str)))
      ;;split to list by comma
      (setq email-list (split-string email-string " *, *"))
      ;;loop over emails
      (dolist (tmpstr email-list)
        ;;get first word of email string
        (setq tmpname (car (split-string tmpstr " ")))
        ;;remove whitespace or ""
        (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
        ;;join to string
        (setq email-name
              (concat email-name ", " tmpname)))
      ;;remove initial comma
      (setq email-name (replace-regexp-in-string "^, " "" email-name))
      ;;see if we want to use the name in the FROM field
      ;;get name in FROM field if available, but only if there is only
      ;;one name in TO field
      (if (< (length email-list) 2)
          (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
            (progn (setq email-name2 (match-string 1 str))
                   ;;prefer name in FROM field if TO field has "@"
                   (when (string-match "@" email-name)
                     (setq email-name email-name2))
                   )))
      email-name))
  ;; define function to display ansi colours for a buffer
  ;; http://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
  (require 'ansi-color)
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  ;; http://pragmaticemacs.com/emacs/how-i-view-my-google-calendar-agenda-in-emacs/
  (defun ii/open-gcal-agenda ()
    "Open my google calendar agenda file. The agenda is displayed in the buffer *gcal*."
    (interactive)
    ;; set name of calendar buffer and location of file containing my agenda
    (let ((tmp-buff-name "*gcal*") (cal-file (expand-file-name "/homeb/bjm/gcal")))
      ;; switch to calendar buffer
      (switch-to-buffer tmp-buff-name)
      ;; turn off read only to overwrite if buffer exists
      (read-only-mode -1)
      ;; clear buffer
      (erase-buffer)
      ;; insert agenda file
      (shell-command "gcalcli agenda" (current-buffer))
      ;; (insert (shell-command "gcalcli agenda"))
      ;; turn on colours
      (display-ansi-colors)
      ;; turn on special mode
      (special-mode)
      ;; turn off line wrapping
      (visual-line-mode -1)))
  (defun togetherly-client-quick-start (the-port)
    (interactive)
    (let* ((host "127.0.0.1")
           (port the-port)
           (name (setq togetherly--client-name (togetherly--read-display-name))))
      (setq togetherly--client-process
            (make-network-process
             :name "togetherly" :host host :service port :noquery t
             :buffer (get-buffer-create "*Togetherly*")
             :sentinel 'togetherly--client-sentinel-function
             :filter 'togetherly--client-filter-function))
      (switch-to-buffer "*Togetherly*")
      (setq togetherly--client-timer-object
            (run-with-timer nil togetherly-cursor-sync-rate
                            'togetherly--client-report-cursor-position))
      (add-hook 'kill-buffer-query-functions 'togetherly--client-kill-buffer-query)
      (togetherly--client-send `(login . ,name))))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;;(global-set-key (kbd "C-a") 'beginning-of-line)
  ;;(global-set-key (kbd "C-y") 'yank)
  ;;(global-set-key (kbd "C-p") 'previous-line)
                                        ; smtp
  (load "/usr/local/share/emacs/site-lisp/forge/lisp/forge.el")
  (add-to-list 'forge-alist '("gitlab.ii.coop" "gitlab.ii.coop/api/v4" "gitlab.ii.coop" forge-gitlab-repository))
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t)
  ;; Disable mu4e until we figure out a nice per user+box config
  ;; (setq mu4e-reply-to-address "hh@ii.coop"
  ;;       user-mail-address "hh@ii.coop"
  ;;       user-full-name  "Hippie Hacker")
  ;; (add-hook 'mu4e-compose-mode-hook
  ;;           (defun my-do-compose-stuff ()
  ;;             "My settings for message composition."
  ;;             (set-fill-column 72)
  ;;             (flyspell-mode)))
  ;; ;; activate debugging
  ;; (setq
  ;;  mu4e-maildir (expand-file-name "~/Maildir")
  ;;  ;; Use for testing
  ;;  ;; mu4e-get-mail-command "true"
  ;;  mu4e-get-mail-command "mbsync gmail"
  ;;  mu4e-change-filenames-when-moving t
  ;;  smtpmail-queue-mail nil
  ;;  smtpmail-queue-dir "~/Maildir/queue/cur"
  ;;  )
  (setq debug-on-error nil
        debug-on-signal nil
        debug-on-quit nil)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (add-hook 'python-mode-hook #'lsp)
  ;; info:org#Conflicts for org 9 and very recent yas
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))
  ;; (add-hook 'message-mode-hook
  ;;           (lambda ()
  ;;             (make-variable-buffer-local 'yas/trigger-key)
  ;;             (setq yas/trigger-key [tab])
  ;;             (setq message-tab-body-function nil)
  ;;             ;; (setq message-tab-body-function 'yas/org-very-safe-expand)
  ;;             (define-key yas/keymap [tab] 'yas/next-field)))
  ;; We need this for gocode etc
  (setenv "PATH" (concat user-home-directory "go/bin:" (getenv "PATH")))
  (defun ssh-find-agent ()
    (interactive)
    (setenv "SSH_AUTH_SOCK" (shell-command-to-string "\
          . ~/.ssh/ssh-find-agent.sh ;\
          ssh-find-agent | grep ssh- | tail -1 \
          | tail -1 | awk '{print $2}' | awk -F= '{print $2}' \
          | tr --delete '\n'"))
    (message (getenv "SSH_AUTH_SOCK"))
    )
  ;; (add-to-list 'load-path "~/.emacs.d/private/local/emacs-zmq/")
  ;; (add-to-list 'load-path "~/.emacs.d/private/local/emacs-jupyter/")
  ;; (load "~/.emacs.d/private/local/emacs-jupyter/jupyter.el")

  ;; Loading /home/hippie/.emacs.d/private/local/ob-async/ob-async.el (source)...
  ;; (Spacemacs) Error in dotspacemacs/user-config: Symbol’s function definition is void: org-combine-plists
  ;; (add-to-list 'load-path "~/.emacs.d/private/local/ob-async/")
  ;; (load "~/.emacs.d/private/local/ob-async/ob-async.el")
  ;; for updating tickets and PRs from emacs
  ;;;;;; We load these pretty late, and we could probably find a way to
  ;;;;;; have the loading cached
  ;; Loading /home/hippie/.spacemacs...done
  ;; Loading /home/hippie/.emacs.d/private/local/ob-tmate.el/ob-tmate.el (source)...done
  ;; Loading /home/hippie/.emacs.d/elpa/26.2/develop/togetherly-20170426.616/togetherly.el (source)...done
  ;; Spacemacs is ready.
  ;; Loading /home/hippie/.emacs.d/private/local/forge/lisp/forge.el (source)...done
  ;; Loading /home/hippie/.emacs.d/.cache/recentf...done

  ;; (add-to-list 'load-path "~/.emacs.d/private/local/forge/lisp/")
  ;; (load "~/.emacs.d/private/local/forge/lisp/forge.el")
  ;; (add-to-list 'forge-alist '("gitlab.ii.coop" "gitlab.ii.coop/api/v4" "gitlab.ii.coop" forge-gitlab-repository))
  ;; (require 'togetherly)
  ;; (spacemacs/set-leader-keys "SPC" 'avy-goto-word-or-subword-1)
  (spacemacs/declare-prefix "o" "o-prefix")
  (spacemacs/set-leader-keys "ob" 'org-switchb)
  (spacemacs/set-leader-keys "o#" 'org-agenda-list-stuck-projects)
  (spacemacs/set-leader-keys "o/" 'org-occur-in-agenda-files)
  (spacemacs/set-leader-keys "oa" 'org-agenda-list)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (spacemacs/set-leader-keys "ol" 'org-store-link)
  (spacemacs/set-leader-keys "oe" 'org-store-agenda-views)
  (spacemacs/set-leader-keys "om" 'org-tags-view)
  (spacemacs/set-leader-keys "oo" 'org-agenda)
  (spacemacs/set-leader-keys "oO" 'org-clock-out)
  (spacemacs/set-leader-keys "op" 'org-projectile/capture)
  (spacemacs/set-leader-keys "oP" 'org-projectile/goto-todos)
  (spacemacs/set-leader-keys "or" 'org-reveal)
  (spacemacs/set-leader-keys "os" 'org-search-view)
  (spacemacs/set-leader-keys "ot" 'org-todo-list)
  ;; (spacemacs/declare-prefix "oc" "oc-prefix")
  ;; (spacemacs/set-leader-keys "ocv" 'org-copy-visible)
  ;; (spacemacs/declare-prefix "ot" "ot-prefix")
  ;; (spacemacs/set-leader-keys "oti" 'org-copy-visible)
  (spacemacs/declare-prefix "oi" "oi-prefix")
  (spacemacs/set-leader-keys "oil" 'org-insert-link)
  (spacemacs/declare-prefix "mo" "major-o-prefix")
  (spacemacs/declare-prefix-for-mode 'org-mode "o" "org-major-o-prefix")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "oc" 'org-capture)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ob" 'org-iswitchb-completing-read)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-0") 'text-scale-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;; (defun dotspacemacs/emacs-custom-settings ()
;;  "Emacs custom settings.
;; This is an auto-generated function, do not modify its content directly, use
;; Emacs customize menu instead.
;; This function is called at the very end of Spacemacs initialization."
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (jupyter zmq org-protocol-capture-html lsp-ui lsp-treemacs helm-lsp company-lsp lsp-mode emms transcribe emacs-reveal oer-reveal ox-reveal deft mu4e-maildirs-extension mu4e-alert helm-mu zeal-at-point xkcd helm-dash dash-docs emojify emoji-cheat-sheet-plus copy-as-format pdf-tools org-babel-eval-in-repl ess matlab-mode eval-in-repl julia-mode gorepl-mode go-playground gotest graphql-mode zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xclip ws-butler writeroom-mode winum white-sand-theme which-key websocket web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme typo twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-evil toxi-theme toml-mode togetherly toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime slim-mode seti-theme seeing-is-believing scss-mode scad-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe reverse-theme restart-emacs rebecca-theme rbenv rake rainbow-delimiters railscasts-theme racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode password-generator paradox pandoc-mode ox-twbs ox-pandoc ox-hugo ox-gfm overseer orgit organic-green-theme org-tree-slide org-re-reveal org-projectile org-present org-pomodoro org-mime org-journal org-download org-cliplink org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-tmux ob-go noctilux-theme nginx-mode naquadah-theme nameless mwim mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-gitflow madhat2r-theme lush-theme lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme kaolin-themes json-navigator js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme groovy-mode grandshell-theme gotham-theme google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc go-dlv gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ ghub gh-md gandalf-theme fuzzy font-lock+ flycheck-rust flycheck-pos-tip flycheck-package flycheck-gometalinter flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator feature-mode farmhouse-theme fancy-narrow fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-vimish-fold evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme emmet-mode elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline dockerfile-mode docker django-theme diminish diff-hl demo-it define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme counsel-projectile company-web company-terraform company-tern company-statistics company-shell company-quickhelp company-lua company-go company-anaconda command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme closql clean-aindent-mode chruby cherry-blossom-theme centered-cursor-mode cargo busybee-theme bundler bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((noeval sql-connect "hasura"
             (concat "*SQL: postgres:data*"))
     (sql-postgres-options
      ("-P" "pager=off" "--no-password"))
     (nextcloud-passwd . set-password-dynamically)
     (nextcloud-login . hh)
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (go-backend . go-mode)
     (go-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
