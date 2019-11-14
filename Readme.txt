                        ━━━━━━━━━━━━━━━━━━━━━━━━
                         IIMACS (PEOPLE MACROS)

                             Hippie Hacker
                        ━━━━━━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1. Dependencies
.. 1. Ubuntu 19.10 (or similar)
.. 2. emacs 26.X
.. 3. kubectl / google-cloud-sdk
.. 4. golang 1.13
.. 5. kind 0.5.1
2. Get your terminal + emacs working
.. 1. clone down iimacs .emacs.d into your system
.. 2. populate the cache
.. 3. ensure when you login, emacs can find .iimacs.d and tooling
.. 4. xterm font size and OS Code settings
3. Possibly use ~/.emacs.d/.spacemacs-hh
4. Usage
.. 1. run xterm


1 Dependencies
══════════════

1.1 Ubuntu 19.10 (or similar)
─────────────────────────────


1.2 emacs 26.X
──────────────

  ┌────
  │ apt install -y emacs #>26.0
  └────


1.3 kubectl / google-cloud-sdk
──────────────────────────────

  ┌────
  │ echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" \
  │     | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
  │ curl https://packages.cloud.google.com/apt/doc/apt-key.gpg \
  │     | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
  │ apt update 
  │ apt install -y kubectl google-cloud-sdk
  └────


1.4 golang 1.13
───────────────

  ┌────
  │ add-apt-repository --yes ppa:longsleep/golang-backports
  │ apt update
  │ apt install -y golang golang-1.13
  └────


1.5 kind 0.5.1
──────────────

  ┌────
  │ curl -Lo /usr/local/bin/kind \
  │      https://github.com/kubernetes-sigs/kind/releases/download/v0.5.1/kind-$(uname)-amd64
  │ chmod +x /usr/local/bin/kind
  └────


2 Get your terminal + emacs working
═══════════════════════════════════

2.1 clone down iimacs .emacs.d into your system
───────────────────────────────────────────────

  ┌────
  │ cd ~/
  │ # mv ~/.emacs.d ~/.emacs.d.before-ii
  │ git clone --recursive https://github.com/iimacs/.emacs.d ~/.iimacs.d
  └────


2.2 populate the cache
──────────────────────

  ┌────
  │ curl https://storage.googleapis.com/apisnoop/dev/iitoolbox-spacemacs-0.6.tgz \
  │     | tar xzfC - ~/.iimacs.d
  └────


2.3 ensure when you login, emacs can find .iimacs.d and tooling
───────────────────────────────────────────────────────────────

  ┌────
  │ cat <<EOF | sudo tee /etc/profile.d/99-iimacs.sh
  │ # Ensures the iitooling is avaliable and loaded by emacs
  │ export IIMACS=~/.iimacs.d
  │ export PATH=${IIMACS}/bin:${PATH}
  │ export EMACSLOADPATH=${IIMACS}:
  │ EOF
  └────


2.4 xterm font size and OS Code settings
────────────────────────────────────────

  ┌────
  │ cp .emacs.d/.xterm-xdefaults ~/.Xdefaults
  │ # edit the above to your preferences ^^^
  │ # In particular the DPI is set to 180 for hidpi, 90 is good for older screens
  │ xrdb ~/.Xdefaults
  └────


3 Possibly use ~/.emacs.d/.spacemacs-hh
═══════════════════════════════════════

  You can have your own config, but I do a lot of work to keep mine
  happy. It's not a bad starting spot.
  ┌────
  │ cp ~/.emacs.d/.spacemacs-hh ~/.emacs.d/.spacemacs-$USER
  └────


4 Usage
═══════

  ┌────
  │ iimacs ~/path/to/orgfile
  └────


4.1 run xterm
─────────────

  ┌────
  │ #!/bin/bash
  │ xterm -T $USER@sharing.io -e \
  │     ssh -tA $USER@sharing.io bash -l \
  │     '~/ii/org/start_osc52_session.sh'  '~/ii/apisnoop/' &
  └────

  ┌────
  │ #!/bin/bash
  │ xterm -T hh@sharing.io/conformance-testing -e \
  │       ssh -tA $USER@sharing.io bash -l \
  │       '~/ii/org/start_osc52_session.sh'  '~/ii/conformance-testing/' &
  └────
