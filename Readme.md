- [Dependencies](#sec-1)
  - [Ubuntu 19.10 (or similar)](#sec-1-1)
  - [emacs 26.X](#sec-1-2)
  - [kubectl / google-cloud-sdk](#sec-1-3)
  - [golang 1.13](#sec-1-4)
  - [kind 0.5.1](#sec-1-5)
  - [tmate 2.3.1](#sec-1-6)
  - [ripgrep 0.10.0](#sec-1-7)
  - [xterm](#sec-1-8)
  - [docker](#sec-1-9)
- [OS Specific](#sec-2)
  - [OSX](#sec-2-1)
    - [Installing brew](#sec-2-1-1)
    - [Installing emacs via brew](#sec-2-1-2)
    - [Ensuring the brew emacs version is found before the installed version](#sec-2-1-3)
- [Ensure github keys are working](#sec-3)
  - [generate a public<sub>private</sub> keypair](#sec-3-1)
- [Get your terminal + emacs working](#sec-4)
  - [clone down iimacs .emacs.d into your system](#sec-4-1)
  - [populate the cache](#sec-4-2)
  - [ensure when you login, emacs can find .iimacs.d and tooling](#sec-4-3)
  - [ensure when you login, emacs can find .iimacs.d and tooling](#sec-4-4)
  - [xterm font size and OS Code settings](#sec-4-5)
  - [tmate settings (including ESC timout)](#sec-4-6)
  - [docker group](#sec-4-7)
- [Possibly use ~/.emacs.d/.spacemacs-hh](#sec-5)
- [Usage](#sec-6)
  - [run xterm](#sec-6-1)


# Dependencies<a id="sec-1"></a>

## Ubuntu 19.10 (or similar)<a id="sec-1-1"></a>

## emacs 26.X<a id="sec-1-2"></a>

```shell
sudo apt install -y emacs #>26.0
```

## kubectl / google-cloud-sdk<a id="sec-1-3"></a>

```shell
echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" \
    | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg \
    | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
sudo apt update 
sudo apt install -y kubectl google-cloud-sdk
```

## golang 1.13<a id="sec-1-4"></a>

```shell
sudo add-apt-repository --yes ppa:longsleep/golang-backports
sudo apt update
sudo apt install -y golang golang-1.13
```

## kind 0.5.1<a id="sec-1-5"></a>

```shell
sudo curl -Lo /usr/local/bin/kind \
     https://github.com/kubernetes-sigs/kind/releases/download/v0.5.1/kind-$(uname)-amd64
sudo chmod +x /usr/local/bin/kind
```

## tmate 2.3.1<a id="sec-1-6"></a>

```shell
curl -L \
 https://github.com/tmate-io/tmate/releases/download/2.3.1/tmate-2.3.1-static-linux-amd64.tar.gz \
 | sudo tar xvfzC - /usr/local/bin --strip-components 1
```

## ripgrep 0.10.0<a id="sec-1-7"></a>

```shell
sudo apt install -y ripgrep
```

## xterm<a id="sec-1-8"></a>

```shell
sudo apt install -y xterm xtermcontrol
```

## docker<a id="sec-1-9"></a>

```shell
sudo apt-get remove docker docker-engine docker.io containerd runc
sudo apt-get install \
     apt-transport-https \
     ca-certificates \
     curl \
     gnupg-agent \
     software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
     "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   disco \
   stable"
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
#export DOCKER_VERSION=5:19.03.3~3-0~ubuntu-disco
#sudo apt-get install docker-ce=$DOCKER_VERISON docker-ce-cli=$DOCKER_VERSION containerd.io
```

# OS Specific<a id="sec-2"></a>

## OSX<a id="sec-2-1"></a>

### Installing brew<a id="sec-2-1-1"></a>

### Installing emacs via brew<a id="sec-2-1-2"></a>

### Ensuring the brew emacs version is found before the installed version<a id="sec-2-1-3"></a>

```shell
sudo ln -sf /usr/local/Cellar/emacs/26.3/bin/* /usr/local/bin/
```

# Ensure github keys are working<a id="sec-3"></a>

## generate a public<sub>private</sub> keypair<a id="sec-3-1"></a>

```shell
ssh-key
```

# Get your terminal + emacs working<a id="sec-4"></a>

## clone down iimacs .emacs.d into your system<a id="sec-4-1"></a>

```shell
cd ~/ # do as your own user
# mv ~/.emacs.d ~/.emacs.d.before-ii
git clone --recursive https://github.com/iimacs/.emacs.d ~/.iimacs.d
```

## populate the cache<a id="sec-4-2"></a>

```shell
curl https://storage.googleapis.com/apisnoop/dev/iitoolbox-spacemacs-0.6.tgz \
    | tar xzfC - ~/.iimacs.d
```

## ensure when you login, emacs can find .iimacs.d and tooling<a id="sec-4-3"></a>

This is done for all users, but has no impact unless ~/.iimacs.d exists

```shell
cat <<EOF | sudo tee /etc/profile.d/99-iimacs.sh
# Ensures the iitooling is avaliable and loaded by emacs
export IIMACS=~/.iimacs.d
export PATH=${IIMACS}/bin:${PATH}
export EMACSLOADPATH=${IIMACS}:
EOF
```

## ensure when you login, emacs can find .iimacs.d and tooling<a id="sec-4-4"></a>

This is done for all users, but has no impact unless ~/.iimacs.d exists

```shell
sudo adduser $USER docker
```

## xterm font size and OS Code settings<a id="sec-4-5"></a>

```shell
cp .emacs.d/.xterm-xdefaults ~/.Xdefaults
# edit the above to your preferences ^^^
# In particular the DPI is set to 180 for hidpi, 90 is good for older screens
xrdb ~/.Xdefaults
```

## tmate settings (including ESC timout)<a id="sec-4-6"></a>

```shell
cat <<EOF >> ~/.tmate.conf
set -s escape-time 0
set-option -g set-clipboard on
set-option -g mouse on
set-option -g history-limit 50000
EOF
```

## docker group<a id="sec-4-7"></a>

We need to login/logout in order for this to work OR use newgrp

```shell
sudo adduser $USER docker
```

# Possibly use ~/.emacs.d/.spacemacs-hh<a id="sec-5"></a>

You can have your own config, but I do a lot of work to keep mine happy. It's not a bad starting spot.

```shell
cp ~/.emacs.d/.spacemacs-hh ~/.emacs.d/.spacemacs-$USER
```

# Usage<a id="sec-6"></a>

```shell
iimacs ~/path/to/orgfile
```

## run xterm<a id="sec-6-1"></a>

```shell
#!/bin/bash
#/usr/local/bin/apisnoop.sh
xterm -T $USER@sharing.io -e \
    ssh -tA $USER@sharing.io bash -l \
    '~/ii/org/start_osc52_session.sh'  '~/ii/apisnoop/' &
```

```shell
#/usr/local/bin/conformance-testing.sh
#!/bin/bash
xterm -T hh@sharing.io/conformance-testing -e \
      ssh -tA $USER@sharing.io bash -l \
      '~/ii/org/start_osc52_session.sh'  '~/ii/conformance-testing/' &
```
