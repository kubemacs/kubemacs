- [Dependencies](#sec-1)
  - [Ubuntu 19.10 (or similar)](#sec-1-1)
  - [emacs 26.X](#sec-1-2)
  - [kubectl / google-cloud-sdk](#sec-1-3)
  - [golang 1.13](#sec-1-4)
  - [kind 0.5.1](#sec-1-5)
- [Get your terminal + emacs working](#sec-2)
  - [clone down iimacs .emacs.d into your system](#sec-2-1)
  - [populate the cache](#sec-2-2)
  - [ensure when you login, emacs can find .iimacs.d and tooling](#sec-2-3)
  - [xterm font size and OS Code settings](#sec-2-4)
- [Possibly use ~/.emacs.d/.spacemacs-hh](#sec-3)
- [Usage](#sec-4)
  - [run xterm](#sec-4-1)


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

# Get your terminal + emacs working<a id="sec-2"></a>

## clone down iimacs .emacs.d into your system<a id="sec-2-1"></a>

```shell
cd ~/ # do as your own user
# mv ~/.emacs.d ~/.emacs.d.before-ii
git clone --recursive https://github.com/iimacs/.emacs.d ~/.iimacs.d
```

## populate the cache<a id="sec-2-2"></a>

```shell
curl https://storage.googleapis.com/apisnoop/dev/iitoolbox-spacemacs-0.6.tgz \
    | tar xzfC - ~/.iimacs.d
```

## ensure when you login, emacs can find .iimacs.d and tooling<a id="sec-2-3"></a>

This is done for all users, but has no impact unless ~/.iimacs.d exists

```shell
cat <<EOF | sudo tee /etc/profile.d/99-iimacs.sh
# Ensures the iitooling is avaliable and loaded by emacs
export IIMACS=~/.iimacs.d
export PATH=${IIMACS}/bin:${PATH}
export EMACSLOADPATH=${IIMACS}:
EOF
```

## xterm font size and OS Code settings<a id="sec-2-4"></a>

```shell
cp .emacs.d/.xterm-xdefaults ~/.Xdefaults
# edit the above to your preferences ^^^
# In particular the DPI is set to 180 for hidpi, 90 is good for older screens
xrdb ~/.Xdefaults
```

# Possibly use ~/.emacs.d/.spacemacs-hh<a id="sec-3"></a>

You can have your own config, but I do a lot of work to keep mine happy. It's not a bad starting spot.

```shell
cp ~/.emacs.d/.spacemacs-hh ~/.emacs.d/.spacemacs-$USER
```

# Usage<a id="sec-4"></a>

```shell
iimacs ~/path/to/orgfile
```

## run xterm<a id="sec-4-1"></a>

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
