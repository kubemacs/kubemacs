# "iiorgmacs" Add "Spacemacs" layer, supporting files and "ii" user
# Version 0.1 Sept 2019

FROM iimacs/base

ENV IIMACSVERSION=0.9.0 \
    EMACSLOADPATH=/var/local/iimacs.d:

RUN useradd -m -G sudo,users -s /bin/bash -u 2000 ii

ADD profile.d-iitoolbox.sh /etc/profile.d/iitoolbox.sh
ADD simple-init.sh /usr/local/bin/simple-init.sh

RUN mkdir -p /etc/sudoers.d && \
    echo "%sudo    ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/sudo

# install some useful packages
RUN apt update && \
    apt upgrade -y && \
    DEBIAN_FRONTEND=noninteractive apt install -y sudo wget acl docker docker-compose apt-transport-https build-essential zsh sqlite3 vim nano apt-utils rsync xterm postgresql-client mariadb-client inotify-tools jq python3-pip xtermcontrol tzdata apt-file

# install Kubernetes client and Google Cloud SDK
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
	curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - && \
	apt update && \
	apt install -y kubectl google-cloud-sdk

# install golang
RUN add-apt-repository --yes ppa:longsleep/golang-backports && \
	apt update && \
	apt install -y golang golang-1.13

# install nodejs
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash - && \
    apt-get install -y nodejs

# ensure that Python3 is default
RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1 && \
    update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# k8s kind
RUN curl -Lo /usr/local/bin/kind https://github.com/kubernetes-sigs/kind/releases/download/v0.7.0/kind-$(uname)-amd64 && \
	chmod +x /usr/local/bin/kind

# get ssh-find-agent
RUN curl https://gitlab.ii.coop/ii/tooling/ssh-find-agent/raw/master/ssh-find-agent.sh > /usr/local/bin/ssh-find-agent.sh && \
	chmod +x /usr/local/bin/ssh-find-agent.sh

RUN git clone --depth 1 --recursive https://github.com/iimacs/.emacs.d /var/local/iimacs.d && \
    cd /var/local/iimacs.d && \
    curl https://storage.googleapis.com/apisnoop/dev/iitoolbox-spacemacs-0.6.tgz | tar xzfC - /var/local/iimacs.d

RUN chgrp -R users /var/local/iimacs.d && \
    chmod -R g+w /var/local/iimacs.d

COPY osc52.sh /usr/local/bin/osc52.sh

USER ii

COPY .iimacs /home/ii

RUN go get -u -v k8s.io/apimachinery/pkg/apis/meta/v1 && \
  go get -u -v k8s.io/client-go/kubernetes && \
  go get -u -v k8s.io/client-go/tools/clientcmd && \
  go get -u -v github.com/nsf/gocode && \
  go get -u -v golang.org/x/tools/...

ENTRYPOINT ["/bin/bash"]
