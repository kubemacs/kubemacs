# "iiorgmacs" Add "Spacemacs" layer, supporting files and "ii" user
# Version 0.1 Sept 2019

FROM iimacs/base

# install some useful packages
RUN apt update && \
    apt upgrade -y && \
    apt install -y sudo wget acl docker docker-compose apt-transport-https build-essential zsh sqlite3 vim nano apt-utils rsync xterm

# install Kubernetes client and Google Cloud SDK
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
	curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - && \
	apt update && \
	apt install -y kubectl google-cloud-sdk

# install golang
RUN add-apt-repository --yes ppa:longsleep/golang-backports && \
	apt update && \
	apt install -y golang golang-1.13

# k8s kind
RUN curl -Lo /usr/local/bin/kind https://github.com/kubernetes-sigs/kind/releases/download/v0.5.1/kind-$(uname)-amd64 && \
	chmod +x /usr/local/bin/kind

# get ssh-find-agent
RUN curl https://gitlab.ii.coop/ii/tooling/ssh-find-agent/raw/master/ssh-find-agent.sh > /usr/local/bin/ssh-find-agent.sh && \
	chmod +x /usr/local/bin/ssh-find-agent.sh

RUN git clone --recursive https://github.com/iimacs/.emacs.d /var/local/iimacs.d && \
    cd /var/local/iimacs.d && \
    curl https://storage.googleapis.com/apisnoop/dev/iitoolbox-spacemacs-0.6.tgz | tar xzfC - /var/local/iimacs.d

CMD ["/bin/bash"]
