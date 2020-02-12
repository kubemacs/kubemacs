FROM ubuntu:eoan-20200114
# Software Versions
ENV KUBEMACS_VERSION=0.0.2 \
  EMACS_VERSION=26.3 \
  DOCKER_VERSION=19.03.5 \
  KIND_VERSION=0.7.0 \
  KUBECTL_VERSION=1.17.2 \
  GO_VERSION=1.13.6 \
  TILT_VERSION=0.12.0 \
  TMATE_VERSION=2.4.0
# GOLANG, path vars
ENV GOROOT=/usr/local/go \
  PATH="$PATH:/usr/local/go/bin"
# Setup Postgresql Upstream REPO - Google Cloud SDK REPO
# Postgres client Related vars
ENV PGUSER=apisnoop \
  PGDATABASE=apisnoop \
  PGHOST=postgres \
  PGPORT=5432 \
  TZ="Pacific/Auckland"
# These vars ensure that emacs loads kubemacs before all else
# Note the : following the KUBEMACS_CONFIGDIR in EMACSLOADPATH
ENV KUBEMACS_CONFIGDIR=/var/local/kubemacs.d
ENV EMACSLOADPATH=$KUBEMACS_CONFIGDIR:
# Node 12, Postgres (pgdg), and Google Cloud SDK are currently available here:
COPY apt/*.list /etc/apt/sources.list.d/
# Ensure the keyfile for each repo above is available
COPY apt/*.gpg /etc/apt/trusted.gpg.d/

# Install emacs (26.3) and curl (to install other binaries)
# Note that ubuntu:eoan defaults to 26.3
# ca-certificates are needed to CA updates for download.docker.com and others
# xz-utils, for tmate tar compression
RUN DEBIAN_FRONTEND=noninteractive \
  apt-get update \
  && apt-get upgrade -y \
  && apt-get install --no-install-recommends -y \
  emacs-nox \
  xz-utils \
  sudo \
  curl \
  ca-certificates \
  && rm -rf /var/apt/lists/*

# docker client binary
RUN curl -fsSL https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}.tgz \
  | tar --directory=/usr/local/bin --extract --ungzip \
  --strip-components=1 docker/docker

# kind binary
RUN curl -Lo /usr/local/bin/kind \
  https://github.com/kubernetes-sigs/kind/releases/download/v${KIND_VERSION}/kind-$(uname)-amd64 \
  && chmod +x /usr/local/bin/kind

# kubectl binary
RUN curl -L https://storage.googleapis.com/kubernetes-release/release/v${KUBECTL_VERSION}/bin/linux/amd64/kubectl \
  -o /usr/local/bin/kubectl \
  && chmod +x /usr/local/bin/kubectl

# tilt binary
RUN curl -fsSL \
  https://github.com/windmilleng/tilt/releases/download/v${TILT_VERSION}/tilt.${TILT_VERSION}.linux.x86_64.tar.gz \
  | tar --directory /usr/local/bin --extract --ungzip tilt

# We stopped using a shell script, and added apt list+gpg files
# # install nodejs
# RUN curl -sL https://deb.nodesource.com/setup_${NODE_VERSION}.x | bash - && \
#   DEBIAN_FRONTEND=noninteractive apt-get install -y nodejs

# install golang
RUN cd /tmp && \
  curl -L https://dl.google.com/go/go${GO_VERSION}.linux-amd64.tar.gz \
  | tar --directory /usr/local --extract --ungzip

# tmate allows others to connect to your session
# they support using self hosted / though we default to using their hosted service
RUN curl -L \
  https://github.com/tmate-io/tmate/releases/download/${TMATE_VERSION}/tmate-${TMATE_VERSION}-static-linux-amd64.tar.xz \
  | tar --directory /usr/local/bin --extract --xz \
  --strip-components 1 tmate-${TMATE_VERSION}-static-linux-amd64/tmate

# Major dependencies (nodejs + postgres come from upstream apt repos)
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive \
  apt-get install --no-install-recommends -y \
  git \
  openssh-client \
  nodejs \
  postgresql-client-12 \
  jq \
  inotify-tools \
  xtermcontrol \
  nodejs \
  gnupg2 \
  tzdata \
  wget \
  python3-dev \
  xz-utils \
  apache2-utils \
  sqlite3 \
  silversearcher-ag \
  && rm -rf /var/apt/lists/*

# # Known dependencies
# RUN apt-get update \
#   && DEBIAN_FRONTEND=noninteractive \
#   apt-get install --no-install-recommends -y \
#   acl \
#   apt-file \
#   apt-utils \
#   build-essential \
#   ripgrep \
#   psmisc \
#   rsync \
#   software-properties-common \
#   sudo \
#   vim \
#   zsh \
#   && rm -rf /var/apt/lists/*
#   # apt-transport-https \

# gopls, gocode, and others needed for dev will install into /usr/local/bin
# RUN GOPATH=/usr/local \
#   go get -u -v github.com/nsf/gocode \
#   && GOPATH=/usr/local \
#   go get -u -v golang.org/x/tools/... \
#   && rm -rf /root/.cache /usr/local/pkg /usr/local/src


# # ensure that Python3 is default
# RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1 && \
#     update-alternatives --install /usr/bin/python python /usr/bin/python3 2

RUN mkdir -p /usr/share/kubemacs
ADD kind-cluster-config.yaml /usr/share/kubemacs/

RUN git clone --depth 1 https://github.com/cncf/apisnoop /usr/share/kubemacs/apisnoop

# Ideally we used a checkout of this repo, but I'm having trouble with the build + submodules
RUN git clone --depth 1 --recursive https://github.com/kubemacs/kubemacs /var/local/kubemacs.d
# TODO I'm unsure how to clone recusively during cloud-build
#RUN mkdir -p $KUBEMACS_CONFIGDIR
# The interesting/configuration parts of iimacs/kubemacs need to be in $EMACSLOADPATH
# COPY init.el site-start.el banners snippets layers spacemacs $KUBEMACS_CONFIGDIR/
# TODO This cache of compiled .elc files should be part of the build cache at some point
#  TARFILE=kubemacs-cache-0.9.32.tgz ; kubectl exec kubemacs-0 -- tar --directory /var/local/iimacs.d --create  --gzip --file - spacemacs/elpa/26.3 > $TARFILE ; gsutil cp $TARFILE gs://kubemacs/cache/$TARFILE
RUN curl https://storage.googleapis.com/apisnoop/dev/kubemacs-cache-0.9.32.tgz \
  | tar xfzC - $KUBEMACS_CONFIGDIR \
  && chgrp -R users $KUBEMACS_CONFIGDIR \
  && chmod -R g+w $KUBEMACS_CONFIGDIR

# we use osc52 support to copy text back to your OS over kubectl exec tmate
COPY bin/* /usr/local/bin/
ADD profile.d-iitoolbox.sh /etc/profile.d/iitoolbox.sh
RUN mkdir -p /etc/sudoers.d && \
  echo "%sudo    ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/sudo

# RUN groupadd -g 999 docker
# RUN groupdel docker && \
#   groupadd -g 999 docker
# From here on out we setup the user
COPY homedir/* /etc/skel/
COPY kubeconfig /etc/skel/.kube/config
RUN chmod 0600 /etc/skel/.pgpass
RUN useradd -m -G sudo,users -s /bin/bash -u 2000 ii
USER ii

# # Fetch Golang dependencies for development
# RUN mkdir -p ~/go/src/k8s.io && git clone https://github.com/kubernetes/kubernetes.git ~/go/src/k8s.io/kubernetes
# RUN cd ~/go/src/k8s.io/kubernetes ; go mod download
#  go get -u -v ...
# RUN go get -u -v k8s.io/apimachinery/pkg/apis/meta/v1
# ENV GO111MODULE=on
# RUN go get -u -v k8s.io/client-go/kubernetes@v0.17.0
# RUN go get -u -v k8s.io/client-go/tools/clientcmd@v0.17.0
# RUN git clone --depth 1 https://github.com/cncf/apisnoop /home/ii/apisnoop
# RUN cd /home/ii/apisnoop/org/tickets ; go mod download

# ENTRYPOINT ["/bin/bash"]
CMD ["simple-init.sh"]
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=5 \
  CMD ["tmate", "-S ", "/tmp/ii.default.target.iisocket", "wait-for", "tmate-ready"] || exit 1
# ca-certificates need to be updated to connect to https://download.docker.com
# RUN apt-get update \
#   && DEBIAN_FRONTEND=noninteractive \
#   apt-get install --no-install-recommends -y \
#   ca-certificates \
#   && rm -rf /var/apt/lists/*

# We need the docker client binary ... do it directly instead
# RUN apt-get update \
#   && DEBIAN_FRONTEND=noninteractive \
#   apt-get install --no-install-recommends -y \
#   docker docker.io \
#   && rm -rf /var/apt/lists/*

  # Installing just kubectl insteaf of everything from google-cloud-sdk
  # but still enabling the repo should someone need it later
  # kubectl \
  # google-cloud-sdk \
