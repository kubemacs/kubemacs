# "iimacs" Add "Spacemacs" layer, supporting files and "ii" user
# Version 0.2 Jan 2020

FROM ubuntu:eoan-20200114

# We need gpupg for apt-key installation to work
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y \
  tzdata \
  wget \
  curl \
  gnupg2 \
  && rm -rf /var/apt/lists/*

# install Kubernetes client and Google Cloud SDK REPO
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" \
  | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
RUN wget --quiet -O - https://packages.cloud.google.com/apt/doc/apt-key.gpg \
  | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

# install postgresql repo to later grab postgresql-client-12
# postgresql-client-12 to connect to the db
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ eoan-pgdg main" \
  |  tee -a /etc/apt/sources.list.d/postgresql.list
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc \
  | apt-key add -

# Install from the upstream repos
  RUN DEBIAN_FRONTEND=noninteractive \
  apt-get update \
  && apt-get upgrade -y \
  && apt-get install --no-install-recommends -y \
  kubectl google-cloud-sdk \
  postgresql-client-12 \
  && rm -rf /var/apt/lists/*

# Our primary tooling layer
RUN DEBIAN_FRONTEND=noninteractive \
  apt-get update \
  && DEBIAN_FRONTEND=noninteractive \
  apt-get install --no-install-recommends -y \
  emacs-nox \
  docker docker.io \
  inotify-tools \
  jq \
  xtermcontrol \
  && rm -rf /var/apt/lists/*

# Secondary tooling layer
RUN DEBIAN_FRONTEND=noninteractive \
  apt-get update \
  && DEBIAN_FRONTEND=noninteractive \
  apt-get install --no-install-recommends -y \
  apache2-utils \
  git \
  sqlite3 \
  zsh \
  vim \
  rsync \
  acl \
  apt-file \
  apt-transport-https \
  apt-utils \
  build-essential \
  software-properties-common \
  silversearcher-ag \
  sudo \
  ripgrep \
  psmisc \
  && rm -rf /var/apt/lists/*

# install golang
RUN cd /tmp && \
  wget --quiet -O - https://dl.google.com/go/go1.13.4.linux-amd64.tar.gz \
  | tar -C /usr/local -xvz

ENV GOROOT=/usr/local/go \
  PATH=$PATH:/usr/local/go/bin
# gopls, gocode, and others needed for dev will install into /usr/local/bin
RUN GOPATH=/usr/local go get -u -v github.com/nsf/gocode && rm -rf /root/.cache /usr/local/pkg /usr/local/src
RUN GOPATH=/usr/local go get -u -v golang.org/x/tools/... && rm -rf /root/.cache /usr/local/pkg /usr/local/src

# We used tilt
RUN curl -fsSL "https://github.com/windmilleng/tilt/releases/download/v0.11.3/tilt.0.11.3.linux.x86_64.tar.gz" \
  | tar -C /usr/local/bin -xzv tilt

# install nodejs
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash - && \
   DEBIAN_FRONTEND=noninteractive apt-get install -y nodejs

# ensure that Python3 is default
RUN update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1 && \
    update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# k8s kind
RUN curl -Lo /usr/local/bin/kind \
  https://github.com/kubernetes-sigs/kind/releases/download/v0.7.0/kind-$(uname)-amd64 \
  && chmod +x /usr/local/bin/kind

# tmate allows others to connect to your session
# they support using self hosted / though we default to using their hosted service
RUN curl -L \
  https://github.com/tmate-io/tmate/releases/download/2.4.0/tmate-2.4.0-static-linux-amd64.tar.xz \
  | tar xvJ -f - --strip-components 1  -C /usr/local/bin tmate-2.4.0-static-linux-amd64/tmate

# This var ensures that emacs loads iimacs before all else
ENV KUBEMACS_CONFIGDIR=/var/local/kubemacs.d
# Note the : following the KUBEMACS_CONFIGDIR in EMACSLOADPATH
ENV IIMACSVERSION=0.9.34 \
  EMACSLOADPATH=$KUBEMACS_CONFIGDIR:
# This used to exist in it's own repo
# RUN git clone --depth 1 --recursive https://github.com/iimacs/.emacs.d /var/local/iimacs.d
RUN mkdir -4 $KUBEMACS_CONFIGDIR
# The interesting/configuration parts of iimacs/kubemacs need to be in $EMACSLOADPATH
COPY init.el site-start.el banners snippets layers spacemacs
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
RUN useradd -m -G sudo,users,docker -s /bin/bash -u 2000 ii
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

# Ensure authentication to apisnoop postgres database
ENV PGUSER=apisnoop \
  PGDATABASE=apisnoop \
  PGHOST=postgres \
  PGPORT=5432
ENV TZ="Pacific/Auckland"
ENTRYPOINT ["/bin/bash"]
CMD ["simple-init.sh"]
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=5 \
  CMD ["tmate", "-S ", "/tmp/ii.default.target.iisocket", "wait-for", "tmate-ready"] || exit 1
