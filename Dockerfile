FROM ubuntu:20.04
# Software Versions
ENV KUBEMACS_VERSION=0.0.2 \
  EMACS_VERSION=26.3 \
  DOCKER_VERSION=19.03.5 \
  KIND_VERSION=0.8.1 \
  KUBECTL_VERSION=1.18.3 \
  GO_VERSION=1.13.6 \
  TILT_VERSION=0.15.0 \
  TMATE_VERSION=2.4.0 \
  BAZEL_VERSION=2.2.0 \
  HELM_VERSION=2.16.9 \
# GOLANG, path vars
  GOROOT=/usr/local/go \
  PATH="$PATH:/usr/local/go/bin" \
# These vars ensure that emacs loads kubemacs before all else
# Note the : following the KUBEMACS_CONFIGDIR in EMACSLOADPATH
  KUBEMACS_CONFIGDIR=/var/local/kubemacs.d \
  EMACSLOADPATH=/var/local/kubemacs.d:

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
  libcap2-bin \
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

RUN curl -L https://get.helm.sh/helm-v2.16.9-linux-amd64.tar.gz | tar --directory /usr/local/bin --extract -xz --strip-components 1 linux-amd64/helm

RUN curl -L -o /usr/local/bin/bazel https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-linux-x86_64 && \
  chmod +x /usr/local/bin/bazel && bazel

# Major dependencies (nodejs + postgres come from upstream apt repos)
RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive \
  apt-get install --no-install-recommends -y \
  git \
  openssh-client \
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
  build-essential \
  vim \
  rsync \
  unzip \
  file \
  bash-completion \
  less \
  iputils-ping \
  dnsutils \
  tree \
  bash-completion \
  && rm -rf /var/apt/lists/*

RUN /bin/env GO111MODULE=on GOPATH=/usr/local/go /usr/local/go/bin/go get golang.org/x/tools/gopls@latest
RUN /bin/env GO111MODULE=on GOPATH=/usr/local/go /usr/local/go/bin/go get -u github.com/stamblerre/gocode

# # ensure that Python3 is default
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

RUN mkdir -p /usr/share/kubemacs
ADD kind-cluster-config.yaml kind-cluster+registry.yaml kustomization.yaml /usr/share/kubemacs/
ADD manifests /usr/share/kubemacs/manifests
ADD manifests/nginx-ingress /usr/share/kubemacs/manifests/nginx-ingress
ADD manifests/tmate /usr/share/kubemacs/manifests/tmate
ADD kustomization.yaml /usr/share/kubemacs/manifests

# Ideally we used a checkout of this repo, but I'm having trouble with the build + submodules
# RUN git clone --depth 1 --recursive https://github.com/kubemacs/kubemacs /var/local/kubemacs.d
ADD --chown=root:users *.el $KUBEMACS_CONFIGDIR/
ADD --chown=root:users banners $KUBEMACS_CONFIGDIR/banners
ADD --chown=root:users layers $KUBEMACS_CONFIGDIR/layers
ADD --chown=root:users snippets $KUBEMACS_CONFIGDIR/snippets
ADD --chown=root:users spacemacs $KUBEMACS_CONFIGDIR/spacemacs
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
ADD profile.d-iitoolbox.sh /etc/profile.d/iitoolbox.sh
RUN mkdir -p /etc/sudoers.d && \
  echo "%sudo    ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/sudo

# RUN groupadd -g 999 docker
# RUN groupdel docker && \
#   groupadd -g 999 docker
# From here on out we setup the user
COPY homedir/* /etc/skel/
RUN mkdir -p /etc/skel/.ssh
COPY known_hosts /etc/skel/.ssh/
COPY kubeconfig /etc/skel/.kube/config
RUN chmod 0600 /etc/skel/.pgpass
RUN groupadd -g 1000 standard && \
  useradd -m -G sudo,users,standard -s /bin/bash -u 2000 ii

COPY bin/* /usr/local/bin/

USER ii

# ENTRYPOINT ["/bin/bash"]
CMD ["simple-init.sh"]
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=5 \
  CMD ["tmate", "-S ", "/tmp/ii.default.target.iisocket", "wait-for", "tmate-ready"] || exit 1
