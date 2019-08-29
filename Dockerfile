FROM ubuntu:19.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update &&\
    apt-get upgrade -y
RUN apt-get install -y software-properties-common \
                       git \
                       iputils-ping \
                       curl \
                       locales \
                       tmate \
                       xclip \
                       tmux  \
                       net-tools \
                       less

RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG en_US.utf8

RUN cd /tmp &&\
    curl -L -O https://github.com/jgm/pandoc/releases/download/2.7.3/pandoc-2.7.3-1-amd64.deb &&\
    dpkg -i /tmp/pandoc-2.7.3-1-amd64.deb

RUN add-apt-repository ppa:kelleyk/emacs && apt-get install -y emacs26

RUN git clone --depth 1 --recurse-submodules \
        https://github.com/iimacs/site-lisp \
        /usr/local/share/emacs/site-lisp

RUN emacs --batch -l /usr/local/share/emacs/site-lisp/default.el

RUN groupadd ii &&\
    useradd -g ii -d /home/ii -s /bin/bash -c "ii" ii

RUN rm -rf /var/lib/apt/lists/*

RUN mkdir /home/ii
COPY ii /home/ii/

ENV HOME=/home/ii

RUN chown -R ii:ii /home/ii
USER ii

ENTRYPOINT ["/bin/bash"]
