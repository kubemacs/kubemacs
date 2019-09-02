FROM iibase

USER root
RUN add-apt-repository ppa:kelleyk/emacs && apt-get install -y emacs26-nox

RUN git clone --depth 1 --recurse-submodules \
        https://github.com/iimacs/site-lisp \
        /usr/local/share/emacs/site-lisp

RUN sed -i "s/(require 'ob-sql-mode)/;; (require 'ob-sql-mode)/" /usr/local/share/emacs/site-lisp/default.el &&\
    emacs --batch -l /usr/local/share/emacs/site-lisp/default.el

RUN rm -rf /var/lib/apt/lists/*

RUN chown -R ii:ii /home/ii
USER ii

ENTRYPOINT ["/bin/bash"]
