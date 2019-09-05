FROM iibase

USER root
RUN add-apt-repository ppa:kelleyk/emacs && apt-get install -y emacs26-nox &&\
    rm -rf /usr/share/emacs/26.2/site-lisp &&\
    rm -rf /usr/share/emacs/site-lisp &&\
    rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --recurse-submodules \
        https://github.com/iimacs/site-lisp \
        /usr/share/emacs/26.2/site-lisp &&\
    echo "Processing default.el"

RUN sed -i "s/(require 'ob-sql-mode)/;; (require 'ob-sql-mode)/" /usr/share/emacs/26.2/site-lisp/default.el &&\
    emacs --batch -l /usr/share/emacs/26.2/site-lisp/default.el

RUN chown -R ii:ii /home/ii
USER ii

ENTRYPOINT ["/bin/bash"]
