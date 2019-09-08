# "iiorgmacs" Add "Spacemacs" layer, supporting files and "ii" user
# Version 0.1 Sept 2019

FROM iibase

RUN rm -rf /usr/local/share/emacs/site-lisp &&\
    git clone --depth 1 --recurse-submodules \
        https://github.com/iimacs/site-lisp \
        /usr/local/share/emacs/site-lisp

RUN sed -i "s/(require 'ob-sql-mode)/;; (require 'ob-sql-mode)/" /usr/local/share/emacs/site-lisp/default.el
RUN emacs --batch -l /usr/local/share/emacs/site-lisp/default.el

RUN groupadd ii &&\
    useradd -g ii -d /home/ii -s /bin/bash -c "ii" ii

RUN mkdir /home/ii
COPY ii /home/ii/
ENV HOME=/home/ii
RUN chown -R ii:ii /home/ii
USER ii

CMD ["/bin/bash"]
