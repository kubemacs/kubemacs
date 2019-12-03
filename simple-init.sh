#!/bin/bash

set -x

cd /home/ii
if [ ! -f "~/.ssh/id_rsa" ]
then
    ssh-keygen -b 4096 -t rsa -f ~/.ssh/id_rsa -q -N ""
fi
xtermcontrol --title us@iimacs.org
export ALTERNATE_EDITOR=""
OSC52E=/usr/local/bin/osc52.sh
tmate -S /tmp/left.iisocket new-session \
      -A -s ii -n emacs \
      "tmate wait tmate-ready \
&& TMATE_CONNECT=\
\$(tmate display -p '#{tmate_ssh} # $(date) # #{tmate_web}') \
; echo \$TMATE_CONNECT \
; (echo \$TMATE_CONNECT | $OSC52E ) \
; echo Share the above with your friends and hit enter here when done? \
; read ; \
emacsclient --tty . 2>&1"
