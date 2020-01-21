#!/bin/bash
set -x
set -e
_FILENAME=$(basename "$1")
if [ ! -n "$_FILENAME" ]; then
    _FILENAME="default"
fi
export ALTERNATE_EDITOR=""
export EMACSSOCKETNAME=${_FILENAME}
export II_SESSION=${USER}.${EMACSSOCKETNAME}
OSC52E=$(dirname "${BASH_SOURCE[0]}")/osc52.sh
xtermcontrol --title $USER.$EMACSSOCKETNAME@sharing.io
echo $OSC52E
# ensure this TMATE starts shell with BASE=$BASE
tmate -S /tmp/${II_SESSION}.iisocket new-session \
      -A -s $USER -n emacs \
      "tmate wait tmate-ready \
&& TMATE_CONNECT=\
\$(tmate display -p '#{tmate_ssh} # ${II_SESSION} # $(date) # #{tmate_web}') \
; echo \$TMATE_CONNECT \
; (echo \$TMATE_CONNECT | $OSC52E ) \
; echo Share the above with your friends and hit enter here when done? \
; read ; \
emacsclient -s $EMACSSOCKETNAME --tty $1 2>&1"
