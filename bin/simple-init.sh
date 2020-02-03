#!/bin/bash
set -x
cd /home/ii
if [ ! -f ".ssh/id_rsa" ]
then
    ssh-keygen -b 4096 -t rsa -f ~/.ssh/id_rsa -q -N ""
fi
kubectl config set-context $(kubectl config current-context) --namespace=$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)
export ALTERNATE_EDITOR=""
export TMATE_SOCKET="/tmp/ii.default.target.iisocket"
export TMATE_SOCKET_NAME=`basename ${TMATE_SOCKET}`
# This background process will ensure tmate attache commands
# call osc52-tmate.sh to set the ssh/web uri for this session via osc52
# But it wait's until the socket exists, and tmate is ready for commands
(
    if [ ! -f "$TMATE_SOCKET" ]
    then
        # wait for socket to appear
        while read i; do
            if [ "$i" = $TMATE_SOCKET_NAME ]; then break; fi
            echo "XXXXX${i}YYYYYYYY"
        done \
            < <(inotifywait  -e create,open --format '%f' --quiet /tmp --monitor)
    fi
    tmate -S $TMATE_SOCKET wait tmate-ready
    tmate -S $TMATE_SOCKET set-hook -ug client-attached
    tmate -S $TMATE_SOCKET set-hook -g client-attached 'run-shell "tmate new-window osc52-tmate.sh"'
)&
tmate -F -v -S $TMATE_SOCKET new-session -d -c ~/apisnoop emacsclient --tty .
# tmate new-session \
#       -A -s ii -n emacs \
#       "tmate wait tmate-ready \
# && TMATE_CONNECT=\
# \$(tmate display -p '#{tmate_ssh} # $(date) # #{tmate_web}') \
# ; echo \$TMATE_CONNECT \
# ; (echo \$TMATE_CONNECT | osc52.sh ) \
# ; echo Share the above with your friends and hit enter here when done? \
# ; read ; \
# emacsclient --tty . 2>&1"
