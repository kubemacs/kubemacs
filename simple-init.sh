#!/bin/bash

set -x

cd /home/ii
if [ ! -f "~/.ssh/id_rsa" ]
then
    su ii -c 'ssh-keygen -b 4096 -t rsa -f ~/.ssh/id_rsa -q -N ""'
fi
su ii -c 'tmate'

