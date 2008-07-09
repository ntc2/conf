#!/bin/bash

# eval the output in a startup script to get the fixssh alias.

# Based on http://www.deadman.org/sshscreen.html.  Make a script
# fix-ssh.sh that exports environment variables needed to make ssh
# agent forwarded work.  This is useful in screen when you log out and
# back in from a different host (or after a reboot or other agent
# restart I think) and want to sync screen with your new agent vars.

for x in SSH_CLIENT SSH_TTY SSH_AUTH_SOCK SSH_CONNECTION DISPLAY; do
    eval echo export $x=\\\"\$$x\\\" # e.g. {export DISPLAY="$DISPLAY"}
done 1>$HOME/local/bin/fix-ssh.sh

alias fixssh="source $HOME/local/bin/fix-ssh.sh"
