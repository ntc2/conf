#!/bin/bash

# Use with
#
#   command="~/local/scripts/lpr-shell.sh",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty
#
# in a key entry in ~/.ssh/authorized_keys to limit that key to only
# be used for printing.
#
# See 'man sshd' for documentation of ~/.ssh/authorized_keys.
#
# Useful in conjunction with sshlpr:
# http://www.masella.name/technical/sshlpr.html

cmd="$SSH_ORIGINAL_COMMAND"

# Log all commands.
log=~/tmp/ssh-command-log.txt
date >> $log
echo "SSH_ORIGINAL_COMMAND=$cmd" >> $log

# Only run the command if it's a print command.
printcmd=$(echo "$cmd" \
  | grep --max-count=1 -E '^lpr (-o [- :_=0-9a-zA-Z]+)* -P [0-9a-zA-Z]+ -#[0-9]+$')
if [[ -z "$printcmd" ]]; then
    echo "Invalid print command: $cmd"
    exit 2
else
    $printcmd
fi