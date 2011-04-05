#!/bin/bash

# temporarily decrypt a gpg symmetric encrypted file, edit it in
# {$EDITOR || $(which editor)}, and then re-encrypt.  to encrypt a
# file the first time, before using this program, use
#
#   gpg -ac PLAIN_TEXT_FILE
#
# NB: if your {$EDITOR || $(which editor)} creates backup files, be
# sure to delete them after editing, since they'll contain the
# decrypted plaintext of the encrypted file. emacs doesn't have this
# problem because the tempfile used by this program is saved in /tmp,
# and emacs doesn't create backups when editing files in /tmp.  nano,
# the default {editor} on ubuntu is also ok.

if [[ $1 == -h || $# -ne 1 ]]; then
    echo "usage: $0 GPG_ENCRYPTED_FILE" >&2
    exit 1
fi

# attempt to choose an editor
edit="$EDITOR"
if [[ -z "$edit" ]]; then
    edit="$(which editor 2>/dev/null)"
    if [[ -z "$edit" ]]; then
        echo "you must set EDITOR in your environment or have \
a program called 'editor' on your path" >&2
        exit 1
    fi
fi

# set up tempfiles for auto deletion
in="$1"
t=$(tempfile -d /tmp -m 0600)
pw=$(tempfile -d /tmp -m 0600)
trap "rm -f $t $pw && stty echo" EXIT # "finally"

# read password
#
# man gpg says to avoid {--passphrase-file} if possible.  use of the
# similar {--passphrase-fd} is not discouraged, but i don't know how
# to get fds without files. concerns i'm aware of (not confident this
# is exhaustive ...) for {--passphrase-file}:
#
# * someone could read password file (permissions above mean only root can here)
# * size of password file reveals length of password (could avoid this
#   by appending random junk to password file, or storing password
#   file somewhere non-privileged users can't {ls -l})
stty -echo
echo -n "Enter passphrase: "
head -n 1 > $pw
echo
stty echo

# edit
gpg --yes --no-use-agent   -o $t --passphrase-file $pw $in && \
$edit $t && \
gpg --yes --no-use-agent -aco $in --passphrase-file $pw $t
