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
        echo "error: you must set EDITOR in your environment or have \
a program called 'editor' on your path" >&2
        exit 1
    fi
fi

# tempfile permissions
name=$(basename $0)
if [[ $name == gpg-edit.sh ]]; then
  mode=edit
elif [[ $name == gpg-view.sh ]]; then
  mode=view
else
  echo "error: unknown usage mode" >&2
  exit 1
fi
# set up tempfiles for auto deletion
in="$1"
mkdir -p ~/tmp
t=$(mktemp ~/tmp/tmp.XXX)
pw=$(mktemp ~/tmp/tmp.XXX)
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

# view file and maybe edit and save
gpg --yes --batch  -o $t --passphrase-file $pw $in && \
if [[ $mode != edit ]]; then
  chmod 0400 $t
fi && \
$edit $t && \
if [[ $mode == edit ]]; then
  gpg --yes --batch -aco $in --passphrase-file $pw $t
fi
