#!/bin/bash

# temporarily decrypt a gpg symmetric encrypted file, edit it in emacs, and then
# re-encrypt.
#
# To encrypt a file the first time, before using this program, use
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

# configure emacs with default setup, no graphics, and no backups.
edit='emacs -nw --eval "(setq make-backup-files nil)" -Q'
if ! which emacs &>/dev/null; then
  echo "error: no emacs!" >&2
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

# view file and maybe edit and save. i get an error about "end of file while
# parsing" if i don't run $edit $t inside a shell, since adding the {--eval ...}
# part to the edit command. it works fine when i paste it into the shell. no
# idea whats' going on???
gpg --yes --batch  -o $t --passphrase-file $pw $in && \
if [[ $mode != edit ]]; then
  chmod 0400 $t
fi && \
sh -c "$edit $t" && \
if [[ $mode == edit ]]; then
  gpg --yes --batch -aco $in --passphrase-file $pw $t
fi
