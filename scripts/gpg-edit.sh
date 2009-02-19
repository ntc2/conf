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

in="$1"
t=$(tempfile -d /tmp)
trap "rm -f $t" EXIT # "finally"

gpg --yes -o $t $in && \
$edit $t && \
gpg --yes -aco $in $t
