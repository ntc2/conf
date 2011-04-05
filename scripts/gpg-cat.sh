#!/bin/bash

# decrypt a gpg encrypted file to the screen

if [[ $1 == -h || $# -ne 1 ]]; then
    echo "usage: $0 GPG_ENCRYPTED_FILE" >&2
    exit 1
fi

gpg --no-use-agent -o - $1
