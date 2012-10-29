#!/bin/bash

# Try again without '-font terminus' if the first version fails
/usr/bin/emacs -font terminus --no-splash --fullscreen --reverse-video --no-desktop "$@" \
|| /usr/bin/emacs --no-splash --fullscreen --reverse-video --no-desktop "$@"
