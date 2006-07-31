#!/bin/bash

# shamelessly lifted from the xmodmap man page.
# run as input file for xmodmap to make the "caps lock" key
# behave correctly.

#
# Swap Caps_Lock and Control_L
#
modmap="
remove Lock = Caps_Lock
remove Control = Control_L
keysym Control_L = Caps_Lock
keysym Caps_Lock = Control_L
add Lock = Caps_Lock
add Control = Control_L
"
xmodmap <(echo "$modmap")
