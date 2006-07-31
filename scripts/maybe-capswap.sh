#!/bin/bash

# Only add :$PATH if $PATH is non empty.
PATH=~/local/scripts${PATH:+:$PATH}

# Put caps lock in the right place.  I don't exactly understand what I
# have done here.  From looking at the output of xmodmap I know that
# 0x42 is what I don't want, and 0x25 is what I do want.  I am a
# little worried this is just checking if caps lock is actually caps
# lock, and switching it if it is.  So this is probably nonsense, in
# that it will break Sun keyboards that have control in the right
# place to begin with.

if [ $(xmodmap | grep lock | awk '{ print $3 }') = '(0x42)' ]; then
    echo Swapping control and caps lock
    capswap.sh
else
    echo Control and caps lock are properly mapped
fi
