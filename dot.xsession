#!/bin/bash
# Make the menu key (usually on the right symmetric to windows on the
# left) another windows key.
#
# THIS DOESN'T WORK, but if it does if I run the command manually
# after already logged in.  So, put it in a zsh init script.
#
#xmodmap -e "keysym Menu = Super_L"

# Fix the screen
xrandr --output DVI-0 --auto --output VGA-0 --auto --left-of DVI-0

gnome-panel &
#gnome-settings-daemon &

# Applets
#
# For gnome applets, you can Alt+Right-click on panel and then
# add-them.  For volume, I found that you can create a custom sound
# applet by choosing "custom application launcher" and entering
# "gnome-control-center sound" as the command to run.
#
# `gnome-control-center` starts the configuration dialog.
gnome-terminal &
gnome-terminal &

function log {
    echo "`date -R`:" "$@" | tee -a ~/.xsession-log ~/.xsession-errors
}

log "~/.xsession was run"

# I couldn't make this work ... would be much simpler than using the
# .session file which uses the .desktop file :P
#
#export WINDOW_MANAGER=`which xmonad`
exec gnome-session --session=local-xmonad-session
