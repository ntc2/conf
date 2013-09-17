#!/bin/bash
# Make the menu key (usually on the right symmetric to windows on the
# left) another windows key.
#
# THIS DOESN'T WORK, but if it does if I run the command manually
# after already logged in.  So, put it in a zsh init script.
#
#xmodmap -e "keysym Menu = Super_R"
#
# Didn't verify that this one doesn't work here.  Also, are the "_R"
# suffixes necessary?
#
#xmodmap -e "keysym ISO_Level3_Shift = Alt_R"

# Delay commands to avoid problems with late starting gnome-panel
# (only have this problem on eeepc).
function delay {
    { sleep 10; "$@" } &
}

function log {
    echo "`date -R`:" "$@" | tee -a ~/.xsession-log ~/.xsession-errors
}

# XXX: this only makes sense on some systems ...

# Fix the screen
xrandr --output DVI-0 --auto --output VGA-0 --auto --left-of DVI-0

# Start gnome-panel first in hopes of avoiding problems with terminals
#gnome-panel & # Causes problems on Ubuntu 11.04
#gnome-settings-daemon &

# Applets
#
# For gnome applets, you can Alt+Right-click on panel and then
# add-them.  For volume, I found that you can create a custom sound
# applet by choosing "custom application launcher" and entering
# "gnome-control-center sound" as the command to run.
#
# `gnome-control-center` starts the configuration dialog.
if which urxvt &>/dev/null; then
    delay urxvt
else
    log "Could not find urxvt; running xterm."
    delay xterm
fi
# gnome-terminal &
# gnome-terminal &


log "~/.xsession was run"

# I couldn't make this work ... would be much simpler than using the
# .session file which uses the .desktop file :P
#
#export WINDOW_MANAGER=`which xmonad`
exec gnome-session --session=local-xmonad-session
#exec xmonad
