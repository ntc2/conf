# Based on snippet from http://forums.gentoo.org/viewtopic-p-176209.html#176209.
case $TERM in
    *xterm*|rxvt|(dt|k|E)term)
        precmd () { print -Pn "\e]0;$(hostname | cut -d . -f 1):%~\a" }
        preexec () { print -Pn "\e]0;$(hostname | cut -d . -f 1):$1\a" }
        ;;
esac

#get a scroll bar.
alias xterm="xterm -sb" 

# Set the title of an xterm
function xtitle () { echo -e '\e]0;'$@'\a'; }

# if we are graphical kill that god awful bell:
if [[ -n "$DISPLAY" ]] && `which xset &>/dev/null`; then
    xset -b
#    xrdb -merge ~/.Xdefaults
fi
