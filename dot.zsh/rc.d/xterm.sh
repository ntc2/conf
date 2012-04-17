# Based on snippet from http://forums.gentoo.org/viewtopic-p-176209.html#176209.
case $TERM in
    *xterm*|rxvt|(dt|k|E)term)
        precmd () { print -Pn "\e]0;$(hostname | cut -d . -f 1):%~\a" }
        # Need to be careful with percent escapes in the command $1.
        # Solution from

        # http://www.zsh.org/mla/users/2010/msg00596.html

        # The problem was that `print -P` expands percent escapes.  I
        # don't have any percent escapes here, so I don't need -P.
        #
        # If I did also want percent expansion, I could use a fancier
        # solution from

        # https://bbs.archlinux.org/viewtopic.php?pid=960428#p960428

        # I.e., use `${~1:gs/%/%%}` to escape the percents before
        # expansion.
        preexec () { printf "\e]0;$(hostname | cut -d . -f 1):$1\a" }
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
