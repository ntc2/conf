if [[ "$TERM" == "rxvt-256color" ]]; then
    # Was getting very strange behavior in Ubuntu 11.04 without this,
    # e.g. backspace inserting a phantom space, and man complaining
    # that my terminal was not fully functional.
    export TERM=rxvt
fi

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
        
        # UPDATE: switching to `printf` from `print -P` made `date
        # +%s` work, but then I had trouble from `echo $((10%3))`.  I
        # don't really understand the leading ~ in the expansion, so I
        # left it out ...
        preexec () { printf "\e]0;$(hostname | cut -d . -f 1):${1:gs/%/%%}\a" }
        ;;
esac

# Set the title of an xterm
function nc:xtitle () { echo -e '\e]0;'$@'\a'; }

# if we are graphical kill that god awful bell:
if [[ -n "$DISPLAY" ]] && `which xset &>/dev/null`; then
    xset -b
fi
