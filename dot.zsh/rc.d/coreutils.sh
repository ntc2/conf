# Historical comment :)

# #10/1/04 a good ol` fashion bashrc
# #
# # to lazy to learn tcsh, not to mention bash rocks anyway.

alias ls="ls --color=auto -Fh"
alias l="ls -l"
alias la="l -a"
# Set LS_COLORS.
#
# Make directory color more readable on dark background.  The
# ~/.Xresources make the bold specified here display as purple.
eval $(dircolors | sed -re 's/\bdi=01;34\b/di=01/')

alias rm="rm -iv"
alias cp="cp -v"
alias j="jobs -l"
alias l.=ldot	# seems i cant call my subroutine l. so this is a workaround.

alias nc:grep="egrep -nH --color=auto"
# Colorized case insensitive egrep with context
alias eg="nc:grep -iC2"

# super cool dir listerator.  trippy.
lsd () { l $@ | egrep '^d'; }

# list hidden files.
#ldot () { if [ $1 ]; then l -d $1/.*; else l -d .*; fi; }
# the previous version doesn't look good when $1 ends in /.
# ${ :+ } is use alternate values.  look in parameter expansion.
# ${parameter/pattern/replacement} replaces first occur.  %, instead of $ for
# some unknown reason, means match at the end, but you put it before the pattern.
# seems a little weird to me.  
# at any rate we make sure there is exactly one / at the end of the argument when
# there is an argument.
ldot () { l -d ${1:+${1/%\//}/}.*; }

# List all the executable files in $1, or ./ if no args.
lsex () { find ${1:-./} -maxdepth 1 -perm +0100 -type f; }
lsex () { \ls -F "$@" | \grep '\*' | sed -re 's/\*$//g'; }
