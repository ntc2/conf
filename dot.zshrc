# -*- shell-script -*-
# The following lines were added by compinstall

if [ -e /etc/profile ]; then
    source /etc/profile
fi

zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '[%l] %SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' prompt 'Error Completion [%e]: '
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/root/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall



# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#10/1/04 a good ol` fashion bashrc
#
# to lazy to learn tcsh, not to mention bash rocks anyway.
#

alias ls="ls --color=auto"
alias l="ls -lF"
alias la="l -a"
eval $(dircolors)

alias rm="rm -iv"
alias cp="cp -v"
alias j="jobs -l"
alias l.=ldot	# seems i cant call my subroutine l. so this is a workaround.

#get a scroll bar.
alias xterm="xterm -sb" 

# Interpret ascii escapes and show verbose status and ignore case in searches
alias less="less -RMi"
# Platform specific extensions
load-custom () {
    FILE=$1
    if [ -e $FILE ]; then
	source $FILE
	echo "Loaded extension $FILE"
    else
	echo "Skipping missing extension $FILE"
    fi
}
load-custom ~/.LESSOPEN
load-custom ~/local/scripts/maybe-capswap.sh

alias grep="egrep --color=auto"
# Colorized case insensitive egrep with context
alias eg="grep -niC2"

# make the xclock look super neat (TM).
## this was problematic because when i started the xclock from ions run menu
## it had default properties.  i set up properties for all instances in my 
## .Xdefaults file.
# alias xclock="xclock -fg black -bg grey -hl red -update 1 -chime"

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

# Fancy log output.  Pass log file name as input.  Highlights perl files and line nums, and indents stack trace.
log-tail(){ tail -f $1 |sed -re 's/\\n/\n\t/gp' -e "s/^$|[0-9]+|[^ ]*\.p[^ ]*/`echo -ne '\e[31m'`&`echo -ne '\e[0m'`/g";}

# Fancy highlighted cat
fancy-cat(){ enscript -w ansi -E -p- $1  | perl -pe '$_ = "$.: $_"';}

# Set the title of an xterm
xtitle(){ echo -e '\e]0;'$@'\a';}

# Interactive perl interpreter
iperl () 
{ 
    perl -ne 'BEGIN { print ">> " }; print eval "$_"; print "\n>> "'
}


# if we are graphical kill that god awful bell:
if [ $DISPLAY ]; then
    xset -b
#    xrdb -merge ~/.Xdefaults
fi

# Set up the prompt.  I think it would be inherited, but a sys script
# (/etc/bash.bashrc) overrides it for each new shell.

# See Prompting in man bash^H^H^H^H zshmisc for details.

rd=%{$'\e[31m'%}	# red
gr=%{$'\e[32m'%}	# green
yl=%{$'\e[33m'%}	# yellow
bl=%{$'\e[34m'%}	# blue
pl=%{$'\e[0m'%}		# plain
bk=%{$'\e[5m'%}		# blinking
bd=%{$'\e[1m'%}		# bold

# Now this is a prompt.
PS1="[$rd%n$pl@$bl%m$pl][$gr%/$pl][%%$rd%j$pl][#$bd%h$pl][%*]
$bl\$$pl "

PS2="$gr%_$pl> "

umask 022

#             #
##           ##
### HISTORY ###
##           ##
#             #

# If these first two aren't set no history is saved or loaded
HISTFILE=~/.zsh-history
# How many command to save
SAVEHIST=1000
HISTSIZE=10000
# Share history between processes.  This is annoying
#setopt SHARE_HISTORY
