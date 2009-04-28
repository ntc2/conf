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
#zstyle :compinstall filename '/root/.zshrc' #???

autoload -U compinit
compinit
# End of lines added by compinstall

# from http://osdir.com/ml/shells.zsh.user/2007-06/msg00001.html
_comp_options+=(globdots)

# Enable "tab once" completion, e.g.
# /u/s/d/python<TAB> => /usr/share/doc/python
compctl -D -f + -U -Q -K multicomp 

# SSH host completion (from Gentoo Wiki:
# http://gentoo-wiki.com/TIP_Advanced_zsh_Completion)
#
# NB: Doesn't work when HashKnownHosts is set to yes (can override in
# ~/.ssh/config if you don't have root).
if [[ -e $HOME/.ssh/known_hosts ]]; then
  local _myhosts
  _myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
  zstyle ':completion:*' hosts $_myhosts
fi

# Path

export PATH=~/local/bin:~/local/scripts:~/local/more-scripts:$PATH

## Prompt (Colors!)

# Set up the prompt.  I think it would be inherited, but a sys script
# (/etc/bash.bashrc) overrides it for each new shell.

# See PROMPT EXPANSION in man zshmisc for details.

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

# Based on snippet from http://forums.gentoo.org/viewtopic-p-176209.html#176209.
case $TERM in
    *xterm*|rxvt|(dt|k|E)term)
        precmd () { print -Pn "\e]0;$(hostname | cut -d . -f 1):%~\a" }
        preexec () { print -Pn "\e]0;$(hostname | cut -d . -f 1):$1\a" }
        ;;
esac


# By default zsh backs up to the beginning of the line (opt name
# implies it's a carriage return) before displaying the prompt, so
# output that doesn't end in newline gets covered up, e.g. if you echo
# -n <some string> then <some string> gets covered by the prompt.
# This can be confusing, so we turn the "feature" off.

setopt nopromptcr

# Fancy looking messages.
message () {
    # -P means interpret % escapes
    print -P "${rd}[${bl}* ${gr}$@ ${bl}*${rd}]${pl}"
}

## Platform specific extensions

message Loading extensions
load-custom () {
    local FILE=$1
    if [ -e $FILE ]; then
	      source $FILE
	      echo "Loaded extension $FILE"
    else
	      echo "Skipping missing extension $FILE"
    fi
}
load-custom ~/.zshrc.system-custom
load-custom ~/local/scripts/maybe-capswap.sh

if which lesspipe &>/dev/null; then
    eval $(lesspipe)
fi

## Two million aliases

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#10/1/04 a good ol` fashion bashrc
#
# to lazy to learn tcsh, not to mention bash rocks anyway.

alias ls="ls --color=auto -Fh"
alias l="ls -l"
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

alias nc:grep="egrep -nH --color=auto"
# Colorized case insensitive egrep with context
alias eg="nc:grep -iC2"

# grep -P may also work. be sure to escape "/"s ...
function perlgrep () { perl -ne "/$1/"' && print "$.:\t$_"' $2 }

# Subversion
alias svnlsjunk="svn st | grep '^\\?' | awk '{print \$2}'"
alias svnrmjunk="svnlsjunk | xargs rm -r"

# make the xclock look super neat (TM).
## this was problematic because when i started the xclock from ions run menu
## it had default properties.  i set up properties for all instances in my 
## .Xdefaults file.
# alias xclock="xclock -fg black -bg grey -hl red -update 1 -chime"

## A million random functions

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

# Fancy log output.  Pass log file name as input.  Highlights perl files and line nums, and indents stack trace.
log-tail(){ tail -f $1 |sed -re 's/\\n/\n\t/gp' -e "s/^$|[0-9]+|[^ ]*\.p[^ ]*/`echo -ne '\e[31m'`&`echo -ne '\e[0m'`/g";}

# Fancy highlighted cat
fancy-cat(){ enscript --color -w ansi -E -p- $1  | cat -n; }

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

## Key bindings

# Make HOME and END work.

bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
case $TERM in (xterm*)
    bindkey '\e[H' beginning-of-line
    bindkey '\eOH' beginning-of-line
    bindkey '\e[F' end-of-line
    bindkey '\eOF' end-of-line ;;
esac

# Enable emacs style line opening in when editing multiline commands
# in history.

bindkey '^O' vi-open-line-above

## HISTORY

# If these first two aren't set no history is saved or loaded
HISTFILE=~/.zsh-history
# How many command to save
SAVEHIST=10000
HISTSIZE=10000
# Share history between processes.  This is annoying
#setopt SHARE_HISTORY
setopt INC_APPEND_HISTORY # use fc -RI to manually merge the other shells' history
alias mergehist="fc -RI"

## Emacs

export VISUAL="emacs -nw" EDITOR="emacs -nw"
function nc:ex () { emacs -fs -rv "$@" &! }
function nc:et () { emacs -nw -rv "$@" }

## Python 

# Doesn't work with ~ in path.
export PYTHONPATH=$PYTHONPATH:$HOME/local/scripts
export PYTHONSTARTUP="$HOME/.pythonrc"

## LESSOPEN

# standard
if which lesspipe &>/dev/null; then
    eval $(lesspipe)
# Gentoo
elif which lesspipe.sh &>/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
else
    message "Unable to setup LESSOPEN filter."
fi

## Screen

alias fixssh='source ~/local/bin/fix-ssh.sh'
alias genfixssh='source ~/local/scripts/gen-fix-ssh.sh'
# screen doesn't see the alias defined by genfixssh?
alias attach='genfixssh ; screen -dRR'

## Java

# pretend deepest dirs are packages and list them
function nc:jar-packages () {
    jar -tf $1 \
    | grep -v META-INF \
    | grep '/$' \
    | sort \
    | xargs python -c 'import sys; as = sys.argv[1:]; ps = [x for (x,y) in zip(as,as[1:]+[""]) if x not in y]; map(lambda p: sys.stdout.write(p[:-1]+"\n"), ps)' \
    | sed -re 's!/!.!g'
}

# print classes in package form
function nc:jar-classes () {
    jar -tf $1 \
    | grep '\.class$' \
    | sed -re 's!/!.!g' -e 's/\.class//g'
}

# cat the MANIFEST file, which contains the library version
function nc:jar-manifest () {
    JAR=$(readlink -f $1)
    DIR=$(mktemp -d)
    (
    cd $DIR
    jar -xf $JAR META-INF
    cat META-INF/MANIFEST.MF
    )
    \rm -r "$DIR"
}

# cat all of META-INF
function nc:jar-meta-inf () {
    JAR=$(readlink -f $1)
    DIR=$(mktemp -d)
    (
    cd $DIR
    jar -xf $JAR META-INF
    cd META-INF
    for f in $(ls); do
        echo "====[FILE $f]===="
        cat $f
    done
    )
    \rm -r "$DIR"
}

## Run last

# Remind me to make sure all my conf repos are current.
message SVN conf dir info
sleep 1 && clear
(
# don't print info about background jobs in this subshell
unset NOTIFY

# Make conf_dir go out of scope after the svn commands
    conf_dir=$(dirname $(readlink -f ~/.zshrc))
    docs_dir=$(dirname $(readlink -f ~/local/more-scripts))
    #svn info  $conf_dir # General info

    # Print uncommitted modifications and available updates, but don't
    # show the revision we're currenty at, so that there is only
    # output when something is changed
    svn st -uq $conf_dir | head -n -1 &
    # No --update when on stupid AFS file system
    [[ -e $docs_dir ]] && svn st $docs_dir &
)

umask 022
