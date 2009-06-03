# -*- shell-script -*-

## Path

export PATH=~/local/bin:~/local/scripts:~/local/more-scripts:$PATH

## HISTORY

# If these first two aren't set no history is saved or loaded
HISTFILE=~/.zsh-history
# How many command to save
SAVEHIST=10000
HISTSIZE=10000
# Share history between processes.  This is annoying
#setopt SHARE_HISTORY
setopt INC_APPEND_HISTORY # use fc -RI to manually merge the other shells' history
alias nc:mergehist="fc -RI"

## Colors

# See PROMPT EXPANSION in man zshmisc for details.

rd=%{$'\e[31m'%}	# red
gr=%{$'\e[32m'%}	# green
yl=%{$'\e[33m'%}	# yellow
bl=%{$'\e[34m'%}	# blue
pl=%{$'\e[0m'%}		# plain
bk=%{$'\e[5m'%}		# blinking
bd=%{$'\e[1m'%}		# bold

## Serialization

function nc:serialize () {
    : "$0 LOCKFILE COMMAND: run COMMAND using LOCKFILE as semaphore (not reentrant)"
    local lockfile="$1"
    shift

    while ! mktemp $lockfile > /dev/null; do
        echo lockfile $lockfile found, sleeping ... >/dev/stderr
        sleep 1
    done
    "$@"
    rm -f $lockfile
}

## Emacs 

export VISUAL="emacs -nw" EDITOR="emacs -nw"

function nc:ex () { emacs -fs -rv "$@" &! }
function nc:et () { emacs -nw -rv "$@" }

## Python 

# Doesn't work with ~ in path.
export PYTHONPATH=$PYTHONPATH:$HOME/local/scripts
export PYTHONSTARTUP="$HOME/.pythonrc"

## Subversion

alias nc:svnlsjunk="svn st | grep '^\\?' | awk '{print \$2}'"
alias nc:svnrmjunk="nc:svnlsjunk | xargs rm -r"

## Load extensions

for f in ~/.zsh/env.d/*; do
    source $f
done
nc:load-custom () {
    local FILE=$1
    if [[ -e $FILE ]]; then
	      source $FILE
    fi
}
nc:load-custom ~/.zshenv.system-custom

## Run last

umask 022
