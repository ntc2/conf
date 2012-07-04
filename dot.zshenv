# -*- shell-script -*-

## Path

# If you want to be sure that path elements are unique, you can use
# `typeset -U`.  Seems to only work on scalars that are "tied".  I.e.,
# do:
#
#   FOO=...
#   typeset -TU FOO foo :
#
# Now 'foo' is an array consisting of 'FOO' split on ':', and both
# have had duplicates removed.  NB: a plain `typeset -U FOO` doesn't
# seem to work, altho my reading of `man zshbuiltins` indicates that
# it should :P OTOH, PATH, MANPATH, and FPATH are already tied, and
# retying them results in an error.

# Having '~/.cabal/bin' first here could be dangerous: a malicious
# package could install a rogue core util.  So, do I need to make a
# point of looking in '~/.cabal/bin' after running 'cabal install'?
# Well, cabal install can spawn arbitrary shell commands for all I
# know, so this is not the primary concern.

typeset -U PATH
export PATH=~/.cabal/bin:~/local/opt/bin:~/local/bin:~/local/scripts:~/local/more-scripts:/opt/bin:$PATH
# It turns out that `manpath`, which is used to find paths to search
# for man pages, will infer MANPATH from PATH.  In particular, when
# D/bin is on path, then D/share/man, if it exists, will be on
# MANPATH.  This is not documented anywhere than I can find, although
# this old email
# http://linux.derkeiler.com/Mailing-Lists/Debian/2003-08/0956.html
# explains that D on PATH implies D/man and D/../man on MANPATH, if
# they exist.

#typeset -U MANPATH
#export MANPATH=~/local/opt/share/man:/opt/share/man:$MANPATH
# zsh looks for "functions" here, which includes completion functions
typeset -U FPATH
export FPATH=~/.zsh/completion:$FPATH
typeset -TU PYTHONPATH pythonpath
export PYTHONPATH=$HOME/local/scripts:$PYTHONPATH


# If these first two aren't set no history is saved or loaded
HISTFILE=~/.zsh-history
# How many command to save
SAVEHIST=10000
HISTSIZE=10000
# Share history between processes.  This is annoying
#setopt SHARE_HISTORY
setopt INC_APPEND_HISTORY # use fc -RI to manually merge the other shells' history
alias nc:mergehist="fc -RI"
alias nc:grephist="fc -l 1 | grep -E --color=auto"

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

    # non-serialized example (doesn't count to 10):
    #
    # echo 0 > /tmp/count 
    # function inc () { n=$(cat /tmp/count); echo $((n+1)) > /tmp/count; }
    # for i in $(seq 1 10); do inc &; done
    # cat /tmp/count
    #
    # serialized example (does count to 10):
    #
    # echo 0 > /tmp/count 
    # function inc () { n=$(cat /tmp/count); echo $((n+1)) > /tmp/count; }
    # for i in $(seq 1 10); do nc:serialize lock inc &; done
    # cat /tmp/count

    # used to be able to use {mktemp} alone in the {while} below by
    # specifying the file name $lockfile, but now {mktemp} (since
    # becoming part of coreutils) won't allow you to specify the file
    # name exactly :P But, {link} is atomic in C according to
    # http://www.unix.com/high-level-programming/95946-atomic-lock-file-creation.html#post302274515
    # and so, guessing the shell command is also atomic, we get a link
    # based solution.  We're not allowed to link across devices, so we
    # create a temp file on the same file system as $lockfile :P
    dummy=$(mktemp -p $(dirname $lockfile))
    while ! link $dummy $lockfile &> /dev/null; do
        echo lockfile $lockfile found, sleeping ... >/dev/stderr
        sleep 1
    done
    "$@"
    rm -f $lockfile $dummy
}

## Emacs 

export VISUAL="emacs -nw" EDITOR="emacs -nw"
# The -font is redundant, since it's also def in dot.emacs, but the
# latter takes effect after emacs has loaded, which changes the screen
# size and confuses xmonad :P
function nc:ex () { emacs -fs -rv "$@" -font terminus &! }
function nc:et () { emacs -nw -rv "$@" }

## Python 

# Doesn't work with ~ in path.
export PYTHONSTARTUP="$HOME/.pythonrc"

## Subversion

alias nc:svnlsjunk="svn st | grep '^\\?' | awk '{print \$2}'"
alias nc:svnrmjunk="nc:svnlsjunk | xargs rm -r"

## Run quietly
function nc:quiet () {
    : Run a command in background with all output discarded.  Useful
    : for starting noisy programs like firefox or evince from the shell.

    "$@" &>/dev/null &!
}

## Load extensions
source ~/.zsh/env.d/util.sh
for f in ~/.zsh/env.d/*; do
    source $f
done
nc:load-custom ~/.zshenv.system-custom

## Run last

# gets later overridden to 022 on some systems ... e.g., on my laptop
# /etc/profile is responsible, although i can't figure out what
# actually calls /etc/profile (the zsh docs indicate only
# /etc/zsh-beta/ scripts would be run, and those don't call
# /etc/profile afaict) ???

# ALSO SET in .zshrc, although that only runs for interactive shells
umask 077
