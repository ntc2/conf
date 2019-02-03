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
export PATH=~/.cabal/bin:~/local/scripts:$PATH

# It turns out that `manpath`, which is used to find paths to search
# for man pages, will infer MANPATH from PATH.  In particular, when
# D/bin is on path, then D/share/man, if it exists, will be on
# MANPATH.  This is not documented anywhere that I can find, although
# this old email
# http://linux.derkeiler.com/Mailing-Lists/Debian/2003-08/0956.html
# explains that D on PATH implies D/man and D/../man on MANPATH, if
# they exist.
#
# If the there is an empty component in MANPATH, then auto generated
# man paths from /etc/manpath.config will be inserted there. I don't
# see where it's defined in /etc/manpath.config, but this will add
# '<path>/share/man' to the man path whenever '<path>/bin' is in
# 'PATH'. So, '~/local/opt/<path>/bin' paths above automatically add
# the appropriate man paths, e.g. for GHC and Rust.
#
# This doesn't seem to play well with `typeset -U`.
#typeset -U MANPATH
export MANPATH=:$MANPATH

# zsh looks for "functions" here, which includes completion functions
#
# ???: the lowercase version, e.g. $fpath, don't seem to do the right
# thing, e.g. I can't use $fpath in .zshenv.system-custom for git
# completion (see ~/v/conf/install-git.sh).
typeset -U FPATH
export FPATH=~/.zsh/completion:$FPATH
typeset -TU PYTHONPATH pythonpath
export PYTHONPATH=$HOME/local/scripts:$PYTHONPATH

## Colors

# See PROMPT EXPANSION in man zshmisc for details.

rd=%{$'\e[31m'%}	# red
gr=%{$'\e[32m'%}	# green
yl=%{$'\e[33m'%}	# yellow
bl=%{$'\e[34m'%}	# blue
pl=%{$'\e[0m'%}		# plain
bk=%{$'\e[5m'%}		# blinking
bd=%{$'\e[1m'%}		# bold

## Editor

export VISUAL="emacs -nw --no-desktop"
export EDITOR="$VISUAL"

## Python 

# Doesn't work with ~ in path.
export PYTHONSTARTUP="$HOME/.pythonrc"

## Run quietly
function nc:quiet () {
    : Run a command in background with all output discarded.  Useful
    : for starting noisy programs like firefox or evince from the shell.

    "$@" &>/dev/null &!
}

## Fancy looking messages.
message () {
  # Only print errors in interactive shells.
  if [[ -o interactive ]]; then
    # -P means interpret % escapes
    print -P "${rd}[${bl}* ${gr}$@ ${bl}*${rd}]${pl}"
  fi
}

## Load extensions
source ~/.zsh/env.d/util.sh
for f in ~/.zsh/env.d/*; do
    source $f
done
nc:load-custom ~/.zshenv.system-custom

## Run last

# ALSO SET in .zshrc, although that only runs for interactive shells
umask 0022
