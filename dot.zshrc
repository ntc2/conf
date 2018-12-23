# -*- shell-script -*-

## ZSH conf
#
# Completion is in ./dot.zsh/rc.d/completion.sh.

# See FILENAME GENERATION section of man zshexpn.
#
# Enabled operators include difference (~), negation (^), repetition (#, ##)
setopt extendedglob

# Allow comments at repl (vs only in scripts). This may interfere with
# some extended globs enabled by the last 'setopt'.
setopt interactivecomments

# Disable implicit `tee`. E.g., with MULTIOS enabled (the default),
# there doesn't seem to be any way to only capture the stderr of a
# process (I have no idea how I didn't run into this many years ago
# ... so confused right now). E.g., in Bash you can do
#
#   { echo stdout ; echo stderr >&2; } 2>&1 1>/dev/null | less
#
# to see only "stderr", but in ZSH with MULTIOS enabled, you'll see
# both "stdout" and "stderr". It's like you did
#
#   { echo stdout ; echo stderr >&2; } 2>&1 | tee /dev/null | less
#
# Baffles the mind ...
setopt no_multios

## Key bindings
#
# See `stty -a` and `bindkey` for key bindings.  The stty bindings
# take precedence (e.g. the C-s freezing the terminal (disabled below)
# taking precedence over C-s incremental forward searching the
# history). Run `read` and then type things to see escapes that `stty`
# and `bindkey` understand.

nc:show-bindinds () {
  : 'Show (some of) the keybindings available in the terminal.'
  : 'Use `read` to discover escapes that can be used with `bindkey` and `stty`.'
  echo "Bindkey bindings:"
  echo "================================================================"
  bindkey
  echo
  echo "Stty bindings:"
  echo "================================================================"
  stty -a
}

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

# Delete word stops at slash, like in bash.
#
# From http://www.zsh.org/mla/users/2005/msg01314.html and
# http://www.zsh.org/mla/users/2001/msg00870.html
backward-delete-to-slash () {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash
# '^[^?' means M-Backspace :P
bindkey '^[^?' backward-delete-to-slash

## Load extensions

for f in ~/.zsh/rc.d/*; do
    source $f
done
nc:load-custom ~/.zshrc.system-custom
# This is usually unused on Linux systems, confusing in Cygwin
# (because only the *nix part gets swapped), and annoys people when I
# use their system to SSH into one of mine and inadvertantly swap
# their caps lock.  So, in the rare case where I want this, just run
# ~/local/scripts/capswap.sh manually once on login, and once before
# logout if using someone else's machine.
#nc:load-custom ~/local/scripts/maybe-capswap.sh

## Run last

# Remind me to make sure all my conf repos are current.
(
# don't print info about background jobs in this sub shell
unset NOTIFY

# Make conf_dir go out of scope after the vc commands
    # .zshrc -> $conf_dir/dot.zshrc
    conf_dir=$(dirname $(readlink -f ~/.zshrc))
    #docs_dir=$(dirname $(readlink -f ~/local/more-scripts))
    #svn info  $conf_dir # General info

    # Print uncommitted modifications and available updates; there is
    # only output when something is changed.
    (
    cd $conf_dir
    git fetch
    git --no-pager diff --stat '@{upstream}'
    ) &!
    # for f in ~/.{zshrc,zshenv,zprofile}.system-custom; do
    #     [[ -e $f ]] && svn st $(readlink -f $f) 2>/dev/null
    # done
    # No --update when on stupid AFS file system
    #[[ -e $docs_dir ]] && svn st $docs_dir &
)

# ALSO SET in .zshenv, although that is sometimes overridden, and this
# only runs for interactive shells
umask 077

# Procrastinate
if which fortune &>/dev/null; then
   # In Ubuntu 18.04 it doesn't show the Spanish fortunes, even if
   # they're the only ones installed :P If trying to debug 'fortune',
   # running 'fortune -f <other args>' is useful: it prints the
   # fortune databases it would be choosing from.
  fortune es es/off 2>/dev/null || fortune es 2>/dev/null || fortune
fi
