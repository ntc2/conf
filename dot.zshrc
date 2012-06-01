# -*- shell-script -*-

## ZSH completion and conf

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

# By default zsh backs up to the beginning of the line (opt name
# implies it's a carriage return) before displaying the prompt, so
# output that doesn't end in newline gets covered up, e.g. if you echo
# -n <some string> then <some string> gets covered by the prompt.
# This can be confusing, so we turn the "feature" off.

setopt nopromptcr

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

## Key bindings
#
# See `stty -a` and `bindkey` for key bindings.  The stty bindings
# take precedence (e.g. the C-s freezing the terminal (disabled below)
# taking precedence over C-s incremental forward searching the
# history).

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

# Force file name completion on C-x TAB, Shift-TAB.
#
# From http://chneukirchen.org/blog/archive/2011/02/10-more-zsh-tricks-you-may-not-know.html
zle -C complete-files complete-word _generic
zstyle ':completion:complete-files:*' completer _files
bindkey "^X^I" complete-files
bindkey "^[[Z" complete-files

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

# Disable flow control with C-s
#
# From http://smlv.cc.gatech.edu/2010/07/08/small-tip-for-terminal-prevent-ctrl-s-ctrl-q/
stty stop undef

# Make incremental history search with C-s and C-r support glob
# patterns
#
# From http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html
bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward

## Prompt

# Set up the prompt.  I think it would be inherited, but a sys script
# (/etc/bash.bashrc) overrides it for each new shell.

# Now this is a prompt.
PS1="[$rd%n$pl@$bl%m$pl][$gr%~$pl][%%$rd%j$pl][#$bd%h$pl][%*]
$bl\$$pl "

PS2="$gr%_$pl> "

## Fancy looking messages.
message () {
    # -P means interpret % escapes
    print -P "${rd}[${bl}* ${gr}$@ ${bl}*${rd}]${pl}"
}

## Poor man's attempt at bash's 'help' command
#
# From http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html
zman () {
  PAGER="less -g -s '+/^       $1\b'" man zshall
}

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
#message SVN conf dir info
#sleep 1 && clear
(
# don't print info about background jobs in this sub shell
unset NOTIFY

# Make conf_dir go out of scope after the svn commands
    # .zshrc -> $conf_dir/dot.zshrc
    conf_dir=$(dirname $(readlink -f ~/.zshrc))
    #docs_dir=$(dirname $(readlink -f ~/local/more-scripts))
    #svn info  $conf_dir # General info

    # Print uncommitted modifications and available updates, but don't
    # show the revision we're currenty at, so that there is only
    # output when something is changed
    svn st -u $conf_dir | head -n -1 &!
    for f in ~/.{zshrc,zshenv,zprofile}.system-custom; do
        [[ -e $f ]] && svn st $(readlink -f $f) 2>/dev/null
    done
    # No --update when on stupid AFS file system
    #[[ -e $docs_dir ]] && svn st $docs_dir &
)

# ALSO SET in .zshenv, although that is sometimes overridden, and this
# only runs for interactive shells
umask 077

# Procrastinate
which fortune &>/dev/null && fortune
