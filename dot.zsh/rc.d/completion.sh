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

# Include dot files in tab completion of file names.
# From http://osdir.com/ml/shells.zsh.user/2007-06/msg00001.html
_comp_options+=(globdots)
# Not sure how that differs from `setopt globdots`.

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

# Force file name completion on C-x TAB, Shift-TAB.
#
# Below version is from
# http://chneukirchen.org/blog/archive/2011/02/10-more-zsh-tricks-you-may-not-know.html
#
# Other versions, including making regular TAB completion try
# filenames when all else fails, available here:
# http://www.zsh.org/mla/users/2011/msg00974.html
zle -C complete-files complete-word _generic
zstyle ':completion:complete-files:*' completer _files
bindkey "^X^I" complete-files
bindkey "^[[Z" complete-files
