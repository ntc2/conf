# See ~/v/conf/dot.zsh/env.d/history.sh for links to ZSH history
# docs. It seems I can't merge this file into that file, since the
# keybindings need to be in a "rc" file; they don't have any affect
# when I put them in the "env" file.

# Below we use 'up-line-or-search' instead of 'up-line-or-history' to
# get this "no dups" effect when pressing '<up>' and '<down>'.
#
# "When searching for history entries in the line editor, do not
# display duplicates of a line previously found, even if the
# duplicates are not contiguous."
setopt HIST_FIND_NO_DUPS

# Share history between processes, but make '<up>' and '<down>' only
# use local history. On the other hand, 'C-r' and 'C-s' search the
# global, shared history. Based on
# https://superuser.com/questions/446594/separate-up-arrow-lookback-for-local-and-global-zsh-history.
#
# Note that 'INC_APPEND_HISTORY_TIME', 'INC_APPEND_HISTORY', and
# 'SHARE_HISTORY' should be used mutually exclusively, according to
# docs for 'INC_APPEND_HISTORY_TIME'.

# "This option both imports new commands from the history file, and
# also causes your typed commands to be appended to the history file
# (the latter is like specifying INC_APPEND_HISTORY, which should be
# turned off if this option is in effect). The history lines are also
# output with timestamps ala EXTENDED_HISTORY (which makes it easier
# to find the spot where we left off reading the file after it gets
# re-written).
#
# "By default, history movement commands visit the imported lines as
# well as the local lines, but you can toggle this on and off with the
# set-local-history zle binding. It is also possible to create a zle
# widget that will make some commands ignore imported commands, and
# some include them.
#
# "If you find that you want more control over when commands get
# imported, you may wish to turn SHARE_HISTORY off, INC_APPEND_HISTORY
# or INC_APPEND_HISTORY_TIME (see above) on, and then manually import
# commands whenever you need them using ‘fc -RI’."
setopt SHARE_HISTORY

# Disable flow control with 'C-s' and bind it to 'C-t' instead. Use
# the default, 'C-q', to resume output.
#
# From http://smlv.cc.gatech.edu/2010/07/08/small-tip-for-terminal-prevent-ctrl-s-ctrl-q/
stty stop undef
stty stop '^T'

# Step through local history with '<up>' and '<down>'. Based on
# http://superuser.com/a/691603/73337. By default 'C-p' and 'M-p', and
# 'C-n' and 'M-p', are bound to search global history, as '<up>' and
# '<down>' did before this override.
#
# Use 'up-line-or-search' instead of 'up-line-or-history', because an
# empty search is just like going up in history, except
# 'HIST_FIND_NO_DUPS' is respected by searches, but not simple history
# stepping. This has the additional benefit that it's also a search,
# so e.g. 'man<up><up><up>' shows the third most recent, unique man
# page viewed.
#
# 'set-local-history': "By default, history movement commands visit
# the imported lines as well as the local lines. This widget lets you
# toggle this on and off, or set it with the numeric argument. Zero
# for both local and imported lines and nonzero for only local lines."

nc:up-line-or-history () {
  zle set-local-history 1
  zle up-line-or-search
  zle set-local-history 0
}
zle -N nc:up-line-or-history

nc:down-line-or-history () {
  zle set-local-history 1
  zle down-line-or-search
  zle set-local-history 0
}
zle -N nc:down-line-or-history

bindkey "${key[Up]}"   nc:up-line-or-history
bindkey "${key[Down]}" nc:down-line-or-history
# Same as
#
#bindkey "^[[A" nc:up-line-or-history
#bindkey "^[[B" nc:down-line-or-history
#
# the mysterious escapes can be discovered by running 'read' and then
# typing. Current bindings are displayed in terms of mysterious
# escapes when you run 'bindkey'.

# Make incremental history search with 'C-s' and 'C-r' support glob
# patterns. From
# http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html.
bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward
