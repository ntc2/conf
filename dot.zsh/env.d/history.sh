# See ~/v/conf/dot.zsh/rc.d/prompt.sh for settings related to history
# and timing -- i.e. showing command run time in prompt. See
# ~/v/conf/dot.zshrc for history-related key bindings.

# ZSH history documentation. See
#
# - http://zsh.sourceforge.net/Doc/Release/Options.html#History
#
# for history options -- e.g. 'INC_APPEND_HISTORY'.
#
# - http://zsh.sourceforge.net/Doc/Release/Parameters.html
#
# for history variables -- e.g. 'HISTSIZE'.
#
# - http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
#
# for ZLE settings, e.g. 'set-local-history'.

# If 'HISTFILE' and 'SAVEHIST' aren't set no history is saved or
# loaded.
HISTFILE=~/.zsh-history
# "The maximum number of history events to save in the history file."
SAVEHIST=10000
# "The maximum number of events stored in the internal history
# list. If you use the HIST_EXPIRE_DUPS_FIRST option, setting this
# value larger than the SAVEHIST size will give you the difference as
# a cushion for saving duplicated history events."
HISTSIZE=20000
# "If the internal history needs to be trimmed to add the current
# command line, setting this option will cause the oldest history
# event that has a duplicate to be lost before losing a unique event
# from the list. You should be sure to set the value of HISTSIZE to a
# larger number than SAVEHIST in order to give you some room for the
# duplicated events, otherwise this option will behave just like
# HIST_IGNORE_ALL_DUPS once the history fills up with unique events."
setopt HIST_EXPIRE_DUPS_FIRST

alias nc:grephist="fc -l -n 0 | grep -E --color=auto"
# I used to manually merge history only when I needed it, because I
# did not know about restricting '<up>' and '<down>' to local history.
#
#setopt INC_APPEND_HISTORY # use fc -RI to manually merge the other shells' history
#alias nc:mergehist="fc -RI"
