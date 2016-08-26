nc_lib_trick_tty=~/v/conf/scripts/trick_tty/lib_trick_tty.so
# Trick command into thinking its stdout is connected to a tty.
alias nc:trick_tty="LD_PRELOAD=$nc_lib_trick_tty:$LD_PRELOAD "

# Add a keyboard shortcut to replace the current command '<cmd>' with
# 'nc:trick_tty <cmd> | less'.
function nc:wrap_current_line_in_trick_tty {
  BUFFER="nc:trick_tty $BUFFER | less"
}
zle -N nc:wrap_current_line_in_trick_tty
# I wanted '^X^T', but that causes my terminal to hang and I don't
# know why ...
bindkey '^X^L' nc:wrap_current_line_in_trick_tty
