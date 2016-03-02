nc_lib_trick_tty=~/v/conf/scripts/trick_tty/lib_trick_tty.so
# Trick command into thinking its stdout is connected to a tty.
alias nc:trick_tty="LD_PRELOAD=$nc_lib_trick_tty:$LD_PRELOAD "
