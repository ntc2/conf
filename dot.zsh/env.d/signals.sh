# Install handler for CTRL-C that kills current command and all its
# background jobs.
#
# This trap affects the whole shell, and moreover will kill any shell
# it runs in! So, only do this in a subshell, with
#
#   (install-ctrl-c-handler; <cmd>)
#
# or in a dedicated shell, with
#
#   zsh -c "install-ctrl-c-handler; <cmd>"
#
# if you don't want the current shell to exit too.
nc:kill-everthing-on-ctrl-c () {
  # In ZSH, defining a TRAP<sig> function is like setting a trap for
  # <sig> with 'trap <cmd> <sig>'.
  TRAPINT () {
    echo "Running SIGINT handler in TRAPINT!"
    # Kill the subprocesses. I don't understand this, or why it needs
    # to be so complicated, but the magic 'kill' comes from
    # https://stackoverflow.com/a/13167238/470844. But use a for loop
    # instead of kill directly, in case the job list is empty.
    for j in ${${(v)jobstates##*:*:}%=*}; do kill $j; done
    # Make the current command die indicating it was killed by SIGINT
    # (the first arg to TRAP<sig> is the signal number). If we return
    # zero here then ZSH assumes everything is OK and moves on to
    # running the next command, so we return non-zero. The
    #
    #   128+<sig number>
    #
    # is the way for a process to indicate that it was killed by
    # <sig>, but any non-zero exit code would work here.
    #
    # Got the non-zero exit idea from
    # https://unix.stackexchange.com/a/230568/19827 and the 128+<sig
    # number> from ???
    #
    # And this needs to be 'return', not 'exit'.
    return $((128 + $1))
  }
}

# Tests for nc:install-ctrl-c-handler.
#
# Run '_nc:test:cmd-with-children' and hit CTRL-C at different times
# to see what happens.

_nc:test:child-cmd () {
  echo "XXX: child $1 starting ..."
  sleep 2 || echo "fail: $?"
  echo "XXX: child $1 middle ..."
  sleep 2
  echo "XXX: child $1 exiting!"
}

_nc:test:cmd-with-children () {(
  ;
  nc:kill-everthing-on-ctrl-c
  trap
  _nc:test:child-cmd 1 &
  _nc:test:child-cmd 2 &
  echo "jobs = $(jobs -l)"
  _nc:test:child-cmd 3
  _nc:test:child-cmd 4
  sleep 2
  echo "XXX: parent exiting!"
)}
