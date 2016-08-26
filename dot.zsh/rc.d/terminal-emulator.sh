if [[ "$TERM" == "rxvt-256color" ]]; then
    # Was getting very strange behavior in Ubuntu 11.04 without this,
    # e.g. backspace inserting a phantom space, and man complaining
    # that my terminal was not fully functional.
    export TERM=rxvt
fi

# Set the title of the terminal to the current command.
#
# The `precmd` is used to reset the title before displaying a prompt,
# and `preexec` is used to set it to the current command before
# running the current command.
#
# See `man zshall` for description of `precmd` and `preexec`.
#
# Based on snippet from http://forums.gentoo.org/viewtopic-p-176209.html#176209.
case $TERM in
    *xterm*|rxvt*|(dt|k|E)term)
        # Run before each prompt is displayed.
        terminal_emulator_precmd () {
          print -Pn "\e]0;$(hostname | cut -d . -f 1):%~\a"
        }
        precmd_functions+=(terminal_emulator_precmd)
        # Need to be careful with percent escapes in the command $1.
        # Solution from

        # http://www.zsh.org/mla/users/2010/msg00596.html

        # The problem was that `print -P` expands percent escapes.  I
        # don't have any percent escapes here, so I don't need -P.
        #
        # If I did also want percent expansion, I could use a fancier
        # solution from

        # https://bbs.archlinux.org/viewtopic.php?pid=960428#p960428

        # I.e., use `${~1:gs/%/%%}` to escape the percents before
        # expansion.
        
        # UPDATE: switching to `printf` from `print -P` made `date
        # +%s` work, but then I had trouble from `echo $((10%3))`.  I
        # don't really understand the leading ~ in the expansion, so I
        # left it out ...

        # UPDATE: had trouble with `\n` in command, e.g. `echo -e
        # 'a\nb'`. Solution is to simply split the terminal setting
        # command into parts: the first and third parts that send
        # special characters to the terminal, which need `print -P` to
        # interpret escapes, and the middle part, which needs `print`
        # without `-P` to avoid interpreting `%`. We also need to
        # avoid interpreting `\n`, which we get by escaping the
        # command string using `${(q)1}`, following
        # http://www.zsh.org/mla/users/2003/msg00593.html.

        # http://zsh.sourceforge.net/Doc/Release/Functions.html#Hook-Functions:
        #
        # Executed just after a command has been read and is about to
        # be executed. If the history mechanism is active (regardless
        # of whether the line was discarded from the history buffer),
        # the string that the user typed is passed as the first
        # argument (`$1`), otherwise it is an empty string. The actual
        # command that will be executed (including expanded aliases)
        # is passed in two different forms: the second argument (`$2`)
        # is a single-line, size-limited version of the command (with
        # things like function bodies elided); the third argument
        # (`$3`) contains the full text that is being executed.

        # UPDATE: more newline trouble, now with multiline
        # commands. E.g.
        #
        #   $ echo abc\
        #   def
        #
        # Would print part of the command before executing
        # it. Limiting to the first line of the command solves the
        # problem with extra printing, but the title does not get
        # updated. A rare case, so I'm not going to worry about fixing
        # the title here.
        terminal_emulator_preexec () {
          print -Pn "\e]0;$(hostname | cut -d . -f 1):"
          print -n "${(q)1}" | head -n1
          print -Pn "\a"
        }
        preexec_functions+=(terminal_emulator_preexec)
        ;;
esac

# Set the title of an xterm
function nc:xtitle () { echo -e '\e]0;'$@'\a'; }

function nc:term {
: "usage: $0 [PATH]"
:
: "Start a terminal, in currently dir, or at PATH if specified."
(
  if [[ $# == 1 ]]; then
    cd "$1"
  fi

  if which urxvt &> /dev/null; then
    urxvt
  elif which rxvt &> /dev/null; then
    rxvt
  elif which xterm &> /dev/null; then
    xterm
  else
    nc:usage "nc:term" "No suitable terminal available."
  fi
) &!
}

# if we are graphical kill that god awful bell:
if [[ -n "$DISPLAY" ]] && `which xset &>/dev/null`; then
    xset -b
fi
