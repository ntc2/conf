# Make 'less' read the whole file so that the percentage from '-M'
# works.
#
# See http://stackoverflow.com/a/19871578/470844.
export MANPAGER='less -s -M +Gg'

# Man page fixed at 65 columns. Full width man pages in a wide
# terminal are hard to read.
alias nc:man="MANWIDTH=65 man"

## Poor man's attempt at bash's 'help' command
#
# From http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html
#
# UPDATE: the MANPAGER var overrides the PAGER var, so need to use
# MANPAGER here since I export MANPAGER above.
nc:zsh:help () {
  MANPAGER="less -g -s '+/^       $1\b'" nc:man zshall
}
