# The -font is redundant, since it's also def in dot.emacs, but the
# latter takes effect after emacs has loaded, which changes the screen
# size and confuses xmonad :P ... and some computers don't have
# terminus, so try again if the '-fn terminus' version fails.
function nc:ex () {
  { emacs -fs "$@" -font terminus || emacs -fs "$@" } &!
}
function nc:et () { emacs -nw "$@" }

function nc:emacs:cask {
  : 'Usage: Run `cask` using my global Cask file.'
  :
  : 'Useful commands:'
  : '- `cask update`: updates all packages in Cask file, even if already installed.'
  : '- `cask install`: installs all packages in Cask file, only if not already installed.'
  : '- `cask outdated`: print outdated packages, i.e. those that `cask update` would update.'
  cask --path ~/.emacs.d --verbose "$@"
}
