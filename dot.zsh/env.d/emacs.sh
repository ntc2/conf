# I used to specify the font with `-font` here, because otherwise
# xmonad got confused when the font changed after startup, because
# that changed the screen size. Doesn't seem to be an issue anymore.
function nc:ex () { emacs -fs "$@" &! }
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

# Don't want to run 'atomic-chrome-server' in all Emacs instances,
# because I want to control which Emacs the GhostText buffers get
# created in. Need to run this command before asking GhostText to edit
# a text area.
function nc:ghosttext () {
  : 'Start Emacs and listen for GhostText connections from Firefox and Chrome'
  nc:ex --funcall atomic-chrome-start-server
}
