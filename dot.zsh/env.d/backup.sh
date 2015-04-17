# The host linuxlab.cs.pdx.edu is faster when it's up, but seems less
# reliable than linux.cecs.pdx.edu .
#
# These default values can be overridden in ~/.zshenv.system-custom.
BACKUP_HOST=linux.cecs.pdx.edu
BACKUP_USER=ntc2

# Mirror a file or directory on linuxlab
function nc:mirror:put {
  : 'usage: $0 (FILE | DIR)'
  :
  : 'Copy (FILE | DIR) to ~/mirror/ on linuxlab.'
  :
  : 'If `basename DIR` is "." the name of dir will used for the backup.'
  if [[ ! ($# -eq 1 && -e "$1") ]]; then
    return $(nc:usage nc:mirror "Wrong number of arguments.")
  fi
  # Normalize path by expanding and then removing any trailing slash.
  # Rsync behaves very differently with a trailing slash on source
  # dirs!
  local abspath=$(readlink -f "$1")
  local normalpath=$(dirname "$abspath")/$(basename "$abspath")
  rsync -avz --human-readable --delete "$normalpath" \
    $BACKUP_USER@$BACKUP_HOST:mirror/$(hostname -f)/
  # Rsync in archive mode mirrors the timestamps as well.  So, we
  # touch the top level dir to get more helpful output from
  # nc:mirror:ls.
  ssh $BACKUP_USER@$BACKUP_HOST "touch mirror/$(hostname -f)/$(basename "$abspath")"
}

# Like 'nc:git:mirror', but uses more general '.nc-mirror-magic' file
# instead of looking for '.git' dir.
function nc:mirror:magic {
  : 'usage: $0 [DIR]'
  :
  : 'Push a mirror of first directory at or above DIR (default ".")'
  : 'containing a ".nc-mirror-magic" file.'
(
  if (($# >= 1)); then
    cd "$1"
  fi

  local last=""
  while [[ "$last" != "$(pwd)" ]]; do
    if [[ -e .nc-mirror-magic ]]; then
      echo "Mirroring $(pwd):\n"
      nc:mirror:put .
      return $?
    else
      last="$(pwd)"
      cd ..
    fi
  done
  nc:usage nc:mirror:magic "Couldn't find a '.nc-mirror-magic' file."
)
}

function nc:mirror:get {
  : 'usage: $0 BACKUP_HOST FILE'
  :
  : 'Recover a backup of FILE created from host BACKUP_HOST.'
  if [[ $# -ne 2 ]]; then
    return $(nc:usage nc:mirror "Wrong number of arguments.")
  fi
  local normalpath=$(dirname "$2")/$(basename "$2")
  rsync -avz --human-readable \
    $BACKUP_USER@$BACKUP_HOST:mirror/"$1"/"$normalpath" ./
}

function nc:mirror:ls {
  : 'usage: $0'
  :
  : 'List mirror in format suitable as input to nc:mirror:get.'
  ssh $BACKUP_USER@$BACKUP_HOST \
    'cd ~/mirror \
     && find * -mindepth 1 -maxdepth 1 -printf "%Tc: %p\n" \
        | sed -re "s|/| |" | column -t'
}
