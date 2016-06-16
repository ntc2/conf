# The host linuxlab.cs.pdx.edu is faster when it's up, but seems less
# reliable than linux.cecs.pdx.edu .
#
# These default values can be overridden in ~/.zshenv.system-custom.
BACKUP_HOST=linux.cecs.pdx.edu
BACKUP_USER=ntc2

# Mirror a file or directory to BACKUP_HOST
function nc:mirror:put {
  : 'usage: $0 (FILE | DIR) [RSYNC_OPTS]'
  :
  : 'Copy (FILE | DIR) to ~/mirror/ on $BACKUP_HOST.'
  : 'Structure of mirror is duplicated in ~/mirror-structure'
  : 'to ease exploration.'
  :
  : 'If `basename DIR` is "." the name of dir will used for the backup.'
  if [[ $# -eq 0 ]]; then
    return $(nc:usage nc:mirror:put "You must specify a file or dir!")
  fi
  file="$1"; shift
  if [[ ! (-e "$file") ]]; then
    return $(nc:usage nc:mirror:put "Source '$1' does not exist.")
  fi
  ssh $BACKUP_USER@$BACKUP_HOST "mkdir -p ~/mirror"
  # Normalize path by expanding and then removing any trailing slash.
  # Rsync behaves very differently with a trailing slash on source
  # dirs!
  local abspath=$(readlink -f "$file")
  local normalpath=$(dirname "$abspath")/$(basename "$abspath")
  echo "Mirroring $file ..."
  # The '--relative' options means to recreate the full source path
  # under the target directory at the destination. The '--delete'
  # option means to delete files on the destination which are not on
  # the host, but only for *directories* that are explicitly
  # synced. So, e.g, separate syncs of 'foo/a' and 'foo/b' will not
  # result in only having 'b' in 'foo' on the destination host.
  rsync -avz --relative --human-readable --delete "$@" "$normalpath" \
        $BACKUP_USER@$BACKUP_HOST:mirror/$(hostname -f)/ \
        2>&1 | \
    perl -pe '$_ = "  $_"' # Indent output.
  # Note what was mirrored, to make exploring the mirror later
  # easier.  Encode the slashes as '<slash>' since they aren't allowed
  # in file names. May there never be a day that I want '<slash>' in
  # my file names ...
  #
  # The "note" format is "<hostname> <encoded backed up path>", and is
  # decoded by 'nc:mirror:ls' below.
  local ghostpath="mirror/$(echo "$(hostname -f) $normalpath" | \
                            sed -re 's|/|<slash>|g')"
  ssh $BACKUP_USER@$BACKUP_HOST "touch '$ghostpath'"
}

function nc:mirror:put-if-exists () {
  : 'usage: $0 PATH'
  :
  : 'Mirror FILE, ignoring common Haskell build artifacts.'
  : "Unlike 'nc:mirror', it's not an error if FILE does not exist."
  file="$1"
  if [ -e "$file" ]; then
    nc:mirror:put "$file" \
                  --exclude='dist/' \
                  --exclude='.stack-work/' \
                  --exclude='.*-sandbox/' \
                  --delete-excluded
  else
    echo "$0: skipping non-existent file: $file ..."
  fi
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
  : 'usage: $0 SOURCE_HOST (FILE|DIR)'
  :
  : 'Recover a backup of (FILE|DIR) created from host SOURCE_HOST.'
  :
  : 'The path (FILE|DIR) is normalized before rsync is called,'
  : 'removing the trailing slash, if any. So, the last component'
  : 'of a DIR will be duplicated locally.'
  :
  : 'So, for example, use'
  :
  :   'cd /home'
  :   'nc:mirror:get <source host> /home/collins'
  :
  : 'to mirror /home/collins from <source host> to /home/collins'
  : 'on local host. This mirroring does not use --delete, so files'
  : 'on local host not present in mirror will not be deleted.'
  :
  : 'Feature idea: support trailing slashes in the DIR, and add a'
  : 'third, destination argument, to avoid needing to cd on first.'
  if [[ $# -ne 2 ]]; then
    return $(nc:usage nc:mirror:get "Wrong number of arguments.")
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
     && find *"<slash>"* -printf "%Tc: %p\n" \
        | sed -re "s|<slash>|/|g" | column -t'
}
