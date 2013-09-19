# Mirror a file or directory on linuxlab
function nc:mirror {
  : 'usage: $0 (FILE | DIR)'
  :
  : 'Copy (FILE | DIR) to ~/mirror/ on linuxlab.'
  :
  : 'If `basename DIR` is "." the name of dir will used for the backup.'
  if [[ ! ($# -eq 1 && -e "$1") ]]; then
    return $(nc:usage nc:mirror "Wrong number of arguments.")
  fi
  # Normalize path expanding and removing any trailing slash.  Rsync
  # behaves very differently with a trailing slash on source dirs!
  local abspath=$(readlink -f "$1")
  local normalpath=$(dirname "$abspath")/$(basename "$abspath")
  rsync -avz --delete "$normalpath" ntc2@linuxlab.cs.pdx.edu:mirror
}
