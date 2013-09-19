# Mirror a file or directory on linuxlab
function nc:mirror {
  : 'usage: $0 (FILE | DIR)'
  :
  : 'Copy (FILE | DIR) to ~/mirror/ on linuxlab.'
  if [[ $# -ne 1 ]]; then
    return $(nc:usage nc:mirror "Wrong number of arguments.")
  fi
  # Normalize path by removing any trailing slash.  Rsync behaves very
  # differently with a trailing slash on source dirs!
  local filepath=$(dirname "$1")/$(basename "$1")
  rsync -avz --delete "$filepath" ntc2@linuxlab.cs.pdx.edu:mirror
}