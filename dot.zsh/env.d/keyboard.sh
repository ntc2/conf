nc:kbd:fix-console () {
  : 'usage: $0'
  :
  : 'Modify the console keymap.  Uses `sudo`.'
  sudo loadkeys -v ~/v/conf/lib/ctrl-slash-is-undo.kmap
}
