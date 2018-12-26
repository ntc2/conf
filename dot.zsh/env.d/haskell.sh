function nc:set_ghc () {
  : Add GHC bin/ of specified version to PATH, removing
  : any other GHC bins from PATH. E.g. 'nc:set_ghc 8.4.4'.
  :
  : Can be used in '~/.zshenv.system-custom' to set default
  : GHC version and then overridden in shells as needed.
  p=$(set_ghc.py "$1" "$PATH")
  if [[ $? -eq 0 ]]; then
    export PATH=$p
  fi
}
