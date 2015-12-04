function nc:xmonad:hack-keyboard {
  : 'See `~/.xsession` for details.'
  # Make the menu key another XMonad meta key.
  xmodmap -e "keysym Menu = Super_R"
}

function nc:xmonad:hack-gnome-panel {
  : 'See `~/.xsession` for details.'
  # This segfaults, but not before fixing the struts (gaps for Gnome
  # panel). However, it does move the windows around a little in the
  # process.
  mutter
}
