function nc:xmonad:apply-hacks {
  : 'usage: Apply XMonad hacks. See `~/.xsession` for details.'
  # This segfaults, but not before fixing the struts (gaps for Gnome
  # panel).
  mutter
  # Make the menu key another XMonad meta key.
  xmodmap -e "keysym Menu = Super_R"
}
