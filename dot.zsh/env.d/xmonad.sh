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

function nc:xmonad:galois:config-monitors {
  : 'Configure monitors for laptop when lid is closed and external monitors are attached.'
   xrandr --output DP-2 --mode 1680x1050 --left-of DP-3 --output DP-3 --mode 1920x1080 --left-of DP-0 --output DP-0 --off
}
