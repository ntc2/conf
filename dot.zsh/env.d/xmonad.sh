# Once upon a time these ran automatically via cron, but that was
# removed in 2022 and these are just for documentation now. I modify
# the keybard in ~/.zshenv.system-custom now.

function nc:xmonad:hack-keyboard {
  : 'See `~/.xsession` for details; use xev to find keycodes.'
  # Make the menu key and printscreen key additional XMonad meta keys.
  xmodmap -e "keysym Menu = Super_L"
  # The Print is keycode 107 on the x230.
  #
  # Using Super_R for Print was causing weird problems with the M-Tab
  # combo, but Super_L seems to work fine.
  xmodmap -e "keysym Print = Super_L"
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
