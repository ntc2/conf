# Run some of these every minute vis cron. See 'crontab -l' or
# fresh-ubuntu-install.org for more details.

function nc:xmonad:hack-keyboard {
  : 'See `~/.xsession` for details; use xev to find keycodes.'
  : 'Note that xmodmap changes DO NOT PERSIST across suspends.'
  # Make the menu key and printscreen key additional XMonad meta keys.

  #xmodmap -e "keysym Menu = Super_L"

  # The Print is keycode 107 on the x230.
  #
  # Using Super_R for Print was causing weird problems with the M-Tab
  # combo, but Super_L seems to work fine.
  #xmodmap -e "keysym Print = Super_L"
  xmodmap -e "keycode 107 = Super_L"
}

function nc:xmonad:galois:config-monitors {
  : 'Configure monitors for laptop when lid is closed and external monitors are attached.'
   xrandr --output DP-2 --mode 1680x1050 --left-of DP-3 --output DP-3 --mode 1920x1080 --left-of DP-0 --output DP-0 --off
}
