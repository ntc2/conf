XMonad + Gnome + Stack
======================

Getting XMonad and Gnome to play nice together has been a pain in the
ass for many years, and it doesn't get simpler when you want to build
`~/.xmonad/xmonad.hs` using Stack.

The directory containing this README gets symlinked to `~/.xmonad/` by
`:/install-conf.py`.

See also `~/v/org/notes/fresh-ubuntu-install.org`.

Gnome/GDM Notes
---------------

The new Gnome in 18.04 no longer writes errors to
`~/.xsession-errors`. Instead, the errors appear in
`journalctl`. There's probably a good way to filter them, but I
haven't looked it up yet.

In any case, common errors include:
- an executable specified by a `TryExec` entry is not on the `PATH`
  (of GDM, not the user `PATH`!), and so the corresponding `.desktop`
  file can't be run or corresponding `.session` isn't being displayed
  in the GDM session chooser menu.
- the correct set of Gnome components aren't specified in the
  `.desktop` or `.session` files, and so other `.desktop` files are
  crashing.

New Setup (tested on Ubuntu 18.04)
----------------------------------

### XMonad + Stack

The XMonad + Stack stuff is based on
http://sitr.us/2018/05/13/build-xmonad-with-stack.html.

To build `./xmonad.hs` with Stack I have a `./my-xmonad.cabal` to
specify the deps, a corresponding `./stack.yaml`, and a `./build` that
`xmonad --recompile` calls, which in turn calls `stack bulid`. To get
an initial `xmonad` binary I install the Ubuntu `xmonad` package,
because I need that for the XMonad + Gnome setup anyway, but otherwise
I could just run `stack build` once in here, and then copy the
`my-xmonad` binary to `/usr/bin/xmonad` (note that GDM does not seems
to take the user's `PATH` extensions into account, so putting a
`xmonad` somewhere in my homedir and adding that to `PATH` doesn't
seem to work).

The `:/submodules/gnome-session-xmonad/minimal-install.sh` installs
all of the X11 deps needed to build XMonad, but doesn't install Ubuntu
GHC, since I'm already getting that from Stack.

### XMonad + Gnome

The XMonad + Gnome stuff is based on
https://github.com/Gekkio/gnome-session-xmonad, which I bring in as a
submodule (with some modifications) at
`:/submodules/gnome-session-xmonad`. Just run
`:/submodules/gnome-session-xmonad/minimal-install.sh` to set it all
up.

This sets up a bunch of Gnome/GDM `.session` and `.desktop` files, and
I mostly don't understand it.

Old Setup (tested on Ubuntu 16.04, and many earlier versions)
-------------------------------------------------------------

This doesn't use Stack. Instead, I build XMonad in a Cabal sandbox,
and avoided `xmonad --recompile`, since I rarely rebuild `xmonad`.

For XMonad + Gnome I have an `~/.xsession` that starts a Gnome +
XMonad session using `local-gnome-panel.desktop`,
`local-unity-settings-daemon.desktop`, and
`local-xmonad-windowmanager-provider.desktop` from
`:/dot.local/share/applications`, and `local-xmonad-session.session`
from `:/dot.config/gnome-session/sessions`.

This works fine in Ubuntu 16.04, but in 18.04 Ubuntu no longer uses
Unity, so the `unity-settings-daemon` stuff broke. Instead of trying
to figure what new `.desktop` files I need to create, and what new
magic I need in the `~/.xsession`, for 18.04 I'm just using the new
setup described above.

A nice thing about the old setup is that it doesn't require root. An
unresolved problem with the old setup in 18.04, besides the
`unity-settings-daemon` stuff, is that I can't figure out how to get
GDM or LightDM to provide the option of running a custom
`~/.xsession`.
