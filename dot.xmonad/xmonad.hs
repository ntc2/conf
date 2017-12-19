-- Reloading this file (copied from fresh-ubuntu-install.org):
--
-- Install deps if necessary:
--
--     sudo aptitude install libx11-dev libxinerama-dev libxrandr-dev libxft-dev
--     cabal update && cabal install xmonad xmonad-contrib
--
-- Rebuild and reload xmonad after each edit:
--
--     cabal sandbox -- xmonad --recompile && cabal sandbox -- xmonad --restart
--
-- This doesn't change the globally installed xmonad binary, but that
-- doesn't seem to matter.

-- Gnome config now given in org/notes/fresh-ubuntu-install.org.
--
-- Gnome config from (OLD)
-- http://haskell.org/haskellwiki/Xmonad/Basic_Desktop_Environment_Integration
-- see also
-- http://donsbot.wordpress.com/2010/03/13/after-3-years-my-xmonad-configuration-now-uses-gnome/
--
-- need to set up gnome to use xmonad though
-- http://haskell.org/haskellwiki/Xmonad/Using_xmonad_in_Gnome#Starting_GNOME_with_xmonad
-- change the windowmanager used by gnome (and then logout, or start a
-- new gnome with {startx -- :1} in a tty):
-- 
--   gconftool-2 -s /desktop/gnome/session/required_components/windowmanager xmonad --type string
--   gconftool-2 -s /desktop/gnome/session/required_components/windowmanager metacity --type string
--
-- this gives multihead for free.
--
-- TODO
--
-- -  better completion of workspace names: something more like iswitchb in emacs?
--
-- -  tabbing or stacking
--
--    see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SubLayouts.html
--
-- -  disable "smart" workspace switching, whereby xmonad swaps
--    current workspace and target when both appear on monitors
--    currently
--
--    see
--    http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Multi_head_and_workspaces_.28desktops.29
--
-- -  dzen or xmobar
-- 
-- DONE
--
-- -  {aptitude install dmenu}
import XMonad
import XMonad.Config.Gnome (gnomeConfig,gnomeRun)
import qualified XMonad.Actions.PhysicalScreens as PS
import XMonad.Actions.UpdateFocus (adjustEventInput)
import XMonad.Actions.UpdatePointer -- mouse follows focus
--import XMonad.Layout.TabBarDecoration -- tabs: sucks by default: the tabs don't do anything :P
import XMonad.Hooks.ManageHelpers -- fullscreen flash Using
-- gnomeConfig and desktopLayoutModifiers take care of avoidStruts and
-- more. See XMonad.Config.Desktop docs.
import XMonad.Hooks.ManageDocks (SetStruts(..),ToggleStruts(..),Direction2D(..),avoidStrutsOn)
import XMonad.Config.Desktop (desktopLayoutModifiers) -- custom layoutHook + gnome


-- Get a prompt for running XMonad X () actions interactively.
import XMonad.Prompt
import XMonad.Prompt.XMonad

-- * Window Layouts
--
-- ** Resizeable Tall Layout
import XMonad.Layout.ResizableTile
  ( ResizableTall(..)
  , MirrorResize(MirrorShrink,MirrorExpand) )
-- ** SubLayouts (UNUSED)
--
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SubLayouts.html
--
-- and below in layoutHook.
import XMonad.Layout.BoringWindows
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

-- * Named workspaces
--
-- ** Send windows to named workspaces
--
--  http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#15
import qualified XMonad.StackSet as W
-- ** Change to workspace by name
--
-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Prompt-Workspace.html
-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#10
import XMonad.Prompt
import XMonad.Prompt.Workspace (workspacePrompt)
-- ** Create new named workspaces
import XMonad.Actions.DynamicWorkspaces
-- See docs:
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CopyWindow.html
-- Lots of good stuff there, e.g. copy firefox to current workspace,
-- do what you want, and then delete copy.
import XMonad.Actions.CopyWindow (copy)

-- from http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CycleWindows.html
import XMonad.Actions.CycleWindows

-- * Work space selection
import XMonad.Actions.CycleWS (toggleWS)

import qualified Data.Map as M


myDefaultConfig = gnomeConfig -- defaultConfig
main = xmonad myDefaultConfig
       { logHook = logHook myDefaultConfig
                   -- mouse follows focus to middle-top
                   >> updatePointer (0.5, 0.0) (0.0, 0.0)
         -- i have two mod keys: the left
         -- windows key (Super_L) and the right menu key (Menu)
         -- 
         -- - use xmodmap to get list of modifiers, e.g. Super_L is a
         --   mod4 modifier.
         --
         -- - use xev to find keysym for a given key.
         --
         -- - use xmodmap to add keys to a modifier, and remap keys,
         --   e.g. adding Menu to mod4:
         --
         --   : xmodmap -e "add Mod4 = Menu"
         --   : xmodmap -e "keysym Menu = Super_L"
         --
         --   the second line alone may be enough, but I didn't do
         --   things in that order ... YES!
       , modMask     = mod4Mask
       , startupHook = myStartupHook
       , manageHook  = myManageHook -- full-screen flash
                       <+> manageHook myDefaultConfig
                       -- no borders for single window
       , layoutHook  = id -- windowNavigation $ subTabbed $ boringWindows

                       -- !!!: Here I say to avoid *no* struts, but
                       -- leaving this out means strut toggling
                       -- operations (M-b an C-M-b) don't work
                       -- ... seems like a bug.
                       . avoidStrutsOn []
                       . smartBorders
                       $ Full ||| tall ||| Mirror tall
       , workspaces = myWorkspaces
       , keys = myKeys
       , terminal = "urxvt" -- "xterm" -- Had trouble with jumpscrolling not working in xterm.
       }
  where
    -- Two master panes, 1/10th resize increment, only show master
    -- panes by default. Unlike plain 'Tall', this also allows
    -- resizing the master panes, via the 'MirrorShrink' and
    -- 'MirrorExpand' messages.
    tall = ResizableTall 2 (1/10) 1 []
    -- Two master panes, 1/10th resize increment, only show master
    -- panes by default.
    --tall = Tall 2 (1/10) 1

-- fullscreen
--
-- - flash in youtube: this managehook fixes it
--
-- - mplayer: add {-fstype none -fs} to your command line.  here's a
--   full command line for playing a dvd, and with panscan enabled
--   ('w' and 'e') for zooming a letter box format:
--
--     mplayer -dvd-device /dev/dvd3 dvd://1 -fstype none -fs -vo xv -panscan 1 -panscanrange 3
fullscreenHook = composeOne [ isFullscreen -?> doFullFloat ]

-- named workspaces -- XXX: better to create this automatically *only when* there are windows to put there.
namedWorkspaces = ["scratch", "web", "update"]
-- !!!: there is a bug in xmonad + xinerama whereby fewer initial
-- workspaces than xinerama screens results in xmonad failing at
-- startup with
--
--   xmonad-i386-linux: xmonad: StackSet: non-positive argument to StackSet.new
--
-- in ~/.xsession-errors.
--
-- Related: to restart xmonad from a tty after hosing your graphical set-up with:
--
--   DISPLAY=:0 xmonad --recompile; DISPLAY=:0 xmonad --restart; DISPLAY=:0 xmonad --replace
--
-- This is better than C-M-BACKSPACE since you don't lose your open
-- windows.  Some subset of those commands should be sufficient ...
myWorkspaces = -- map show [1 .. (9 - length namedWorkspaces)] ++
               namedWorkspaces
-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#15
-- Use 'xprop' to find the resource strings: run 'xprop' and then click on a window of the app in question.
namedWorkspaceHook = composeAll [ resource =? "update-manager" --> doF (W.shift "update") 
                                , resource =? "synaptic"       --> doF (W.shift "update")
                                , isFirefoxBrowserWindow       --> doF (W.shift "web")
                                ]
  where
    -- Don't want to match dialogs, since they should stay with the
    -- window that generates them, which isn't always in the "web"
    -- workspace..
    --
    -- Based on
    -- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples.
    --
    -- Using 'xprop', I see
    --
    --     WM_CLASS(STRING) = "Navigator", "Firefox"
    --
    -- for the browser windows and
    --
    --     WM_CLASS(STRING) = "Dialog", "Firefox"
    --
    -- and
    --
    --     WM_CLASS(STRING) = "firefox", "Firefox"
    --
    -- for the dialogs (save-or-open dialog, and save-as dialog,
    -- respectively). So, I'm guessing the first part is the
    -- "resource" and the second part is the "class name".
    isFirefoxBrowserWindow = className =? "Firefox" <&&> resource =? "Navigator"

myManageHook = fullscreenHook <+> namedWorkspaceHook

-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#10
-- but use gnomeConfig instead of defaultConfig
--
-- I guess this is where my caps-lock = control comes from?
--
-- See EZConfig lib for emacs style key specs.
myKeys x = M.fromList (newKeys x) `M.union` keys myDefaultConfig x
newKeys conf@(XConfig {XMonad.modMask = modm}) =
             -- * Cycle windows
             [ ((modm                , xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
             , ((modm                , xK_z), rotOpposite)
             , ((modm .|. controlMask, xK_i), rotUnfocusedUp)
             , ((modm .|. controlMask, xK_u), rotUnfocusedDown)
             , ((modm                , xK_i), rotFocusedUp)
             , ((modm                , xK_u), rotFocusedDown)

             -- From XMonad.Prompt.XMonad:
             , ((modm                , xK_x), xmonadPrompt defaultXPConfig)

             -- * Cycle workspace
             , ((modm,               xK_Tab), toggleWS)

             -- * Resize windows
             , ((modm,               xK_Left),  sendMessage MirrorExpand)
             , ((modm,               xK_Up),    sendMessage MirrorExpand)
             , ((modm,               xK_Right), sendMessage MirrorShrink)
             , ((modm,               xK_Down),  sendMessage MirrorShrink)
             -- * Rebind launcher.
             --
             -- Work around for idiotic ubuntu bug: Mod4+p is
             -- hardcoded to toggle display mode, for *all* users on
             -- all platforms, because some laptops work this way.
             --
             -- See:
             --
             -- - Ubuntu bug report:
             --   https://bugs.launchpad.net/ubuntu/+source/gnome-settings-daemon/+bug/694910
             -- - Not sure if there's a good way to reply to this, but
             --   could encourage people to mark the ubuntu bug as "affects me"
             --   http://www.haskell.org/pipermail/xmonad/2011-March/011161.html

             -- Original 'keys' defined in
             -- http://hackage.haskell.org/packages/archive/xmonad/0.10/doc/html/src/XMonad-Config.html#defaultConfig
             -- and in 
             -- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Gnome.html#gnomeConfig
             -- for gnome:
             --
             --  , ((modm, xK_p), gnomeRun)
             , ((modm, xK_F4), gnomeRun)

             -- * Struts
             --
             -- Toggle all struts.
             , ((modm .|. controlMask, xK_b), sendMessage $ ToggleStruts)
             -- Toggle top strut. I usually don't actually care about
             -- the bottom strut.
             , ((modm,                 xK_b), sendMessage $ ToggleStrut U)
             -- Disable all struts.
             -- , ((modm .|. controlMask, xK_b), sendMessage $ SetStruts [] [minBound .. maxBound])
             -- , ((modm                , xK_x), evalPrompt defaultEvalConfig defaultXPConfig)

             -- * Named workspaces
             --
             -- Bindings from http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-DynamicWorkspaces.html

               -- Remove current.  Moves windows to another workspace
             , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
               -- Rename given workspace.
             , ((modm .|. controlMask, xK_r      ), renameWorkspace defaultXPConfig)
               -- Select by name. If hitting tab is annoying, see
               -- http://www.haskell.org/pipermail/xmonad/2011-April/011319.html
             , ((modm              , xK_g        ), selectWorkspace defaultXPConfig)
               -- Move window.
             , ((modm .|. shiftMask, xK_g        ), withWorkspace defaultXPConfig (windows . W.shift))
               -- Copy window.
             --, ((modm .|. shiftMask, xK_m        ), withWorkspace defaultXPConfig (windows . copy))

             -- Prompt for named workspace to switch to. Can use
             -- W.shift instead to move current window to named
             -- workspace.  Version above is better: creates workspace
             -- if it doesn't exist.
             --
             -- , ((modm, xK_g), workspacePrompt defaultXPConfig (windows . W.view))
             ]
             -- * Override usual M-<n>/M-S-<n> workspace bindings.
             --
             -- From from
             -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-DynamicWorkspaces.html
             --
             -- mod-[1..9]       %! Switch to workspace N
             -- mod-shift-[1..9] %! Move client to workspace N
             ++
             zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
             ++
             zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
             ++
             -- Rebind @M-{w,e,r}@ physical, left-to-right screen
             -- order. This avoids the problem with the default
             -- behavior where the @M-{w,e,r}@ bindings don't always
             -- correspond to the left-to-right order of the screens.
             --
             -- Based on
             -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-PhysicalScreens.html.
             --
             -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
             -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
             --
             -- NOTE: the screen order is determined by upper left
             -- corner, FIRST TOP TO BOTTOM, and then left to
             -- right. so, if you use the Gnome GUI to arrange your
             -- displays, you might accidentally make one a little
             -- lower than the others, leading to confusion when it
             -- appears out of order.
             [ ((modm .|. mask, key), f sc)
             | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
             , (f, mask) <- [ (PS.viewScreen, 0)
                            , (PS.sendToScreen, shiftMask) ] ]

-- this is probably not the right way: sometimes needs a mod-q to reload the xmodmap setting?
--       , startupHook = spawn "xmodmap -e \"keysym Menu = Super_L\""
myStartupHook = do
  startupHook myDefaultConfig
  adjustEventInput
  spawn "~/.xmonad/startup-hook.sh"


-- Hide/show all gaps. From XMonad.Hooks.ManageDocks docs.
-- hideStruts = sendMessage $ SetStruts [] [minBound .. maxBound]
-- showStruts = sendMessage $ SetStruts [minBound .. maxBound] [] 

-- http://hackage.haskell.org/packages/archive/xmonad-contrib/latest/doc/html/XMonad-Layout-Tabbed.html
-- http://haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Multi_head_and_workspaces_.28desktops.29
-- 

-- complicated config, could be educational:
--
-- http://snipt.net/doitian/xmonad-configuration/

-- Another config with lots of good stuff.  Has lots of comments, and
-- cool named workspace set-up and search engines (I have similar in
-- vimperator, but I have to recreate them on each new vimperator
-- install).
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
