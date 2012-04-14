-- Gnome config from
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
import XMonad.Config.Gnome
import XMonad.Actions.UpdatePointer -- mouse follows focus
--import XMonad.Layout.TabBarDecoration -- tabs: sucks by default: the tabs don't do anything :P
import XMonad.Hooks.ManageHelpers -- fullscreen flash
import XMonad.Config.Desktop (desktopLayoutModifiers) -- custom layoutHook + gnome
import XMonad.Layout.NoBorders (smartBorders)

-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SubLayouts.html
--
-- and below in layoutHook.  Not actually doing anything right now ...
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows

-- named workspaces
-- ================
--
-- send windows to named workspaces
--
--  http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#15
import qualified XMonad.StackSet as W
-- function to change to workspace by name
--
--   http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Prompt-Workspace.html
import XMonad.Prompt
import XMonad.Prompt.Workspace (workspacePrompt)
-- add keybinding to change to workspace by name function
--
--   http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#10
import qualified Data.Map as M

-- from http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CycleWindows.html
import XMonad.Actions.CycleWindows

myDefaultConfig = gnomeConfig -- defaultConfig
main = xmonad myDefaultConfig
       { logHook = logHook myDefaultConfig
                   -- mouse follows focus to middle-top
                   >> updatePointer (Relative 0.5 0.0) 
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
-- this is probably not the right way: sometimes needs a mod-q to reload the xmodmap setting?
--       , startupHook = spawn "xmodmap -e \"keysym Menu = Super_L\""
       , manageHook  = myManageHook -- full-screen flash
                       <+> manageHook myDefaultConfig
                       -- no borders for single window
       , layoutHook  = id -- windowNavigation $ subTabbed $ boringWindows
                       $ smartBorders
         -- http://haskell.cs.yale.edu/haskellwiki/Xmonad/Using_xmonad_in_Gnome#Layouts
                       $ desktopLayoutModifiers 
                       $ Full ||| tall ||| Mirror tall
--       , layoutHook = simpleTabBar $ layoutHook gnomeConfig
       , workspaces = myWorkspaces
       , keys = myKeys
       }

-- two master, 1/10th resize increment, only show master by default
tall = Tall 2 (1/10) 1

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

-- named workspaces
namedWorkspaces = ["update"]
-- put the named workspaces at the end of the list: ??? turns out
-- there are still 9 numeric workspaces ???
myWorkspaces = map show [1 .. (9 - length namedWorkspaces)] ++ namedWorkspaces
-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#15
-- use xprop to find the resource strings
namedWorkspaceHook = composeAll [ resource =? "update-manager" --> doF (W.shift "update") 
                                , resource =? "synaptic"       --> doF (W.shift "update")
                                ]

myManageHook = fullscreenHook <+> namedWorkspaceHook

-- http://hackage.haskell.org/packages/archive/xmonad-contrib/0.9.1/doc/html/XMonad-Doc-Extending.html#10
-- but use gnomeConfig instead of defaultConfig
--
-- I guess this is where my caps-lock = control comes from?
myKeys x = M.union (keys myDefaultConfig x) (M.fromList (newKeys x))
newKeys conf@(XConfig {XMonad.modMask = modm}) =
             [ {- ((modm, xK_F12), xmonadPrompt defaultXPConfig)
             , ((modm, xK_F3 ), shellPrompt  defaultXPConfig)
                       -- "go"                                          -- use W.shift instead
                                                                        -- to move current window
             ,-} ((modm, xK_g), workspacePrompt defaultXPConfig (windows . W.view))

-- import XMonad.Actions.CycleWindows
             , ((mod4Mask,  xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
             , ((modm, xK_z),                 rotOpposite)
             , ((modm                , xK_i), rotUnfocusedUp)
             , ((modm                , xK_u), rotUnfocusedDown)
             , ((modm .|. controlMask, xK_i), rotFocusedUp)
             , ((modm .|. controlMask, xK_u), rotFocusedDown)
             ]

-- http://hackage.haskell.org/packages/archive/xmonad-contrib/latest/doc/html/XMonad-Layout-Tabbed.html
-- http://haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Multi_head_and_workspaces_.28desktops.29
-- 

-- complicated config, could be educational:
--
-- http://snipt.net/doitian/xmonad-configuration/
