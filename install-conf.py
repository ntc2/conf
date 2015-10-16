#!/usr/bin/env python

'''Run from home dir to (re)install conf files.

NB: this install script will check out a copy of the conf repo
containing it, so the easiest way to do an initial install is to copy
this file (install-conf.py) from an existing install into the new home
dir and run the copy.

The directory v(ersioned) contains working copies.'''
from subprocess import call
from os import chdir, getenv
from os.path import exists, realpath

def c(cmd):
    return call(cmd,shell=True)

def main():
    """Set up home dir.

    Creates directories, downloads conf, and creates appropriate
    symlinks.  It's idempotent, meaning you can rerun it to update an
    existing install.
    """

    home = realpath(getenv('HOME')) 
    chdir(home)

    for d in ('v', 'tmp', 'local', 'local/opt', '.subversion', '.ghc', '.emacs.d',
              '.xmonad', '.local/share/applications', '.config/gnome-session/sessions'):
        if not exists(d): c('mkdir -p %s' % d)
    chdir('v')

    if not exists('conf'):
        if c('git clone git@github.com:ntc2/conf.git conf') != 0:
            print
            print '\033[5mWarning:\033[0;31m cloning ~/v/conf repo over HTTPS. You need to update'
            print 'the remote to use SSH before you can push without a password.\033[0m'
            c('sleep 5')
            print
            c('git clone https://github.com/ntc2/conf.git conf')

    # {ln -T} mean treat destination as a normal file, i.e. don't
    # create file *in* target if target is a dir.  this is needed for
    # the idempotence of {ln ~/v/conf/dot.zsh ~/.zsh}.
    for f in ('.emacs', '.pythonrc', '.screenrc', '.subversion/config',
              '.zsh', '.zshrc', '.zshenv', '.zprofile',
              '.ghc/ghci.conf', '.ghc/ghci-prompt.conf', '.haskeline',
              '.vimperatorrc',
              '.gitconfig', '.gitattributes',
              '.xmonad/xmonad.hs', '.xmonad/startup-hook.sh',
              '.Xresources', '.xsession',
              '.config/gnome-session/sessions/local-xmonad-session.session',
              '.local/share/applications/local-xmonad-windowmanager-provider.desktop',
              '.local/share/applications/local-gnome-panel.desktop',
              '.local/share/applications/local-gnome-settings-daemon.desktop',
              '.gdbinit'
              ):
        from_ = '%(home)s/v/conf/dot%(f)s' % locals()
        to = '%(home)s/%(f)s' % locals()
#         if exists(to):
#             c('mv %(to)s %(to)s.backup' % locals())
        c('ln -Tfs %(from_)s %(to)s'  % locals())
    # for f in ('.zshrc', '.zshenv'):
    #     from_ = '%(home)s/.zsh/dot%(f)s' % locals()
    #     to = '%(home)s/%(f)s' % locals()
    #     c('ln -Tfs %(from_)s %(to)s'  % locals())

    # emacs extensions.
    for f in ('extensions', 'Cask'):
        c('ln -fs %(home)s/v/conf/dot.emacs.d/%(f)s %(home)s/.emacs.d/'
          % locals())
    # Install haskell-mode via darcs. Managed by el-get now.
    #
    # to = '%(home)s/local/opt/haskellmode-emacs' % locals()
    # if not exists(to):
    #     # tags are listed on the main page
    #     # http://projects.haskell.org/haskellmode-emacs/, or use
    #     # 'darcs list tags'.
    #     if c('which darcs') == 0:
    #         print 'Downloading haskell-mode ...'
    #         c('darcs get --lazy --tag 2.8.0 \
    #             http://code.haskell.org/haskellmode-emacs %(to)s'
    #           % locals())
    #         c('cd %(to)s && make' % locals())
    #     else:
    #         print 'Error downloading haskellmode-emacs.  Do you have darcs installed?'

    # misc programs.
    c('ln -fs %(home)s/v/conf/scripts %(home)s/local/' % locals())

    # use zsh?
    shell = getenv('SHELL')
    if 'zsh' not in shell:
        print '''You are using "%(shell)s" but this conf is for zsh.  Changing shell to zsh ...''' % locals()
        c('chsh -s /bin/zsh')

if __name__ == '__main__': main()
