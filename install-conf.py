#!/usr/bin/env python2

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
    return call(cmd,shell=True,executable='/bin/bash')

def main():
    """Set up home dir.

    Creates directories, downloads conf, and creates appropriate
    symlinks.  It's idempotent, meaning you can rerun it to update an
    existing install.
    """

    home = realpath(getenv('HOME')) 
    chdir(home)

    for d in ('v', 'tmp', 'local', 'local/opt', '.subversion', '.ghc',
              '.emacs.d', '.emacs.d/straight', '.pip',
              '.local/share/applications', '.config/gnome-session/sessions'):
        c('mkdir -p %s' % d)
    chdir('v')

    if not exists('conf'):
        if c('git clone git@github.com:ntc2/conf.git conf') != 0:
            print
            print '\033[5mWarning:\033[0;31m cloning ~/v/conf repo over HTTPS. You need to update'
            print 'the remote to use SSH before you can push without a password.\033[0m'
            c('sleep 5')
            print
            c('git clone https://github.com/ntc2/conf.git conf')
    c('git -C conf submodule update --init')

    # {ln -T} mean treat destination as a normal file, i.e. don't
    # create file *in* target if target is a dir.  this is needed for
    # the idempotence of directory links, e.g. {ln ~/v/conf/dot.zsh ~/.zsh}.
    for f in ('.emacs', '.emacs.d/extensions', '.emacs.d/README', '.emacs.d/straight/versions',
              '.pythonrc', '.screenrc', '.subversion/config',
              '.zsh', '.zshrc', '.zshenv', '.zprofile',
              '.ghc/ghci.conf', '.haskeline',
              '.tridactylrc',
              '.gitconfig', '.gitattributes', '.gitignore',
              '.xmonad',
              '.Xresources',
              '.gdbinit',
              '.tmux.conf',
              '.sqliterc',
              '.pip/pip.conf',
              ):
        from_ = '%(home)s/v/conf/dot%(f)s' % locals()
        to = '%(home)s/%(f)s' % locals()
        c('ln -Tfs %(from_)s %(to)s'  % locals())
    # Create *hard* links. Due to apparmor stupidity, it's not
    # possible to have the conf file be a symlink since Ubuntu 22.04.
    for f in ('.config/redshift.conf', ):
        from_ = '%(home)s/v/conf/dot%(f)s' % locals()
        to = '%(home)s/%(f)s' % locals()
        c('ln -Tf %(from_)s %(to)s'  % locals())

    # misc programs.
    c('ln -fs %(home)s/v/conf/scripts %(home)s/local/' % locals())

    # use zsh?
    shell = getenv('SHELL')
    if 'zsh' not in shell:
        print '''You are using "%(shell)s" but this conf is for zsh.  Changing shell to zsh ...''' % locals()
        c('chsh -s /bin/zsh')

    # Suggest setting xmonad if we have root.
    print 'You may want to run `:/submodules/gnome-session-xmonad/minimal-install.sh`'
    print 'if you have root and want xmonad.'

if __name__ == '__main__': main()
