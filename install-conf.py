#!/usr/bin/env python

'''Run from your home dir.'''
from subprocess import call
from os import chdir, getcwd, getenv
from os.path import exists

def c(cmd):
    call(cmd,shell=True)

def main():
    """Set up home dir.

    Creates directories, downloads conf, and creates appropriate
    symlinks.  It's idempotent, meaning you can rerun it to update an
    existing install.
    """

    home = getcwd()
    if home != getenv('HOME'):
        raise Exception('You should run this from your home dir.')

    for d in ('versioned', 'local', '.subversion'):
        if not exists(d): c('mkdir %s') % d

    chdir('versioned')

    if not exists('conf'):
        c('svn co https://nathan-collins--conf.googlecode.com/svn conf --username nathan.collins')

    # {ln -T} mean treat destination as a normal file, i.e. don't
    # create file *in* target if target is a dir.  this is needed for
    # the idempotence of {ln ~/versioned/conf/dot.zsh ~/.zsh}.
    for f in ('.emacs', '.pythonrc', '.screenrc', '.subversion/config', '.zsh'):
        from_ = '%(home)s/versioned/conf/dot%(f)s' % locals()
        to = '%(home)s/%(f)s' % locals()
#         if exists(to):
#             c('mv %(to)s %(to)s.backup' % locals())
        c('ln -Tfs %(from_)s %(to)s'  % locals())
    for f in ('.zshrc', '.zshenv'):
        from_ = '%(home)s/.zsh/dot%(f)s' % locals()
        to = '%(home)s/%(f)s' % locals()
        c('ln -Tfs %(from_)s %(to)s'  % locals())

    c('ln -fs %(home)s/versioned/conf/scripts %(home)s/local/' % locals())

    if not exists('%(home)s/local/more-scripts' % locals()):
        print """You may need to download versioned/docs and create a link from versioned/docs/scripts to ~/local/more-scripts."""

if __name__ == '__main__': main()
