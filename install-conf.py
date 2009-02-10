'''Run from your home dir.'''
from subprocess import call
from os import chdir, getcwd, getenv
from os.path import exists

def c(cmd):
    call(cmd,shell=True)

home = getcwd()
if home != getenv('HOME'):
    raise Exception('You should run this from your home dir.')
c('mkdir versioned local .subversion')
chdir('versioned')
c('svn co https://nathan-collins--conf.googlecode.com/svn conf --username nathan.collins')
for f in ('.emacs', '.pythonrc', '.screenrc', '.subversion/config', '.zshrc'):
    from_ = '%(home)s/versioned/conf/dot%(f)s' % locals()
    to = '%(home)s/%(f)s' % locals()
    if exists(to):
        c('mv %(to)s %(to)s.backup' % locals())
    c('ln -s %(from_)s %(to)s'  % locals())
c('ln -s %(home)s/versioned/conf/scripts %(home)s/local/' % locals())
