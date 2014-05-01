#!/bin/zsh

# XXX: to make this resumable, could instead use a Makefile, with a
# sequence of "checkpoint" targets.

# You can clone the git.git source repo and then checkout a particular
# tag, corresponding to a stable release, but it easy to just choose a
# stable release source tarball

release=1.9.2
prefix=~/local/opt/git-${release}
# Another place to get Git: https://code.google.com/p/git-core/downloads/list
url=https://www.kernel.org/pub/software/scm/git/

cd /tmp
wget $url/git-${release}.tar.gz -O- | tar xzf -
cd git-${release}

# Git actually needs to be installed, not just built.
#
# Instead of configure, we can use `make prefix=...` and then `make
# prefix=... install`.  However, configure automatically checks
# dependencies and disables features for which we don't have the
# corresponding header files.  NOTE: if libcurl-dev is not installed,
# then git is built without http(s) support.  There are multiple
# versions of libcurl-dev, so we have to choose one :P
sudo aptitude install libcurl4-openssl-dev
mkdir -p ~/local/opt
# make configure
# ./configure --prefix=$(readlink -f $prefix)
make prefix=$(readlink -f $prefix)
make prefix=$(readlink -f $prefix) install

# The docs can be built, but that is slow, and requires asciidoc and
# xmlto, which are not installed on linuxlab machines.  Instead, just
# download the prebuilt docs (there is also a
# git-htmldocs-${release}):

(cd $prefix/share/man &&
wget $url/git-manpages-${release}.tar.gz -O- | tar xzf -)

# ZSH configuration.
#
# MANPATH works automatically for sane PATH, so we only need to set
# PATH.

# Git/bin.
#
# Need -T (--no-target-directory) to prevent ln from creating the link
# *inside* an existing git dir when upgrading :P
(cd ~/local/opt && ln -Tfs git-$release git)
# ZSH completion.
cp -r contrib $prefix
(cd $prefix/contrib/completion && ln -fs git-completion.zsh _git)

customize () {
    echo $@ >> ~/.zshenv.system-custom
}
customize ''
customize '# Git'
customize 'export PATH=~/local/opt/git/bin:$PATH'
customize 'export FPATH=~/local/opt/git/contrib/completion:$FPATH'

print -P '%K{red}YOUR ~/.zshenv.system-custom HAS BEEN UPDATED!%k'
