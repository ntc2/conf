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


################################################################
# libcurl
#
# If libcurl is not installed and I have root:
#
# $ sudo aptitude install libcurl4-openssl-dev
#
# If I don't have root, build w/o curl support (so no http(s) URLs):
#
#flair="NO_CURL=1"
#
# Or, install curl locallys:
#
# $ ./curl.sh
#
# and then:
#
# (I just had my mind blown: in zsh, strings with spaces are preserved
# when substituted, unless 'SH_WORD_SPLIT' is set! This explains some
# confusion I've had in the past ... however, using arrays (space
# separated values in parens) in zsh makes splitting happen as in bash
# and sh. Splitting is important for the 'flair' variable!
# http://zsh.sourceforge.net/FAQ/zshfaq03.html)
#
# The 'NO_R_TO_GCC_LINKER' is an obscure option I found grepping the
# Git Makefile after getting an error about "cc: ... unrecognized -R"
# or something. Only happens when 'CURLDIR' is set.
#
# Also, unsurprisingly but causing me confusion: the tilde is not
# expanded after the equal sign in
# 'CURLDIR=~/local/opt/curl-7.36.0'. This makes sense: tildes
# shouldn't be expanded arbitrarily. The confusing part is that if the
# shell is were interpreting that assignment, then the tilde would be
# expanded. Tricky tricky ...
#
#flair=(NO_R_TO_GCC_LINKER=1 CURLDIR=$HOME/local/opt/curl-7.36.0)
#
################################################################

mkdir -p ~/local/opt
# make configure
# ./configure --prefix=$(readlink -f $prefix)
make -j $flair prefix=$(readlink -f $prefix) && \
make -j $flair prefix=$(readlink -f $prefix) install

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

if ! grep -q '^# Git$' ~/.zshenv.system-custom; then

    customize () {
	echo $@ >> ~/.zshenv.system-custom
    }
    customize ''
    customize '# Git'
    customize 'export PATH=~/local/opt/git/bin:$PATH'
    customize 'export FPATH=~/local/opt/git/contrib/completion:$FPATH'

    print -P '%K{red}YOUR ~/.zshenv.system-custom HAS BEEN UPDATED!%k'
else
    print -P '%K{red}YOUR ~/.zshenv.system-custom already has custom Git%k'
fi
