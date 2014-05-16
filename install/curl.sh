#!/bin/zsh

# Install curl locally.
#
# Git needs libcurl for http(s) URL support.

release=7.36.0
prefix=~/local/opt/curl-${release}
# Another place to get Git: https://code.google.com/p/git-core/downloads/list
url=http://curl.haxx.se/download/curl-${release}.tar.gz

cd /tmp
wget $url -O- | tar xzf -
cd curl-${release}

mkdir -p ~/local/opt

./configure --prefix=$prefix
make -j && make -j install

# Curl is now installed ${prefix}. I only care about libcurl right
# now, so not bothering with PATH or curl -> curl-$release link.
