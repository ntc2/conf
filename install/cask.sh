#! /bin/bash

if [ -d ~/local/opt/cask.git ]; then
  echo "Cask is already installed!" > /dev/stderr
  echo "You could still run 'cask upgrade-cask' ..." > /dev/stderr
  exit 1
fi

mkdir -p ~/local/opt
cd ~/local/opt
git clone https://github.com/cask/cask.git cask.git
./cask.git/bin/cask upgrade-cask

customize () {
  echo $@ >> ~/.zshenv.system-custom
}
customize ''
customize '# Cask'
customize 'export PATH=~/local/opt/cask.git/bin:$PATH'

echo "Run nc:reload-all-shells to add Cask to your PATH."
