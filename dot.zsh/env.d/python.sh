# See ~/.zshenv.
typeset -TU PYTHONPATH pythonpath
export PYTHONPATH=$HOME/local/scripts:$PYTHONPATH

# Doesn't work with ~ in path.
export PYTHONSTARTUP="$HOME/.pythonrc"

# Keep .pyc files in one place (Python 3.8 and later). This stops
# Python from littering project dirs with __pycache__ directories
export PYTHONPYCACHEPREFIX="$HOME/.cache/pycache/"

# Pyenv: Install Python versions locally.

# Setup pyenv.
function nc:install-pyenv () {
  # Run 'pyenv doctor' to check that all build deps are
  # installed. Otherwise when pyenv builds a python it will just
  # disable the features with no corresponding build dep.
  sudo apt install libsqlite3-dev libbz2-dev
  # See https://github.com/pyenv/pyenv-installer .
  curl https://pyenv.run | bash
  echo "E.g. 'pyenv install 3.9.1' to install Python 3.9.1"
}

function nc:upgrade-pyenv () {
  echo "You need to 'rm -rf ~/.pyenv' and then run nc:install-pyenv."
}

if [[ -e "$HOME/.pyenv" ]]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# This helps keep the virtual env path settings at the front of the
# PATH, e.g. inside shells started by tmux. We use typeset -U PATH to
# make the path elements unique, but redundant additions still move
# the addition to the front of the path.
if [[ -n "$VIRTUAL_ENV" && -e "$VIRTUAL_ENV/bin/activate" ]]; then
  # This breaks projectile.
  #echo "Sourcing $VIRTUAL_ENV/bin/activate in ~/v/conf/dot.zsh/env.d/python.sh ..." >&2
  source "$VIRTUAL_ENV/bin/activate"
fi
