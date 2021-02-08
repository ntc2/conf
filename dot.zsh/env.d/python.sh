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

if [[ -e "$HOME/.pyenv/bin" ]]; then
  export PATH="$HOME/.pyenv/bin:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi
