# Fix environment on screen resume.
#
# Things like ssh-agents and $DISPLAY can change each time you ssh
# into a host.  Here nc:genfixssh generates an updated environment and
# nc:fixssh loads the environment.

alias nc:fixssh='source ~/local/bin/fix-ssh.sh'
alias nc:genfixssh='source ~/local/scripts/gen-fix-ssh.sh'
# screen doesn't see the alias defined by genfixssh?
alias nc:screen='nc:genfixssh ; screen'

# Set the title of the screen.
#
# See the 'TITLES (naming windows)' section of 'man screen' for more
# info.  See http://aperiodic.net/phil/prompt/ for some much fancier
# examples.
#
# Another strategy would be to always display the last command run,
# and use a different screen facility to see a list of the current
# directories of each screen ... the idea is to make is easy to know
# which screen to switch to to continue what you were doing or to run
# a new command.
if [[ $TERM == "screen" ]]; then
    precmd () {
        : fix env and display current directory while waiting for commands
        nc:fixssh
        echo -ne "\ek$(basename $(pwd))/\e\\"
        #'screen -X title <title>' also works ...
    }

    preexec () {
        : display current command while it runs
        echo -ne "\ek$1\e\\"
    }
fi
