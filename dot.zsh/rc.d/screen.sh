alias fixssh='source ~/local/bin/fix-ssh.sh'
alias genfixssh='source ~/local/scripts/gen-fix-ssh.sh'
# screen doesn't see the alias defined by genfixssh?
alias attach='genfixssh ; screen -dRR'
