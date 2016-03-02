## Prompt

# Set up the prompt.  I think it would be inherited, but a sys script
# (/etc/bash.bashrc) overrides it for each new shell.

# By default zsh backs up to the beginning of the line (opt name
# implies it's a carriage return) before displaying the prompt, so
# output that doesn't end in newline gets covered up, e.g. if you echo
# -n <some string> then <some string> gets covered by the prompt.
# This can be confusing, so we turn the "feature" off.
setopt nopromptcr

# Allow command substitution in prompt. Without this, commands in
# prompt will only be run once.
setopt prompt_subst

# Copied from
# http://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh,
# which is essentially copied from the ZSH docs:
# http://zsh.sourceforge.net/Doc/Release/User-Contributions.html
#
# The '%F{n}' escapes turn on foreground ANSI color 'n', and '%f'
# turns off foreground color.  The other escapes are specific to the
# 'vcs_info', e.g. '%b' for branch and '%r' for revision.
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
    '[%F{magenta}%s%f:%F{2}%b%f %K{1}%a%k%u%c%m]'
zstyle ':vcs_info:*' formats       \
    '[%F{magenta}%s%f:%F{2}%b%F{5}%f%u%c%m]'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%f:%F{yellow}%r%f'
zstyle ':vcs_info:*' enable git svn

# This is disabled by default, because it can be slow for large repos
# ...
zstyle ':vcs_info:*' check-for-changes true
# Enable %i to refer to revision in git and hg.
#zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' unstagedstr ' %F{red}M%f'
zstyle ':vcs_info:*' stagedstr ' %F{green}M%f'

# DEBUGGING.
#zstyle ':vcs_info:*+*:*' debug true

# Following hooks based on
# https://github.com/zsh-users/zsh/blob/master/Misc/vcs_info-examples

### Display the existence of files not yet known to VCS

### git: Show marker (T) if there are untracked files in repository
# Make sure you have added unstaged to your 'formats': %u
+vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null ; then
        # This will show the marker if there are any untracked files in repo.
        # If instead you want to show the marker only if there are untracked
        # files in $PWD, use:
        #[[ -n $(git ls-files --others --exclude-standard) ]] ; then
        hook_com[unstaged]+=' %F{red}?%f'
    fi
}

+vi-svn-untracked() {
    if $(svn info &> /dev/null); then
        if svn status | grep '^?' &> /dev/null ; then
        # This will show the marker if there are any untracked files in repo.
        # If instead you want to show the marker only if there are untracked
        # files in $PWD, use:
        #[[ -n $(git ls-files --others --exclude-standard) ]] ; then
            hook_com[unstaged]+=' %F{red}?%f'
        fi
        if svn status | grep '^[MDA!]' &> /dev/null ; then
            hook_com[unstaged]+=' %F{red}M%f'
        fi
    fi
}

### Compare local changes to remote changes

### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
# Make sure you have added misc to your 'formats': %m
function +vi-git-st() {
    local ahead behind
    local -a gitstatus

    # for git prior to 1.7
    # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    (( $ahead )) && gitstatus+=( "%F{green}+%f${ahead}" )

    # for git prior to 1.7
    # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
    (( $behind )) && gitstatus+=( "%F{red}-%f${behind}" )

    #hook_com[misc]+=" "${(j:/:)gitstatus}
    [[ -n "$gitstatus" ]] && hook_com[misc]+=" "${(j::)gitstatus}
}

function +vi-git-set-message { +vi-git-st ; +vi-git-untracked ; }

# Set hooks.
zstyle ':vcs_info:git*+set-message:*' hooks git-set-message
zstyle ':vcs_info:svn*+set-message:*' hooks svn-untracked

# Print vcs_info on it's own line.
function _nc:vcs_info {
    vcs_info
    if [[ -n "$vcs_info_msg_0_" ]]; then
        echo -e "\n$vcs_info_msg_0_"
    fi
}

# The green block on the last line has a point: the command still
# stands out from the output, but triple-click-to-copy also copies a
# valid command, whereas a leading '$' makes the command invalid when
# copied.
#
# The '%(?..[$rd%?$pl])' prints the previous command's exit code in
# red when non-zero. See Section 'CONDITIONAL SUBSTRINGS IN PROMPTS'
# in 'man zshall' for '%(..)' ternary op docs.
# http://stackoverflow.com/questions/4466245/customize-zshs-prompt-when-displaying-previous-command-exit-code
#
# Strange: at some point in early 2014 the '%m' (unqualified hostname)
# started printing as 'linux', and '%M' as 'linux.cecs.pdx.edu' ???
# Easy test with 'print -P %M'.
#
# The 'nc_timer_dt' is computed below in 'nc:timer:preexec' and
# 'nc:timer:precmd'.
PS1='[$rd%n$pl@$gr$(hostname)$pl][$gr%~$pl][%%$rd%j$pl][%*]%(?..[?$rd%?$pl])[%F{yellow}$nc_timer_dt%fs]\
$(_nc:vcs_info)
%K{green} %k'

PS2="$gr%_$pl> "

################################################################
# Prompt hooks
#
# See "SPECIAL FUNCTIONS -> Hook Functions" in 'man zshall'. The
# 'precmd' is run before each prompt is drawn, and the 'preexec' is
# run before each command is run, but after each prompt is drawn.

################################################################
# Optionally reload all shells.

zsh_load_time=$(date +%s)
function maybe_reload_zsh {
  : "Reload ZSH if '~/.zsh.reload' exists and has been touched"
  : "since ZSH was started."
  if [[ -e ~/.zsh.reload ]] && \
     (( $(date --reference ~/.zsh.reload +%s) > $zsh_load_time )); then
    print -P "%K{red}Reloading ZSH because ~/.zsh.reload has been touched!%k" >&2
    # Don't want to simply do 'source ~/.zshrc' since my configs are
    # not idempotent. So, restart ZSH with 'exec zsh'. However, we
    # starting a new ZSH forgets the current command (in "$3"), so we
    # have to run the command manually. Finally, when run with '-c
    # <cmd>', ZSH exits after running '<cmd>', so we nest another
    # 'exec zsh' to keep zsh running :P
    exec zsh -c "$3; exec zsh"
  fi
}
preexec_functions+=(maybe_reload_zsh)

function nc:reload-all-shells {
  : "Reload all interactive ZSH instances before they execute"
  : "their next command".
  touch ~/.zsh.reload
}

################################################################
# Time each command as it runs.

# See time of last command in prompt, based on:
# https://coderwall.com/p/kmchbw/zsh-display-commands-runtime-in-prompt
# The 'nc_timer_dt' is used in 'PS1' above.
function nc:timer:preexec () {
  nc_timer=$SECONDS
}
preexec_functions+=(nc:timer:preexec)

function nc:timer:precmd () {
  # The 'preexec' runs right before a command is executed. However, if
  # you just hit ENTER with no command, then the 'precmd' runs without
  # an intervening 'preexec'. So, we unset 'nc_timer' after drawing
  # the prompt, so that it will only be set when a command has been
  # run since the last prompt was drawn. If no command has been run
  # since last prompt, then we preserve the existing dt -- just like
  # '$?' preserves the existing exit code -- default dt to zero when
  # the shell first starts.
  if ((${+nc_timer})); then
    nc_timer_dt=$(($SECONDS - $nc_timer))
  # Set to 0 if unset.
  elif ! ((${+nc_timer_dt})); then
    nc_timer_dt=0
  fi
  unset nc_timer
}
precmd_functions+=(nc:timer:precmd)

# http://stackoverflow.com/questions/2704635/is-there-a-way-to-find-the-running-time-of-the-last-executed-command-in-the-shel
#
# Show 'time' stats for last command if it consumed more than
# specified number of seconds of *CPU* time (Note: *not* wall time).
export REPORTTIME=60
# Show time for all commands in history; use 'history -D' to see the times.
setopt inc_append_history_time
