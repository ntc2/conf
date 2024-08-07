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

### git: Show marker (?) if there are untracked files in repository
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
#
# Make sure you have added misc to your 'formats': %m
#
# Now that I'm using a "push remote" distinct from "upstream" for
# feature branches [1], I want to show the diversion from the
# push-remote and upstream separately, and make it clear that any
# diversion from upstream is less relevant when a separate push remote
# is set.
#
# When no push remote is set, the format is the same as always:
#
#     +A-B
#
# for A the number of commits and ahead and B the number of commits
# behind upstream. However, when a push remote is set, I instead show
#
#     p+A-B (u+A-B)
#
# In both cases, I omit any part where both A and B are zero.
#
# [1]: push remotes are described in the Magit docs:
# - https://magit.vc/manual/magit/The-Two-Remotes.html
# - https://magit.vc/manual/magit/Branch-Git-Variables.html
function +vi-git-st() {
    local u_ahead=0 u_behind=0 push_remote p_ahead=0 p_behind=0
    local -a gitstatus

    # Calculate push-remote (p) deltas.
    push_remote=$(git config --get branch.${hook_com[branch]}.pushRemote)
    if [[ -n "$push_remote" ]]; then
      p_ahead=$(git rev-list $push_remote/${hook_com[branch]}..HEAD 2>/dev/null | wc -l)
      p_behind=$(git rev-list HEAD..$push_remote/${hook_com[branch]} 2>/dev/null | wc -l)
    fi

    # Calculate upstream (u) deltas.
    u_ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    u_behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)

    # Show p deltas prefixed with "p".
    (( $p_ahead + $p_behind )) && gitstatus+=( "%F{yellow}p%f" )
    (( $p_ahead )) && gitstatus+=( "%F{green}+%f${p_ahead}" )
    (( $p_behind )) && gitstatus+=( "%F{red}-%f${p_behind}" )

    # Add a padding space if both p and u deltas will be shown.
    (( $p_ahead + $p_behind )) && (( $u_ahead + $u_behind )) && gitstatus+=( " " )

    # Show u deltas. If a push remote is set, then prefix with "u" and
    # wrap in parens, to emphasize that this delta might be less
    # important: the ahead count is certainly less important in this
    # case, since we can't do anything about it until the feature
    # branch gets merged.
    [[ -n $push_remote ]] && (( $u_ahead + $u_behind )) && gitstatus+=( "(%F{yellow}u%f" )
    (( $u_ahead )) && gitstatus+=( "%F{green}+%f${u_ahead}" )
    (( $u_behind )) && gitstatus+=( "%F{red}-%f${u_behind}" )
    [[ -n $push_remote ]] && (( $u_ahead + $u_behind )) && gitstatus+=( ")" )

    [[ -n "$gitstatus" ]] && hook_com[misc]+=" "${(j::)gitstatus}
}

function +vi-git-set-message { +vi-git-st ; +vi-git-untracked ; }

# Set hooks.
zstyle ':vcs_info:git*+set-message:*' hooks git-set-message
zstyle ':vcs_info:svn*+set-message:*' hooks svn-untracked

# Print vcs_info on its own line.
function _nc:vcs_info {
    vcs_info
    if [[ -n "$vcs_info_msg_0_" ]]; then
        echo -e "\n$vcs_info_msg_0_"
    fi
}

# Print nix info on its own line.
function _nc:nix_info {
    if [[ -n "$IN_NIX_SHELL" ]]; then
        print -n "\n[%F{magenta}nix-shell%f][name=$name]"
    fi
}

# Add a string to PS1_PREFIX if it's not already present.
function nc:push_ps1_prefix {
  s="$1"
  if ! [[ "$PS1_PREFIX" =~ "$s" ]]; then
    if [[ -n "$PS1_PREFIX" ]]; then
      export PS1_PREFIX="$s $PS1_PREFIX"
    else
      export PS1_PREFIX="$s"
    fi
  fi
}

# Print a prompt prefix consisting of '(<py venv>)[$PS1_PREFIX]', with
# each part included iff corresponding env var is defined.
function _nc:ps1_prefix {
  # Special handling of Python virtual env prompt prefixes: because
  # PS1 is not inherited by new shells, we recover the venv prefix
  # from the inherited VIRTUAL_ENV var.
  if [[ -n "$VIRTUAL_ENV" ]]; then
    if ! [[ "$PS1" =~ "($(basename "$VIRTUAL_ENV"))" ]]; then
      print -n "(%F{yellow}$(basename "$VIRTUAL_ENV")%f)"
    fi
  fi
  if [[ -n "$PS1_PREFIX" ]]; then
    print -n "[%F{yellow}$PS1_PREFIX%f]"
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
PS1='$(_nc:ps1_prefix)[$rd%n$pl@$gr$(hostname)$pl]\
[$gr%~$pl]\
[%%$rd%j$pl]\
[%*]\
[%F{yellow}$nc_timer_dt_m_s_ms%f]\
$(_nc:vcs_info)\
$(_nc:nix_info)
%(?..[?$rd%?$pl])\
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

# Use millisecond-accurate float for seconds-since-ZSH-started
# variable.
typeset -F SECONDS

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
  local dt=$nc_timer_dt
  # Experiments indicate that value here is 0.001 to 0.002 larger than
  # value computed by 'time <cmd>', so limit to hundredths accuracy
  # where they should agree better.
  nc_timer_dt_m_s_ms=$(printf "%i:%05.2f" $(($dt / 60)) $(($dt % 60)))
  unset nc_timer
}
precmd_functions+=(nc:timer:precmd)

# http://stackoverflow.com/questions/2704635/is-there-a-way-to-find-the-running-time-of-the-last-executed-command-in-the-shel
#
# Show 'time' stats for last command if it consumed more than
# specified number of seconds of *CPU* time (Note: *not* wall time).
export REPORTTIME=60
# Show time for all commands in history; use 'history -D' to see the times.
#
# Docs say this is mutually exclusive with 'share_history', but I'm
# not sure why. In any case, with this off at least but
# 'share_history' on, running 'history -D' only shows run time for
# local commands run in current shell, not for commands merged into
# history from other shells, and not for commands that were in
# existing history when current shell started.
#
#setopt inc_append_history_time
