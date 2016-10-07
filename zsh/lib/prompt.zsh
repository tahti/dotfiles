#Initialize themes system
autoload -U promptinit && promptinit
#prompt -l  - list themes
#prompt -p bart - preview theme

# This will set the default prompt to the walters theme (old one)
#prompt walters

# needed to get things like current git branch
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn # You can add hg too if needed: `git hg`
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' use-simple true
#zstyle ':vcs_info:git*' max-exports 2
# formats - A list of formats, used when actionformats is not used (which is
#   most of the time). Default: " (%s)-[%b]%u%c-"
zstyle ':vcs_info:*' formats ' %b' 'x%R'
# actionformats - A list of formats, used if there is a special action going on
#  in your current repository; like an interactive rebase or a merge conflict. 
# branchformat - Some backends replace %b in the formats and actionformats
# styles above, not only by a branch name but also by a revision number. This
# style lets you modify how that string should look. Default:"%b:%r" (for bzr, svn, svk and hg)  
zstyle ':vcs_info:*' actionformats ' %b|%a' 'x%R'
#zstyle ':vcs_info:svn*' branchformat '%b:%r'

autoload colors && colors

dirty() {
  if svn info >/dev/null 2>&1; then
    #svn_dirty
    local root=$(LANG=C svn info 2> /dev/null | sed -n 's/^Working Copy Root Path: //p')
    if svn status $root 2> /dev/null | command grep -Eq '^\s*[ACDIM!?L]'; then
      # Grep exits with 0 when "One or more lines were selected", return "dirty".
      #echo "red"
      echo "%F{red}✗%f"
    else
      # Otherwise, no lines were found, or an error occurred. Return clean.
      #echo "green"
      echo "%F{green}✔%f"
    fi
  else
    #echo "git"
    git_dirty
  fi
}


git_dirty() {
    # check if we're in a git repo
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    # check if it's dirty
    command git diff --quiet --ignore-submodules HEAD &>/dev/null;
    if [[ $? -eq 1 ]]; then
        echo "%F{red}✗%f"
    else
        echo "%F{green}✔%f"
    fi
}


arrows() {
  if svn info >/dev/null 2>&1; then
    echo ""
  else
    git_arrows
  fi
}


# get the status of the current branch and it's remote
# If there are changes upstream, display a ⇣
# If there are changes that have been committed but not yet pushed, display a ⇡
git_arrows() {
    # do nothing if there is no upstream configured
    command git rev-parse --abbrev-ref @'{u}' &>/dev/null || return

    local arrows=""
    local status
    arrow_status="$(command git rev-list --left-right --count HEAD...@'{u}' 2>/dev/null)"

    # do nothing if the command failed
    (( !$? )) || return

    # split on tabs
    arrow_status=(${(ps:\t:)arrow_status})
    local left=${arrow_status[1]} right=${arrow_status[2]}
# spectrum_ls to get all colors
    (( ${right:-0} > 0 )) && arrows+="%F{226}▼%f"
    (( ${left:-0} > 0 )) && arrows+="%F{196}▲%f"

    echo $arrows
}

# indicate a job (for example, vim) has been backgrounded
# If there is a job in the background, display a ✱
suspended_jobs() {
    local sj
    sj=$(jobs 2>/dev/null | tail -n 1)
    if [[ $sj == "" ]]; then
        echo ""
    else
        echo "%{$FG[208]%}✱%f"
    fi
}

precmd() {
    vcs_info
    print -P '\n%F{51}%F{green}%~'
}

export PROMPT='%(?..%? )%(?.%F{yellow}.%F{red})▶%f '
export RPROMPT='`dirty`%F{241}$vcs_info_msg_0_%f`arrows``suspended_jobs`'
#export RPROMPT='`dirty`%F{241}$vcs_info_msg_0_%f `suspended_jobs`'
