__p_setprompt () {
  showLastRet=
  if [ $lastRet -ne 0 ]; then
    showLastRet="\[\e[31m\]($lastRet) \[\e[0m\]"
  fi

  showJobs=
  js=$(jobs | grep ^\\[ | sed -e 's/ \+/ /g' | cut -f 3 -d' ')
  if [ -n "$js" ]; then
    showJobs=" \[\e[0;38;5;60m\][$(echo $js | sed -e 's/ /, /g')]\[\e[0m\]"
  fi

  systemColor="1;38;5;34m"
  if [ -n "$SSH_TTY" ]; then
    # Different color if we're logged in remotely
    systemColor="32m"
  fi

  sandbox=$(__p_wutSandbox)

  delim="\[\e[90;45m\] \[\e[0m\]"
  PS1="${delim} \[\e]0;\w\a\]${showLastRet}\[\e[${systemColor}\]\u@\h \[\e[0;33m\]\w\[\e[0m\]${showJobs}\$(__git_ps1) ${sandbox}${delim} "
}

__p_wutSandbox () {
    sandbox=
    # Symlinks can be weird so let's just bail
    if git root >/dev/null 2>&1 && echo $PWD | grep -q $(git root) >/dev/null 2>&1
    then
        ldir=$PWD
        git_root=$(git root)
        while [ $ldir != $git_root ]
        do
            if ls $ldir/*.cabal >/dev/null 2>&1
            then
                if [ ! -e "$ldir/cabal.sandbox.config" ]
                then
                    sandbox="\[\e[31m(no sandbox)\[\e[0m "
                fi
                break
            fi
            ldir=$(dirname $ldir)
        done
    fi
    echo "$sandbox"
}

export PROMPT_COMMAND='lastRet=$?; __p_setprompt; unset lastRet'
