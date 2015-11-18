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

  delim="\[\e[90;45m\] \[\e[0m\]"
  PS1="${delim} \[\e]0;\w\a\]${showLastRet}\[\e[${systemColor}\]\u@\h \[\e[0;33m\]\w\[\e[0m\]${showJobs}\$(__git_ps1) ${delim} "
}

export PROMPT_COMMAND='lastRet=$?; __p_setprompt; unset lastRet'
