function _jsautocom_15110 {
  export COMP_CWORD
  COMP_WORDS[0]=./cal.exe
  if type readarray > /dev/null
  then readarray -t COMPREPLY < <("${COMP_WORDS[@]}")
  else IFS="
" read -d "" -A COMPREPLY < <("${COMP_WORDS[@]}")
  fi
}
complete -F _jsautocom_15110 ./cal.exe
