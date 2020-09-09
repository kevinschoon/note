# TODO: Core.Command completion is terrible 
function _jsautocom_742571 {
  export COMP_CWORD
  COMP_WORDS[0]=note
  if type readarray > /dev/null
  then readarray -t COMPREPLY < <("${COMP_WORDS[@]}")
  else IFS="
" read -d "" -A COMPREPLY < <("${COMP_WORDS[@]}")
  fi
}
complete -F _jsautocom_742571 note
