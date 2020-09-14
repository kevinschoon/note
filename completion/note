function _note_complete {
  export COMP_CWORD
  COMP_WORDS[0]=note
  if type readarray > /dev/null
  then readarray -t COMPREPLY < <("${COMP_WORDS[@]}")
  else IFS="
" read -d "" -A COMPREPLY < <("${COMP_WORDS[@]}")
  echo $COMP_CWORD $COMP_WORDS
  fi
}

complete -F _note_complete note
