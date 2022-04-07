#!/bin/bash

echo "$1 | $2 |$3" >> ~/backwards.txt
#echo $2 >> ~/backwards.txt


function rewrite_path() {
  prefixes=('V:' 'C:/Users/Frank/paper-gits-tmp')
  REPLACE_WITH="/machines/frank-lpc/paper-gits"
  path="$1"
  for p in "${prefixes[@]}" ; do
    path="$(sed "s|$p|$REPLACE_WITH|" <<< "$path")"
  done
  result="$path"
}





rewrite_path "$2"
file="$result"

echo "$file" >> ~/backwards.txt

#echo $file

#echo $file >> ~/backwards.txt

~/.config/configs/scripts/synctex_emacsclient_tmux_session.sh "+$1" "$file"
