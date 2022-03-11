#!/bin/bash

#echo $1 >> ~/backwards.txt
#echo $2 >> ~/backwards.txt


function rewrite_path() {
  result="$(sed 's|V:|/machines/frank-lpc/paper-gits|' <<< $1)"
}




rewrite_path "$2"
file="$result"

#echo $file

#echo $file >> ~/backwards.txt

~/.config/configs/scripts/synctex_emacsclient_tmux_session.sh +$1 $file
