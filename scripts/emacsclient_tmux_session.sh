#!/bin/bash

if [ -z "$TMUX" ]
then
      cname="notmux"
else
      cname="tmux-$(tmux display-message -p '#S')"
fi

echo session is "$cname"


#This will fail if the server is down and return immedediately if it is
emacsclient -s "$cname" --eval '"Hello"' &> /dev/null


# Start emacs server if there is none for this session
if [ $? -ne 0 ]; then
  emacs --daemon=$cname
fi

exec emacsclient -s "$cname" -t $*
