#!/bin/bash

if [ -z "$TMUX" ] && [ -z "$TMUX_PANE" ]
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
  TERM=xterm-direct emacs --daemon=$cname
fi

# TODO: instead of xterm-direct, pass tmux-direct if in tmux pane and COLORTERM set to truecolor
TERM=xterm-direct exec emacsclient -s "$cname" -t $*
