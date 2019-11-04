#!/bin/bash


# Lookup the last emacs server that was used for compiling a PDF
# and forward synctex request there

echo "synctex_args: " $* >> ~/syntex_args.txt


SERVER_NAME_FILE="/tmp/emacs-latex-server"


if [ -f $SERVER_NAME_FILE ] ; then
  server=$(cat $SERVER_NAME_FILE)
else
  echo "$SERVER_NAME_FILE" "doesn't exist, aborting"
  exit 1
fi

echo "sever is " $server



emacsclient -s "$server" --no-wait $*
