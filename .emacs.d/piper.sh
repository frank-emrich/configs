#!/bin/bash

#input_pid=$!


tmpfile=$(mktemp)

#exec 0> $tmpfile

cat - > $tmpfile <&0 &
cat_pid=$!




#This does give us the cat pid :(
#{ cat - > $tmpfile  ; cat_pid=$! ; echo $'\n*** Terminated ***' >> $tmpfile ; } <&0 &





{
    while kill -0 $cat_pid &>/dev/null; do
	sleep 0.25
    done
    echo $'\n*** Terminated ***' >>  $tmpfile
} &


#emacsclient -t -e "(tail-file \"test\" \"$tmpfile\")"

#emacsclient  -t --eval "(pager-read-pipe \"$tmpfile\")"

#emacsclient -e "(new-tail-file \"$tmpfile\")" -t $tmpfile

#emacsclient -t $tmpfile #-e '(auto-revert-tail-mode)

emacsclient -t -e "(open-color-file \"$tmpfile\" )"

#cat > $tmpfile &

#close pipe
#exec 0<&-

if ps -p $cat_pid &> /dev/null ; then
	kill $cat_pid

fi


rm $tmpfile

#echo $cat_pid
#echo "Got here"
#echo "cmd:" $cmd
