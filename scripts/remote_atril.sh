#!/bin/bash

echo $* >> ~/remote_atril_output.txt

ssh majagua.ddns.net /home/frank/start_atril.sh "$PWD" $*
