#!/bin/bash

echo $* >> ~/remote_okular_output.txt

# time rsync -v -e "ssh -v" *.pdf *.synctex.gz frank@majagua-via-pi-uoe-with-cm:/home/frank/test
# time ssh frank@majagua-via-pi-uoe-with-cm /home/frank/start_okular.sh "$PWD" $*

time rsync -v -e "ssh -v" *.pdf *.synctex.gz frank@majagua-via-ts-with-cm:/home/frank/test
time ssh frank@majagua-via-ts-with-cm /home/frank/start_okular.sh "$PWD" $*
