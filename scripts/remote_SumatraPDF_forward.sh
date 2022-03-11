#!/bin/bash

echo $* >> ~/remote_sumatra_args.txt


function rewrite_path() {
  result="$(sed 's|/machines/frank-lpc/paper-gits/||' <<< $1)"
}


rewrite_path "$PWD"
target_dir="$result"


orig_file="$2"

synctex_gz_file="${orig_file%.pdf}.synctex.gz"
synctex_file="${orig_file%.pdf}.synctex"


#echo "target_dir:" "$target_dir"

file='V:\\'"$target_dir"'\\'"$2"

#echo "file:" "$file" >> ~/start_sumatra_output.txt

args="/mnt/c/Program\ Files/SumatraPDF/SumatraPDF.exe $1 $file $3 $4 $5"

#echo "args:" "$args" >> ~/start_sumatra_output.txt




gunzip -f -k $synctex_gz_file
sed -i 's|'"$PWD"'/||' "$synctex_file"

echo 'V:\\'"$target_dir"  "$1" "$file" "$3" "$4" "$5"

# for openssh server running in WSL:
#ssh -4 frank-pc.ts.emrich.io "/mnt/c/Windows/system32/cmd.exe" '/mnt/c/Users/Frank/start_Sumatra.bat' 'V:\\'"$target_dir"  "$1" "$file" "$3" "$4" "$5"

# Connect via windows SSH server (on port 22) to WSL SSH server (on port 2222)
WINDOWS_IP=192.168.122.1
ssh -p 2222 -J ${WINDOWS_IP}:22 localhost  "/mnt/c/Windows/system32/cmd.exe" '/mnt/c/Users/Frank/configs/scripts/start_SumatraPDF_forward.bat' 'V:\\'"$target_dir"  "$1" "$file" "$3" "$4" "$5"
#ssh -4 frank-pc.ts.emrich.io 'C:\Windows\system32\cmd.exe' '/mnt/c/Users/Frank/start_Sumatra.bat' 'V:\\'"$target_dir"  "$1" "$file" "$3" "$4" "$5"
