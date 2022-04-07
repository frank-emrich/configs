#!/bin/bash

#echo "$*" >> ~/remote_sumatra_args.txt


function rewrite_path() {
  result="$(sed 's|/machines/frank-lpc/paper-gits/||' <<< $1)"
}

function log() {
  echo "$*" | tee ~/remote_SumatraPDF_forward.log
}

log "pwd is" "$PWD"
log "3 is" "$3"
log "4 is" "$4"
log "5 is" "$5"
log "6 is" "$6"
log "7 is" "$7"

method="$1"
windows_host="$2"

pdf_filename="$4"
tex_filename="$6"

WINDOWS_RSYNC_TARGET_DIR='C:\\Users\\Frank\\paper-gits-tmp'
WSL_RSYNC_TARGET_DIR="/mnt/c/Users/Frank/paper-gits-tmp"

#if [[ $method = ]]


rewrite_path "$PWD"

# this may be more than a single folder name, but the relative path from paper-gits onward, like "fml/fml-in-context"
project_folder="$result"
project_folder_backslash="$(echo "$project_folder" | sed 's|/|\\\\|')"
echo "project_folder_backslash is $project_folder_backslash"

orig_file="$4"

synctex_gz_file="${orig_file%.pdf}.synctex.gz"
synctex_file="${orig_file%.pdf}.synctex"


#echo "project_folder:" "$project_folder"



#echo "file:" "$file" >> ~/start_sumatra_output.txt

#args="/mnt/c/Program\ Files/SumatraPDF/SumatraPDF.exe $3 $file $5 $6 $7"

#echo "args:" "$args" >> ~/start_sumatra_output.txt


SSH_CONTROL_MASTER_ARGS=(
 '-oControlPath=~/.ssh/controlmasters/latex-%h-%p-%r'
 '-oControlMaster=auto'
 '-oControlPersist=10m'
)
ALL_SSH_CONTROL_MASTER_ARGS="$SSH_CONTROL_MASTER_ARGS[0] $SSH_CONTROL_MASTER_ARGS[1] $SSH_CONTROL_MASTER_ARGS[2]"

gunzip -f -k $synctex_gz_file
sed -i 's|'"$PWD"'/||' "$synctex_file"

if [[ "$method" = "direct" ]] ; then
  windows_target_dir='V:\\'"$project_folder_backslash"
elif [[ "$method" = "rsync" ]] ; then
  # TODO this breaks when parent directories are missing!
  rsync --rsh "ssh $ALL_SSH_CONTROL_MASTER_ARGS" --rsync-path="wsl rsync" "$pdf_filename" "$synctex_file" "${windows_host}.ts.emrich.io:${WSL_RSYNC_TARGET_DIR}/$project_folder"
  windows_target_dir="$WINDOWS_RSYNC_TARGET_DIR"'\\'"$project_folder_backslash"
else
  echo "unknown method"
  exit 1
fi

file="$windows_target_dir"'\\'"$pdf_filename"

echo "file is $file"
echo "windows_target_dir is $windows_target_dir"

#echo 'V:\\'"$project_folder"  "$3" "$file" "$5" "$6" "$7"

# for openssh server running in WSL:
#ssh -4 frank-pc.ts.emrich.io "/mnt/c/Windows/system32/cmd.exe" '/mnt/c/Users/Frank/start_Sumatra.bat' 'V:\\'"$project_folder"  "$1" "$file" "$3" "$4" "$5"

# Connect via windows SSH server (on port 22) to WSL SSH server (on port 2222)

ssh ${SSH_CONTROL_MASTER_ARGS[@]} -p 2222 -J ${windows_host}.ts.emrich.io:22 ${windows_host}.localhost.emrich.io  "/mnt/c/Windows/system32/cmd.exe" '/mnt/c/Users/Frank/configs/scripts/start_SumatraPDF_forward.bat' "$windows_target_dir"  "$3" "$file" "$5" "$6" "$7"
#ssh -4 frank-pc.ts.emrich.io 'C:\Windows\system32\cmd.exe' '/mnt/c/Users/Frank/start_Sumatra.bat' 'V:\\'"$project_folder"  "$1" "$file" "$3" "$4" "$5"
