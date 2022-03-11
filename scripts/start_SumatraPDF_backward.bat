REM echo %1 >> C:\Users\Frank\test.txt
REM echo %2 >> C:\Users\Frank\test.txt


REM In Sumatra, set inverse search command to the following:
REM wscript "C:\Users\Frank\runHidden.vbs" "C:\Users\Frank\configs\scripts\start_Sumatra_backwards.bat" %l %f

set "variable=%2"
set "variable=%variable:\=/%"
echo "%variable%"


REM echo %variable% >> C:\Users\Frank\test.txt
wsl ssh -4 frank-lpc.ts.emrich.io /home/frank/.config/configs/scripts/remote_SumatraPDF_backwards.sh "%1" "%variable%"
