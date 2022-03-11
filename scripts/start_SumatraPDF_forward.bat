echo %1
cd /d %1

REM This needs DDE Command
REM Sumatra reverse config as follows: wscript "C:\Users\Frank\runHidden.vbs" "C:\Users\Frank\start_backwards_SumatraPDF.bat" %l %f
REM must set oplocks = False in smbd.conf as well



tasklist /FI "IMAGENAME eq SumatraPDF.exe" 2>NUL | find /I /N "SumatraPDF.exe">NUL

IF "%ERRORLEVEL%"=="0" (
REM powershell.exe start-sleep -m 1000
"C:\Program Files (x86)\DDE Command\DDECmd.exe" execute --server SUMATRA -t control --command "[Open(\"%3\", 0, 0, 1)]"
REM powershell.exe start-sleep -m 200
"C:\Program Files (x86)\DDE Command\DDECmd.exe" execute --server SUMATRA -t control --command "[ForwardSearch(\"%3\",\"%5\",%6,0,0,0)]"
) ELSE (
"C:\Program Files\SumatraPDF\SumatraPDF.exe" %2 %3 %4 %5 %6
)
