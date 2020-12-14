echo OFF
echo ENTER _NO-BN_MakeDna.bat

Rem Set DNA version using files with names '__xxxxx.txt':
call _NO-BN_DefineDnaVersion.bat

Rem Get DNA version info from '__xxxxx.txt' files (since we cannot pass Windows environment variables from child process to parent process):
Rem The /p option instructs the SET command to stop and prompt for input, which we can use to redirect text from a file.
set /p ADM=<__adm.txt
set /p RELEASE=<__release.txt
set /p LOG=<__log.txt
set /p COPY3D=<__copy3d.txt
set /p TUTORIALS=<__tutorials.txt
set /p CLEAN=<__clean.txt

echo     Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

call ..\..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CompileDna.bat

echo EXIT _NO-BN_MakeDna.bat
pause
