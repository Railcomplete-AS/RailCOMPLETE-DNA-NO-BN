echo OFF
echo ENTER MakeDna.bat

Rem Set DNA version and other parameters, needed by the batch files, using files with names '__xxxxx.txt':
call DefineDnaVersion.bat

Rem Get DNA version info from '__xxxxx.txt' files (since we cannot pass Windows environment variables from child process to parent process):
Rem The /p option instructs the SET command to stop and prompt for input, which we can use to redirect text from a file.
set /p ADM=<__adm.txt
set /p RELEASE=<__release.txt
set /p LOG=<__log.txt
set /p COPY3D=<__copy3d.txt
set /p TUTORIALS=<__tutorials.txt
set /p CLEAN=<__clean.txt

echo     Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

echo EXIT MakeDna.bat
pause
