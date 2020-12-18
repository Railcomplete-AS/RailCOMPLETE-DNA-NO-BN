rem Info on troubles starting .scr file from accoreconsole:
rem https://www.theswamp.org/index.php?topic=49223.0
rem
echo OFF
echo ENTER _MakeSymbolLibrary.bat

Rem Set DNA version using files with names '__xxxxx.txt':
call ..\DNA\_DefineDnaVersion.bat

Rem Get DNA version info from '__xxxxx.txt' files (since we cannot pass Windows environment variables from child process to parent process):
Rem The /p option instructs the SET command to stop and prompt for input, which we can use to redirect text from a file.
set /p ADM=<..\DNA\__adm.txt
set /p RELEASE=<..\DNA\__release.txt
set /p LOG=<..\DNA\__log.txt
set /p COPY3D=<..\DNA\__copy3d.txt
set /p TUTORIALS=<..\DNA\__tutorials.txt
set /p CLEAN=<..\DNA\__clean.txt

echo     Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

Rem Remove VERY old .BAK file if any:
;del %ADM%-%RELEASE%.bak

Rem Make .BAK from previous .DWG file:
;rename %ADM%-%RELEASE%.dwg %ADM%-%RELEASE%.bak

if exist %ADM%-%RELEASE%.dwg rename %ADM%-%RELEASE%.dwg %ADM%-%RELEASE%.bak
call ..\..\..\Customization\_TOOLBOX\BatchFiles\GenerateSymbolLibrary.bat

echo EXIT _MakeSymbolLibrary.bat
pause


