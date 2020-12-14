rem Info on troubles starting .scr file from accoreconsole:
rem https://www.theswamp.org/index.php?topic=49223.0
rem
echo OFF
echo ENTER _NO-BN_MakeSymbolLibrary.bat

set ADM=NO-BN
set RELEASE=2019.1o
set LOG=0
set CLEAN=0
rem Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not.
rem Set COPY3D to 0 (off) or 1 (on) to decide if copying the whole 3D object library is needed or not.
rem Set TUTORIALS to 0 (off) or 1 (on) to decide if copying the tutorials files is needed or not.
rem Set CLEAN to 0 (off) or 1 (on) to decide if existing files in target folders shall be deleted.

call ..\..\..\Customization\_TOOLBOX\BatchFiles\GenerateSymbolLibrary.bat

echo EXIT _NO-BN_MakeSymbolLibrary.bat
pause


