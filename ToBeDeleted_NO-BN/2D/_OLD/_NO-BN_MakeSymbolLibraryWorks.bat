echo OFF
echo ENTER _NO-BN_MakeSymbolLibraryWorks.bat

set ADM=NO-BN
set RELEASE=2019.1
set LOG=0
rem Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not.
echo ...setting Adm=%ADM%, Release=%RELEASE%, Log=%LOG%

rem call ..\..\..\Customization\_TOOLBOX\BatchFiles\GenerateSymbolLibrary.bat
call GenerateSymbolLibraryWorks.bat

echo EXIT _NO-BN_MakeSymbolLibraryWorks.bat
pause


