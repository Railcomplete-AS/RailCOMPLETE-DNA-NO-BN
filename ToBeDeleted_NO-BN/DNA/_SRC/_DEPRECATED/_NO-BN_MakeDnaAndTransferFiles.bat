echo OFF
echo ENTER _NO-BN_MakeDnaAndTransferFiles.bat

set ADM=NO-BN
set RELEASE=2019.1
set LOG=0
rem Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not.

call ..\..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CompileDna.bat
call ..\..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat
call ..\..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyDnaEtcToSolutionsRcBundle.bat

:TheEnd
echo EXIT _NO-BN_MakeDnaAndTransferFiles.bat
pause
