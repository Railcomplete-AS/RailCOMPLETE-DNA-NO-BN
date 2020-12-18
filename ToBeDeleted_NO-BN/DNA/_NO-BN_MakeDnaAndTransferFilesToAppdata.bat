echo OFF
echo ENTER _NO-BN_MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call _NO-BN_DefineDnaVersion.bat

Rem Move to XX-XX\DNA folder:
cd ..\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\XX-XX folder:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToSolutionsRcBundle.bat

call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToAppdataRcBundle.bat
call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToSolutionsRcBundle.bat

:TheEnd
echo EXIT _NO-BN_MakeDnaAndTransferFilesToAppdata.bat
pause
