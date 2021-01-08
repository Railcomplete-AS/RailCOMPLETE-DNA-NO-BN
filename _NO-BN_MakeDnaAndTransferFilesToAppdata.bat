echo OFF
echo ENTER _NO-BN_MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call _DefineDnaVersion.bat

Rem Move to ..\Customization\XX-XX\DNA folder:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-XX folder:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToSolutionsRcBundle.bat

rem Deprecated?:
rem call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToAppdataRcBundle.bat
rem call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToSolutionsRcBundle.bat

:TheEnd
echo EXIT _NO-BN_MakeDnaAndTransferFilesToAppdata.bat
pause
