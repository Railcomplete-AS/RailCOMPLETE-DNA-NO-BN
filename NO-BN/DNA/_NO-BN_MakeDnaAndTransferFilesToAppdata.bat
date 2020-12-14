echo OFF
echo ENTER _NO-BN_MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call _NO-BN_DefineDnaVersion.bat

Rem Move to ..\Customization\XX-XX\DNA folder:
cd ..\DNA
call ..\..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-XX folder:
cd ..
call ..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat
call ..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyDnaEtcToSolutionsRcBundle.bat

call ..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyCommonStuffToAppdataRcBundle.bat
call ..\..\..\..\GitHub\RailCOMPLETE\Customization\_TOOLBOX\BatchFiles\CopyCommonStuffToSolutionsRcBundle.bat

:TheEnd
echo EXIT _NO-BN_MakeDnaAndTransferFilesToAppdata.bat
pause
