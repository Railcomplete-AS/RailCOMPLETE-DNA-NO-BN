echo OFF
echo ENTER MakeDnaAndTransferFilesToProgramdata.bat

Rem Set DNA version info in Windows environment variables:
call DefineDnaVersion.bat

Rem Move to ..\Customization\XX-YY\DNA folder:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-YY folder:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToProgramdataRcBundle.bat
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToSolutionsRcBundle.bat

rem Deprecated?:
rem call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToProgramdataRcBundle.bat
rem call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToSolutionsRcBundle.bat

:TheEnd
echo EXIT MakeDnaAndTransferFilesToProgramdata.bat
pause
