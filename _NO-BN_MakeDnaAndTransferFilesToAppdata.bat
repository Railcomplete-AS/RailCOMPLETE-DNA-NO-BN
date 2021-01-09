echo OFF
echo ENTER _NO-BN_MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call _DefineDnaVersion.bat

Rem Move to ..\Customization\XX-XX\DNA 'home' folder, then compile DNA:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-XX 'home' folder, then copy files to local %appdata% RC.bundle folder, for testing purposes on own PC:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat

:TheEnd
echo EXIT _NO-BN_MakeDnaAndTransferFilesToAppdata.bat
pause
