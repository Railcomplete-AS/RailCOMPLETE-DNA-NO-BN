echo OFF
echo ENTER MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call DefineDnaVersion.bat

Rem Move to ..\Customization\XX-YY\DNA 'home' folder, then compile DNA:
cd .\%ADM%\DNA

echo 
pause
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-YY 'home' folder, then copy files to local %appdata% RC.bundle folder, for testing purposes on own PC:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat

:TheEnd
echo EXIT MakeDnaAndTransferFilesToAppdata.bat
pause
