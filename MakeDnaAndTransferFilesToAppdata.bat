echo OFF
rem (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
echo ENTER MakeDnaAndTransferFilesToAppdata.bat

Rem Set DNA version info in Windows environment variables:
call DefineDnaVersion.bat

Rem Move to ..\XX-YY\DNA 'home' folder, then compile DNA:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\XX-YY 'home' folder, then copy files to local %appdata% RC.bundle folder, for testing purposes on own PC:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat

:TheEnd
echo EXIT MakeDnaAndTransferFilesToAppdata.bat
pause
