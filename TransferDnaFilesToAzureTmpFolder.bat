echo OFF
rem Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
echo ENTER TransferFilesToAzureTmpFolder.bat

Rem Set DNA version info in Windows environment variables
call DefineDnaVersion.bat

echo %ADM%>__adm.txt
echo %RELEASE%>__release.txt
echo %LOG%>__log.txt
echo %COPY3D%>__copy3d.txt
echo %TUTORIALS%>__tutorials.txt
echo %CLEAN%>__clean.txt

echo         Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

Rem Move to ..\Customization\XX-YY\DNA folder:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-YY folder:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAzureTmpFolder.bat

:TheEnd
echo EXIT TransferFilesToAzureTmpFolder.bat
pause
