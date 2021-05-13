echo OFF
echo ENTER TransferFilesToAzureTmpFolder.bat


Rem Set DNA version info in Windows environment variables DOES NOT WORK ON AZURE CLOUD BUILD SERVICE - SET EXPLICITLY HERE ????:
call DefineDnaVersion.bat

rem 2021-05-13 CLFEY Removed explicit SET statements, replaced with the DefineDnaVersion btch file to keep a single source.
rem set       ADM=NO-BN
rem set   RELEASE=2021.a
rem set       LOG=no
rem set    COPY3D=yes
rem set TUTORIALS=yes
rem set     CLEAN=no

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
