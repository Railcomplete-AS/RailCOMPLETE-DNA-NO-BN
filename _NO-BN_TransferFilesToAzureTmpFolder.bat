echo OFF
echo ENTER _NO-BN_TransferFilesToAzureTmpFolder.bat


set       ADM=NO-BN
set   RELEASE=2021.a
set       LOG=no
set    COPY3D=yes
set TUTORIALS=yes
set     CLEAN=no

echo %ADM%>__adm.txt
echo %RELEASE%>__release.txt
echo %LOG%>__log.txt
echo %COPY3D%>__copy3d.txt
echo %TUTORIALS%>__tutorials.txt
echo %CLEAN%>__clean.txt

echo         Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

Rem Move to ..\Customization\XX-XX\DNA folder:
cd .\%ADM%\DNA
call ..\..\_TOOLBOX\BatchFiles\CompileDna.bat

Rem Move to ..\Customization\XX-XX folder:
cd ..
call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAzureTmpFolder.bat


:TheEnd
echo EXIT _NO-BN_TransferFilesToAzureTmpFolder.bat
pause
