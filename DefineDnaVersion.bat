echo off
echo     ENTER DefineDnaVersion.bat

rem There is no 'NOBN' in the batch file's name because it will be called from also the 3D symbol library batch file, which accesses it without specific adm name.
rem Set LOG to no or yes to decide if logging to file is needed or not.
rem Set COPY3D to no or yes to decide if copying the whole 3D object library is needed or not.
rem Set TUTORIALS to no or yes to decide if copying the tutorials files is needed or not.
rem Set CLEAN to no or yes to decide if existing files in target folders shall be deleted.

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
echo     EXIT DefineDnaVersion.bat