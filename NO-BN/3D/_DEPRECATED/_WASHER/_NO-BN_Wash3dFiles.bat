echo OFF
echo ENTER _NO-BN_Wash3dFiles.bat

Rem Cleanup first:
rem xcopy ..\3D\STD\*.dwg ..\3D\STD-BACKUP\*.dwg
rem del ..\3D\STD\.bak
rem del ..\3D\STD\.dwh

call ..\..\..\Customization\_TOOLBOX\BatchFiles\WashAllFilesIn3dLibrary.bat

echo EXIT _NO-BN_Wash3dFiles.bat
pause

