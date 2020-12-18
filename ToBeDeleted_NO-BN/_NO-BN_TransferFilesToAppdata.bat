echo OFF
echo ENTER _NO-BN_TransferFilesToAppdataRcBundle.bat

set ADM=NO-BN
set RELEASE=2021.1
set LOG=0
set COPY3D=1
set TUTORIALS=1
set CLEAN=0
rem Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not.
rem Set COPY3D to 0 (off) or 1 (on) to decide if copying the whole 3D object library is needed or not.
rem Set TUTORIALS to 0 (off) or 1 (on) to decide if copying the tutorials files is needed or not.
rem Set CLEAN to 0 (off) or 1 (on) to decide if existing files in target folders shall be deleted.

call ..\_TOOLBOX\BatchFiles\CopyDnaEtcToAppdataRcBundle.bat
call ..\_TOOLBOX\BatchFiles\CopyCommonStuffToAppdataRcBundle.bat

:TheEnd
echo EXIT _NO-BN_TransferFilesToAppdataRcBundle.bat
pause
