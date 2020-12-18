set ADM=NO-BN
set RELEASE=2019.1
set LOG=0
cd _SRC
dir 
pause
cls
echo Trying:  "C:\Program Files\Autodesk\AutoCAD 2017\accoreconsole.exe" /b /s "%~dp0GenerateSymbolLibraryWorks.scr" /l en-US /isolate
pause
"C:\Program Files\Autodesk\AutoCAD 2017\accoreconsole.exe" /b /s "%~dp0GenerateSymbolLibraryWorks.scr" /l en-US /isolate
dir
pause
copy 2D.dwg ..\%ADM%-%RELEASE%-2D.dwg
pause