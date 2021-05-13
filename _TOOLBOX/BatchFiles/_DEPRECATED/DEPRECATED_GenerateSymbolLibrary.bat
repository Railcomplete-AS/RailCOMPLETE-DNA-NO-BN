rem Uncomment the next lines in order to test script directly without arguments:
rem set ADM=NO-BN
rem set RELEASENAME=2019.1
rem set LOG=0
rem Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not.
rem Set COPY3D to 0 (off) or 1 (on) to decide if copying the whole 3D object library is needed or not.
rem Set CLEAN to 0 (off) or 1 (on) to decide if existing files in target folders shall be deleted.

rem Please read about batch file processing here: https://ss64.com/nt
echo off
echo ENTER GenerateSymbolLibrary.bat
echo Adm=%ADM% Release=%RELEASE% Log=%LOG% Clean=%CLEAN%

rem ***
rem Generate Symbol Library for RailCOMPLETE, with DWG 2013 format objects.
rem .
rem Usage: _GenerateSymbolLibrary ADMSPEC
rem where ADMSPEC is the common abbreviation for the railway in question, e.g. NO-BN or FR-SR etc.
rem       ADMSPEC set to XX-GL means 'GenericLand Railways', a fictitious administration used in tutorials.
rem .
rem This Windows .BAT procedure uses the oldest of acad 2016 or 2017 to generate the library.
rem This batch file is supposed to reside in folder C:\Users\clfey\Documents\GitHub\RailCOMPLETE\Customization\NO-BN\2D
rem (or a similar folder on the machine where the agent will build a new version of the 2D symbol library).
rem .
rem (c) Railcomplete AS, 2016-2019
rem .
rem ***

rem Search for a auitable AutoCAD 
SET ACCOREEXEPATH="C:\Program Files\Autodesk\AutoCAD 2016\accoreconsole.exe"
IF NOT EXIST %ACCOREEXEPATH% SET ACCOREEXEPATH="C:\Program Files\Autodesk\AutoCAD 2017\accoreconsole.exe"

rem We require that the LISP source code for this administration's 2D symbol library resides in the following folder:
cd _SRC

rem Verify that ADM is one of the known ones:
if .%ADM%. == .XX-GL. goto MAKE-LIBRARY
if .%ADM%. == .NO-BN. goto MAKE-LIBRARY
if .%ADM%. == .FR-SR. goto MAKE-LIBRARY
if .%ADM%. == .DE-DB. goto MAKE-LIBRARY
goto FAILURE

:MAKE-LIBRARY
echo Generating symbol library for ADM=%ADM%, RELEASE=%RELEASE%, LOG=%LOG%...
set BATCH_FOLDER=%~dp0
echo This batch file was called from drive and path BATCH_FOLDER=%BATCH_FOLDER%
set SCRIPT_FOLDER=%BATCH_FOLDER%..\AutoCadScripts
echo Running AutoCAD with script found in folder SCRIPT_FOLDER=%SCRIPT_FOLDER%
echo Starting %ACCOREEXEPATH% /s %SCRIPT_FOLDER%\GenerateSymbolLibrary.scr /l en-US /isolate
pause
if %LOG% equ 1 (
	%ACCOREEXEPATH% /s %SCRIPT_FOLDER%\GenerateSymbolLibrary.scr /l en-US /isolate > ..\%ADM%-%RELEASE%-2D_Logfile.txt
	goto SUCCESS
) else (
	%ACCOREEXEPATH% /s %SCRIPT_FOLDER%\GenerateSymbolLibrary.scr /l en-US /isolate
	goto SUCCESS
)

:FAILURE
ECHO ***
ECHO Unknown administration abbreviation '%ADM%' - try one of XX-GL, NO-BN, FR-SR etc.
echo ***
pause
exit

:SUCCESS
echo .
echo ***
echo Done generating RailCOMPLETE 2D symbol library.
echo .
if ".%RELEASE%." EQU ".." (
	echo f | xcopy /Y /I /E 2D.dwg ..\%ADM%-2D.dwg
) ELSE (
	echo f | xcopy /Y /I /E 2D.dwg ..\%ADM%-%RELEASE%-2D.dwg
)
echo Please verify that a new symbol library DWG file has been created. See also log file.
echo ***

echo EXIT GenerateSymbolLibrary.bat