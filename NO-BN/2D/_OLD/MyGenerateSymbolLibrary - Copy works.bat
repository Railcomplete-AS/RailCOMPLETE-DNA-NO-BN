rem Uncomment the next three lines in order to test script directly without arguments:
rem set ADM=NO-BN
rem set RELEASENAME=2019.1
rem set LOG=0
rem *** Set LOG to 0 (off) or 1 (on) to decide if logging to file is needed or not ***

echo off
echo ENTER MyGenerateSymbolLibrary.bat (with Adm=%ADM%, Release=%RELEASE%, Log=%LOG%)
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
echo Dir *.*...
dir *.*
pause

rem Verify that ADM is one of the known ones:
if .%ADM%. == .XX-GL. goto MAKE-LIBRARY
if .%ADM%. == .NO-BN. goto MAKE-LIBRARY
if .%ADM%. == .FR-SR. goto MAKE-LIBRARY
goto FAILURE

:MAKE-LIBRARY
echo Generating symbol library for ADM=%ADM%, RELEASE=%RELEASE%, LOG=%LOG%...

set LIB=..
echo Running AutoCAD script from LIB=%LIB%
echo dir %LIB%
dir %LIB%
pause

if %LOG% equ 1 (
	echo Starting %ACCOREEXEPATH%
	%ACCOREEXEPATH% /s ..\GenerateSymbolLibrary.scr /l en-US /isolate > ..\%ADM%-%RELEASE%-2D_Logfile.txt
	goto SUCCESS
) else (
	echo Starting %ACCOREEXEPATH%
	%ACCOREEXEPATH% /s ..\GenerateSymbolLibrary.scr /l en-US /isolate
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
	copy 2D.dwg ..\%ADM%-2D.dwg
) ELSE (
	copy 2D.dwg ..\%ADM%-%RELEASE%-2D.dwg
)
echo Please verify that a new symbol library DWG file has been created. See also log file.
echo ***

echo EXIT MyGenerateSymbolLibrary.bat