rem NO-BN_generateSymbolLibrary.bat
echo off
echo ***
echo Generate Symbol Library for RailCOMPLETE, with DWG 2013 objects.
echo .
echo Usage: ADMSPEC_generateSymbolLibrary
echo where ADMSPEC is the common abbreviation for the railway in question, e.g. NOBN or FRSR etc.
echo       ADMSPEC 'XXGL' means 'GenericLand Railways', a fictitious administration used in tutorials.
echo .
echo This Windows .BAT procedure uses the oldest of acad 2016 or 2017 to generate the library.
echo .
echo (c) Railcomplete AS, 2016-2019
echo .
echo ***
SET ACCOREEXEPATH="C:\Program Files\Autodesk\AutoCAD 2016\accoreconsole.exe"
IF NOT EXIST %ACCOREEXEPATH% SET ACCOREEXEPATH="C:\Program Files\Autodesk\AutoCAD 2017\accoreconsole.exe"

rem :XXGL 
rem echo Generating symbol library for administration'GenericLand' (XXGL)...
rem %ACCOREEXEPATH% /s "%~dp0XX-GL_generateSymbolLibrary" /l en-US /isolate >  XX-GL-2D_Logfile.txt
REM >  XX-GL-2D_Logfile.txt
rem goto SUCCESS

:NOBN 
echo Generating 2D symbol library for administration Bane NOR (NOBN)...
%ACCOREEXEPATH% /s "%~dp0NO-BN_generateSymbolLibrary" /l en-US /isolate 
REM >  NO-BN-2D_Logfile.txt
goto SUCCESS

rem :FRSR
rem echo Generating symbol library for administration SNCF Réseau (FRSR)...
rem pause
rem %ACCOREEXEPATH% /s "%~dp0FR-SR_generateSymbolLibrary" /l en-US /isolate > FR-SR-2D_Logfile.txt
REM > FR-SR-2D_Logfile.txt
rem goto SUCCESS

rem :FAILURE
rem ECHO ***
rem ECHO Unknown administration abbreviation '%1' - try one of XXGL, NOBN, FRSR etc.
rem echo ***
rem pause
rem exit

:SUCCESS
echo .
echo ***
echo Done generating RailCOMPLETE 2D symbol library.
echo .
echo Please verify that a new symbol library DWG file has been created. See also log file.
echo ***
pause
