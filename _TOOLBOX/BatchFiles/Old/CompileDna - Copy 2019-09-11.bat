rem Assume that environent variables ADM, RELEASENAME and LOG are defined elsewhere. RELEASENAME may be undefined, ADM not.

@echo off
@echo ****************************************
@echo Batch file to generate DNA for %ADM%
@echo ****************************************
@echo Release name is as extracted from the DNA Rootfile.xml IRI element.
@echo Move previous files to subfolder _DEPRECATED, then generate new DNA using XPPq.
@echo .
@echo Use other batch file to copy result as release-candidate to 
@echo ...\Github\RailCOMPLETE\Solutions\RC.bundle\Adm\%ADM%\DNA.
@echo ****************************************

if "%ADM%" equ "" (
	@echo *
	@echo ****************************************
	@echo No argument given - should be one of the
	@echo administration abbrevs such as NO-BN.
	@echo ****************************************
	@echo *
	goto TheEnd
) else (
	@echo *
	@echo Generating DNA file for Adm=%ADM%, ReleaseName=%RELEASENAME%...
	@echo *

REM In order to delay variable expansion inside FOR loops, we must enable delayed expansion.
REM Then, use bangs (!) instead of percents (%) around variables to delay their assignment.
SETLOCAL ENABLEDELAYEDEXPANSION
rem Note: Use double %%a when in batch file. Use single percent sign %a when directly in command shell.

for /f "tokens=3 delims=><  " %%a in ('type %ADM%-RootFile.xml ^| FIND "<VersionNumber>"') do set versionNumber=%%a

rem Expect the wmic os get LocalDateTime call to return a string such as:  "LocalDateTime=20190814132807.507000+060"
rem ldt == YYYY (skip 0, use 4)   MM (skip 4, use 2)   DD (skip 6, use 2)
	set ldt=`wmic os get LocalDateTime`
	for /F "usebackq tokens=1,2 delims=." %%i in (`wmic os get LocalDateTime ^| FIND "."`) do SET ldt=%%i
	set isodate=!ldt:~0,4!-!ldt:~4,2!-!ldt:~6,2!
	set fileNameWithDate=!isodate!-%ADM%-DNA-!versionNumber!
	set fileNameForReleaseCandidate=%ADM%-DNA-!versionNumber!
	echo !fileNameWithDate!
	echo !fileNameForReleaseCandidate!
pause
	cd _SRC
dir
pause
	..\..\Customization\_TOOLBOX\Batchfiles\xppq "%ADM%-RootFile.xml" > ..\!fileNameWithDate!.xml
	cd ..
	dir !fileNameWithDate!.xml
pause
	rem Copy to Solutions\RC.bundle:
	rem copy !fileNameWithDate!.xml ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\!fileNameForReleaseCandidate!.xml
rem pause
)

:TheEnd
pause