@echo off
@echo ****************************************
@echo Batch file to generate DNA.
@echo Move previous files to folder OLD,
@echo then generate new DNA using XPPq,
@echo at last copy result to release-candidate
@echo in Solutions\RC.bundle\Adm\'XX-XX'\DNA.
@echo NB! Keeping the RC.bundle DNA unchanged
@echo allows Github to track changes to the
@echo XPPq'ed DNA file.
@echo ****************************************

if "%ADM%" equ "" (
	@echo *
	@echo ****************************************
	@echo No argument given - should be one of the
	@echo administration abbrevs such as NO-BN.
	@echo ****************************************
	@echo *
	pause
	goto TheEnd
) else (
	@echo *
	@echo ****************************************
	@echo ***              %ADM%               ***
	@echo ****************************************
	@echo *

REM In order to delay variable expansion inside for loops, we must enable delayed expansion.
REM Use bangs (!) instead of percents (%) around variables to delay their assignment.
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

	rem Either...
	rem move *-%ADM%-DNA-*.xml OLD
	rem xppq "%ADM%-RootFile.xml" > !fileNameWithDate!.xml
	rem copy !fileNameWithDate!.xml ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\!fileNameForReleaseCandidate!.xml
	rem ...or...

	xppq "%ADM%-RootFile.xml" > !fileNameWithDate!.xml
	dir !fileNameWithDate!.xml
	pause
	move !fileNameWithDate!.xml ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\!fileNameForReleaseCandidate!.xml
)

:TheEnd
