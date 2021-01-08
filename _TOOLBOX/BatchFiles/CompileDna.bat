echo off
echo     ENTER CompileDna.bat (with Adm=%ADM%, Release=%RELEASE%)
echo         Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

rem Assume that environent variables ADM, RELEASE and LOG are defined elsewhere. RELEASE may be undefined, ADM not.
rem This Batch file is supposed to be called from another batchfile which has set the environment variables 
rem ADM RELEASE and LOG. RELEASE may be empty, ADM may not.
rem
rem  Release name shall be same as extracted from the DNA Rootfile.xml IRI element.
rem Use other batch file to copy result as release-candidate to folders such as:
rem ...\Github\RailCOMPLETE-XX-YY\Solutions\RC.bundle\Adm\%ADM%\DNA.

if "%ADM%" equ "" (
	echo         *
	echo         ***********************************************************************************
	echo         No argument given - should be one of the echo administration abbrevs such as NO-BN.
	echo         ***********************************************************************************
	echo         *
	goto TheEnd
	
) else (
	echo         *
	echo         ***********************************************************************************
	echo         Generating DNA file %ADM%-%RELEASE%-DNA.xml
	echo         ***********************************************************************************
	echo         *

	REM *** Check that relaseName found in Windows environment variable matches IRI info in DNA RootFile.xml:
	REM In order to delay variable expansion inside for loops, we must enable delayed expansion.
	REM Use bangs (!) instead of percents (%) around variables to delay their assignment.
	SETLOCAL ENABLEDELAYEDEXPANSION
	rem Note: Use double %%a when in batch file. Use single percent sign %a when directly in command shell.
	for /f "tokens=3 delims=><  " %%a in ('type .\_SRC\%ADM%-RootFile.xml ^| FIND "<VersionNumber>"') do set versionNumber=%%a
	set fileNameForReleaseCandidate=%ADM%-!versionNumber!-DNA
	if "!fileNameForReleaseCandidate!" NEQ "%ADM%-%RELEASE%-DNA" (
		echo         *** WARNING: Caller's release name does not match DNA IRI VersionNumber: Caller:%RELEASE% / DNA:!versionNumber!
		echo         DNA file name will be: !fileNameForReleaseCandidate!.xml
		pause 
		exit
		) ELSE (
		rem Nothing...
	)

	cd _SRC
	rem dir
	rem pause
		rem A copy of xppq.exe, xppq.xcfg and xppq.xlcl can be found in the \Github\RailCOMPLETE\Customization\_TOOLBOX\XPPq folder.
		rem These three files must be present in the folder from which xppq is run:
		xppq.exe "%ADM%-RootFile.xml" > ..\%ADM%-%RELEASE%-DNA.xml
		echo f | xcopy /Y /I /E %ADM%-%RELEASE%-StyleDefinitions.xml ..\%ADM%-%RELEASE%-StyleDefinitions.xml
		cd ..
		dir %ADM%-%RELEASE%-*.xml
	pause
)

:TheEnd
echo     EXIT CompileDna.bat
