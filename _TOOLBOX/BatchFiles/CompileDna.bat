echo off
rem Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
echo	ENTER CompileDna.bat (with Adm=%ADM%, Release=%RELEASE%)
echo		Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%
rem			.
rem			*********************************************************************************
rem			Batch file to expand XPPq macros and assemble partial DNA XML files into one DNA
rem			XML file. The source files and the XPPq.exe application and config files must 
rem			reside in this folder:
rem			...\Github\RailCOMPLETE-XX-YY\DNA\_SRC
rem			where 'RailCOMPLETE-XX-YY' is the name of the Github clone for this adm's DNA stuff.
rem			.
rem			Assume that environent variables ADM, RELEASE and LOG are defined elsewhere. 
rem			This Batch file is supposed to be called from another batchfile which has set the 
rem			environment variables ADM RELEASE and LOG. RELEASE may be empty, ADM may not.
rem			.
rem			Release name shall be same as extracted from the DNA Rootfile.xml IRI element.
rem			Use other batch file to copy result as release-candidate to folders such as:
rem			...\Github\RailCOMPLETE-XX-YY\Solutions\RC.bundle\Adm\%ADM%\DNA.
rem			*********************************************************************************
rem			.
rem			This batch file must be called from this folder:
rem			...\Github\RailCOMPLETE-XX-YY
rem			.
rem			Please read about batch file processing here: https://ss64.com/nt
rem			.
rem			Please note that general folders and paths, which RC must know before a RC-START'ed 
rem			document has been opened, must be specified in the RC.bundle\startup.xml file.
rem			The administration-specific folders and paths, which RC must know when using RC-START'ed 
rem			document, must be specified in the RC.bundle\Adm\%ADM%\DNA folder's DNA file.
rem			.
rem			*********************************************************************************

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

	REM Use bangs (!) instead of percent (%) around variables to delay their assignment.
	REM Read about "Delayed Expansion" -  this is crucial to how this batch file works.
	REM In order to delay variable expansion inside 'for' loops, we must enable delayed expansion.
	SETLOCAL ENABLEDELAYEDEXPANSION

	REM The FIND batch command is a basic MS-DOS command, so the PATH environment var must contain "C:\Windows\System32;".
	REM Deprecated code - can't use FINDSTR because it also need C:\Windows\System32;...
	REM Usage:     FINDSTR /I pattern filespec      where /I means "ignore uppercase/lowercase". Returns the full source or nothing.
	REM findstr /I "C:\Windows\System32" __tmp.txt 
	REM if NOT ERRORLEVEL 0 set PATH=C:\Windows\System32;!PATH!
	REM Brute force approach: Just add to path:
	set path=C:\Windows\System32;!path!

	rem Note: Use double %%a when in batch file. Use single percent sign %a when directly in command shell.
	for /f "tokens=3 delims=><  " %%a in ('type .\_SRC\%ADM%-RootFile.xml ^| FIND "<VersionNumber>"') do set versionNumber=%%a
	set fileNameForReleaseCandidate=%ADM%-!versionNumber!-DNA
	if "!fileNameForReleaseCandidate!" NEQ "%ADM%-%RELEASE%-DNA" (
		echo         *** WARNING: Caller's release name '%RELEASE%' from DefineDnaVersion.bat does not match DNA source rootfile's DNA IRI VersionNumber '!versionNumber!'
		echo         DNA file name will be: !fileNameForReleaseCandidate!.xml
		pause 
		exit
		) ELSE (
		rem Nothing...
	)

	cd _SRC
	rem dir
	rem pause
		rem A copy of xppq.exe, xppq.xcfg and xppq.xlcl can be found in the \Github\RailCOMPLETE\Customization\_TOOLBOX\XPPq folder as well as documentation and source code.
		rem These three files must be present in the folder from which xppq is run:
		xppq.exe "%ADM%-RootFile.xml" > ..\%ADM%-%RELEASE%-DNA.xml
		cd ..
		dir %ADM%-%RELEASE%-*.xml
	pause
)

:TheEnd
echo	EXIT CompileDna.bat
