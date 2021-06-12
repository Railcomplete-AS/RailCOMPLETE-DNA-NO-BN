echo off
echo     ENTER CopyDnaEtcToAzureTmpFolder.bat
echo        Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%
rem			*********************************************************************************
rem			.
rem			In the following, 'XX-YY' denotes any administration. This batch file is identical for all.
rem
rem			Batch file to transfer DNA and related files from folder/subfolders:
rem
rem			...\Github\RailCOMPLETE-XX-YY
rem
rem			to folder/subfolders used by the Azure cloud build service used to create ADM install file:
rem
rem			..\TMP
rem
rem			Read more about batch file processing here: https://ss64.com/nt.
rem			.
rem			Note that general folders and paths, which RC must know before a RC-START'ed 
rem			document has been opened, must be specified in the RC.bundle\startup.xml file.
rem			The administration-specific folders and paths, which RC must know when using RC-START'ed 
rem			document, must be specified in the RC.bundle\Adm\%ADM%\DNA folder's DNA file.
rem			.
rem			*********************************************************************************
rem
rem Note on ECHO syntax:
rem
rem Use caret '^' as escape character for pipe ^|, caret ^^, angles ^> and ^< etc. An '@'
rem in front of a command name suppresses echoing for that command only: @MyCommand
rem 'echo.' (a period right after the command name) prints an empty line.
rem 'echo d | MyCommand' sets up a file system pipe, the output of 'echo d' becomes the input of 'MyCommand'. The 'd' means ???????????
rem
rem Note on XCOPY syntax:
rem
rem Usage:     echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt
rem /exclude:..\xcopyignore.txt : The 'ignore' file contains snippets of folder names or file names that, if encountered, shall be ignored by XCOPY.
rem /Y = Suppress prompt to confirm overwriting a file. Can be preset in the echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txtCMD env
rem /E = Create folder if non-existent
rem /I = If in doubt always assume the destination is a folder e.g. when the destination does not exist.

if "%ADM%" neq "" goto Continue1
	echo          *
	echo          *********************************************************************************
	echo          The ADM argument cannot be undefined - should be one of the
	echo          administration abbrevs such as NO-BN.
	echo          Set ADM environment variable in calling Azure YAML script.
	echo          *********************************************************************************
	echo          *
goto TheEnd
 
:Continue1
	echo          *********************************************************************************
	echo          Transfering customization files for %ADM% to a new folder called TMP used by installer script...
	echo          *********************************************************************************
	echo          ON
	
	echo - 2D symbols and thumbnails for RC-CreateXxxx functions:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\2D    "..\TMP\%ADM%\2D"

if "%COPY3D%" neq "yes" goto Continue2
	echo - 3D object models:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\3D    "..\TMP\%ADM%\3D"

:Continue2
	echo - AutoCAD stuff (color table, fonts etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\AutoCAD    "..\TMP\%ADM%\AutoCAD"

	echo - DNA, style definitions and switches:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA    "..\TMP\%ADM%\DNA"

	echo - FAQ:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\FAQ    "..\TMP\%ADM%\FAQ"

	echo - Lua snippets and Tooltips (Lua and XAML code):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua    "..\TMP\%ADM%\Lua"

	@echo.
	@echo - Release notes:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\ReleaseNotes    "..\TMP\%ADM%\ReleaseNotes"

if "%TUTORIALS%" neq "yes" goto Continue3
 	echo - Administration-specific tutorials:
	set SOURCEFOLDER=..\%ADM%\Tutorials
	set TARGETFOLDER=%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Tutorials
	echo Source=%SOURCEFOLDER%
	echo Target=%TARGETFOLDER%
	echo .
	rem echo d | xcopy /Y /E /I: /Y Suppress prompt to confirm overwriting a file. Can be preset in the echo d | xcopy /Y /E /ICMD env
	rem echo d | xcopy /Y /E /I: /E echo d | xcopy /Y /E /I folders and subfolders, including Empty folders. Can be used to modify /T.
	rem echo d | xcopy /Y /E /I: /I If in doubt always assume the destination is a folder e.g. when the destination does not exist.
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt "%SOURCEFOLDER%" "%TARGETFOLDER%"
	echo .
	rem Remove excess target folders (folder name starts with an underscore) - and its files, including subfolders:
	echo Run: for /f "tokens=*" %G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%G"
	for /f "tokens=*" %%G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%%G"
	echo .
:Continue3

	echo - Vector images (administration's logo etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\VectorImages    "..\TMP\%ADM%\VectorImages"

	echo - WebLinks:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\WebLinks    "..\TMP\%ADM%\WebLinks"

	echo OFF
	echo          *********************************************************************************
	echo          ...Transfer completed.
	echo          *********************************************************************************

:TheEnd
echo     EXIT CopyDnaEtcToAppdataBundle.bat
