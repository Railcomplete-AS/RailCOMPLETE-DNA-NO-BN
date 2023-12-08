echo off
rem (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
echo	ENTER CopyDnaEtcToAppdataRcBundle.bat
echo        Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%
rem			*********************************************************************************
rem			.
rem			Batch file to transfer DNA and related files from these folders with subfolders:
rem			...\Github\RailCOMPLETE-XX-YY
rem			where 'RailCOMPLETE-XX-YY' is the name of the Github clone for this adm's DNA stuff.
rem			The embedded DNA for XX-GL resides in ...github\RailCOMPLETE\Customization.
rem			
rem			to the folder/subfolders used by your local AutoCAD installation to run tests:
rem			%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\XX-YY
rem			.
rem			This batch file must be called from this folder ('XX-YY' is NO-BN etc given by ADM):
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
rem ECHO syntax:
rem Use caret '^' as escape character for pipe ^|, caret ^^, angles ^> and ^< etc. An '@'
rem in front of a command name suppresses echoing for that command only: @MyCommand
rem 'echo.' (a period right after the command name) prints an empty line.
rem 'echo d | MyCommand' sets up a file system pipe, the output of 'echo d' becomes the input of 'MyCommand'. The 'd' means ???????????
rem
rem XCOPY syntax:
rem Usage:     echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt
rem /exclude:..\xcopyignore.txt : The 'ignore' file contains snippets of folder names or file names that, if encountered, shall be ignored by XCOPY.
rem /Y = Suppress prompt to confirm overwriting a file. Can be preset in the echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txtCMD env
rem /E = Create folder if non-existent
rem /I = If in doubt always assume the destination is a folder e.g. when the destination does not exist.

if "%ADM%" neq "" goto DeletePrevious
	echo          *
	echo          *********************************************************************************
	echo          The ADM argument cannot be undefined - should be one of the
	echo          administration abbrevs such as NO-BN.
	echo          Set ADM environment variable in calling batch file.
	echo          *********************************************************************************
	echo          *
goto TheEnd
 
:DeletePrevious
if "%CLEAN%" neq "yes" goto Continue
	rem 
	rem RMDIR [/S] [/Q] [drive:]path
	rem RD [/S] [/Q] [drive:]path
	rem 
    rem /S      Removes all directories and files in the specified directory
    rem         in addition to the directory itself.  Used to remove a directory
    rem         tree.
	rem 
    rem /Q      Quiet mode, do not ask if ok to remove a directory tree with /S
	rem
	echo          *
	echo          *********************************************************************************
	echo          *  WARNING Press ENTER to delete the current target folders, or Ctrl+C to abort *
	echo          *  (set 'CLEAN' to 'no' in batch file DefineVersion.bat to skip this step)      *
	echo          *********************************************************************************
	echo          *
	pause
	rmdir /s /q "..\..\Solutions\RC.bundle\Adm\%ADM%"
  
:Continue
	echo          *********************************************************************************
	echo          Transfering customization files for %ADM% to Solutions\RC-bundle 'build' folder...
	echo          *********************************************************************************
	echo          ON
	
	echo - 2D symbols and thumbnails for RC-CreateXxxx functions:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\2D    "..\..\Solutions\RC.bundle\Adm\%ADM%\2D"

if "%COPY3D%" neq "yes" goto Noecho
	echo - 3D object models:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\3D    "..\..\Solutions\RC.bundle\Adm\%ADM%\3D"
:Noecho

	echo - AutoCAD stuff (color table, fonts etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\AutoCAD    "..\..\Solutions\RC.bundle\Adm\%ADM%\AutoCAD"

	echo - DNA, style definitions and switches:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA    "..\..\Solutions\RC.bundle\Adm\%ADM%\DNA"

	echo - FAQ:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\FAQ    "..\..\Solutions\RC.bundle\Adm\%ADM%\FAQ"

	echo - Lua snippets and Tooltips (Lua and XAML code):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua    "..\..\Solutions\RC.bundle\Adm\%ADM%\Lua"

	@echo.
	@echo - Release notes:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\ReleaseNotes    "..\..\Solutions\RC.bundle\Adm\%ADM%\ReleaseNotes"

if "%TUTORIALS%" neq "yes" goto Noxcopy /Y /E /ITutorials
 	echo - Administration-specific tutorials:
	set SOURCEFOLDER=..\%ADM%\Tutorials
	set TARGETFOLDER=%USERPROFILE%\Documents\Github\RailCOMPLETE\Solutions\RC.bundle\Adm\%ADM%\Tutorials
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
:Noxcopy /Y /E /ITutorials

	echo - Vector images (administration's logo etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\VectorImages    "..\..\Solutions\RC.bundle\Adm\%ADM%\VectorImages"

	echo - WebLinks:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\WebLinks    "..\..\Solutions\RC.bundle\Adm\%ADM%\WebLinks"

	echo OFF
	echo          *********************************************************************************
	echo          ...Transfer completed.
	echo          *********************************************************************************

:TheEnd
echo     EXIT CopyDnaEtcToSolutionsRcBundle.bat
