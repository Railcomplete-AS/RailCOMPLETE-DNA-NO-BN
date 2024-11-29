echo off
rem Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
echo	ENTER CopyDnaEtcToAppdataRcBundle.bat
echo		Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%
rem			*********************************************************************************
rem			
rem			In the following, 'XX-YY' denotes any administration. This batch file is identical for all.
rem
rem			Batch file to transfer DNA and related files from folder/subfolders:
rem
rem			...\Github\RailCOMPLETE-XX-YY
rem
rem			to folder/subfolders used by your local AutoCAD installation to run tests:
rem
rem			%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\XX-YY
rem			
rem			Read more about batch file processing here: https://ss64.com/nt.
rem			
rem			Note that general folders and paths, which RC must know before a RC-START'ed 
rem			document has been opened, must be specified in the RC.bundle\startup.xml file.
rem			The administration-specific folders and paths, which RC must know when using RC-START'ed 
rem			document, must be specified in the RC.bundle\Adm\%ADM%\DNA folder's DNA file.
rem			
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
if "%CLEAN%" neq "yes" goto Continue1
	rem 
	rem Note on RMDIR syntax:
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
	rmdir /s /q "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%"
  
:Continue1
	echo          *********************************************************************************
	echo          Transfering customization files for %ADM% to local machine's APPDATA folder...
	echo          *********************************************************************************
	echo          ON
	
	echo - 2D symbols and thumbnails for RC-CreateXxxx functions:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\2D    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D"

if "%COPY3D%" neq "yes" goto Continue2
	echo - 3D object models:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\3D    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\3D"

:Continue2
	echo - AutoCAD stuff (color table, fonts etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\AutoCAD    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\AutoCAD"

	echo - DNA, switches and DNA mappings:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA"

	echo - FAQ:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\FAQ    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\FAQ"

	echo - Lua scripts, Lua functions snippets and Tooltips (Lua and XAML code):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua"

	echo - Psets:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Psets    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Psets"

	echo - Release notes:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\ReleaseNotes    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\ReleaseNotes"

if "%TUTORIALS%" neq "yes" goto Continue3
 	echo - Administration-specific tutorials:
	set SOURCEFOLDER=..\%ADM%\NationalTutorials
	set TARGETFOLDER=%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\NationalTutorials
	echo Source=%SOURCEFOLDER%
	echo Target=%TARGETFOLDER%
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt "%SOURCEFOLDER%" "%TARGETFOLDER%"
	rem Remove excess target folders (folder name starts with an underscore) - and its files, including subfolders:
	echo Run: for /f "tokens=*" %G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%G"
	for /f "tokens=*" %%G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%%G"

:Continue3
	echo - Vector images (administration's logo etc):
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\VectorImages    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\VectorImages"

	echo - WebLinks:
	echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\WebLinks    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\WebLinks"

	echo OFF
	echo          *********************************************************************************
	echo          ...Transfer completed.
	echo          *********************************************************************************

:TheEnd
echo	EXIT CopyDnaEtcToAppdataBundle.bat
