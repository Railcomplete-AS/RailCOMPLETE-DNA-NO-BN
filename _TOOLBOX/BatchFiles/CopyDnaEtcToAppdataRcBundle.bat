@echo off
@echo	ENTER CopyDnaEtcToAppdataRcBundle.bat
@echo		Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%
@echo		*********************************************************************************
rem			.
rem			Batch file to transfer DNA and related files from these folders with subfolders:
rem			...\Github\RailCOMPLETE-XX-YY
rem			where 'RailCOMPLETE-XX-YY' is the name of the Github clone for this adm's DNA stuff.
rem			.
rem			to the folder/subfolders used by your local AutoCAD installation to run tests:
rem			%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\XX-XX
rem			.
rem			This batch file must be called from this folder ('XX-XX' is NO-BN etc given by ADM):
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
pause

if "%ADM%" neq "" goto Continue
	@echo.
	@echo          *********************************************************************************
	@echo          The ADM argument cannot be undefined - should be one of the
	@echo          administration abbrevs such as NO-BN.
	@echo          Set ADM environment variable in calling batch file.
	@echo          *********************************************************************************
	@echo.
goto TheEnd
 
:Continue
	@echo          *********************************************************************************
	@echo          Transfering customization files for %ADM% to local machine's APPDATA folder...
	@echo          *********************************************************************************
	@echo          ON
	
	@echo.
	@echo - 2D symbols and thumbnails for RC-CreateXxxx functions:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\2D\*.dwg    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\2D\*.rc    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D"

if "%COPY3D%" neq "yes" goto Noecho
	@echo.
	@echo - 3D object models:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\3D\STD    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\3D\STD"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\3D\LayerMappings    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\3D\LayerMappings"
:Noecho
	@echo.
	@echo - AutoCAD stuff (color table, fonts etc):
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\AutoCAD\*.*    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\AutoCAD"

	@echo.
	@echo - DNA, style definitions, switches and DNA mappings:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA\*.xml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA\Switches    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA\Switches"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\DNA\DnaMappings    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA\DnaMappings"

	@echo.
	@echo - FAQ:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\FAQ\*.xml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\FAQ"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\FAQ\Images\*.*    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\FAQ\Images"

	@echo.
	@echo - Lua snippets and Tooltips (Lua and XAML code):
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\*.xml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\TooltipViews\*.xaml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipViews"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\TooltipImages\TooltipImages.xaml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipImages"

	@echo.
	@echo - Release notes:
	@echo.
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\ReleaseNotes\*.txt    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\ReleaseNotes"

if "%TUTORIALS%" neq "yes" goto Noecho
	@echo.
 	@echo - Administration-specific tutorials:
	@echo.
	set SOURCEFOLDER=..\%ADM%\Tutorials
	set TARGETFOLDER=%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Tutorials
	@echo Source=%SOURCEFOLDER%
	@echo Target=%TARGETFOLDER%
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt "%SOURCEFOLDER%" "%TARGETFOLDER%"
	rem Remove excess target folders (folder name starts with an underscore) - and its files, including subfolders:
	@echo Run: for /f "tokens=*" %G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%G"
	for /f "tokens=*" %%G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%%G"
:Noecho d | xcopy /Y /E /I /exclude:..\xcopyignore.txtTutorials

	@echo .
	@echo - Vector images (administration's logo etc):
	@echo .
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\VectorImages\*.xaml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\VectorImages"

	@echo .
	@echo - WebLinks:
	@echo .
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\WebLinks\*.xml    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\WebLinks"
	@echo d | xcopy /Y /E /I /exclude:..\xcopyignore.txt ..\%ADM%\WebLinks\Images\*.*    "%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\WebLinks\Images"

	@echo .
	@echo OFF
	@echo          *********************************************************************************
	@echo          ...Transfer completed.
	@echo          *********************************************************************************
	@echo .

:TheEnd
@echo	EXIT CopyDnaEtcToAppdataBundle.bat
