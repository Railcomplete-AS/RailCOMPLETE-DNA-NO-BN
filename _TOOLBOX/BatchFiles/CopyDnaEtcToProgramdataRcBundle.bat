echo off
echo     ENTER CopyDnaEtcToAppdataRcBundle.bat
echo          Settings Adm=%ADM% Release=%RELEASE% Log=%LOG% Copy3D=%COPY3D% Tutorials=%TUTORIALS% Clean=%CLEAN%

echo          *********************************************************************************
echo          .
echo          Batch file to transfer DNA and related files from these folders with subfolders:
echo          ...\Github\RailCOMPLETE\Customization\XX-XX
echo          .
echo          to the folder/subfolders used by your local AutoCAD installation to run tests:
echo          %programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\XX-XX
echo          .
echo          This batch file must be called from this folder ('XX-XX' is NO-BN etc given by ADM):
echo           ...\Github\RailCOMPLETE\Customization\XX-XX
echo          .
echo          Please read about batch file processing here: https://ss64.com/nt
echo          .
echo          Please note that general folders and paths, which RC must know before a RC-START'ed 
echo          document has been opened, must be specified in the RC.bundle\startup.xml file.
echo          The administration-specific folders and paths, which RC must know when using RC-START'ed 
echo          document, must be specified in the RC.bundle\Adm\%ADM%\DNA folder's DNA file.
echo          .
echo          *********************************************************************************
pause

if "%ADM%" neq "" goto Continue
	echo          *
	echo          *********************************************************************************
	echo          The ADM argument cannot be undefined - should be one of the
	echo          administration abbrevs such as NO-BN.
	echo          Set ADM environment variable in calling batch file.
	echo          *********************************************************************************
	echo          *
goto TheEnd
 
:Continue
	echo          *********************************************************************************
	echo          Transfering customization files for %ADM% to local machine's APPDATA folder...
	echo          *********************************************************************************
	echo          ON
	
	echo - 2D symbols and thumbnails for RC-CreateXxxx functions:
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\2D\%ADM%-%RELEASE%-2D.dwg            					"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\2D\%ADM%-%RELEASE%-SymbolThumbnails.rc				"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D"

if "%COPY3D%" neq "yes" goto Noecho
	echo - 3D object models:
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\3D\STD\*.dwg											"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\3D\STD"
 	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\3D\LayerMappings\*.*									"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\3D\LayerMappings"
:Noecho

	echo - AutoCAD stuff (color table, fonts etc):
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\AutoCAD\*.*											"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\AutoCAD"

	echo - DNA, style definitions and switches:
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\DNA\%ADM%-%RELEASE%-DNA.xml							"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\DNA\%ADM%-%RELEASE%-StyleDefinitions.xml				"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\DNA\Switches\%ADM%-%RELEASE%-SwitchGeometries.xml 	"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA\Switches"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\DNA\DnaMappings\*.xml 								"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA\DnaMappings"

	echo - FAQ:
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\FAQ\%ADM%-%RELEASE%-faq.xml            				"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\FAQ"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\FAQ\Images\*.*					       				"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\FAQ\Images"

	echo - Lua snippets and Tooltips (Lua and XAML code):
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\*.xml								"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\TooltipViews\*.xaml				"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipViews"
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\Lua\LuaTooltipPages\TooltipImages\TooltipImages.xaml	"%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipImages"

if "%TUTORIALS%" neq "yes" goto Noxcopy /Y /E /ITutorials
 	echo - Administration-specific tutorials:
	set SOURCEFOLDER=..\%ADM%\Tutorials
	set TARGETFOLDER=%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\Tutorials
	echo Source=%SOURCEFOLDER%
	echo Target=%TARGETFOLDER%
	echo .
	rem echo d | xcopy /Y /E /I: /Y Suppress prompt to confirm overwriting a file. Can be preset in the echo d | xcopy /Y /E /ICMD env
	rem echo d | xcopy /Y /E /I: /E echo d | xcopy /Y /E /I folders and subfolders, including Empty folders. Can be used to modify /T.
	rem echo d | xcopy /Y /E /I: /I If in doubt always assume the destination is a folder e.g. when the destination does not exist.
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt "%SOURCEFOLDER%" "%TARGETFOLDER%"
	echo .
	rem Remove excess target folders (folder name starts with an underscore) - and its files, including subfolders:
	echo Run: for /f "tokens=*" %G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%G"
	for /f "tokens=*" %%G in ('dir /b "%TARGETFOLDER%\_*"') do rd /Q /S "%TARGETFOLDER%\%%G"
	echo .
:Noxcopy /Y /E /ITutorials

	echo - Vector images (administration's logo etc):
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\VectorImages\*.xaml									%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\VectorImages

	echo - WebLinks:
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\WebLinks\%ADM%-%RELEASE%-weblinks.xml   	         	%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\WebLinks
	echo d | xcopy /Y /E /I /exclude:xcopyignore.txt ..\%ADM%\WebLinks\Images\*.*									%programdata%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\WebLinks\Images

	echo OFF
	echo          *********************************************************************************
	echo          ...Transfer completed.
	echo          *********************************************************************************

:TheEnd
echo     EXIT CopyDnaEtcToAppdataBundle.bat
