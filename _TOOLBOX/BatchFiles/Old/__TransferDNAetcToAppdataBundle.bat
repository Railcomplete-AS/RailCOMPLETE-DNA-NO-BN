@echo off
@echo ****************************************
@echo Batch file to transfer DNA from Github
@echo to appdata RC.Bundle local folder.
@echo ****************************************

if "%ADM%" equ "" (
	@echo *
	@echo ****************************************
	@echo No argument given - should be one of the
	@echo administration abbrevs such as NO-BN.
	@echo ****************************************
	@echo *
	@pause
	@goto TheEnd
) else (
	@echo *
	@echo ****************************************
	@echo *** Transfering files for %ADM%...   ***
	@echo ****************************************
	@echo *
	
	@copy ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\%fileNameForReleaseCandidate%*.xml %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA
	@copy ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\StyleDefinitions.xml %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA
	@copy ..\..\Solutions\RC.bundle\Adm\%ADM%\DNA\Switches\%ADM%-SwitchGeometries.xml %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\DNA\Switches
	@copy ..\..\Solutions\RC.bundle\Adm\%ADM%\2D\%ADM%-2D.dwg %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D
	@copy ..\..\Solutions\RC.bundle\Adm\%ADM%\2D\SymbolThumbnails.rc %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%ADM%\2D
	@echo .
	@echo ****************************************
	@echo .
	@echo Transfer done from Github local repository to local % appdata % folder: 
	@echo .
	@echo - DNA
	@echo - Style definitions
	@echo - Switch geometries
	@echo - 2D symbols
	@echo - Thumbnails for CreateXxx functions
	@echo .
	@echo ****************************************
	@pause
)
:TheEnd
