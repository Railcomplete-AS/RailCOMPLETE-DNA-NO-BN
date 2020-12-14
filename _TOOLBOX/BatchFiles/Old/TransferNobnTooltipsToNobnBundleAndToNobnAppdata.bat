set ADM=NO-BN
set TARGET=NO-BN
rem Until we have separated out FR-SR etc as their own installation objects, all will be copied to NO-BN in appdata "act under cover as NO-BN"

@echo on
@echo ****************************************
@echo Batch file to transfer tooltips:
@echo from Github\RailCOMPLETE\Customization\%ADM%\Lua
@echo to Github\RailCOMPLETE\Solutions\RC.bundle\Adm\%ADM%\Lua 
@echo and to %APPDATA%\Roaming\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua

@echo ****************************************

	@echo *
	@echo ****************************************
	@echo *** Transfering tooltip files from %ADM% Customization folder to RC.bundle\...\%ADM%, and to Appdata RC.bundle\...\%TARGET% for testing...
	@echo ****************************************
	@echo *
	  
	echo *** Deleting old tooltip files from Solutions\...\%ADM%...
@dir ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\*.xml
pause
	@del ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\*.xml
	@del ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipViews\*.xaml
	@del ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipImages\*.xaml

	echo *** Deleting old tooltip files from appdata\%TARGET%...
@dir %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\*.xml
pause
	@del %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\*.xml
	@del %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\TooltipViews\*.xaml
	@del %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\TooltipImages\*.xaml

	echo ***Copying updated tooltips files from Github\RailCOMPLETE\Customization\%ADM%\Lua to Github\Solutions\RC.bundle\%ADM%\Lua...
@dir .\LuaTooltipPages\*.*
pause
	@copy .\LuaTooltipPages\*.xml                ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages
	@copy .\LuaTooltipPages\TooltipViews\*.xaml  ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipViews
	@copy .\LuaTooltipPages\TooltipImages\*.xaml ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipImages
	@echo .
	@echo ****************************************

	
	echo ***Copying new tooltips files from Github\%ADM% to appdata\%TARGET%...
@dir ..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\*.*
pause
	@copy ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\*.xml                %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages
	@copy ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipImages\*.xaml %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\TooltipImages
	@copy ..\..\..\Solutions\RC.bundle\Adm\%ADM%\Lua\LuaTooltipPages\TooltipViews\*.xaml  %APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\%TARGET%\Lua\LuaTooltipPages\TooltipViews
	@echo .
	@echo ****************************************

Pause
