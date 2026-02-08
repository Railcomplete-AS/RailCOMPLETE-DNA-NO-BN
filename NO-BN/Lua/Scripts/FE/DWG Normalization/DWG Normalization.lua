--[[
	2025-12-15 v1.0 WIWIJ Created.
	2026-02-08 v1.1 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
--]]

--FUNCTIONS--
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua") --Common Lua functions
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua") --Scripting-only Lua functions

local language = lib2._DNA_COUNTRY_

--CONSTANTS--
local _HEADER_ = "DWG Normalization"
local _VERSION_ = "1.1"

local _NORMALIZE_ = "Normalize 3D geometry files"
local _HELP_ = "Help"
local _TERMINATE_ = "Terminate"

local usageMsg = [[
This script normalizes an AutoCAD DWG file to a consistent drafting and unit configuration for use with RailCOMPLETE 3D libraries.

Open your scripting log window before running this script to keep track of progress.

Version ]].._VERSION_

local helpMsg = [[
- Disables object snaps and snap mode.
- Forces absolute coordinate input behavior.
- Disables grid and ortho mode.
- Sets drawing and insertion units to meters (no rescaling).
- Sets linear and angular units to decimal with 3-decimal precision.
- Adjusts selection behavior and pickbox size.
- Configures dynamic input and attribute prompting behavior.
- Zooms the active viewport to drawing extents.
- Purges the .DWG file

Actual script contents:

runCommand('(vla-activate (vla-open (vla-get-documents (vlax-get-acad-object)) "'..file..'" :vlax-false)) ')
runCommand('(command "._SNAP" 1.0 "._OSNAP" "OFF" "._OSNAPCOORD" 1 "._SNAPMODE" 0 "._GRID" "OFF") ')
runCommand('(command "._UNITS" 2 3 1 3 0 "_NO" "._LUNITS" 2 "._LUPREC" 3 "._AUNITS" 0 "._AUPREC" 3 "._INSUNITS" 6 "._LIGHTINGUNITS" 2 ) ')
runCommand('(command "._DIMZIN" 8 "._OSMODE" 0 "._COORDS" 1 "._PICKBOX" 5 "._DYNPICOORDS" 1 "._DYNPIFORMAT" 1 "._ORTHOMODE" 0 "._PICKFIRST" 1 "._PICKADD" 0 "._ATTREQ" 0 "._ATTDIA" 0 "._FILEDIA" 1 ) ')
runCommand('(command "._ZOOM" "_EXTENTS") ')
runCommand('(command "._PLINE" "0,0" "_WIDTH" 0 0 "") ')
runCommand('(command "._PURGE" "ALL" "" "NO") ')
]]


--SCRIPT--
lib2.show(usageMsg, _HEADER_)

local option
repeat
	option = askForKeyword("Select action:", {_NORMALIZE_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)
		
	elseif option == _NORMALIZE_ then
		lib2.show("Choose the folder containing the 3D geometries", _HEADER_)
		local folderName = askForFolderName("Choose the folder containing the 3D geometries")
		local fileNames = table.where(getFilesInFolder(folderName, "*.dwg"), function (x) return x end)
		local t
		for _, f in pairs(fileNames) do
			t = t and t.."\n"..f or f
		end
		lib2.show("Folder:\n"..folderName.."\n\nFiles:\n"..t, _HEADER_)
		
		local nNormalized = 0
		for _, fileName in pairs(fileNames) do
			local fileName = fileName:gsub("\\", "\\\\")
			nNormalized = nNormalized + 1
			write(string.format("File %04d: %s...", nNormalized, fileName))
			
			runCommand('(vla-activate (vla-open (vla-get-documents (vlax-get-acad-object)) "'..fileName..'" :vlax-false)) ')
			
			runCommand('(command "._SNAP" 1.0 "._OSNAP" "OFF" "._OSNAPCOORD" 1 "._SNAPMODE" 0 "._GRID" "OFF") ')
			runCommand('(command "._UNITS" 2 3 1 3 0 "_NO" "._LUNITS" 2 "._LUPREC" 3 "._AUNITS" 0 "._AUPREC" 3 "._INSUNITS" 6 "._LIGHTINGUNITS" 2 ) ')
			runCommand('(command "._DIMZIN" 8 "._OSMODE" 0 "._COORDS" 1 "._PICKBOX" 5 "._DYNPICOORDS" 1 "._DYNPIFORMAT" 1 "._ORTHOMODE" 0 "._PICKFIRST" 1 "._PICKADD" 0 "._ATTREQ" 0 "._ATTDIA" 0 "._FILEDIA" 1 ) ')
			runCommand('(command "._ZOOM" "_EXTENTS") ')
			runCommand('(command "._PLINE" "0,0" "_WIDTH" 0 0 "") ')
			runCommand('(command "._PURGE" "ALL" "" "NO") ')
			runCommand("qsave ")
			runCommand("close ")
			write("Done\n")
		end
		lib2.show(nNormalized.." files normalized.", _HEADER_)
		
	elseif option == _TERMINATE_ then
		--Fall through
		
	else
		--Provoke error and stop
		lib2.show("Bad option ["..option.."] - contact support@railcomplete.com.", _HEADER_)
		lib2.stop()
	end
until option == _TERMINATE_
lib2.show("Terminated.", _HEADER_)
