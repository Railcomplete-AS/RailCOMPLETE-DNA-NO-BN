--[[
	DWG Normalization
	=================
	Normalizes 3D geometry DWG files by setting standard AutoCAD system variables
	and purging unused content.

	2025-12-15 v1.0 WIWIJ Created.
	2026-02-09 v1.1 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
--]]



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _VERSION_ = "1.1"
local _HEADER_ = lib2.language(
	{EN="Normalize 3D geometry files",
	NO="Normaliser 3D DWG-filer",
	FR="Normaliser les fichiers géométriques 3D",
	DE="3D-Geometriedateien normalisieren"})

-- Commands:
local _NORMALIZE_ = _HEADER_
local _HELP_ = lib2.language({EN="Help", NO="Hjelp", FR="Aide", DE="Hilfe"})
local _TERMINATE_ = lib2.language({EN="Terminate", NO="Avslutt", FR="Terminer", DE="Abbrechen"})

-- Dialogs:
local _SELECT_ACTION_MSG_ = lib2.language(
	{EN="Select action:\n\nOpen your scripting log window before running this script to keep track of progress.",
	NO="Velg handling:\n\nÅpne skriptloggvinduet før du kjører dette skriptet for å følge med på fremdriften.",
	FR="Sélectionnez l'action :\n\nOuvrez la fenêtre du journal des scripts avant d'exécuter ce script afin de suivre sa progression.",
	DE="Aktion auswählen:\n\nÖffnen Sie vor dem Ausführen dieses Skripts das Skriptprotokollfenster, um den Fortschritt zu verfolgen."})

local _ASK_FOR_3D_GEOMETRY_FOLDER_MSG_ = lib2.language(
	{EN="Choose the folder containing the 3D geometries",
	NO="Velg katalog som inneholder 3D-geometrifiler",
	FR="Sélectionnez le dossier contenant les géométries 3D",
	DE="Wählen Sie den Ordner aus, der die 3D-Geometrien enthält"})

local _BAD_SELECTION_MSG_ = lib2.language({EN="Invalid menu selection", NO="Ugyldig menyvalg", FR="Sélection de menu non valide", DE="Ungültige Menüauswahl"})
local _TERMINATED_MSG_ = lib2.language({EN="Terminated.", NO="Utført.", FR="Terminé.", DE="Beendet."})

local greetingMsg = _HEADER_ .. "\n\n" .. "Version " .. _VERSION_

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
runCommand('(command "._SNAP" 1.0 "._OSNAP" "_OFF" "._OSNAPCOORD" 1 "._SNAPMODE" 0 "._GRID" "_OFF") ')
runCommand('(command "._UNITS" 2 3 1 3 0 "_NO" "._LUNITS" 2 "._LUPREC" 3 "._AUNITS" 0 "._AUPREC" 3 "._INSUNITS" 6 "._LIGHTINGUNITS" 2 ) ')
runCommand('(command "._DIMZIN" 8 "._OSMODE" 0 "._COORDS" 1 "._PICKBOX" 5 "._DYNPICOORDS" 1 "._DYNPIFORMAT" 1 "._ORTHOMODE" 0 "._PICKFIRST" 1 "._PICKADD" 0 "._ATTREQ" 0 "._ATTDIA" 0 "._FILEDIA" 1 ) ')
runCommand('(command "._ZOOM" "_EXTENTS") ')
runCommand('(command "._PLINE" "0,0" "_WIDTH" 0 0 "") ')
runCommand('(command "._PURGE" "_ALL" "" "_NO") ')
]]



---SCRIPT---
lib2.show(greetingMsg, _HEADER_)

local option
repeat
	option = askForKeyword(_SELECT_ACTION_MSG_, {_NORMALIZE_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)

	elseif option == _TERMINATE_ then
		-- Fall through

	elseif option == _NORMALIZE_ then
		lib2.show(_ASK_FOR_3D_GEOMETRY_FOLDER_MSG_, _HEADER_)
		local folderName = askForFolderName(_ASK_FOR_3D_GEOMETRY_FOLDER_MSG_)
		local fileNames = table.where(getFilesInFolder(folderName, "*.dwg"), function(x) return x end)
		local t
		for _, f in pairs(fileNames) do
			t = t and t .. "\n" .. f or f
		end
		lib2.show("Folder:\n" .. folderName .. "\n\nFiles:\n" .. t, _HEADER_)

		local nNormalized = 0
		for _, fileName in pairs(fileNames) do
			local fileName = fileName:gsub("\\", "\\\\")
			nNormalized = nNormalized + 1
			write(string.format("File %04d: %s...", nNormalized, fileName))

			runCommand('(vla-activate (vla-open (vla-get-documents (vlax-get-acad-object)) "' .. fileName .. '" :vlax-false)) ')

			runCommand('(command "._SNAP" 1.0 "._OSNAP" "_OFF" "._OSNAPCOORD" 1 "._SNAPMODE" 0 "._GRID" "_OFF") ')
			runCommand('(command "._UNITS" 2 3 1 3 0 "_NO" "._LUNITS" 2 "._LUPREC" 3 "._AUNITS" 0 "._AUPREC" 3 "._INSUNITS" 6 "._LIGHTINGUNITS" 2 ) ')
			runCommand('(command "._DIMZIN" 8 "._OSMODE" 0 "._COORDS" 1 "._PICKBOX" 5 "._DYNPICOORDS" 1 "._DYNPIFORMAT" 1 "._ORTHOMODE" 0 "._PICKFIRST" 1 "._PICKADD" 0 "._ATTREQ" 0 "._ATTDIA" 0 "._FILEDIA" 1 ) ')
			runCommand('(command "._ZOOM" "_EXTENTS") ')
			runCommand('(command "._PLINE" "0,0" "_WIDTH" 0 0 "") ')
			runCommand('(command "._PURGE" "_ALL" "" "_NO") ')
			runCommand("_QSAVE ")
			runCommand("_CLOSE ")
			write("Done\n")
		end
		lib2.show(nNormalized .. " files normalized.", _HEADER_)

	else
		-- Provoke error and stop:
		lib2.stop(_BAD_SELECTION_MSG_ .. " [" .. option .. "].")
	end
until option == _TERMINATE_
lib2.show(_TERMINATED_MSG_, _HEADER_)
