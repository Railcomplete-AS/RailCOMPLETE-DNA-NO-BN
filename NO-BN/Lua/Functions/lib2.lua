--[[
	lib2.lua
	========
	Lua library functions callable from a Lua script context.
	
	The base folder for includeLuaFile() calls is: "...\RC.bundle\Adm\XX-YY", where 'XX-YY'
	is your railway administration's DNA abbreviation.

	Usage: 
	
	--Include library functions for use in a Lua script or in a Lua function:
	lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
	return lib1.p2s(getPoint3D()) --Pretty-print X Y Z coordinates to  3 decimal places.

	2026-01-30 v1.0 CLFEY Created.
	2026-02-09 v1.1 CLFEY Updated stop() function to use show() before stopping. Added language support.
--]]


--CONSTANTS--
--The version of the lib2 scripting function library. Read as lib2._VERSION_.
_VERSION_ = "1.1"

--The ISO 3166-2 2-letter country identifier of the current document's DNA. Read as lib2._DNA_COUNTRY_.
_DNA_COUNTRY_ = DocumentData.DnaIri:match("^(%w%w)%-%w%w.+$")


--FUNCTIONS--
--Writes a message to the log window and appends a newline. Call as lib2.writeln(msg, symbol = nil). The optional symbol is one of {nil | _ok | _warning | _error}. Symbols affect the color of the message which is echoed to the log window.
function writeln(msg, symbol)
	return msg and write(msg.."\n", symbol or _noSymbol) or write("\n", symbol or _noSymbol)
end

--Creates a popup-window showing a message and an 'OK' menu choice. Call as lib2.show(msg, header = nil, symbol = nil). The optional header replaces 'Keyword' in the window's header. The optional symbol is one of {nil | _ok | _warning | _error}. Symbols affect the color of the message which is echoed to the log window.
function show(msg, header, symbol)
	writeln(msg, symbol)
	askForKeyword(msg, {"OK"}, header) --Window caption becomes "Keyword" if header is nil.
end

--Provokes an error. Call as stop(msg). A popup-window alerts the user before the script stops. The corresponding Lua source code line number is found in the scripting or debugger log window.
function stop(msg)
	--DNA country-dependent error message header.
	local _ERROR_ = (_DNA_COUNTRY_ == "NO" and "ERROR: ") or (_DNA_COUNTRY_ == "FR" and "ERREUR : ") or (_DNA_COUNTRY_ == "DE" and "FEHLER: ")
	show(_ERROR_..msg.."\n\nContact support@railcomplete.com.", _ERROR_) local x = ""..nil
end

--Selects everything on visible layers in modelspace. Call as lib2.selectAll(). Note: Sets PICKADD to 2 without returning it to its to previous value.
function selectAll()
	runCommand('_PICKADD 2 ') return runCommand('_SELECT _AL \n').result
end

--Selects the last thing that was treated by the CAD system. Call as lib2.selectLast().
function selectLast()
	return runCommand('_SELECT _L \n').result[0]
end

--Returns a table with paths to shapefiles. Each entry is a table of file paths. Call as lib2.getShapefilePaths().
function getShapefilePaths()
	local _SELECT_SHAPEFILE_FOLDER_ = (_DNA_COUNTRY_ == "NO" and "Select folder containing Shapefiles")
		or (_DNA_COUNTRY_ == "FR" and "Sélectionnez le dossier contenant le fichier Shapefile")
		or (_DNA_COUNTRY_ == "DE" and "Ordner auswählen, der die Shapefile-Datei enthält")
	local function getShapefilesInFolder(folder, shapefileTable)
		local filesInFolder = table.select(getFilesInFolder(folder))
		for _, file in pairs(filesInFolder) do
			local a = string.sub(file, #file - 3)
			if a == ".shp" then
				table.insert(shapefileTable, file)
			end
		end
		local foldersInFolder = table.select(getFoldersInFolder(folder))
		for _, f in pairs(foldersInFolder) do
			getShapefilesInFolder(f, shapefileTable)
		end
	end
	local shapefileFolder = askForFolderName(_SELECT_SHAPEFILE_FOLDER_)
	if not shapefileFolder then return nil end
	local t = {}
	getShapefilesInFolder(shapefileFolder, t)
	return t
end

--Sets the CAD environment to default. Call as lib2.setDefaultCadSettings(). Actions taken are: 1. PICKADD 0: Disables PICKADD. The last selected objects become the selection set. 2. FILEDIA 1: Open files with a normal explorer window. 3. ORTHOMODE 0: The cursor is not snapped to grid lines. 4. GRID OFF: Do not display grid lines. 5. SNAP OFF: Do not snap to grid. 6. PICKBOX 5: Set the default cursor size. 7. NAVVCUBE ON: Enable the navigation cube at the top right. 8. DYNMODE 3: Display text input and suggested commands next to the cursor while typing. 9. SELECTIONCYCLING 2: If you click on multiple objects at the same time, a window will appear allowing you to choose the one you want. 10. UNITS: Use measurements with an accuracy of 3 decimal places. East is on the right, angles in decimal degrees are measured counterclockwise. 11. OSNAP: Enable all OSNAPs. 12. COLOR: Set the default color to ByLayer.
function setDefaultCadSettings()
	runCommand("_PICKADD 0 ") -- Disables PICKADD. The last selected objects become the selection set.
    runCommand("_FILEDIA 1 ") -- Open files with a normal explorer window.
	runCommand("_ORTHOMODE 0 ") -- The cursor is not snapped to grid lines.
    runCommand("_GRID _OFF ") -- Do not display grid lines.
    runCommand("_SNAP _OFF ") -- Do not snap to grid.
	runCommand("_PICKBOX 5 ") -- Set the default cursor size.
    runCommand("_NAVVCUBE _OFF ")
    runCommand("_NAVVCUBE _ON ") -- Enable the navigation cube at the top right.
	runCommand("_DYNMODE 3 ") -- Display text input and suggested commands next to the cursor while typing.
	runCommand("_SELECTIONCYCLING 2 ") -- If you click on multiple objects at the same time, a window will appear allowing you to choose the one you want.
	runCommand("_-UNITS 2 3 1 3 0 _NO ") -- Use measurements with an accuracy of 3 decimal places. East is on the right, angles in decimal degrees are measured counterclockwise.
	runCommand("_-OSNAP _END,_MID,_CEN,_GCE,_NOD,_QUA,_INT,_EXT,_INS,_PER,_TAN,_NEA,_APP,_PAR ") -- Enable all OSNAPs.
	runCommand("_-COLOR _BYLAYER ") -- Set the default color.
end

--Hides (setting 0) polyline grips (0=hide grips / 1=display grips / 2=display additional midpoint grips on polyline segments (default)). Call as gripsOff().
function gripsOff()
	runCommand("_GRIPS 0 ")
end

--Shows (setting 2) polyline grips including midpoint grips (0=hide grips / 1=display grips / 2=display additional midpoint grips on polyline). Call as gripsOn().
function gripsOn()
	runCommand("_GRIPS 2 ")
end

--Zooms modelspace to extents of everything that is visible. Call as zoomExtents().
function zoomExtents()
	runCommand('(command "._ZOOM" "_EXTENTS") ')
end

--Return a language-dependent string from the input argument dictionary of strings. Call as lib2.language({EN="this", NO="dette", FR="ceci", DE="dieses"}). The returned string is selected on the basis of the ISO 3166-2 two-letter country abbreviation that is part of the active document's DNA signature (your current railway administration).
function language(tableOfIso3166_2IndexedStrings)
	return tableOfIso3166_2IndexedStrings[_DNA_COUNTRY_]
end
