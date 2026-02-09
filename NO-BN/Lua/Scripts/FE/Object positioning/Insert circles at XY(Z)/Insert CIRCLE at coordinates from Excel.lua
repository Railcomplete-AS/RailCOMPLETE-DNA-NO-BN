--[[
	2024-10-10 v1.0 CLFEY Created.
	2024-10-11 v1.1 KNHEL Added call to cadInterface.addEntitiesToModelSpace({circle}).
	2024-10-12 v1.2 CLFEY Beauty fixes, 'local' declarations, tolerate non-number rows.
	2026-02-09 v1.3 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
--]]


--FUNCTIONS--
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua") --Common Lua functions
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua") --Scripting-only Lua functions


--CONSTANTS--
local _VERSION_ = "1.3"
local _HEADER_ = lib2.language(
	{EN="Insert circles at XY(Z) coordinates given from Excel file",
	NO="Sett inn sirkler på XY(Z)-koordinater gitt fra Excel-fil",
	FR="Insérer des cercles aux coordomnnées XY(Z) fournis par le fichier Excel",
	DE="Einfügen von Kreisen an XY(Z)-Koordinaten aus Excel-Datei"})

--Commands
local _CIRCLES_ = _HEADER_
local _HELP_ = lib2.language({EN="Help", NO="Hjelp", FR="Aide", DE="Hilfe"})
local _TERMINATE_ = lib2.language({EN="Terminate", NO="Avslutt", FR="Terminer", DE="Abbrechen"})

--Dialogs
local _SELECT_ACTION_MSG_ = lib2.language(
	{EN="Select action:\n\nClose the Excel file and open your scripting log window before running this script to keep track of progress.",
	NO="Velg handling:\n\nLukk Excel-filen og åpne skriptloggvinduet før du kjører dette skriptet for å følge med på fremdriften.",
	FR="Sélectionnez l'action :\n\nFermez le fichier Excel et ouvrez la fenêtre du journal des scripts avant d'exécuter ce script afin de suivre sa progression.",
	DE="Aktion auswählen:\n\nSchließen Sie die Excel-Datei und öffnen Sie Ihr Skriptprotokollfenster, bevor Sie dieses Skript ausführen, um den Fortschritt zu verfolgen."})

local _ASK_FOR_EXCEL_FILE_MSG_ = lib2.language(
	{EN="Select Excel file with XY coordinates columns with caption 'X' and 'Y'.",
	NO="Velg Excel-fil med XY-koordinatkolonner med overskriften «X» og «Y».",
	FR="Sélectionnez le fichier Excel contenant les colonnes de coordonnées XY intitulées « X » et « Y ».",
	DE="Wählen Sie eine Excel-Datei mit XY-Koordinatenspalten mit den Beschriftungen „X“ und „Y“ aus."})

local _BAD_SELECTION_MSG__ = lib2.language({EN="Invalid menu selection", NO="Ugyldig menyvalg", FR="Sélection de menu non valide", DE="Ungültige Menüauswahl"})
local _TERMINATED_MSG_ = lib2.language({EN="Terminated.", NO="Utført.", FR="Terminé.", DE="Beendet."})

local greetingMsg = _HEADER_.."\n\n".."Version ".._VERSION_

local helpMsg = [[
	Input:
	-	Set current layer using CLAYER command or other method.
	-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row.
	-	Subsequent rows contain X and Y coordinates in the 'X' and the 'Y' columns.
	-	Ensure your model represents the same coordinate system as your XY points.
	-	Close the Excel file before running the script.
	-	Use Edit Script and enable the Log output window to see more info from the execution.
	
	Output:
	-	CIRCLEs are created on the current layer for each XY pair.
]]


--SCRIPT--
lib2.show(greetingMsg, _HEADER_)

local option
repeat
	option = askForKeyword(_SELECT_ACTION_MSG_, {_CIRCLES_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)
		
	elseif option == _TERMINATE_ then
		--Fall through
		
	elseif option == _CIRCLES_ then
		local xCaption = "X"
		local yCaption = "Y"
		local radius = 1
		
		lib2.show(_ASK_FOR_EXCEL_FILE_MSG_, _HEADER_)
		local filename =  askForFileName(_ASK_FOR_EXCEL_FILE_MSG_) 
		local file = getContentsFromFile(FileType.Excel,"", filename)
		local sheets = getExpandoObjectPropertyNames(file)
		local sheetName = sheets[0]
		local items = file[sheetName]
		local nItems = getCollectionLength(items)
		local nTreated = 0
		lib2.show(nItems.." rows found in sheet "..sheetName.." in file "..filename, _HEADER_)
		
		beginUndoBufferItem()
		for i = 0,nItems-1 do
		    local item = items[i]
			local x = item[xCaption]
			local y = item[yCaption]
		    if type(x) == "number" and type(y) == "number" then
				nTreated = nTreated + 1
			    --This call ensures that undo buffering works (Ctrl+Z will undo all CIRCLEs in one operation):
		    	local insertionPoint = cadInterface.createCadEntity("Geometry.Point3d", {x, y, 0})
		    	local normalVector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
		    	local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {insertionPoint, normalVector, radius})
		    	cadInterface.addEntitiesToModelSpace({circle}) --Add graphics to drawing
				write(string.format("%04d Inserted circle at (%.03f, %.03f)\n", i, x, y))
			else
				write(string.format("%04d Skipping row since %s=%s or %s=%s is not a number\n", i,  xCaption, x, yCaption, y), _error)
			end
		end
		endUndoBufferItem()
		lib2.show(nTreated.." circles were inserted.", _HEADER_)
		lib2.zoomExtents()
		
	else
		--Provoke error and stop
		lib2.stop(_BAD_SELECTION_MSG_.." ["..option.."].")
	end
until option == _TERMINATE_
lib2.show(_TERMINATED_MSG_, _HEADER_)
