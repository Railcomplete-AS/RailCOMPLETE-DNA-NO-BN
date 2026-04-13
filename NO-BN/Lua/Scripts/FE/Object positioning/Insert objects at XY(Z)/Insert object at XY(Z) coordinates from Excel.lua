--[[
	Insert object at XY(Z) coordinates from Excel
	===============================================
	Inserts RC objects at XY(Z) coordinates read from an Excel file,
	using a template object's RcType, Variant and Alignment.

	2024-12-05 v1.0 CLFEY Created (from similar script inserting just a circle at XY).
	2025-01-17 v2.0 KNHEL Changed elevation to use elevation above mean sea level rather than relative to track.
	2026-02-09 v2.1 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
--]]



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _VERSION_ = "2.1"
local _HEADER_ = lib2.language(
	{EN="Insert objects at XY(Z) coordinates from Excel",
	NO="Sett inn objekter på XY(Z)-koordinater gitt fra Excel-fil",
	FR="Insérer des objets aux coordonnées XY(Z) fournis par le fichier Excel",
	DE="Einfügen von Objekten an XY(Z)-Koordinaten aus Excel-Datei"})

-- Commands:
local _INSERT_OBJECTS_XY_ = lib2.language(
	{EN="Insert objects at XY coordinates (locked to its alignment's local elevation)",
	NO="Sett inn objekter ved XY-koordinater (låst til den lokale høyden i egen linje)",
	FR="Insérer des objets aux coordonnées XY (verrouillés à l'élévation locale de leur axe)",
	DE="Objekte an XY-Koordinaten einfügen (an die lokale Höhe der Achse gebunden)"})
local _INSERT_OBJECTS_XYZ_ = lib2.language(
	{EN="Insert objects' at XYZ coordinates (removing possible DNA formula on the VerticalOffset property)",
	NO="Sett inn objekter på XYZ-koordinater (fjerner eventuell DNA-formel på VerticalOffset-egenskapen)",
	FR="Insérer des objets aux coordonnées XYZ (en supprimant la formule ADN éventuelle dans la propriété VerticalOffset)",
	DE="Objekte an den XYZ-Koordinaten einfügen (mögliche DNA-Formel in der Eigenschaft „VerticalOffset" wird entfernt)"})
local _HELP_ = lib2.language({EN="Help", NO="Hjelp", FR="Aide", DE="Hilfe"})
local _TERMINATE_ = lib2.language({EN="Terminate", NO="Avslutt", FR="Terminer", DE="Abbrechen"})

-- Dialogs:
local _SELECT_ACTION_MSG_ = lib2.language(
	{EN="Select action:\n\nClose the Excel file and open your scripting log window before running this script to keep track of progress.",
	NO="Velg handling:\n\nLukk Excel-filen og åpne skriptloggvinduet før du kjører dette skriptet for å følge med på fremdriften.",
	FR="Sélectionnez l'action :\n\nFermez le fichier Excel et ouvrez la fenêtre du journal des scripts avant d'exécuter ce script afin de suivre sa progression.",
	DE="Aktion auswählen:\n\nSchließen Sie die Excel-Datei und öffnen Sie Ihr Skriptprotokollfenster, bevor Sie dieses Skript ausführen, um den Fortschritt zu verfolgen."})

local _ASK_FOR_XY_EXCEL_FILE_MSG_ = lib2.language(
	{EN="Select Excel file where the first worksheet has column captions 'X' and 'Y' (a 'Z' column will be ignored)",
	NO="Velg Excel-fil der det første regnearket har kolonneoverskriftene «X» og «Y» (en «Z»-kolonne vil bli ignorert).",
	FR="Sélectionnez le fichier Excel dont la première feuille de calcul comporte les en-têtes de colonne « X » et « Y » (la colonne « Z » sera ignorée).",
	DE="Wählen Sie eine Excel-Datei aus, in der das erste Arbeitsblatt die Spaltenüberschriften „X" und „Y" enthält (eine Spalte „Z" wird ignoriert)."})

local _ASK_FOR_XYZ_EXCEL_FILE_MSG_ = lib2.language(
	{EN="Select Excel file where the first worksheet has column captions 'X' and 'Y' and 'Z'",
	NO="Velg Excel-fil der det første regnearket har kolonneoverskriftene «X», «Y» og «Z».",
	FR="Sélectionnez le fichier Excel dont la première feuille de calcul comporte les en-têtes de colonnes « X », « Y » et « Z ».",
	DE="Wählen Sie eine Excel-Datei aus, in der das erste Arbeitsblatt die Spaltenüberschriften „X", „Y" und „Z" enthält."})

local _SELECT_TEMPLATE_OBJECT_MSG_ = lib2.language(
	{EN="Select an existing object as template.\n\nIts RcType and Variant will be used as a template for inserting similar objects if no RcType and Variant is stated in the Excel file",
	NO="Velg et eksisterende objekt som mal.\n\nDets RcType og Variant vil bli brukt som mal for å sette inn lignende objekter hvis ingen RcType og Variant er angitt i Excel-filen.",
	FR="Sélectionnez un objet existant comme modèle. Son RcType et son Variant seront utilisés comme modèle pour insérer des objets similaires si aucun RcType et Variant n'est spécifié dans le fichier Excel",
	DE="Wählen Sie ein vorhandenes Objekt als Vorlage aus. Sein RcType und Variant werden als Vorlage für das Einfügen ähnlicher Objekte verwendet, wenn in der Excel-Datei kein RcType und Variant angegeben ist"})

local _BAD_SELECTION_MSG_ = lib2.language({EN="Invalid menu selection", NO="Ugyldig menyvalg", FR="Sélection de menu non valide", DE="Ungültige Menüauswahl"})
local _TERMINATED_MSG_ = lib2.language({EN="Terminated.", NO="Utført.", FR="Terminé.", DE="Beendet."})

local greetingMsg = _HEADER_ .. "\n\n" .. "Version " .. _VERSION_

local helpMsg = [[
Input:
-	Script is run inside a RailCOMPLETE model based on any DNA, running under RC 2026.1 or later.
-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row (the insertion point coords).
-	The Excel file may contain an optional column 'Z' providing insertion point elevation above mean sea level.
-	The Excel file may contain optional columns 'RcType' and 'Variant'. Otherwise, the template object's RC type
	and Variant will be applied.
-	Subsequent rows contain X and Y coordinates (and Z) in the 'X' and the 'Y' (and Z) columns.
-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
-	Close the Excel file before running the script.
-	Use Edit Script and enable the Log output window to see more info from the execution.
Usage:
-	Select Excel coordinate file, select an existing RC object as template (to get its RcType), start insertion.
-	If no Z column exists in Excel, then default formulas and values apply for the VerticalOffset property.

Output:
-	Fresh RC Objects are inserted on their default layer (as per current DNA) for each Excel file XY(Z) row.
-	Objects' Alignment property are set to the same as the template object's Alignment.
]]



---SCRIPT---
lib2.show(greetingMsg, _HEADER_)

local option
repeat
	option = askForKeyword(_SELECT_ACTION_MSG_, {_INSERT_OBJECTS_XY_, _INSERT_OBJECTS_XYZ_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)

	elseif option == _TERMINATE_ then
		lib2.show(_TERMINATED_MSG_, _HEADER_)

	elseif option == _INSERT_OBJECTS_XY_ or option == _INSERT_OBJECTS_XYZ_ then
		local xCaption = "X"
		local yCaption = "Y"
		local zCaption = "Z"
		local rctypeCaption = "RcType"
		local variantCaption = "Variant"

		-- Open Excel file:
		if option == _INSERT_OBJECTS_XY_ then
			lib2.show(_ASK_FOR_XY_EXCEL_FILE_MSG_, _HEADER_)
		else
			lib2.show(_ASK_FOR_XYZ_EXCEL_FILE_MSG_, _HEADER_)
		end
		local filename = askForFileName("Select Excel file")
		local file = getContentsFromFile(FileType.Excel, "", filename)
		local sheets = getExpandoObjectPropertyNames(file)
		local sheetName = sheets[0]
		local items = file[sheetName]
		local nItems = getCollectionLength(items)
		local nObjectsInserted = 0
		lib2.show(nItems .. " rows found in sheet '" .. sheetName .. "' in file:\n\n" .. filename, _HEADER_)

		local objTable = {}

		-- Select object type:
		lib2.show(_SELECT_TEMPLATE_OBJECT_MSG_, _HEADER_)
		local templateObject = askForObject(_SELECT_TEMPLATE_OBJECT_MSG_)

		if templateObject.Alignment then
			lib2.show(
				"Template object:\n" ..
				"\nIdentification = " .. RC__identify(templateObject) ..
				"\nRC type = " .. tostring(templateObject.RcType) ..
				"\nVariant = " .. tostring(templateObject.Variant) ..
				"\n" ..
				"\nAlignment = " .. tostring((templateObject.Alignment.name and templateObject.Alignment ~= "" and templateObject.Alignment.name) or templateObject.Alignment.code), _HEADER_)

			lib2.show("The insertion process will read rows with X and Y (and Z) coordinates from the Excel file " ..
				"and then insert objects similar to the template object found at each XY point.\n\n" ..
				"The inserted object's RC type will be same as the template's RC type if no column 'RcType' " ..
				"is given in the Excel file. If no column 'Variant' is given in the Excel file, then the " ..
				"template object's Variant will be applied.", _HEADER_)

			local obj

			beginUndoBufferItem()
			for i = 0, nItems - 1 do
				local item = items[i]
				local x = item[xCaption]
				local y = item[yCaption]
				local z = item[zCaption]
				local rctype = item[rctypeCaption]
				local variant = item[variantCaption]

				if (option == _INSERT_OBJECTS_XY_) and (type(x) ~= "number" or type(y) ~= "number") then
					write(string.format("%04d Skipping row: %s='%s' or %s='%s' is not a number.\n",
						i + 1, xCaption, tostring(x), yCaption, tostring(y)), _error)

				elseif (option == _INSERT_OBJECTS_XYZ_) and (type(x) ~= "number" or type(y) ~= "number" or type(z) ~= "number") then
					write(string.format("%04d Skipping row: %s='%s' or %s='%s' or %s='%s' is not a number.\n",
						i + 1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)), _error)

				else
					-- Valid coordinates found:
					-- Z is 0 or DNA-bound to its alignment's elevation:
					local targetPosition = getPoint3D(x, y)

					if type(rctype) == "string" and type(variant) == "string" then
						obj = insertPointObject(templateObject.Alignment, rctype, variant, targetPosition)
					elseif type(rctype) == "string" then
						obj = insertPointObject(templateObject.Alignment, rctype, targetPosition)
					else
						-- Use template object:
						obj = insertPointObject(templateObject.Alignment, templateObject.RcType, templateObject.Variant, targetPosition)
					end

					-- Adjust to target position after possible DNA Lua formula effects:
					obj.Mileage = "="
					obj.ReferenceMileage = "="
					obj.LateralOffset = "="
					obj.DistanceAlong = "="
					obj.DistanceToAlignment = "="
					obj.LongitudinalOffset = "="
					obj.VerticalOffset = "="

					local linearAddress = getLinearAddress(targetPosition, templateObject.Alignment)
					obj.LateralOffset = linearAddress.LateralOffset
					obj.DistanceAlong = linearAddress.DistanceAlong
					obj.LongitudinalOffset = linearAddress.LongitudinalOffset

					if option == _INSERT_OBJECTS_XY_ then
						-- Lock to vertical profile of alignment:
						obj.VerticalOffset = "=0"
					else
						targetPosition = getPoint3D(x, y, z)
						linearAddress = getLinearAddress(targetPosition, templateObject.Alignment)
						obj.VerticalOffset = linearAddress.VerticalOffset
						-- Note: Inside an UndoBuffer() loop, objects' VerticalOffset has not yet been written to the database
					end

					table.insert(objTable, obj)
					nObjectsInserted = nObjectsInserted + 1
					write(string.format("%04d %s (%s)   Rctype='%s'   Variant='%s'   inserted at %s\n",
						tostring(i), obj.name or obj.code or "", obj.id, obj.RcType, obj.Variant, lib1.p2s(targetPosition)))
				end
			end
			endUndoBufferItem()
			lib2.zoomExtents()
			setSelectionSet(objTable)
			lib2.show("\n" .. nObjectsInserted .. " objects were inserted.", _HEADER_)

		else
			-- templateObject.Alignment == nil:
			lib2.show("Template object '" .. RC__identify(templateObject) .. "' has no valid Alignment property - select another as template.", _HEADER_, _error)
		end

	else
		-- Provoke error and stop:
		lib2.stop(_BAD_SELECTION_MSG_ .. " [" .. option .. "].")
	end
until option == _TERMINATE_
lib2.show(_TERMINATED_MSG_, _HEADER_)
