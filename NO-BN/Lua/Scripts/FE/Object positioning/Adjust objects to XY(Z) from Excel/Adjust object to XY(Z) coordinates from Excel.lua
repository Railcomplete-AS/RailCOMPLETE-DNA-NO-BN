--[[
	Adjust object to XY(Z) coordinates from Excel
	===============================================
	Adjusts existing object positions to XY(Z) coordinates read from an Excel file.
	Objects are matched by proximity within a user-specified radius.

	2025-01-30 v1.0 KNHEL Created (from similar script inserting object at XYZ).
	2026-02-09 v1.1 CLFEY Minor changes. To be introduced with DNA 2021.a patch 13.
--]]



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _VERSION_ = "1.1"
local _HEADER_ = lib2.language(
	{EN="Adjust object positions to XY(Z) coordinates from Excel",
	NO="Juster objektposisjoner til XY(Z)-koordinater fra Excel",
	FR="Ajuster les positions des objets aux coordonnées XY(Z) à partir d'Excel",
	DE="Objektpositionen an XY(Z)-Koordinaten aus Excel anpassen"})

-- Commands:
local _ADJUST_OBJECTS_XY_ = lib2.language(
	{EN="Adjust objects' XY coordinates",
	NO="Juster objektposisjoner til XY-koordinater",
	FR="Ajuster les positions des objets aux coordonnées XY",
	DE="Objektpositionen an XY-Koordinaten anpassen"})
local _ADJUST_OBJECTS_XYZ_ = lib2.language(
	{EN="Adjust objects' XYZ coordinates",
	NO="Juster objektposisjoner til XYZ-koordinater",
	FR="Ajuster les positions des objets aux coordonnées XYZ",
	DE="Objektpositionen an XYZ-Koordinaten anpassen"})
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
	NO="Velg Excel-fil der det første regnearket har kolonneoverskriftene «X» og «Y» (en «Z»-kolonne vil bli ignorert)",
	FR="Sélectionnez le fichier Excel dont la première feuille de calcul comporte les en-têtes de colonne « X » et « Y » (la colonne « Z » sera ignorée)",
	DE="Wählen Sie eine Excel-Datei aus, in der das erste Arbeitsblatt die Spaltenüberschriften „X“ und „Y“ enthält (eine Spalte „Z“ wird ignoriert)"})

local _ASK_FOR_XYZ_EXCEL_FILE_MSG_ = lib2.language(
	{EN="Select Excel file where the first worksheet has column captions 'X' and 'Y' and 'Z'",
	NO="Velg Excel-fil der det første regnearket har kolonneoverskriftene «X», «Y» og «Z»",
	FR="Sélectionnez le fichier Excel dont la première feuille de calcul comporte les en-têtes de colonnes « X », « Y » et « Z »",
	DE="Wählen Sie eine Excel-Datei aus, in der das erste Arbeitsblatt die Spaltenüberschriften „X“, „Y“ und „Z“ enthält"})

local _SELECT_TEMPLATE_OBJECT_MSG_ = lib2.language(
	{EN="Select an existing object as template.\n\nIts RcType and Variant will be used as a template for inserting similar objects if no RcType and Variant is stated in the Excel file",
	NO="Velg et eksisterende objekt som mal.\n\nDets RcType og Variant vil bli brukt som mal for å sette inn lignende objekter hvis ingen RcType og Variant er angitt i Excel-filen.",
	FR="Sélectionnez un objet existant comme modèle. Son RcType et son Variant seront utilisés comme modèle pour insérer des objets similaires si aucun RcType et Variant n'est spécifié dans le fichier Excel",
	DE="Wählen Sie ein vorhandenes Objekt als Vorlage aus. Sein RcType und Variant werden als Vorlage für das Einfügen ähnlicher Objekte verwendet, wenn in der Excel-Datei kein RcType und Variant angegeben ist"})

local _SELECT_RADIUS_MSG_ = lib2.language(
	{EN="Enter radius for how far around each survey point objects shall be searched for",
	NO="Angi radius for hvor langt rundt hvert målepunkt objekter skal søkes etter",
	FR="Entrez le rayon pour déterminer la distance autour de chaque point d'enquête à laquelle les objets doivent être recherchés",
	DE="Geben Sie den Radius ein, in dem um jeden Vermessungspunkt herum nach Objekten gesucht werden soll"})

local _ADJUSTMENT_PROCESS_MSG_ = lib2.language(
	{EN="The adjustment process will read rows with X and Y (and Z) coordinates from the Excel file and then adjust all objects similar to the template object found within the given 2D radius of each such XY survey point.",
	NO="Justeringsprosessen vil lese rader med X- og Y- (og Z-) koordinater fra Excel-filen og deretter justere alle objekter som ligner på malobjektet som finnes innenfor den gitte 2D-radiusen for hvert slikt XY-målepunkt.",
	FR="Le processus d'ajustement lit les lignes contenant les coordonnées X et Y (et Z) du fichier Excel, puis ajuste tous les objets similaires à l'objet modèle trouvé dans le rayon 2D donné de chaque point d'enquête XY.",
	DE="Der Anpassungsprozess liest Zeilen mit X- und Y- (und Z-)Koordinaten aus der Excel-Datei und passt dann alle Objekte an, die dem Vorlagenobjekt ähneln, das sich innerhalb des angegebenen 2D-Radius jedes solchen XY-Vermessungspunkts befindet"})

local _BAD_SELECTION_MSG_ = lib2.language({EN="Invalid menu selection", NO="Ugyldig menyvalg", FR="Sélection de menu non valide", DE="Ungültige Menüauswahl"})
local _TERMINATED_MSG_ = lib2.language({EN="Terminated.", NO="Utført.", FR="Terminé.", DE="Beendet."})

local greetingMsg = _HEADER_ .. "\n\n" .. "Version " .. _VERSION_

local helpMsg = [[
Input:
-	Script is run inside a RailCOMPLETE model based on any DNA, running under RC 2024.2 or later.
-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row (the insertion point coords).
-	The Excel file may contain an additional column 'Z' providing insertion point elevation above mean sea level.
-	Subsequent rows contain X and Y coordinates (and Z) in the 'X' and the 'Y' (and Z) columns.
-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
-	Close the Excel file before running the script.
-	Use Edit Script and enable the Log output window to see more info from the execution.

Usage:
-	Select Excel coordinate file, select an existing RC object (to get its RcType), input the radius around
	the coordinates to search for objects, start the insertion.
-	Any formula on Mileage, ReferenceMileage, DistanceAlong, DistanceToAlingment or LateralOffset will be
	replaced by the coordinate data.
-	Any formula on VerticalOffset property will be replaced by the value corresponding to the new elevation
	Z read from Excel file.
-	If adjustment without Z coordinate is selected, then existing formulas and values on the object's vertical
	offset will not be modified.

Output:
-	For each valid row in the coordinate file the closest RC object of the same RcType and variant as the selected
	template object and being within the tolerance in 2D distance is adjusted to the target XY or XYZ coordinates.

Note:
-	Only point objects that posess a linear position (i.e., has a non-nil 'Alignment' property) can be adjusted.
]]



---SCRIPT---
lib2.show(greetingMsg, _HEADER_)

local option
repeat
	option = askForKeyword(_SELECT_ACTION_MSG_, {_ADJUST_OBJECTS_XY_, _ADJUST_OBJECTS_XYZ_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)

	elseif option == _TERMINATE_ then
		lib2.show(_TERMINATED_MSG_, _HEADER_)

	elseif option == _ADJUST_OBJECTS_XY_ or option == _ADJUST_OBJECTS_XYZ_ then
		-- Open Excel file:
		if option == _ADJUST_OBJECTS_XY_ then
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
		local nObjectsAdjusted = 0
		lib2.show(nItems .. " rows found in sheet '" .. sheetName .. "' in file:\n\n" .. filename, _HEADER_)

		local objTable = {}

		-- Select object type:
		lib2.show(_SELECT_TEMPLATE_OBJECT_MSG_, _HEADER_)
		local templateObject = askForObject(_SELECT_TEMPLATE_OBJECT_MSG_)

		local tolerance = askForDouble(_SELECT_RADIUS_MSG_)

		lib2.show(
			"Template object:\n" ..
			"\nIdentification = " .. RC__identify(templateObject) ..
			"\nRC type = " .. tostring(templateObject.RcType) ..
			"\nVariant = " .. tostring(templateObject.Variant) ..
			"\n" ..
			"\nSearch radius = " .. tostring(tolerance),
			_HEADER_)

		lib2.show(_ADJUSTMENT_PROCESS_MSG_, _HEADER_)

		local xCaption = "X"
		local yCaption = "Y"
		local zCaption = "Z"
		local obj

		beginUndoBufferItem()
		for i = 0, nItems - 1 do
			local item = items[i]
			local x = item[xCaption]
			local y = item[yCaption]
			local z = item[zCaption]

			if (option == _ADJUST_OBJECTS_XY_) and (type(x) ~= "number" or type(y) ~= "number") then
				write(string.format("%04d Skipping row: %s='%s' or %s='%s' is not a number.\n",
					i + 1, xCaption, tostring(x), yCaption, tostring(y)), _error)

			elseif (option == _ADJUST_OBJECTS_XYZ_) and (type(x) ~= "number" or type(y) ~= "number" or type(z) ~= "number") then
				write(string.format("%04d Skipping row: %s='%s' or %s='%s' or %s='%s' is not a number.\n",
					i + 1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)), _error)

			else
				local targetPosition = getPoint3D(x, y)
				local nearbyObjects, nNearbyObjects = getNearbyPointObjects2D(templateObject.RcType, targetPosition, tolerance)
				if nNearbyObjects > 0 then
					-- Assume all objects have a non-nil Alignment:
					obj = nearbyObjects[0]
					local initialPosition = getPoint3D(obj)
					local newPosition
					if option == _ADJUST_OBJECTS_XY_ then
						newPosition = getPoint3D(x, y, initialPosition.Z)
					else
						-- Include the Z coordinate:
						newPosition = getPoint3D(x, y, z)
					end
					local newLinearAddress = getLinearAddress(newPosition, obj.Alignment)

					-- Delete possible formulas that may stop us from moving the object:
					obj.Mileage = "="
					obj.ReferenceMileage = "="
					obj.LateralOffset = "="
					obj.DistanceAlong = "="
					obj.DistanceToAlignment = "="
					obj.LongitudinalOffset = "="

					-- Move object in the XY plane at new linear address:
					obj.DistanceAlong = newLinearAddress.DistanceAlong
					obj.LateralOffset = newLinearAddress.LateralOffset
					obj.LongitudinalOffset = newLinearAddress.LongitudinalOffset

					if option == _ADJUST_OBJECTS_XY_ then
						-- Don't touch the Z coordinate value or possible formula on VerticalOffset
					else
						obj.VerticalOffset = "="
						obj.VerticalOffset = newLinearAddress.VerticalOffset
					end

					table.insert(objTable, obj)
					write(string.format("%04d %20s (%s) %s ==> %s\n",
						tostring(i + 1), obj.name or obj.code or "", obj.id, lib1.p2s(initialPosition), lib1.p2s(newPosition)))
					nObjectsAdjusted = nObjectsAdjusted + 1

				else
					-- No nearby objects (but ok XY or XYZ target coordinates):
					write(string.format("%04d No relevant objects found within %f m from target point (%.03f, %.03f)\n",
						tostring(i + 1), tostring(tolerance), targetPosition.X, targetPosition.Y), _warning)
				end
			end
		end
		endUndoBufferItem()
		lib2.zoomExtents()
		setSelectionSet(objTable)
		lib2.show("\n" .. nObjectsAdjusted .. " objects were adjusted.", _HEADER_)

	else
		-- Provoke error and stop:
		lib2.stop(_BAD_SELECTION_MSG_ .. " [" .. option .. "].")
	end
until option == _TERMINATE_
lib2.show(_TERMINATED_MSG_, _HEADER_)
