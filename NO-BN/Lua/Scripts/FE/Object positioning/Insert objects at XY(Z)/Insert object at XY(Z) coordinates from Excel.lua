--[[
	2024-12-05 v1.0 CLFEY Created (from similar script inserting just a circle at XY).
	2025-01-17 v2.0 KNHEL Changed elevation to use elevation above mean sea level rather than relative to track
	2026-02-08 v2.1 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
]]

--FUNCTIONS--
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua") --Common Lua functions
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua") --Scripting-only Lua functions


--CONSTANTS--
local _HEADER_ = "Insert object at XY(Z) coordinates from Excel"
local _VERSION_ = "2.1"

local _INSERT_OBJECTS_XY_ = "Insert objects at XY values (locked to its alignment's local elevation)"
local _INSERT_OBJECTS_XYZ_ = "Insert objects' XYZ values (removing possible DNA formula on the VerticalOffset property)"
local _TERMINATE_ = "Terminate"
local _HELP_ = "Help"
local _SELECT_ACTION_ = "Select action:\n\nClose the Excel file before continuing.\nOpen your scripting log window before running this script to keep track of progress."

local xCaption = "X"
local yCaption = "Y"
local zCaption = "Z"
local rctypeCaption = "RcType"
local variantCaption = "Variant"


local usageMsg = [[
Insert object at XY(Z) coordinates from Excel.

Version ]].._VERSION_

local helpMsg = [[
Input:
-	Script is run insed a RailCOMPLETE model based on any DNA, running under RC 2026.1 or later.
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


--SCRIPT--
lib2.show(usageMsg, _HEADER_)

local option
repeat
	option = askForKeyword(_SELECT_ACTION_, {_INSERT_OBJECTS_XY_, _INSERT_OBJECTS_XYZ_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)
		
	elseif option == _INSERT_OBJECTS_XY_ or option == _INSERT_OBJECTS_XYZ_ then
		--Open Excel file:
		if option == _INSERT_OBJECTS_XY_ then
			lib2.show("Select Excel file where the first worksheet has column captions 'X' and 'Y' (a 'Z' column will be ignored).", _HEADER_)
		else
			--_INSERT_OBJECTS_XYZ_
			lib2.show("Select Excel file where the first worksheet has column captions 'X' and 'Y' and 'Z'.", _HEADER_)
		end
		local filename =  askForFileName("Select Excel file")
		local file = getContentsFromFile(FileType.Excel,"", filename)
		local sheets = getExpandoObjectPropertyNames(file)
		local sheetName = sheets[0]
		local items = file[sheetName]
		local nItems = getCollectionLength(items)
		local nObjectsInserted = 0
		lib2.show(nItems.." rows found in sheet '"..sheetName.."' in file:\n\n"..filename, _HEADER_)

		local objTable = {}

		--Select object type:
		lib2.show("Select an existing object as template.\n\nIts RcType and Variant will be used as a template for inserting similar objects if no RcType and Variant is stated in the Excel file.", _HEADER_)
		local templateObject = askForObject("Select template object")
		
		if templateObject.Alignment then
			lib2.show(
				"Template object:\n"..
				"\nIdentification = "..RC__identify(templateObject)..
				"\nRC type = "..tostring(templateObject.RcType)..
				"\nVariant = "..tostring(templateObject.Variant)..
				"\n"..
				"\nAlignment = "..tostring((templateObject.Alignment.name and templateObject.Alignment ~= "" and templateObject.Alignment.name) or templateObject.Alignment.code), _HEADER_)
			
			lib2.show("The insertion process will read rows with X and Y (and Z) coordinates from the Excel file and then insert objects similar to the template object found at each XY point.\n\nThe inserted object's RC type will be same as the template's RC type if no column 'RcType' is given in the Excel file. If no column 'Variant' is given in the Excel file, then the template object's Variant will be applied.", _HEADER_)

			local obj
			
			beginUndoBufferItem()
			for i = 0, nItems-1 do
			    local item = items[i]
				local x = item[xCaption]
				local y = item[yCaption]
				local z = item[zCaption]
				local rctype = item[rctypeCaption]
				local variant = item[variantCaption]
	
			    if (option == _INSERT_OBJECTS_XY_) and (type(x) ~= "number" or type(y) ~= "number") then
					write(string.format("%04d Skipping row: %s='%s' or %s='%s' is not a number.\n", i+1, xCaption, tostring(x), yCaption, tostring(y)), _error)
	
			    elseif (option == _INSERT_OBJECTS_XYZ_) and (type(x) ~= "number" or type(y) ~= "number" or type(z) ~= "number") then
					write(string.format("%04d Skipping row: %s='%s' or %s='%s' or %s='%s' is not a number.\n", i+1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)), _error)
				
				else
					--Valid coordinates found
					local targetPosition = getPoint3D(x, y) --Z is 0 or DNA-bound to its alignment's elevation

					if type(rctype) == "string" and type(variant) == "string" then
						obj = insertPointObject(templateObject.Alignment, rctype, variant, targetPosition)
					elseif type(rctype) == "string" then
						obj = insertPointObject(templateObject.Alignment, rctype, targetPosition)
					else
						--Use template object
						obj = insertPointObject(templateObject.Alignment, templateObject.RcType, templateObject.Variant, targetPosition)
					end

					--Adjust to target position after possible DNA Lua formula effects:
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
						obj.VerticalOffset = "=0" --Lock to vertical profile of Alignment
					else
						--_INSERT_OBJECTS_XYZ_
						targetPosition = getPoint3D(x, y, z)
						linearAddress = getLinearAddress(targetPosition, templateObject.Alignment)
						obj.VerticalOffset = linearAddress.VerticalOffset
						--Note: Inside an UndoBuffer() loop, objects' VerticalOffset has not yet been written to the database.
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
			lib2.show("\n"..nObjectsInserted.." objects were inserted.", _HEADER_)

		else
			--templateObject.Alignment == nil
			lib2.show("Template object '"..RC__identify(templateObject).."' has no valid Alignment property - select another as template.", _HEADER_, _error)
		end

	elseif option == _TERMINATE_ then
		lib2.show("Terminated.", _HEADER_)

	else
		--Provoke error and stop
		lib2.stop("Bad option ["..option.."].")
	end
	
until option == _TERMINATE_ or option == nil
