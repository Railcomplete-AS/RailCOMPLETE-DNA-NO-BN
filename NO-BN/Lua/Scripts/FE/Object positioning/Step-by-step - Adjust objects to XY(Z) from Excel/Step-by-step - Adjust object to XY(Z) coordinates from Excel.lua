--[[
	2025-01-30 v1.0 KNHEL Created (from similar script inserting object at XYZ).
	2026-02-07 v1.1 CLFEY Minor changes. To be introduced with DNA 2021.a patch 13.
--]]


--FUNCTIONS--
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua") --Common Lua functions
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua") --Scripting-only Lua functions


--CONSTANTS--
local _HEADER_ = "Adjust object at XY(Z) coordinates from Excel"
local _VERSION_ = "1.1"

local _ADJUST_OBJECTS_XY_ = "Adjust objects' XY values"
local _ADJUST_OBJECTS_XYZ_ = "Adjust objects' XYZ values"
local _TERMINATE_ = "Terminate"
local _HELP_ = "Help"

local usageMsg = [[Adjust linearly placed objects of a given type and variant based on survey data from Excel file.

Version ]].._VERSION_

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


--SCRIPT--
lib2.show(usageMsg, _HEADER_)

local option
repeat
	option = askForKeyword("Select action:", {_ADJUST_OBJECTS_XY_, _ADJUST_OBJECTS_XYZ_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)
		
	elseif option == _ADJUST_OBJECTS_XY_ or option == _ADJUST_OBJECTS_XYZ_ then
		--Open Excel file:
		if option == _ADJUST_OBJECTS_XY_ then
			lib2.show("Select Excel file where the first worksheet has column captions 'X' and 'Y' (a 'Z' column will be ignored).", _HEADER_)
		else
			lib2.show("Select Excel file where the first worksheet has column captions 'X' and 'Y' and 'Z'.", _HEADER_)
		end
		local filename =  askForFileName("Select Excel file")
		local file = getContentsFromFile(FileType.Excel,"", filename)
		local sheets = getExpandoObjectPropertyNames(file)
		local sheetName = sheets[0]
		local items = file[sheetName]
		local nItems = getCollectionLength(items)
		local nObjectsAdjusted = 0
		lib2.show(nItems.." rows found in sheet '"..sheetName.."' in file:\n\n"..filename, _HEADER_)
		
		local objTable = {}
		
		--Select object type:
		lib2.show("Select an existing object. Its RcType and Variant will be used as a template for selecting similar existing objects to adjust.", _HEADER_)
		local templateObject = askForObject("Select template object")
		
		local tolerance = askForDouble("Enter radius for how far around each survey point objects shall be searched for")

		lib2.show(
			"Template object:\n"..
			"\nIdentification = "..RC__identify(templateObject)..
			"\nRC type = "..tostring(templateObject.RcType)..
			"\nVariant = "..tostring(templateObject.Variant)..
			"\n"..
			"\nSearch radius = "..tostring(tolerance),
			_HEADER_)
		
		lib2.show("The adjustment process will read rows with X and Y (and Z) coordinates from the Excel file and then adjust all objects similar to the template object found within the given 2D radius of each such XY survey point.", _HEADER_)

		local xCaption = "X"
		local yCaption = "Y"
		local zCaption = "Z"
		local obj
		
		beginUndoBufferItem()
		for i = 0,nItems-1 do
		    local item = items[i]
			local x = item[xCaption]
			local y = item[yCaption]
			local z = item[zCaption]

		    if (option == _ADJUST_OBJECTS_XY_) and (type(x) ~= "number" or type(y) ~= "number") then
				write(string.format("%04d Skipping row: %s='%s' or %s='%s' is not a number.\n", i+1, xCaption, tostring(x), yCaption, tostring(y)), _error)

		    elseif (option == _ADJUST_OBJECTS_XYZ_) and (type(x) ~= "number" or type(y) ~= "number" or type(z) ~= "number") then
				write(string.format("%04d Skipping row: %s='%s' or %s='%s' or %s='%s' is not a number.\n", i+1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)), _error)
			
			else
				local targetPosition = getPoint3D(x, y)
				local nearbyObjects, nNearbyObjects = getNearbyPointObjects2D(templateObject.RcType, targetPosition, tolerance)
				if nNearbyObjects > 0 then
					obj = nearbyObjects[0] --Assume all objects have a non-nil Alignment.
					local initialPosition = getPoint3D(obj) --Has always X Y and Z values, possibly with formulas
					local newPosition
					if option == _ADJUST_OBJECTS_XY_ then
						newPosition = getPoint3D(x, y, initialPosition.Z)
					else
						--Include the Z coordinate
						newPosition = getPoint3D(x, y, z)
					end
					local newLinearAddress = getLinearAddress(newPosition, obj.Alignment)
					
					--Delete possible formulas that may stop us from moving the object
					
					obj.Mileage = "="
					obj.ReferenceMileage = "="
					obj.LateralOffset = "="
					obj.DistanceAlong = "="
					obj.DistanceToAlignment = "="
					obj.LongitudinalOffset = "="

					--Move object in the XY plane object at new linear address
					obj.DistanceAlong = newLinearAddress.DistanceAlong
					obj.LateralOffset = newLinearAddress.LateralOffset
					obj.LongitudinalOffset = newLinearAddress.LongitudinalOffset
		
					if option == _ADJUST_OBJECTS_XY_ then
						--Don't touch it Z coordinate value or possible formula on VerticalOffset
					else
						obj.VerticalOffset = "="
						obj.VerticalOffset = newLinearAddress.VerticalOffset
					end
		
					table.insert(objTable, obj)
					write(string.format("%04d %20s (%s) %s ==> %s\n",
						tostring(i+1), obj.name or obj.code or "", obj.id, lib1.p2s(initialPosition), lib1.p2s(newPosition)))
					nObjectsAdjusted = nObjectsAdjusted + 1
					
				else
					--No nearby objects (but ok XY or XYZ target coordinates)
					write(string.format("%04d No relevant objects found within %f m from target point (%.03f, %.03f)\n",
						tostring(i+1), tostring(tolerance), targetPosition.X, targetPosition.Y), _warning)
				end
			end
		end
		endUndoBufferItem()
		lib2.zoomExtents()
		setSelectionSet(objTable)
		lib2.show("\n"..nObjectsAdjusted.." objects were adjusted.", _HEADER_)

	elseif option == _TERMINATE_ then
		lib2.show("Terminated.", _HEADER_)

	else
		--Provoke error and stop
		lib2.stop("Bad option ["..option.."].")
	end
	
until option == _TERMINATE_ or option == nil

