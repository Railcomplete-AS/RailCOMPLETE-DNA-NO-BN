--[[
	2024-10-10 v1.0 CLFEY Created.
	2024-10-11 v1.1 KNHEL Added call to cadInterface.addEntitiesToModelSpace({circle}).
	2024-10-12 v1.2 CLFEY Beauty fixes, 'local' declarations, tolerate non-number rows.
	2026-02-08 v1.3 CLFEY Added windows and usage msg etc, using lib1 and lib2 functions.
--]]


--FUNCTIONS--
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua") --Common Lua functions
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua") --Scripting-only Lua functions

local language = lib2._DNA_COUNTRY_

--CONSTANTS--
local _HEADER_ = "Insert CIRCLE at coordinates from Excel"
local _VERSION_ = "1.3"

local _CIRCLES_ = "Insert circles at 2D points given from Excel file"
local _HELP_ = "Help"
local _TERMINATE_ = "Terminate"

local usageMsg = [[
Insert circles at 2D points given from Excel file.

Open your scripting log window before running this script to keep track of progress.

Version ]].._VERSION_

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
lib2.show(usageMsg, _HEADER_)

local option
repeat
	option = askForKeyword("Select action:", {_CIRCLES_, _HELP_, _TERMINATE_}, _HEADER_)

	if option == _HELP_ then
		lib2.show(helpMsg, _HEADER_)
		
	elseif option == _CIRCLES_ then
		local xCaption = "X"
		local yCaption = "Y"
		local radius = 1
		
		lib2.show("Select Excel file with XY coordinates columns with caption 'X' and 'Y'.", _HEADER_)
		local filename =  askForFileName("Select Excel file with XY coordinates columns with caption 'X' and 'Y'") 
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
				write(string.format("%04d Skipping row since %s=%s or %s=%s is not a number\n", i,  xCaption, x, yCaption, y))
			end
		end
		endUndoBufferItem()
		lib2.show(nTreated.." circles were inserted.", _HEADER_)
		lib2.zoomExtents()
		
	elseif option == _TERMINATE_ then
		--Fall through
		
	else
		--Provoke error and stop
		lib2.stop("Bad option ["..option.."].")
	end
until option == _TERMINATE_
lib2.show("Terminated.", _HEADER_)
