function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

show([[
	Insert object at XY(Z) coordinates from Excel
	=============================================
	2024-12-05_000 CLFEY Created (from similar script inserting just a circle at XY).
	
	Input:
	-	Script is run insed a RailCOMPLETE model based on any DNA, running under RC 2024.2 or later.
	-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row (the insertion point coords).
	-	The Excel file may contain an additional column 'Z' providing insertion point elevation above mean sea level.
	-	Subsequent rows contain X and Y coordinates (and Z) in the 'X' and the 'Y' (and Z) columns.
	-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
	-	Close the Excel file before running the script.
	-	Use Edit Script and enable the Log output window to see more info from the execution.
	
	Usage:
	-	Select Excel coordinate file, select an existing RC object (to get its RcType), start the insertion.
	-	Any DNA-formula on VerticalOffset property will be replaced by the difference in elevation between the
		closest relevant alignment (railway tracks for most objects, contact wire for CW insulators etc) and then
		Z elevation from the Excel file (if Z is provided).
	-	If no 'Z' column exists in Excel, then default formulas and values apply for the VerticalOffset property.
	
	Output:
	-	Fresh RC Objects are inserted on their default layer (as per current DNA) for each Excel file XY(Z) row.
	]])

local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")

--[[Example output from RC-ShowVersion:
RailCOMPLETE® version 2024.2.1.0
DNA version information:
Name: NO-BN 2021.a (Patch 9 2024-10-11: Ny variant for KL 'Bjelkemast, forsterket' - HEM260 med 3D). Fjernet 'legacy' Pset data (unntatt legacy NOBN_com_Pset[...] funksjoner).
Version: 2021.a
Administration: Bane NOR SF
Agent: Railcomplete AS
Date: 2021-11-27T21:11:27+01:00
Description: RailCOMPLETE(r) Definisjon av nettverkselementer for Bane NOR
--]]

--Open Excel file:
local filename =  askForFileName("Select Excel file with XY coordinates columns with captions 'X', 'Y' (and optionally 'Z')")
local file = getContentsFromFile(FileType.Excel,"", filename)
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]
local items = file[sheetName]
local nItems = getCollectionLength(items)
local nObjectsCreated = 0
show(nItems.." rows found in sheet "..sheetName.." in file "..filename)

local objTable = {}
local alternativeInsertPointObjectOptionName

--Select object type:
local modelObject = askForObject("Select an existing object, we will use its RcType, Variant and its alignment's type when inserting new objects")

if modelObject.Alignment == nil then
	show("\nAborting: Selected model object must have a non-nil alignment (we need to see that alignment's RcType).")
else

	local xCaption = "X"
	local yCaption = "Y"
	local zCaption = "Z"
	local obj
	
	if rcVersion > "2024.2" then beginUndoBufferItem() end
	for i = 0,nItems-1 do
	    local item = items[i]
		local x = item[xCaption]
		local y = item[yCaption]
		local z = item[zCaption]
	
	    if type(x) == "number" and type(y) == "number" and (type(z) == "number" or type(z) == "nil") then --Trip on strings instead of a number (or no z value at all)
	
	    	if showPositionWithCircleOnCurrentLayer then
			    if rcVersion > "2024.2" then
			    	--This call ensures that undo buffering works (Ctrl+Z will undo all CIRCLEs in one operation):
		    		local insertionPoint = cadInterface.createCadEntity("Geometry.Point3d", {x, y, 0})
			    	local normalVector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
		    		local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {insertionPoint, normalVector, radius})
	    			cadInterface.addEntitiesToModelSpace({circle}) --Add graphics to drawing
		    	else
					runCommand("_CIRCLE "..x..","..y..(z and ", "..z or "").." "..radius.." ")
				end
		end			

			local p = getPoint3D(x,y)
	   		local bestGuessAlignment = table.firstOrNil(getNearbyAlignments(p, modelObject.Alignment.RcType))
	   		if bestGuessAlignment == nil then
	   			writeln("Skipping row "..tostring(i+1)..": Object cannot be inserted at ("..x..", "..y"..) because no alignment of type '"..modelObject.Alignment.RcType.."' was found.")
	   		else
	   			if modelObject.RcType:match("JBTSA_SIG Signal") then
					--Special case: Class B optical signals do not have a Variant at all (too many combinations exist):
					--(must be one of the <InsertPointOject> 'Name' options from DNA):
		   			obj = createPointObject(bestGuessAlignment, modelObject.RcType, "3-lys hovedsignal", p)
		   		else
		   			obj = createPointObject(bestGuessAlignment, modelObject.RcType, modelObject.Variant, p)
		   		end
			    if obj then
			    	--Adjust z if needed:
		    		if z then
			    		obj.VerticalOffset = "=" --remove formula, if existing
		    			obj.VerticalOffset = z
		    		end
	    			table.insert(objTable, obj)
					writeln(tostring(i+1)..": ("..x..", "..y..(z and ", "..z or "")..")")
					nObjectsCreated = nObjectsCreated + 1
		    	else
		    		writeln("Skipping row number "..tostring(i+1)..": Failed to create object at ("..tostring(x)..", "..tostring(y)..(z and ", "..tostring(z) or "")..").")
		    	end
		    end
		else
			if z then
				writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' or %s='%s' is not a number.", i+1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)))
			else
				writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' is not a number.", i+1, xCaption, tostring(x), yCaption, tostring(y)))
			end
		end
	end
	if rcVersion > "2024.2" then endUndoBufferItem() end
end
show("\n"..nObjectsCreated.." objects were inserted.")
setSelectionSet(objTable)
