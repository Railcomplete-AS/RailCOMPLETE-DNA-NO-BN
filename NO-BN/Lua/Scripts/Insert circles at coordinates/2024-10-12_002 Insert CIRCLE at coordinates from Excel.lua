--Insert CIRCLE at coordinates from Excel

function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

show([[
	Insert CIRCLE at coordinates from Excel
	=======================================
	2024-10-10_000 CLFEY Created.
	2024-10-11_001 KNHEL Added call to cadInterface.addEntitiesToModelSpace({circle}).
	2024-10-12_002 CLFEY Beauty fixes, 'local' declarations, tolerate non-number rows.
	
	Input:
	-	Set current layer using CLAYER command or other method.
	-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row.
	-	Subsequent rows contain X and Y coordinates in the 'X' and the 'Y' columns.
	-	Ensure your model represents the same coordinate system as your XY points.
	-	Close the Excel file before running the script.
	-	Use Edit Script and enable the Log output window to see more info from the execution.
	
	Output:
	-	CIRCLEs are created on the current layer for each XY pair.
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

local xCaption = "X"
local yCaption = "Y"
local radius = 1

local filename =  askForFileName("Select Excel file with XY coordinates columns with caption 'X' / 'Y'") 
local file = getContentsFromFile(FileType.Excel,"", filename)
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]
local items = file[sheetName]
local nItems = getCollectionLength(items)
local n = 0
show(nItems.." rows found in sheet "..sheetName.." in file "..filename)

if rcVersion > "2024.2" then beginUndoBufferItem() end
for i = 0,nItems-1 do
    local item = items[i]
	local x = item[xCaption]
	local y = item[yCaption]
    if type(x) == "number" and type(y) == "number" then
	    if rcVersion > "2024.2" then
		    --This call ensures that undo buffering works (Ctrl+Z will undo all CIRCLEs in one operation):
	    	local insertionPoint = cadInterface.createCadEntity("Geometry.Point3d", {x, y, 0})
	    	local normalVector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
	    	local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {insertionPoint, normalVector, radius})
	    	cadInterface.addEntitiesToModelSpace({circle}) --Add graphics to drawing
	    else
			runCommand("_CIRCLE "..x..","..y.." "..radius.." ")
	    end
		writeln(tostring(i+1)..": ("..x..", "..y..")")
		n = n + 1
	else
		writeln(tostring(i+1)..": Skipping row number "..i..", "..xCaption.."='"..x.."' or "..yCaption.."='"..y.."' is not a number.")
	end
end
if rcVersion > "2024.2" then endUndoBufferItem() end
show(n.." circles were inserted.")
