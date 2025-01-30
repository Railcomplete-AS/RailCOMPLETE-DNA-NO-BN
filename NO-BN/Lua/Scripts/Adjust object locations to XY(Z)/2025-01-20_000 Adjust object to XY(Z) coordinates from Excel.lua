function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

show([[
	Adjust object at XY(Z) coordinates from Excel
	=============================================
	2024-12-05_000 CLFEY Created (from similar script inserting just a circle at XY).
	2025-01-17_001 KNHEL Changed elevation to use elevation above mean sea level rather than relative to track
	
	Input:
	-	Script is run insed a RailCOMPLETE model based on any DNA, running under RC 2024.2 or later.
	-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row (the insertion point coords).
	-	The Excel file may contain an additional column 'Z' providing insertion point elevation above mean sea level.
	-	Subsequent rows contain X and Y coordinates (and Z) in the 'X' and the 'Y' (and Z) columns.
	-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
	-	Close the Excel file before running the script.
	-	Use Edit Script and enable the Log output window to see more info from the execution.
	
	Usage:
	-	Select Excel coordinate file, select an existing RC object (to get its RcType), input the radius around
		the coordinates to search for objects,  start the insertion.
	-	Any DNA-formula on VerticalOffset property will be replaced by the difference in elevation between the
		closest relevant alignment (railway tracks for most objects, contact wire for CW insulators etc) and then
		Z elevation from the Excel file (if Z is provided).
	-	If no 'Z' column exists in Excel, then default formulas and values apply for the VerticalOffset property.
	
	Output:
	-	For each valid row in the coordinate file the closest RC object of the same RcType as the selected object
		and is within the tolerance in 2D distance is adjusted to the coordinates.
	]])


--Open Excel file:
local filename =  askForFileName("Select Excel file with XY coordinates columns with captions 'X', 'Y' (and optionally 'Z')")
local file = getContentsFromFile(FileType.Excel,"", filename)
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]
local items = file[sheetName]
local nItems = getCollectionLength(items)
local nObjectsAdjusted = 0
show(nItems.." rows found in sheet "..sheetName.." in file "..filename)

local objTable = {}

--Select object type:
local modelObject = askForObject("Select an existing object, we will use its RcType, Variant and its alignment's type when inserting new objects")

local tolerance = askForDouble("Input the radius around data points to be searched for objects")


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

    if type(x) == "number" and type(y) == "number" and (type(z) == "number" or type(z) == "nil") then --Trip on strings instead of a number (or no z value at all)

		local p = getPoint3D(x, y, z)

		local nearbyObjects, numberOfObjects = getNearbyPointObjects2D(modelObject.RcType, p, tolerance)
		if numberOfObjects > 0 then
			local obj = nearbyObjects[0]
			local newLinearAddress = getLinearAddress(p, obj.Alignment)
			
			--Delete any formulas that may stop us from moving askForObject()
			obj.Mileage = "="
			obj.ReferenceMileage = "="
			obj.DistanceAlong = "="
			
			obj.DistanceToAlignment = "="
			obj.LateralOffset = "="
			
			--Place object at new linear address
			obj.DistanceAlong = newLinearAddress.DistanceAlong
			obj.LateralOffset = newLinearAddress.LateralOffset
			obj.LongitudinalOffset = newLinearAddress.LongitudinalOffset

    	--Adjust z if needed:
		if z then
			obj.VerticalOffset = "=" --remove formula, if existing
			obj.RelativeElevation = "="
			
			obj.VerticalOffset = newLinearAddress.VerticalOffset
		end

		table.insert(objTable, obj)
		writeln(tostring(i+1)..": ("..x..", "..y..(z and ", "..z or "")..")")
		nObjectsAdjusted = nObjectsAdjusted + 1
			
		else
			writeln("Skipping row number "..tostring(i+1)..": No point object of type "..modelObject.RcType.." found within "..tostring(tolerance).." m of point ("..x..", "..y..").")
		end
   
	else
		if z then
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' or %s='%s' is not a number.", i+1, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)))
		else
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' is not a number.", i+1, xCaption, tostring(x), yCaption, tostring(y)))
		end
	end
end

endUndoBufferItem()

show("\n"..nObjectsAdjusted.." objects were adjusted.")
setSelectionSet(objTable)
