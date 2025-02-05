function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

show([[
	Adjust object at XY(Z) coordinates from Kof
	=============================================
	2025-02-04_000 KNHEL Created (from similar script adjusting objects from Excel).
	
	Input:
	-	Script is run inside a RailCOMPLETE model based on any DNA, running under RC 2024.2 or later.
	-	Kof file with XY(Z) survey points.
	-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
	-	Use Edit Script and enable the Log output window to see more info from the execution.
	
	Usage:
	-	Select kof coordinate file, select an existing RC object (to get its RcType), input the radius around
		the coordinates to search for objects,  start the insertion.
	-	Any DNA-formula on Mileage, ReferenceMileage, DistanceAlong, DistanceToAlingment or LateralOffset will be
		replaced by the coordinate data.
	-	Any DNA-formula on VerticalOffset property will be replaced by the difference in elevation between the
		closest relevant alignment (railway tracks for most objects, contact wire for CW insulators etc) and then
		Z elevation from the Excel file (if Z is provided).
	-	If no 'Z' column exists in Excel, then default formulas and values apply for the VerticalOffset property.
	
	Output:
	-	For each valid row in the coordinate file the closest RC object of the same RcType as the selected object
		and is within the tolerance in 2D distance is adjusted to the coordinates.
	]])


--Open Kof file:
local filename =  askForFileName("Select Kof coordinate file")

local file = getContentsFromFile(FileType.Text,"", filename)

lines = {}
for s in file:gmatch("[^\r\n]+") do
    table.insert(lines, s)
end

local items = {}
local enheter = {retning = 1, vinkler = 1, avstand = 1}
for i, line in ipairs(lines) do
	if line:sub(1,1) ~= "-" then
		if line:sub(2,3) == "00" then
			--do nothing
		elseif line:sub(2,3) == "01" then
			enheter.retning = tonumber(line:sub(45,45))			
			enheter.vinkler = tonumber(line:sub(46,46))
			enheter.avstand = tonumber(line:sub(47,47))
		elseif line:sub(2,3) == "05" then
			local record = {}
			record.code = line:sub(5, 14):gsub("%s+", "")
			--record.info = line:sub(16,23):gsub("%s+", "")
			local scale = 1
			if enheter.avstand == 2 then
				scale = 3.280839895 -- scale feet to meters
			end
			
			if enheter.retning == 1 then
				record.Y = tonumber(line:sub(25, 36))*scale
				record.X = tonumber(line:sub(38, 48))*scale
			elseif enheter.retning == 2 then
				record.X = tonumber(line:sub(25, 36))*scale
				record.Y = tonumber(line:sub(38, 48))*scale
			end			
			record.Z = tonumber(line:sub(50, 57))*scale
			--record.bk = tonumber(line:sub(59, 60))
			--record.merknad = line:sub(62, 68):gsub("%s+", "")
			table.insert(items, record)
		else
			writeln("Data block type "..line:sub(2,3).." is not implemented. Skipping line "..i)
		end
	end
end

local objTable = {}

--Select object type:
local modelObject = askForObject("Select an existing object, we will adjust objects with its RcType")

local tolerance = askForDouble("Input the radius around data points to be searched for objects")


local xCaption = "X"
local yCaption = "Y"
local zCaption = "Z"
local obj

beginUndoBufferItem()

for i, item in ipairs(items) do
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
		writeln(tostring(i)..": Adjusted object with code "..obj.code.." and id "..obj.id.." to coordinates ("..x..", "..y..(z and ", "..z or "")..")")

			
		else
			writeln("Skipping row number "..tostring(i)..": No point object of type "..modelObject.RcType.." found within "..tostring(tolerance).." m of point ("..x..", "..y..").")
		end
   
	else
		if z then
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' or %s='%s' is not a number.", i, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)))
		else
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' is not a number.", i, xCaption, tostring(x), yCaption, tostring(y)))
		end
	end
end

endUndoBufferItem()

show("\n"..#objTable.." objects were adjusted.")
setSelectionSet(objTable)
