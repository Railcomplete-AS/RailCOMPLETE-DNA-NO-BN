function writeln(t, symbol) write((t or "").."\n", symbol and symbol or _noSymbol) end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end
function parseStrictKOF(lines)
	local result = {}
	local enheter = {retning = 1, vinkler = 1, avstand = 1}
	
	for i, line in ipairs(lines) do
		writeln(line)
		if line:sub(1,1) ~= "-" then
			if line:sub(2,3) == "00" then
				writeln("Datablock 00, no data")
			elseif line:sub(2,3) == "01" then
				enheter.retning = tonumber(line:sub(45,45))			
				enheter.vinkler = tonumber(line:sub(46,46))
				enheter.avstand = tonumber(line:sub(47,47))
				writeln("Datablock 01, changed units to {retning: "..enheter.retning..", vinkler: "..enheter.vinkler..", avstand: "..enheter.avstand.."}")				
			elseif line:sub(2,3) == "05" then
				local record = {}
				record.key = line:sub(5, 14):gsub("%s+", "")
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
				table.insert(result, record)
				writeln("Data block 05, read data as { key: "..record.key..", X: "..record.X..", Y: "..record.Y..", Z: "..record.Z.."}")
			else
				writeln("Data block type "..line:sub(2,3).." is not implemented. Skipping line "..i, _warning)
			end
		else
			writeln("Comment line, no data")
		end
	end
	return result
end
function parseSimpleKOF(lines)
	local result = {}
	local enheter = {retning = 1, vinkler = 1, avstand = 1}
	
	for i, line in ipairs(lines) do
		writeln(line)

		if line:sub(1,1) ~= "-" then
			local fields = {}
			for field in line:gmatch("[^%s]+") do 
				table.insert(fields, field)
			end


			if fields[1] == "00" then
				writeln("Datablock 00, no data")
			elseif fields[1] == "01" then
				enheter.retning = tonumber(line:sub(45,45))			
				enheter.vinkler = tonumber(line:sub(46,46))
				enheter.avstand = tonumber(line:sub(47,47))
				writeln("Datablock 01, changed units to {retning: "..enheter.retning..", vinkler: "..enheter.vinkler..", avstand: "..enheter.avstand.."}")				
			elseif fields[1] == "05" then
				local record = {}
				record.key = fields[2]:gsub("%s+", "")
				local scale = 1
				if enheter.avstand == 2 then
					scale = 3.280839895 -- scale feet to meters
				end
				
				if enheter.retning == 1 then
					record.Y = tonumber(fields[4])*scale
					record.X = tonumber(fields[5])*scale
				elseif enheter.retning == 2 then
					record.X = tonumber(fields[4])*scale
					record.Y = tonumber(fields[5])*scale
				end			
				record.Z = tonumber(fields[6])*scale
				table.insert(result, record)
				writeln("Data block 05, read data as { key: "..record.key..", X: "..record.X..", Y: "..record.Y..", Z: "..record.Z.."}")
			else
				writeln("Data block type "..fields[1].." is not implemented. Skipping line "..i, _warning)
			end
		else
			writeln("Comment line, no data")
		end
	end
	return result
end


show([[
Adjust object at XY(Z) coordinates from Excel
=============================================
2025-02-04_000 KNHEL Created (from similar script inserting object at XYZ).

Input:
-	Script is run inside a RailCOMPLETE model based on any DNA, running under RC 2024.2 or later.
-	The source data can come from an Excel file a KOF 2.0 file or a 'simple KOF' file.
-	Excel file with captions 'X'=Easting and 'Y'=Northing coordinates in the top row (the insertion point coords).
-	The Excel file may contain an additional column 'Z' providing insertion point elevation above mean sea level.
-	The Excel file may contain a column which matches either 'name', 'code' or 'id' of the objects.
-	Subsequent rows contain X and Y coordinates (and Z) in the 'X' and the 'Y' (and Z) columns.
-	Close the Excel file before running the script.
-	For both KOF 2.0 or simple KOF only datablocks 01 and 05 are read.
-	For KOF 2.0 the 'Punkt' field is read as keydata
-	For Simple KOF the second non-whitespace block is read as keydata
-	For both KOF formats scale and northing easting configurations from 01 blocks are respected
-	Ensure your model represents the same coordinate system as your XY(Z) survey points are referencing.
-	Use Edit Script and enable the Log output window to see more info from the execution.

Usage:
-	Select file format and the coordinate file, the file is then read.
-	Select whether to find objects based on key or proximity and select either the RC property to match with or
	input the radius around	the coordinates to search for objects.
-	Select an existing RC object to get its RcType.
-	The script starts searching for objects and adjusting their position.
-	Any formula on Mileage, ReferenceMileage, DistanceAlong, DistanceToAlingment or LateralOffset will be
	replaced by the coordinate data.
-	Any formula on VerticalOffset property will be replaced by the difference in elevation between the object's
	own alignment (railway tracks for most objects, contact wire for CW insulators etc) and then Z elevation from 
	the coordinate file (if Z is provided).
-	If no 'Z' column exists in the coordinate file, then default formulas and values apply for the VerticalOffset property.

Output:
-	For each valid row in the coordinate file the best match RC object of the same RcType as the selected object
	is adjusted to the coordinates.
]])


local items

local fileFormat = askForKeyword("Select input file type", {"Excel", "KOF 2.0", "Simple KOF"})

if fileFormat == "Excel" then
	--Open Excel file:
	local filename =  askForFileName("Select Excel file with XY coordinates columns with captions 'X', 'Y' (and optionally 'Z')")
	local file = getContentsFromFile(FileType.Excel,"", filename)
	local sheets = getExpandoObjectPropertyNames(file)
	local sheetName = sheets[0]
	items = table.select(file[sheetName])
	show(#items.." rows found in sheet "..sheetName.." in Excel file "..filename)

else	
	--Open Kof file:
	local filename =  askForFileName("Select Kof coordinate file")
	
	local file = getContentsFromFile(FileType.Text,"", filename)
	
	lines = {}
	for s in file:gmatch("[^\r\n]+") do
	    table.insert(lines, s)
	end
	if fileFormat == "KOF 2.0" then
		items = parseStrictKOF(lines)
	elseif fileFormat == "Simple KOF" then
		items = parseSimpleKOF(lines)
	end
	show(#items.." rows read from file "..filename)
end

local selectionStrategy = askForKeyword("Select objects to update by key or by proximity?", {"Key", "Proximity"})

local keyColumn = "key"

if fileFormat == "Excel" and selectionStrategy == "Key" then
	keyColumn = askForKeyword("Select key column from Excel data", table.select(items[1], function(x) return x.Key end))
end

local keyProperty
if selectionStrategy == "Key" then
	keyProperty = askForKeyword("Select RC property to match with key", {"Name", "Code", "Id"})
end

local tolerance = 1
if selectionStrategy == "Proximity" then
	tolerance = askForDouble("Input the radius around data points to be searched for objects")
end



--Select object type:
local modelObject = askForObject("Select an existing object, we will adjust objects with its RcType")

local objectCollection
if selectionStrategy == "Key" then
	objectCollection = DocumentData.ObjectCollection:filter(function(o) return o.RcType == modelObject.RcType end)	
end

local objTable = {}

beginUndoBufferItem()

local xCaption = "X"
local yCaption = "Y"
local zCaption = "Z"

for i, item in ipairs(items) do
	local x = item[xCaption]
	local y = item[yCaption]
	local z = item[zCaption]

    if type(x) == "number" and type(y) == "number" and (type(z) == "number" or type(z) == "nil") then --Trip on strings instead of a number (or no z value at all)

		local p = getPoint3D(x, y, z)

		local obj 
		
		if selectionStrategy == "Proximity" then
			local nearbyObjects, numberOfObjects = getNearbyPointObjects2D(modelObject.RcType, p, tolerance)
			if numberOfObjects > 0 then
				obj = nearbyObjects[0]
			else
				writeln("Skipping row number "..tostring(i)..": No point object of type "..modelObject.RcType.." found within "..tostring(tolerance).." m of point ("..x..", "..y..").", _warning)
			end
		else
			local candidates
			if keyProperty == "Name" then
				candidates = objectCollection:filter(function(o) return item[keyColumn] == o.name end)
			elseif keyProperty == "Code" then
				candidates = objectCollection:filter(function(o) return item[keyColumn] == o.code end)
			else
				candidates = objectCollection:filter(function(o) return item[keyColumn] == o.id end)
			end
			if candidates.Count > 0 then
				obj = candidates[0]
			else
				writeln("Skipping row number "..tostring(i)..": No point object of type "..modelObject.RcType.." found with "..keyProperty.." "..tostring(item[keyColumn])..")", _warning)
			end
		
		end
		
		if obj then
			local newLinearAddress = getLinearAddress(p, obj.Alignment)
			
			--Delete any formulas that may stop us from moving object
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
		end
   
	else
		if z then
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' or %s='%s' is not a number.", i, xCaption, tostring(x), yCaption, tostring(y), zCaption, tostring(z)), _warning)
		else
			writeln(string.format("Skipping row number %d: %s='%s' or %s='%s' is not a number.", i, xCaption, tostring(x), yCaption, tostring(y)), _warning)
		end
	end
end

endUndoBufferItem()

show("\n"..#objTable.." objects were adjusted.")
setSelectionSet(objTable)

