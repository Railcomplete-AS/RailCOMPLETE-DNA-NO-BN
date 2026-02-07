--[[
Run AutoCAD ATTSYNC command on selected RailCOMPLETE point object

=========================================================================
2025-02-28_001 THBEN Created script


Input:
-	RC point object.

Output:
-	Prints the total number of blocks located in the selected point object.
-	Runs the ATTSYNC command and prints the block name for each block in the RC object.
]]
	
local pointObject = askForPointObject("Select point object:")

if not pointObject then
	goto endOfScript
end

-- Create table of the point object block names
local blockNames = table.select(pointObject:getBlockNames())


write(#blockNames.." blocks found\n")

for k, blockName in pairs(blockNames) do
	runCommand("ATTSYNC n\n"..blockName.."\n")	
	write("Attsync used on block "..blockName.."\n")
end

runCommand("REGEN\n")

::endOfScript::