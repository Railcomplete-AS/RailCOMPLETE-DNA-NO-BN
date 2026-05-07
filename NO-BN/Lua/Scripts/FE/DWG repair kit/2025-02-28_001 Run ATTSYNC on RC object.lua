--[[
	Run ATTSYNC on RC object
	========================
	Runs the AutoCAD ATTSYNC command on each block in a selected
	RailCOMPLETE point object and then regenerates the display.

	2025-02-28 v1.0 THBEN Created.
--]]



---SCRIPT---

local pointObject = askForPointObject("Select point object:")
if not pointObject then return end

-- Get block names from the point object:
local blockNames = table.select(pointObject:getBlockNames())

write(#blockNames .. " blocks found\n")

for k, blockName in pairs(blockNames) do
	runCommand("ATTSYNC n\n" .. blockName .. "\n")
	write("Attsync used on block " .. blockName .. "\n")
end

runCommand("REGEN\n")
