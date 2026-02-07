--[[
	lib1.lua
	========
	Lua library functions callable from any Lua context - RC objects' properties and scripts.
	
	The base folder for includeLuaFile() calls is: "...\RC.bundle\Adm\XX-YY", where 'XX-YY'
	is your railway administration's DNA abbreviation.

	Usage: 
	
	--Include library functions for use in a Lua script or in a Lua function:
	lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
	return lib1.p2s(getPoint3D()) --Pretty-print X Y Z coordinates to  3 decimal places.

	2026-01-30 v1.0 CLFEY Created.
--]]

function p2s(p) return string.format("(%.03f, %.03f, %.03f)", p.X, p.Y, p.Z) end --Point-to-string conversion

function round(x) return string.format("%.03f",x) end

function splitString(s, splitChar)
	local splitTable = {}
	for splitValue in string.gmatch(s, "([^%"..splitChar.."]+)") do
		table.insert(splitTable, splitValue)
	end
	return splitTable
end
