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

	2026-02-09 v1.0 CLFEY Created.
--]]

--Point-to-string conversion. Call as lib1.p2s(p) where p is a 3D point (such as a getPoint3D()).
function p2s(p)
	return
string.format("(%.03f, %.03f, %.03f)", p.X, p.Y, p.Z) end

--Returns a string containing the input number 'x' rounded to three decimal places. Call as lib1.round(math.pi) which returns '3.142'.
function round(x)
	return string.format("%.03f",x)
end

--Returns a table with partial strings extracted from the input string, split at the given split character. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
function splitString(s, splitChar)
	local splitTable = {}
	for splitValue in string.gmatch(s, "([^%"..splitChar.."]+)") do
		table.insert(splitTable, splitValue)
	end
	return splitTable
end
