--[[
	Patch Lua code in existing objects according to property deprecations with RC 2023.1.lua
	----------------------------------------------------------------------------------------
	2023-08-23_000	SINBRE/CLFEY	Created
	2023-08-23_001	THBEN			Optimized script by using tables instead of collections. 
									Added line "obj.lang = obj.lang" to force saving of objects.
	2023-08-24_002	SINBRE/THBEN	There was a problem with the use of the "Formula" variable in THBEN's version that was fixed.
									There was a problem with "Model3DName" being changed to "Name", but the DNA functions
									still have Model3DName in their name. The script has thus been changed so that everywhere it
									says "Model3DName" and then go through all places where it says "_Name(" and change to
									"_Model3DName(" to take care of the DNA functions 
	2023-08-28_003	CLFEY			Added user interaction dialogs, translated code into English, documented the code.
	
	Purpose
	-------
	Although RC 2023.1.0.x patches deprecated property names into new property names, it did not replace references to the deprecated
	properties in existing Lua code, already present as Object XML in RC obects in an existing RC model.
	
	This script uses a table with 'from' and 'to'. Amend this table to suit your own future needs. Take care to make a temporary
	name for identifiers that shall NOT be patched, see example below (first and last table row).
--]]

--Constants and functions
function writeln(t) write((t or "").."\n") end

local t = {
		--Substitution dictionary (a two-level table with named keys) 'from' ==> 'to'.
		--Substitution of type '0'/'2' are used to stash/restore items that shall be preserved. '1' is a substitution.
		{substitutionType = 0, from = "Model3DName%(",				to = "MMooddeellll33DDNNaammee("}, 
		{substitutionType = 1, from = "Model3DName",				to = "Name"},
		{substitutionType = 1, from = "Annotations3D",				to = "Annotations"}, 
		{substitutionType = 1, from = "Offset3D",					to = "Offset"}, 
		{substitutionType = 1, from = "Rotation3D",					to = "Rotation"}, 
		{substitutionType = 1, from = "PoleRouting3D",				to = "PoleRouting"},
		{substitutionType = 1, from = "SweepProfile",				to = "Name"},
		{substitutionType = 1, from = "Model3DSeparation",			to = "IteratedGeometry3D_1.Separation"},
		{substitutionType = 1, from = "Model3DTransformation",		to = "CoordinateSystemChange3D"},
		{substitutionType = 1, from = "Sweep3D",					to = "(SweptGeometry3D_0 and not SweptGeometry3D_0.Frozen)"},
		{substitutionType = 1, from = "Model3D_0",					to = "Geometry3D_0"},
		{substitutionType = 1, from = "Model3D_1",					to = "Geometry3D_1"},
		{substitutionType = 1, from = "Model3D_2",					to = "Geometry3D_2"},
		{substitutionType = 1, from = "Model3D_3",					to = "Geometry3D_3"},
		{substitutionType = 1, from = "Model3D_4",					to = "Geometry3D_4"},
		{substitutionType = 1, from = "Model3D_5",					to = "Geometry3D_5"},
		{substitutionType = 1, from = "Model3D_6",					to = "Geometry3D_6"},
		{substitutionType = 1, from = "Model3D_7",					to = "Geometry3D_7"},
		{substitutionType = 1, from = "Model3D_8",					to = "Geometry3D_8"},
		{substitutionType = 1, from = "Model3D_9",					to = "Geometry3D_9"},
		{substitutionType = 2, from = "MMooddeellll33DDNNaammee%(",	to = "Model3DName("}
	}

--Prolog
writeln("Patch Lua code in existing objects according to property deprecations with RC 2023.1")
writeln("----------------------------------------------------------------------------------------\n")
local objects = DocumentData.ObjectCollection:filter(function (x) return not x.isXref end)
local nObjects = getCollectionLength(objects)
writeln("Number of objects that will be analyzed, and patched if matches are found in Lua snippets: "..nObjects)
writeln()
local s = nil
local i,item
for i,item in pairs(t) do
	s = (s and s.."\n" or "Substitution list - Lua formulas in existing objects will be patched as follows:\n\n")..(item.from).." => "..(item.to)
end
s = s.."\n\nNote: Too see progress during execution, use _RC-EditScript and run the script with an open Log Window.\n\n"
writeln(s)
local choice = askForKeyword(s,{"Run","Cancel"},"Run")
if choice ~= "Run" then
	writeln("\n*** Cancelled by user ***\n")
	askForKeyword("Script was cancelled - no database changes have been applied.",{"OK"},"OK")
	goto quit
end
writeln()

--Main loop
beginUndoBufferItem()
local nObjectsPatched = 0
local nSubstitutionsMade = 0
for i=0, nObjects-1 do
	obj = objects[i]
	write(string.format("%5d   %-40s   %-12s   %-60s   ",i,obj.id,obj.RcType:sub(1,9),RC__identify(obj)))
	for j=0,getCollectionLength(obj.LuaExpressions)-1 do
		local property = obj.LuaExpressions[j].Name
		local patched = false
		local formula
		for k=1, #t do 
			formula = tostring(obj.LuaExpressions[j].Formula)
			if formula:match(t[k].from) then
				local substitutionType = t[k].substitutionType
				if substitutionType == 1 then
					write(property..": "..t[k].from.."=>"..t[k].to.."   ")
					nSubstitutionsMade = nSubstitutionsMade + 1
					patched = true
				else
					--substitutionType == 0 (stash) or 2 (restore)
					write(property..(substitutionType == 0 and "/stashed   " or "/restored   "))
				end
				obj.LuaExpressions[j].Formula = formula:gsub(t[k].from, t[k].to)
			end
		end
		formula = tostring(obj.LuaExpressions[j].Formula)
		if patched then
			nObjectsPatched = nObjectsPatched + 1
		end
	end
	writeln()
end
endUndoBufferItem()

--Epilog
msg = "\nDone.\n"..nObjectsPatched.." out of "..nObjects.." objects were patched.\n"..nSubstitutionsMade.." Lua formula substitutions were made.\n"
writeln(msg)
askForKeyword(msg,{"OK"},"OK")

::quit::
--End of Script--