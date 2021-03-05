--=========================================================================================================================
--
-- InsertAllPointObjectsFromDna.lua
-- 
-- Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
-- RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
--
-- Change log:
-- 2021-03-05_000 THBEN Created
-- 2021-03-05_001 CLFEY Renamed identifiers, added guidelines, renamed Lua file, moved to github.
--
--=========================================================================================================================

write("================== Insert all objects from DNA =====================\n")
write("Note: Start a fresh drawing with the DNA i question before running this script.\n")
runCommand('_.ERASE _All \n_No ')
runCommand('_.-PURGE _All \n_No ')
dna = getContentsFromFile(FileType.Xml, "Select DNA file","*.xml")
placedObjects = table.where(
					dna.RailwayObjectTypeDefinitions.ObjectType, 
					function(x) return x.Class == "RailwayPlacedObject" end, function (x) return x.Name end
				):select(
					function (x) 
						if getCollectionLength(x.InsertPointObject) > 0 then 
							return table.select(x.InsertPointObject, function(y) return y.DisplayName or y.VariantName end) 
						else 
							return {x.InsertPointObject.DisplayName or x.InsertPointObject.VariantName} 
						end
					end
				)
keys = {}
for k,v in pairs(placedObjects) do table.insert(keys, k) end
table.insert(keys, "All")
table.sort(keys)

segment = createLineSegment(getPoint3D(0,0), getPoint3D(10000,0))
profile = createHorizontalProfile({segment})
alignment = createAlignmentObject("KO-SPO Spor", "Sviller og skinner", profile)
alignment.code = "Det lange spor"
pos = 10.0

selectedType = askForKeyword("Select RC object type", keys)
write("\n"..selectedType.."\n")
if selectedType ~= "All" then
	write("RC type ------ ".. selectedType.."\n")
	for ik, variant in pairs(placedObjects[selectedType]) do
		write("		Variant ------ ".. variant.."\n")
		obj = createPointObject(alignment, selectedType, variant, pos, 5.0, true)
		pos = pos + 30 
	end
else
	for k, rcType in pairs(keys) do
		write("RC type : ".. rcType.."\n")
		
		for ik, variant in pairs(placedObjects[rcType]) do
			write("\tVariant : ".. variant.."\n")
			obj = createPointObject(alignment, rcType, variant, pos, 5.0, true)
			pos = pos + 30 
		end
	end
end
