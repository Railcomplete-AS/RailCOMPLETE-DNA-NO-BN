--=========================================================================================================================
--
-- InsertAllPointObjectsFromDna.lua
-- 
-- Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
-- RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
--
-- Change log:
-- 2021-03-05_000 THBEN Created
-- 2021-03-06_001 CLFEY Renamed identifiers, added guidelines, renamed Lua file, moved to github, added Contact Wire alignment.
--
--=========================================================================================================================

write("================== Insert all objects from DNA =====================\n")
write("Note: Start a fresh drawing with the DNA i question before running this script.\n")
runCommand('_.ERASE _All \n_No ')
runCommand('_.-PURGE _All \n_No ')
--fileSpec = "*.xml"
fileSpec = "C:\\Users\\Claus Feyling\\Documents\\GitHub\\RailCOMPLETE-NO-BN\\NO-BN\\DNA\\NO-BN-2021.a-DNA.xml"
dna = getContentsFromFile(FileType.Xml, "Select DNA file",fileSpec)

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
table.sort(keys)
_all_ = "All"
_quit_ = "_Quit"
table.insert(keys, 1, _all_)
table.insert(keys, 2, _quit_)

maxLen = 10000
geoEdge = createLineSegment(getPoint3D(0,0), getPoint3D(maxLen,0))
topoEdge = createHorizontalProfile({geoEdge})
track1 = createAlignmentObject("KO-SPO Spor", "Sviller og skinner", topoEdge)
track1.code = "T-1"

cwGeoEdges = {}
step = 50
N = maxLen / step
zigZag = 1.50 --exaggerated
for i = 0,N-1 do 
	ge = createLineSegment( getPoint3D(step*i,zigZag*(-1)^i), getPoint3D(step*(i+1),zigZag*(-1)^(i+1)) ) 
	table.insert(cwGeoEdges,ge)
end
for i = 0,getCollectionLength(cwGeoEdges)-1 do write(cwGeoEdges[i].id.."\n") end
cwTopoEdge = createHorizontalProfile(getCollectionFromTable(cwGeoEdges))
contactWire = createAlignmentObject("EH-KTL Kontaktledning", "Kontaktledning, Sort, ikke kjørbar", cwTopoEdge)
contactWire.code = "CW-1"

DocumentData.SymbolMode = "Geographic"
pos = 10.0
while true do
	selectedType = askForKeyword("Select RC object type", keys)
	write("\n"..selectedType.."\n")
	if selectedType == _quit_ then 
		write("\n================== Done =====================\n")
		return
	elseif selectedType == _all_ then
		for k, rcType in pairs(keys) do
			write("RC type : ".. rcType.."\n")
			
			for ik, variant in pairs(placedObjects[rcType]) do
				write("\tVariant : ".. variant.."\n")
				obj = createPointObject(alignment, rcType, variant, pos, 5.0, true)
				pos = pos + 30 
			end
		end
	else
		write("RC type ------ ".. selectedType.."\n")
		for ik, variant in pairs(placedObjects[selectedType]) do
			write("		Variant ------ ".. variant.."\n")
			obj = createPointObject(track1, selectedType, variant, pos, 5.0, true)
			pos = pos + 30 
		end
	end
end
