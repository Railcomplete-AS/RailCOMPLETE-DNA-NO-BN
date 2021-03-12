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
runCommand('erase all \n')
runCommand('-purge a \nn ')

--fileSpec = "*.xml"
fileSpec = "C:\\Users\\Norctest\\Documents\\GitHub\\RailCOMPLETE-NO-BN\\NO-BN\\DNA\\NO-BN-2021.a-DNA.xml"
dna = getContentsFromFile(FileType.Xml,"Choose file", fileSpec)

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

drawTails = table.where(
					dna.RailwayObjectTypeDefinitions.ObjectType, 
					function(x) return x.Class == "RailwayPlacedObject" and x.DrawTail ~= nil and string.match(x.DrawTail, "true") end, function (x) return x.Name end)

maxLen = 100000
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
cwTopoEdge = createHorizontalProfile(cwGeoEdges)
contactWire = createAlignmentObject("EH-KTL Kontaktledning", "Kontaktledning, Sort, ikke kjørbar", cwTopoEdge)
contactWire.code = "CW-1"

DocumentData.SymbolMode = "Geographic"
useJig = askForKeyword("Use jig command?", {"Yes", "No"})
pos = 10.0
--if useJig ~= "Yes" then
--	beginUndoBufferItem()
--end
while true do
	selectedType = askForKeyword("Select RC object type", keys)
	write("\n"..selectedType.."\n")
	if selectedType == _quit_ then 
		write("\n================== Done =====================\n")
		return
	elseif selectedType == _all_ then
		for k, rcType in pairs(keys) do
			if rcType == _all_ or rcType == _quit_ then goto continue end
			
			write("RC type : ".. rcType.."\n")
			
			for ik, variant in pairs(placedObjects[rcType]) do
				write("\tVariant : ".. variant.."\n")
				if useJig == "Yes" then
					drawTail = drawTails[selectedType] ~= nil
					tailPoints = ""..(pos+3)..",13"
					runCommand('RC-CREATEPOINTOBJECT "'..rcType..'" "'..variant..'" '..pos..',10 '..(drawTail and tailPoints or "")..' \x03\x03')
				else
					o = createPointObject(track1, rcType, variant, pos, 5.0, true)
				end
				pos = pos + 30 
			end
			::continue::
		end
	else
		write("RC type ------ ".. selectedType.."\n")
		for ik, variant in pairs(placedObjects[selectedType]) do
			write("		Variant ------ ".. variant.."\n")
			if useJig == "Yes" then
				drawTail = drawTails[selectedType] ~= nil
				tailPoints = ""..(pos+3)..",13"
				runCommand('RC-CREATEPOINTOBJECT "'..selectedType..'" "'..variant..'" '..pos..',10 '..(drawTail and tailPoints or "")..' \x03\x03')
			else
				obj = createPointObject(track1, selectedType, variant, pos, 5.0, true)
			end
			pos = pos + 30 
		end
	end
end
--if useJig ~= "Yes" then
--	endUndoBufferItem()
--end
