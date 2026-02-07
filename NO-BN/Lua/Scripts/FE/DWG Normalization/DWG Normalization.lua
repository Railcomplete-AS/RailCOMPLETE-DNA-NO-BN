--[[

This script normalizes an AutoCAD DWG file to a consistent drafting and unit configuration. This includes:
- Disables object snaps and snap mode.
- Forces absolute coordinate input behavior.
- Disables grid and ortho mode.
- Sets drawing and insertion units to meters (no rescaling).
- Sets linear and angular units to decimal with 3-decimal precision.
- Adjusts selection behavior and pickbox size.
- Configures dynamic input and attribute prompting behavior.
- Zooms the active viewport to drawing extents.
- Purges the .DWG file

--]]

write("Choose the folder containing the 3D-geometries")
folder = askForFolderName("Choose the folder containing the 3D-geometries")
write("Folder: "..folder.."\n")
files = getFilesInFolder(folder, "*.dwg")
for i = 0, files.count -1 do
	file = files[i]:gsub("\\", "\\\\")
	write("File: "..file.."\n")
	
	runCommand('(vla-activate (vla-open (vla-get-documents (vlax-get-acad-object)) "'..file..'" :vlax-false)) ')
	
	runCommand('(command "._SNAP" 1.0 "._OSNAP" "OFF" "._OSNAPCOORD" 1 "._SNAPMODE" 0 "._GRID" "OFF") ')
	runCommand('(command "._UNITS" 2 3 1 3 0 "_NO" "._LUNITS" 2 "._LUPREC" 3 "._AUNITS" 0 "._AUPREC" 3 "._INSUNITS" 6 "._LIGHTINGUNITS" 2 ) ')
	runCommand('(command "._DIMZIN" 8 "._OSMODE" 0 "._COORDS" 1 "._PICKBOX" 5 "._DYNPICOORDS" 1 "._DYNPIFORMAT" 1 "._ORTHOMODE" 0 "._PICKFIRST" 1 "._PICKADD" 0 "._ATTREQ" 0 "._ATTDIA" 0 "._FILEDIA" 1 ) ')
	runCommand('(command "._ZOOM" "_EXTENTS") ')
	runCommand('(command "._PLINE" "0,0" "_WIDTH" 0 0 "") ')
	runCommand('(command "._PURGE" "ALL" "" "NO") ')
	
	
	
	write("Units changed..\n")
	
	-- save and close file
	runCommand("qsave ")
	runCommand("close ")
end

