--[[
	init.lua
	
	RailCOMPLETE script to prepare for revised test after small DNA updates (no DNA mapping required).
	
	2021-10-04_000 CLFEY Created
	
	- Load DNA from default location
	- Replace DNA in drawing
	- Reset Lua formulas
	- Refresh all objects
	- Reload 2D symbol library, apply to objects
	- Refresh ribbon / object insertion menus
	- Purge all
	- Save to file
--]]
runCommand("_RC-AGENT-LoadDnaFromXml  ") --3 x ENTER: Accept warning / Use previous location & file, 
--runCommand("")
