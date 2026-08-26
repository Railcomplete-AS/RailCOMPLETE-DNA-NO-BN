local _HEADER_ = "Copy or move objects to neighbor track"
local _VERSION_ = "2026-07-18_000 CLFEY Created."

local _USAGE_ = [[
1) Click somewhere in modelspace to activate the script.
2) Select objects one by one, complete your selection by pressing ENTER.
3) Select a nearby alignment to which the objects shall be moved or copied.
4) Select side-of-track. The objects' distance to alignment will be preserved.

Relations internal to the selected group of objects will be preserved.

]].._VERSION_

local _OK_ = "OK"
local _COPY_ = "Copy object group"
local _MOVE_ = "Move object group"
local _TERMINATE_ = "Terminate"
local _ESC_ = nil --askForKeyword() returns nil when ESC is pressed or if the the user clicks the 'x' in upper right corner

local _RCTYPE_RAILWAY_TRACK_ = "JBTKO_SPO Spor"

local option
repeat
	option = askForKeyword(_USAGE_, {_COPY_, _MOVE_, _TERMINATE_}, _HEADER_)
until option == _COPY_ or option == _MOVE_ or option == _TERMINATE_ or option == _ESC_

if option == _TERMINATE_ or option == _ESC_ then
	write("Terminated.")
	return
end


local actionName = option == _COPY_ and "copied" or "moved" --Used in prompts


--Select object(s) to copy or move:
local objects = {}
local obj
local n = 0

local s
repeat
	local t
	local prompt = "Select object(s) to be "..actionName..", press ENTER (or space) to end selection"
	obj = askForPointObject(s and s.."\nN="..n.."\n"..prompt or prompt)
	if obj then
		t = RC__identify(obj)
		if objects[obj.id] == nil then
			--New object found, add to list
			objects[obj.id] = obj
			n = n + 1
			write(n..": "..t.."\n")
			s = s and s.."\n"..t or t
		else
			write(n..": "..t.." (already in selection)\n")
		end
	end
until obj == nil

--Select target alignment:
local trk
repeat
	trk = askForAlignment("Select a railway track alignment to which the "..actionName.." object(s) will be attached.")
	if trk.Rctype ~= _RCTYPE_RAILWAY_TRACK_ then 
		askForKeyword(RC__identify(trk).." has wrong Rctype [".._RCTYPE_RAILWAY_TRACK_.."].\n", {_OK_}, _HEADER_)
	else
		write(RC__identify(trk).."\n")
	end
until trk.Rctype == _RCTYPE_RAILWAY_TRACK_

local p = askForPoint("Pick side of track to place "..actionName.." objects")
local sign = getLinearAddress(p, trk).lateralOffset > 0 and 1 or -1

--[[
	_P: Use previous selection set
	_M: Mode, either Multiple (_M - keep asking for more) or single (_S).
	'(0 0 0) '(0 0 0) : From 0,0,0 to 0,0,0 (i.e. copy to same position as the source)
	_E: Exit, terminate the command
--]]
local source = {}
for k, v in pairs(objects) do table.insert(source, v) end
setSelectionSet(getCollectionFromTable(source))
if option == _COPY_ then 
	--Copy previous selection set to same position, while preserving relations within the selection set:
	runCommand("_COPY _P _M _S '(0 0 0) '(0 0 0) _E ")
end

for k, v in pairs(objects) do
	--Move items to new target alignment and side of alignment:
	if option == _COPY_ then
		--operate on the cloned item (search around original items for their clone)
		item = getNearbyPointObjects2D(v, v.RcType, 0.1):filter(function (x) return x.Variant == v.Variant and x.Dir == v.Dir end)[0]
	else
		--option == _MOVE_, operate on the item itself
		item = v
	end
	local lateralOffset = item.LateralOffset
	item.Alignment = trk.id --attach to target alignment - must assign using the ID (an illogical API...)
	if getLuaFormulaString(item, "DistanceToAlignment") then
		item.DistanceToAlignment = "=" --remove formula on DistanceToAlignment, if any
	end
	if getLuaFormulaString(item, "LateralOffset") then
		item.DistanceToAlignment = "=" --remove formula on DistanceToAlignment, if any
	end
	item.LateralOffset = sign * math.abs(lateralOffset)
end

write("Done.")

