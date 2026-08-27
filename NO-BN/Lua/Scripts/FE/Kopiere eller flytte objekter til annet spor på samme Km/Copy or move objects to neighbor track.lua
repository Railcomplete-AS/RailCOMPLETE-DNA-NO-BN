local _HEADER_ = "Copy or move objects to neighbor track"
local _VERSION_ = "2026-08-27_001 CLFEY Preserve Km via the reference line, and verify all target positions before copying."

local _USAGE_ = [[
1) Click somewhere in modelspace to activate the script.
2) Select objects one by one, complete your selection by pressing ENTER.
3) Select a nearby alignment to which the objects shall be moved or copied.
4) Select side-of-track. The objects' distance to alignment will be preserved.

The objects keep their Km, i.e. their position along the reference (main) line.
Relations internal to the selected group of objects will be preserved.

]].._VERSION_

local _OK_ = "OK"
local _COPY_ = "Copy object group"
local _MOVE_ = "Move object group"
local _COPY_THE_REST_ = "Copy the remaining objects"
local _MOVE_THE_REST_ = "Move the remaining objects"
local _CANCEL_ = "Cancel - change nothing"
local _TERMINATE_ = "Terminate"
local _ESC_ = nil --askForKeyword() returns nil when ESC is pressed or if the the user clicks the 'x' in upper right corner

local _RCTYPE_RAILWAY_TRACK_ = "JBTKO_SPO Spor"
local _SEARCH_HALFWIDTH_ = 500 --[m] Half-length of the reference line's normal used when searching for the target alignment

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

if n == 0 then
	write("Terminated - nothing was selected.")
	return
end

--Select target alignment:
local trk
repeat
	trk = askForAlignment("Select a railway track alignment to which the "..actionName.." object(s) will be attached.")
	if trk.RcType ~= _RCTYPE_RAILWAY_TRACK_ then
		askForKeyword(RC__identify(trk).." has wrong RcType [".._RCTYPE_RAILWAY_TRACK_.."].\n", {_OK_}, _HEADER_)
	else
		write(RC__identify(trk).."\n")
	end
until trk.RcType == _RCTYPE_RAILWAY_TRACK_

local p = askForPoint("Pick side of track to place "..actionName.." objects")
if p == nil then
	write("Terminated.")
	return
end
local sign = getLinearAddress(p, trk).lateralOffset > 0 and 1 or -1


--Returns the DistanceAlong on 'alignment' which has the same Km as 'obj', or nil plus an explanation:
local function getPosAtSameKm(obj, alignment)
	local pObj = getPoint3D(obj)

	--An object's Km is its mileage measured perpendicularly onto its track's reference (main) line, so the
	--object must be transferred via that reference line - not by projecting it onto the target alignment:
	local refId = getAlignmentInfo(obj).ReferenceAlignmentId
	if refId == nil then return nil, "its track has no reference alignment" end
	local refAi = getAlignmentInfo(refId, pObj)
	if not refAi.NormalProjectionExists then return nil, "it has no normal projection onto its reference alignment" end

	--Intersect the reference line's normal at the object's Km with the target alignment:
	local nx, ny = -refAi.Tangent.Y, refAi.Tangent.X
	local m = math.sqrt(nx*nx + ny*ny)
	nx, ny = nx/m, ny/m
	local a = getPoint3D(refAi.Point.X - _SEARCH_HALFWIDTH_*nx, refAi.Point.Y - _SEARCH_HALFWIDTH_*ny)
	local b = getPoint3D(refAi.Point.X + _SEARCH_HALFWIDTH_*nx, refAi.Point.Y + _SEARCH_HALFWIDTH_*ny)
	local hits, nHits = alignment:getIntersections({a, b})
	if nHits == 0 then
		return nil, string.format("the target alignment does not reach Km %.3f", refAi.Mileage/1000)
	end

	--The normal may cross the target alignment more than once - use the crossing nearest to the object:
	local pos, best
	for i = 0, nHits - 1 do
		local q = getAlignmentInfo(alignment.id, hits[i]).Point
		local d = (q.X - pObj.X)^2 + (q.Y - pObj.Y)^2
		if best == nil or d < best then pos, best = hits[i], d end
	end
	return pos
end


--Establish every target position before anything is cloned or moved, so that a copy is never
--left behind on top of its original because it later turns out to be unplaceable:
local target = {} --[obj.id] = {pos = DistanceAlong on trk, lateralOffset = lateral offset at the source}
local source = {} --Those objects which can actually be placed on the target alignment
local rejected = {}
for k, v in pairs(objects) do
	local pos, reason = getPosAtSameKm(v, trk)
	if pos then
		target[v.id] = {pos = pos, lateralOffset = v.LateralOffset}
		table.insert(source, v)
	else
		table.insert(rejected, RC__identify(v).." - "..reason)
	end
end

if #source == 0 then
	askForKeyword("None of the "..n.." selected object(s) can be placed on "..RC__identify(trk)..":\n\n"..
		table.concat(rejected, "\n").."\n\n"..
		"Nothing was "..actionName..", and the drawing is unchanged.\n"..
		"Extend "..RC__identify(trk).." past the Km listed above, then run the script again.\n",
		{_OK_}, _HEADER_)
	write("Terminated - no object could be placed on the target alignment.")
	return
end

if #rejected > 0 then
	--State what each option leads to - the user is about to modify the drawing:
	local proceedOption = option == _COPY_ and _COPY_THE_REST_ or _MOVE_THE_REST_
	local msg = {
		#rejected.." of the "..n.." selected object(s) cannot be placed on "..RC__identify(trk)..":",
		"",
		table.concat(rejected, "\n"),
		"",
		"["..proceedOption.."]",
		"    The other "..#source.." object(s) are "..actionName.." to "..RC__identify(trk).." at their own Km.",
		"    The "..#rejected.." object(s) listed above are left untouched on their present track."
	}
	if option == _COPY_ then
		table.insert(msg, "    No copy is made of them, so relations from the copied objects to them are lost.")
	end
	table.insert(msg, "")
	table.insert(msg, "[".._CANCEL_.."]")
	table.insert(msg, "    Nothing is copied and nothing is moved - the drawing is left exactly as it is.")
	table.insert(msg, "    Extend "..RC__identify(trk).." past the Km listed above, then run the script again.")

	local answer
	repeat
		answer = askForKeyword(table.concat(msg, "\n").."\n", {proceedOption, _CANCEL_}, _HEADER_)
	until answer == proceedOption or answer == _CANCEL_ or answer == _ESC_
	if answer ~= proceedOption then
		write("Terminated - nothing was "..actionName..".")
		return
	end
end

--[[
	_P: Use previous selection set
	_M: Mode, either Multiple (_M - keep asking for more) or single (_S).
	'(0 0 0) '(0 0 0) : From 0,0,0 to 0,0,0 (i.e. copy to same position as the source)
	_E: Exit, terminate the command
--]]
--Remember the current view in AutoLISP globals, so that it can be restored once the drawing has
--been modified - both _COPY and the repositioning of objects may otherwise shift the camera:
runCommand('(setq *rcViewCtr* (getvar "VIEWCTR") *rcViewSize* (getvar "VIEWSIZE")) ')

setSelectionSet(getCollectionFromTable(source))
if option == _COPY_ then
	--Copy previous selection set to same position, while preserving relations within the selection set:
	runCommand("_COPY _P _M _S '(0 0 0) '(0 0 0) _E ")
end

local claimed = {} --Guards against mistaking an original for a clone, or claiming the same clone twice
for k, v in pairs(objects) do claimed[v.id] = true end

local placed = {}
for _, v in ipairs(source) do
	--Move items to new target alignment and side of alignment:
	local item
	if option == _COPY_ then
		--operate on the cloned item (search around original items for their clone)
		item = getNearbyPointObjects2D(v, v.RcType, 0.1):filter(
			function (x) return not claimed[x.id] and x.Variant == v.Variant and x.Dir == v.Dir end)[0]
		if item then claimed[item.id] = true end
	else
		--option == _MOVE_, operate on the item itself
		item = v
	end

	if item == nil then
		write("Could not identify the copy of "..RC__identify(v).." - it was left in place.\n", _warning)
	else
		item.Alignment = trk.id --attach to target alignment - must assign using the ID (an illogical API...)

		--Remove any formulas which would prevent us from repositioning the object:
		item.Mileage = "="
		item.ReferenceMileage = "="
		item.DistanceAlong = "="
		item.DistanceToAlignment = "="
		item.LateralOffset = "="
		item.LongitudinalOffset = "="

		item.DistanceAlong = target[v.id].pos
		item.LongitudinalOffset = 0 --Would otherwise be carried over from the source alignment
		item.LateralOffset = sign * math.abs(target[v.id].lateralOffset)
		table.insert(placed, item)
	end
end

setSelectionSet(getCollectionFromTable(placed))

--Put the camera back where the user left it:
runCommand('(if *rcViewCtr* (command "._ZOOM" "_CENTER" *rcViewCtr* *rcViewSize*)) ')
runCommand('(setq *rcViewCtr* nil *rcViewSize* nil) ')

write("Done - "..#placed.." object(s) "..actionName..".")

