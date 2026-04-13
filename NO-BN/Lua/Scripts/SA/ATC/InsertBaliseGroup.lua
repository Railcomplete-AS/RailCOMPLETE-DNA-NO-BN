--[[
	InsertBaliseGroup
	=================
	Interactively inserts an NSS balise group with associated balises
	on a selected alignment.

	TODO:
	- Compute rounded Km for each balise.
	- Adjust text style and height for balise group.

	2024-05-20 v1.0 CLFEY Created.
	2026-01-11 v1.1 CLFEY Changed RC__toKm() into NOBN_trk_toKm() in line with DNA 2026.1 and 2021.a.11.
--]]



---LOCAL CONSTANTS---
local _RCTYPE_BALISE_ = "JBTSA_ATB NSS balise"
local _RCTYPE_BALISE_GROUP_ = "JBTSA_ATC NSS balisegruppe"
local _VARIANTS_BALISE_GROUP_ = {
	"Hovedsignalbalisegruppe", "Forsignalbalisegruppe", "Repeterbalisegruppe", "Sporvekselbalisegruppe",
	"Hastighetsbalisegruppe", "Lenkingsbalisegruppe", "Signalhøyningsbalisegruppe", "ET-balisegruppe",
	"Grensebalisegruppe", "Radioområdebalisegruppe", "Diversebalisegruppe"
}
local _SELECT_PLACEMENT_MSG_ = "Select balise group placement (Km will be rounded)"
local _SELECT_DIRECTION_MSG_ = "Select balise group direction"
local _SELECT_TYPE_MSG_ = "Select balise group type"
local _P_BALISE_MSG_ = "P-balise"
local _SELECT_DIRECTION_MODE_MSG_ = "Select single/dual direction"
local _A_BALISE_MSG_ = "A-balise"
local _B_BALISE_MSG_ = "B-balise"
local _C_BALISE_MSG_ = "C-balise"
local _CONFIRM_INSERTION_MSG_ = "Confirm insertion"



---FUNCTIONS---

-- Writes a message to the log window and appends a newline:
local function writeln(t) write(t and (tostring(t) or "") .. "\n") end



---SCRIPT---

local p = askForPoint(_SELECT_PLACEMENT_MSG_)
local alg = p:getNearbyAlignments()[0]
local inKm = getAlignmentInfo(getAlignmentInfo(alg.id, p).ReferenceAlignmentId, p).Mileage
local km = RC__round(inKm)
local inPos = getAlignmentInfo(alg.id, p).RelativePosition
local pos = inPos + (km - inKm)
local dir = askForKeyword(_SELECT_DIRECTION_MSG_, {"up", "down"})
local groupType = askForKeyword(_SELECT_TYPE_MSG_, _VARIANTS_BALISE_GROUP_)
local configP, configA, configB, configC
local dualDirection
if RC__isMemberOf({"Hovedsignalbalisegruppe", "Forsignalbalisegruppe", "Repeterbalisegruppe"}, groupType) then
	configP = askForKeyword(_P_BALISE_MSG_, {"No P", "Fixed P", "Controlled P"})
	dualDirection = "Single"
else
	configP = "No P"
	dualDirection = askForKeyword(_SELECT_DIRECTION_MODE_MSG_, {"Single", "Dual"})
end

configA = askForKeyword(_A_BALISE_MSG_, {"Fixed A", "Controlled A"})
configB = askForKeyword(_B_BALISE_MSG_, {"Fixed B", "Controlled B"})
if RC__isMemberOf({"Hovedsignalbalisegruppe", "Forsignalbalisegruppe", "Repeterbalisegruppe", "Hastighetsbalisegruppe"}, groupType) then
	configC = askForKeyword(_C_BALISE_MSG_, {"No C", "Fixed C", "Controlled C"})
else
	configC = "No C"
end

writeln(string.format("Alignment: %s, dir:= %s, Km: %s, groupType: %s, configuration: %s / %s / %s / %s",
	RC__identify(alg), dir, NOBN_trk_toKm(km), groupType, configP, configA, configB, configC))

-- Is the insertion confirmed?
if askForKeyword(_CONFIRM_INSERTION_MSG_, {"Insert group", "Cancel"}) == "Insert group" then
	local baliseGroupAbsDistanceToAlignment = 10
	-- Is p on the left side of the alignment?
	local baliseGroupSideOfAlignmentIsLeft = getAlignmentInfo(alg.id, p).DistanceToAlignment < 0
	local baliseGroup = createPointObject(alg, _RCTYPE_BALISE_GROUP_, groupType, pos,
		baliseGroupAbsDistanceToAlignment, baliseGroupSideOfAlignmentIsLeft)
	baliseGroup.dir = "="
	baliseGroup.dir = dir

	local posP = pos + (dir == "up" and 1 or -1) * (-3)
	local baliseP
	if configP == "Controlled P" then
		baliseP = createPointObject(alg, _RCTYPE_BALISE_, "Balise fylt/styrt", posP, 0.010, true)
	elseif configP == "Fixed P" then
		baliseP = createPointObject(alg, _RCTYPE_BALISE_, "Balise fylt/fast", posP, 0.010, true)
	end

	local posA = pos
	local baliseA
	if configA == "Controlled A" then
		baliseA = createPointObject(alg, _RCTYPE_BALISE_, baliseP and "Balise tom/styrt" or "Balise fylt/styrt", posA, 0.010, true)
	else
		baliseA = createPointObject(alg, _RCTYPE_BALISE_, baliseP and "Balise tom/fast" or "Balise fylt/fast", posA, 0.010, true)
	end

	local posB = pos + (dir == "up" and 1 or -1) * 3
	local baliseB
	if configB == "Controlled B" then
		-- It is very rare that double directed groups are controlled in the reverse direction:
		baliseB = createPointObject(alg, _RCTYPE_BALISE_, dualDirection == "Dual" and "Balise fylt/styrt" or "Balise tom/styrt", posB, 0.010, true)
	else
		baliseB = createPointObject(alg, _RCTYPE_BALISE_, dualDirection == "Dual" and "Balise fylt/fast" or "Balise tom/fast", posB, 0.010, true)
	end

	local posC = pos + (dir == "up" and 1 or -1) * 6
	local baliseC
	if configC == "Controlled C" then
		baliseC = createPointObject(alg, _RCTYPE_BALISE_, "Balise tom/styrt", posC, 0.010, true)
	elseif configC == "Fixed C" then
		baliseC = createPointObject(alg, _RCTYPE_BALISE_, "Balise tom/fast", posC, 0.010, true)
	end

	-- Should always work:
	if baliseA then setRelation(baliseA, baliseGroup, "Definerer posisjon for NSS_balisegruppe") end

	if baliseP then setRelation(baliseP, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseA then setRelation(baliseA, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseB then setRelation(baliseB, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseC then setRelation(baliseC, baliseGroup, "Tilhører NSS_balisegruppe") end

	-- Unlock layers - delete layer name formula:
	if baliseGroup then baliseGroup.Layer = "=" end
	if baliseP then baliseP.Layer = "=" end
	if baliseA then baliseA.Layer = "=" end
	if baliseB then baliseB.Layer = "=" end
	if baliseC then baliseC.Layer = "=" end

	if RC__DNA_VERSION():match("2021%-11%-27") then
		setTextPositionFormula(baliseGroup, "OBJEKTNAVN",
			"RC__acsVector2wcsVector((RightSided and 1 or -1) * 8/DocumentData.Document.Database.Cannoscale.Scale, 0)")
	else
		baliseGroup.TextAttribute_OBJEKTNAVN.Position = "="
		baliseGroup.TextAttribute_OBJEKTNAVN.Position = "=(RightSided and 1 or -1), 0"
	end

	runCommand('(ALERT "Complete the balisegroup configuration using relations relevant to the group type / set its Sequence number") ')

	-- The user may now use LAYMCH to set the right layer:
	setSelectionSet({baliseGroup, baliseP, baliseA, baliseB, baliseC})

	writeln("Done.")
else
	writeln("Cancelled.")
end
