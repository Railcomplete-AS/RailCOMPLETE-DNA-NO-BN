--[[
	Insert balise group
	==========================
	2024-05-20_000 CLFEY Created.
	2026-01-11_001 CLFEY Changed RC__toKm() into NOBN_trk_toKm() in line with DNA 2026.1 and 2021.a.11.
	
	TODO:
	- Compute rounded Km for each balise.
	- Adjust text style and height for balise group.
--]]

local rctype_balise = "JBTSA_ATB NSS balise"
local rctype_baliseGroup = "JBTSA_ATC NSS balisegruppe"
local variants_balisegroup = {"Hovedsignalbalisegruppe", "Forsignalbalisegruppe", "Repeterbalisegruppe", "Sporvekselbalisegruppe", 
	"Hastighetsbalisegruppe", "Lenkingsbalisegruppe", "Signalhøyningsbalisegruppe", "ET-balisegruppe", "Grensebalisegruppe", 
	"Radioområdebalisegruppe", "Diversebalisegruppe"}

function writeln(t) write(t and (tostring(t) or "") .."\n") end

local p = askForPoint("Select balise group placement (Km will be rounded)")
local alg = p:getNearbyAlignments()[0]
local inKm = getAlignmentInfo(getAlignmentInfo(alg.id, p).ReferenceAlignmentId, p).Mileage
local km = RC__round(inKm)
local inPos = getAlignmentInfo(alg.id, p).RelativePosition
local pos = inPos + (km - inKm)
local dir = askForKeyword("Select balise group direction", {"up", "down"})
local groupType = askForKeyword("Select balise group type", variants_balisegroup)
local configP, configA, configB, configC
local dualDirection
if RC__isMemberOf({"Hovedsignalbalisegruppe", "Forsignalbalisegruppe", "Repeterbalisegruppe"}, groupType) then
	configP = askForKeyword("P-balise", {"No P", "Fixed P", "Controlled P"})
	dualDirection = "Single"
else
	configP = "No P"
	dualDirection = askForKeyword("Select single/dual direction", {"Single", "Dual"})
end

configA = askForKeyword("A-balise", {"Fixed A", "Controlled A"})
configB = askForKeyword("B-balise", {"Fixed B", "Controlled B"})
if RC__isMemberOf({"Hovedsignalbalisegruppe" or "Forsignalbalisegruppe", "Repeterbalisegruppe", "Hastighetsbalisegruppe"}, groupType) then
	configC = askForKeyword("C-balise", {"No C", "Fixed C", "Controlled C"})
else
	configC = "No C"
end

writeln(string.format("Alignment: %s, dir:= %s, Km: %s, groupType: %s, configuration: %s / %s / %s / %s", RC__identify(alg), dir, NOBN_trk_toKm(km), groupType, configP, configA, configB, configC))

if askForKeyword("Confirm insertion", {"Insert group", "Cancel"}) == "Insert group" then
	local baliseGroupAbsDistanceToAlignment = 10
	local baliseGroupSideOfAlignmentIsLeft = getAlignmentInfo(alg.id, p).DistanceToAlignment < 0 --true iff p is on left side of alignment
	baliseGroup = createPointObject(alg, rctype_baliseGroup, groupType, pos, baliseGroupAbsDistanceToAlignment, baliseGroupSideOfAlignmentIsLeft) --Object snaps itself to a nice distance
	baliseGroup.dir = "="
	baliseGroup.dir = dir
	
	posP = pos + (dir == "up" and 1 or -1) * (-3)
	if configP == "Controlled P" then
		baliseP = createPointObject(alg, rctype_balise, "Balise fylt/styrt", posP, 0.010, true)
	elseif configP == "Fixed P" then
		baliseP = createPointObject(alg, rctype_balise, "Balise fylt/fast", posP, 0.010, true)
	else
		--no P balise
	end

	posA = pos
	if configA == "Controlled A" then
		baliseA = createPointObject(alg, rctype_balise, baliseP and "Balise tom/styrt" or "Balise fylt/styrt", posA, 0.010, true)
	else
		baliseA = createPointObject(alg, rctype_balise, baliseP and "Balise tom/fast" or "Balise fylt/fast", posA, 0.010, true)
	end

	posB = pos + (dir == "up" and 1 or -1) * 3
	if configB == "Controlled B" then
		--It is very rare that double directed groups are controlled in the reverse direction.
		baliseB = createPointObject(alg, rctype_balise, dualDirection == "Dual" and "Balise fylt/styrt" or "Balise tom/styrt", posB, 0.010, true)
	else
		baliseB = createPointObject(alg, rctype_balise, dualDirection == "Dual" and "Balise fylt/fast" or "Balise tom/fast", posB, 0.010, true)
	end

	posC = pos + (dir == "up" and 1 or -1) * 6
	if configC == "Controlled C" then
		baliseC = createPointObject(alg, rctype_balise, "Balise tom/styrt", posC, 0.010, true)
	elseif configC == "Fixed C" then
		baliseC = createPointObject(alg, rctype_balise, "Balise tom/fast", posC, 0.010, true)
	else
		--no C balise
	end

	if baliseA then setRelation(baliseA, baliseGroup, "Definerer posisjon for NSS_balisegruppe") end --should always work

	if baliseP then setRelation(baliseP, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseA then setRelation(baliseA, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseB then setRelation(baliseB, baliseGroup, "Tilhører NSS_balisegruppe") end
	if baliseC then setRelation(baliseC, baliseGroup, "Tilhører NSS_balisegruppe") end
	
	--Unlock layers - delete layer name formula:
	if baliseGroup then baliseGroup.Layer = "=" end
	if baliseP then baliseP.Layer = "=" end
	if baliseA then baliseA.Layer = "=" end
	if baliseB then baliseB.Layer = "=" end
	if baliseC then baliseC.Layer = "=" end
	
	if RC__DNA_VERSION():match("2021%-11%-27") then
		setTextPositionFormula(baliseGroup, "OBJEKTNAVN", "RC__acsVector2wcsVector((RightSided and 1 or -1) * 8/DocumentData.Document.Database.Cannoscale.Scale, 0)")
	else
		balisegroup.TextAttribute_OBJEKTNAVN.Position = "="
		balisegroup.TextAttribute_OBJEKTNAVN.Position = "=(RightSided and 1 or -1), 0"
	end
	
	runCommand('(ALERT "Complete the balisegroup configuration using relations relevant to the group type / set its Sequence number") ')
	
	setSelectionSet({baliseGroup, baliseP, baliseA, baliseB, baliseC}) --The user may now use LAYMCH to set the right layer.
	
	writeln("Done.")
else
	writeln("Cancelled.")
end


