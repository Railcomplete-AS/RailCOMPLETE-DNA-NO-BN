function _JBTSA_ATC_code()
	local s, r, n, sig, bg, nbg, b1, b2, b3, b4, d1, d2, d3, d4, a, ocpMsg, ocp
	if Variant == "Hastighetsbalisegruppe" then
		a = "-H"
	elseif Variant == "Signalhøyningsbalisegruppe" then
		a = "-S"
	elseif Variant == "Grensebalisegruppe" then
		a = "-G"
	elseif Variant == "Lenkingsbalisegruppe" then
		a = "-L"
	elseif Variant == "Sporvekselbalisegruppe" then
		a = "-V"
	elseif Variant == "Hovedsignalbalisegruppe" then 
		a = "_"
	elseif Variant == "Forsignalbalisegruppe" then
		a = "F"
	elseif Variant == "Repeterbalisegruppe" then
		s = "Gjelder for signal/skilt/stolpe/sporveksel"
		r, n = getRelatedObjects(s)
		if n == 0 then
			-- Balise group has no relation to a signal yet:
			return "UNFINISHED - Relate with '"..s.."'."
		else
			sig = r[0] --should be a signal here, repeater groups don't repeat switches directly
			bg = getRelatedObjects("Har NSS_balisegruppe", sig)
			bg = bg:filter(function (x) return x.Variant == "Repeterbalisegruppe" end)
			nbg = getCollectionLength(bg)
			if nbg == 0 then
				--I am here, so n *must* be greater than zero:
				return "Should not happen - bad relation loop." 
			elseif nbg == 1 then
				-- I am the only repeater group:
				a = "R"
			elseif nbg == 2 then
				-- Assign 'R' to the one farthest away, 'U' to the one closest to signal: 
				b1 = bg[0]
				b2 = bg[1]
				if distance(b1, sig) > distance(b2, sig) then
					b1, b2 = b2, b1
					--Now b1 is closest to signal
				end
				if this.id == b1.id then
					--I am the closest repeater group:
					a = "U"
				else
					--I am the farthest repeater group:
					a = "R" -- CHANGED
				end
			elseif nbg == 3 then
				-- Assign 'R' to the one farthest away, then 'U' and then 'V' to the one closest to signal:
				b1 = bg[0]
				b2 = bg[1]
				b3 = bg[2]
				-- Three pairwise swaps and they're bubble sorted:
				if distance(b1, sig) > distance(b2, sig) then b1, b2 = b2, b1 end
				if distance(b2, sig) > distance(b3, sig) then b2, b3 = b3, b2 end
				if distance(b1, sig) > distance(b2, sig) then b1, b2 = b2, b1 end
				if this.id == b1.id then
					--I am the closest repeater group:
					a = "V"
				elseif this.id == b2.id then
					--I am the middle repeater group:
					a = "U"
				else 
					a = "R"
				end
			elseif nbg == 4 then
				-- Assign 'R' to the one farthest away, then 'U' and then 'V' to the one closest to signal:
				b1 = bg[0]
				b2 = bg[1]
				b3 = bg[2]
				b4 = bg[3]
				d1 = distance(b1, sig)
				d2 = distance(b2, sig)
				d3 = distance(b3, sig)
				d4 = distance(b4, sig)
				-- Six pairwise swaps and they're bubble sorted:
				if d1 > d2 then b1, b2 = b2, b1 end
				if d2 > d3 then b2, b3 = b3, b2 end
				if d3 > d4 then b3, b4 = b4, b3 end
				if d1 > d2 then b1, b2 = b2, b1 end
				if d2 > d3 then b2, b3 = b3, b2 end
				if d1 > d2 then b1, b2 = b2, b1 end
				if this.id == b1.id then
					--I am the closest repeater group:
					a = "W"
				elseif this.id == b2.id then
					--I am the next to closest repeater group:
					a = "V"
				elseif this.id == b3.id then
					--I am the next to farthest repeater group:
					a = "U"
				else 
					a = "R"
				end
			end
		end
	end
	ocpMsg = "Create an Operation / Control Point (OCP) area around your object and refresh it to replace '???' with the relevant OCP's codes."
	if Variant == "Hastighetsbalisegruppe" then
		r, n = getRelatedObjects("Har bremsekurve målpunkt i NSS_balisegruppe")
		if n == 0 then
			--Group is braking curve target:
			ocp = RC_com_getOcpCode(getObjectFromId(id))
		else
			--Warning board, use target's ocp:
			ocp = RC_com_getOcpCode(r[0])
		end
		return ocp..a..string.format("%02d", seq)
	elseif Variant == "Signalhøyningsbalisegruppe" or Variant == "Sporvekselbalisegruppe" then
		ocp = RC_com_getOcpCode(this)
		if seq == 0 then
			return ocp..a.."UNFINISHED - Set balise group's 'seq' property to the required balise group number (01..99), use '100' for '0'."
		else
			return ocp..a..string.format("%02d", seq % 100)
		end
	elseif Variant =="Lenkingsbalisegruppe" or Variant == "Grensebalisegruppe" then
		s = "Lenker til NSS_balisegruppe"
		r, n = getRelatedObjects(s)
		if n == 0 then
			ocp = RC_com_getOcpCode(getObjectFromId(id))
		else
			ocp = RC_com_getOcpCode(r[0])
		end
		if seq == 0 then
			return ocp..a.."UNFINISHED - Set balise group's 'seq' property to the required balise group number (01..99), use '100' for '0'."
		else
			return ocp..a..string.format("%02d", seq % 100)
		end
	else
		s = "Gjelder for signal/skilt/stolpe/sporveksel"
		r, n = getRelatedObjects(s)
		if n == 0 then
			return "UNFINISHED - Relate with '"..s.."'."
		else
			--Assume r[0] holds object dictating the group's number:
			ocp = RC_com_getOcpCode(r[0])
			if r[0].code == nil then 
				return ocp..a.."???", _info(ocpMsg)
			elseif tostring(r[0].code):match("%d+") == nil then
				return ocp..a.."???", _info(ocpMsg)
			else
				return ocp..a..tostring(r[0].code):match("%d+"):sub(-3) 
			end 
		end
		return ocp..a..code:match("%d+"):sub(-3)
	end
end
