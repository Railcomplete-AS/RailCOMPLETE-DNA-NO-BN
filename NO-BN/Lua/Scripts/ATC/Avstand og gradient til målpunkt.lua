--Distance and gradient to target

--Set by user before running script:
local useOwntrackElevation = true --true==>måle høyder i eget spor, false==>måle høyder i referanselinjene.
local _IS_FATC_ = true

--Do not tamper:
local _IS_DATC_ = not _IS_FATC_
local _FATC_GRADIENT_LIMIT_ = -5 --o/oo, i.e., ignore gradients above limit when encoding FATC balises (and round down to closest multiple of 5 o/oo)
local _DATC_GRADIENT_LIMIT_ = -10 --o/oo, i.e., ignore gradients above limit when encoding DATC balises (round down to closest multiple of 5 o/oo)
local _TARGET_DISTANCE_GRADIENT_LIMIT_ = -1 --o/oo, i.e. ignore gradients above limit when computing braking curve target distances.

function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end
function stop() t = ""..nil end  --provoke error here, to see the Lua source code line number 

local usage = [[
	Distance and gradient to target (ref Bane NOR TRV:06212)
	========================================================
	2024-10-19_000 CLFEY Created.
	2024-11-02_001 CLFEY Script continues, also after non-successful choice of object.
	
	Beregning av høyder, målavstand og gradient til målet.
	
	Enkel formel - måler sporhøyden i faktisk startpunkt og faktisk målpunkt ("eget spor").
	Vi avrunder begge høyder og avstanden mellom dem til 2 desimaler først, og viser så (h2-h1)/d.
	Formelen fungerer for signaler og for balisegrupper. Formelen forutsetter at utgangsobjekt og
	målobjekt har blitt relatert med "er forsignal for" hhv "har bremsekurve til" relasjon.
	
	
	Bane NOR TRV:06213 (FATC)
	c) Fall. 
		1.	C-balise skal benyttes ved gjennomsnittlig fall ≥ 5 ‰.
		2.	(...) Gjennomsnittlig fall (‰) over balisegruppens målavstand skal benyttes, forhøyet
			til nærmeste 5, 10, 15, 20 eller 25 ‰.
		Unntak: Dersom gjennomsnittlig fall på de siste 2/3 av balisegruppens målavstand er
		større enn gjennomsnittlig fall, skal dette fallet benyttes, forhøyet til nærmeste 5,
		10, 15, 20 eller 25 ‰.
	RC TOLKNING: Først avrunde så sammenligne: 4.78 ‰ ==> 5 ‰ kodes, mens 4.47 ‰ ==> 4 ‰ kodes ikke.

	Bane NOR TRV:06165 (DATC)
	b) C-balise skal benyttes ved gjennomsnittlig fall ≥ 10 ‰.

	Bane NOR TRV:06166 (DATC)
	c) C-balise skal kodes i henhold til Tabell: Fall. Gjennomsnittlig fall (‰) over balisegruppens
	   målavstand skal benyttes, forhøyet til nærmeste 10, 15, 20 eller 25 ‰.
	Unntak: Dersom gjennomsnittlig fall på de siste 2/3 av balisegruppens målavstand er større enn
	gjennomsnittlig fall, skal dette fallet benyttes, forhøyet til nærmeste 10, 15, 20 eller 25 ‰.


	Input:
	-	Connected RC model with signals and/or balise groups.
	-	Relate objects using the RcType's appropriate distant signal / braking curve target relation.
	-	Select source object. Select target objects in succession to see distance and gradient curve info.
	-	Note: Use Edit Script and enable the Log output window to see more info from the execution.
	
	Output:
	-	A popup window showing distance and gradient curve info (and written to log window).
	
	Planned extension:
	-	Show EbiCab700 braking curve (then speed info is needed as input).
]]

local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")

--[[Example output from RC-ShowVersion:
RailCOMPLETE® version 2024.2.1.0
DNA version information:
Name: NO-BN 2021.a (Patch 9 2024-10-11: Ny variant for KL 'Bjelkemast, forsterket' - HEM260 med 3D). Fjernet 'legacy' Pset data (unntatt legacy NOBN_com_Pset[...] funksjoner).
Version: 2021.a
Administration: Bane NOR SF
Agent: Railcomplete AS
Date: 2021-11-27T21:11:27+01:00
Description: RailCOMPLETE(r) Definisjon av nettverkselementer for Bane NOR
--]]

function gradientEncoding(effectiveGradient)
	--[[
		TRV 550.10 ATC
		7.8 Tabell: Fall
		CZ	Fall [‰]
		0	35 < fall ≤ 40
		1	30 < fall ≤ 35
		2	25 < fall ≤ 30
		3	20 < fall ≤ 25
		4	15 < fall ≤ 20
		5	10 < fall ≤ 15
		6	5 < fall ≤ 10
		7	1 < fall ≤ 5		-- 1 ‰ is the limit when computing braking curves.
	--]]
	local descent, CZ
	descent = -RC__round(effectiveGradient)
	if     descent <= -30 then CZ = '14'
	elseif descent <= -25 then CZ = '13'
	elseif descent <= -20 then CZ = '12'
	elseif descent <= -15 then CZ = '11'
	elseif descent <= -10 then CZ = '10'
	elseif descent <=  -5 then CZ = '9'
	elseif descent <=   0 then CZ = '8'
	elseif descent <=   5 then CZ = '7'
	elseif descent <=  10 then CZ = '6'
	elseif descent <=  15 then CZ = '5'
	elseif descent <=  20 then CZ = '4'
	elseif descent <=  25 then CZ = '3'
	elseif descent <=  30 then CZ = '2'
	elseif descent <=  35 then CZ = '1'
	else CZ = '0'
	end
	return CZ
end


function baliseEncoding(targetDistance, roundedGradient)
	--TODO: Handle distances above 10900 m
	local targetDistanceTable = {
		--Columns: BY=0..13
		--Rows: BZ/CY=1..14
		--0		1		2		3		4		5		6		7		8		9		10		11		12		13
		{0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0	 },--0 not in use
		{12.5,	187.5,	362.5,	537.5,	725,	1075,	1450,	2200,	3600,	5000,	6400,	7800,	9200,	10600},--1
		{25,	200,	375,	550,	750,	1100,	1500,	2300,	3700,	5100,	6500,	7900,	9300,	10700},--2
		{37.5,	212.5,	387.5,	562.5,	775,	1125,	1550,	2400,	3800,	5200,	6600,	8000,	9400,	10800},--3
		{50,	225,	400,	575,	800,	1150,	1600,	2500,	3900,	5300,	6700,	8100,	9500,	10900},--4
		{62.5,	237.5,	412.5,	587.5,	825,	1175,	1650,	2600,	4000,	5400,	6800,	8200,	9600,	11000},--5
		{75,	250,	425,	600,	850,	1200,	1700,	2700,	4100,	5500,	6900,	8300,	9700,	11100},--6
		{87.5,	262.5,	437.5,	612.5,	875,	1225,	1750,	2800,	4200,	5600,	7000,	8400,	9800,	11200},--7
		{100,	275,	450,	625,	900,	1250,	1800,	2900,	4300,	5700,	7100,	8500,	9900,	11300},--8
		{112.5,	287.5,	462.5,	637.5,	925,	1275,	1850,	3000,	4400,	5800,	7200,	8600,	10000,	11400},--9
		{125,	300,	475,	650,	950,	1300,	1900,	3100,	4500,	5900,	7300,	8700,	10100,	11500},--10
		{137.5,	312.5,	487.5,	662.5,	975,	1325,	1950,	3200,	4600,	6000,	7400,	8800,	10200,	11600},--11
		{150,	325,	500,	675,	1000,	1350,	2000,	3300,	4700,	6100,	7500,	8900,	10300,	11700},--12
		{162.5,	337.5,	512.5,	687.5,	1025,	1375,	2050,	3400,	4800,	6200,	7600,	9000,	10400,	11800},--13
		{175,	350,	525,	700,	1050,	1400,	2100,	3500,	4900,	6300,	7700,	9100,	10500,	11900} --14
--		 ---------12.5M trinn--------,	-----25-----,	-50-,	-------------------100m trinn------------------------
	}
	
	local row, column
	local BX, BY, BZ_CY, CX, CZ
	BX = 9 --fixed encoding
	CX = 14 --fixed encoding
	col = -1
	for column = 0, 13 do
		col = col + 1
		rw = 0
		for row = 1, 14 do
			rw = rw + 1
			if targetDistance >= targetDistanceTable[row+1][column+1] then --tables are indexed from 1 and up
				BY = column
				BZ_CY = row
				val = targetDistanceTable[rw][col]
			end
		end
	end
	writeln("Target distance "..targetDistance.." / rounded gradient "..roundedGradient.." ==> BY="..BY.." BZ/CY="..BZ_CY)
	
	if (_IS_FATC_ and roundedGradient > -5) or (_IS_DATC_ and roundedGradient > -10) then
		return string.format("B(9,%d,%d)", BY, BZ_CY) --No C balise
	else
		CZ = gradientEncoding(roundedGradient)
		return string.format("B(9,%d,0) + C(14,%d,%d)", BY, BZ_CY, CZ)
	end
end

function distanceAndGradientToTarget(obj1, obj2, orig1, orig2)
	--Assume that there is a braking curve relationship from obj1 to obj2
	local h1, h2, h1RefId, h2RefId
	if useOwntrackElevation then
		h1 = RC__round(obj1:getAlignmentInfo().Elevation, 2)
		h2 = RC__round(obj2:getAlignmentInfo().Elevation, 2)
	else
		h1RefId = obj1:getAlignmentInfo().ReferenceAlignmentId
		h2RefId = obj2:getAlignmentInfo().ReferenceAlignmentId
		h1 = RC__round(obj1:getAlignmentInfo(h1RefId).Elevation, 2)
		h2 = RC__round(obj2:getAlignmentInfo(h2RefId).Elevation, 2)
	end
	
	if RC__isNan(getDistance(obj1,obj2)) then
		return "No path found from '"..RC__identify(obj1).."' to target '"..RC__identify(obj2).."'."
	end

	--There is a path from source to target
	local d = math.abs(RC__round(getDistance(obj1,obj2), 0))
	local pos1 = getAlignmentPos(obj1)
	local pos13 = getAlignmentPos(pos1.Ref, pos1.Pos + (obj1.dir == "up" and d/3 or -d/3)) --1/3 towards target
	local km1 = RC__toKm(obj1.ReferenceMileage)
	local km2 = RC__toKm(obj2.ReferenceMileage)
	local kmDiff = RC__round(1000*math.abs(km2-km1))
	local h13 = RC__round(getAlignmentInfo(pos13).Elevation,2)
	local gradient = 1000*(h2-h1)/d
	local gradient23 = 1000*(h2-h13)/(d*2/3)
	local effectiveGradient = gradient < gradient23 and gradient or gradient23
	local roundedGradient = RC__round(effectiveGradient) --to nearest integer, i.e. -5.45 ==> -5 etc, -7.89 ==> -8, +3.77 ==> 4 etc.
	local t = string.format(
		"%-15s %-15s %-15s %-15s "..
		"%8.03f %8.03f %8.0f %8.02f "..
		"%10.02f  %10.02f  %10.02f  %10.02f  %10.02f "..
		"%10.02f %10.02f %10.0f",
		RC__identify(orig1), RC__identify(obj1), RC__identify(orig2), RC__identify(obj2),
		km1, km2, kmDiff, d,
		h1, h2, (h2-h1), h13, (h2-h13),
		gradient, gradient23, roundedGradient)
	return {["Text"] = t, ["TargetDistance"] = kmDiff, ["RoundedGradient"] = roundedGradient}
end



local tSingleTarget = "Specific target's distance and gradient"
local tRelatedTargets = "All related targets' distance and gradient"
local tFindAllTargets = "Search for all potential targets' distance and gradient"
local mode = askForKeyword(usage, {tSingleTarget, tRelatedTargets, tFindAllTargets})
writeln("Distance and gradient to target (ref Bane NOR TRV:06212).")

local caption =	
	"Fra objekt        Fra proxy         Til objekt          Til proxy          "..
	"Fra Km   Til Km    Km-diff   Avstand  "..
	"Fra moh Til moh   H-diff   H13 moh  H13diff   "..
	"Grad.%  Grad2/3%  Kodet‰ Balisekoding\n"..
	"----------         ----------          ----------          ----------         "..
	"--------   --------   --------   --------  "..
	"--------   --------   --------   --------   --------   "..
	"---------  ---------  ---------  ------------"

local finished = false


function findSourceProxy(obj)
	local proxy
	if obj.RcType == "JBTSA_ATC NSS balisegruppe" then
		if obj.Variant == "Hovedsignalbalisegruppe"
		or obj.Variant == "Forsignalbalisegruppe"
		or obj.Variant == "Sporvekselbalisegruppe" then
			--For SVG, use the associated signal as proxy if SVG connects to it, otherwise use SCG's balise group as proxy:
			local tmp = obj:getRelatedObjects("Gjelder for signal/skilt/stolpe/sporveksel")[0]
			if tmp.RcType == "JBTSA_SIG Signal" then
				proxy = tmp
			else
				proxy = obj:getRelatedObjects("Har posisjonsdefinerende NSS_balise")[0]
			end
		else
			proxy = obj:getRelatedObjects("Har posisjonsdefinerende NSS_balise")[0]
		end
	else
		proxy = obj
	end
	return proxy
end


function findTargetProxy(obj)
	local proxy
	if obj.RcType == "JBTSA_ATC NSS balisegruppe" then
		if obj.Variant == "Hovedsignalbalisegruppe"
		or obj.Variant == "Forsignalbalisegruppe" then
			proxy = obj:getRelatedObjects("Gjelder for signal/skilt/stolpe/sporveksel")[0]
		else
			proxy = obj:getRelatedObjects("Har posisjonsdefinerende NSS_balise")[0]
		end
	else
		proxy = obj
	end
	return proxy
end

local obj1, obj2

repeat
	obj1 = askForObject("Select source object (ESC terminates)")
	if not obj1 then
		finished = true
	else
		if mode == tSingleTarget then
			local proxy1 = findSourceProxy(obj1)
			writeln("Source = ["..obj1.RcType.."] "..RC__identify(obj1).." with proxy "..RC__identify(proxy1))
			obj2 = askForObject("Select a reachable target object of RcType '"..obj1.RcType.."'.")
			local proxy2 = findTargetProxy(obj2)
			writeln("	Target = ["..obj2.RcType.."] "..RC__identify(obj2).." with proxy "..RC__identify(proxy2))
			local dag = distanceAndGradientToTarget(proxy1, proxy2, obj1, obj2)
			local u1 = dag.Text
			local u2 = " "..baliseEncoding(dag.TargetDistance, dag.RoundedGradient)
			show(caption.."\n"..u1.."\t"..u2)

		else
			local r, n, s
			if mode == tRelatedTargets then
				if obj1.RcType == "JBTSA_SIG Signal" then
					s = "Er forsignal for signal"
				elseif obj1.RcType == "JBTSA_MSS Signal 60 ATC" then
					r, n = obj1:getRelatedObjects("Har NSS_balisegruppe")
					if n > 0 then
						obj1 = r[0]
						s = "Har bremsekurve målpunkt i NSS_balisegruppe"
					end
				elseif obj1.RcType == "JBTSA_ATC NSS balisegruppe" then
					s = "Har bremsekurve målpunkt i NSS_balisegruppe"
				elseif obj1.RcType == "JBTSA_ERT ERTMS-signal" then
					s = "Er forsignal for signal"
				elseif obj1.RcType == "JBTSA_ETC ETCS balisegruppe" then
					s = "Har bremsekurve målpunkt i ETCS_balisegruppe"
				else
					show("Distance/gradient formula cannot be used for objects of RcType '"..obj1.RcType.."'.")
				end
				r, n = obj1:getRelatedObjects(s)

			elseif mode == tFindAllTargets then
			
				local goalfunction = function (x)
					if x.id == obj1.id then return false end
					if x.RcType ~= "JBTSA_SIG Signal" 
					and x.RcType ~= "JBTSA_ATC NSS balisegruppe"
					and x.RcType ~= "JBTKO_SPV Sporveksel" then
						return false
					end
					if obj1.RcType == "JBTSA_SIG Signal" then
						if x.RcType ~= obj1.RcType then return false end
						if x.MainSignal == "Hs2" or x.MainSignal == "Hs3" or x.DistantSignal == "Ja" then
							return x.dir == obj1.dir
						else 
							return false
						end
					elseif obj1.RcType == "JBTSA_MSS Signal 60 ATC"
					or obj1.RcType == "" then
						r, n = obj1:getRelatedObjects("Har NSS_balisegruppe")
						if n > 0 then
							tmp = r[0]
							return x.id == tmp:getRelatedObjects("Har bremsekurve målpunkt i NSS_balisegruppe")[0].id
						end
					elseif obj1.RcType == "JBTSA_ATC NSS balisegruppe" then
						if obj1.Variant == "Hovedsignalbalisegruppe"
						or obj1.Variant == "Forsignalbalisegruppe"
						or obj1.Variant == "Repeterbalisegruppe"
						or obj1.Variant == "Lenkingsbalisegruppe"
						or obj1.Variant == "Signalhøyningsbalisegruppe" then
							if x.Variant == "Hovedsignalbalisegruppe" 
							or x.Variant == "Forsignalbalisegruppe"
							or x.Variant == "Lenkingsbalisegruppe" then
								return x.dir == obj1.dir
							else
								return false
							end
						elseif obj1.Variant == "Sporvekselbalisegruppe" then
							if _IS_FATC_ then
								--Expect a balise group as braking curve target
								--NB Not always true... braking curve points at SRJ usually:
								return x.id == obj1:getRelatedObjects("Har bremsekurve målpunkt i NSS_balisegruppe")[0].id
							else
								--Expect a switch stock rail as braking curve target
								if x.RcType ~= "JBTKO_SPV Sporveksel" then return false end
								return x.dir == obj1.dir --Target is assumed to be first switch found in current direction
							end
							
						elseif obj1.Variant == "Hastighetsbalisegruppe" then
							if x.Variant ~= obj1.Variant then return false end
							local r1, n1 = obj1:getRelatedObjects("Har bremsekurve målpunkt i NSS_balisegruppe")
							if n1 == 0 then
								return true --bad case, target is undefined, has no board - return it anyway
							else
								return x.id == r1[0].id --There is a braking curve relation to target x (which may be counterdirected)
							end
						end
					else
						return false
					end
				end --goalfunction
					
				if obj1.dir == "up" then
					r = getCollectionFromTable({obj1:getUpObject(goalfunction)})
				else
					r = getCollectionFromTable({obj1:getDownObject(goalfunction)})
				end
				n = getCollectionLength(r)
			else
				show("Illegal mode '"..mode.."' encountered - contact support@railcomplete.com.")
				halt()
			end
			
			if n == 0 then
				--TODO: Show balise group encoding for groups which do not feature a braking curve.
				show("No relevant related targets were found from source '"..RC__identify(obj1).."'.")
			else
				local proxy1 = findSourceProxy(obj1)
				--Some balise group types shall use their associated object's position instead of the balise group's position:

				writeln("Source = ["..obj1.RcType.."] "..RC__identify(obj1)
					.. (proxy1.id ~= obj1.id and " proxy "..RC__identify(proxy1) or ""))
				local t, u1, u2
				for i = 0, n-1 do
					obj2 = r[i]
					local proxy2 = findTargetProxy(obj2)
					writeln("Target "..i.." = ["..obj2.RcType.."] "..RC__identify(obj2)
						.. (proxy2.id ~= obj2.id and " proxy "..RC__identify(obj2) or ""))
					local dag = distanceAndGradientToTarget(proxy1, proxy2, obj1, obj2)
					local u1 = dag.Text
					local u2 = " "..baliseEncoding(dag.TargetDistance, dag.RoundedGradient)
					t = t and t.."\n"..u1.."\t"..u2 or u1.."\t"..u2
				end
				show(caption.."\n"..t)
			end
		end
		writeln()
	end
until finished
writeln("Bye.")
