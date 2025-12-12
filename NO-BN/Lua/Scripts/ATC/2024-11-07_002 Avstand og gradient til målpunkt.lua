--[[
	Avstand og gradient til målpunkt
	================================
	Change history: See 'aboutMsg' below.
	Usage: See usageMsg below.
	About: See aboutMsg below.
	
--]]
	
local tWindowTitle = "Avstand, gradient og ATC koding mot et målpunkt"
local aboutMsg = [[
ABOUT
=========================================================================================

This script requires RC 2024.2.6 or more recent versions.

2024-10-19_000 CLFEY Created.
2024-11-02_001 CLFEY Script continues, also after non-successful choice of object.
2025-11-07_002 CLFEY Print using monospace font. Added better explanations in the Usage window. Added RC rounding method.

Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
]]

--Set by user before running script:
local useOwntrackElevation = true --true==>måle høyder i eget spor, false==>måle høyder i referanselinjene.
local _IS_FATC_ = true

--Do not tamper:
local _IS_DATC_ = not _IS_FATC_
local _FATC_GRADIENT_LIMIT_ = -5 --o/oo, i.e., ignore gradients above limit when encoding FATC balises (and round down to closest multiple of 5 o/oo)
local _DATC_GRADIENT_LIMIT_ = -10 --o/oo, i.e., ignore gradients above limit when encoding DATC balises (round down to closest multiple of 5 o/oo)
local _TARGET_DISTANCE_GRADIENT_LIMIT_ = -1 --o/oo, i.e. ignore gradients above limit when computing braking curve target distances.

function writeln(t) write((t or "").."\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}, "Avstand og gradient til målpunkt", FontType.Monospace) end
function stop() t = ""..nil end  --provoke error here, to see the Lua source code line number 


local usageMsg = [[
BEREGNING AV HØYDE, MÅLAVSTAND OG GRADIENT (FALL)
=========================================================================================

Inndata:
- RC-modell som inneholder signaler koblet med relasjon for forsignalering, samt baliser og balisegrupper.
- Lyssignaler og ERTMS markerboards skal være relatert til hverandre med gjeldende relasjoner for forsignalering.
- Balisegrupper for signaler og sporveksler skal være relatert til sine respektive objekter.
- Balisegrupper for fullstendig utrustet hastighetsnedsetting skal være relatert til målpunkt-gruppe.

- Tips: Aktiver utdata-vinduet i script-verktøyet for å se mer informasjon.

Utdata:
- Et popup-vindu som viser informasjon om avstand og gradientkurve (og skrives til loggvinduet).
- Velg «Vis output-format» nedenfor for å se mer informasjon om de ulike utdatafeltene.

Planlagte utvidelser:
- Vise EbiCab700-bremsekurver (da er hastighetsinformasjon nødvendig som inndata).
- Tilrettelegge for DATC, dvs at hastighetsbalisegrupper relateres til skilt i målpunktet.
- Tilrettelegge for ERTMS bremsekurver.
]]


local outputFormatMsg = [[
BESKRIVELSE AV OUTPUTFORMATET
=========================================================================================

RailCOMPLETE måler laveste skinnes høyde over havet i faktisk startpunkt og faktisk målpunkt (i proxy-objektets eget spor). Vi
viser begge høyder og avstanden mellom dem til 2 desimaler først, og viser så (h2-h1)/d. Formelen fungerer for signaler og for 
balisegrupper. Formelen forutsetter at utgangsobjekt og målobjekt har blitt relatert med "er forsignal for" hhv "har bremsekurve 
til" relasjon.

"Proxyobjekt" = "stedfortredende objekt", f.eks. en hovedsignal-balisegruppe vil benytte hovedsignalets Km i stedet for
A-balisens Km.

Fra objekt  Et objekt som angis av brukeren, enten en balisegruppe eller et optisk signal.
Fra proxy   Et stedfortredende objekt (*) som benyttes som startpunkt i stedet for det angitte startobjektet.
Til objekt  (målobjekt) er, avhengig av valgt metode, et målobjekt angitt av brukeren eller et objekt relatert til startobjektet 
            eller en serie bestående av alle påfølgende kompatible målobjekter som kan nås fra startobjektet. Dersom 
            startobjektet er et optisk signal så vil scriptet søke etter alle signaler som er relatert med forsignalert av 
            startsignalet. Dersom startobjektet er en hastighetsbalisegruppe så vil scriptet søket etter alle relaterte 
            balisegrupper som er målpunkt for bremsekurve fra det angitte startobjektet.
Til proxy   Et stedfortredende objekt (*) som benyttes i stedet for det valgte målobjektet.
Fra Km      Startobjektets 'Km', dvs 'ReferenceMileage' som er innsettingspunktets projeksjon på bestemmende spor, avrundet til 
            nærmeste hele meter.
Til Km      Målobjektets 'Km', dvs 'ReferenceMileage' som er innsettingspunktets projeksjon på bestemmende spor, avrundet til 
            nærmeste hele meter.
Km-diff     Absoluttverdien av differansen mellom Km for start- og målobjekt, i hele meter.
Målavst.    Absoluttverdien av differansen mellom ikke-avrundede ReferenceMileage verdier, som deretter avrundes til nærmeste 
            hele meter.
Fra moh.    Laveste skinnes høyde ved startobjektet eller dets proxy, avrundet til nærmeste centimeter.
Til moh.    Laveste skinnes høyde ved målobjektet eller dets proxy, avrundet til nærmeste centimeter.


MÅLAVSTAND måles langs den korteste stien fra startobjektets (*) posisjon i eget spor til målobjektets (*) posisjon i eget spor 
           og avrundes til nærmeste hele meter.
HØYDE-DIFFERANSE måles som startobjektets vertikalprofil (dvs eget spors laveste skinnes overkant) ved mellom startobjekt og
           målobjekt (negativ verdi angir fall).
GRADIENT er beregnes faktisk høyde-differanse, dividere med målavstand i eget spor avrundet til nærmeste heltall, deretter 
           sammenligne: 4.78 ‰ ==> 5 ‰ kodes, mens 4.47 ‰ ==> 4 ‰ kodes ikke.

(*): Et proxy-objekt (stedfortredende objekt) som benyttes i stedet for det valgte objektet. 
     - Alle signaler benytter egen posisjon (ikke isolert skjøt / akselteller men mastens egen posisjon)
     - Hovedsignal-balisegruppe benytter benytter hovedsignalets posisjon
     - Alle andre balisegrupper benytter A-balisens posisjon
]]


local rcCalculationMethodMsg =
[[
RailCOMPLETE BEREGNINGSMETODE - Endringsforslag til TRV pr desember 2025 fra C. Feyling
=========================================================================================

*** GENERELT OM GRADIENTER
RC avrunder slik at reell gradient avrundes til nærmeste heltall. Dvs. at et fall på 9.500..10.499 o/oo avrundes til 10 promille,
som igjen betyr at fallbalise skal benyttes med C-balise og verdi 10 promille i ATC kodetabell. Tilsvarende så vil 10.500-15.499
promille fall avrundes til hhv. 11/12/13/14/15 i ATC kodetabell (og bør angis slik i skjematisk tegning, uten desimaler i 
promille-angivelsen) og resulterer i ATC-koding tilsvarende tabellverdien for 15 promille. For FATC så vil et fall på 
0.500..1.499 o/oo resultere i avrundet verdi 1 promille som betyr at fallbalise skal benyttes med ATC-koding tilsvarende 
tabellverdien for 5 promille. Valg av målavstand-tabell for bremsekurve følger samme logikk: Beregne fall, avrunde til nærmeste
heltallige promille, deretter følger TRV bestemmelser som angitt over.

*** BEREGNING AV KILOMETER OG MÅLAVSTAND I RAILCOMPLETE
Kilometer 'Km' for et objekt finnes ved å reise normalen fra objektets naturlige referansepunkt på senterlinjen i bestemmende 
spor i XY-planet. Lengdeangivelsen, det vil si bestemmende spors profilkilometer målt i XY-planet, avrundes til næ rmeste hele 
meter og kalles objektets "Km". Målavstand 'MA' skal normalt angis som differansen mellom start-Km og slutt-Km, justert for 
eventuelle kjedebrudd. Dersom reell avstand 'MAreell' fra startpunkt til sluttpunkt, målt langs sporlinjen fra startpunkt til 
sluttpunkt og avrundet til næ rmeste hele meter, avviker mer enn 1,5% fra normalt beregnet MA, så skal avrundet MAreell benyttes 
i den videre beregningen. I skjematiske tegninger og i kodetabeller bør det da angis "Reell avstand" i en fotnote.

*** BEREGNING AV HØYDER OG HØYDEFORSKJELL I RAILCOMPLETE
Høyde over havet 'H' for et objekt finnes ved å reise normalen fra objektets naturlige referansepunkt på senterlinjen i 
bestemmende spor i XY-planet og der beregne sporhøyden (overkant laveste skinne) som den interpolerte høydeverdien mellom 
foregående og påfølgende høybrekkpunkt (HBP) / lavbrekkpunkt (LBP), uten å ta hensyn til eventuelle sirkelavrundinger i 
vertikalplanet, og avrundet tilnæ rmeste desimeter. Høyden angis ved behov i signaltegninger ved signaler, ved hastighetssignaler 
og ved høybrekkpunkter og / lavbrekkpunkter." Dersom reell høyde 'Hreell' avviker mer enn 25 cm fra H så skal Hreell, avrundet 
til næ rmeste hele desimeter, benyttes i den videre beregningen i skjematiske tegninger og kodetabeller. I skjematiske tegninger 
bør det da angis "Reell høyde" i en fotnote. Høydeforskjell 'dH' skal normalt angis som differansen mellom start-høyde og slutt-
høyde. Dersom reell høydeforskjell 'dHreell', avrundet til næ rmeste hele desimeter, avviker mer enn 25 cm fra normalt beregnet 
dH, så skal avrundet dHreell benyttes i den videre beregningen. 

*** BEREGNING AV GRADIENT I RAILCOMPLETE
Gradient 'G' skal normalt angis som høydeforskjellen dH (eller dHreell) dividert med målavstanden MA (eller MAreell), uttrykt i 
promille og avrundet til næ rmeste heltall. Dersom reell gradient 'Greell' fra startpunkt til sluttpunkt, avrundet til nærmeste 
hele promille, avviker fra G, så skal avrundet Greell benyttes i den videre beregningen. I skjematiske tegninger og i
kodetabeller bør det da angis "Reell gradient" i en fotnote. 
]]


local trvMsg = [[
UTDRAG FRA TRV
=========================================================================================

Se Bane NOR Teknisk Regelverk, Signalanlegg, ATC, TRV artikler for DATC: 06164-06165-06166 og for FATC: 06211-06212-06213.
Lenke: https://trv.banenor.no/wiki/Signal/Prosjektering/ATC

Bane NORs Teknisk Regelverk artikkel TRV:06212 angir at fallet skal beregnes fra startobjekt til sluttobjekt samt i de siste
2/3 av stien til målobjektet, og at det mest begrensende fallet skal benyttes i bremsekurver.

TRV:06164 (DATC)
a) Delvis utrustet område skal ikke benyttes ved linjehastighet >130 km/h.

TRV:06165 (DATC)
b) C-balise skal benyttes ved gjennomsnittlig fall ≥ 10 ‰.

TRV:06166 (DATC)
c) C-balise skal kodes i henhold til Tabell: Fall. Gjennomsnittlig fall (‰) over balisegruppens målavstand skal benyttes, 
   forhøyet til nærmeste 10, 15, 20 eller 25‰.
   Unntak: Dersom gjennomsnittlig fall på de siste 2/3 av balisegruppens målavstand er større enn gjennomsnittlig fall, skal 
   dette fallet benyttes, forhøyet til nærmeste 10, 15, 20 eller 25‰.

TRV:06211 (FATC)
a) Fullstendig utrustet område skal benyttes ved linjehastighet > 130 km/h.

TRV:06212 (FATC)
b) Alle balisegrupper som inneholder målavstand og som ikke ligger ved signaler skal plasseres i henhold til nedenstående 
   generelle formel for målavstand.
   Unntak: På Ofotbanen skal målavstand for hastighetsnedsettelser være ≥ 1000 m.

Formelen danner grunnlag for målavstandstabellene i vedlegg a. Formelen er basert på en bremsemodell hvor toget først kjører
en tid ved linjehastigheten, og deretter bremser med konstant retardasjon (negativ akselerasjon) som er 0,7m/s^2. Denne 
retardasjonen skal ligge til grunn for alle målavstander, utenom på Ofotbanen.

MA = (L/3,6 * T)  +  (L^2 - MH^2)/(2 * R * 3,6^2) [m],   hvor:

MA = målavstand [m]
L = linjehastighet [km/h] (største tillatte skiltede hastighet inkludert eventuell plusshastighet)
MH = målhastighet [km/h]
T = summen av reaksjonstid og tilsetningstid. For signalbalisegrupper benyttes T = 8 s, og for faste hastighetsbalisegrupper 
    benyttes T = 13 s
R = retardasjon = [-0,2 * (L-150)/150] - C/100 + 0,7 [m/s^{2}], der leddet i firkantparenteser bare brukes dersom L>150 km/h
C = fall i promille. Gjennomsnittlig fall over målavstanden skal benyttes, forhøyet til nærmeste 1, 5, 10, 15, 20 eller 25‰.

Unntak: Dersom gjennomsnittlig fall på de siste 2/3 av balisegruppens målavstand er større enn gjennomsnittlig fall, skal dette 
fallet benyttes, forhøyet til nærmeste 1, 5, 10, 15, 20 eller 25 ‰. Fall har positivt fortegn.

NOR TRV:06213 (FATC)
c) Fall. 
	1.	C-balise skal benyttes ved gjennomsnittlig fall ≥ 5 ‰.
	2.	(...) Gjennomsnittlig fall over balisegruppens målavstand skal benyttes, forhøyet til nærmeste 5, 10, 15, 20 eller 25‰.
	Unntak: Dersom gjennomsnittlig fall på de siste 2/3 av balisegruppens målavstand er større enn gjennomsnittlig fall, skal 
	dette fallet benyttes, forhøyet til  nærmeste 5, 10, 15, 20 eller 25 ‰.
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
	writeln("Målavstand "..targetDistance.." / avrundet gradient "..roundedGradient.." ==> BY="..BY.." BZ/CY="..BZ_CY)
	
	if (_IS_FATC_ and roundedGradient > -5) or (_IS_DATC_ and roundedGradient > -10) then
		return string.format("B(9,%d,%d)", BY, BZ_CY) --No C balise
	else
		CZ = gradientEncoding(roundedGradient)
		return string.format("B(9,%d,0) + C(14,%d,%d)", BY, BZ_CY, CZ)
	end
end


--TODO clfey 2025-12-12 FIKSE KODEN SLIK AT DEN FØLGER TRV ENDRINGSFORSLAGET
function distanceAndGradientToTarget(obj1, obj2, orig1, orig2)
	--Assume that there is a braking curve relationship from obj1 to obj2
	local h1, h2, h1RefId, h2RefId
	if useOwntrackElevation then
		h1real = obj1:getAlignmentInfo().Elevation
		h2real = obj2:getAlignmentInfo().Elevation
		--Compare to interpolated height between two adjacent PVIs:
		nearestVcs1 = obj1:getAlignmentInfo().NearestVerticalVertex
		nearestVcs2 = obj2:getAlignmentInfo().NearestVerticalVertex
		a = b
	else
		h1RefId = obj1:getAlignmentInfo().ReferenceAlignmentId
		h2RefId = obj2:getAlignmentInfo().ReferenceAlignmentId
		h1real = obj1:getAlignmentInfo(h1RefId).Elevation
		h2real = obj2:getAlignmentInfo(h2RefId).Elevation
	end
	
	if RC__isNan(getDistance(obj1,obj2)) then
		return "Ingen sti funnet fra objekt '"..RC__identify(obj1).."' til målobjekt '"..RC__identify(obj2).."'."
	end

	--There is a path from source to target
	local d = math.abs(RC__round(getDistance(obj1,obj2), 0))
	local pos1 = getAlignmentPos(obj1)
	local pos13 = getAlignmentPos(pos1.Ref, pos1.Pos + (obj1.dir == "up" and d/3 or -d/3)) --1/3 towards target
	local km1 = NOBN_trk_toKm(obj1.ReferenceMileage)
	local km2 = NOBN_trk_toKm(obj2.ReferenceMileage)
	local kmDiff = RC__round(1000*math.abs(km2-km1))
	local h13 = RC__round(getAlignmentInfo(pos13).Elevation,2)
	local gradient = 1000*(h2-h1)/d
	local gradient23 = 1000*(h2-h13)/(d*2/3)
	local effectiveGradient = gradient < gradient23 and gradient or gradient23
	local roundedGradient = RC__round(effectiveGradient) --to nearest integer, i.e. -5.45 ==> -5 etc, -7.89 ==> -8, +3.77 ==> 4 etc.
	local t = string.format(
		"%-15s %-15s %-15s %-15s "..
		"%-8.03f %-8.03f %-8.0f %-8.0f "..
		"%-8.02f %-8.02f %-8.02f %-8.02f %-8.02f "..
		"%-8.02f %-8.02f %-8.0f",
		RC__identify(orig1), RC__identify(obj1), RC__identify(orig2), RC__identify(obj2),
		km1, km2, kmDiff, d,
		h1, h2, (h2-h1), h13, (h2-h13),
		gradient, gradient23, roundedGradient)
	return {["Text"] = t, ["TargetDistance"] = kmDiff, ["RoundedGradient"] = roundedGradient}
end



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



--===============================================================================================================================
--
-- M A I N   M E N U
--
--===============================================================================================================================
local tSingleTarget = "Velg spesifikt start- og sluttobjekt, finn tilhørende målavstand og gradient"
local tRelatedTargets = "Velg startobjekt, finn målavstand og gradient til alle dets relaterte målobjekter"
local tFindAllTargets = "Velg startobjekt, finn målavstand og gradient ved å søke etter potensielle målobjekter"
local tOutputFormat = "Vis output-format"
local tRcCalculationMethod = "Vis RailCOMPLETEs beregningsmetode (TRV endringsforslag)"
local tBaneNorTrv = "Vis utdrag fra Bane NOR Teknisk Regelverk"
local tAbout = "About"
local tQuit = "Avslutt"
local mode
repeat 
	mode = askForKeyword(usageMsg, {tSingleTarget, tRelatedTargets, tFindAllTargets, tOutputFormat, tRcCalculationMethod, tBaneNorTrv, tAbout, tQuit}, "Avstand og gradient til målpunkt", false,  FontType.Proportional)
	if mode == tOutputFormat then
		writeln(outputFormatMsg)
		showMessage(outputFormatMsg, "OUTPUT-FORMAT - "..tWindowTitle, FontType.Proportional, false)
	elseif mode == tRcCalculationMethod then
		writeln(rcCalculationMethodMsg)
		showMessage(rcCalculationMethodMsg, "BEREGNINGSMETODE - "..tWindowTitle, FontType.Proportional, false)
	elseif mode == tBaneNorTrv then
		writeln(trvMsg)
		showMessage(trvMsg, "TRV UTDRAG - "..tWindowTitle, FontType.Proportional, true)
	elseif mode == tAbout then
		writeln(aboutMsg)
		showMessage(aboutMsg, "ABOUT - "..tWindowTitle, FontType.Proportional, false)
	elseif mode == tQuit then
		stop()
	end
until mode == tSingleTarget or mode == tRelatedTargets or mode == tFindAllTargets or mode == tQuit
writeln("Avstand og gradient til målpunkt")



--=================================================================================================================================
--
-- M A I N   L O O P
--
--=================================================================================================================================
local finished = false
local caption =	
	"Fra objekt      Fra proxy       Til objekt      Til proxy       Fra Km   Til Km   Km-diff  Målavst. Fra moh  Til moh  H-diff   H2/3moh  H2/3dif  Gradient Gr.2/3   Stigning Balisekoding        \n"..
	"--------------- --------------- --------------- --------------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- ------------------- "
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
			show(caption.."\n"..u1..u2)

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
					t = t and t.."\n"..u1.." "..u2 or u1.." "..u2
				end
				show(caption.."\n"..t)
			end
		end
		writeln()
	end
until finished
writeln("Bye.")
