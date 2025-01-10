--[[
	Pset2021a.lua
	=============
	2024-11-21_000 CLFEY Extracted Pset automation functions from DNA 2021.a patch 10.

	Usage: The calling Pset definition's property must include this file and access the appropriate functions as 'funcTable.funcName()'.
	Note: 'funcTable.funcName()' is syntactic sugar for 'funcTable["funcName"]()' as long as 'funcName' does not contain whitespace.
	
	Example:
		local pset = includeLuaFile("..\\NO-BN\\Lua\\Functions\\Pset2021a.lua")
		return pset.NOBN_com_FDVBaneDataObjektID()
		
	or shorter:
	includeLuaFile("..\\NO-BN\\Lua\\Functions\\Pset2021a.lua").NOBN_com_Pset_BaneNORBaneDataID()
	
	Functions for Pset 'Bane NOR' (Lua name 'Bane_NOR'):
		NOBN_com_Pset_BaneNORBaneDataID
		NOBN_com_Pset_BaneNORRives
		NOBN_com_Pset_BaneNORRivesFase
		NOBN_com_Pset_BaneNORByggesFase
		
	Functions for Pset 'FDV BaneData' (Lua name FDV_BaneData'):
		NOBN_com_FDVBaneDataObjektID
		NOBN_com_FDVBaneDataBeskrivelse
		NOBN_com_FDVBaneDataReferansesporFra
		NOBN_com_FDVBaneDataKmFra
		NOBN_com_FDVBaneDataSideFra
		NOBN_com_FDVBaneDataSportypeFra
		NOBN_com_FDVBaneDataSporNrFra
		NOBN_com_FDVBaneDataAvstSpormidtFra
		NOBN_com_FDVBaneDataNordFra
		NOBN_com_FDVBaneDataOestFra
		NOBN_com_FDVBaneDataHoeydeFra
--]]



function NOBN_com_Pset_BaneNORBaneDataID()
	local usage =
	"Slett denne formelen og legg inn korrekt Banedata Objekt-ID manuelt eller via fletting i Object Manager.\n\n"..
	"Avledet fra property 'RC Type' (RcType). Returnerer BaneData objekt-ID, på formen 'ff-ttt-nnnnnn' der ff er en av [FE|KU|KO|EH|SA|TE|EL], ttt er 3-bokstavers BaneData\n".. "objekttypekode innenfor fagkoden, nnnnnn er et unikt nummer i innenfor angitt fag og objekttype. Forklaring til fagkoder:\n\n"..
	"FE : felles (markører, tabeller, områder m.m.)\n"..
	"KU : konstruksjon underbygning (terreng, fundamenter, føringsveier, broer, veier, bygninger, drenering)\n"..
	"KO : konstruksjon overbygning (ballast, spor, sporobjekter, plattformer)\n"..
	"EH : elektro høyspent (KL, jording, kjørestrøm)\n"..
	"SA : signalanlegg\n"..
	"TE : tele\n"..
	"EL : elektro lavspent.\n"..
	"\n"..
	"Skilt er fordelt på alle disipliner. Merk at fagkode FE er ikke en offisiell BaneData fagkode. Funksjonen henter informasjon fra objektets egenskap 'RcType'.\n\n"..
	"RcType må være på formen 'JBTff_ttt Objekttypenavn' for å gjenkjennes feilfritt.\n\n"
	--Expected RcType format: 'JBTff_ttt ObjectTypeName'
	a, b = RcType:upper():match("^JBT(%a%a)_(%a%a%a)")
	if a and b then 
		return string.format("%s-%s-%06d",a:upper(),b:upper(),0),
			_info(usage)
	else
		return _warning, "Ukjent", 
			_info(usage.."RcType har ikke forventet format 'JBTkk_ttt-dddddd <objekttypenavn>' - slett denne formelen og skriv inn Objekt-ID manuelt på korrekt format.")
	end
end



function NOBN_com_Pset_BaneNORRives()
	local usage = 
	"Avledet fra property 'Fasekode XXxx-YYyy' (Stage). Returnerer 'Ja' dersom fasekoden har en har Ut-mot-fase lavere enn 9999, som representerer 'før prosjektavslutning'.\n"..  "Returnerer 'Nei' dersom dersom fasekoden har Ut-mot-fase lik 9999, som representerer 'etter prosjektavslutning'. Returnerer 'Ukjent' dersom fasekoden ikke har gyldig\n"..
	"format 'XXxx-YYyy'.\n\n"
	YYyy = tostring(Stage):match("^%d%d%d%d%-(%d%d%d%d)$")
	if YYyy == nil then 
	return _warning, 'Ukjent', 
		_info(usage.."WARNING - Ugyldig fasekode i 'Fasekode XXxx-YYyy' (Stage), kan ikke avgjøre om objektet skal rives.")
	else
		if YYyy == '9999' then
			return 'Nei'
		else
			return 'Ja'
		end
	end
end



function NOBN_com_Pset_BaneNORRivesFase()
	local usage =
	"Avledet fra property 'Fasekode XXxx-YYyy' (Stage). Returnerer Ut-mot-Fase YYyy, der YYyy angir fasen der objektet først fremstår som fjernet. Returnerer '---' dersom\n".. "objektet aldri rives. Returnerer 'Ukjent' dersom fasekoden ikke har gyldig format 'XXxx-YYyy' eller dersom ingen faseinformasjon er angitt.\n\n"
	YYyy = tostring(Stage):match("^%d%d%d%d%-(%d%d%d%d)$")
	if YYyy == nil then
		return _warning, 'Ukjent', 
			_info(usage.."WARNING - Ugyldig fasekode i 'Fasekode XXxx-YYyy' (Stage), kan ikke avgjøre når objektet rives.")
	else
		if YYyy == 9999 then
			return "---"
		else
			return string.format("%02d.%02d",YYyy//100, YYyy%100)
		end
	end
end


			
function NOBN_com_Pset_BaneNORByggesFase()
	local usage =
	"Avledet fra property 'Fasekode XXxx-YYyy' (Stage). Returnerer Inn-i-fase XXxx, der XXxx angir fasen som objektet første gang eksisterer i. Returnerer 'Ukjent' dersom\n".. "fasekoden ikke har gyldig format 'XXxx-YYyy' eller dersom ingen faseinformasjon er angitt.\n\n"
	XXxx = tostring(Stage):match("^.*(%d%d%d%d)%-%d%d%d%d.*")
	if XXxx == nil then 
		return _warning, 'Ukjent',
			_info(usage.."WARNING - Ugyldig fasekode i 'Fasekode XXxx-YYyy' (Stage), kan ikke avgjøre når objektet bygges.")
	else
		return string.format("%02d.%02d",XXxx//100, XXxx%100)
	end
end



function NOBN_com_FDVBaneDataObjektID()
	local usage = 
	"Returnerer Pset property 'Bane_NOR.BaneData_ID' og utfører en formatkontroll 'ff-ttt-nnnnnn'.\n\n"
	a, b, c = Bane_NOR.BaneData_ID:upper():match("^(%a%a)%-(%a%a%a)%-(%d%d%d%d%d%d)$")
	if a and b and c then 
		--Syntax OK, check semantics:
		if a == "KU" or a == "KO" or a == "EH" or a == "SA" or a == "TE" or a == "EL" then
			if tonumber(c) == 0 then
				return _warning, string.format("%s-%s-%06d",a:upper(),b:upper(),tonumber(c)),
					_info(usage.."Objekt-ID format er syntaktisk korrekt, men nummer kan ikke være null.")
			else
				return string.format("%s-%s-%06d",a:upper(),b:upper(),tonumber(c)),
					_info(usage.."Objekt-ID format er syntaktisk korrekt, men unikhet er ikke kontrollert her.")
			end
		elseif a == "FE" then
			return _warning, string.format("%s-%s-%06d",a:upper(),b:upper(),tonumber(c)),
				_info(usage.."Objekt-ID av kategori 'FE-xxx-nnnnnn' må avklares med Bane NOR ved levering til BaneData.")
		else
			return _warning, string.format("%s-%s-%06d",a:upper(),b:upper(),tonumber(c)),
				_info(usage.."Objekt-ID av ukjent kategori må avklares med Bane NOR ved levering til BaneData.")
		end
	else
		return _warning, "Formatfeil", _info(usage.."Pset property 'Bane_NOR.BaneDataID' må korrigeres, se denne.")
	end
end



function NOBN_com_FDVBaneDataBeskrivelse()
	local usage = "Returnerer en beskrivelse, hentet fra Pset property 'Bane_NOR.Beskrivelse'. Verdien kan være blank (tomt felt).\n\n"
	return Bane_NOR.Beskrivelse, _info(usage)
end



function NOBN_com_FDVBaneDataReferansesporFra()
	local usage =
	"Returnerer referansespor for punktobjekt, eller referansespor i linjeobjekts startpunkt. MERK: For at referansespor-Km skal kunne utledes korrekt så må (1) absolutt\n"..
	"alle punktobjekter tilhøre en egen linje (property 'Egen linje' (Alignment), og (2) absolutt alle linjer må definere hvilket referansespor de avhenger av. For å\n"..
	"definere et linjeobjekts referansespor: (a) Start kommando 'RC-AlignmentManager' (b) Åpne flik 'Reference Alignment' (c) Aktiver editeringsmodus (d) Selekter\n".. "linjeobjektet (e) Editer radene i tabellvisningen. Dersom tabellen er tom så er sporet sin egen referanse (og kan være referanse for andre linjer). Hver rad i tabellen\n"..
	"angir et startsted i linjen samt hvilken linje som er dens referansespor herfra. Angivelsen gjelder fram til neste rads startpunkt, eller til linjens ende.\n\n"
	if RcAlignment then 
		--Linjeobjekt:
		x = RcAlignment.StartPoint.X
		y = RcAlignment.StartPoint.Y
		refId = getAlignmentInfo(this.id, x, y).ReferenceAlignmentId
		if getObjectFromId(refId).RcType ~= "JBTKO_SPO Spor" and getObjectFromId(refId).RcType ~= "JBTFE_HJL Hjelpelinje" then
			return _warning, (getObjectFromId(refId).name or getObjectFromId(refId).code or ""),
				_info(usage.."Referanselinjen "..RC__identify(getObjectFromId(refId)).." som er angitt i linjeobjektets startpunkt må ha RcType 'JBTKO_SPO Spor' eller 'JBTFE_HJL Hjelpelinje'.")
		else
			if refId == this.id then
				return (getObjectFromId(refId).name or getObjectFromId(refId).code or ""), _info(refId).." (self)"
			else
				return (getObjectFromId(refId).name or getObjectFromId(refId).code or ""), _info(refId)
			end
		end
	elseif Alignment then 
		--Punktobjekt som tilhører en linje:
		x = geoCoord.X
		y = geoCoord.Y
		if not getAlignmentInfo(Alignment.id, x, y).NormalProjectionExists then
			return _warning, "Ukjent",
			_info(usage.."Punktobjektet har ingen projeksjon på 'Egen linje' (Alignment), kan ikke utlede hvilken referanselinje som er gjeldende.")
		end
		refId = getAlignmentInfo(Alignment.id, x, y).ReferenceAlignmentId
		if getObjectFromId(refId).RcType ~= "JBTKO_SPO Spor" and getObjectFromId(refId).RcType ~= "JBTFE_HJL Hjelpelinje" then
			return _warning, refId.." / "..(getObjectFromId(refId).name or getObjectFromId(refId).code or ""),
				_info(usage.."Punktobjektets referanselinje "..RC__identify(getObjectFromId(refId)).." må ha RcType 'JBTKO_SPO Spor' eller 'JBTFE_HJL Hjelpelinje'.")
		else
			return (getObjectFromId(refId).name or getObjectFromId(refId).code or ""), _info(refId)
		end
	else
		--Punktobjekt som ikke tilhører en linje:
		return _warning, "Ukjent", 
			_info(usage.."Punktobjektet mangler angivelse av 'Egen linje' (Alignment), kan ikke utlede hvilken referanselinje som er gjeldende.")
	end
end



function NOBN_com_FDVBaneDataKmFra()
	local usage =
	"Returnerer 'Km' (ReferenceMileage). Punktobjekt: objektets Km målt som projeksjon på referanselinjen. Linjeobjekt: Startpunktets Km målt som projeksjon\n"..
	"på linjens referansespor.\n\n"
	if RcAlignment then 
		--Linjeobjekt:
		x = RcAlignment.StartPoint.X
		y = RcAlignment.StartPoint.Y
		refId = getAlignmentInfo(this.id, x, y).ReferenceAlignmentId
		ai = getAlignmentInfo(refId, x, y)
		if ai.NormalProjectionExists then
			return string.format("%.03f",ai.Mileage/1000,3), 
				_info(usage.."Ref. "..RC__identify(getObjectFromId(refId)))
		else
			return _warning, "Ukjent",
				_info(usage.."Linjeobjektets startpunkt har ikke en projeksjon på referanselinjen "..RC__identify(getObjectFromId(refId))..", kan ikke utlede Km.")
		end
	elseif Alignment then 
		--Punktobjekt som tilhører en linje:
		x = geoCoord.X
		y = geoCoord.Y
		if not getAlignmentInfo(Alignment.id, x, y).NormalProjectionExists then
			return _warning, "Ukjent",
				_info(usage.."Punktobjektet har ingen projeksjon på 'Egen linje' (Alignment), kan ikke utlede Km.")
		end
		refId = getAlignmentInfo(Alignment.id, x, y).ReferenceAlignmentId
		ai = getAlignmentInfo(refId, x, y)
		if ai.NormalProjectionExists then
			return string.format("%.03f",ai.Mileage/1000,3), 
				_info(usage.."Ref. "..RC__identify(getObjectFromId(refId)))
		else
			return _warning, "Ukjent",
				_info(usage.."Punktobjektet har ikke en projeksjon på referanselinjen "..RC__identify(getObjectFromId(refId))..", kan ikke utlede Km.")
		end
	else
		--Punktobjekt som ikke tilhører en linje:
		return _warning, "Ukjent", 
				_info(usage.."Punktobjektet mangler angivelse av 'Egen linje' (Alignment), kan ikke utlede Km.")
	end
end



function NOBN_com_FDVBaneDataSideFra()
	local usage =
	"Returnerer side av spor [Ukjent | Senter | Venstre | Høyre]. Punktobjekt: side i forhold til eget spor, eller side av nærmeste spor dersom objektet ikke tilhører\n"..
	"et bestemt spor. Linjeobjekt: startpunktets side av referanselinjen. Merk: 'Ukjent' er ikke en gyldig kategori ved levering til BaneData.\n\n"
	eps = 4e-4
	if RcAlignment then 
		--Linjeobjekt:
		x = RcAlignment.StartPoint.X
		y = RcAlignment.StartPoint.Y
		refId = getAlignmentInfo(this.id, x, y).ReferenceAlignmentId
		ai = getAlignmentInfo(refId, x, y)
		if ai.NormalProjectionExists then
			d = ai.DistanceToAlignment
			t = (math.abs(d) < eps) and "Senter" or (d < 0) and "Venstre" or "Høyre"
			return t, _info(t.." i forhold til referansespor "..RC__identify(getObjectFromId(refId))..".")
		else
			return _warning, "Ukjent",
				_info(usage.."Linjeobjektets startpunkt har ikke en projeksjon på referanselinjen "..RC__identify(getObjectFromId(refId))..", kan ikke utlede side av spor.")
		end
	elseif Alignment then 
		--Punktobjekt som tilhører en linje:
		x = geoCoord.X
		y = geoCoord.Y
		if not getAlignmentInfo(Alignment.id, x, y).NormalProjectionExists then
			return _warning, "Ukjent",
				_info(usage.."Punktobjektet har ingen projeksjon på 'Egen linje' (Alignment), kan ikke utlede side av spor.")
		end
		if Alignment.RcType == "JBTKO_SPO Spor" then
			--Punktobjekt som tilhører et jernbanespor:
			d = getAlignmentInfo(Alignment.id, x, y).DistanceToAlignment
			t = (math.abs(d) < eps) and "Senter" or (d < 0) and "Venstre" or "Høyre"
			return t, _info(t.." i forhold til eget spor "..RC__identify(Alignment))
		else
			--Punktobjekt som tilhører en linje som ikke er et jernbanespor:
			refId = getAlignmentInfo(Alignment, x, y).ReferenceAlignmentId
			ai = getAlignmentInfo(refId, x, y)
			if ai.NormalProjectionExists then
				d = ai.DistanceToAlignment
				t = (math.abs(d) < eps) and "Senter" or (d < 0) and "Venstre" or "Høyre"
				return t, _info(t.." i forhold til referanselinje "..RC__identify(getObjectFromId(refId))).."."
			else
				return _warning, "Ukjent",
					_info(usage.."Punktobjektet har ikke en projeksjon på sin referanselinje "..RC__identify(getObjectFromId(refId))..", kan ikke utlede side av spor.")
			end
		end
	else
		--Punktobjekt som ikke tilhører en linje:
		return _warning, "Ukjent", 
			_info(usage.."Punktobjektet mangler angivelse av property 'Egen linje' (Alignment), kan ikke utlede side av spor.")
	end
end



function NOBN_com_FDVBaneDataSportypeFra()
	local usage =
	"Identifiserer eget spors anvendelsestype [Ukjent | Hovedspor | Høyre hovedspor | Sidespor | Venstre hovedspor | Overkjøringsspor | Togspor | Øvrige spor].\n"..
	"Sporbjekter: sporets anvendelsestype. Øvrige linjeobjekter: nærmeste spors anvendelsestype ved linjens startpunkt. Punktobjekter: eget spors anvendelsestype,\n"..
	"evt. nærmeste spors anvendelsestype dersom objektet ikke tilhører et bestemt spor. Merk: 'Ukjent' er ikke en gyldig kategori ved levering til BaneData.\n\n"
	if RcAlignment and RcType == "JBTKO_SPO Spor" then 
		--Linjeobjekt av type jernbanespor:
		--(Use getPropertyValue() to avoid an endless loop, since also tracks will be assigned this formula)
		return FDV_BaneData:getPropertyValue("Sportype_fra_")

	elseif Alignment and Alignment.RcType == "JBTKO_SPO Spor" then 
		--Punktobjekt i jernbanespor:
		return Alignment.FDV_BaneData.Sportype_fra_

	else
		--Ingen direkte angivelse av sportilhørighet:
		if RcAlignment then
			--Linjetype av annen type enn jernbanespor:
			x = RcAlignment.StartPoint.X
			y = RcAlignment.StartPoint.Y
		else
			--Punktobjekt i annen linjetype enn jernbanespor:
			x = geoCoord.X
			y = geoCoord.Y
		end
		t = getClosestTracks(x, y)
		n = getCollectionLength(t)
		--Exploit the fact that closest tracks are sorted by proximity:
		for i = 0, n-1 do
			ai = getAlignmentInfo(t[i].id, x, y)
			if ai.NormalProjectionExists then  
				return t[i].FDV_BaneData.Sportype_fra_
			end
		end
		--Fant ingen spor vi kunne projisere oss inn på:
		if RcAlignment then 
			return _warning, "Ukjent", 
				_info(usage.."Linjens startpunkt har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede tilhørende spors anvendelsestype.")
		else
			return _warning, "Ukjent", 
				_info(usage.."Punktobjektet har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede tilhørende spors anvendelsestype.")
		end
	end
end



function NOBN_com_FDVBaneDataSporNrFra()
	local usage =
	"Identifiserer sportilhørighet:\n\n"..
	"Sporbjekter: sporets navn/kode/id.\n"..
	"Øvrige linjeobjekter: nærmeste spors navn/kode/id ved linjens startpunkt.\n"..
	"Punktobjekter: eget spors navn/kode/id, eller til nærmeste spors navn/kode/id dersom objektet ikke tilhører et bestemt spor.\n\n"
	if RcAlignment and RcType == "JBTKO_SPO Spor" then 
		--Linjeobjekt av type jernbanespor:
		return RC__identify(this)

	elseif Alignment and Alignment.RcType == "JBTKO_SPO Spor" then 
		--Punktobjekt i jernbanespor:
		return RC__identify(Alignment)
		
	else
		--Ingen direkte angivelse av sportilhørighet:
		if RcAlignment then
			--Linjetype av annen type enn jernbanespor:
			x = RcAlignment.StartPoint.X
			y = RcAlignment.StartPoint.Y
		else
			--Punktobjekt i annen linjetype enn jernbanespor:
			x = geoCoord.X
			y = geoCoord.Y
		end
		t = getClosestTracks(x, y)
		n = getCollectionLength(t)
		--Exploit the fact that closest tracks are sorted by proximity:
		for i = 0, n-1 do
			ai = getAlignmentInfo(t[i].id, x, y)
			if ai.NormalProjectionExists then  
				return RC__identify(t[i])
			end
		end
		--Fant ingen spor vi kunne projisere oss inn på:
		if RcAlignment then 
			return _warning, "Linjens startpunkt har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede sportilhørighet."
		else
			return _warning, "Punktobjektet har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede sportilhørighet."
		end
	end
end



function NOBN_com_FDVBaneDataAvstSpormidtFra()
	local usage =
	"Retunerer vinkelrett avstand i meter til eget spor. Sporbjekter: Alltid null. Punktobjekter: vinkelrett avstand til eget spor eller til nærmeste spor\n"..
	"dersom objektet ikke tilhører et bestemt spor. Øvrige linjeobjekter: vinkelrett sideveis avstand fra linjens startpunkt til nærmeste spor.\n\n"
	if RcAlignment and RcType == "JBTKO_SPO Spor" then 
		--Linjeobjekt av type jernbanespor:
		return string.format("%.03f",0.000), _info(usage.."Null avstand til seg selv, spor "..RC__identify(this))

	elseif Alignment and Alignment.RcType == "JBTKO_SPO Spor" then 
		--Punktobjekt i jernbanespor:
		return string.format("%.03f",RC__round(DistanceToAlignment)), _info(usage.."Avstand til spor "..RC__identify(Alignment))
		
	else
		--Ingen direkte angivelse av sportilhørighet:
		if RcAlignment then
			--Linjetype av annen type enn jernbanespor:
			x = RcAlignment.StartPoint.X
			y = RcAlignment.StartPoint.Y
		else
			--Punktobjekt tilhørende en annen linjetype enn jernbanespor:
			x = geoCoord.X
			y = geoCoord.Y
		end
		t = getClosestTracks(x, y)
		n = getCollectionLength(t)
		--Exploit the fact that closest tracks are sorted by proximity:
		for i = 0, n-1 do
			ai = getAlignmentInfo(t[i].id, x, y)
			if ai.NormalProjectionExists then  
				return string.format("%.03f",RC__round(ai.DistanceToAlignment)), _info(usage.."Avstand til spor "..RC__identify(t[i]))
			end
		end
		--Fant ingen spor vi kunne projisere oss inn på:
		if RcAlignment then 
			return _warning, 0, _info(usage.."Linjens startpunkt har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede avstand sideveis til noe spor.")
		else
			return _warning, 0, _info(usage.."Punktobjektet har ikke en projeksjon på noen av sporene i modellen, kan ikke utlede avstand sideveis til noe spor.")
		end
	end
end



function NOBN_com_FDVBaneDataNordFra()
	local usage =
	"Returnerer nord-koordinat. Punktobjekt: Y-koordinat i gjeldende horisontalt datum. Linjeobjekt: startpunktets Y-koordinat i gjeldende horisontalt datum.\n\n"
	if RcAlignment then
		--Alignment object:
		return RC__round(RcAlignment.StartPoint.Y,3), _info(usage.."Nord (Y) i kartprojeksjon "..DocumentData.CoordinateSystem)
	else
		--Point object:
		return string.format("%.3f",geoCoord.Y), _info(usage.."Nord (Y) i kartprojeksjon "..DocumentData.CoordinateSystem)
	end
end



function NOBN_com_FDVBaneDataOestFra()
	local usage =
	"Returnerer øst-koordinat. Punktobjekt: X-koordinat i gjeldende horisontalt datum. Linjeobjekt: startpunktets X-koordinat i gjeldende horisontalt datum.\n\n"
	if RcAlignment then
		--Alignment object:
		return RC__round(RcAlignment.StartPoint.X,3), _info(usage.."Øst (X) i kartprojeksjon "..DocumentData.CoordinateSystem)
	else
		--Point object:
		return string.format("%.3f",geoCoord.X), _info(usage.."Øst (X) i kartprojeksjon "..DocumentData.CoordinateSystem)
	end
end



function NOBN_com_FDVBaneDataHoeydeFra()
	local usage =
	"Returnerer høyde over midlere havnivå. Punktobjekt: Z-koordinat i gjeldende vertikalt datum. Linjeobjekt: startpunktets Z-koordinat i gjeldende vertikalt datum.\n\n"
	if RcAlignment then
		--Alignment object:
		return RC__round(RcAlignment.StartPoint.Z,3), _info(usage.."Høyde over midlere havnivå (Z) i vertikalt datum "..DocumentData.CoordinateSystem)
	else
		--Point object:
		return string.format("%.3f",geoCoord.Z), _info(usage.."Høyde over midlere havnivå (Z) i vertikalt datum "..DocumentData.CoordinateSystem)
	end
end

