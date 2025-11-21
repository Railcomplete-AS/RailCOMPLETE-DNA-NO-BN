function writeln(s) write((s or "").."\n") end

writeln()

writeln("***************************************************")
writeln("(You must click in modelspace to start dialog)")
writeln("Select first cantilever, then this script will strech the contact wire from here to the end of the relation sequence 'Next cantilever'.")
thisClv = askForObject("Select first cantilever")
writeln(RC__identify(clv).." was selected.")

segments = {}
s1 = "Possède pince fil_de_contact"
s2 = "Suivant armement"
repeat
	r1,n1 = RC__getCollectionOfRelatedObjects(s1,thisClv)
	if n1 > 0 then
		P1 = r1[0].geoCoord
	else
		writeln("Cantilever "..RC__identify(thisClv).." n'a pas de pince_fil_de_contact relié par '"..s1.."'.")
		return
	end
	r2,n2 = RC__getCollectionOfRelatedObjects(s2,thisClv)
	if n2 > 0 then
		P2 = r2[0].geoCoord
		table.insert(segments,createLineSegment(P1,P2))
	end
until n2 == 0


end
	runCommand("RC-Exporter3dAvecParametresActuels ")
	
else
	writeln("Nothing to do.")
end
writeln("***************************************************")
setSelectionSet(clv)

-- 305168.389 6703289.093 8.788
--7ac7e56d-df42-476d-8715-4f36f3dbbcc5