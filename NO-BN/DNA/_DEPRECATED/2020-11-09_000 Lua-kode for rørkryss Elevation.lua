function _KU_KFOE_Package_Offset3dZ(cursor)
	local zBelowTopOfRail = 0.9 --[m]
	local m1 = getNearbyPointObjects2D(RcAlignment.StartPoint, 2.5, "KU-KFØ Trekkekum")
	local m2 = getNearbyPointObjects2D(RcAlignment.EndPoint, 2.5, "KU-KFØ Trekkekum")
	local z1 = m1 and m1[0].geoCoord.Z or getClosestTracks()[0]:getAlignmentInfo(RcAlignment.StartPoint).geoCoord.Z - zBelowTopOfRail
	local z2 = m2 and m2[0].geoCoord.Z or getClosestTracks()[0]:getAlignmentInfo(RcAlignment.EndPoint).geoCoord.Z - zBelowTopOfRail
	local p = cursor.pos
	local L = RcAlignment.HorizontalProfile.Length
	local Z = ((z2-z1)/L)*p + z1 - zBelowTopOfRail
	return Z
end
return _KU_KFOE_Package_Offset3dZ(_position)