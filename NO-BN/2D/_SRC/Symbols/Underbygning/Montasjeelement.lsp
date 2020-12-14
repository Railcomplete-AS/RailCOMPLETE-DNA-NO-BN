;
; BEHOP/CLFEY 2017-02-15
; Ref. http://euroskilt.no/oppsetningsutstyr
;
(defun C:MONTASJEELEMENT ()
	(newLayer "JBTUB__FE_FUN_LOKALISERING" 62 "Fundamentlokalisering")
	(newLayer "JBTUB__FE_FUN_TYPENAVN" 62 "Fundament og montasjeelement type")
	(Montasjeelement_Betong_60_225 "500")
	(Montasjeelement_Betong_60_225 "700")
	(Montasjeelement_Betong_60_200_1200)
	(Montasjeelement_Staal_Jordfundament_60_700)
	(Montasjeelement_Staal_Jordspyd_60)
	(Montasjeelement_Staal_Veggfeste_60_110)
	(Montasjeelement_Staal_Universalfeste_Spesial_60_308)
	(Montasjeelement_Staal_Brakett_Paa_Annen_Mast)
	(Montasjeelement_Tilbakefylt_Grop)
	(Montasjeelement_Strips_Eller_Patentbaand)
	(STOLPE-S-LAAS "ENKEL")
	(STOLPE-S-LAAS "DOBBEL")
	(STOLPE-LOKALSTILLER)
	(STOLPE-MED-SKAP-OG-KABEL "1600")
	(STOLPE-MED-SKAP-OG-KABEL "2000")
	(ROERMAST-MED-LJERN)
)



(defun Montasjeelement_Betong_60_225 (spec / s1 s2 x d radius)
	(setq
		blockName (strcat "NO-BN-2D-JBTUB-MONTASJEELEMENT-BETONG-60-225-" spec)
		s1 (/ 0.1500 2)
		s2 (/ 0.2550 2)
		x 0.2429
		d 0.12
		radius 0.03
	)
	(command 
		"._LINE" (list s2 0) (list s2 s1) (list s1 s2) (list 0 s2) ""
		"._LINE" (polar  (list x x) (D->R -45) d) (polar  (list x x) (D->R (+ 90 45)) d) ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "N"
		"._MIRROR" "ALL" "" "0,0" "1,0" "N"
		"._CIRCLE" "0,0" radius
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(if (= spec "700")
		(addMText "BETONG FUNDAMENT Ø60/H700" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
		(addMText "BETONG FUNDAMENT Ø60/H500" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	)
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun Montasjeelement_Betong_60_200_1200 (/ blockName ang1 d_ang R1 R2 rad1 rad2)
	(setq
		blockName "NO-BN-2D-JBTUB-MONTASJEELEMENT-BETONG-60-200-1200"
		ang1 5.0
		d_ang 45.0
		R1 0.40
		R2 0.44
		rad1 0.03
		rad2 0.10
	)
	(command
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R ang1) R1) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R ang1) R2) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 90)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 90)) R2) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 180)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 180)) R2) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 270)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (+ ang1 270)) R2) "Angle" (rtos d_ang)
		"._CIRCLE" "0,0" rad1
		"._CIRCLE" "0,0" rad2
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG FUNDAMENT Ø60/H1200" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun Montasjeelement_Staal_Jordfundament_60_700 (/ blockName rad1 rad2 cos45)
	(setq
		blockName "NO-BN-2D-JBTUB-MONTASJEELEMENT-STAAL-JORDFUNDAMENT-60-700"
		rad1 0.03
		rad2  0.181
		cos45 (cos (D->R 45))
	)
	(command 
		"._CIRCLE" "0,0" rad1
		"._LINE" (list (* rad1 cos45) (* rad1 cos45)) (list (* rad2 cos45) (* rad2 cos45)) ""
		"._ARRAY" "L" "" "PO" "0,0" 4 360 "Y"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "JORDFUNDAMENT Ø60/H700" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun Montasjeelement_Staal_Jordspyd_60 ()
	(command "._CIRCLE" "0,0" 0.03)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "JORDSPYD Ø60/H600" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock "NO-BN-2D-JBTUB-MONTASJEELEMENT-STAAL-JORDSPYD-60")
	blockName
)



(defun Montasjeelement_Staal_Veggfeste_60_110 (/ blockName wingStart wingLength wingIncrY)
	(setq
		blockName "NO-BN-2D-JBTUB-MONTASJEELEMENT-STAAL-VEGGFESTE-60-110"
		wingStart 0.25
		wingLength 1.13
		wingIncrY 0.05
	)
	(command  ; Three "eagle wings" to each side of insertion point:
		"._LINE" (list wingStart 0) (list (+ wingStart wingLength) 0) ""
		"._LINE" (list wingStart wingIncrY) (list (+ wingStart wingLength) wingIncrY) ""
		"._LINE" (list wingStart (- wingIncrY)) (list (+ wingStart wingLength) (- wingIncrY)) ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "N"
	)
	(command ; Outline of wall-mount bracket (for boards etc Ø60):
		"._LINE" (list (- (/ wingStart 2)) 0) (list (/ wingStart 2) 0) ""
		"._RECTANGLE" 
			(list (- (/ wingStart 4)) 0)
			(list (/ wingStart 4) (- (/ wingStart 2)))
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "VEGGFESTE Ø60/110" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun Montasjeelement_Staal_Universalfeste_Spesial_60_308 (/ blockName ang1 d_ang R1 R2 rad1 rad2)
	(setq
		blockName "NO-BN-2D-JBTUB-MONTASJEELEMENT-STAAL-UNIVERSALFESTE-SPESIAL-60-308"
		rad1 0.03
		rad2  0.007
		pos 0.082
		side 0.255
	)
	(command
		"._CIRCLE" "0,0" rad1
		"._RECTANG" 
			(list (/ side -2) (/ side -2))
			(list (/ side 2) (/ side 2))
			""
		"._CIRCLE" (list pos pos) rad2
		"._ARRAY" "L" "" "PO" "0,0" 4 360 "Y"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BRAKETT MOT GULV Ø60/308" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun Montasjeelement_Staal_Brakett_Paa_Annen_Mast ()
	(command 
		"Arc" "C" "0,0" (list 0.25 0.433) (list -0.25 0.433)
		"._MIRROR" "All" "" "0,0" "1,0" "N"
		"._CIRCLE" "0,0" 0.03
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BRAKETT PÅ ANNEN MAST" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock "NO-BN-2D-JBTUB-MONTASJEELEMENT-STAAL-BRAKETT-PAA-ANNEN-MAST")
	blockName
)



(defun Montasjeelement_Tilbakefylt_Grop ()
	(command 
		"._PLINE" 
			"0.3094,0.5743" 
			"Arc"
				"CE" "0.1572,0.5517" "0.0839,0.6870"
				"CE" "-0.0163,0.5716" "-0.1528,0.6263"
				"CE" "-0.2561,0.5066" "-0.4019,0.5570"
				"CE" "-0.4856,0.4273" "-0.6361,0.4616"
				"CE" "-0.4869,0.4239" "-0.4713,0.2708"
				"CE" "-0.4470,0.1070" "-0.5927,0.0281"
				"CE" "-0.6526,-0.1172" "-0.8096,-0.1106"
				"CE" "-0.7679,-0.2624" "-0.8963,-0.3534"
				"CE" "-0.7367,-0.3321" "-0.6621,-0.4748"
				"CE" "-0.5606,-0.3520" "-0.4105,-0.4054"
				"CE" "-0.2817,-0.3217" "-0.1590,-0.4141"
				"CE" "-0.0258,-0.3273" "0.1013,-0.4227"
				"CE" "0.1893,-0.29" "0.3441,-0.3274"
				"CE" "0.4638,-0.2306" "0.5957,-0.31"
				"CE" "0.5641,-0.1448" "0.7085,-0.0586"
				"CE" "0.5549,-0.024" "0.5350,0.1321"
				"CE" "0.4179,0.2440" "0.4829,0.3922"
				"CE" "0.3324,0.4225" "0.3094,0.5743"
				""
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "TILBAKEFYLT GROP" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock "NO-BN-2D-JBTUB-MONTASJEELEMENT-TILBAKEFYLT-GROP")
	blockName
)



(defun Montasjeelement_Strips_Eller_Patentbaand ()
	(command "._PLINE"
	    "-0.5987,0.25"
	    "-0.4487,0.25"
	    "Arc"
	    "CE" "-0.4487,0.1513"
	    "Angle" -90
	    "Line"
	    "-0.35,0"
	    "Arc"
	    "CE" "0,0" "0,-0.35"
	    ""
	    "._MIRROR" "All" ""
	    "0,0" "0,1" "N"
	    "._CIRCLE" "0,0" "0.03"
	    "._ROTATE" "ALL" "" (list 0 0) 180
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "STRIPS ELLER PATENTBÅND" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock "NO-BN-2D-JBTUB-MONTASJEELEMENT-STRIPS-ELLER-PATENTBAAND")
	blockName
)



(defun STOLPE-S-LAAS (variant / blockName)
	(setq blockName (strcat "NO-BN-2D-JBTSI-MONTASJEELEMENT-STOLPE-S-LAAS-" variant))
  	(command
		"._LINE" "-3,0" "3,0" ""
		"._ARC" "C" "0,0" "-1.5,0" "1.5,0"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText (strcat "STOLPE FOR " variant " S-LÅS") "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)
	
	
	
(defun STOLPE-LOKALSTILLER (/ blockName)
	(setq blockName "NO-BN-2D-JBTSI-MONTASJEELEMENT-STOLPE-LOKALSTILLER")
	(command
		"._CIRCLE" "0,0" 0.03
		"._CIRCLE" "0,0" 0.055
	)
	(addText "L" "0,-0.8" 0.8 0 "iso" "ML")
  	(command "._SCALE" "ALL" "" "0,0" "2.5")
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "STOLPE FOR LOKALSTILLER" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun STOLPE-MED-SKAP-OG-KABEL (spec / blockName)
	(setq blockName (strcat "NO-BN-2D-JBTSI-MONTASJEELEMENT-STOLPE-SKAP-OG-KABEL-" spec))
	(command
		"._CIRCLE" "0,0" 0.03
  		"._CIRCLE" "0,0" 0.055
		"._RECTANGLE" "-0.06,-0.055" "0.06,-0.1"
	)
  	(command "._SCALE" "ALL" "" "0,0" "2.5")
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(if (= spec "1600")
		(addMText "STOLPE Ø76 MED SKAP OG KABEL, 1.6m" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
  	)
  	(if (= spec "2000")
		(addMText "STOLPE Ø76 MED SKAP OG KABEL, 2.0m" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	)
  	(if (= spec "3000")
		(addMText "STOLPE Ø76 MED SKAP OG KABEL, 3.0m" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	)
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)



(defun ROERMAST-MED-LJERN ( / blockName len1 len2 h r)
	(setq blockName (strcat "NO-BN-2D-JBTSI-MONTASJEELEMENT-ROERMAST-MED-LJERN")
		len1 (/ 0.008 2)
		len2 (/ 0.052 2)
		h (/ 0.5 2)
		r 0.038
	)
	(command
		"._CIRCLE" "0,0" r
		"._RECTANGLE" (list r h) (list (+ r len1) (- h))
		"._RECTANGLE" (list (+ r len1) h) (list (+ r len1 len2) (- h))
		"._RECTANGLE" (list r (- r)) (list (- h) (- (+ r len1)))
		"._RECTANGLE" (list (+ r len1 len2) (- r)) (list h (- (+ r len1)))
		"._LINE" (list (+ r len1 len2) (- (- r len2))) (list h (- (- r len2))) (list h (- r)) ""
		"._LINE" (list (- (* r (cos (asin (/ (- r len2) r ))))) (- (- r len2))) (list (- h) (- (- r len2))) (list (- h) (- r)) ""
		"._LINE" (list (* r (cos (asin (/ (- r len2) r )))) (- (- r len2))) (list r (- (- r len2))) ""
	)
	(command "._SCALE" "ALL" "" "0,0" "5")
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "RØRMAST MED LJERN FOT" "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(drawFoundationLocator "M" 2.5)
	(newBlock blockName)
	blockName
)