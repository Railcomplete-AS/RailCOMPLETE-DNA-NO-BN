;=========================================================================================================================
;
; Lightweight foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2017-02-15 BEHOP/CLFEY 
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Lightweight mounting (installation) devices (smaller than 100 kg / for boards and smaller items / can be installed without de-stabilizing the track)

; Ref. http://euroskilt.no/oppsetningsutstyr

(defun C:LIGHTWEIGHT-FOUNDATION ( / )

	; KU-SKF Skiltfeste
	(Skiltfeste_Betongfot_60_225 "500")
	(Skiltfeste_Betongfot_60_225 "700")
	(Skiltfeste_Betongfot_60_200_1200)
	(Skiltfeste_Jordfundament_60_700)
	(Skiltfeste_Jordspyd_60)
	(Skiltfeste_Tilbakefylt_Grop)
	(Skiltfeste_Strips_Eller_Patentbaand)
	(Skiltfeste_Brakett_Mot_Vegg_60_110)
	(Skiltfeste_Brakett_Mot_Gulv_60_308)
	(Skiltfeste_Brakett_Mot_Mast)

	; SA-FUN Stolpe med fot / rørmast med fot
	(UNP80-STOLPE-MED-LJERN-FOT 1) ; Enkel S-lås
	(UNP80-STOLPE-MED-LJERN-FOT 2) ; Dobbel S-lås
	(UNP80-STOLPE-MED-LJERN-FOT 3) ; Lokalstiller
	(UNP80-STOLPE-MED-LJERN-FOT 4) ; Overdragstrafo
	(ROERMAST-MED-LJERN-FOT 1) ; Ø76 høyde 2000 mm for togsporsignal og dvergsignal
	(ROERMAST-MED-LJERN-FOT 2) ; Ø76 høyde 3000 mm for høye dvergsignaler og togsporsignaler
	(ROERMAST-MED-LJERN-FOT 3) ; Ø76 høyde 1600 mm for kryssvekselsignal
	(ROERMAST-MED-LJERN-FOT 4) ; Ø127 høyde 3200 mm for lave signaler
)



(defun Skiltfeste_Betongfot_60_225 ( variation / blockName description s1 s2 x d r )
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-SKILTFESTE-BETONGFOT-60-225-" variation)
		description	"" ; See below
		s1 (/ 0.150 2)
		s2 (/ 0.255 2)
		x 0.150 ; four "flaps"
		d 0.12
		r 0.03
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(cond
		((= variation "500") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H500")))
		((= variation "700") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H700")))
		(T (setq description "Something is fishy with SKILTFESTE BETONGFOT "))
	)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(command 
		"._LINE" (list s2 0) (list s2 s1) (list s1 s2) (list 0 s2) ""
		"._LINE" (polar  (list x x) (D->R -45) d) (polar  (list x x) (D->R (+ 90 45)) d) ""
		"._MIRROR" "_ALL" "" _origo_ "0,1" "_NO"
		"._MIRROR" "_ALL" "" _origo_ "1,0" "_NO"
		"._CIRCLE" _origo_ r
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Betongfot_60_200_1200 ( / blockName description ang1 d_ang R1 R2 rad1 rad2 )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BETONGFOT-60-200-1200"
		description	(strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H1200")
		ang1 5.0
		d_ang 45.0
		R1 0.18
		R2 0.22
		rad1 0.03
		rad2 0.10
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(command
		"._ARC" "C" _origo_ (polar _origo_ (D->R ang1) R1) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R ang1) R2) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 90)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 90)) R2) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 180)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 180)) R2) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 270)) R1) "Angle" (rtos d_ang)
		"._ARC" "C" _origo_ (polar _origo_ (D->R (+ ang1 270)) R2) "Angle" (rtos d_ang)
		"._CIRCLE"  _origo_ rad1
		"._CIRCLE"  _origo_ rad2
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Jordfundament_60_700 ( / blockName description rad1 rad2 ang )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-JORDFUNDAMENT-60-700"
		description	(strcat "SKILTFESTE, JORDFUNDAMENT " _uOE_ "60/H700")
		rad1	0.030 ; Ø060 pole
		rad2	0.181 ; 0.151 side wings from the inner pole/tube
		ang		45.0	; DD
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(command 
		"._CIRCLE" _origo_ rad1
		"._LINE" (list (* rad1 (DDcos ang)) (* rad1 (DDsin ang))) (list (* rad2 (DDcos ang)) (* rad2 (DDsin ang))) ""
		"._ARRAY" "_LAST" "" "PO" _origo_ 4 360 "_YES"
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Jordspyd_60 ( / blockName description rad x y )
	(setq 
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-JORDSPYD-60"
		description	(strcat "SKILTFESTE, JORDSPYD " _uOE_ "60/H600")
		rad	0.030	; For Ø60 poles, an L-iron with a Ø60 hose on top
		x   0.100	; Square symbolizes the L-iron bar
		y	0.100
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(drawCircle layer_Zero rad _noWipeout_)
	(drawBox layer_Zero x y _noWipeout_)
	(scaleAll _four_) ; S = 4x real size
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Tilbakefylt_Grop ( / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-TILBAKEFYLT-GROP"
		description	"SKILTFTESTE, TILBAKEFYLT GROP"
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(command 
		"._PLINE" 
			"0.3094,0.5743" 
			"_ARC"
				"_CE" "0.1572,0.5517" "0.0839,0.6870"
				"_CE" "-0.0163,0.5716" "-0.1528,0.6263"
				"_CE" "-0.2561,0.5066" "-0.4019,0.5570"
				"_CE" "-0.4856,0.4273" "-0.6361,0.4616"
				"_CE" "-0.4869,0.4239" "-0.4713,0.2708"
				"_CE" "-0.4470,0.1070" "-0.5927,0.0281"
				"_CE" "-0.6526,-0.1172" "-0.8096,-0.1106"
				"_CE" "-0.7679,-0.2624" "-0.8963,-0.3534"
				"_CE" "-0.7367,-0.3321" "-0.6621,-0.4748"
				"_CE" "-0.5606,-0.3520" "-0.4105,-0.4054"
				"_CE" "-0.2817,-0.3217" "-0.1590,-0.4141"
				"_CE" "-0.0258,-0.3273" "0.1013,-0.4227"
				"_CE" "0.1893,-0.29" "0.3441,-0.3274"
				"_CE" "0.4638,-0.2306" "0.5957,-0.31"
				"_CE" "0.5641,-0.1448" "0.7085,-0.0586"
				"_CE" "0.5549,-0.024" "0.5350,0.1321"
				"_CE" "0.4179,0.2440" "0.4829,0.3922"
				"_CE" "0.3324,0.4225" "0.3094,0.5743"
				""
			"._SCALE" "_ALL" "" _origo_ 0.5
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Strips_Eller_Patentbaand ( / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-STRIPS-ELLER-PATENTBAAND"
		description	(strcat "SKILTFESTE, STRIPS ELLER PATENTB" _uAA_ "ND")
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(command 
		"._PLINE"
			"-0.5987,0.25"
			"-0.4487,0.25"
			"_ARC"
			"_CE" "-0.4487,0.1513"
			"Angle" -90
			"Line"
			"-0.35,0"
			"_ARC"
			"_CE" "0,0" "0,-0.35"
			""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
		"._ROTATE" "_ALL" "" (list 0 0) 180
		"._SCALE" "_ALL" "" _origo_ 0.25
		"._CIRCLE" "0,0" "0.03"
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Brakett_Mot_Gulv_60_308 ( / blockName description ang1 rad1 rad2 pos side )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-MOT-GULV-60-308"
		description	(strcat "SKILTFESTE, BRAKETT MOT GULV " _uOE_ "60/308")
		rad1 0.03
		rad2  0.007
		pos 0.082
		side 0.255
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(command
		"._CIRCLE" "0,0" rad1
		"._RECTANGLE" 
			(list (/ side -2) (/ side -2))
			(list (/ side 2) (/ side 2))
			""
		"._CIRCLE" (list pos pos) rad2
		"._ARRAY" "_LAST" "" "PO" "0,0" 4 360 "_YES"
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Brakett_Mot_Mast ( / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-PAA-ANNEN-MAST"
		description	(strcat "SKILTFESTE, BRAKETT P" _uAA_ " ANNEN MAST")
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(command 
		"_ARC" "C" "0,0" '(0.1 0.2) '(-0.1 0.2)
		"._MIRROR" "_ALL" "" "0,0" "1,0" "_NO"
		"._CIRCLE" "0,0" 0.03
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun Skiltfeste_Brakett_Mot_Vegg_60_110 ( / blockName description wingStart wingLength wingIncrY )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-MOT-VEGG-60-110"
		description	(strcat "SKILTFESTE, VEGGFESTE " _uOE_ "60/110")
		backPlate	0.200
		wingStart	0.15
		wingLength	0.15
		wingIncrY	0.05
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(command  ; Three "eagle wings" to each side of insertion point:
		"._LINE" (list wingStart 0) (list (+ wingStart wingLength) 0) ""
		"._LINE" (list wingStart wingIncrY) (list (+ wingStart wingLength) wingIncrY) ""
		"._LINE" (list wingStart (- wingIncrY)) (list (+ wingStart wingLength) (- wingIncrY)) ""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
	)
	(command ; Outline of wall-mount bracket (for boards etc Ø60):
		"._LINE" (list (- (/ backPlate 2)) 0) (list (/ backPlate 2) 0) ""
		"._RECTANGLE" 
			(list (- (/ backPlate 4)) 0)
			(list (/ backPlate 4) (- (/ backPlate 2)))
	)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun UNP80-STOLPE-MED-LJERN-FOT ( variation / blockName description t1 t2 x1 x2 x3 y1 y2 y3 p1 p2 p3 xBar yBar )
	; Ref S.018812 Stolpe for festeplater for lokalstiller type Satema S-låser type DSI Overdragstrafo TM 14
	; Stolpen er en UNP80, 2200 høy / 800 under bakken, 1400 over bakken. Kommer med 4 stk festeplate-varianter,
	; enkel S-lås (bxh = 170x350), dobbel S-lås (410x350), lokalstiller 148x96), overdragstrafo 195x240). 6mm tykke plater.
	; Platene flukter i overkant stolpe. U vender forover. Dekkplate over kabler i høyder 700-870 og 930-1400 over UK stolpe.
	;
	;        _
	;       | |
	;       | |
	;    ___|_|___________
	;   |________4________|
	;       | +=====+
	;       |5||2,3||
	;       =====.=====    . = 1
	;       | |
	;       | |
	;       +-+ 
	;
	;
	(cond
		((= variation 1) (setq t1 "ENKEL-SAMLELAAS"  t2 (strcat "ENKEL S-L" _uAA_ "S")  x1 0.170)) ; x1 = plate width
		((= variation 2) (setq t1 "DOBBEL-SAMLELAAS" t2 (strcat "DOBBEL S-L" _uAA_ "S") x1 0.410))
		((= variation 3) (setq t1 "LOKALSTILLER"     t2 "LOKALSTILLER"                  x1 0.148))
		((= variation 4) (setq t1 "OVERDRAGSTRAFO"   t2 "OVERDRAGSTRAFO"                x1 0.195))
	)
	(setq 
		blockName	(strcat "NO-BN-2D-JBTSI-STOLPE-MED-LJERN-FOR-" t1)
		description	(strcat "STOLPE UNP80 MED L-JERN FOT FOR " t2)
		y1 0.006 	; plate thickness
		p1 '(0.000 0.003) ; center thin plate
		x2 0.080	; UNP width
		y2 0.046	; UNP depth (well, .045)
		p2 '(0.000 0.029) ; center UNP80
		x3 0.072	; Inner UNP lining
		y3 0.040	
		p3 '(0.000 0.026)
		xBar 0.500 ; L-bar foot length
		yBar 0.060 ; L-bar foot width
		p4 '(0.000 0.082) ; y=0.006 + 0.046 + 0.060/2
		p5 '(-0.070 0.029)
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(setLayer layer_Zero)
	(drawBoxAtPos layer_Zero x1 y1 p1 _noWipeout_)
	(drawBoxAtPos layer_Zero x2 y2 p2 _noWipeout_)
	(drawBoxAtPos layer_Zero x3 y3 p3 _noWipeout_)
	(drawBoxAtPos layer_Zero xBar yBar p4 _noWipeout_)
	(drawLine layer_Zero '(-0.250 0.058) '(0.250 0.058)) ; L-bar ==
	(drawBoxAtPos layer_Zero yBar xBar p5 _noWipeout_)
	(drawLine layer_Zero '(-0.046 0.250) '(-0.046 -0.250)) ; L-bar ||
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)



(defun ROERMAST-MED-LJERN-FOT ( variation / blockName description len1 len2 h r h2 )
	; Ref. S-002347, for togsporsignal+dvergsignal (2000), dvergsignal+togsporsignal (3000)og kryssvekselsignal (1600)
	; Ø76 rørmast lengder 2000, 3000, 1600 
	; Hull Ø33 for kabelboks plasseres 350 over bakkenivå (1260 over UK rørmast)
	; 2000 = 910 under bakken, 1090 over bakken, benyttes til normal høyde dverg- og togsporsignaler
	; 3000 = 910 under bakken, 2090 over bakken, benyttes for høye dverg- og togsporsignaler
	; 1600 = 910 under bakken, 690 over bakken, benyttes for kryssvekselsignal
	;
	; Benyttes sammen med 2 stk L-jern S.005382 (for Ø76 og for Ø127 master, har hull for begge typer U-bøyler).
	; U-bøyler for Ø127 rørmast: S.043753, Ø10 rundstål med 53mm gjengede ender (R64-R74-Ø10, total høyde 190mm)
	; Dersom rørmasten benyttes kun for små skap så påsettes svart plasthette S.040149 høyde 35+ca10 = ca 45 mm høy, 10mm over topp rørmast.
	(cond
		((= variation 1) (setq r 0.038 d "76"  t1 "DS-OG-TSS-2000"         t2 "DVERGSIGNAL / TOGSPORSIGNAL, h=2000 (910+1090)"))
		((= variation 2) (setq r 0.038 d "76"  t1 "HOEY-DS-OG-TSS-3000"    t2 (strcat "H" _uOE_ "Y DVERG / H" _uOE_ "YT TOGSPORSIGNAL, h=3000 (910+2090)")))
		((= variation 3) (setq r 0.038 d "76"  t1 "KRYSSVEKSELSIGNAL-1600" t2 "KRYSSVEKSELSIGNAL, h=1600 (910+690)"))
		((= variation 4) (setq r 0.064 d "127" t1 "SIGNAL-3200"            t2 (strcat "SIGNAL P" _uAA_ " KORT MAST, h=3200")))
	)
	(setq 
		blockName	(strcat "NO-BN-2D-JBTSI-ROERMAST-" d "-MED-LJERN-FOR-" t1)
		description	(strcat _uOE_ d " R" _uOE_ "RMAST MED L-JERN FOT FOR " t2)
		; Actual measures from construction drawing:
		r2 (* 0.9 r) ; Inner diameter of mast is approx 9/10 of outer diameter
		xBar 0.500 ; L-bar foot length
		yBar 0.060 ; L-bar foot width
		p4 (cond ((= variation 4) '(0.000 0.040)) (T '(0.000 0.014))) ; r + 0.006 - 0.060/2 ==
		p5 (cond ((= variation 4) '(0.094 0.000)) (T '(0.068 0.000))) ; r + 0.060/2         ||
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(drawProxySymbol layer_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(drawCircle layer_Zero r _noWipeout_)
	(drawCircle layer_Zero r2 _noWipeout_)
	(drawBoxAtPos layer_Zero xBar yBar p4 _noWipeout_)
	(drawLine layer_Zero (list -0.250 r) (list 0.250 r)) ; L-bar ==
	(drawBoxAtPos layer_Zero yBar xBar p5 _noWipeout_)
	(drawLine layer_Zero (list (+ r 0.006) 0.250) (list (+ r 0.006) -0.250)) ; L-bar ||
	(moveUp r)
	(scaleAll _four_)
	(addScaledGraphicsFromBlock blockName _one_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
