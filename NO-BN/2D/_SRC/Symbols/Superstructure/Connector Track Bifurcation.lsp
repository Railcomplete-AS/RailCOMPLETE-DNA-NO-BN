;=========================================================================================================================
;
; Connector Track Bifurcation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Symbols for switches (turnouts), with graphics from Superstructure / Catenary / Signaling.

;-------------------------------------------------------------------------------------------------------------------------
;
; Guide to understanding the RailCOMPLETE switch object symbols
; =============================================================
;
; Each RailCOMPLETE switch has graphical elements coming from three parts of the LISP 2D symbol library source code:
;
; SWITCH-SYMBOL-SUPERSTRUCTURE	The track and embankment's viewpoint (concerning geometry, sleepers, forbidden sensor area etc)
; SWITCH-SYMBOL-HIGH-VOLTAGE	The high voltage viewpoint (concerning electrification of one or both tracks)
; SWITCH-SYMBOL-SIGNALING		The signaling viewpoint (throwing actuator type and interlocking control type)
;
; Elements from all disciplines are assembled by the switch object's DNA type declaration, using <SymbolDefinition> XML 
; declarations. The difference discipline's graphics will be switched on or off using the RC-ShowLayer tool, which manipulates
; CAD layers.
;
; See also the 2D symbol source file for creation of standard layers, where the various discipline's graphics layers are 
; defined. Note that basic symbol discipline-dependent graphics have color "ByBlock", where as "annotation graphics" will 
; usually have color "ByLayer".
;
; Note that switch 2D symbols cannot be scaled for various drawing scales because they follow the track's geometry in 
; scale 1:1.
; TODO: However, if we will add textual elements to switches, we might want to scale these.
;
;-------------------------------------------------------------------------------------------------------------------------

(defun C:CONNECTOR-TRACK-BIFURCATION ( / )
	(setCadSystemDefaults)  
	(CONNECTOR-SWITCH-UNSPECIFIED) 		; Schematic symbol for unknown geometry (fixed 1:1 scale)
	(CONNECTOR-SWITCH-WITH-GEOMETRY) 	; Turnout - switch - point with recognized geometry (fixed 1:1 scale)
)



(defun CONNECTOR-SWITCH-UNSPECIFIED ( / blockNameBase blockName u )
	(setq
		blockName "NO-BN-2D-JBTOB-CONNECTOR-SWITCH-UNSPECIFIED"
		u 1.0 ; unit
	)
	(subStep (strcat "Unspecified switch"))
	(setLayer layer_Zero)

	 ; Quadrant 1
	 (command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
    (createSchematicBlockFromCurrentGraphics (strcat blockName "-1"))

	 ; Quadrant 2
	(command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "_ALL" "" (list 0 0) (list 0 1) "_YES")
    (createSchematicBlockFromCurrentGraphics (strcat blockName "-2"))

	 ; Quadrant 3
	 (command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "_ALL" "" (list 0 0) (list 0 1) "_YES")
	(command "._MIRROR" "_ALL" "" (list 0 0) (list 1 0) "_YES")
    (createSchematicBlockFromCurrentGraphics (strcat blockName "-3"))
	
	 ; Quadrant 4
	(command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "_ALL" "" (list 0 0) (list 1 0) "_YES")
    (createSchematicBlockFromCurrentGraphics (strcat blockName "-4"))
)



(defun CONNECTOR-SWITCH-WITH-GEOMETRY ( / Switch_Drawing_List element_no Drawing_Number quadrant )
	; Ref: Bane NOR standard tegninger for sporveksler (tegningsnummer KO.nnnnnn)
	; TODO: Include details on guard rails, tongue hinge / swivel point etc.
	(setq 
		Switch_Drawing_List (list
			; Enkel 54E3
			"KO-800157" ;1:7 R190 mangler tegning
			"KO-701334" ;1:9 R190   OK
			
			; Fast kryss 54E3
			"KO-701287" ;1:9 R300
			"KO-701306" ;1:12 R500
			"KO-701319" ;1:14 R760
	
			; Fast kryss 60E1
			"KO-701409" ;1:9 R300
			"KO-800068-2" ;1:11.66 R500
			"KO-800068" ;1:12 R500
			"KO-701372" ;1:14 R760
			"KO-701382" ;1:15 R760
			
			; Bevegelig kryss 60E1
			"KO-800099" ; 1:9 R300
			"KO-065306" ; 1:8.21 R300
			"KO-800090" ;1:12 R500
			"KO-800108" ;1:14 R760
			"KO-800164" ;1:15 R760
			"KO-800081" ;1:18,4 R12001 klotoideveksel mangler forbudtområde
			"KO-701399" ;1:26,1 R25001 klotoideveksel
		)
	)
	(subStep "Switches:")
	(SWITCH-SYMBOL-SIGNALING-CONTROL-METHOD)		; Turnout - switch - right side, left side or both sides key-locked control position symbols (3 symbols generates here)
	(setq element_no 0)
	(repeat (length Switch_Drawing_List)
		(setq Drawing_Number (nth element_no Switch_Drawing_List))
		(setq quadrant 1)
		(SWITCH-SYMBOL-SIGNALING-THROWING-METHOD Drawing_Number) ; Used in the signaling symbols (same for all variants fylt/tom/lett per switch type)
		(subSubStep (strcat "Switch " Drawing_Number))
		(repeat 4
			(SWITCH-SYMBOL-SIGNALING quadrant Drawing_Number) ; Signaling discipline symbols in switch object
			(SWITCH-SYMBOL-SUPERSTRUCTURE quadrant Drawing_Number) ; Track discipline symbols in switch object
			(SWITCH-SYMBOL-HIGH-VOLTAGE quadrant Drawing_Number) ; Catenary discipline symbols in switch object
			(setq quadrant (+ 1 quadrant))
		)
		(setq element_no (+ 1 element_no))
	)
)



(defun SWITCH-SYMBOL-SIGNALING-THROWING-METHOD (Drawing_Number / blockName switchParameters A radius )
	; Generate symbol for filled circle at theoretical crossing in switch - used to signify "motorized hand-thrown with push-button operation"
	; German: "Elektrisch Ortsgestellte Weichen", EOW.
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		A	  		(/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius 	  	0.75
	)
	(setq blockName (strcat "NO-BN-2D-JBTSI-SWITCH-THROWING-METHOD" "-" (rtos A 2 3)))
	(setLayerAndObjectColor layer_Zero "_ByBlock")
	(command "._CIRCLE" (list A 0) radius)
	(drawHatch _filledHatch_)
	(createSchematicBlockFromCurrentGraphics blockName)
)



;TODO 2019-05-08 CLFEY: Data is missing for almost all switches
(defun getAreaOne (Switch_Drawing_Number)
	; Forbidden area for axle counter sensors - area 1 of 2
	(cadr
		(assoc Switch_Drawing_Number
			(list	 
				(list 	"KO-800157"	nil)
				(list 	"KO-701334"	(list "-0.43,-1.25" "15.5364,-1.2457" "15.4105,1.8757" "-0.43,1.25" "_CLOSE"))
						
				(list 	"KO-701287"	nil)
				(list 	"KO-701306"	nil)
				(list 	"KO-701319"	nil)
				
				(list 	"KO-701409"	(list "-0.4320,-1.2975" "19.2564,-1.2933" "19.1546,1.8757" "-0.4320,1.2975" "_CLOSE"))
				(list 	"KO-800068-2" (list "-0.4320,-1.2975" "24.7622,-1.2919" "24.6836,1.9032" "-0.4320,1.2975" "_CLOSE"))
				(list 	"KO-800068"	(list "-0.4320,-1.2975" "24.7622,-1.2919" "24.6836,1.9032" "-0.4320,1.2975" "_CLOSE"))
				(list 	"KO-701372"	nil)
				(list 	"KO-701382"	nil)
						
				(list 	"KO-800099"	nil)
				(list 	"KO-065306"	nil)
				(list 	"KO-800090"	nil)
				(list 	"KO-800108"	nil)
				(list 	"KO-800164"	nil)
				(list 	"KO-800081"	nil)
				(list 	"KO-701399"	nil)
			);list																												
	);assoc
  );cadr
)



(defun getAreaTwo (Switch_Drawing_Number)
	; Forbidden area for axle counter sensors - area 2 of 2
	(cadr
		(assoc Switch_Drawing_Number
			(list	 
				(list 	"KO-800157"	nil)
				(list 	"KO-701334"	(list "19.5777,-1.2575" "19.5258,-0.2513" "18.3102,-0.3116" "18.2375,1.1904" "19.4483,1.2506" "19.3964,2.2568" "27.3973,3.1264" "27.4662,1.8826" "30.0187,2.1662" "30.1387,0" "27.5705,0" "27.6393,-1.2433" "_CLOSE"))
						
				(list 	"KO-701287"	nil)
				(list 	"KO-701306"	nil)
				(list 	"KO-701319"	nil)
	
				(list 	"KO-701409"	(list "25.5829,-1.3027" "25.5366,-0.2189" "23.1623,-0.3150" "23.1033,1.2070" "25.4716,1.3032" "25.4253,2.3869" "33.4828,3.1717" "33.5542,1.8823" "35.9395,2.1473" "36.0585,0" "33.6585,0" "33.7299,-1.2894" "_CLOSE"))
				(list 	"KO-800068-2" (list "46.2083,0" "46.1204,2.1120" "41.9549,1.7646" "41.9012,3.0543" "33.5221,2.4215" "33.5591,1.3251" "29.9200,1.2094" "29.9658,-0.3127" "33.6104,-0.1970" "33.6104,-1.2934" "42.0820,-1.2910" "42.0283,0" "_CLOSE"))
				(list 	"KO-800068"	(list "46.2083,0" "46.1204,2.1120" "41.9549,1.7646" "41.9012,3.0543" "33.5221,2.4215" "33.5591,1.3251" "29.9200,1.2094" "29.9658,-0.3127" "33.6104,-0.1970" "33.6104,-1.2934" "42.0820,-1.2910" "42.0283,0" "_CLOSE"))
				(list 	"KO-701372"	nil)
				(list 	"KO-701382"	nil)
	
				(list 	"KO-800099"	nil)
				(list 	"KO-065306"	nil)
				(list 	"KO-800090"	nil)
				(list 	"KO-800108"	nil)
				(list 	"KO-800164"	nil)
				(list 	"KO-800081"	nil)
				(list 	"KO-701399"	nil)
			);list																												
		);assoc
	);cadr
)
  
  
  
(defun getSwitchParameters (Switch_Drawing_Number)
	; Inserted from Excel file Sporveksler.xlsx. 
	; Basic information was extracted from Bane NOR TRV (Technical Regulations), 'Overbygning/Prosjektering/Sporveksler/4. Hovedmål/4. Enkel sporveksel'.
	;
	; 4.1 Enkle sporveksler
	; ---------------------
	;
	;  SS      TK      R2      BK
	;  |<--A-->|<--C-->|<--D-->|        ______
	;                           _______/     |
	;                   _______/       |_____|
	;           _______/ / / / /       |     |
	;  .-------O*******+-------+-------+-----+
	;      
	;  |<--A-->|<------B------>|<--E-->|<-F->| 
	;  |<-----------L--------->|
	;
	; Tegnforklaring:
	; SS: stokkskinneskjøt
	; BK: bakkant sporveksel
	; R2: sirkelkurvens endepunkt i avvik
	; TK: teoretisk kryss
	; L: byggelengde
	; A: tangentlengde/lengde i X-retning før teoretisk kryss
	; C: tangentlengde til sirkelkurven etter teoretisk kryss
	; B: lengde i X-retning av C + D
	; D: rettlinjet parti i avvik
	; E: lengde av parti med langsviller utenfor BK
	; F: lengde av parti med kortviller utenfor BK	
	;
	; Tabell 1: Enkel sporveksel for spor uten persontrafikk i avvik, hovedmål
	; ------------------------------------------------------------------------
	; Profil	Sviller	Stigning 	Radius	A		B		C		D		L		E		F		Masse	Tegn.nr
	; 54E3		Betong	1:7 		R190	13503	13503	13503	-		27006	3654	2375			KO-800157
	; 54E3		Betong	1:9 		R190	10523	16616	10523	6093	27139	3000	3575	36,3	KO-701334	
	; 
	; Sporveksler i Tabell 1 skal ikke prosjekteres med persontrafikk i sporvekselens avvikespor.
	; Alle mål er i mm.
	; Masse er oppgitt i tonn og inkluderer alle skinner, ståldeler og sviller
	; 
	; Tabell 2: Sporveksler med fast skinnekryss, hovedmål
	; ----------------------------------------------------
	; Profil	Sviller	Stigning Radius		A		B		C		D		L		E		F		Masse	Tegn.nr
	; 54E3		Betong	1:9 		R300	16616	16616	16616	-		33231	3575	3000	40,5	KO-701287
	; 54E3		Betong	1:12 		R500	20797	20797	20797	-		41594	5835	4775	50,3	KO-701306
	; 54E3		Betong	1:14 		R760	27108	27108	27108	-		54216	3900	5375	52,0	KO-701319
	; 60E1		Betong	1:9 		R300	16615	16615	16615	-		33230	5400	2350	43,4	KO-701409
	; 60E1		Betong	1:11,66		R500	21393	21391	21393	-		42783	7314	2400	53,8	KO-800068
	; 60E1		Betong	1:12 		R500	20797	21985	20797	1188	42783	7314	3600	53,8	KO-800068
	; 60E1		Betong	1:14 		R760	27108	27108	27108	-		54216	7200	9000	57,2	KO-701372
	; 60E1		Betong	1:15 		R760	25305	28911	25305	3606	54216	7200	9600	54,2	KO-701382
	; 
	; Alle mål er i mm.
	; Masse er oppgitt i tonn og inkluderer alle skinner, ståldeler og sviller
	; 
	; 
	; Tabell 3: Sporveksler med bevegelig skinnekryss, hovedmål
	; ---------------------------------------------------------
	; Profil	Sviller	Stigning 	Radius	A		B		C		D		L		E		F		Masse	Tegn.nr
	; 60E1		Betong	1:9 		R300	16615	19760	16615	3145	36375	4150	-				KO-800099
	; 60E1		Betong	1:8,21		R300	18188	18188	18188	-		36375	3550	-				KO-065306
	; 60E1		Betong	1:12 		R500	20797	23306	20797	2509	44103	5994	-				KO-800090
	; 60E1		Betong	1:14 		R760	27108	27108	27108	-		54216	2400	9000			KO-800108
	; 60E1		Betong	1:15 		R760	25305	28911	25305	3606	54216	2400	9600			KO-800164
	; 60E1		Betong	1:18,4 		R1200 1	32829	34429	34429	-		67257	10781	3600			KO-800081
	; 60E1		Betong	1:26,1 		R2500 1	48109	46491	46491	-		94600	17400	-				KO-701399
	; Alle mål er i mm.
	; 
	; 1) Klotoideveksel
	
	(cadr
		(assoc Switch_Drawing_Number ; list was copied from Bane NOR TRV:
			(list	                  
				;     Drawing number            Rail Head Profile              Sleeper Type
				(list "KO-800157"   (list '("RailProfile" "54E3") '("SleeperType" Concrete) '("x" 7    ) '("R" 190 ) '("A" 13503 ) '("B" 13503) '("C" 13503	) '("D" 0   ) '("L" 27006 ) '("E" 3654 ) '("F" 2375) '("SwitchCrossing" "FX")))
				(list "KO-701334"   (list '("RailProfile" "54E3") '("SleeperType" Concrete) '("x" 9    ) '("R" 190 ) '("A" 10523 ) '("B" 16616) '("C" 10523	) '("D" 6093) '("L" 27139 ) '("E" 3000 ) '("F" 3575) '("SwitchCrossing" "FX")))
																																																				 
				(list "KO-701287"   (list '("RailProfile" "54E3") '("SleeperType" Concrete) '("x" 9    ) '("R" 300 ) '("A" 16616 ) '("B" 16616) '("C" 16616	) '("D" 0   ) '("L" 33231 ) '("E" 3575 ) '("F" 3000) '("SwitchCrossing" "FX")))
				(list "KO-701306"   (list '("RailProfile" "54E3") '("SleeperType" Concrete) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 20797) '("C" 20797	) '("D" 0   ) '("L" 41594 ) '("E" 5835 ) '("F" 4775) '("SwitchCrossing" "FX")))
				(list "KO-701319"   (list '("RailProfile" "54E3") '("SleeperType" Concrete) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 3900 ) '("F" 5375) '("SwitchCrossing" "FX")))
																																																				 
				(list "KO-701409"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 9    ) '("R" 300 ) '("A" 16615 ) '("B" 16615) '("C" 16615	) '("D" 0   ) '("L" 33230 ) '("E" 5400 ) '("F" 2350) '("SwitchCrossing" "FX")))
				(list "KO-800068-2" (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 11.66) '("R" 500 ) '("A" 21393 ) '("B" 21391) '("C" 21393	) '("D" 0   ) '("L" 42783 ) '("E" 7314 ) '("F" 2400) '("SwitchCrossing" "FX")))
				(list "KO-800068"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 21985) '("C" 20797	) '("D" 1188) '("L" 42783 ) '("E" 7200 ) '("F" 3600) '("SwitchCrossing" "FX")))
				(list "KO-701372"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 7200 ) '("F" 9000) '("SwitchCrossing" "FX")))
				(list "KO-701382"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 15   ) '("R" 760 ) '("A" 25305 ) '("B" 28911) '("C" 25305	) '("D" 3606) '("L" 54216 ) '("E" 7200 ) '("F" 9600) '("SwitchCrossing" "FX")))
																																																				 
				(list "KO-800099"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 9    ) '("R" 300 ) '("A" 16615 ) '("B" 19760) '("C" 16615	) '("D" 3145) '("L" 36375 ) '("E" 1750 ) '("F" 2950) '("SwitchCrossing" "BX")))
				(list "KO-065306"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 8.21 ) '("R" 300 ) '("A" 18188 ) '("B" 18188) '("C" 18188	) '("D" 0   ) '("L" 36375 ) '("E" 3550 ) '("F" 0   ) '("SwitchCrossing" "BX")))
				(list "KO-800090"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 23306) '("C" 20797	) '("D" 2509) '("L" 44103 ) '("E" 5994 ) '("F" 0   ) '("SwitchCrossing" "BX")))
				(list "KO-800108"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 10825) '("F" 9000) '("SwitchCrossing" "BX")))
				(list "KO-800164"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 15   ) '("R" 760 ) '("A" 25305 ) '("B" 28911) '("C" 25305	) '("D" 3606) '("L" 54216 ) '("E" 2400 ) '("F" 9600) '("SwitchCrossing" "BX")))
				(list "KO-800081"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 18.4 ) '("R" 1200) '("A" 32829 ) '("B" 34429) '("C" 34429	) '("D" 0   ) '("L" 67257 ) '("E" 10781) '("F" 3600) '("SwitchCrossing" "BX")))
				(list "KO-701399"   (list '("RailProfile" "60E1") '("SleeperType" Concrete) '("x" 26.1 ) '("R" 2500) '("A" 48109 ) '("B" 46491) '("C" 46491	) '("D" 0   ) '("L" 94600 ) '("E" 17400) '("F" 0   ) '("SwitchCrossing" "BX")))
			);list																														
		);assoc
	);cadr
)
