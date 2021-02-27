;=========================================================================================================================
;
; Connector Track Bifurcation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
; Note that geo switch 2D symbols are purely metric (non-annotative).
;
;-------------------------------------------------------------------------------------------------------------------------

(defun C:CONNECTOR-SWITCH ( / )
	(setCadSystemDefaults)  
	(CONNECTOR-SWITCH-ANONYMOUS) 		; Schematic symbol for unknown geometry (fixed 1:1 scale)
	(CONNECTOR-SWITCH-WITH-GEOMETRY) 	; Turnout - switch - point with recognized geometry (fixed 1:1 scale)
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
	; Symbolism:
	; **: Dense hatch pattern
	; //: Sparse hatch pattern
	; SS: stokkskinneskjøt / stock rail joint
	; BK: bakkant sporveksel / rear end of turnout
	; R2: sirkelkurvens endepunkt i avvik / circular arc's end in the deviating track
	; TK: teoretisk kryss / Theoretical Crossing = point of tangent intersections from rear ends of straight and deviating tracks
	; L: byggelengde / Constructive length
	; A: tangentlengde/lengde i X-retning før teoretisk kryss / Tangent line from Stock rail joint to TK
	; C: tangentlengde til sirkelkurven etter teoretisk kryss / Tangent line from rear end of deviating track's circular arc part
	; D: rettlinjet parti i avvik / straight part in deviating track, after the circular arc part, up to the end of the factory-delivered constructive length
	; B: lengde i X-retning av C + D / Sum of C and D
	; E: lengde av parti med langsviller utenfor BK / Part with long sleepers (both tracks on same sleeper) after turnout's constructive end
	; F: lengde av parti med kortviller utenfor BK	/ Part with shortened sleepers up to the point where normal sleepers can be used (with enough track separation)
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



(defun CONNECTOR-SWITCH-ANONYMOUS ( / blockNameBase blockName u bn q )
	(setq
		blockName "NO-BN-2D-JBTOB-CONNECTOR-SWITCH-ANONYMOUS"
		u 1.0 ; unit
	)
	(subStep (strcat "SWITCH: ANONYMOUS"))
	(setLayer layDef_Zero)
	(command 
		_POLYLINE_ (list 0 u) (list 0 (* -1 u)) _ENTER_ ; Stock rail joint
		_POLYLINE_ (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) _origo_ _ENTER_
	)
	(drawHatchFromPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) _angleMinus45_ _offsetZero_)
	(createSchematicBlockFromCurrentGraphics "tmp")

	(foreach q '(1 2 3 4)
		(setq bn (strcat blockname "-" (rtos q _decimal_ _zero_)))
		(addGraphicsFromScaledSchematicBlock "tmp" _one_)
		(moveLeft (* 2 u))
		(moveToQuadrant q _selectAll_)
		(createSchematicBlockFromCurrentGraphics bn)
		(addGraphicsFromScaledSchematicBlock "tmp" _one_)
		(moveToQuadrant q _selectAll_)
		(createMetricBlockFromCurrentGraphics bn)
		(eraseSchematicBlock "tmp")
	)
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
			"KO-800081" ;1:18,4 R12001 klotoideveksel
			"KO-701399" ;1:26,1 R25001 klotoideveksel
		)
	)
	(subStep "Switches:")
	(SWITCH-SYMBOL-SIGNALING-KEYLOCKED)		; Turnout - switch - right side, left side or both sides key-locked control position symbols (3 symbols generates here)
	(setq element_no 0)
	(repeat (length Switch_Drawing_List)
		(setq Drawing_Number (nth element_no Switch_Drawing_List))
		(setq quadrant 1)
		(SWITCH-SYMBOL-SIGNALING-THROWING-METHOD Drawing_Number) ; Used in the signaling symbols (same for all variants fylt/tom/lett per switch type)
		(subSubStep (strcat "SWITCH: " Drawing_Number))
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
	(setLayerAndObjectColor layDef_Zero "_ByBlock")
	(command _CIRCLE_ (list A 0) radius)
	(drawHatch _solidHatch_)
	(createMetricBlockFromCurrentGraphics blockName)
)
