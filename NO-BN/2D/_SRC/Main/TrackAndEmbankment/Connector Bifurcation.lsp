;=========================================================================================================================
;
; Connector Bifurcation.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Symbols for switches (turnouts), with graphics from Superstructure / Catenary / Signalling.

;-------------------------------------------------------------------------------------------------------------------------
;
; Guide to understanding the RailCOMPLETE switch object symbols
; =============================================================
;
; Each RailCOMPLETE switch has graphical elements coming from three parts of the LISP 2D symbol library source code:
;
; SWITCH-SYMBOL-SUPERSTRUCTURE	The track and embankment's viewpoint (concerning geometry, sleepers, forbidden sensor area etc)
; SWITCH-SYMBOL-HIGH-VOLTAGE	The high voltage viewpoint (concerning electrification of one or both tracks)
; SWITCH-SYMBOL-SIGNALLING		The signaling viewpoint (throwing actuator type and interlocking control type)
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

(defun CONNECTOR-BIFURCATIONS ( / )
	(SetCadSystemDefaults)  
	(CONNECTOR-SWITCH-GENERAL) 				; Schematic symbol for unknown geometry (fixed 1:1 scale)
	(CONNECTOR-SWITCH-WITH-GEOMETRY) 			; Switch - turnout - point with recognized geometry (fixed 1:1 scale)
)



(defun CONNECTOR-SWITCH-GENERAL ( / blockName u currentBlockName q )
	; Triangular arrowhead with 'stock rail joint line' across the track at the SRJ:
	;
	;   2       4
	;   |  ___/*|
	;   ./------+	Arrow head with solid hatch
	;   |        
	;   1        
	;              
	(setq blockName (strcat _TRK_ "SPV-" "FORBINDELSE-SPORVEKSEL-GENERELL"	))
	(setq description (strcat "FORBINDELSE, SPORVEKSEL, GENERELL"			))
	(setq
		u 1.0 ; unit
	)
	(TraceLevel2 (strcat "...SWITCH: GENERAL"))
	(SetLayer layDef_Zero)
	(command 
		_POLYLINE_ (list 0 u) (list 0 (* -1 u)) _closedPolyline_ ; Stock rail joint
		_POLYLINE_ (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) _origin_ _closedPolyline_
	)
	(DrawHatchAtPoint _denseHatch_ (list (* 3 u) (* 0.5 u)) _angle15_ _offsetZero_)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics "tmp")

	(foreach q '(1 2 3 4)
		(setq currentBlockName (strcat blockname "-" (rtos q _decimal_ _zero_)))

		(AddGraphicsFromScaledSchematicBlock "tmp" _one_)
		(MoveLeft (* 2 u))
		(MoveToQuadrant q _selectAll_)
		(CreateSchematicBlockFromCurrentGraphics currentBlockName)

		(AddGraphicsFromScaledSchematicBlock "tmp" _one_)
		(MoveToQuadrant q _selectAll_)
		(CreateMetricBlockFromCurrentGraphics currentBlockName)

		(EraseSchematicBlock "tmp")
	)
)



(defun CONNECTOR-SWITCH-WITH-GEOMETRY ( / switchDrawingList itemNumber drawingNumber quadrant )
	; Ref: Bane NOR standard tegninger for sporveksler (tegningsnummer KO.nnnnnn)
	; TODO: Include details on guard rails, tongue hinge / swivel point etc.
	(setq blockNameSig (strcat _SIG_ "SPV-" "FORBINDELSE-SPORVEKSEL"	))
	(setq descriptionSig (strcat "SPORVEKSEL, SIGNALSYMBOL"				))
	(TraceLevel2 "...SWITCH: WITH TRUE GEOMETRY")
	(setq
		switchDrawingList (list
			; Enkel 54E3
			"KO-800157" 	;1:7 R190 mangler tegning
			"KO-701334" 	;1:9 R190
			
			; Fast kryss 54E3 (fixed diamond)
			"KO-701287" 	;1:9 R300
			"KO-701306" 	;1:12 R500
			"KO-701319" 	;1:14 R760
	
			; Fast kryss 60E1 (fixed diamond)
			"KO-701409" 	;1:9 R300
			"KO-800068" 	;1:12 R500
			"KO-800068-2" 	;1:11.66 R500
			"KO-701372" 	;1:14 R760
			"KO-701382" 	;1:15 R760
			
			; Bevegelig kryss 60E1 (switchable diamond)
			"KO-800099" 	;1:9 R300
			"KO-065306" 	;1:8.21 R300
			"KO-800090" 	;1:12 R500
			"KO-800108" 	;1:14 R760
			"KO-800164" 	;1:15 R760
			"KO-800081" 	;1:18,4 R12001 klotoideveksel
			"KO-701399" 	;1:26,1 R25001 klotoideveksel
		)
	)
	(SWITCH-SYMBOL-SIGNALLING-KEYLOCKED blockNameSig)		; Switch - turnout - right side, left side or both sides key-locked control position symbols (3 symbols generates here)
	(setq itemNumber 0)
	(repeat (length switchDrawingList)
		(setq drawingNumber (nth itemNumber switchDrawingList))
		(setq quadrant 1)
		(SWITCH-SYMBOL-SIGNALLING-CIRCLE-AT-TANGENT-INTERSECTION drawingNumber blockNameSig) ; Used in the signaling symbols (same for all variants fylt/tom/lett per switch type)
		(TraceLevel3 (strcat "...SWITCH DRAWING #: " drawingNumber))
		(repeat 4
			(SWITCH-SYMBOL-SIGNALLING quadrant drawingNumber blockNameSig descriptionSig) ; Signalling discipline symbols in switch object
			(SWITCH-SYMBOL-SUPERSTRUCTURE quadrant drawingNumber) ; Track discipline symbols in switch object
			(SWITCH-SYMBOL-HIGH-VOLTAGE quadrant drawingNumber) ; Catenary discipline symbols in switch object
			(setq quadrant (+ 1 quadrant))
		)
		(setq itemNumber (+ 1 itemNumber))
	)
)



(defun getSwitchParameters ( switchDrawingNumber / )
	; Concerning Bane NOR (Norway):
	; Source: Excel file NO-BN Switches.xlsx.
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
	; BK: bakkant sporveksel / rear end of switch
	; R2: sirkelkurvens endepunkt i avvik / circular arc's end in the deviating track
	; TK: teoretisk kryss / Theoretical Crossing = point of tangent intersections from rear ends of straight and deviating tracks
	; L: byggelengde / Constructive length
	; A: tangentlengde/lengde i X-retning før teoretisk kryss / Tangent line from Stock rail joint to TK
	;  tangentlengde til sirkelkurven etter teoretisk kryss / Tangent line from rear end of deviating track's circular arc section
	; D: rettlinjet parti i avvik / straight section in deviating track, after the circular arc section, up to the end of the factory-delivered constructive length
	; B: lengde i X-retning av C + D / Sum of C and D
	; E: lengde av parti med langsviller utenfor BK / Section with long sleepers (both tracks on same sleeper) after switch's constructive end
	; F: lengde av parti med kortviller utenfor BK	/ Section with shortened sleepers up to the point where normal sleepers can be used (with enough track separation)
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
		(assoc switchDrawingNumber ; list was copied from Bane NOR TRV. See data source in Excel file "NO-BN Switches.xlsx" located close to this LISP source file.
			(list	                  
				;     Drawing number            Rail Type              Sleeper Type
				(list "KO-800157"   (list '("RailType" "54E3") '("SleeperType" Betong) '("x" 7    ) '("R" 190 ) '("A" 13503 ) '("B" 13503) '("C" 13503	) '("D" 0   ) '("L" 27006 ) '("E" 3654 ) '("F" 2375) '("SwitchDiamondType" "FX")))
				(list "KO-701334"   (list '("RailType" "54E3") '("SleeperType" Betong) '("x" 9    ) '("R" 190 ) '("A" 10523 ) '("B" 16616) '("C" 10523	) '("D" 6093) '("L" 27139 ) '("E" 3000 ) '("F" 3575) '("SwitchDiamondType" "FX")))
																																																				
				(list "KO-701287"   (list '("RailType" "54E3") '("SleeperType" Betong) '("x" 9    ) '("R" 300 ) '("A" 16616 ) '("B" 16616) '("C" 16616	) '("D" 0   ) '("L" 33231 ) '("E" 3575 ) '("F" 3000) '("SwitchDiamondType" "FX")))
				(list "KO-701306"   (list '("RailType" "54E3") '("SleeperType" Betong) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 20797) '("C" 20797	) '("D" 0   ) '("L" 41594 ) '("E" 5835 ) '("F" 4775) '("SwitchDiamondType" "FX")))
				(list "KO-701319"   (list '("RailType" "54E3") '("SleeperType" Betong) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 3900 ) '("F" 5375) '("SwitchDiamondType" "FX")))
																																																				
				(list "KO-701409"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 9    ) '("R" 300 ) '("A" 16615 ) '("B" 16615) '("C" 16615	) '("D" 0   ) '("L" 33230 ) '("E" 5400 ) '("F" 2350) '("SwitchDiamondType" "FX")))
				(list "KO-800068-2" (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 11.66) '("R" 500 ) '("A" 21393 ) '("B" 21391) '("C" 21393	) '("D" 0   ) '("L" 42783 ) '("E" 7314 ) '("F" 2400) '("SwitchDiamondType" "FX")))
				(list "KO-800068"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 21985) '("C" 20797	) '("D" 1188) '("L" 42783 ) '("E" 7200 ) '("F" 3600) '("SwitchDiamondType" "FX")))
				(list "KO-701372"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 7200 ) '("F" 9000) '("SwitchDiamondType" "FX")))
				(list "KO-701382"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 15   ) '("R" 760 ) '("A" 25305 ) '("B" 28911) '("C" 25305	) '("D" 3606) '("L" 54216 ) '("E" 7200 ) '("F" 9600) '("SwitchDiamondType" "FX")))
																																																				
				(list "KO-800099"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 9    ) '("R" 300 ) '("A" 16615 ) '("B" 19760) '("C" 16615	) '("D" 3145) '("L" 36375 ) '("E" 1750 ) '("F" 2950) '("SwitchDiamondType" "BX")))
				(list "KO-065306"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 8.21 ) '("R" 300 ) '("A" 18188 ) '("B" 18188) '("C" 18188	) '("D" 0   ) '("L" 36375 ) '("E" 3550 ) '("F" 0   ) '("SwitchDiamondType" "BX")))
				(list "KO-800090"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 12   ) '("R" 500 ) '("A" 20797 ) '("B" 23306) '("C" 20797	) '("D" 2509) '("L" 44103 ) '("E" 5994 ) '("F" 0   ) '("SwitchDiamondType" "BX")))
				(list "KO-800108"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 14   ) '("R" 760 ) '("A" 27108 ) '("B" 27108) '("C" 27108	) '("D" 0   ) '("L" 54216 ) '("E" 10825) '("F" 9000) '("SwitchDiamondType" "BX")))
				(list "KO-800164"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 15   ) '("R" 760 ) '("A" 25305 ) '("B" 28911) '("C" 25305	) '("D" 3606) '("L" 54216 ) '("E" 2400 ) '("F" 9600) '("SwitchDiamondType" "BX")))
				(list "KO-800081"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 18.4 ) '("R" 1200) '("A" 32829 ) '("B" 34429) '("C" 34429	) '("D" 0   ) '("L" 67257 ) '("E" 10781) '("F" 3600) '("SwitchDiamondType" "BX")))
				(list "KO-701399"   (list '("RailType" "60E1") '("SleeperType" Betong) '("x" 26.1 ) '("R" 2500) '("A" 48109 ) '("B" 46491) '("C" 46491	) '("D" 0   ) '("L" 94600 ) '("E" 17400) '("F" 0   ) '("SwitchDiamondType" "BX")))
			);list																														
		);assoc
	);cadr
)
