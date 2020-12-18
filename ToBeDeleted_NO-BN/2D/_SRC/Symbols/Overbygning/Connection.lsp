;
; Connection.lsp
;
; Relies on NO-BN-KO-SPV.lsp, NO-BN-EH-SPV.lsp, NO-BN-SA-SPV.lsp 
;

(defun C:CONNECTION (/)

	(CONNECTION-CONTINUATION)	; Connecting a track that continues over into another track. Note - symbol may be used by general alignment connections.
	(CONNECTION-CROSSING) 		; Crossing - two tracks that cross in the same plane, or as flyovers / tunnel/bridge over/under other track
	(CONNECTION-SWITCH) 		; Turnout - switch - point with recognized geometry
	(CONNECTION-SWITCH-UNSPECIFIED)	; Turnout - switch - point with unrecognized or unspecified geometry
)



(defun CONNECTION-CONTINUATION (/)
	;With mileage increasing towards right, the symbols look like this:
	(CONTINUATION 0 0) ; [>>]
	(CONTINUATION 0 1) ; [><]
	(CONTINUATION 1 0) ; [<>]
	(CONTINUATION 1 1) ; [<<]
)



(defun CONTINUATION (leftArrow rightArrow / blockName x y leftArrowStart leftArrowMid rightArrowStart rightArrowMid)
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-CONNECTION-CONTINUATION-" (rtos leftArrow 2 0) "-" (rtos rightArrow 2 0))
		x (/ 1.5 2.0)
		y (/ 1.0 2.0))
	(command
		"._LINE" (list (- x) (- y)) (list (- x) y) ""
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
		"._LINE" (list (- x) (/ y 2.0)) (list x (/ y 2.0)) ""
		"._MIRROR" "L" "" (list 0 0) (list 1 0) "N"
	)
	(if (= leftArrow 0)
		(setq 
			leftArrowStart (- (* 2.0 (/ x 3.0)))
			leftArrowMid (- (/ x 3.0))
		)
		;else:
		(setq 
			leftArrowStart (- (/ x 3.0))
			leftArrowMid (- (* 2.0 (/ x 3.0)))
		)
	)
	(if (= rightArrow 0)
		(setq 
			rightArrowStart (/ x 3.0)
			rightArrowMid (* 2.0 (/ x 3.0))
		)
		(setq 
			rightArrowStart (* 2.0 (/ x 3.0))
			rightArrowMid (/ x 3.0)
		)
	)
	(command
		"._LINE"
			(list leftArrowStart (/ y 2.0))
			(list leftArrowMid 0)
			(list leftArrowStart (- (/ y 2.0))) 
			""
		"._LINE"
			(list rightArrowStart (/ y 2.0))
			(list rightArrowMid 0)
			(list rightArrowStart (- (/ y 2.0))) 
			""
	)
	(newBlock blockName)
	blockName
)



; Circle with Andrew cross
(defun CONNECTION-CROSSING (/ blockName radius )
	(setq
		blockName "NO-BN-2D-JBTOB-CONNECTION-CROSSING" 
		radius 0.5 ; Radius of 180 deg half-circle arc, to be shown at the ends of a long horizontal and short vertical line meeting at the crossing
	)
	(command
		"._LAYER" "SET" "0" ""
		"._LINE" (list (* -4 radius) 0) (list (* 4 radius) 0) "" ; long horizontal line
		"._LINE" (list 0 (* -2 radius)) (list 0 (* 2 radius)) "" ; short vertical line
		"ARC" "CE" (list (* -5 radius) 0) (list (* -5 radius) (* -1 radius)) (list (* radius -5) (* 1 radius)) ; Left arc
		"ARC" "CE" (list (* 5 radius) 0) (list (* 5 radius) (* 1 radius)) (list (* radius 5) (* -1 radius)) ; Right arc
		"ARC" "CE" (list 0 (* -3 radius)) (list (* 1 radius) (* -3 radius)) (list (* -1 radius) (* -3 radius)) ; Bottom arc
		"ARC" "CE" (list 0 (* 3 radius)) (list (* -1 radius) (* 3 radius)) (list (* 1 radius) (* 3 radius)) ; Top arc
	)
    (newBlock blockName)
)



; Symbol to be used when the geometry for a branch is not recognized by RC - re-use Crossing symbol.
; TODO: Make a better 'unspecified' symbol.
(defun CONNECTION-SWITCH-UNSPECIFIED ( / blockName radius )
	(setq
		blockName "NO-BN-2D-JBTOB-CONNECTION-SWITCH-UNSPECIFIED"
		u 1 ; unit
	)
	(command "._LAYER" "SET" "0" "")

	 ; Quadrant 1
	 (command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint 0.1 (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
    (newBlock (strcat blockName "-1" ))

	 ; Quadrant 2
	(command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint 0.1 (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "ALL" "" (list 0 0) (list 0 1) "Y")
	(newBlock (strcat blockName "-2" ))

	 ; Quadrant 3
	 (command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint 0.1 (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "ALL" "" (list 0 0) (list 0 1) "Y")
	(command "._MIRROR" "ALL" "" (list 0 0) (list 1 0) "Y")
    (newBlock (strcat blockName "-3" ))
	
	 ; Quadrant 4
	(command 
		"._PLINE" (list 0 u) (list 0 (* -1 u)) "" ; Stock rail joint
		"._PLINE" (list (* 2 u) 0) (list (* 4 u) (* 2 u)) (list (* 4 u) 0) (list 0 0) ""
	)
	(drawHatchOptionsSelectPoint 0.1 (list (* 3 u) (* 0.5 u)) -45 0 "ANSI31")
	(command "._MIRROR" "ALL" "" (list 0 0) (list 1 0) "Y")
    (newBlock (strcat blockName "-4" ))
)



; Ref: Bane NOR standard tegninger for sporveksler (tegningsnummer KO.nnnnnn)
(defun CONNECTION-SWITCH (/ Switch_Drawing_List element_no Drawing_Number quadrant)
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
	(NO-BN-SA-SPV-KONTROLL)		; Turnout - switch - right side, left side or both sides key-locked control position symbols (3 symbols generates here)
	(setq element_no 0)
	(repeat (length Switch_Drawing_List)
		(setq Drawing_Number (nth element_no Switch_Drawing_List))
		(setq quadrant 1)
		(FILLED-CIRCLE Drawing_Number) ; Used in the signaling symbols (same for all variants fylt/tom/lett per switch type)
		(repeat 4
			(NO-BN-SA-SPV quadrant Drawing_Number) ; Signaling discipline symbols in switch object
			(NO-BN-KO-SPV quadrant Drawing_Number) ; Track discipline symbols in switch object
			(NO-BN-EH-SPV quadrant Drawing_Number) ; Catenary discipline symbols in switch object
			(setq quadrant (+ 1 quadrant))
		)
		(setq element_no (+ 1 element_no))
	)
)



; Generate symbol for filled circle at theoretical crossing in switch - used to signify "motorized hand-thrown with push-button operation"
; German: "Elektrisch Ortsgestellte Weichen", EOW.
(defun FILLED-CIRCLE (Drawing_Number 
	/ 	blockName
		switchParameters
		A  radius
	)
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		A	  		(/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius 	  	0.75
	)
	(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTION-FILLED-CIRCLE" "-" (rtos A 2 3)))
	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByBlock"
		"._CIRCLE" (list A 0) radius
	)
	(drawHatch 0.01)
	(newBlock blockName)
)



;TODO 2019-05-08 CLFEY: Det mangler data for de fleste sporvekslene
(defun getArea1 (Switch_Drawing_Number)
	; Forbudtområde for akseltellere - område 1
	(cadr
		(assoc Switch_Drawing_Number
			(list	 
				(list 	"KO-800157"	nil)
				(list 	"KO-701334"	(list "-0.43,-1.25" "15.5364,-1.2457" "15.4105,1.8757" "-0.43,1.25" "Close"))
						
				(list 	"KO-701287"	nil)
				(list 	"KO-701306"	nil)
				(list 	"KO-701319"	nil)
				
				(list 	"KO-701409"	(list "-0.4320,-1.2975" "19.2564,-1.2933" "19.1546,1.8757" "-0.4320,1.2975" "Close"))
				(list 	"KO-800068-2" (list "-0.4320,-1.2975" "24.7622,-1.2919" "24.6836,1.9032" "-0.4320,1.2975" "Close"))
				(list 	"KO-800068"	(list "-0.4320,-1.2975" "24.7622,-1.2919" "24.6836,1.9032" "-0.4320,1.2975" "Close"))
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



;TODO 2019-05-08 CLFEY: Det mangler data for de fleste sporvekslene
(defun getArea2 (Switch_Drawing_Number)
	;Forbudtområde for akseltellere - område 2
	(cadr
		(assoc Switch_Drawing_Number
			(list	 
				(list 	"KO-800157"	nil)
				(list 	"KO-701334"	(list "19.5777,-1.2575" "19.5258,-0.2513" "18.3102,-0.3116" "18.2375,1.1904" "19.4483,1.2506" "19.3964,2.2568" "27.3973,3.1264" "27.4662,1.8826" "30.0187,2.1662" "30.1387,0" "27.5705,0" "27.6393,-1.2433" "Close"))
						
				(list 	"KO-701287"	nil)
				(list 	"KO-701306"	nil)
				(list 	"KO-701319"	nil)
	
				(list 	"KO-701409"	(list "25.5829,-1.3027" "25.5366,-0.2189" "23.1623,-0.3150" "23.1033,1.2070" "25.4716,1.3032" "25.4253,2.3869" "33.4828,3.1717" "33.5542,1.8823" "35.9395,2.1473" "36.0585,0" "33.6585,0" "33.7299,-1.2894" "Close"))
				(list 	"KO-800068-2" (list "46.2083,0" "46.1204,2.1120" "41.9549,1.7646" "41.9012,3.0543" "33.5221,2.4215" "33.5591,1.3251" "29.9200,1.2094" "29.9658,-0.3127" "33.6104,-0.1970" "33.6104,-1.2934" "42.0820,-1.2910" "42.0283,0" "Close"))
				(list 	"KO-800068"	(list "46.2083,0" "46.1204,2.1120" "41.9549,1.7646" "41.9012,3.0543" "33.5221,2.4215" "33.5591,1.3251" "29.9200,1.2094" "29.9658,-0.3127" "33.6104,-0.1970" "33.6104,-1.2934" "42.0820,-1.2910" "42.0283,0" "Close"))
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
	; Limes inn fra Sporveksler.xlsx
	(cadr
		(assoc Switch_Drawing_Number ; list was copied from Bane NOR TRV:
			(list	 
				(list 	"KO-800157"	(list	 '("Skinneprofil" 	"54E3"	)'("Sviller"	Betong	) '("x" 	7		) '("R" 	190	) '("A" 	13503	) '("B" 	13503	) '("C" 	13503	) '("D" 	0	) '("L" 	27006	) '("E" 	3654	) '("F" 	2375	) '("Masse" 	0.0		) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-701334"	(list	 '("Skinneprofil" 	"54E3"	)'("Sviller"	Betong	) '("x" 	9		) '("R" 	190	) '("A" 	10523	) '("B" 	16616	) '("C" 	10523	) '("D" 	6093) '("L" 	27139	) '("E" 	3000	) '("F" 	3575	) '("Masse" 	36.3	) '("Skinnekryss" 	"FX"	)))
																														
				(list 	"KO-701287"	(list	 '("Skinneprofil" 	"54E3"	)'("Sviller"	Betong	) '("x" 	9		) '("R" 	300	) '("A" 	16616	) '("B" 	16616	) '("C" 	16616	) '("D" 	0	) '("L" 	33231	) '("E" 	3575	) '("F" 	3000	) '("Masse" 	40.5	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-701306"	(list	 '("Skinneprofil" 	"54E3"	)'("Sviller"	Betong	) '("x" 	12		) '("R" 	500	) '("A" 	20797	) '("B" 	20797	) '("C" 	20797	) '("D" 	0	) '("L" 	41594	) '("E" 	5835	) '("F" 	4775	) '("Masse" 	50.3	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-701319"	(list	 '("Skinneprofil" 	"54E3"	)'("Sviller"	Betong	) '("x" 	14		) '("R" 	760	) '("A" 	27108	) '("B" 	27108	) '("C" 	27108	) '("D" 	0	) '("L" 	54216	) '("E" 	3900	) '("F" 	5375	) '("Masse" 	52.0	) '("Skinnekryss" 	"FX"	)))
	
				(list 	"KO-701409"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	9		) '("R" 	300	) '("A" 	16615	) '("B" 	16615	) '("C" 	16615	) '("D" 	0	) '("L" 	33230	) '("E" 	5400	) '("F" 	2350	) '("Masse" 	43.4	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-800068-2" (list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	11.66	) '("R" 	500	) '("A" 	21393	) '("B" 	21391	) '("C" 	21393	) '("D" 	0	) '("L" 	42783	) '("E" 	7314	) '("F" 	2400	) '("Masse" 	53.8	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-800068"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	12		) '("R" 	500	) '("A" 	20797	) '("B" 	21985	) '("C" 	20797	) '("D" 	1188) '("L" 	42783	) '("E" 	7200	) '("F" 	3600	) '("Masse" 	53.8	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-701372"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	14		) '("R" 	760	) '("A" 	27108	) '("B" 	27108	) '("C" 	27108	) '("D" 	0	) '("L" 	54216	) '("E" 	7200	) '("F" 	9000	) '("Masse" 	57.2	) '("Skinnekryss" 	"FX"	)))
				(list 	"KO-701382"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	15		) '("R" 	760	) '("A" 	25305	) '("B" 	28911	) '("C" 	25305	) '("D" 	3606) '("L" 	54216	) '("E" 	7200	) '("F" 	9600	) '("Masse" 	54.2	) '("Skinnekryss" 	"FX"	)))
																														
				(list 	"KO-800099"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	9		) '("R" 	300	) '("A" 	16615	) '("B" 	19760	) '("C" 	16615	) '("D" 	3145) '("L" 	36375	) '("E" 	1750	) '("F" 	2950	) '("Masse" 	55.2	) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-065306"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	8.21	) '("R" 	300	) '("A" 	18188	) '("B" 	18188	) '("C" 	18188	) '("D" 	0	) '("L" 	36375	) '("E" 	3550	) '("F" 	0		) '("Masse" 			) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-800090"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	12		) '("R" 	500	) '("A" 	20797	) '("B" 	23306	) '("C" 	20797	) '("D" 	2509) '("L" 	44103	) '("E" 	5994	) '("F" 	0		) '("Masse" 	56.2	) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-800108"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	14		) '("R" 	760	) '("A" 	27108	) '("B" 	27108	) '("C" 	27108	) '("D" 	0	) '("L" 	54216	) '("E" 	10825	) '("F" 	9000	) '("Masse" 	57.2	) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-800164"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	15		) '("R" 	760	) '("A" 	25305	) '("B" 	28911	) '("C" 	25305	) '("D" 	3606) '("L" 	54216	) '("E" 	2400	) '("F" 	9600	) '("Masse" 	58.2	) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-800081"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	18.4	) '("R" 	1200) '("A" 	32829	) '("B" 	34429	) '("C" 	34429	) '("D" 	0	) '("L" 	67257	) '("E" 	10781	) '("F" 	3600	) '("Masse" 	59.2	) '("Skinnekryss" 	"BX"	)))
				(list 	"KO-701399"	(list	 '("Skinneprofil" 	"60E1"	)'("Sviller"	Betong	) '("x" 	26.1	) '("R" 	2500) '("A" 	48109	) '("B" 	46491	) '("C" 	46491	) '("D" 	0	) '("L" 	94600	) '("E" 	17400	) '("F" 	0		) '("Masse" 	60.2	) '("Skinnekryss" 	"BX"	)))
			);list																														
		);assoc
	);cadr
)


