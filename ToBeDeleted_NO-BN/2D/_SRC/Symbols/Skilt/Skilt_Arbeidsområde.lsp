(defun C:ARBEIDSOMRAADESKILT (/)
	(Skilt_Arbeidsomraade_1-2)
	(Skilt_Arbeidsomraade_1-3)
	(Skilt_Arbeidsomraade_2-2)
  	(Skilt_Arbeidsomraade_2-3)
	(Skilt_Arbeidsomraade_3)
)
  
(defun MakeArrow (tip ang height / legLength p1 p2 p3)
	(setq legLenth (/ height (cos (/ pi 4)))
		  p1 tip
		  p2 (polar p1 (+ ang (* (/ pi 4) 3)) legLenth)
		  p3 (polar p1 (+ ang (* (/ pi 4) 5)) legLenth)
	)
	(list p2 p1 p3)
)

(defun ConnectArrows (arrow1 arrow2)
	(append arrow1 (reverse arrow2) (list (car arrow1)))
)

(defun MakeThickArrow(tip ang height thickness / offset secondTip secondHeight arrow1 arrow2)
	(setq offset (/ thickness (cos (/ pi 4)))
		  secondTip (polar tip (+ ang pi) offset)
		  secondHeight (- height offset)
		  arrow1 (MakeArrow tip ang height)
		  arrow2 (MakeArrow secondTip ang secondHeight)
	)
    (ConnectArrows arrow1 arrow2)
)

(defun DrawThickArrow(tip ang height thickness hatchSize / thickArrow hatchPoint)
	(setq Arrow (MakeArrow tip ang height)
	      ;hatchPoint (polar tip (+ ang pi) thickness)
	)
    (command "_.LINE" (mapcar 'command Arrow))
	; (drawHatchSelectPoint hatchSize hatchPoint 315 0)
)


(defun Skilt_Arbeidsomraade_1-2 (/ blockName description height width botLeft topRight arrowHeight borderToArrowDist arrowThickness hatchSize textHeight leftArrowAngle rightArrowAngle arrow1Tip arrow2Tip textPos1 textPos1 textPos1 scaleFactor)
	(setq scaleFactor 20.0
		blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-1-2"
		description "Skilt Arbeidsområde 1-2"
		height (+ (* scaleFactor (/ 150 1000.0)) 0.5)
		width (* scaleFactor (/ 500 1000.0))
		botLeft (list 0 0)
		topRight (list width height)
		arrowHeight (* scaleFactor (/ 65 1000.0))
		borderToArrowDist (* scaleFactor (/ 7 1000.0))
		arrowThickness (* scaleFactor (/ 15 1000.0))
		hatchSize (* scaleFactor 0.0025)
		textHeight (* scaleFactor 0.045)
		leftArrowAngle pi
		rightArrowAngle 0
		arrow1Tip (list borderToArrowDist (/ height 2))
		arrow2Tip (list (- width borderToArrowDist) (/ height 2))
		textPos1 (list (/ width 2) (* 3(/ height 4)))
		textPos2 (list (/ width 2) (* 1(/ height 4)))
	)
	(command
		"._RECTANG" botLeft topRight
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0"
		""
	)  
	(DrawThickArrow arrow1Tip leftArrowAngle arrowHeight arrowThickness hatchSize)
	(DrawThickArrow arrow2Tip rightArrowAngle arrowHeight arrowThickness hatchSize) 
	(addAtt "TEKST1" "Tekst 1" "Arbeidsområde" textPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST2" "Tekst 2" "OSL 1" textPos2 textHeight 0 "iso" "MC" 16)
	(command "._MOVE" "ALL" "" (list (/ width 2) 0) (list 0 0))
	(newBlock blockName)
	blockName
)

(defun Skilt_Arbeidsomraade_1-3(/ blockName description height width botLeft topRight arrowHeight borderToArrowDist arrowThickness hatchSize textHeight leftArrowAngle rightArrowAngle arrow1Tip arrow2Tip textPos1 textPos1 textPos1 scaleFactor)
	(setq scaleFactor 20.0
		blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-1-3"
		description "Skilt Arbeidsområde 1-3"
		height (+ 0.5 (* scaleFactor (/ 150 1000.0)))
		width (* scaleFactor (/ 500 1000.0))
		botLeft (list 0 0)
		topRight (list width height)
		arrowHeight (* scaleFactor (/ 65 1000.0))
		borderToArrowDist (* scaleFactor (/ 7 1000.0))
		arrowThickness (* scaleFactor (/ 15 1000.0))
		hatchSize (* scaleFactor 0.0025)
		textHeight (* scaleFactor 0.035)
		leftArrowAngle pi
		rightArrowAngle 0
		arrow1Tip (list borderToArrowDist (/ height 2))
		arrow2Tip (list (- width borderToArrowDist) (/ height 2))
		textPos1 (list (/ width 2) (- (* 5(/ height 6)) 0.165))
		textPos2 (list (/ width 2) (/ height 2))
		textPos3 (list (/ width 2) (+ (/ height 6) 0.165))
	)
	(command
		"._RECTANG" botLeft topRight
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" 
		""
	)  
	(DrawThickArrow arrow1Tip leftArrowAngle arrowHeight arrowThickness hatchSize)
	(DrawThickArrow arrow2Tip rightArrowAngle arrowHeight arrowThickness hatchSize) 
	(addAtt "TEKST1" "Tekst 1" "" textPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST2" "Tekst 2" "Arbeidsområde" textPos2 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST3" "Tekst 3" "" textPos3 textHeight 0 "iso" "MC" 16)
	(command "._MOVE" "ALL" "" (list (/ width 2) 0) (list 0 0))
	(newBlock blockName)
	blockName
)

(defun Skilt_Arbeidsomraade_2-2( / blockName description height width botLeft topRight midBarWidth midBarBotLeft midBarTopRight midBarHatchPoint arrowHeight borderToArrowDist arrowThickness hatchSize hatchSize leftArrowAngle rightArrowAngle arrow1Tip arrow2Tip leftTextPos leftTextPos1 leftTextPos2 leftTextPos3 rightTextPos1 rightTextPos2 rightTextPos3 scaleFactor)
	(setq
		scaleFactor 20.0
	    blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-2-2"
		description "Skilt Arbeidsområde 2-2"
		height (+ 0.5 (* scaleFactor (/ 150 1000.0)))
		width (* scaleFactor (/ 1000 1000.0))
		botLeft (list 0 0)
		topRight (list width height)
		midBarWidth (* scaleFactor (/ 7 1000.0))
		midBarBotLeft (list (- (/ width 2) (/ midBarWidth 2)) 0)
		midBarTopRight (list (+ (/ width 2) (/ midBarWidth 2)) height)
		midBarHatchPoint (list (/ width 2) (/ height 2))
		arrowHeight (* scaleFactor (/ 65 1000.0))
		borderToArrowDist (* scaleFactor (/ 7 1000.0))
		arrowThickness (* scaleFactor (/ 15 1000.0))
		hatchSize (* scaleFactor 0.0025)
		textHeight (* scaleFactor 0.045)
		leftArrowAngle pi
		rightArrowAngle 0
		arrow1Tip (list borderToArrowDist (/ height 2))
		arrow2Tip (list (- width borderToArrowDist) (/ height 2))
		leftTextPos1 (list (+ (/ width 4) (/ (+ arrowHeight borderToArrowDist) 2)) (* 3(/ height 4)))
		leftTextPos2 (list (+ (/ width 4) (/ (+ arrowHeight borderToArrowDist) 2)) (* 1(/ height 4)))
		rightTextPos1 (list (- (* (/ width 4) 3) (/ (+ arrowHeight borderToArrowDist) 2)) (* 3(/ height 4)))
		rightTextPos2 (list (- (* (/ width 4) 3) (/ (+ arrowHeight borderToArrowDist) 2)) (* 1(/ height 4)))
	)

	(command
		"._RECTANG" botLeft topRight
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0"
		""
	)
    (command "._RECTANG" midBarBotLeft midBarTopRight)
	(DrawThickArrow arrow1Tip leftArrowAngle arrowHeight arrowThickness hatchSize)
	(DrawThickArrow arrow2Tip rightArrowAngle arrowHeight arrowThickness hatchSize)
	; (drawHatchSelectPoint hatchSize midBarHatchPoint 315 0)
	(addAtt "TEKST1" "Venstretekst 1" "Arbeidsområde" leftTextPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST2" "Venstretekst 2" "OSL 1" leftTextPos2 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST4" "Høyretekst 1" "Arbeidsområde" rightTextPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST5" "Høyretekst 2" "OSL 3" rightTextPos2 textHeight 0 "iso" "MC" 16)
	(command "._MOVE" "ALL" "" (list (/ width 2) 0) (list 0 0))
	(newBlock blockName)
	blockName
)

(defun Skilt_Arbeidsomraade_2-3( / blockName description height width botLeft topRight midBarWidth midBarBotLeft midBarTopRight midBarHatchPoint arrowHeight borderToArrowDist arrowThickness hatchSize hatchSize leftArrowAngle rightArrowAngle arrow1Tip arrow2Tip leftTextPos leftTextPos1 leftTextPos2 leftTextPos3 rightTextPos1 rightTextPos2 rightTextPos3 scaleFactor)
	(setq
		scaleFactor 20.0
	    blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-2-3"
		description "Skilt Arbeidsområde 2-3"
		height (+ 0.5 (* scaleFactor (/ 150 1000.0)))
		width (* scaleFactor (/ 1000 1000.0))
		botLeft (list 0 0)
		topRight (list width height)
		midBarWidth (* scaleFactor (/ 7 1000.0))
		midBarBotLeft (list (- (/ width 2) (/ midBarWidth 2)) 0)
		midBarTopRight (list (+ (/ width 2) (/ midBarWidth 2)) height)
		midBarHatchPoint (list (/ width 2) (/ height 2))
		arrowHeight (* scaleFactor (/ 65 1000.0))
		borderToArrowDist (* scaleFactor (/ 7 1000.0))
		arrowThickness (* scaleFactor (/ 15 1000.0))
		hatchSize (* scaleFactor 0.0025)
		textHeight (* scaleFactor 0.035)
		leftArrowAngle pi
		rightArrowAngle 0
		arrow1Tip (list borderToArrowDist (/ height 2))
		arrow2Tip (list (- width borderToArrowDist) (/ height 2))
		leftTextPos1 (list (+ (/ width 4) (/ (+ arrowHeight borderToArrowDist) 2)) (- (* 5(/ height 6)) 0.165))
		leftTextPos2 (list (+ (/ width 4) (/ (+ arrowHeight borderToArrowDist) 2)) (/ height 2))
		leftTextPos3 (list (+ (/ width 4) (/ (+ arrowHeight borderToArrowDist) 2)) (+ (/ height 6) 0.165))
		rightTextPos1 (list (- (* (/ width 4) 3) (/ (+ arrowHeight borderToArrowDist) 2)) (- (* 5(/ height 6)) 0.165))
		rightTextPos2 (list (- (* (/ width 4) 3) (/ (+ arrowHeight borderToArrowDist) 2)) (/ height 2))
		rightTextPos3 (list (- (* (/ width 4) 3) (/ (+ arrowHeight borderToArrowDist) 2)) (+ (/ height 6) 0.165))
	)
	(command
		"._RECTANG" botLeft topRight
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0"
		""
	)
	(DrawThickArrow arrow1Tip leftArrowAngle arrowHeight arrowThickness hatchSize)
	(DrawThickArrow arrow2Tip rightArrowAngle arrowHeight arrowThickness hatchSize)
	(command "._RECTANG" midBarBotLeft midBarTopRight)
	; (drawHatchSelectPoint hatchSize midBarHatchPoint 315 0)
	(addAtt "TEKST1" "Venstretekst 1" "Venstre" leftTextPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST2" "Venstretekst 2" "Arbeidsområde" leftTextPos2 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST3" "Venstretekst 3" "OSL 1" leftTextPos3 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST4" "Høyretekst 1" "Høyre" rightTextPos1 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST5" "Høyretekst 2" "Arbeidsområde" rightTextPos2 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST6" "Høyretekst 3" "OSL 3" rightTextPos3 textHeight 0 "iso" "MC" 16)
	(command "._MOVE" "ALL" "" (list (/ width 2) 0) (list 0 0))
	(newBlock blockName)
	blockName
)

(defun Skilt_Arbeidsomraade_3(/ blockName description height width botLeft topRight textHeight smallTextHeight textPos1 textPos2 textPos3 scaleFactor)
	(setq scaleFactor 33.3
		blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-3"
		description "Skilt Arbeidsområde 3"
		height (+ 0.5 (* scaleFactor (/ 200 1000.0)))
		width (* scaleFactor (/ 350 1000.0))
		botLeft (list 0 0)
		topRight (list width height)
		textHeight 0.9
		smallTextHeight 0.7
		textPos1 (list (/ width 2) 6.316)
		textPos2 (list (/ width 2) 5.317)
		textPos3 (list (/ width 2) 4.257)
		textPos4 (list (/ width 2) 3.146)
		textPos5 (list (/ width 2) 2.02)
		textPos6 (list (/ width 2) 0.876)
	)
	(command "._RECTANG" botLeft topRight
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
	(addAtt "TEKST1" "Tekst 1" "" textPos1 smallTextHeight 0 "iso" "MC" 16)
	(addAtt "TEKST2" "Tekst 2" "Port til:" textPos2 smallTextHeight 0 "iso" "MC" 16)
	(addAtt "TEKST3" "Tekst 3" "" textPos3 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST4" "Tekst 4" "Arbeidsområde" textPos4 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST5" "Tekst 5" "OSL-OSL" textPos5 textHeight 0 "iso" "MC" 16)
	(addAtt "TEKST6" "Tekst 6" "" textPos6 textHeight 0 "iso" "MC" 16)
	(command "._MOVE" "ALL" "" (list (/ width 2) 0) (list 0 0))
	(newBlock blockName)
	blockName
)
