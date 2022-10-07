;=========================================================================================================================
;
; Switch Symbol High Voltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See Bane NOR, TRV, Elektro/Høyspent, sporvekselsymboler

; Catenary system / line electrification discipline's graphics for inclusion in turnout symbols

;-------------------------------------------------------------------------------------------------------------------------
;
; Guide to understanding the RailCOMPLETE switch object symbols
; =============================================================
;
; See also guide in the source file for the GEOGRAPHICAL-CONNECTIONS LISP function.
;
; Square-hatched switch symbol means "non-electrified deviating track".
;
;-------------------------------------------------------------------------------------------------------------------------

(defun SWITCH-SYMBOL-HIGH-VOLTAGE ( quadrant drawingNumber
	/ 	blockName 
		description 
		switchParameters 
		switchDiamondType  A B C D E F L R x railProfile ang
		zeroPad
		variation
	)
	(setq
		switchParameters	(getSwitchParameters drawingNumber)
		switchDiamondType	(cadr (assoc "SwitchDiamondType" switchParameters))
		A					(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B					(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 					(cadr (assoc "R" switchParameters))
		x					(cadr (assoc "x" switchParameters))
		railProfile			(cadr (assoc "RailProfile" switchParameters))
		ang	  				(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
	)
	(cond 
		((= _ADM_ _XXGL_) (setq blockName1 (strcat _OCS_ "SWI-" "CONNECTOR-SWITCH"				)))
		((= _ADM_ _NOBN_) (setq blockName1 (strcat _OCS_ "SPV-" "FORBINDELSE-SPORVEKSEL"			)))
		((= _ADM_ _FRSR_) (setq blockName1 (strcat _OCS_ "AIG-" "CONNEXION-AIGUILLAGE"			)))
		((= _ADM_ _DEDB_) (setq blockName1 (strcat _OCS_ "WEI-" "VERBINDUNG-WEICHE"				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description1 (strcat "SWITCH, HIGH VOLTAGE SYMBOL"				)))
		((= _ADM_ _NOBN_) (setq description1 (strcat "SPORVEKSEL, KL-SYMBOL"						)))
		((= _ADM_ _FRSR_) (setq description1 (strcat "AIGUILLAGE, SYMBOLE TRACTION ELECTRIQUE"	)))
		((= _ADM_ _DEDB_) (setq description1 (strcat "WEICHE, OBERLEITUNG-SYMBOL"					)))
	)

	(if (< x 10)
		(setq zeroPad "0") ; Pad with leading zeros
	;else
		(setq zeroPad _emptyString_)
	)
	(foreach variation '("BOTH_LEGS_ELECTRIFIED" "ONE_LEG_ELECTRIFIED" "UNKNOWN_ELECTRIFICATION")
		(setq
			blockName	(strcat blockName1 "-"    zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
			description	(strcat description1 ", " zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
		)
		(SetLayer layDef_Zero)
		(cond
			((= variation "BOTH_LEGS_ELECTRIFIED")
				; Both legs equipped with overhead contact wire for electric traction:
				(command _POLYLINE_ (list A 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatchFromSelectionUsingStyle _mediumHatch_ _lastSelection_ _hatchPatternLosanges_)
			)
			((= variation "ONE_LEG_ELECTRIFIED")
				; Just one leg equipped:
				(command _POLYLINE_ (list A 0) (list (+ A (/ B 2.0)) 0) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatchFromSelectionUsingStyle _mediumHatch_ _lastSelection_ _hatchPatternLosanges_)
				(command _POLYLINE_ (list (+ A (/ B 2.0)) 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatch _mediumHatch_)
			)
			((= variation "UNKNOWN_ELECTRIFICATION")
				; No legs equipped:
				(command _POLYLINE_ (list A 0) (list (+ A (/ B 2.0)) 0) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
				(command _POLYLINE_ (list (+ A (/ B 2.0)) 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
			)
		)
		(command
			_LINE_ (list (+ A (* B 0.75)) 0) (list (+ A B) 0) _ENTER_
			_LINE_ (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A B) (* B (tan (D->R ang)))) _ENTER_
		)
		;Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(MoveToQuadrant quadrant _selectAll_)
		(AddDescriptionBelowOrigin description _two_)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)
