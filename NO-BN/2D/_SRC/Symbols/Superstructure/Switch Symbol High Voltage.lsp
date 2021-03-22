;=========================================================================================================================
;
; Switch Symbol High Voltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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

(defun SWITCH-SYMBOL-HIGH-VOLTAGE ( quadrant drawingNumber / blockName description switchParameters switchDiamondType A B R x railProfile ang str variation )
	(setq
		switchParameters	(getSwitchParameters drawingNumber)
		switchDiamondType	(cadr (assoc "SwitchCrossing" switchParameters))
		A					(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B					(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 					(cadr (assoc "R" switchParameters))
		x					(cadr (assoc "x" switchParameters))
		railProfile			(cadr (assoc "RailProfile" switchParameters))
		ang	  				(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
	)
	(if (< x 10)
		(setq str "0")
	;else
		(setq str "")
	)
	(foreach variation '("ELEKTRIFISERING-ETT-SPOR" "ELEKTRIFISERING-BEGGE-SPOR" "ELEKTRIFISERING-UKJENT")
		(setq
			blockName	(strcat "NO-BN-2D-JBTKL-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
			description	(strcat "SPORVEKSEL / KL-SYMBOL, "         str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
		)
		(SetLayer layDef_Zero)
		(cond
			((= variation "ELEKTRIFISERING-BEGGE-SPOR")
				; Both legs equipped with overhead contact wire for electric traction:
				(command _POLYLINE_ (list A 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatchFromSelectionUsingStyle _mediumHatch_ _lastSelection_ _hatchPatternLosanges_)
			)
			((= variation "ELEKTRIFISERING-ETT-SPOR")
				; Just one leg equipped:
				(command _POLYLINE_ (list A 0) (list (+ A (/ B 2.0)) 0) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatchFromSelectionUsingStyle _mediumHatch_ _lastSelection_ _hatchPatternLosanges_)
				(command _POLYLINE_ (list (+ A (/ B 2.0)) 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closedPolyline_)
				(DrawHatch _mediumHatch_)
			)
			((= variation "ELEKTRIFISERING-UKJENT")
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
		(AddDescriptionBelowOrigo description _two_)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)
