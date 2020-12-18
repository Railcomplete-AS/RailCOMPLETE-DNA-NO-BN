;=========================================================================================================================
;
; Switch Symbol High Voltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
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

(defun SWITCH-SYMBOL-HIGH-VOLTAGE (quadrant Drawing_Number 
	/ 
		blockName
		switchParameters
		crossType
		A B R x railProfile ang 
		str i hatchVariant
	)
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		crossType 	(cadr (assoc "SwitchCrossing" switchParameters))
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "RailProfile" switchParameters))
		ang	  	(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
	)
	(setq str "")
	(if (< x 10)
		(setq str "0")
	)
	(setq i 0)
	(repeat 2
		(cond
			((= i 0) (setq hatchVariant "LEDNING-ENKELT-SPOR"))
			((= i 1) (setq hatchVariant "LEDNING-BEGGE-SPOR"))
		)
		(setq
			blockName (strcat "NO-BN-2D-JBTKL-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0))
		)
		(setLayer layer_Zero)
		(if (= hatchVariant "LEDNING-ENKELT-SPOR")
			(progn
				(command "._PLINE" (list A 0) (list (+ A (/ B 2.0)) 0) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closed_)
				(drawHatchOptions _mediumHatch_ 0 0 "ANSI37" "_LAST")
				(command	"._PLINE" (list (+ A (/ B 2.0)) 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) _closed_)
				(drawHatch _mediumHatch_)
			)
			(progn
				(command "._PLINE" (list A 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) _closed_)
				(drawHatchOptions _mediumHatch_ 0 0 "ANSI37" "_LAST")
			)
		)
		(command
			"._LINE" (list (+ A (* B 0.75)) 0) (list (+ A B) 0) ""
			"._LINE" (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A B) (* B (tan (D->R ang)))) ""
		)
		;Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(moveToQuadrant quadrant "_ALL")
		(createSchematicBlockFromCurrentGraphics blockName)
		(setq i (+ 1 i))
	);repeat
)