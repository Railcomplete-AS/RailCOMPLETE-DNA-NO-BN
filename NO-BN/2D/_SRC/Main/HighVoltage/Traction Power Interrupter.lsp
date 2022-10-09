;=========================================================================================================================
;
; Traction Power Interrupter.lsp 
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; OCS in/out interrupters, to be mounted on top of OCS masts

(defun ANYADM-TRACTION-POWER-INTERRUPTER ( / )
 	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "NOBN-TRACTION-POWER-INTERRUPTER")			(NOBN-TRACTION-POWER-INTERRUPTER)
)



(defun NOBN-TRACTION-POWER-INTERRUPTER ( / )
	(setq ; temp vars:
		distBelow				0
		tOcs					"KL"
		tInterrupterAbbrev		"NO-BN-2D-JBTEH_BRY"
		tInterrupter			(strcat "SKILLEBRYTER"		)
		tLoadInterrupter		(strcat "LASTSKILLEBRYTER"	)
		tCircuitInterrupter		(strcat "EFFEKTBRYTER"		)
		tEarthingInterrupter	(strcat "JORDINGSBRYTER"	)
		tRailEarthPotential		(strcat "SKINNEJORD"		)
		tOpen		 			(strcat _uARING_ "PEN"		)
		tClosed					(strcat "LUKKET"			)
		tTwoPole				(strcat "TOPOLET"			)
		tThreePole				(strcat "TREPOLET"			)
	)

	(TraceLevel3 "NOBN-HIGH-VOLTAGE-INTERRUPTER")			(NOBN-HIGH-VOLTAGE-INTERRUPTER)
  	(TraceLevel3 "NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER")		(NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER)
  	(TraceLevel3 "NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER")	(NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER)
   	(TraceLevel3 "NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER")	(NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER)

	; Purge temp vars after use:
	(setq 
		distBelow				nil
		tOcs					nil
		tInterrupterAbbrev		nil
		tInterrupter			nil
		tLoadInterrupter		nil
		tCircuitInterrupter		nil
		tEarthingInterrupter	nil
		tRailEarthPotential		nil
		tOpen		 			nil
		tClosed					nil
		tTwoPole				nil
		tThreePole				nil
	)
)



(defun NOBN-HIGH-VOLTAGE-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tOpen)
		description	(strcat tOcs " " tInterrupter ", " tOpen)
	)
	(DrawOcsInterrupter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tClosed)
		description	(strcat tOcs " " tInterrupter ", " tClosed)
	)
	(DrawOcsInterrupter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tTwoPole "-" tOpen)
		description	(strcat tOcs " " tInterrupter ", " tTwoPole " " tOpen)
	)
	(DrawOcsInterrupter tOpen 0)
	(DrawOcsInterrupter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tTwoPole "-" tClosed)
		description	(strcat tOcs " " tInterrupter ", " tTwoPole " " tClosed)
	)
	(DrawOcsInterrupter tClosed 0)
	(DrawOcsInterrupter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tThreePole "-" tOpen)
		description	(strcat tOcs " " tInterrupter ", " tThreePole " " tOpen)
	)
	(DrawOcsInterrupter tOpen 0)
	(DrawOcsInterrupter tOpen (- 6.0))
	(DrawOcsInterrupter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat tInterrupterAbbrev "-" tInterrupter "-" tThreePole "-" tClosed)
		description	(strcat tOcs " " tInterrupter ", " tThreePole " " tClosed)
	)
	(DrawOcsInterrupter tClosed 0)
	(DrawOcsInterrupter tClosed (- 6.0))
	(DrawOcsInterrupter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
	
	
	
(defun NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tLoadInterrupter "-" tOpen)
		description	(strcat tOcs " " tLoadInterrupter ", " tOpen)
	)
	(DrawOcsLoadInterrupter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tLoadInterrupter "-" tClosed)
		description	(strcat tOcs " " tLoadInterrupter ", " tClosed)
	)
	(DrawOcsLoadInterrupter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tLoadInterrupter "-" tTwoPole "-" tOpen)
		description	(strcat tOcs " " tLoadInterrupter ", " tTwoPole " " tOpen)
	)
	(DrawOcsLoadInterrupter tOpen 0)
	(DrawOcsLoadInterrupter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tLoadInterrupter "-" tTwoPole "-" tClosed)
		description	(strcat tOcs " " tLoadInterrupter ", " tTwoPole " " tClosed)
	)
	(DrawOcsLoadInterrupter tClosed 0)
	(DrawOcsLoadInterrupter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tLoadInterrupter "-" tThreePole "-" tOpen)
		description	(strcat tOcs " " tLoadInterrupter ", " tThreePole " " tOpen)
	)
	(DrawOcsLoadInterrupter tOpen 0)
	(DrawOcsLoadInterrupter tOpen (- 6.0))
	(DrawOcsLoadInterrupter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat"NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-TREPOLET-" tClosed)
		description	(strcat tOcs " " tLoadInterrupter ", " tThreePole " " tClosed)
	)
	(DrawOcsLoadInterrupter tClosed 0)
	(DrawOcsLoadInterrupter tClosed (- 6.0))
	(DrawOcsLoadInterrupter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tOpen)
		description	(strcat tOcs " " tCircuitInterrupter ", " tOpen)
	)
	(DrawOcsPowerSwitch tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tClosed)
		description	(strcat tOcs " " tCircuitInterrupter ", " tClosed)
	)
	(DrawOcsPowerSwitch tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tTwoPole "-" tOpen)
		description	(strcat tOcs " " tCircuitInterrupter ", " tTwoPole " " tOpen)
	)
	(DrawOcsPowerSwitch tOpen 0)
	(DrawOcsPowerSwitch tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tTwoPole "-" tClosed)
		description	(strcat tOcs " " tCircuitInterrupter ", " tTwoPole " " tClosed)
	)
	(DrawOcsPowerSwitch tClosed 0)
	(DrawOcsPowerSwitch tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tThreePole "-" tOpen)
		description	(strcat tOcs " " tCircuitInterrupter ", " tThreePole " " tOpen)
	)
	(DrawOcsPowerSwitch tOpen 0)
	(DrawOcsPowerSwitch tOpen (- 6.0))
	(DrawOcsPowerSwitch tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tCircuitInterrupter "-" tThreePole "-" tClosed)
		description	(strcat tOcs " " tCircuitInterrupter ", " tThreePole " " tClosed)
	)
	(DrawOcsPowerSwitch tClosed 0)
	(DrawOcsPowerSwitch tClosed (- 6.0))
	(DrawOcsPowerSwitch tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER ( / blockName description )
	;--------- Hand-thrown interrupter---------
	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tOpen)
		description	(strcat tOcs tEarthingInterrupter ", " tOpen)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tClosed)
		description	(strcat tOcs tEarthingInterrupter ", " tClosed)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tTwoPole "-" tOpen)
		description	(strcat tOcs tEarthingInterrupter ", " tTwoPole " " tOpen)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tOpen 0)
	(DrawOcsInterrupter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tTwoPole "-" tClosed)
		description	(strcat tOcs tEarthingInterrupter ", " tTwoPole " " tClosed)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tClosed 0)
	(DrawOcsInterrupter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tThreePole "-" tOpen)
		description	(strcat tOcs tEarthingInterrupter ", " tThreePole " " tOpen)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tOpen 0)
	(DrawOcsInterrupter tOpen (- 6.0))
	(DrawOcsInterrupter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat tInterrupterAbbrev "-" tEarthingInterrupter "-" tThreePole "-" tClosed)
		description	(strcat tOcs tEarthingInterrupter ", " tThreePole " " tClosed)
	)
	(DrawRailEarthPotential (list 5.5426 3.2))
	(DrawOcsInterrupter tClosed 0)
	(DrawOcsInterrupter tClosed (- 6.0))
	(DrawOcsInterrupter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawOcsInterrupter ( variation y / len len2 len3 )
	(setq 
		len 6.0
		len2 6.4
		len3 2.4
	)
	(SetLayer layDef_Zero)
	(if (= variation tOpen)
		(command 
			_LINE_ (list 0 y) (list len2 y) _ENTER_
			_ROTATE_ _lastSelection_ _ENTER_ (list 0 y) _angle30_
		)
		(command _LINE_ (list 0 y) (list len y) _ENTER_)
	)
	(command _LINE_ (list len (+ y (/ len3 2.0))) (list len (+ y (- (/ len3 2.0)))) _ENTER_)
)



(defun DrawOcsLoadInterrupter ( variation y / r len len2 len3 )
	(setq 
		r 0.8
		len 6.4
		len2 6.0
		len3 2.4
	)
	(SetLayer layDef_Zero)
	(if (= variation tOpen)
		(command
			_LINE_ (list 0 y) (list len y) _ENTER_
			_ROTATE_ _lastSelection_ _ENTER_ (list 0 y) _angle30_
		)
		(command _LINE_ (list 0 y) (list (- len2 (* r 2.0)) y) _ENTER_)
	)
	(command 
		_LINE_ (list len2 (+ y (/ len3 2.0))) (list len2 (+ y (- (/ len3 2.0)))) _ENTER_
		_CIRCLE_ (list (- len2 r) y) r
	)
)


(defun DrawOcsPowerSwitch ( variation y / r len len2 len3 )
	(setq 
		r 0.8
		len 6.4
		len2 6.0
		len3 1.2
	)
	(SetLayer layDef_Zero)
	(if (= variation tOpen)
		(command
			_LINE_ (list 0 y) (list len y) _ENTER_
			_ROTATE_ _lastSelection_ _ENTER_ (list 0 y) _angle30_
		)
		(command _LINE_ (list 0 y) (list len2 y) _ENTER_)
	)
	(command 
		_LINE_ (list (- len2 len3) (- y len3)) (list (+ len2 len3) (+ y len3)) _ENTER_
		_LINE_ (list (- len2 len3) (+ y len3)) (list (+ len2 len3) (- y len3)) _ENTER_
	)
)


(defun DrawRailEarthPotential ( offset / len1 len2 r x y )
	;
	;       ( ) 
	;        |
	; .------+
	;        |
	;     +--+--+
	;     |  *  |
	;     +--+--+
	;
	(setq 
		len1 4.0
		len2 (/ 2.5 2.0)
		r 0.5
		x (/ 2.5 2.0)
		y 0.9
	)
	(SetLayer layDef_Zero)
	(command 
		_LINE_ _origin_ (list len1 0) _ENTER_
		_LINE_ (list len1 len2) (list len1 (- len2)) _ENTER_
		_CIRCLE_ (list len1 (+ len2 r)) r
		_RECTANGLE_ (list (- len1 x) (- len2)) (list (+ len1 x) (- (+ len2 y)))
	)
	(DrawHatchAtPoint _solidHatch_ (list len1 (- (+ len2 (HalfOf y)))) _angleZero_ _offsetZero_)
	(if offset (MoveUp offset))
)



(defun drawVerticalLine(x y len)
	(command _LINE_ (list x y) (list x (- y len)) _ENTER_)
)



(defun DrawTwoPoleCrossLine (x y / edge space line )
	(setq 
    	edge 1.25
    	space 0.875
    	line 1.75
    )
	(drawVerticalLine x y edge)
	(drawVerticalLine x (- y edge space) line)
	(drawVerticalLine x (- y edge space line space) edge)	
)



(defun DrawThreePoleCrossLine (x y / edge space line )
	(setq 
    	edge 1.625
    	space 0.875
    	line 1.75
    )
	(drawVerticalLine x y edge)
	(drawVerticalLine x (- y edge space) line)
	(drawVerticalLine x (- y edge space line space) line)	
	(drawVerticalLine x (- y edge space line space line space) line)
	(drawVerticalLine x (- y edge space line space line space line space) edge)
)