;=========================================================================================================================
;
; ANYADM Traction Power Interrupter.lsp 
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

	; Implemented only for some administrations:
	(cond 
		((= _ADM_ _XXGL_) 
		)
		((= _ADM_ _NOBN_) 
			(TraceLevel3 "NOBN-TRACTION-POWER-INTERRUPTER")			(NOBN-TRACTION-POWER-INTERRUPTER)
		)
		((= _ADM_ _FRSR_) 
			; TODO - model FRSR objects, do not use NOBN objects:
			(TraceLevel3 "NOBN-TRACTION-POWER-INTERRUPTER")			(NOBN-TRACTION-POWER-INTERRUPTER)
		)
		((= _ADM_ _DEDB_) 
		)
		((= _ADM_ _JPTX_) 
			; TODO 2022-03-15 - Replace NOBN stuff with JPTX graphics:
			(TraceLevel3 "NOBN-TRACTION-POWER-INTERRUPTER")			(NOBN-TRACTION-POWER-INTERRUPTER)
		)
	)
)



(defun NOBN-TRACTION-POWER-INTERRUPTER ( / )
	(setq ; temp vars:
		distBelow				0
		;tInterrupterAbbrev		"BRY-" ; Bane NOR
		tOpen		 			(strcat _uARING_ "PEN"		)
		tClosed					(strcat "LUKKET"			)
		;tInterrupter			(strcat "SKILLEBRYTER"		)
		;tLoadInterrupter		(strcat "LASTSKILLEBRYTER"	)
		;tEarthingInterrupter	(strcat "JORDINGSBRYTER"	)
		;tRailEarth				(strcat "SKINNEJORD"		)
	)

	(TraceLevel3 "NOBN-HIGH-VOLTAGE-INTERRUPTER")			(NOBN-HIGH-VOLTAGE-INTERRUPTER)
  	(TraceLevel3 "NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER")		(NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER)
  	(TraceLevel3 "NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER")	(NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER)
   	(TraceLevel3 "NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER")	(NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER)

	; Purge temp vars after use:
	(setq 
		distBelow				nil
		;tInterrupterAbbrev		nil
		tOpen 					nil
		tClosed					nil
		;txtInterrupter			nil
		;txtLoadInterrupter		nil
		;txtEarthingInterrupter	nil
		;txtRailEarth			nil
	)
)



(defun NOBN-HIGH-VOLTAGE-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-" tOpen)
		description	(strcat "KL SKILLEBRYTER, " tOpen)
	)
	(DrawSkillebryter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-" tClosed)
		description	(strcat "KL SKILLEBRYTER, " tClosed)
	)
	(DrawSkillebryter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-TOPOLET-" tOpen)
		description	(strcat "KL SKILLEBRYTER, TOPOLET " tOpen)
	)
	(DrawSkillebryter tOpen 0)
	(DrawSkillebryter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-TOPOLET-" tClosed)
		description	(strcat "KL SKILLEBRYTER, TOPOLET " tClosed)
	)
	(DrawSkillebryter tClosed 0)
	(DrawSkillebryter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-TREPOLET-" tOpen)
		description	(strcat "KL SKILLEBRYTER, TREPOLET " tOpen)
	)
	(DrawSkillebryter tOpen 0)
	(DrawSkillebryter tOpen (- 6.0))
	(DrawSkillebryter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-SKILLEBRYTER-TREPOLET-" tClosed)
		description	(strcat "KL SKILLEBRYTER, TREPOLET " tClosed)
	)
	(DrawSkillebryter tClosed 0)
	(DrawSkillebryter tClosed (- 6.0))
	(DrawSkillebryter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
	
	
	
(defun NOBN-HIGH-VOLTAGE-LOAD-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-" tOpen)
		description	(strcat "KL LASTSKILLEBRYTER, " tOpen)
	)
	(DrawLastSkilleBryter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-" tClosed)
		description	(strcat "KL LASTSKILLEBRYTER, " tClosed)
	)
	(DrawLastSkilleBryter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-TOPOLET-" tOpen)
		description	(strcat "KL LASTSKILLEBRYTER, TOPOLET " tOpen)
	)
	(DrawLastSkilleBryter tOpen 0)
	(DrawLastSkilleBryter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-TOPOLET-" tClosed)
		description	(strcat "KL LASTSKILLEBRYTER, TOPOLET " tClosed)
	)
	(DrawLastSkilleBryter tClosed 0)
	(DrawLastSkilleBryter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-TREPOLET-" tOpen)
		description	(strcat "KL LASTSKILLEBRYTER, TREPOLET " tOpen)
	)
	(DrawLastSkilleBryter tOpen 0)
	(DrawLastSkilleBryter tOpen (- 6.0))
	(DrawLastSkilleBryter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat"NO-BN-2D-JBTEH_BRY-LASTSKILLEBRYTER-TREPOLET-" tClosed)
		description	(strcat "KL LASTSKILLEBRYTER, TREPOLET " tClosed)
	)
	(DrawLastSkilleBryter tClosed 0)
	(DrawLastSkilleBryter tClosed (- 6.0))
	(DrawLastSkilleBryter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-HIGH-VOLTAGE-CIRCUIT-INTERRUPTER ( / blockName description )
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-" tOpen)
		description	(strcat "KL EFFEKTBRYTER, " tOpen)
	)
	(DrawEffektBryter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-" tClosed)
		description	(strcat "KL EFFEKTBRYTER, " tClosed)
	)
	(DrawEffektBryter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-TOPOLET-" tOpen)
		description	(strcat "KL EFFEKTBRYTER, TOPOLET " tOpen)
	)
	(DrawEffektBryter tOpen 0)
	(DrawEffektBryter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-TOPOLET-" tClosed)
		description	(strcat "KL EFFEKTBRYTER, TOPOLET " tClosed)
	)
	(DrawEffektBryter tClosed 0)
	(DrawEffektBryter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-TREPOLET-" tOpen)
		description	(strcat "KL EFFEKTBRYTER, TREPOLET " tOpen)
	)
	(DrawEffektBryter tOpen 0)
	(DrawEffektBryter tOpen (- 6.0))
	(DrawEffektBryter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	(strcat"NO-BN-2D-JBTEH_BRY-EFFEKTBRYTER-TREPOLET-" tClosed)
		description	(strcat "KL EFFEKTBRYTER, TREPOLET " tClosed)
	)
	(DrawEffektBryter tClosed 0)
	(DrawEffektBryter tClosed (- 6.0))
	(DrawEffektBryter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-HIGH-VOLTAGE-EARTHING-INTERRUPTER ( / blockName description )
	;--------- manuelle brytere ---------
	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-" tOpen)
		description	(strcat "KL JORDINGSBRYTER, " tOpen)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tOpen 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-" tClosed)
		description	(strcat "KL JORDINGSBRYTER, " tClosed)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tClosed 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-TOPOLET-" tOpen)
		description	(strcat "KL JORDINGSBRYTER, TOPOLET " tOpen)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tOpen 0)
	(DrawSkillebryter tOpen (- 6.0))
	(DrawTwoPoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-TOPOLET-" tClosed)
		description	(strcat "KL JORDINGSBRYTER, TOPOLET " tClosed)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tClosed 0)
	(DrawSkillebryter tClosed (- 6.0))
	(DrawTwoPoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-TREPOLET-" tOpen)
		description	(strcat "KL JORDINGSBRYTER, TREPOLET " tOpen)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tOpen 0)
	(DrawSkillebryter tOpen (- 6.0))
	(DrawSkillebryter tOpen (- 12.0))
	(DrawThreePoleCrossLine 3.0 1.6629)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_BRY-JORDINGSBRYTER-TREPOLET-" tClosed)
		description	(strcat "KL JORDINGSBRYTER, TREPOLET " tClosed)
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter tClosed 0)
	(DrawSkillebryter tClosed (- 6.0))
	(DrawSkillebryter tClosed (- 12.0))
	(DrawThreePoleCrossLine 3.0 0)
	(AddDescriptionBelowOrigin description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawSkillebryter ( variation y / len len2 len3 )
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



(defun DrawLastSkilleBryter ( variation y / r len len2 len3 )
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


(defun DrawEffektBryter ( variation y / r len len2 len3 )
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


(defun DrawSkinneJord ( offset / len1 len2 r x y )
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