;=========================================================================================================================
;
; ANYADM Manhole.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Manhole

(defun ANYADM-MANHOLE ( / )
	; Manhole radius, manholeLength and manholeDepth are given in mm, the others in meters.

	(cond
		((= _ADM_ _XXGL_)
		)
		((= _ADM_ _NOBN_)
			; Diameter, cover diameter, cover offset X and Y
			(ANYADM-MANHOLE-CIRCULAR "1400" 0.700 0.126 0.137) ; Top left placement of cover
			;(MANHOLE-CIRCULAR "1400" 0.700 0.000 0.186)  ; Middle top placement of cover

			; Length (mm along track), depth [mm] (across track), cover diameter [m]  (or zero), cover length [m] (or don't care) 
			; cover depth [m] (or don't care), cover offset X and Y [m]
			(ANYADM-MANHOLE-RECTANGULAR "1600"  "900" 0 1.32 0.68  0.000 0.450) ; radius=0 ==> rectangular cover size (1.32 0.68) at (0 .450)
	
			; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) 
			; cover depth [m] (or don't care), cover offset X and Y [m]
			(ANYADM-MANHOLE-RECTANGULAR "1400" "1400" 0.660 0 0 -0.225 0.925)
	
			; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) 
			; cover depth [m] (or don't care), cover offset X and Y [m]	
			(ANYADM-MANHOLE-RECTANGULAR "2300" "2300" 0.660 0 0 -0.625 1.775)
		)
		((= _ADM_ _FRSR_)
			(ANYADM-MANHOLE-CIRCULAR "1400" 0.700 0.126 0.137)
			(ANYADM-MANHOLE-RECTANGULAR "1600"  "900" 0 1.32 0.68  0.000 0.450)
			(ANYADM-MANHOLE-RECTANGULAR "1400" "1400" 0.660 0 0 -0.225 0.925)
			(ANYADM-MANHOLE-RECTANGULAR "2300" "2300" 0.660 0 0 -0.625 1.775)
		)
		((= _ADM_ _DEDB_)
		)
	)
)


	
(defun ANYADM-MANHOLE-CIRCULAR ( manholeDiameter coverDiameter coverOffsetX coverOffsetY / blockName description mr cr p1 )
	; Manholeradius is given in mm, the others in meters.
	;   _____
	;  /  (1)\
	; |       |
 	; |   .   |
	; |       |
	;  \_____/
	;   
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SUB_ "MHO-" "MANHOLE-CIRCULAR" 				)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SUB_ "KF" _uOSLASH_ "-" "TREKKEKUM-RUND"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SUB_ "RGD-" "REGARD-CIRCULAIRE"				)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SUB_ "KKA-" "RUNDSCHACHT"					)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "MANHOLE, CIRCULAR"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "TREKKEKUM, RUND"			)))
		((= _ADM_ _FRSR_) (setq description (strcat "REGARD, CIRCULAIRE"		)))
		((= _ADM_ _DEDB_) (setq description (strcat "RUNDSCHACHT"				)))
	)
	(setq				   
		blockName (strcat blockName "-" manholeDiameter)
		description (strcat description " " _uOSLASH_ manholeDiameter)
		mr	(/ (atof manholeDiameter) 2000.0)
		cr	(HalfOf coverDiameter)	
		p1 	(list coverOffsetX coverOffsetY)
	)
	(DrawCircleAtPos layDef_Zero _origin_ mr _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 cr _noWipeout_)
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description mr)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ANYADM-MANHOLE-RECTANGULAR ( manholeLength manholeDepth coverDiameter coverLength coverDepth coverOffsetX coverOffsetY / blockName description x y cr p1 )
	; ManholeLength and manholeDepth are given in mm, the others in meters.
	; Interpreted as inside measures (open to debate...)
	;
	;  +---------+
	;  |         |
	;  |         |
 	;  | +----+  |
	;  | | p1 |  |
	;  | +----+  |
	;  |         |
	;  +----.----+
	;
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SUB_ "MHO-" "MANHOLE-RECTANGULAR"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SUB_ "KF" _uOSLASH_ "-" "TREKKEKUM"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SUB_ "RGD-" "REGARD-RECTANGULAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SUB_ "KKA-" "VIERECKIGES SCHACHT"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "MANHOLE, RECTANGULAR, "		)))
		((= _ADM_ _NOBN_) (setq description (strcat "TREKKEKUM, FIRKANTET, "		)))
		((= _ADM_ _FRSR_) (setq description (strcat "REGARD, RECTANGULAIRE, "		)))
		((= _ADM_ _DEDB_) (setq description (strcat "SCHACHT, VIERECKIGES, "		)))
	)NO-BN-2D-JBTKU_KFØ-TREKKEKUM-L1600-D900-{{SymbolMode}}
	(setq				   
		blockName (strcat blockName "-L" manholeLength "-D" manholeDepth)
		description (strcat description "L=" manholeLength ", D=" manholeDepth)
		x	(/ (atof manholeLength) 1000.0)
		y	(/ (atof manholeDepth) 1000.0)
		cr	(HalfOf coverDiameter)
		p1	(list coverOffsetX coverOffsetY)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(MoveUp (HalfOf y))
	(if (= coverDiameter 0)
		; Rectangular cover:
		(DrawBoxAtPos layDef_Zero p1 coverLength coverDepth _noWipeout_)
	;else
		;Circular cover:
		(DrawCircleAtPos layDef_Zero p1 cr _noWipeout_)
    )
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
