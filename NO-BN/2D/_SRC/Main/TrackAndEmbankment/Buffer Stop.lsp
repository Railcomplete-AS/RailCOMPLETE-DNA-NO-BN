;=========================================================================================================================
;
; Buffer Stop.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Buffer stop



(defun BUFFER-STOPS ( / )
	(cond 
		((= _ADM_ _XXGL_)
		) 
		((= _ADM_ _NOBN_)
			(BUFFER-STOP "F") 	; fixed
			(BUFFER-STOP "S")	; sliding
			(BUFFER-STOP "H")	; hydraulic
		) 
		((= _ADM_ _FRSR_)
			(BUFFER-STOP "F") 	; fixe
			(BUFFER-STOP "S")	; glissant
			(BUFFER-STOP "H")	; hydraulique
		)
		((= _ADM_ _DEDB_)
		 	(BUFFER-STOP "F")	; fest
			(BUFFER-STOP "B")	; brems
			(BUFFER-STOP "A")	; abklappbar 
		) 		
		((= _ADM_ _JPTX_)
			(BUFFER-STOP "F") 	; fixed
			(BUFFER-STOP "S")	; sliding
			(BUFFER-STOP "H")	; hydraulic
		) 
	)
)



(defun BUFFER-STOP ( variation / blockName description x y )

	(cond 
		((= _ADM_ _XXGL_) (setq vv (cond ((= variation "F") "FIXED")	((= variation "S") "SLIDING")		((= variation "H") "HYDRAULIC"		))))
		((= _ADM_ _NOBN_) (setq vv (cond ((= variation "F") "FAST")		((= variation "S") "GLIDBAR")		((= variation "H") "HYDRAULISK"		))))
		((= _ADM_ _FRSR_) (setq vv (cond ((= variation "F") "FIXE")		((= variation "S") "GLISSANT")		((= variation "H") "HYDRAULIQUE"	))))
		((= _ADM_ _DEDB_) (setq vv (cond ((= variation "F") "FEST")		((= variation "B") "BREMSEND")		((= variation "A") "ABKLAPPBAR"		))))
		((= _ADM_ _JPTX_) (setq vv (cond ((= variation "F") "FIXED")	((= variation "S") "SLIDING")		((= variation "H") "HYDRAULIC"		))))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _TRK_ "BST-" "BUFFER-STOP-"	vv	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _TRK_ "SST-" "SPORSTOPPER-"	vv	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _TRK_ "HEU-" "HEURTOIR-"		vv	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _TRK_ "PRB-" "PRELLBOCK-"		vv	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _TRK_ "BST-" "BUFFER-STOP-"	vv	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "BUFFER STOP, "	vv	)))
		((= _ADM_ _NOBN_) (setq description (strcat "SPORSTOPPER, "	vv	)))
		((= _ADM_ _FRSR_) (setq description (strcat "HEURTOIR, "	vv	)))
		((= _ADM_ _DEDB_) (setq description (strcat "PRELLBOCK, "	vv	)))
		((= _ADM_ _JPTX_) (setq description (strcat "BUFFER STOP, "	vv	)))
	)
	(cond 
		((= _ADM_ _XXGL_)
		) 
		((= _ADM_ _NOBN_)
			;
			; +-----+
			; |     | ; Fixed     : no hatch
			; |     | ; Sliding   : medium hatch
			; |     | ; Hydraulic : solid hatch
			; +--.--+
			;
			(setq	
				x 8.5
				y 10.0
			)
			(DrawBox layDef_Zero x y _noWipeout_)
			; (No hatch if "F")
			(if (= variation "S") ; sliding
				(DrawHatch _sparseHatch_)
			)
			(if (= variation "H")
				(DrawHatch _mediumHatch_) ; hydraulic
			)
			(MoveUp (HalfOf y))
			(AddDescriptionBelowOrigin description 1.0)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		) 
		((= _ADM_ _FRSR_)
			; TODO: Replace Lisp code here with real FRSR stuff:
			;
			; +-----+
			; |     | ; Fixed     : no hatch
			; |     | ; Sliding   : sparse hatch
			; |     | ; Hydraulic : medium hatch
			; +--.--+
			;
			(setq	
				x 8.5
				y 10.0
			)
			(DrawBox layDef_Zero x y _noWipeout_)
			; (No hatch if "F")
			(if (= variation "S") ; sliding
				(DrawHatch _sparseHatch_)
			)
			(if (= variation "H")
				(DrawHatch _mediumHatch_) ; hydraulic
			)
			(MoveUp (HalfOf y))
			(AddDescriptionBelowOrigin description 1.0)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		)
		((= _ADM_ _DEDB_)
			; TODO: Check dimensions with DWG examples from DB
			(cond
				((= variation "F")
					; Festprellblock
					;
					; 3----.----4		^  UP direction
					; |         |		| 
					; 1         2		|	
					;
					(setq	
						p1 (list -1.250 -1.000)
						p2 (list  1.250 -1.000)
						p3 (list -1.250  0.000)
						p4 (list  1.250  0.000)
					)
							
					(DrawLine layDef_Zero p1 p3)
					(DrawLine layDef_Zero p3 p4)
					(DrawLine layDef_Zero p2 p4)
					
					(AddDescriptionBelowOrigin description 0)
					(CreateSchematicBlockFromCurrentGraphics blockName)
					(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
				)
			
				((= variation "B")
					; Bremsprellbock
					;
					; 3         4		^	UP direction
					; |         |		|
					; 1----.----2		|
					;
					(setq	
						p1 (list -1.250 0.000)
						p2 (list  1.250 0.000)
						p3 (list -1.250 1.000)
						p4 (list  1.250 1.000)
					)
		
					(DrawLine layDef_Zero p1 p2)
					(DrawLine layDef_Zero p1 p3)
					(DrawLine layDef_Zero p2 p4)
			
					(AddDescriptionBelowOrigin description 0)
					(CreateSchematicBlockFromCurrentGraphics blockName)
					(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
				)
				((= variation "A")
					; Abklappbarer Prellbock
					;
					;	 ___7___ 		
					;   /       \ 		
					;   |       | 		
					; 3-4-------5-6		^	UP direction
					; |           |		|
					; 1     .     2		|
					;
					(setq	
						p1 (list -1.250 0.000)
						p2 (list  1.250 0.000)
						p3 (list -1.250 1.000)
						p4 (list -0.887 1.000)
						p5 (list  0.887 1.000)
						p6 (list  1.250 1.000)
						p7 (list  0.000 1.887)
					)
		
					(DrawLine layDef_Zero p1 p3)
					(DrawLine layDef_Zero p3 p6)
					(DrawLine layDef_Zero p2 p6)
					(DrawArc layDef_Zero p4 p7 p5)

					(AddDescriptionBelowOrigin description 0)
					(CreateSchematicBlockFromCurrentGraphics blockName)
					(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
				)
			)
		)
		((= _ADM_ _JPTX_)
			; TODO: Replace Lisp code here with real JPTX stuff:
			;
			; +-----+
			; |     | ; Fixed     : no hatch
			; |     | ; Sliding   : sparse hatch
			; |     | ; Hydraulic : medium hatch
			; +--.--+
			;
			(setq	
				x 8.5
				y 10.0
			)
			(DrawBox layDef_Zero x y _noWipeout_)
			; (No hatch if "F")
			(if (= variation "S") ; sliding
				(DrawHatch _sparseHatch_)
			)
			(if (= variation "H")
				(DrawHatch _mediumHatch_) ; hydraulic
			)
			(MoveUp (HalfOf y))
			(AddDescriptionBelowOrigin description 1.0)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		)
	)
)
