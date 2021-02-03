;=========================================================================================================================
;
; Manhole.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Manhole

(defun C:MANHOLE ( / )

	; Diameter, cover diameter, cover offset X and Y
	(TREKKEKUM-RUND "1400" 0.700 0.126 0.137)
	
	; Length (mm along track), depth [mm] (across track), cover diameter [m]  (or zero), cover length [m] (or don't care) 
	; cover depth [m] (or don't care), cover offset X and Y [m]
	(TREKKEKUM-REKTANGULAER "1600"  "900" 0 1.32 0.68  0.000 0.450) 
	
	; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) 
	; cover depth [m] (or don't care), cover offset X and Y [m]
	(TREKKEKUM-REKTANGULAER "1400" "1400" 0.660 0 0 -0.225 0.925)
	
	; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) 
	; cover depth [m] (or don't care), cover offset X and Y [m]	
	(TREKKEKUM-REKTANGULAER "2300" "2300" 0.660 0 0 -0.625 1.775)
)


	
(defun TREKKEKUM-RUND ( manholeDiameter coverDiameter coverOffsetX coverOffsetY / blockName )
	(setq				   
		blockName (strcat "NO-BN-2D-JBTUB-TREKKEKUM-RUND-" manholeDiameter)
	)
	(command
		_CIRCLE_ _origo_ (/ (atof manholeDiameter) 2000.0)
		_CIRCLE_ (list coverOffsetX coverOffsetY) (/ coverDiameter 2)
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText (strcat "TREKKEKUM RUND \U+00D8" manholeDiameter) (list 0 (- (/ (atof manholeDiameter) -2000) 0.5)) _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_) ; \U+00D8 = Ø
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun TREKKEKUM-REKTANGULAER ( manholeLength manholeDepth coverDiameter coverLength coverDepth coverOffsetX coverOffsetY / blockName )
	; Sizes are given in mm.
	; Interpreted as inside measures (open to debate...)
	(setq				   
		blockName (strcat "NO-BN-2D-JBTUB-TREKKEKUM-L" manholeLength "-D" manholeDepth)
	)
	; Draw 'box':
	(command
		_RECTANGLE_ (list (/ (atof manholeLength) -2000.0) 0) (list (/ (atof manholeLength) 2000.0) (/ (atof manholeDepth) 1000.0))
	)
	(if (= coverDiameter 0)
		; Rectangular cover:
		(command
			_RECTANGLE_ 
				(list (+ coverOffsetX (/ coverLength -2)) (+ coverOffsetY (/ coverDepth -2)))
				(list (+ coverOffsetX (/ coverLength 2)) (+ coverOffsetY (/ coverDepth 2)))
		)
	;else
		;Circular cover:
		(command 
			_CIRCLE_ (list coverOffsetX coverOffsetY) (/ coverDiameter 2)
		)
    )
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText (strcat "TREKKEKUM L=" manholeLength ", D=" manholeDepth) (list 0 -0.5) _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
