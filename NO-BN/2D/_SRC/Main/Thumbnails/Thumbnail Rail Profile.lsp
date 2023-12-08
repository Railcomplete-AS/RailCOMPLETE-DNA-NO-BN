;=========================================================================================================================
;
; Thumbnail Rail Profile.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for sweeped rail profile alignment selection (for 3D generation)

(defun THUMBNAIL-RAIL-PROFILE ( / )
	(THUMBNAIL-RAIL-PROFILE)
)



(defun THUMBNAIL-RAIL-PROFILE ( / blockName ) 
	; Rail profile. 2D cross-section of a rail.
	(setq blockName (strcat _RC_ thumbnailInfix "-SKINNEPROFIL"		))
	(command
		_POLYLINE_ 
			_origin_ ; Top-of-rail
			(list 0.60  0.00)
			(list 1.10 -0.10)
			(list 1.30 -0.25)
			(list 1.45 -0.50)
			(list 1.50 -1.50)
			(list 0.75 -1.75)
			(list 0.60 -1.85)
			(list 0.45 -2.20)
			(list 0.35 -2.80)
			(list 0.35 -4.75)
			(list 0.40 -5.30)
			(list 0.60 -5.85)
			(list 2.00 -6.35)
			(list 2.85 -6.40)
			(list 3.00 -6.50)
			(list 3.00 -6.80)
			(list 2.95 -6.85)
			(list 0.00 -6.85)
			_ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 3.5)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
