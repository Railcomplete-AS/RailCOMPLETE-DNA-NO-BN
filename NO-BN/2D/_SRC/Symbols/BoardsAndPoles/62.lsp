;=========================================================================================================================
;
; 62.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Possessed signal (not in operation)

; For debugging:
; (62A)

(defun 62A ( / blockName description angle x len offset xoff yoff pt0 pt1 pt2 pt3 pt4 pt5 pt6	pt7 pt8 pt9 )
	; St Andrew Cross with white and black fields
	;
	;  ^     ^
	; / \   / \
	; \\ \ / //
	;  \\ V //
	;  / /.\ \
	; / // \\ \
	; \//   \\/
	;  V     V
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-62-UGYLDIGHETSSKILT"
		description "SKILT SIGNAL 62 UGYLDIGHETSSKILT"
		x	9.0 	; Before rotation
		y	0.75	; Before rotation
		p1	(list (* -0.5 x) 0)
		p2	(list (*  0.5 x) 0)
		ang	 27
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawBoxAtPos layer_Zero x (halfOf y) (list 0 (halfOf (halfOf y))) _noWipeout_)
	(drawHatch _solidHatch_)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ angle)
	(moveUp (halfOf (/ y (DDcos ang))))
	(mirrorAboutYaxis _keepMirrorSource_)
	(addDescriptionBelowOrigo description 0.5)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
