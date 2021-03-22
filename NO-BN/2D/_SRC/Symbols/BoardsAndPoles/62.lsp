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

(defun 62A ( / blockName description x y p1 p2 )
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
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawBoxAtPos layDef_Zero (list 0 (HalfOf (HalfOf y))) x (HalfOf y) _noWipeout_)
	(DrawHatch _solidHatch_)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle27_)
	(MoveUp (HalfOf (/ y (DDcos _angle27_))))
	(MirrorAboutYaxis _keepMirrorSource_)
	(AddDescriptionBelowOrigo description 0.5)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
