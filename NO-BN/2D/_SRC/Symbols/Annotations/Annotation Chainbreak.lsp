;=========================================================================================================================
;
; Annotation Chainbreak.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Chain break symbol, scalable inside RailCOMPLETE (as you zoom in and out)

(defun C:ANNOTATION-CHAINBREAK ( / blockName description )
	(setq 
		blockName "NO-BN-2D-JBTFE-ANNOTATIONS-CHAINBREAK"
		description "KJEDEBRUDD"
	)
	(command _POLYLINE_ "-11,0" "9,0" "11,4" "-9,4" _closedPolyline_)
	(addWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)
	(setLayer layDef_Zero)
	(command _LINE_ "-10.876,0.248" "9.124,0.248" _ENTER_)
	(command _LINE_ "-10.752,0.4961" "9.248,0.4961" _ENTER_)
	(command _LINE_ "-9.248,3.5039" "10.752,3.5039" _ENTER_)
	(command _LINE_ "-9.124,3.752" "10.876,3.752" _ENTER_) 
	(addAtt "FRA_KM" "Fra km:"    "15"   "-1.0,-2.75" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
	(addAtt "FRA_M"  "Fra meter:" "422"  "-1.0,-5.50" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
	(addAtt "TIL_KM" "Til km:"    "15"   "1.0,-2.75"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(addAtt "TIL_M"  "Til meter:" "450"  "1.0,-5.50"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(addAtt "SPRANG" "Sprang:"    "+28m" "0,5.5"      _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)

	(addTextAtPos layDef_Zero _th180_ "0,2" "KJEDEBRUDD")
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_)
	(moveUp 23)
	(command _LINE_ _origo_ "23,0" _ENTER_)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)

	; DO NOT generate this one in multiple drawing scales,
	; instead, use RC-CopyAnnotationToDrawing command in RC - the <SymbolDefinition> DNA declaration 
	; may be found inside the StyleDefinitions XML file, annotating chainbreaks.
	(createSchematicBlockFromCurrentGraphics blockName)
)