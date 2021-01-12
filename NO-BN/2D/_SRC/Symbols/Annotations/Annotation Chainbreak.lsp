;=========================================================================================================================
;
; Annotation Chainbreak.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Chain break symbol, scalable inside RailCOMPLETE (as you zoom in and out)

(defun C:ANNOTATION-CHAINBREAK ( / blockName description )
	(setq 
		blockName "NO-BN-2D-JBTFE-ANNOTATIONS-CHAINBREAK"
		description "Annotation Kjedebrudd"
	)
	(command "._PLINE" "-11,0" "9,0" "11,4" "-9,4" _closed_)
	(setLayer layer_BoardOrPole_Wipeout)
	(command "._WIPEOUT" "_POLYLINE" "_LAST" "" "_YES")
	(setLayer layer_Zero)

	(command "._LINE" "-10.876,0.248" "9.124,0.248" "")
	(command "._LINE" "-10.752,0.4961" "9.248,0.4961" "")
	(command "._LINE" "-9.248,3.5039" "10.752,3.5039" "")
	(command "._LINE" "-9.124,3.752" "10.876,3.752" "") 
	(addAtt "FRA_KM" "Fra km:" "15" "-1.0,-2.75" 1.8 0 "iso" "Right" _lockPosition_)
	(addAtt "FRA_M" "Fra meter:" "422" "-1.0,-5.50" 1.8 0 "iso" "Right" _lockPosition_)
	(addAtt "TIL_KM" "Til km:" "15" "1.0,-2.75" 1.8 0 "iso" "Left" _lockPosition_)
	(addAtt "TIL_M" "Til meter:" "450" "1.0,-5.50" 1.8 0 "iso" "Left" _lockPosition_)
	(addAtt "SPRANG" "Sprang:" "+28m" "0,5.5" 1.8 0 "iso" "_MC" _lockPosition_)
	(addText "KJEDEBRUDD" "0,2" 1.8 0 "iso" "_MC")
	(command "._ROTATE" "_ALL" "" "0,0" "-90")
	(command "._MOVE" "_ALL" "" "Displacement" "23,0")
	(command "._LINE" "0,0" "23,0" "")
	(command "._ROTATE" "_ALL" "" "0,0" "90")

	; DO NOT generate this one in multiple drawing scales, instead 
	; use RC-CopyAnnotationToDrawing command in RC:
	(createSchematicBlockFromCurrentGraphics blockName)
)