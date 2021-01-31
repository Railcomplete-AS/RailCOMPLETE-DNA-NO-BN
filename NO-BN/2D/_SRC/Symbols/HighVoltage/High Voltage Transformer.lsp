;=========================================================================================================================
;
; High Voltage Transformer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; High-voltage transfromer

(defun C:HIGH-VOLTAGE-TRANSFORMER ( / )

	;2D layer/symbol: JBTEH_Komponenter / EH_Sugetrafo
	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(SUGETRANSFORMATOR 4 1.25)

	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(SUGETRANSFORMATOR-I-KIOSK 4 1.25)

	(AUTOTRANSFORMATOR 3 1.25)
)



(defun SUGETRANSFORMATOR ( len r / blockName )
	(setq
		blockName	"NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO")
	)
	(drawCoil len r)
	(command 
		"._ROTATE" "_ALL" "" "0,0" 90
		"._MOVE" "_ALL" "" "0,0" (list (* 4 r) 0)
	)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SUGETRANSFORMATOR-I-KIOSK ( len r / blockName )
	(setq
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO-I-KIOSK"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO I KIOSK")
	)
	(drawCoil len r)
	(command 
		"._ROTATE" "_ALL" "" "0,0" 90
		"._MOVE" "_ALL" "" "0,0" (list (* 4 r) 0)
	)
	(command
		"._RECTANGLE" 
			(list (* (- 5) r) (- r))
			(list (* (+ 5) r) (+ len r r))
	)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun AUTOTRANSFORMATOR ( len r / blockName )
	(setq 
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-AUTOTRAFO"
		description	(strcat "KL TRANSFORMATOR, AUTOTRAFO")
	)
	(drawCoil len r)
	(command 
		"._MOVE" "_ALL" "" (list len 0) "0,0" 
		"._LINE" (list 0 0) (list (+ len r) 0) "" 
		"._LINE" (list 0 (* 4 r)) (list (+ r len) (* 4 r)) ""
	)
	(setLayer layer_AutoTransformerTerminals)
	(addText "NL" (list (- (+ len r r)) (* 8 r)) 1.8 0 "iso" "_MC")
	(addText "PL" (list (- (+ len r r)) 0) 1.8 0 "iso" "_MC")
	(setLayer layer_Zero)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; draw...X...() functions
;==========================
(defun drawCoil ( len r / )
	(command
		"._PLINE"
			(list 0 0)
			(list len 0)
			"_ARC" "_RADIUS" r
			(list len (* 2 r)) 
			"_RADIUS" (- r)
			(list len (* 4 r))
			"_RADIUS" (- r)
			(list len (* 6 r))
			"_RADIUS" (- r)
			(list len (* 8 r))
			"_LINE"
			(list 0 (* 8 r))
			""
	)
)
