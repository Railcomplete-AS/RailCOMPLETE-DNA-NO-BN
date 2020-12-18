;=========================================================================================================================
;
; Virtual signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Fictitous signal marker (start / end of train route, via point)

(defun C:VIRTUAL-SIGNAL ()
	(VIRTUAL-SIGNAL)
	(VIRTUAL-INTERMEDIATE-SIGNAL)
)



(defun VIRTUAL-SIGNAL ( / blockName x y ang len1 width )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-FIKTIVT-PUNKT"
		x 3.0		; Schematic symbol (simple angle bracket)
		y 3.0
		ang 130		; Geo symbols ('fat' arrow - better visibility):
		len1 3.0
		width (/ 0.416 2.0)
	)
	; Schematic symbol
	(command "._PLINE" (list (- x) y) "0,0" (list (- x) (- y)) _open_) ; the '>' angle bracket
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(command ; 'Fat' angle bracket
		"._PLINE"
			"0,0" 
			(strcat (rtos len1) "<" (rtos ang))
			(strcat "@" (rtos width) "<" (rtos (+ ang 90)))
			(strcat "-" (rtos (/ width (sin (D->R ang)))) "," "0")
			""
		"._MIRROR" "_LAST" "" "0,0" "1,0" "_NO" 
	)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 1.0 blockName)
)



(defun VIRTUAL-INTERMEDIATE-SIGNAL ( / blockName s gs gs2 )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-VIAPUNKT"
		s	3.0			; Schematic symbol (simple triangle)
		gs	1.5			; Geo symbols ('fat' triangle - better visibility):
		gs2	(/ gs 2)
	)
	
	; Schematic symbol
	(command ; simple triangle
		"._POLYGON" 3 "0,0" "_INSCRIBED" s
		"._ROTATE" "_ALL" "" "0,0" "-90"
	)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(command ; 'fat' triangle
		"._POLYGON" 3 (list 0 0) "_INSCRIBED" gs
		"._POLYGON" 3 (list 0 0) "_INSCRIBED" (- gs gs2)
		"._ROTATE" "_ALL" "" (list 0 0) (- 90)
		"._MOVE" "_ALL" "" (list 0 0) (list (- gs) 0)
	)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 1.0 blockName)
)
