;=========================================================================================================================
;
; Virtual signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
		description "FIKTIVT SLUTTPUNKT FOR TOGVEI OG SKIFTEVEI"
		x 3.0		; Schematic symbol (simple angle bracket)
		y 3.0
		ang 130		; Annotative symbol ('fat' arrow - better visibility):
		len1 3.0
		width (/ 0.416 2.0)
	)
	; Schematic symbol
	(command _POLYLINE_ (list (- x) y) _origo_ (list (- x) (- y)) _openPolyline_) ; the '>' angle bracket
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command ; 'Fat' angle bracket
		_POLYLINE_
			_origo_ 
			(strcat (rtos len1) "<" (rtos ang))
			(strcat "@" (rtos width) "<" (rtos (+ ang _angle90_)))
			(strcat "-" (rtos (/ width (sin (D->R ang)))) "," "0")
			_ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
	)
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)



(defun VIRTUAL-INTERMEDIATE-SIGNAL ( / blockName s gs gs2 )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-VIAPUNKT"
		description "VIAPUNKT FOR TOGVEI OG SKIFTEVEI UTPEKING"
		s	3.0			; Schematic symbol (simple triangle)
		gs	1.5			; Annotative symbol ('fat' triangle - better visibility):
		gs2	(/ gs 2)
	)
	
	; Schematic symbol
	(command ; simple triangle
		"._POLYGON" 3 _origo_ "_INSCRIBED" s
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_
	)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(command ; 'fat' triangle
		"._POLYGON" 3 _origo_ "_INSCRIBED" gs
		"._POLYGON" 3 _origo_ "_INSCRIBED" (- gs gs2)
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list (- gs) 0)
	)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)
