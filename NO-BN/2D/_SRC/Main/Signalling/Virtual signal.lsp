;=========================================================================================================================
;
; Virtual signal.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Fictitous signal marker (start / end of train route, via point)

(defun VIRTUAL-SIGNAL ( / )
	(VIRTUAL-SIGNAL-VIAPOINT)
	(VIRTUAL-SIGNAL-ENDPOINT)
)



(defun VIRTUAL-SIGNAL-VIAPOINT ( / blockName description s gs gs2 )
	;
	; |+			Single outline: Schematic symbol
	; ||\\			Double outline: Annotative symbol
	; ||  \\
	; ||   >>
	; ||  //
	; ||//
	; |+
	;
	(setq blockName (strcat _SIG_ "FIK-" "VIAPUNKT"		))
	(setq description (strcat "VIAPUNKT"				))
	
	(setq
		s	3.0			; Schematic symbol (simple triangle)
		gs	1.5			; Annotative symbol ('fat' triangle - better visibility):
		gs2	(/ gs 2)
	)
	
	; Schematic symbol - 'skinny' triangle
	(command ; simple triangle
		_POLYGON_ 3 _origin_ _inscribedPolygon_ s
		_ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_
	)
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol - 'fat' triangle
	(command
		_POLYGON_ 3 _origin_ _inscribedPolygon_ gs
		_POLYGON_ 3 _origin_ _inscribedPolygon_ (- gs gs2)
		_ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list (- gs) 0)
	)
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun VIRTUAL-SIGNAL-ENDPOINT ( / blockName description x y ang len1 width )
	;
	; \\			Single outline: Schematic symbol
	;   \\			Double outline: Annotative symbol
	;     \\
	;      >>
	;     //
	;   //
	; //
	;
	(setq blockName (strcat _SIG_ "FIK-" "FIKTIVT-SLUTTPUNKT"	))
	(setq description (strcat "FIKTIVT SLUTTPUNKT"				))
	
	(setq
		x 3.0		; Schematic symbol (simple angle bracket)
		y 3.0
		ang 130		; Annotative symbol ('fat' arrow - better visibility):
		len1 3.0
		width (/ 0.416 2.0)
	)
	; Schematic symbol
	(command _POLYLINE_ (list (- x) y) _origin_ (list (- x) (- y)) _openPolyline_) ; the '>' angle bracket
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command ; 'Fat' angle bracket
		_POLYLINE_
			_origin_ 
			(strcat (rtos len1) "<" (rtos ang))
			(strcat "@" (rtos width) "<" (rtos (+ ang _angle90_)))
			(strcat "-" (rtos (/ width (sin (D->R ang)))) "," "0")
			_ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
	)
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
