;=========================================================================================================================
;
; ANYADM Virtual signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Fictitous signal marker (start / end of train route, via point)

(defun ANYADM-VIRTUAL-SIGNAL ( / )
	(VIRTUAL-SIGNAL-VIAPOINT)
	(VIRTUAL-SIGNAL-ENDPOINT)
)



(defun VIRTUAL-SIGNAL-VIAPOINT ( / blockName description s gs gs2 )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "FIC-" "VIAPOINT"						)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "FIK-" "VIAPUNKT"						)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "PFJ-" "POINT-FICTIF-JALON-ITINERAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "FIK-" "VIAPUNKT"						)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "VIAPOINT"							)))
		((= _ADM_ _NOBN_) (setq description (strcat "VIAPUNKT"							)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT FICTIF JALON D'ITINERAIRE"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "VIAPUNKT"							)))
	)
	
	; TODO: Make individual symbols for each adm.
	
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
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "FIC-" "FICTITIOUS-END-OF-ROUTE"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "FIK-" "FIKTIVT-SLUTTPUNKT"				)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "PFF-" "POINT-FICTIF-FIN-ITINERAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "FIK-" "FIKTIVER-ENDPUNKT"				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "FICTITIOUS END OF ROUTE"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "FIKTIVT SLUTTPUNKT"				)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT FICTIF FIN D'ITINERAIRE"		)))
		((= _ADM_ _DEDB_) (setq description (strcat "FIKTIVER ENDPUNKT"				 	)))
	)
	
	; TODO: Make individual symbols for each adm.
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
