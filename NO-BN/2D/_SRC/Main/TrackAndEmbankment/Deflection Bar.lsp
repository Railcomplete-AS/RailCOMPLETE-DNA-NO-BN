;=========================================================================================================================
;
; Deflection Bar.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Deflection bar (for installation in the track, to protect axle counter sensors from snow-clearing equipment in winter time)

(defun DEFLECTION-BAR ()
	(OPPKJOERSBJELKE)
)



(defun OPPKJOERSBJELKE ( / blockName description fotX fotY bjelkeX bjelkeY )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _TRK_ "DFL-" "DEFLECTION-BAR"						)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _TRK_ "MVS-" "OPPKJOERSBJELKE"					)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _TRK_ "BPR-" "BARRE-DE-DEFLECTION"				)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _TRK_ "SPS-" "SCHNEEPFLUGSCHUTZ"					)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "PROTECTION BAR FOR AXLE COUNTERS"				)))
		((= _ADM_ _NOBN_) (setq description (strcat "OPPKJ" _uOSLASH_ "RSBJELKE"					)))
		((= _ADM_ _FRSR_) (setq description (strcat "BARRE DE DEFLECTION POUR COMPTEURS D'ESSIEUX"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "SCHNEEPFLUGSCHUTZ"								)))
	)
	(setq
		fotX (/ 0.16 2)
		fotY (/ 0.8 2)
		bjelkeX (/ 1.0 2)
		bjelkeY (/ 0.1 2)
		bjelke2X (* (/ (* 2 bjelkeX) 100) 28)
        distFot (* (/ (* 2 bjelkeX) 10) 3)
	)
	(command
		_RECTANGLE_ (list (- bjelkeX) (- bjelkeY)) (list bjelkeX bjelkeY)
		_RECTANGLE_ (list (- (+ bjelkeX bjelke2X)) (- bjelkeY)) (list (- bjelkeX) bjelkeY)
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_RECTANGLE_ (list (- (+ distFot fotX)) bjelkeY) (list (- (- distFot fotX)) fotY)
		_ARRAY_ _lastSelection_ _ENTER_ _rectangularArray_ 2 2 (- (+ bjelkeY fotY)) (* 2 distFot)
	)
	(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_)
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
