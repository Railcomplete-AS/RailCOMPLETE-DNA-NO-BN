;=========================================================================================================================
;
; Deflection Bar.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Deflection bar (for installation in the track, to protect axle counter sensors from snow-clearing equipment in winter time)

(defun C:DEFLECTION-BAR ()
	(OPPKJOERSBJELKE)
)



(defun OPPKJOERSBJELKE ( / blockName fotX fotY bjelkeX bjelkeY )
	(setq
		blockName "NO-BN-2D-JBTOB-SPOROBJEKT-OPPKJOERSBJELKE"
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
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_RECTANGLE_ (list (- (+ distFot fotX)) bjelkeY) (list (- (- distFot fotX)) fotY)
		"._ARRAY" _lastSelection_ _ENTER_ _rectangularArray_ 2 2 (- (+ bjelkeY fotY)) (* 2 distFot)
	)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)