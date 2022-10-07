;=========================================================================================================================
;
; Connector Alignment Crossing.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

(defun CONNECTOR-CROSSINGS ( / )
	
	(SetCadSystemDefaults)  
	(CONNECTOR-CROSSING)
)



(defun CONNECTOR-CROSSING ( / blockName description r )
	;
	; A cross (8r wide, 4r high) with four "cusps" 
	;
	;        U
	;        |
	;  )-----.-----(
	;        |
	;        A    
	;
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _TRK_ "CRO-" "CONNECTION-CROSSING"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _TRK_ "SPV-" "FORBINDELSE-SPORKRYSS"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _TRK_ "TOB-" "CONNEXION-TRAVERSEE-OBLIQUE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _TRK_ "KRZ-" "VERBINDUNG-KREUZUNG"			)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _TRK_ "CRO-" "CONNECTION-CROSSING"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "CONNECTOR, CROSSING"						)))
		((= _ADM_ _NOBN_) (setq description (strcat "FORBINDELSE, SPORKRYSS"					)))
		((= _ADM_ _FRSR_) (setq description (strcat "CONNEXION, TRAVERSEE OBLIQUE"				))) ; Trouble again with accents...
		((= _ADM_ _DEDB_) (setq description (strcat "VERBINDUNG, KREUZUNG"						)))
		((= _ADM_ _JPTX_) (setq description (strcat "CONNECTOR, CROSSING"						)))
	)
	(setq
		r 0.5 ; r of 180 deg half-circle arc, to be shown at the ends of a long horizontal and short vertical line meeting at the crossing
	)
	(SetLayer layDef_Zero)
	(command
		_LINE_ (list (* -4 r) 0) (list (* 4 r) 0) _ENTER_ ; long horizontal line
		_LINE_ (list 0 (* -2 r)) (list 0 (* 2 r)) _ENTER_ ; short vertical line
		_ARC_ _setArcCenter_ (list (* -5 r) 0) (list (* -5 r) (* -1 r)) (list (* r -5) (* 1 r)) ; Left arc
		_ARC_ _setArcCenter_ (list (* 5 r) 0) (list (* 5 r) (* 1 r)) (list (* r 5) (* -1 r)) ; Right arc
		_ARC_ _setArcCenter_ (list 0 (* -3 r)) (list (* 1 r) (* -3 r)) (list (* -1 r) (* -3 r)) ; Bottom arc
		_ARC_ _setArcCenter_ (list 0 (* 3 r)) (list (* -1 r) (* 3 r)) (list (* 1 r) (* 3 r)) ; Top arc
	)
	(AddDescriptionBelowOrigin description (* 4 r))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
