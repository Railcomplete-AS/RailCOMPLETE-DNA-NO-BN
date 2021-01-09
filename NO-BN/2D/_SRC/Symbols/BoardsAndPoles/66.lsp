;=========================================================================================================================
;
; 66.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; End of train route

; For debugging:
; (66A)
; (The board's real name is '66', but '66' is not a valid LISP identifier)

(defun 66A ( /	blockName description x y posCircle radius1 radius2 description )
	; An 'S' shape
	;
	; TL-------TR
	; |   ___   |  
	; |  /   \  |
	; | (       | 
	; |  \___   |
	; |      \  |
	; |       ) |
	; |  \___/  |
	; |         |
	; BL---.---BR 
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-66-TOGVEI-SLUTT"
		description "SKILT SIGNAL 66 TOGVEI SLUTT"
		x 3.0
		y 4.5
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "S")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
