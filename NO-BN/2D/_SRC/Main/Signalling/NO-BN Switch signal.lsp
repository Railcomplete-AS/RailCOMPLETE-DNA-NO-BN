;=========================================================================================================================
;
; NO-BN Switch signal.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Ordinary switch / single or double slip switch position signal

(defun NOBN-SPORVEKSELSIGNAL ( / )
	(SPORVEKSELSIGNAL "ENKELVEKSEL")
	(SPORVEKSELSIGNAL "KRYSSVEKSEL")
)



(defun SPORVEKSELSIGNAL ( type / blockName description r p1 p2 )
	; Signal for point position, simple switch or single/double slip switch ('enkel / dobbel kryssveksel')
	;          ___       
	;         / 1*\      Ordinary switch signal
	;        (  .**)     Two semi-circles, hatched to the right
	;         \_2*/      
	;      
	;      ___     ___ 
	;     / 1*\   /*1 \  Single or double slip switch signal
	;    (  |**).(**|  ) 2 x Two semi-circles, hatched towards the center
	;     \_2*/   \*2_/  
	;      
	(setq 
		blockName	(strcat _SIG_ "SIG-" "SIGNAL-SPORVEKSELSIGNAL-" type)
		description (strcat "SPORVEKSELSIGNAL, " type)
		r (NOBN_GetLargeLanternRadius)
		p1 (list 0 (+ r)) 
		p2 (list 0 (- r))
	)
	(cond
		((= type "ENKELVEKSEL")
			(DrawCircle layDef_Zero r _noWipeout_)
			(DrawLine layDef_Zero p1 p2)
			(DrawHatchAtPoint _denseHatch_ _slightlyRight_ _angleZero_ _offsetZero_)
		)
		((= type "KRYSSVEKSEL")
			(DrawCircle layDef_Zero r _noWipeout_)
			(DrawLine layDef_Zero p1 p2)
			(DrawHatchAtPoint _denseHatch_ _slightlyRight_ _angleZero_ _offsetZero_)
			(MoveRight r)
			(MirrorAboutYaxis _keepMirrorSource_)
		)
		(T (alert (strcat "*** ERROR: SPORVEKSELSIGNAL() bad argument [" type "].")))
	)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
