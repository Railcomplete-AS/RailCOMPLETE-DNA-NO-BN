;=========================================================================================================================
;
; Switch signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Ordinary switch / single or double slip switch position signal

(defun C:SWITCH-SIGNAL ( / )
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
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-SPORVEKSELSIGNAL-" type)
		description (strcat "SPORVEKSELSIGNAL, " type)
		r (getLargeLanternRadius)
		p1 (list 0 (+ r)) 
		p2 (list 0 (- r))
	)
	(cond
		((= type "ENKELVEKSEL")
			(drawCircle layDef_Zero r _noWipeout_)
			(drawLine layDef_Zero p1 p2)
			(drawHatchFromPoint _denseHatch_ _slightlyRight_ _angleZero_ _offsetZero_)
		)
		((= type "KRYSSVEKSEL")
			(drawCircle layDef_Zero r _noWipeout_)
			(drawLine layDef_Zero p1 p2)
			(drawHatchFromPoint _denseHatch_ _slightlyRight_ _angleZero_ _offsetZero_)
			(moveRight r)
			(mirrorAboutYaxis _keepMirrorSource_)
		)
		(T (alert (strcat "*** ERROR: SPORVEKSELSIGNAL() bad argument [" type "].")))
	)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
