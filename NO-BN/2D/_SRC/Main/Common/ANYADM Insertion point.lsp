;=========================================================================================================================
;
; ANYADM CAD-point.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Graphical symbol (circle with cross) to better see where the insertion point for a 2D symbol is. 

(defun CAD-POINTS ( / )
	(TraceLevel3 "INSERTION-POINT")	(INSERTION-POINT)
	(TraceLevel3 "AUXILIARY-POINT")	(AUXILIARY-POINT)
)



(defun INSERTION-POINT ( / radius blockName )
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ "INSERTION-POINT"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ "INNSETTINGSPUNKT"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ "POINT-INSERTION"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ "EINFUEGEPUNKT"		)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ "INSERTION-POINT"	)))
	)
	; Insertion point has no description - it is just an add-on to another symbol
	(setq 
		radius 0.05
	)
	(SetLayer layDef_InsertionPoint)
	(command
		_CIRCLE_ _origin_ radius
		_LINE_ (list (- radius) 0) (list radius 0) _ENTER_
		_LINE_ (list 0 (- radius)) (list 0 radius) _ENTER_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)



(defun AUXILIARY-POINT ( / radius blockName )
	; Auxiliary point, to be used as a point object that the user can snap other things to (a kind of watch object).
	; Intended for placement on the InsertionPoint layer, since these are generally not to be shown in print/PDF.
	;
	; NB: AutoLISP has no command-line version to se PTYP to "2" in absolute units (circle radius=1 / diameter=2),
	; this must be done by user or preferentiably by RC supervising the sysvar and showing offering a flashbox, when
	; appropriate, to let the user easily change PTYP appearance.
	;
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ "AUXILIARY-POINT"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ "HJELPEPUNKT"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ "POINT-AUXILIAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ "HILFSPUNKT"			)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ "AUXILIARY-POINT"	)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat _RC_ "AUXILIARY POINT"	)))
		((= _ADM_ _NOBN_) (setq description (strcat _RC_ "HJELPEPUNKT"		)))
		((= _ADM_ _FRSR_) (setq description (strcat _RC_ "POINT AUXILIAIRE"	)))
		((= _ADM_ _DEDB_) (setq description (strcat _RC_ "HILFSPUNKT"		)))
		((= _ADM_ _JPTX_) (setq description (strcat _RC_ "AUXILIARY POINT"	)))
	)
	(setq 
		radius 1.0
	)
	(SetLayer layDef_InsertionPoint)
	(command
		_POINT_ _origin_ _ENTER_
	)
	(AddDescriptionBelowOrigin description _zero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
