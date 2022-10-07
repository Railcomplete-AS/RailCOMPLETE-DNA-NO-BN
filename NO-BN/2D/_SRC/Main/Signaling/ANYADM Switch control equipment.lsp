;=========================================================================================================================
;
; ANYADM Switch control equipment.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Turnout, derailer and level crossing boom operation (point machine, derailer machine, boom machine)

(defun ANYADM-SWITCH-CONTROL-EQUIPMENT ( / )
	(TraceLevel3 "ANYADM-POINT-MACHINE")		(ANYADM-POINT-MACHINE)
	(TraceLevel3 "ANYADM-DERAILER-MACHINE")		(ANYADM-DERAILER-MACHINE)
	(TraceLevel3 "ANYADM-LOCAL-CONTROL-PANEL")	(ANYADM-LOCAL-CONTROL-PANEL)
)



(defun ANYADM-POINT-MACHINE ( / blockName description r1 r2 )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "PMA-" "POINT-MACHINE"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "DRV-" "SPORVEKSELDRIVMASKIN"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "MOA-" "MOTEUR-AIGUILLAGE"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "WAN-" "WEICHENANTRIEB"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "POINT MACHINE"						)))
		((= _ADM_ _NOBN_) (setq description (strcat "SPORVEKSELDRIVMASKIN"				)))
		((= _ADM_ _FRSR_) (setq description (strcat "MOTEUR POUR AIGUILLAGE"			)))
		((= _ADM_ _DEDB_) (setq description (strcat "WEICHENANTRIEB"				 	)))
	)
	(cond 
		((= _ADM_ _XXGL_)
			; TODO
		)
		((= _ADM_ _NOBN_)
			;    _____
			;   /  _  \
			;  | ( . ) |
			;   \_____/ 
			;       
			(setq 
				r1 (NOBN_GetLargeLanternRadius)
				r2 (HalfOf r1)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			; no hatch
			(AddDescriptionBelowOrigin description r1)
		)
		((= _ADM_ _FRSR_)
			; TODO
		)
		((= _ADM_ _DEDB_)
			; TODO - Check symbol appearance with DB (this one is our own fantasy...)
			;    _____
			;   /  _  \
			;  | ( . ) |
			;   \__|__/ 
			;      1		A line from origin to "down" direction
			(setq 
				r1	1.500
				r2	0.500
				p1	(list 0.000 -2.000)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			; no hatch
			(DrawLine layDef_Zero _origin_ p1)
			(AddDescriptionBelowOrigin description r1)
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ANYADM-DERAILER-MACHINE ( / blockName description r1 r2 )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "DMA-" "DERAILER-MACHINE"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "SPD-" "SPORSPERREDRIVMASKIN"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "MOD-" "MOTEUR-DERAILLEUR"			)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "GAN-" "GLEISSPERREANTRIEB"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "DERAILER MACHINE"						)))
		((= _ADM_ _NOBN_) (setq description (strcat "SPORSPERREDRIVMASKIN"					)))
		((= _ADM_ _FRSR_) (setq description (strcat "MOTEUR DERAILLEUR"						)))
		((= _ADM_ _DEDB_) (setq description (strcat "GLEISSPERREANTRIEB"					)))
	)
	(cond 
		((= _ADM_ _XXGL_)
			; TODO
		)
		((= _ADM_ _NOBN_)
			;    _____
			;   /  _  \
			;  | ( . ) |		; Same as switch point machine
			;   \_____/ 
			;       
			(setq 
				r1 (NOBN_GetLargeLanternRadius)
				r2 (* 0.5 r1)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			; no hatch
			(AddDescriptionBelowOrigin description r1)
		)
		((= _ADM_ _FRSR_)
			; TODO
		)
		((= _ADM_ _DEDB_)
			; TODO - Check symbol appearance with DB (this one is our own fantasy...)
			;    _____
			;   /  _  \
			;  | ((.)) |
			;   \__|__/ 
			;      1		A line from origin to "down" direction
			(setq 
				r1	1.500
				r2	0.500
				r3	0.700
				p1	(list 0.000 -2.000)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			(DrawCircle layDef_Zero r3 _noWipeout_)
			; no hatch
			(DrawLine layDef_Zero _origin_ p1)
			(AddDescriptionBelowOrigin description r1)
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ANYADM-LOCAL-CONTROL-PANEL ( / blockName description r1 r2 )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "LCP-" "LOCAL-CONTROL-PANEL"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "LOK-" "LOKALSTILLER"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "CMA-" "COMMUTATEUR-ADV"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "UMS-" "UMSTELLTASTE"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "LOCAL CONTROL PANEL FOR SWITCH AND DERAILER"	)))
		((= _ADM_ _NOBN_) (setq description (strcat "LOKALSTILLER FOR SPORVEKSEL OG SPORSPERRE"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "COMMUTATEUR POUR AIGUILLAGE ET DERAILLEUR"		)))
		((= _ADM_ _DEDB_) (setq description (strcat "UMSTELLTASTE FUER WEICHE UND GLEISSPERRE"		)))
	)
	(cond 
		((= _ADM_ _XXGL_)
			; TODO
		)
		((= _ADM_ _NOBN_)
			;    _____
			;   /  _  \
			;  | (*.*) |	Dense hatch in inner circle
			;   \_____/ 
			;       
			(setq 
				r1 (NOBN_GetLargeLanternRadius)
				r2 (* 0.5 r1)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			(DrawHatch _denseHatch_)
			(AddDescriptionBelowOrigin description r1)
		)
		((= _ADM_ _FRSR_)
			; TODO
		)
		((= _ADM_ _DEDB_)
			; TODO - Check that DB accepts the symbol
			;    _____
			;   /  _  \
			;  | (*.*) |	Dense hatch in inner circle
			;   \__|__/ 
			;      1		A line from origin to "down" direction
			(setq 
				r1	1.500
				r2	0.500
				p1	(list 0.000 -2.000)
			)
			(DrawCircle layDef_Zero r1 _noWipeout_)
			(DrawCircle layDef_Zero r2 _noWipeout_)
			(DrawHatch _denseHatch_)
			(DrawLine layDef_Zero _origin_ p1)
			(AddDescriptionBelowOrigin description r1)
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
