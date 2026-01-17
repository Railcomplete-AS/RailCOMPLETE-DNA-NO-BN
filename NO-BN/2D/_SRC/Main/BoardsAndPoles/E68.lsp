;=========================================================================================================================
;
; E68.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
; 2024-04-16 SVNOE renamed
; 2026-01-16 CLFEY Fixed this file header (was E202.lsp). Removed Speed restriction 10 and 30. Added "End of restriction".
;                  This file replaces the previous files E200, E201, E202, E203.
;
;=========================================================================================================================

; E68A Speed restriction 20
; E68B End of speed restriction

;=================================================================================================================================
; Utdrag fra Operativt Regelverk (ORV) pr 2026-01-16
; ---------------------------------------------------
; 8.79 Hastighetssignaler på strekning med ERTMS
; På strekning med ERTMS er det satt opp hastighetssignaler for faste og midlertidige hastigheter under 40 km/t. Signalene gjelder 
; kun for kjøretøy som ikke har kjøretillatelse fra systemet, der tillatt hastighet ikke indikeres i førerpanelet, og der toget er 
; overvåket til 40 km/t.
; Signal E68A «Hastighetsrestriksjon» er satt opp der hastigheten gjelder fra, og gjelder fram til signal E68B «Hastighetsrestriksjon
; opphører» eller til nytt signal E68A «Hastighetsrestriksjon» med annen hastighet under 40 km/t. Signal E68A «Hastighetsrestriksjon»
; kan også settes opp for et helt område der nedsatt hastighet gjelder, og signal E68B «Hastighetsrestriksjon opphører» settes da 
; opp ved utkjøringen fra området.
; Signal E68B «Hastighetsrestriksjon opphører» er satt opp der hastighetsrestriksjonen opphører.
;
; Signal							Signalnummer og signalnavn			Signalbetydning
; ------------------------------	------------------------------		------------------------------
; Gult sirkelformet skilt med sort	Signal E68A 						Hastighetsrestriksjon lavere enn 40 km/t.
; kant og sorte tall.				«Hastighetsrestriksjon»				Signalet angir hastigheten i km/t.
;
; Gult sirkelformet skilt med sort	Signal E68B							Hastighetsrestriksjon lavere enn 40 km/t opphører.
; kant og sorte skråstreker.		«Hastighetsrestriksjon opphører»



(defun E68A ( /	blockName description r )
; ERTMS board - peed restriction 20
;
; Speed restriction for trains that are machine-supervised at 40 but the area requires 20 max speed.
; Note: ERTMS restricted speeds 10 km/h or 30 km/h are not in use in Norway.
	;      ______ 
	;     /      \    
	;    (   20   )    Circle with '20'
	;     \______/  
	;     
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E68A-HASTIGHETSRESRIKSJON"
		description "SKILT ERTMS HASTIGHETSRESTRIKSJON"
		r 2.0
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPoint layDef_Zero _th200_	 _origin_ "20") 
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E68B ( /	blockName description r )
; ERTMS board - End of speed restriction
	;      _____
	;     /  ///\    
	;    (  /./  )    Circle with 3 slashes
	;     \///__/  
	;     
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E68B-HASTIGHETSRESRIKSJON-OPPHOERER"
		description "SKILT ERTMS HASTIGHETSRESRIKSJON OPPHØRER"
		r 2.0
		d 0.4
	)
	(setq rVar (sqrt (- (* r r) (* d d))))
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawLine layDef_Zero (list 0.0 (- r)) (list 0.0 r))
 	(DrawLine layDef_Zero (list d (- rVar)) (list d rVar))
	(DrawLine layDef_Zero (list (- d) (- rVar)) (list (- d) rVar))
	(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus45_)
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
