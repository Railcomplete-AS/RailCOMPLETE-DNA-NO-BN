;=========================================================================================================================
;
; E37.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL New sign E37A. See also E37B.
; 2026-01-16 CLFEY Renamed E37A => E37. Moved E37B => E37-ZERO to here. File header name fixed (was incorrect "201.lsp").
;
;=========================================================================================================================

; Level transition

;=================================================================================================================================
; Utdrag fra Operativt Regelverk (ORV) pr 2026-01-16
; ---------------------------------------------------
; 8.77 Signal for systemovergang til og fra nivå 2
; 1) Signal E37 «Systemovergang» er satt opp ved sted for systemovergang til eller fra strekning med nivå 2. Når skiltet står
;    alene, markerer det overgang til nivå 2.
; 2) Ved systemovergang til nivå 0 er tallet «0» påført nederst i signal E37 «Systemovergang».
; 3) Systemovergang til nivå NTC og strekning med fjernstyring er i tillegg til signal E37 «Systemovergang» markert med signal
;    60F «FATC» eller signal 60G «DATC». Om signal 72A «Strekning med fjernstyring» ved systemovergang, se punkt 8.70.
; 4) Systemovergang til nivå NTC og grensestasjon er i tillegg til signal E37 «Systemovergang» markert med signal 60F «FATC»
;    eller signal 60G «DATC». Om signal 72B «Ikke fjernstyrt» ved systemovergang, se punkt 8.70.
;
; Signal							Signalnummer og signalnavn			Signalbetydning
; ------------------------------	------------------------------		------------------------------
; Hvit kvadratisk skive med sort	Signal E37							Markerer sted for teknisk systemovergang
; kant med sorte bokstaver			«Systemovergang»					tileller fra strekning med nivå 2.
;=================================================================================================================================
;
; Utdrag fra endringslogg tjn-2024-endringslogg-kap1-8-og-vedlegg1-2.pdf.
; -----------------------------------------------------------------------
; Signal E37B «Nivå 0» er fjernet. Signalet erstattes med mulighet til å 
; tilføye tallet «0» nederst i signal E37 «Systemovergang», jf. forslag til 
; ny europeisk standardisering, som vi antar er vedtatt innen 8.12.2024. 
; Signalnummer for signal E37A endres til E37.  
; Erstatter følgende tekst i nr. 2: 
; 2. Systemovergang til nivå 0 er i tillegg til signal E37A 
; «Systemovergang» markert med signal E37B «Nivå 0». 
; Signal E37B er fjernet i nr. 5. 
; Teknisk regelverk i Bane NOR må tilpasses.  
;=================================================================================================================================



(defun E37 ( /	blockName description x y )
	;
	; +---------+
	; |         |
	; |   LT    |
	; |  ETCS   |
	; |         |
	; +----.----+ 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-ERTMS-SYSTEMOVERGANG-NIVAA-TO"
		description "SKILT ERTMS SYSTEMOVERGANG TIL NIVÅ 2"
		x 4.0
		y 4.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th0736_ (Point21 y) "LT")
	(AddTextAtPoint layDef_Zero _th0736_ (Point22 y) "ETCS")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E37-ZERO ( /	blockName description x y )
	;
	; +-------+
	; |  LT   |
	; | ETCS  |
	; |   0   |
	; +---.---+ 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-ERTMS-SYSTEMOVERGANG-NIVAA-NULL"
		description "SKILT ERTMS SYSTEMOVERGANG TIL NIVÅ 0"
		x 4.0
		y 4.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th0736_ (Point31 y) "LT")
	(AddTextAtPoint layDef_Zero _th0736_ (Point32 y) "ETCS")
	(AddTextAtPoint layDef_Zero _th0736_ (Point33 y) "0")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
