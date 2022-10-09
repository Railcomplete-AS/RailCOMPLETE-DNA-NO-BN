;=========================================================================================================================
;
; NO-BN Signalkombinasjoner.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-07-30 CLFEY Adjusted all mainPole sizes. Use floating point numbers to avoid possible later mistakes with integer division. Sorted and deleted duplicates.
; 2020-08-01 CLFEY New LISP code for ALL 'plain' symbols, allowing for use of nested loops and 'combinatorial computations' to ensure that all combinations
;                  are covered. Some might not be very meaningful, though.
; 2020-09-02 CLFEY Added NOBN_AddMb function / ERTMS markerboards
; ==========
;
;=========================================================================================================================

; Signal combinations - combinatorial algorithm 'with twists and tweaks' generating thousands of different signal symbols

; Inkl høyt skiftesignal og andre formsignaler som henger på hovedsignals mast.
;
; See Bane NOR TRV, download of signal symbol library DWG files.
;
; TODO list
; ==========
; TODO: 2020-07-31 CLFEY Remove NOBN_DrawTVs() since signal 66 is treated in other LISP code file - or upgrade LISP code and include it (=best)


(defun NOBN-KLASSE-B-LYSSIGNAL ( / hs fs tvs zs avs bs mks fks ls lhs ds yokeMounted )
;
; Bane NOR signal combos.
;
; Togsporsignal, sporsperresignal, sporvekselsignal, planovergangsignal og veisignaler modelleres annet sted.
; 
; Note: Even if e.g. an "A" signal is shown to the left of a mast, it might be built physically on the right side.
;
; Prinsipper: 
; - Mb kan ikke forekomme på samme mast som HS eller FS eller TVs
; - Hs kan ikke forekomme på samme mast som MB eller FS eller TVs
; - Ds kan erstattes av Zs på eldre stasjoner (og noen nye)
; - MKs benyttes på utkjørhovedsignaler, aldri i innkjørtogvei
; - LHs Lysende hastighetssignal kan benyttes både i innkjør- og i utkjørtogvei
; - FKs er lite benyttet i utkjørtogvei
; 
; Rs Rasvarslingssignal has no luminous effect, so just combine a Hs with the 'R' board, and suppress the board's tail in a schematic drawing.
; 
;                    +----------------------------- Mb ERTMS markerboard, signal E35 left/right/down
;                    |  +--------------------------- Hs Hovedsignal, signal 20/21/22
;                    |  |  +------------------------- Fs Forsignal, signal 23/24/25
;                    |  |  |  +----------------------- TVs Togvei slutt, signal 66 (i stedet for 20/21/22)
;                    |  |  |  |   +-------------------- Zs Høyt skiftesignal, signal 41/42
;                    |  |  |  |   |  +------------------ AVs Avgangsignal, lysende 'A'
;                    |  |  |  |   |  |   +--------------- BPs Bremseprøvesignal, lysende 'T' eller 'L'
;                    |  |  |  |   |  |   |   +------------ MKs MiddelKontroll signal, signal 4C
;                    |  |  |  |   |  |   |   |   +--------- FKs Forsiktig Kjøring, signal 32
;                    |  |  |  |   |  |   |   |   |   +------ Ls Linjesignal, signal 35B, angir linjen som togvei er stilt til
;                    |  |  |  |   |  |   |   |   |   |  +---- LHs Lysende Hastighetssignal, signal 68E (10-er hastighet lyser)
;                    |  |  |  |   |  |   |   |   |   |  |   +- Ds Dvergsignal, signal 43/44/45/46
;                    |  |  |  |   |  |   |   |   |   |  |   |
;-------------------MB HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 

	(SetCadSystemDefaults)
	
	(foreach yokeMounted '(nil "AAK")
		
		; The 'Unknown combination' signal symbol
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   0   0   0   0  0   0  yokeMounted)

		; 0-lys Frittstående Ds (Hs=Fs=0) Ds + MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   0   0   0   0  0   1  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   0   1   0   0  0   1  yokeMounted)

		; 0-lys Frittstående Fs / Fs+MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  1  0   0  0   0   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  1  0   0  0   0   1   0   0  0   0  yokeMounted)

		; 0-lys Togvei Slutt signal (Hs=Fs=0) TVs + MKs,Ds
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  1   0  0   0   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  1   0  0   0   1   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  1   0  0   0   0   0   0  0   1  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  1   0  0   0   1   0   0  0   1  yokeMounted)

		; 0-lys Avgangsignal AVs + Ds,MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  1   0   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  1   0   1   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  1   0   0   0   0  0   1  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  1   0   1   0   0  0   1  yokeMounted)
		
		; 0-lys Bremseprøvesignal BPs + Ds,MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   1   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   1   1   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   1   0   0   0  0   1  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   0  0   1   1   0   0  0   1  yokeMounted)

		; 0-lys Høyt skiftesignal Zs + MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   1  0   0   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 0  0  0   1  0   0   1   0   0  0   0  yokeMounted)

		; 1-lys RepHs / Stopplykt Hs1 + Ds,MKs
		;---------------MB-HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS 
		(NOBN-COMPLEX-SIGNAL nil 1  0  0   0  0   0   0   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 1  0  0   0  0   0   1   0   0  0   0  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 1  0  0   0  0   0   0   0   0  0   1  yokeMounted)
		(NOBN-COMPLEX-SIGNAL nil 1  0  0   0  0   0   1   0   0  0   1  yokeMounted)

		; Only needed if one or more of the "foreach" loops have been commented out, 
		; to avoid 'nil' values for commented-out arguments:
		(setq mb nil hs 0 fs 0 tvs 0 zs 0 avs 0 bps 0 mks 0 fks 0 ls 0 lhs 0 ds 0) 

		(foreach hs '(2 3)
			(foreach fs '(0 1)
				(foreach fks '(0 1)
					(foreach avs '(0 1)
						(foreach bps '(0 1)
							(foreach mks '(0 1)
								(foreach ls '(0 1)
									(foreach lhs '(0 1)
										(progn
											; Zs and Ds never occur on the same mast.
											(setq zs 0  ds 0 ) (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
											(setq zs 0  ds 1 ) (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
											(setq zs 1  ds 0 ) (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
										)
									);LHS
								);LS
							);MKS
						);BPS
					);AVS
				);FKS
			);FS
		);HS
	);yoke or mast
	
	; ERTMS combinations:
	(setq mb nil hs 0 fs 0 tvs 0 zs 0 avs 0 bps 0 mks 0 fks 0 ls 0 lhs 0 ds 0)
	(setq mb _left_  ds 0 mks 0 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 0 mks 1 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 1 mks 0 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 1 mks 1 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)

	(setq mb _right_ ds 0 mks 0 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 0 mks 1 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 1 mks 0 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 1 mks 1 yokeMounted nil)   (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)

	(setq mb _left_  ds 0 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 1 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 0 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _left_  ds 1 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)

	(setq mb _right_ ds 0 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 1 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 0 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _right_ ds 1 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)

	(setq mb _down_  ds 0 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _down_  ds 1 mks 0 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _down_  ds 0 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
	(setq mb _down_  ds 1 mks 1 yokeMounted "YOKE") (NOBN-COMPLEX-SIGNAL mb hs fs tvs zs avs bps mks fks ls lhs ds yokeMounted)
)



(defun NOBN-COMPLEX-SIGNAL ( MB HS FS TVS ZS AVS BPS MKS FKS LS LHS DS yokeMounted / config blockName yokePole basePole minMastWithoutDs )
	;	MB - ERTMS markerboard, stop sign (blue/yellow). Incompatible with HS and FS. One of [ nil | _left_ | _right_ | _down_ ]
	;	HS - Number of lanterner i Hovedsignal [0..3]
	;	FS - Number of Forsignaler [0..1]
	;	TVS - Number of TogveiSlutt signal [0..1]
	;	ZS - Number of Høyt skiftesignal på hovedsignals mast [0..1]
	;	AVS - Number of Departure signal (AVgang)
	;	BPS - Number of Brake test signal (BremsePrøve)
	;	MKS - Number of Middelkontrollampe [0..1]
	;	FKS - Number of signal Forsiktig Kjøring [0..1]
	;	LS - Number of Linjesignal [0..1]
	;	LHS - Number of Lysende hastighetssignal (signal 68), avvikende hastighet i sporveksel (annen enn 40 km/h) [0..1]
	;	DS -  Antall Dvergsignal [0..1]
	;
	;	yokeMounted - Mouting method [nil = upright mast | "AAK" = yoke mounting]
	
	;==================================================================================================================
	; A few TEST CASES for debugging - copy into VLIDE and step through LISP program:
	;---------------HS-FS-TVS-ZS-AVS-BPS-MKS-FKS-LS-LHS-DS
	;(setq MB nil HS 0  FS 1  TVS 0  ZS 0  AVS 0  BPS 0  MKS 0  FKS 0  LS 0  LHS 0  DS 0 yokemounted nil) ; Freestanding distant signal
	;(setq MB nil HS 0  FS 0  TVS 0  ZS 0  AVS 0  BPS 0  MKS 0  FKS 0  LS 0  LHS 0  DS 0 yokemounted nil) ; Missing symbol
	;(setq MB nil HS 3  FS 1  TVS 0  ZS 0  AVS 0  BPS 0  MKS 0  FKS 0  LS 0  LHS 0  DS 1 yokemounted nil) ; Hs3+Fs+Ds+Zs
	;(setq MB nil HS 0  FS 0  TVS 0  ZS 0  AVS 0  BPS 0  MKS 0  FKS 0  LS 0  LHS 0  DS 1 yokemounted nil) ; Ds

	;(setq MB nil HS 3  FS 1  TVS 1  ZS 1  AVS 1  BPS 1  MKS 1  FKS 1  LS 1  LHS 1  DS 1 yokemountednil )   ; Hs3+Fs+Ds+Zs  ;Everything, upright mast
	;(setq MB nil HS 3  FS 1  TVS 1  ZS 1  AVS 1  BPS 1  MKS 1  FKS 1  LS 1  LHS 1  DS 1 yokemounted"AAK" ) ; Hs3+Fs+Ds+Zs  ;Everything, yoke mounted

	;(if _DEBUG_ <paste test case here and remove semicolon, then set _DEBUG_ to true with (setq _DEBUG_ T)>)
	;(if _DEBUG_ (setq MB nil HS 3  FS 1  TVS 0  ZS 1  AVS 1  BPS 1  MKS 1  FKS 1  LS 1  LHS 1  DS 1 yokeMounted nil ))
	;(if _DEBUG_ (setq MB nil HS 0  FS 0  TVS 0  ZS 0  AVS 0  BPS 0  MKS 1  FKS 0  LS 0  LHS 0  DS 1 yokeMounted "AAK" ))
	;(if _DEBUG_ (setq MB _left_ HS 0  FS 0  TVS 0  ZS 0  AVS 0  BPS 0  MKS 1  FKS 0  LS 0  LHS 0  DS 1 yokeMounted "AAK" ))
	;Print current setting: (defun show ( / ) (foreach x '(HS FS TVS ZS AVS BPS MKS FKS LS LHS DS yokemounted) (progn (print x) (print (eval x)))))
	;==================================================================================================================

	(TraceLevel3
		(strcat "COMPLEX SIGNAL" 
			" MB:" (if (= mb nil) "-" mb)
			" HS:" (itoa hs)
			" FS:" (itoa fs)
			" TVS:" (itoa tvs)
			" ZS:" (itoa zs) 
			" AVS:" (itoa avs)
			" BPS:" (itoa bps)
			" MKS:" (itoa mks)
			" FKS:" (itoa fks)
			" LS:"  (itoa ls)
			" LHS:" (itoa lhs)
			" DS:" (itoa ds)
			" - " (if (= yokeMounted nil) "(mast)" "(yoke)")
		)
	)

	; Set locals
	(setq 
		config _emptyString_
		blockName (strcat _SIG_ "SIG-" "SIGNAL")
		description "SIGNAL"
		yokePole 2.0			; above lanterns if suspended from yoke.
		basePole 3.0			; a short pole stub below the lowest signal item down to the signal mast base, if upright mast.
		minMastWithoutDs 9.0	; minimum from base to lowest Hs lantern / top element, when no Ds is present.
	)
	
	; Set globals
	(InitalizeSignalSymbol)

	; The algorithm centers the very first item (when meaningful, such as solo MKs). Subsequent additions use the side of mast with the most free space.
	; Items are added in this order: HS -> Fs (-> AVs -> TLs) -> MKs -> Ls -> LHs -> Zs -> Ds

	; The 'Unknown combination' signal symbol
	
	(if (and (not MB) (= 0 (+ HS FS TVS ZS AVS BPS MKS FKS LS LHS DS)))
		(progn
			(setq blockName (strcat blockName "")) ; Nothing added => then RC will choose this one.
			(AddMissingSymbol)
		)
	)

	; ERTMS markerboard
	(cond
		((= MB nil) (progn)) ; No marker board
		((= MB _left_) (NOBN_AddMb _left_))
		((= MB _right_) (NOBN_AddMb _right_))
		((= MB _down_) (NOBN_AddMb _down_))
	)

	; Main signal ('Hovedsignal')
	; 0, 1, 2 or 3 times
	(repeat HS 
		(NOBN_AddHs1)
	)

	; End of train movement signal ('Togvei slutt')
	(if (= TVS 1)
		(NOBN_AddTVs)
	)

	; Distant signal ('Forsignal')
	(if (= FS 1)
		(NOBN_AddFs)
	)
	
	; High shunting signal ('Høyt skiftesignal') (#1 of 2 possible locations)
	; Place above above items such as AVs or BPs and MKs when neither Hs nor TVs is present.
	(if (= ZS 1)
		(if (= (+ HS TVS) 0)
			(NOBN_AddZs)
		)
	)

	; Cautious driving ('Forsiktig kjøring')
	; Strongly biased to the right side.
	(if (= FKS 1)
		(NOBN_AddFKs)
	)

	; Position lamp ('Middelkontrollampe') (#1 of 3 possible locations)
	; Place MKs as high as possible whenever there is a main signal lantern or 'S' signal on the mast,
	; otherwise try again below items Zs, AVs, BPs.
	; Biased to the left side.
	(if (= MKS 1)
		(if (> HS 0) ; Hs1, Hs2, Hs3
			(NOBN_AddMKs)
		)
	)

	; Departure signal ('Avgangsignal')
	(if (= AVS 1)
		(NOBN_AddFormSignalWithText "A") ; 'Avgangsignal'
	)

	; Brake test signal ('Bremseprøvesignal')
	(if (= BPS 1)
		(NOBN_AddFormSignalWithText "TL") ; 'Tilsett brems' / 'Løs brems'
	)

	; Position lamp ('Middelkontrollampe') (#2 of 3 possible locations)
	; When neither Hs or TVs is on the mast, then MKs is postponed, placed below possible items Zs, AVs, BPs.
	; Special treatment if Ds is present, then DsMKs is treated as a combo symbol.
	(if (= MKS 1)
		(if (= HS 0) ; No Hs...
			(if (= DS 0) ; ..and no Ds...
				(NOBN_AddMKs) ; Place MKs below previous item (no Ds on mast)
			;else 
				; MKs will be added further down as the DsMKs combo
			)
		)
	)

	; Line signal ('Linjesignal')
	(if (= LS 1)
		(NOBN_AddLs)
	)

	; Luminous speed signal ('Lysende hastighetssignal')
	(if (= LHS 1)
		(NOBN_AddLHs)
	)

	; High shunting signal ('Høyt skiftesignal') - alternative #2, with Hs or TVs present.
	; Place at bottom, occuoying the same spot as if Ds were present instead of Zs.
	; Note: Zs and Ds shall not occur on the same mast.
	(if (= ZS 1)
		(if (/= (+ HS TVS) 0)
			(NOBN_AddZs)
		)
	)

	; Dwarf signal ('Dvergsignal'), and alternative #3 for the MKs placement.
	; Add as Ds+MKs combo if MKs has not already been added
	; Note: Zs and Ds shall not occur on the same mast.
	(if (= DS 1) 
		(if (and (= HS 0) (= MKS 1))
			(NOBN_AddDsMKs) ; Free-standing Ds with MKs
		; else
			(NOBN_AddDs) ; Free-standing Ds without Mk, or MKs has previously been added to Hs/TVs mast
		)
	)

	; Construct configuration string:
	
	(cond
		((= MB _left_) (setq config (strcat config "-ERTMS-E35-HSIDE"))) 	; Left arrow
		((= MB _right_) (setq config (strcat config "-ERTMS-E35-VSIDE"))) 	; Right arrow
		((= MB _down_) (setq config (strcat config "-ERTMS-E35-OVER"))) 	; Down arrow
	)
	(if (= HS 1) (setq config (strcat config "-HS1")))
	(if (= HS 2) (setq config (strcat config "-HS2")))
	(if (= HS 3) (setq config (strcat config "-HS3")))
	(if (= FS 1) (setq config (strcat config "-FS")))
	(if (= TVS 1) (setq config (strcat config "-TVS")))
	(if (= DS 1) (setq config (strcat config "-DS")))
	(if (= ZS 1) (setq config (strcat config "-ZS")))
	(if (= MKS 1) (setq config (strcat config "-MKS")))
	(if (= FKS 1) (setq config (strcat config "-FKS")))
	(if (= LS 1) (setq config (strcat config "-LS")))
	(if (= LHS 1) (setq config (strcat config "-LHS")))
	(if (= AVS 1) (setq config (strcat config "-AVS")))
	(if (= BPS 1) (setq config (strcat config "-BPS")))
	(if yokeMounted (setq config (strcat config "-AAK")))
	
	(setq description (strcat description " " (substr config 2 ))); Skip first '-' character in 'config'

	; Add yoke pole / add rest of main pole with Hs base
	(if yokeMounted
		(progn
			(ShiftSignalItemsUp (- (+ yokePole totalHeight))) ; shift *down*
			(NOBN_DrawVerticalPole (- yokePole))
		)
	;else
		(progn
			(if (and (= DS 0) (< (+ topOfMast basePole) minMastWithoutDs)) ;If no Ds present, check if current mast top including "base pole" (low part) is at least minMastWithoutDs high
				(setq missing (- minMastWithoutDs (+ topOfMast basePole)))
			; else
				; Otherwise (with a Ds present, or the mast is already long enough):
				(setq missing 0.0) ; ...just add a short pole below the lowest signal item
			)
			(ShiftSignalItemsUp (+ missing basePole))
			(NOBN_DrawVerticalPole (+ missing basePole)) ; ensures that the missing part is at least basePole
			(NOBN_DrawHorizontalHsBase)
		)
	)
	
	; Create blocks
	(setq blockName (strcat blockName config))
	(AddDescriptionBelowOrigin description _descriptionTextHeight_) 
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;====================================================================================
; Manage global variables, keep track of the growing signal as items are added
;====================================================================================
(defun InitalizeSignalSymbol ( / )
	(setq 
		isTopmostItem 		T	; If True ('T'): next free-standing auxiliary signal item will be centered on pole axis
		topOfMast			0.0 ; Global, keep track of where current bottom center of uppermost item is
		totalHeight			0.0 ; Global, keep track of current total symbol height
		freeSpaceLeftSide	0.0 ; Global, space for extra lanterns to the left of main pole
		freeSpaceRightSide	0.0 ; Global, space for extra lanterns to the right of main pole
		freeSpaceBias		3.0 ; Use such as: 'Give priority to RIGHT side if freeSpaceRightSide >= freeSpaceLeftSide + freeSpaceBias'
	)
	; DEBUG: Use VLIDE or other means to set (setq _DEBUG_ T) to include model space cleanup and printing of globals:
	(if _DEBUG_ (command _ERASE_ _selectAll_ _ENTER_)) ; For debugging - clean up modelspace (your computer screen)
	(if _DEBUG_ (PrintSignalCombinationGlobals))
)



(defun PrintSignalCombinationGlobals ( / ) 
	; Show global values for signal combination generation - useful when debugging
	(princ 
		(strcat 
			"GLOBALS: isTopmostItem=" (if isTopmostItem "Yes" "No")
			" totalHeight=" (rtos totalHeight 2 2)
			" topOfMast=" (rtos topOfMast 2 2)
			" freeSpaceLeftSide=" (rtos freeSpaceLeftSide 2 2)
			" freeSpaceRightSide=" (rtos freeSpaceRightSide 2 2)
			" freeSpaceRightSide=" (rtos freeSpaceRightSide 2 2) 
			" freeSpaceBias=" (rtos freeSpaceBias 2 2) 
			" (end)"
		)
	)
)

;====================================================================================
; Add signal items
;====================================================================================
(defun AddAnchor ( / )
	; Force next items to start here, on the right side
	; no pushing up
	; Please comment out the next line - graphics only added in debug versions
	;(command _CIRCLE_ _origin_ 0.5) ; For debugging - add small 'anchor' circle
	(setq freeSpaceLeftSide 0.0)
	(setq freeSpaceRightSide 0.0)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
)



(defun ShiftSignalItemsUp ( offs / )
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 offs)) ; Shift everything up by 'd'. Negative 'd' shifts down.
	(setq topOfMast (+ topOfMast offs))
	(setq totalHeight (+ totalHeight offs))
)



(defun AddMissingSymbol ( / )
	(ShiftSignalItemsUp 3.5) ; Update globals - make space for two lines of 1.25 height text plus line spacing
	(AddMText layDef_Zero _th125_ (* 90 _th125_) _origin_ "MANGLER SYMBOL")
)


(defun NOBN_AddMb ( dir / )
	(setq 
		itemHeight 6.0 ; Markerboard height (and width)
	)
	(ShiftSignalItemsUp itemHeight) ; Not really needed, Mb is already the topmost item 
	(NOBN_DrawMb dir)
	(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
);NOBN_AddMb



(defun NOBN_AddHs1 ( / )
	(setq 
		r (NOBN_GetLargeLanternRadius)
		itemHeight (* 2 r)
	)
	(ShiftSignalItemsUp r)
	(NOBN_DrawLantern _origin_ r) ; Centered on main pole
	(ShiftSignalItemsUp r)
	(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
);NOBN_AddHs1



(defun NOBN_AddTVs ( / ) 
	; Togvei slutt signal
	(setq 
		itemHeight 4.0 ; An 'S' shape
	)
	(ShiftSignalItemsUp itemHeight)
	(NOBN_DrawTVs 0 0) ; The 'S' shape will always be centered on mast axis
	(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
);NOBN_AddTVs



(defun NOBN_AddFs ( / ) 
	(setq 
		r (NOBN_GetLargeLanternRadius)
		itemHeight (* 4 r)
	)
	(ShiftSignalItemsUp r)
	(NOBN_DrawLantern (list r 0) r) ; Fs lantern placed right of main pole
	(ShiftSignalItemsUp (* 2 r))
	(NOBN_DrawLantern (list r 0) r) ; Fs lantern placed right of main pole
	(ShiftSignalItemsUp r)
	(NOBN_DrawVerticalPole itemHeight)
	(setq freeSpaceLeftSide (+ freeSpaceLeftSide itemHeight))
	(setq freeSpaceRightSide 0.0)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddFs



(defun NOBN_AddZs ( / itemHeight itemWidth w2 missing )
	(setq
		itemHeight 4.5 ; standing rectangle with horizontal bar
		itemWidth 3.0
		w2 (/ itemWidth 2)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp itemHeight)
			(NOBN_DrawZs 0 0) ; Pos for bottom center of box
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			(setq freeSpaceLeftSide 0.0)
			(setq freeSpaceRightSide 0.0)
			(setq isTopmostItem nil)
		)
	;else
		(progn
			; Give priority to RIGHT side if Right + bias >= Left
			(if (>= (+ freeSpaceRightSide freeSpaceBias) freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawZs w2 0) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawZs w2 (- freeSpaceRightSide itemHeight)) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawZs (- w2) 0) ; draw on left side
							(setq freeSpaceLeftSide 0) ; reset free space left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawZs (- w2) (- freeSpaceLeftSide itemHeight)) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddZs



(defun NOBN_AddFormSignalWithText ( text / itemHeight itemWidth w2 missing )
	(setq
		itemHeight 3.0 ; Box 3x3 with fixed text
		itemWidth 3.0
		w2 (/ itemWidth 2)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp itemHeight)
			(NOBN_DrawFormSignalWithText text 0 0) ; Pos for bottom center of box
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			(setq freeSpaceLeftSide 0.0)
			(setq freeSpaceRightSide 0.0)
			(setq isTopmostItem nil)
		)
	;else
		(progn
			; Give priority to RIGHT side if Right + bias >= Left
			(if (>= (+ freeSpaceRightSide freeSpaceBias) freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawFormSignalWithText text w2 0) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawFormSignalWithText text w2 (- freeSpaceRightSide itemHeight)) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawFormSignalWithText text (- w2) 0) ; draw on left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
							(setq freeSpaceLeftSide 0) ; reset free space left side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawFormSignalWithText text (- w2) (- freeSpaceLeftSide itemHeight)) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddFormSignalWithText



(defun NOBN_AddMKs ( / r itemHeight missing )
	(setq 
		r (NOBN-GetMediumLanternRadius)
		itemHeight (* 2 r)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp r)
			(NOBN_DrawLantern _origin_ r)
			(ShiftSignalItemsUp r)
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			; No change to freeSpaceLeft or freeSpaceRight
		)
	;else
		(progn
			; Give priority to RIGHT side only if Right > Left (no bias)
			(if (> freeSpaceRightSide freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawMKs r r) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawLantern (list (+ r) (- freeSpaceRightSide r)) r) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawMKs (- r) r) ; draw on left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
							(setq freeSpaceLeftSide 0) ; reset free space left side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawLantern (list (- r) (- freeSpaceLeftSide r)) r) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)
);NOBN_AddMKs



(defun NOBN_AddFKs ( / itemHeight itemWidth w2 missing )
	(setq
		itemHeight 3.0 ; the FKs32 square box with five small circles
		itemWidth 3.0 
		w2 (/ itemWidth 2)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp itemHeight)
			(NOBN_DrawFKs 0 0) ; Pos for bottom center of box
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			(setq freeSpaceLeftSide 0.0)
			(setq freeSpaceRightSide 0.0)
			(setq isTopmostItem nil)
		)
	;else
		(progn
			; Give STRONG priority to RIGHT side if Right + 3 x bias >= Left (i.e., place right, and immediately below Fs (if present)):
			(if (>= (+ freeSpaceRightSide (* 3 freeSpaceBias)) freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawFKs w2 0) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawFKs w2 (- freeSpaceRightSide itemHeight)) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawFKs (- w2) 0) ; draw on left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
							(setq freeSpaceLeftSide 0) ; reset free space left side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawFKs (- w2) (- freeSpaceLeftSide itemHeight)) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddFKs



(defun NOBN_AddLs ( / itemHeight itemWidth w2 missing )
	(setq
		itemHeight 3.0
		itemWidth 3.0 ; Box with first character of each name for the possible lines from here
		w2 (/ itemWidth 2)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp itemHeight)
			(NOBN_DrawLs 0 0) ; Pos for bottom center of box
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			(setq freeSpaceLeftSide 0.0)
			(setq freeSpaceRightSide 0.0)
			(setq isTopmostItem nil)
		)
	;else
		(progn
			; Give priority to RIGHT side if Right + bias >= Left:
			(if (>= (+ freeSpaceRightSide freeSpaceBias) freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawLs w2 0) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawLs w2 (- freeSpaceRightSide itemHeight)) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawLs (- w2) 0) ; draw on left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
							(setq freeSpaceLeftSide 0) ; reset free space left side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawLs (- w2) (- freeSpaceLeftSide itemHeight)) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddLs



(defun NOBN_AddLHs ( / itemHeight itemWidth w2 missing )
	(setq
		itemHeight 3.0
		itemWidth 3.0 ; Box with speed attribute
		w2 (/ itemWidth 2)
		missing nil
	)
	(if isTopmostItem ; centered
		(progn
			(ShiftSignalItemsUp itemHeight)
			(NOBN_DrawLHs 0 0) ; Pos for bottom center of box
			(setq topOfMast (- topOfMast itemHeight)) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
			(setq freeSpaceLeftSide 0.0)
			(setq freeSpaceRightSide 0.0)
			(setq isTopmostItem nil)
		)
	;else
		(progn
			; Give priority to RIGHT side if Right + bias >= Left:
			(if (>= (+ freeSpaceRightSide freeSpaceBias) freeSpaceLeftSide)
				(progn ; Use right side
					(setq missing (- itemHeight freeSpaceRightSide))
					(if (>= missing 0.0)  ; Not enough free space on right side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawLHs w2 0) ; draw on right side
							(setq freeSpaceLeftSide (+ freeSpaceLeftSide missing)) ; increase free space left side
							(setq freeSpaceRightSide 0) ; reset free space right side
						)
					;else - enough free space right side
						(progn
							(NOBN_DrawLHs w2 (- freeSpaceRightSide itemHeight)) ; draw on right side, as high as possible
							; no change in free space left side
							(setq freeSpaceRightSide (- freeSpaceRightSide itemHeight)) ; reduce free space right side
						)
					)
				)
				(progn ; Use left side
					(setq missing (- itemHeight freeSpaceLeftSide))
					(if (>= missing 0.0)  ; Not enough free space on left side?
						(progn
							(ShiftSignalItemsUp missing) ; shift up just enough
							(NOBN_DrawVerticalPole missing) ; add to pole
							(NOBN_DrawLHs (- w2) 0) ; draw on left side
							(setq freeSpaceRightSide (+ freeSpaceRightSide missing)) ; increase free space right side
							(setq freeSpaceLeftSide 0) ; reset free space left side
						)
					;else - enough free space left side
						(progn
							(NOBN_DrawLHs (- w2) (- freeSpaceLeftSide itemHeight)) ; draw on left side, as high as possible
							; no change in free space right side
							(setq freeSpaceLeftSide (- freeSpaceLeftSide itemHeight)) ; reduce free space left side
						)
					)
				)
			)
		)
	)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
);NOBN_AddLHs



(defun NOBN_AddDs ( / itemHeight curvedHeight p1 p2 p3 x y p4 extraMastAbove )
	(setq
		itemHeight 4.0   ; Height of dwarf + extra space above dwarf, for instance in combination TVs/AVs/BPs+Ds
		curvedHeight (* itemHeight (/ (sqrt 3.0) 2)) ; Ds height measured along main pole axis, where the curved Ds arc intersects with the main axis
		extraMastAbove 2.0	; Extra space above dwarf, required for tcombinations Hs/TVs/Zs/AVs/BPs + Ds
	)
	; Assume that the Ds is suspended below other signals (which can not be an 'S').
	; The cases for free-standing shunting signals, with TogveiSlutt 'S' and MKs are covered in other LISP routines.

	(if (not isTopmostItem)
		; There is already something above the dwarf:
		(progn
			; Add a pole stub above dwarf head, to produce a minimum separation between Ds and Hs (needed since left part of Ds is higher than the arced middle)
			(setq missing (- (+ extraMastAbove itemHeight) curvedHeight))
			(ShiftSignalItemsUp missing) 
			(NOBN_DrawVerticalPole missing)
		)
	)
	(ShiftSignalItemsUp curvedHeight)
	(NOBN_DrawDs)
	(AddAnchor)
);NOBN_AddDs



(defun NOBN_AddDsMKs ( / itemHeight curvedHeight p1 p2 p3 x y p4 extraMastAbove )
	(setq
		itemHeight 4.0   ; Height of dwarf
		curvedHeight (* itemHeight (/ (sqrt 3.0) 2)) ; Ds height measured along main pole axis, where the curved Ds arc intersects with the main axis
		extraMastAbove 2.0	; Extra space above dwarf, required for tcombinations Hs/TVs/Zs/AVs/BPs + Ds
	)
	; Assume that the Ds is suspended below other signals (which can not be an 'S').
	; The cases for free-standing shunting signals, with TogveiSlutt 'S' and MKs are covered in other LISP routines.

	(if (not isTopmostItem)
		; There is already something above the dwarf...
		(progn
			; Add a pole stub above dwarf head, to produce a minimum separation between Ds and Hs (needed since left part of Ds is higher than the arced middle)
			(setq missing (- (+ extraMastAbove itemHeight) curvedHeight))
			(ShiftSignalItemsUp missing) 
			(NOBN_DrawVerticalPole missing)
		)
	)
	(ShiftSignalItemsUp curvedHeight)
	(NOBN_DrawDsMKs)
	(AddAnchor)
);NOBN_AddDsMKs
