;=========================================================================================================================
;
; GetBoardAndPoleNames.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Function called by 21_GENERATE-BOARDSANDPOLES-OBJECTS in order to call individual board generation routines one by one.
; Note: Stated names must conform with the routine's name, but the name of that LISP file does not matter,
; only that æøåÆØÅ cannot be used in LISP identifiers, such as function names.

; Use autocad 'DATAEXTRACTION' in acad existing symbol file to export symbol info, then add quotation marks and paste from Excel.
; NB: No spaces before/after block names.

(defun GetBoardAndPoleNames ( / functionNames f )		
	(cond 
		((= _ADM_ _XXGL_) 
			; XX-GL actions:
			(TraceLevel2 "No boards defined for XX-GL yet")
		)
		((= _ADM_ _NOBN_) 
			; NO-BN actions:
			(setq 
				functionNames (list 
					; Skilt med signalnummer i TRV / SJT forskrifter:
					"101-1" "101-2" "101-3" "101-5" "101-6" "101-7" "101-8"
					"102-1" "102-2" "102-3"
					"60A" "60B" "60C" "60D" "60D-10" "60D-40" "60E" "60F" "60F-TILLEGGSTEKST" "60G" "60G-TILLEGGSTEKST" "60H"
					"61A" "61B" "61C" "E61"
					"62A"
					"63A" "63B" 
					"64A" "64B" "64C" "64D" "64E" "64F" 
					"65A" "65B" "65C" "65D" "65E" "65F" "65G-1" "65G-2" "65G-3" "E65H" "E65J" "E65K" "E65L" "E65M" "E65N"
					"66A"
					"67A" "67B" "67C" "67D" "67E" "68A"
					"68B" "68C" "68D-1" "68D-2" "68D-3" "68F" "68G"
					"69A" "69B" 
					"70A"

					"72A" "72B" "73A"
					"74-1" "74-2"
					"75A-1" "75A-2" "75A-3" "75B" "75C-1" "75C-2" "75D-1" "75D-2" "75E"
					"E200"
					"E201"
					"E202"
					"E203"
					"E204"
					"E205"
					"E206"
					; "ANYADM-ANNOTATION-CHAINBREAK" 2020-07-26 CLFEY: Moved to "Annotation Chainbreak.lsp" since annotations shall scale with CAD zoom level, not with drawing size.
		
					; Skilt uten signalnummer:
					"Skilt Anleggsomraade begynner" "Skilt Anleggsomraade slutter"
					"Skilt Arbeidsomraade 1-2" "Skilt Arbeidsomraade 1-3" "Skilt Arbeidsomraade 2-2" "Skilt Arbeidsomraade 2-3" "Skilt Arbeidsomraade 3"
					"Skilt ATC Innkoblet" "Skilt ATC Slutter" "Skilt ATC Utkoblet"  ; Not in use for new lines, see 60.lsp
					"Skilt Balise"
					"Skilt Bevegelig kryss"
					"Skilt Driftsbanegaard" "Skilt Driftsbanegaard slutter"
					"Skilt Fritekst" "Skilt Fritekst slutter"
					"Skilt Roemningsavstand H" "Skilt Roemningsavstand V" "Skilt Roemningsavstand VH"
					"Skilt Sidespor"
					"Skilt Spornummer"
					"Skilt Stopp se og lytt"
					"Skilt Strekning uten linjeblokk"
					"Skilt Togvarmeanlegg 1000V"
					"Skilt Tredjeperson 138 mot vei" "Skilt Tredjeperson 138 mot vei flere spor"
					"Skilt Underskilt Meterangivelse"
				)
			)
		)
		((= _ADM_ _FRSR_) 
			; FR-SR actions:
			(TraceLevel2 "No boards defined for FR-SR yet")
		)
		((= _ADM_ _DEDB_) 
			; DE-DB actions:
			(TraceLevel2 "No boards defined for DE-DB yet")
		)
	)

	; Transform files names into function names:
	(setq f (mapcar 'TransformFileNameIntoFunctionName functionNames))
)



(defun TransformFileNameIntoFunctionName ( f )
	(repeat 10 (setq f (vl-string-subst  "-"  " "  f))) ; Assume no function name has more than 10 spaces
	(setq f (vl-string-right-trim "\t" f))
	(setq f (vl-string-left-trim "\t" f))
)
