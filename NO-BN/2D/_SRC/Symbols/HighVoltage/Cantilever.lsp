;=========================================================================================================================
;
; Cantilever.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Cantilever

(defun C:CANTILEVER ( / )
  	
	(KONSOLL-FOR-SEKSJONSUTLIGGER) ; For mounting of multiple cantilevers on the same mast. Argument: Separation of cantilevers.

	; System 20, 25 and 35:
	; S=strekk (pull-off = the contact wire sideways force strecthes the cantilever)
	; T=trykk (push-off = the contact wire sideways force compresses the cantilever)
	(subSubStep "UTLIGGER 20A-S")		(UTLIGGER-20-25-35 "20A" "S")
	(subSubStep "UTLIGGER 20A-T")		(UTLIGGER-20-25-35 "20A" "T")
	(subSubStep "UTLIGGER 20B-S")		(UTLIGGER-20-25-35 "20B" "S")
	(subSubStep "UTLIGGER 20B-T")		(UTLIGGER-20-25-35 "20B" "T")
	(subSubStep "UTLIGGER 20C-S")		(UTLIGGER-20-25-35 "20C" "S")
	(subSubStep "UTLIGGER 20C-T")		(UTLIGGER-20-25-35 "20C" "T")
  	(subSubStep "UTLIGGER 25-S")		(UTLIGGER-20-25-35 "25" "S")
  	(subSubStep "UTLIGGER 25-T")		(UTLIGGER-20-25-35 "25" "T")
  	(subSubStep "UTLIGGER 35-S")		(UTLIGGER-20-25-35 "35" "S")
  	(subSubStep "UTLIGGER 35-T")		(UTLIGGER-20-25-35 "35" "T")
  	(subSubStep "UTLIGGER-CARIBONI")	(UTLIGGER-CARIBONI)
)



(defun KONSOLL-FOR-SEKSJONSUTLIGGER ( / blockName description p1 p2 )
	; See Elkraftportalen EH.707164 (for B-master) og EH.707165 (for H-master, passer også for HEB-master).
	; UNP120 beam (for H-masts of varying mast widths) or UNP100..UNP200 (for B1..B6 masts). HEB masts use the H-xxx variants.
	; Two cantilevers are to be attached, at +/- 0.600 from center console.
	; Insertion point is center back of UNP-beam.
	; In 3D, two such consoles are needed, on for the lower cantilever consoles, one for the upper consoles.
	;
	; p1--------.--------p2
	; 
	(setq 
		blockName	"NO-BN-2D-JBTKL-KONSOLL-FOR-SEKSJONSUTLIGGER"
		description	"KL KONSOLL FOR SEKSJONSUTLIGGER"
		p1	'(-0.72 0)
		p2	'( 0.72 0)
	)
	(drawLine layDef_Zero p1 p2)
	(addDescriptionBelowOrigo description 0) 
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun UTLIGGER-20-25-35 ( variation pushPull / blockName description minLen maxLen step curLength len lenM lenString cantileverName )
	; pushPull values: ; S=strekk (pull-off), T=trykk (push-off)
	; Prefered parameters: 0.50..6.00 in steps of 0.05 (110 x 10 variations x push+pull => 2200 symbols)
	; But due to CAD symbol generation times being excessive, restrict to step=500 (gives about 2 minutes computing time for cirka 110 cantilevers)
	(setq 
		blockName	(strcat "NO-BN-2D-JBTKL-UTLIGGER-" variation "-" pushPull)
		description	"" ; See below
		; Integer values:
		minLen 1000	; shortest cantilever symbol [mm]
		maxLen 6000	; longest [mm]
		len minLen
		step 100	; step [mm]
		cantileverName "" ; See below
	)
	(setLayer layDef_Zero)
	(while (<= len maxLen)
		(princ (strcat "L = " (rtos len) " mm\n"))
		(setq lenM (/ len 1000.0)) ; length in decimal meters
		(progn
			; Push:
			; The cantilever 'trunk' is always visible (layer zero):
			(drawLine layDef_Zero _origo_ (list 0 (- lenM))) ; The 'arm'
			(setLayer layDef_PushPullDirection)
			(cond 
				((= pushPull "S") ; 'Strekk' - Pull = The contact wire forces stretch the cantilever ----->
					(command
						_LINE_ (list 0 (+ (- lenM) (- 0.500))) "@1.25<45" _ENTER_
						_LINE_ (list 0 (+ (- lenM) (- 0.500))) "@1.25<135" _ENTER_
					)
				)
				((= pushPull "T") ; 'Trykk' - Push = The contact wire forces compress the cantilever -----<
					(command
						_LINE_ (list 0 (+ (- lenM) (+ 0.500))) "@1.25<-45" _ENTER_
						_LINE_ (list 0 (+ (- lenM) (+ 0.500))) "@1.25<-135" _ENTER_
					)
				)
			)
			(addTextAtPos layDef_CantileverType _th070_ (list 2.5 (+ lenM -2)) (strcat  variation "/" pushPull))
		)
		(setq lenString (rtos len 2 0)) ; from 50 to 6000, say (i.e. 2 to 4 digits)
		(while (< (strlen lenString) 4) (setq lenString (strcat "0" lenString))) ; Leftsided zero-padding till we have 4 digits

		(setq description (strcat "KL UTLIGGER SYSTEM " variation "/" pushPull " L=" lenString))
		(addDescriptionBelowOrigo description (+ lenM 0.5))

		(setq cantileverName (strcat blockName "-" lenString))

		; Schematic symbol
		(scaleAll _four_)
		(createSchematicBlockFromCurrentGraphics cantileverName)

		; No annotative symbol - use metric in geographic symbol mode

		; Metric symbol
		(addGraphicsFromScaledSchematicBlock cantileverName _quarter_)
		(createMetricBlockFromCurrentGraphics cantileverName)

		(setq len (+ len step))
	)
)



(defun UTLIGGER-CARIBONI ( / blockName description p1 p2 )
	(setq 
		blockName 	"NO-BN-2D-JBTKL-UTLIGGER-CARIBONI"
		description	"KL UTLIGGER CARIBONI"
		p1	'(0.0 -1.2) ; Standard placement (without cant) is 60 mm out from center of 100x150 suspension mast, located at 1.275 from track axis
		p2	'(2.0 -2.7) ; Push-Pull text (letter S/T)
	)
	; S symbol:
	(drawLine layDef_Zero _origo_ p1)  ; Just one size Cariboni cantilever
	(addTextAtPos layDef_CantileverType _th070_ p2 "Car")
	(addDescriptionBelowOrigo description -3)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
