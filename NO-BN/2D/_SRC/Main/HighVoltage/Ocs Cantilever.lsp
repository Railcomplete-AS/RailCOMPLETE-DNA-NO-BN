;=========================================================================================================================
;
; Ocs Cantilever.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Cantilever

(defun OCS-CANTILEVER ( step / )
	; Implemented for all administrations:
	(TraceLevel3 "BRACKET-FOR-MULTIPLE-CANTILEVERS")	(BRACKET-FOR-MULTIPLE-CANTILEVERS) ; For mounting of multiple cantilevers on the same mast. Argument: Separation of cantilevers.

	; Specific to this administration:
	(setq
		; Let these be global for a while...
		; Preferred step parameter: 1.000..6.000 metres in steps of 0.050 (110 x 10 variations x push+pull => 2200 symbols)
		; But due to CAD symbol generation times being excessive, restrict to cantileverStep=500 (gives about 2 minutes computing time for cirka 110 cantilevers)
		minLen 500	; shortest cantilever symbol [mm]
		maxLen 6000	; longest [mm]
		cantileverStep step ; [mm] set by caller - usually 20 = production value (time consuming), 500 = OK while debugging 2D library
	)
	; System 20, 25 and 35:
	; S=strekk (pull-off = the contact wire sideways force strecthes the cantilever)
	; T=trykk (push-off = the contact wire sideways force compresses the cantilever)
	;S20A and S20AR
	(TraceLevel3 "NOBN-CANTILEVER-20A-PUSH")	(NOBN-CANTILEVER-20-25-35 "20A" "push")
	(TraceLevel3 "NOBN-CANTILEVER-20A-PULL")	(NOBN-CANTILEVER-20-25-35 "20A" "pull") 
	
	;S20B and S20BR
	(TraceLevel3 "NOBN-CANTILEVER-20B-PUSH")	(NOBN-CANTILEVER-20-25-35 "20B" "push")
	(TraceLevel3 "NOBN-CANTILEVER-20B-PULL")	(NOBN-CANTILEVER-20-25-35 "20B" "pull")
	
	;S20C1 and S20C2
	(TraceLevel3 "NOBN-CANTILEVER-20C-PUSH")	(NOBN-CANTILEVER-20-25-35 "20C" "push")
	(TraceLevel3 "NOBN-CANTILEVER-20C-PULL")	(NOBN-CANTILEVER-20-25-35 "20C" "pull")
	
	;S25
	(TraceLevel3 "NOBN-CANTILEVER-25-PUSH")		(NOBN-CANTILEVER-20-25-35 "25" "push")
	(TraceLevel3 "NOBN-CANTILEVER-25-PULL")		(NOBN-CANTILEVER-20-25-35 "25" "pull")
	
	;S35 and S35MS
	(TraceLevel3 "NOBN-CANTILEVER-35-PUSH")		(NOBN-CANTILEVER-20-25-35 "35" "push")
	(TraceLevel3 "NOBN-CANTILEVER-35-PULL")		(NOBN-CANTILEVER-20-25-35 "35" "pull")
	
	; Cariboni for regular tunnels
	(TraceLevel3 "NOBN-CANTILEVER-CARIBONI")	(NOBN-CANTILEVER-CARIBONI)
	
	; TET08 (Thor Egil Thoresen...) for narrow / low tunnels
	(TraceLevel3 "NOBN-CANTILEVER-TET08")	(NOBN-CANTILEVER-TET08)

	(setq
		; Remove temporary globals:
		minLen nil
		maxLen nil
		cantileverStep nil
	)
)



(defun BRACKET-FOR-MULTIPLE-CANTILEVERS ( / blockName description p1 p2 )
	; Console for fitting of two cantievers or more to the same catenary support mast.
	;
	;              (*)               <-- The OCS pole (*)
	;  1-----5   7--.--8   6-----4   <-- The insertion point '.' shall snap to outside of OCS pole
	;  2-------------------------3   <-- Outside of console becomes the new mounting surface for the cantilevers
	;     C1                  C2     <-- The cantilevers (C1,C2) shall move out by the console's width
	;
	(setq blockName	(strcat _OCS_ "UTK-" "KONSOLL-FOR-SEKSJONSUTLIGGER"		))
	(setq description	(strcat "KONSOLL FOR SEKSJONSUTLIGGER"				))
	; Fitting: See Elkraftportalen EH.707164 (for B-master) og EH.707165 (for H-mast, suitable also for HEB-mast).
	; UNP120 beam (for H-masts of varying mast widths) or UNP100..UNP200 (for B1..B6 masts). HEB masts use the H-xxx variants.
	; Two cantilevers are to be attached, at +/- 0.600 from center console.
	; Insertion point is center back of UNP-beam.
	; In 3D, two such consoles are needed, on for the lower cantilever consoles, one for the upper consoles.
	;
	; From Sicat Candrop manual for Bane NOR:
	;
	; Vinkling: Vinkling er sideforskyvning av dobbeltutliggere i seksjons- og vekslingsfelt. De fra mastmidt til mastmidt 
	; angitte spennlengder, korrigeres i seksjons- og vekslingsfelter med størrelsen på vinklingen, slik at man automatisk 
	; får en spennlengde som vil være fra utligger til utligger. Vinkling har standardverdi 0,65 m, som er en halv konsollengde.
	; Gjelder ikke for sporsløyfer.
	; NB! Bane NOR says elsewhere that spacing between dual cantilevers shall be 1,20 m.
	(setq
		p1	'(-0.72  0.000)	
		p2	'(-0.72 -0.055)
		p3	'( 0.72 -0.055)
		p4	'( 0.72  0.000)
		p5	'(-0.40  0.000)
		p6	'( 0.40  0.000)
		p7	'(-0.20  0.000)
		p8	'( 0.20  0.000)
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p1 p5)
	(DrawLine layDef_Zero p4 p6)
	(DrawLine layDef_Zero p7 p8)

	; Schematic symbol
	(ScaleAll _four_) ; suitable size - 4x real size
	(AddDescriptionBelowOrigin description 0) 
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; No annotative symbol - use metric in geographic symbol mode

	; Metric symbol
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_) ; Scale down to real size again
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-CANTILEVER-20-25-35 ( variation pushPull / blockName description len cantileverName lenM lenString pp )
	;
	;  .            .   
	;  |            |
	;  |            |
	; \|/           ^
	;  V Push      /|\ Pull         Push=compression force in cantilever, Pull=tensile force in cantilever
	;
	(setq pp (if (= pushPull "push") "T"		"S"		))		; Trykk/Strekk
	(setq blockName (strcat _OCS_ "UTL-" "UTLIGGER-"		variation "-" pp	))
	(setq description (strcat "KL UTLIGGER, SYSTEM "		variation "/" pp	))
	(setq
		len minLen
		cantileverName _emptyString_ ; See below
	)
	(SetLayer layDef_Zero)
	(while (<= len maxLen)
		(princ (strcat "L = " (rtos len) " mm\n"))
		(setq lenM (/ len 1000.0)) ; length in decimal meters
		
		; The cantilever 'trunk' is always visible (layer zero):
		(DrawLine layDef_Zero _origin_ (list 0 (- lenM))) ; The 'arm'
		(SetLayer layDef_PushPullDirection)
		(cond 
			((= pushPull "pull") ; Pull = The contact wire force stretches the cantilever ----->
				(command
					_LINE_ (list 0 (+ (- lenM) (+ 0.500))) "@1.25<-45" _ENTER_
					_LINE_ (list 0 (+ (- lenM) (+ 0.500))) "@1.25<-135" _ENTER_
				)
			)
			((= pushPull "push") ; Push = The contact wire force compresses the cantilever -----<
				(command
					_LINE_ (list 0 (+ (- lenM) (- 0.500))) "@1.25<45" _ENTER_
					_LINE_ (list 0 (+ (- lenM) (- 0.500))) "@1.25<135" _ENTER_
				)
			)
		)
		; Place text vertically at the right side of a leftsided cantilever:
		(SetLayer layDef_CantileverType)
		(command 
			_TEXT_
				_SetTextStyle_ _rcTextStyle_
				_justifyText_ _middleCenter_
				(list 0.2 -2.0) ; pos 
				_th020_
				_angle90_
				(strcat  variation "/" pp)
		)
		(setq lenString (rtos len _decimal_ 0)) ; Convert integer number to string
		(while (< (strlen lenString) 4) (setq lenString (strcat "0" lenString))) ; Leftsided zero-padding till we have 4 digits

		(setq cantileverName (strcat blockName "-" lenString))

		; Schematic symbol
		(ScaleAll _four_) ; suitable size - 4x real size
		(AddDescriptionBelowOrigin (strcat description " L=" lenString) (+ lenM 0.5))
		(CreateSchematicBlockFromCurrentGraphics cantileverName)

		; No annotative symbol - use metric in geographic symbol mode

		; Metric symbol
		(AddGraphicsFromScaledSchematicBlock cantileverName _quarter_) ; Scale down to real size again
		(CreateMetricBlockFromCurrentGraphics cantileverName)

		(setq len (+ len cantileverStep))
	)
)



(defun NOBN-CANTILEVER-CARIBONI ( / blockName description p1 p2 )
	(setq blockName (strcat _OCS_ "UTL-" "UTLIGGER-"	"CARIBONI" 	))
	(setq description (strcat "KL UTLIGGER, "			"CARIBONI"	))
	(setq 
		p1	'(0.0 -1.2) ; Standard placement (without cant) is 60 mm out from center of 100x150 suspension mast, located at 1.275 from track axis
	)
	; Schematic symbol:
	(DrawLine layDef_Zero _origin_ p1)  ; Just one size Cariboni cantilever
	(SetLayer layDef_CantileverType)
	(command 
		_TEXT_
			_SetTextStyle_ _rcTextStyle_
			_justifyText_ _middleCenter_
			(list 0.2 -0.5) ; pos 
			_th020_
			_angle90_
			"Car"
	)
	(AddDescriptionBelowOrigin description -3)
	(ScaleAll _four_) ; suitable size - 4x real size
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; No annotative symbol - use metric in geographic symbol mode

	; Metric symbol
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_) ; Scale down to real size again
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-CANTILEVER-TET08 ( / blockName description p1 p2 )
	; Bane NOR cantilever for very narrow tunnels, low ceilings. Fixed distance from wall console to wire clamp. L=700 800 or 900 mm for the steady arm (lett direksjonsstag).
	(setq blockName (strcat _OCS_ "UTL-" "UTLIGGER-"	"TET08" ))
	(setq description (strcat "KL UTLIGGER, "			"TET08"	))
	(setq 
		p1	'(0.0 -1.001) ; Standard placement (without cant) is tunnel wall at 1001 mm from CW wire clamp (ataggered) position.
	)
	; Schematic symbol:
	(DrawLine layDef_Zero _origin_ p1)  ; Just one size Cariboni cantilever
	(SetLayer layDef_CantileverType)
	(command 
		_TEXT_
			_SetTextStyle_ _rcTextStyle_
			_justifyText_ _middleCenter_
			(list 0.2 -0.5) ; pos 
			_th020_
			_angle90_
			"TET08"
	)
	(AddDescriptionBelowOrigin description -3)
	(ScaleAll _four_) ; suitable size - 4x real size
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; No annotative symbol - use metric in geographic symbol mode

	; Metric symbol
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_) ; Scale down to real size again
	(CreateMetricBlockFromCurrentGraphics blockName)
)
