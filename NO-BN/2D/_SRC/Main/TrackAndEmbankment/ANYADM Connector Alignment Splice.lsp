;=========================================================================================================================
;
; ANYADM Connector Alignment Splice.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

; Concerns schematic symbols that scale with paperspace drawing scale.

(defun ANYADM-CONNECTOR-SPLICE ( / )
	; Connects an alignment to another alignment when it has been recognized as a valid extension of the first alignment.
	; The symbols look like this: [>>], [><], [<>], and [<<]. The arrows represent each alignment's direction of increasing mileages.
	(SetCadSystemDefaults)  
	(TraceLevel3 "ANYADM-TRACK-CONTINUATION")				(ANYADM-TRACK-CONTINUATION)
	(TraceLevel3 "ANYADM-HIGH-VOLTAGE-CABLE-CONTINUATION")	(ANYADM-HIGH-VOLTAGE-CABLE-CONTINUATION)
	(TraceLevel3 "ANYADM-CONTACT-WIRE-CONTINUATION")		(ANYADM-CONTACT-WIRE-CONTINUATION)
)



(defun ANYADM-TRACK-CONTINUATION ( / blockName description x y leftArrow rightArrow )
	(setq
		x	3.0
		y	1.0
	)
	(foreach leftArrow '(0 1)
		(foreach rightArrow '(0 1)
			(cond 
				((= _ADM_ _XXGL_) (setq blockName (strcat _TRK_ "SPL-" "CONNECTOR-SPLICE-" 					(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _NOBN_) (setq blockName (strcat _TRK_ "SPF-" "FORBINDELSE-FORTSETTELSE-"			(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _FRSR_) (setq blockName (strcat _TRK_ "PRO-" "CONNEXION-PROLONGATION-"			(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _DEDB_) (setq blockName (strcat _TRK_ "AFO-" "VERBINDUNG-ACHSEN-FORTSETZUNG-"		(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _JPTX_) (setq blockName (strcat _TRK_ "SPL-" "CONNECTOR-SPLICE-" 					(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
			)
			(cond 
				((= _ADM_ _XXGL_) (setq description (strcat "CONNECTION SPLICE "		(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _NOBN_) (setq description (strcat "SPORFORTSETTELSE "			(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _FRSR_) (setq description (strcat "CONNEXION PROLONGATION "	(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _DEDB_) (setq description (strcat "ACHSEN-FORTSETZUNG "		(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _JPTX_) (setq description (strcat "CONNECTION SPLICE "		(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
			)
			(DrawContinuation x y leftArrow rightArrow)
			(AddDescriptionBelowOrigin description y)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		)
	)
)



(defun ANYADM-HIGH-VOLTAGE-CABLE-CONTINUATION ( / blockName description x y leftArrow rightArrow )
	(setq
		x	3.0
		y	1.0
	)
	(foreach leftArrow '(0 1)
		(foreach rightArrow '(0 1)
			(cond 
				((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "CSP-" "CABLE-SPLICE-" 						(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "LEF-" "LEDNINGFORTSETTELSE-"				(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "CPC-" "CONNEXION-PROLONGATION-CABLE"		(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "VKF-" "VERBINDUNG-KABEL-FORTSETZUNG-"		(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _JPTX_) (setq blockName (strcat _OCS_ "CSP-" "CABLE-SPLICE-" 						(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
			)
			(cond 
				((= _ADM_ _XXGL_) (setq description (strcat "CABLE SPLICE "				(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _NOBN_) (setq description (strcat "LEDNINGFORTSETTELSE "		(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _FRSR_) (setq description (strcat "PROLONGATION DE CABLE "	(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _DEDB_) (setq description (strcat "KABEL-FORTSETZUNG "		(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _JPTX_) (setq description (strcat "CABLE SPLICE "				(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
			)
			(DrawContinuation x y leftArrow rightArrow)
			(AddDescriptionBelowOrigin description y)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		)
	)
)



(defun ANYADM-CONTACT-WIRE-CONTINUATION ( / blockName description x y leftArrow rightArrow )
	(setq
		x	3.0
		y	1.0
	)
	(foreach leftArrow '(0 1)
		(foreach rightArrow '(0 1)
			(cond 
				((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "WSP-" "CONTACT-WIRE-SPLICE-" 					(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "KTF-" "KONTAKTLEDNING-FORTSETTELSE-"			(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "CPF-" "CONNEXION-PROLONGATION-FIL-DE-CONTACT"	(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "AFO-" "VERBINDUNG-KONTAKTLEITUNG-FORTSETZUNG-"	(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
				((= _ADM_ _JPTX_) (setq blockName (strcat _OCS_ "WSP-" "CONTACT-WIRE-SPLICE-" 					(rtos leftArrow 2 0) "-" (rtos rightArrow 2 0)	)))
			)
			(cond 
				((= _ADM_ _XXGL_) (setq description (strcat "CONTACT WIRE SPLICE "			(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _NOBN_) (setq description (strcat "KONTAKTLEDNINGFORTSETTELSE "	(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _FRSR_) (setq description (strcat "PROLONGATION FIL-DE-CONTACT "	(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _DEDB_) (setq description (strcat "KONTAKTLEITUNG-FORTSETZUNG "	(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
				((= _ADM_ _JPTX_) (setq description (strcat "CONTACT WIRE SPLICE "			(if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))	))
			)
			(DrawContinuation x y leftArrow rightArrow)
			(AddDescriptionBelowOrigin description y)
			(CreateSchematicBlockFromCurrentGraphics blockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
		)
	)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawContinuation (x y a b / x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 )
	;
	; 1                   3
	; |         x         | 
	; TL--5--6-----7--8--TR
	; |    \        \     |
	; |     \        \    |
	; |   9 10    11 12   |   y
	; |     /        /    |
	; |    /        /     |
	; BL-13-14----15-16--BR
	; |                   |
	; 2                   4
	;
	(setq
		p1	(list (* -0.500 x) (*  1.000 y))
		p2	(list (* -0.500 x) (* -1.000 y))
		p3	(list (*  0.500 x) (*  1.000 y))
		p4	(list (*  0.500 x) (* -1.000 y))

		p5	(list (* -0.333 x) (*  0.500 y))
		p6	(list (* -0.167 x) (*  0.500 y))
		p7	(list (*  0.167 x) (*  0.500 y))
		p8	(list (*  0.333 x) (*  0.500 y))
		
		p9	(list (* -0.333 x) (*  0.000 y))
		p10 (list (* -0.167 x) (*  0.000 y))
		p11 (list (*  0.167 x) (*  0.000 y))
		p12	(list (*  0.333 x) (*  0.000 y))

		p13 (list (* -0.333 x) (* -0.500 y))
		p14 (list (* -0.167 x) (* -0.500 y))
		p15 (list (*  0.167 x) (* -0.500 y))
		p16	(list (*  0.333 x) (* -0.500 y))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(if (= a 0)
		(command _POLYLINE_ p5 p10 p13 _openPolyline_)	;'>'
	;else
		(command _POLYLINE_ p6 p9 p14 _openPolyline_)	;'<'
	)
	(if (= b 0)
		(command _POLYLINE_ p7 p12 p15 _openPolyline_)	;'>'
	;else
		(command _POLYLINE_ p8 p11 p16 _openPolyline_)	;'<'
	)
)
