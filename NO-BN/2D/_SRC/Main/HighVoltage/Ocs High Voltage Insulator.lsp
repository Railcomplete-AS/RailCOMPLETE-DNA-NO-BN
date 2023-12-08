;=========================================================================================================================
;
; Ocs High Voltage Insulator.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Isolator (in contact wire, catenary, fix lines, and other lines)

(defun OCS-HIGH-VOLTAGE-INSULATOR ( / )
	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "OCS-STRAIN-INSULATOR")			(OCS-STRAIN-INSULATOR)
	(TraceLevel3 "OCS-SECTION-INSULATOR")			(OCS-SECTION-INSULATOR)
	(TraceLevel3 "OCS-COVERED-WIRE-INSULATOR")		(OCS-COVERED-WIRE-INSULATOR)
)



(defun OCS-STRAIN-INSULATOR ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 )
	;2020-04-07 Bane NOR schematic 2D layer/symbol: JBTEH_Komponenter / EH_KL_Isolator
	;TODO: Ask Bane NOR for permission to use EH-ISO for this one.
	;Two arcs forming a ")(" with a wipeout bewtween them. 
	;Default insertion direction is "up" in wire / catenary / contact wire alignment.
	;
	;    1   2
	;     \ /
	;     3-4
	; ----5.6----
	;     7-8
	;     / \
	;    9   10
	;
	(setq 
		blockName	(strcat _OCS_ "ISO-" "ISOLATOR-LINEISOLATOR")
		description "KL LINEISOLATOR"
		x 3.25
		y 4.5
		p1  (list (* -0.500 x) (*  0.500 y))
		p2  (list (*  0.500 x) (*  0.500 y))
		p3  (list (* -0.265 x) (*  0.222 y))
		p4  (list (*  0.265 x) (*  0.222 y))
		p5  (list (* -0.215 x) (*  0.000 y))
		p6  (list (*  0.215 x) (*  0.000 y))
		p7  (list (* -0.265 x) (* -0.222 y))
		p8  (list (*  0.265 x) (* -0.222 y))
		p9  (list (* -0.500 x) (* -0.500 y))
		p10 (list (*  0.500 x) (* -0.500 y))
	)
	; Wipeout between the two curved bars to remove the isolated part of wire under the line insulator:
	(command _POLYLINE_ p3 p4 p6 p8 p7 p5 p3 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_CatenaryInsulator_Wipeout _eraseWipeoutSource_)
	; Two curved bars ')(' across the contact wire:
	(SetLayer layDef_Zero)
	(DrawArc layDef_Zero p1 p5 p9)
	(DrawArc layDef_Zero p2 p6 p10)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun OCS-SECTION-INSULATOR ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 )
	; Ref. Bane NOR schematic 2D layer/symbol: JBTEH_Komponenter / EH_KL_Seksjonsisolator
	; Two bars forming a "||" with a wipeout between them. 
	; Default insertion direction is "up" in wire / catenary / contact wire alignment.
	;
	;     1 2
	;     5-6
	; ----|.|----
	;     7-8
	;     3 4
	;
	(setq
		blockName	(strcat _OCS_ "SIL-" "ISOLATOR-SEKSJONSISOLATOR")
		description	"KL SEKSJONSISOLATOR"
		x 1.4
		y 4.5
		p1 (list (* -0.5 x) (*  0.500 y))
		p2 (list (*  0.5 x) (*  0.500 y))
		p3 (list (* -0.5 x) (* -0.500 y))
		p4 (list (*  0.5 x) (* -0.500 y))
		p5 (list (* -0.5 x) (*  0.278 y))
		p6 (list (*  0.5 x) (*  0.278 y))
		p7 (list (* -0.5 x) (* -0.278 y))
		p8 (list (*  0.5 x) (* -0.278 y))
	)
	; Wipeout to remove the isolated part of wire under the line insulator (no box!)
	(command _POLYLINE_ p5 p6 p8 p7 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_CatenaryInsulator_Wipeout _eraseWipeoutSource_)
	; Two vertical bars '||' across the contact wire:
	(DrawLine layDef_Zero p1 p3)
	(DrawLine layDef_Zero p2 p4)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun OCS-COVERED-WIRE-INSULATOR ( / blockName x y description p1 p2 p3 p4 p5 p6 p7 p8 )
	; 2020-04-14 Norconsult schematic 2D layer/symbol: not found in Bane NOR TRV - JBTEH_Komponenter / EH_KL_Stavisolator
	; Four horizontal lines inside a lying rectangle, to be inserted in direction "both". 
	; Default insertion direction is "up" in wire / catenary / contact wire alignment.
	;
	;TODO: Ask Bane NOR for permission to use EH-ISS for this one.
	;
	;                 +--------------------------------------+
	;                 1--------------------------------------2
	;                 3--------------------------------------4
	;  ---------------+                  .                   +--------------- Contact wire
	;                 5--------------------------------------6
	;                 7--------------------------------------8
	;                 +--------------------------------------+
	;
	(setq
		blockName	(strcat _OCS_ "ISS-" "ISOLATOR-STAVISOLATOR")
		description "KL STAVISOLATOR"
		x 2.0 		;halfLength 1:500 scale
		y 0.25		;halfHeight of symbol
		p1 (list (* -0.5 x) (*  0.3 y))
		p2 (list (*  0.5 x) (*  0.3 y))
		p3 (list (* -0.5 x) (*  0.1 y))
		p4 (list (*  0.5 x) (*  0.1 y))
		p5 (list (* -0.5 x) (* -0.1 y))
		p6 (list (*  0.5 x) (* -0.1 y))
		p7 (list (* -0.5 x) (* -0.3 y))
		p8 (list (*  0.5 x) (* -0.3 y))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(DrawLine layDef_Zero p7 p8)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
