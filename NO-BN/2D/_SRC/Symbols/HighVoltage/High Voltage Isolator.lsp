;=========================================================================================================================
;
; High Voltage Isolator.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Isolator (in contact wire, catenary, fix lines, and other lines)

(defun C:HIGH-VOLTAGE-ISOLATOR ( / )
	
	(LINEISOLATOR)
  	(SEKSJONSISOLATOR)
	(STAVISOLATOR)
)



(defun LINEISOLATOR ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 )
	;2020-04-07 Bane NOR schematic 2D layer/symbol: JBTEH_Komponenter / EH_KL_Isolator
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
		blockName "NO-BN-2D-JBTKL-ISOLATOR-LINEISOLATOR"
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
	; Two curved bars ')(' across the contact wire:
	(setlayer layer_Zero)
	(command "._ARC" p1 p5 p9)
	(command "._ARC" p2 p6 p10)
	; Wipeout between the two curved bars to remove the insulated part of wire under the line insulator:
	(setLayer layer_CatenaryIsolator_Wipeout)
	(command "._PLINE" p3 p4 p6 p8 p7 p5 p3 _closed_)
	(command "._WIPEOUT" "_POLYLINE" "_LAST" "" "_YES") ; delete source shape after use
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SEKSJONSISOLATOR ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 )
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
		blockName "NO-BN-2D-JBTKL-ISOLATOR-SEKSJONSISOLATOR"
		description "KL SEKSJONSISOLATOR"
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
	; Two vertical bars '||' across the contact wire:
	(drawLine layer_Zero p1 p3)
	(drawLine layer_Zero p2 p4)
	; Wipeout to remove the insulated part of wire under the line insulator (no box!)
	(setLayer layer_CatenaryIsolator_Wipeout)
	(command "._PLINE" p5 p6 p8 p7 _closed_)
	(command "._WIPEOUT" "_POLYLINE" "_LAST" "" "_YES") ; delete source shape after use
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun STAVISOLATOR ( / blockName x y description p1 p2 p3 p4 p5 p6 p7 p8 )
	; 2020-04-14 Norconsult schematic 2D layer/symbol: not found in Bane NOR TRV - JBTEH_Komponenter / EH_KL_Stavisolator
	; Four horizontal lines inside a lying rectangle, to be inserted in direction "both". 
	; Default insertion direction is "up" in wire / catenary / contact wire alignment.
	;
	;
	; TL------------------------------------TR
	; 1--------------------------------------2
	; 3--------------------------------------4
	; 5--------------------------------------6
	; 7--------------------------------------8
	; BL------------------------------------BR
	;
	(setq
		blockName "NO-BN-2D-JBTKL-ISOLATOR-STAVISOLATOR"
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
	(drawBox layer_Zero x y _noWipeout_)
	(drawLine layer_Zero p1 p2)
	(drawLine layer_Zero p3 p4)
	(drawLine layer_Zero p5 p6)
	(drawLine layer_Zero p7 p8)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
