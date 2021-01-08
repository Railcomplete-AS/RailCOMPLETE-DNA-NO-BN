;=========================================================================================================================
;
; Earthing.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Earthing (general symbol, earthing busbar)

(defun C:EARTHING ( / )
	(JORDPOTENSIAL)
	(JORDINGSSKINNE)
)



(defun JORDPOTENSIAL ( / blockName )
	; Generelt jordsymbol - kan benyttes som jordpotensial-markør, eller som kråkefotsymbol osv.
	; Ref. TRV KL skjemasymboler.
	;
	;      .
	;      |
	;      |
	; 1----0----2
	;   3-----4
	;     5,6
	;     (7)
	
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDPOTENSIAL"
		p0 (list  0.0 -3.0)
		p1 (list -2.1 -3.0)
		p2 (list  2.1 -3.0)
		p3 (list -1.4 -3.7)
		p4 (list  1.4 -3.7)
		p5 (list -0.5 -4.4)
		p6 (list  0.5 -4.4)
		p7 (list  0.0 -5.0)
	)
	(drawLine layer_Zero _origo_ p0)
	(drawLine layer_Zero p1 p2)
	(drawLine layer_Zero p3 p4)
	(drawLine layer_Zero p5 p6)
	(addTextAtPos layer_Description _descriptionTextHeight_ p7 "Jordpotensial")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName) ; 100% size is 1:500 scale
)


(defun JORDINGSSKINNE ( / blockName x y circDist r )
	; For montasje under datagulv i bygninger, på vegg, i trekkekum med mer. 
	; TODO - lage flere varianter, modeller faktisk utseende med flerer gjengede hull for skruforbindere / kabelsko.
	;
	;  TL------------------------------TR
	;  |  (1)   (2)   (.)   (3)   (4)   |  
	;  BL------------------------------BR
	;
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDINGSSKINNE"
		x 6.0
		y 2.0
		d 1.0
		r 0.35
		p1 (list (* -2 d) 0)
		p2 (list (* -1 d) 0)
		p3 (list (*  1 d) 0)
		p4 (list (*  2 d) 0)
	)
	(drawBox layer_Zero x y _noWipeout_)
	(drawCircleAtPos layer_Zero r p1 _noWipeout_)
	(drawCircleAtPos layer_Zero r p2 _noWipeout_)
	(drawCircleAtPos layer_Zero r _origo_ _noWipeout_)
	(drawCircleAtPos layer_Zero r p3 _noWipeout_)
	(drawCircleAtPos layer_Zero r p4 _noWipeout_)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) "Jordingsskinne")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _quarter_ blockName)
)
