;=========================================================================================================================
;
; Thumbnail Catenary.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for contact wire and catenary alignment selection

(defun C:THUMBNAIL-CATENARY ( / )
	(ALIGNMENT-KONTAKTLEDNINGSOPPHENG)
)



(defun ALIGNMENT-KONTAKTLEDNINGSOPPHENG ( / 
		blockName 
		w1 w2
		c1 c2 c3 c4 c5 c6 c7 c8 
		s11 s12 s13 s21 s22 s23
		d11 d12 d21 d22 d31 d32 d41 d42 d51 d52 d61 d62
	) 
	;
	; 18 c1                                                         c8
	; 16   \                                                       /
	; 14     \                                                   /
	; 12       c2=s13                                    s21=c7
	; 10 s11-s12     \                                  /      s22-s23
	; 8     =d11       c3--                        --c6       =d61
	; 6      |        =d21 \-----c4--------c5-----/ =d51       |
	; 4      |         |        =d31      =d41       |         |
	; 2      |         |         |         |         |         |
	; 0  ----d12-------d22-------d32-------d42-------d52-------d62---
	;  
	;   0    5 7       15        25        35        45      5355   60
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-THUMBNAIL-KONTAKTLEDNINGSOPPHENG")
		; spanLength 60
		; stitchWireLength 14
		; systemHeight (* 1.8 5)  ; System height is exaggerated 10-fold in Z direction
		; Contact wire, one spanlength:
		w1 _origo_
		w2 (list 60 0)
		; Catenary, one spanlength:
		c1 (list 0 18)
		c2 (list 7 12)
		c3 (list 15 8)
		c4 (list 25 6)
		c5 (list 35 6)
		c6 (list 45 8)
		c7 (list 53 12)
		c8 (list 60 18)
		; Stitch wire, two halves:
		s11 (list 0 10) 
		s12 (list 5 10)
		s13 (list 7 12)
		s21 (list 53 12)
		s22 (list 55 10)
		s23 (list 60 10)
		; Droppers, 6 pcs:
		d11 (list 5 10)
		d12 (list 5 0)
		d21 (list 15 8)
		d22 (list 15 0)
		d31 (list 25 6)
		d32 (list 25 0)
		d41 (list 35 6)
		d42 (list 35 0)
		d51 (list 45 8)
		d52 (list 45 0)
		d61 (list 55 10)
		d62 (list 55 0)
	)
	; Draw one span, then mirror into two full span lengths:
	(command
		_POLYLINE_ w1 w2 _ENTER_
		_POLYLINE_ c1 c2 c3 c4 c5 c6 c7 c8 _ENTER_
		_POLYLINE_ s11 s12 s13 _ENTER_
		_POLYLINE_ s21 s22 s23 _ENTER_
		_POLYLINE_ d11 d12 _ENTER_
		_POLYLINE_ d21 d22 _ENTER_
		_POLYLINE_ d31 d32 _ENTER_
		_POLYLINE_ d41 d42 _ENTER_
		_POLYLINE_ d51 d52 _ENTER_
		_POLYLINE_ d61 d62 _ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	; Reduce to about 10 drawing units overall size (standard for alignment thumbnails, even if they will be auto-scaled in RailCOMPLETE)
	(command _SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
