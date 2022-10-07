;=========================================================================================================================
;
; Thumbnail Manhole.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for escape route alignment selection (from tunnels, e.g. side-tunnels where distances shall be measured and depicted)

(defun THUMBNAIL-MANHOLE ( / )
	(THUMBNAIL-MANHOLE-RECTANGULAR)
	(THUMBNAIL-MANHOLE-CIRCULAR)
)



(defun THUMBNAIL-MANHOLE-CIRCULAR ( / blockName p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 dCylinder dCover )
	; A cube drawn in -7:42 "poor man's perspective"
	; The X-axis is tilted down by 7 degrees, unit = .99 East and -.12 North (cos-7, sin-7).
	; The Y-axis (pointing backwards) is tilted up by 42 degrees, unit = .37 East and .33 North (cos42,sin42).
	; The Z-axis points right up, unit = .00 East and 1.00 North (cos90,sin90).
	;
	;   ______
	;  /   ( )\			p13-p14 and p15-p16 circular cover (elliptical in perspective)
	; 10  11  12		Ellipse as top p11,p12
	; |\______/|
	; |        |
	; |        |
	; 1        9
	;  \2345678/		Four arcs
	;
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ thumbnailInfix "-MANHOLE-ROUND"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ thumbnailInfix "-TREKKEKUM-RUND"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ thumbnailInfix "-REGARD-CIRCULAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ thumbnailInfix "-SCHACHT-RUND"		)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ thumbnailInfix "-MANHOLE-ROUND"		)))
	)
	(setq
		p1  '(  0    0.0)
		p2  '(  6  -10.0)
		p3  '( 10  -12.5)
		p4  '( 30  -19.3)
		p5  '( 50  -21.0)
		p6  '( 70  -19.3)
		p7  '( 90  -12.5)
		p8  '( 94  -10.0)
		p9  '(100    0.0)
		p10 '(  0  100)
		p11 '( 50  100)
		p12 '(100  100)
		p13 '( 65  110)
		p14 '( 90  110)
		p15 '( 65  115)
		p16 '( 90  115)
		dCylinder	23
		dCover		8
	)
	(command 
		_ARC_ p1 p2 p3
		_ARC_ p3 p4 p5
		_ARC_ p5 p6 p7
		_ARC_ p7 p8 p9
		_LINE_ p1 p10 _ENTER_
		_LINE_ p9 p12 _ENTER_
		_ELLIPSE_ _setEllipseCenter_ p11 p12 dCylinder
		_ELLIPSE_ _setEllipseCenter_ p13 p14 dCover
		_ELLIPSE_ _setEllipseCenter_ p15 p16 dCover
	)
	(MoveLeft 55)
	(MoveDown 55)
	(ScaleAll 0.04) ; to 1 x 1 x 1 meter approx in 1:250 scale (4x4x4 m3 in 1:1000 or schematic)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun THUMBNAIL-MANHOLE-RECTANGULAR ( / blockName p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 dCover )
	; A cube drawn in -7:42 "poor man's perspective"
	; The X-axis is tilted down by 7 degrees, unit = .99 East and -.12 North (cos-7, sin-7).
	; The Y-axis (pointing backwards) is tilted up by 42 degrees, unit = .37 East and .33 North (cos42,sin42).
	; The Z-axis points right up, unit = .00 East and 1.00 North (cos90,sin90).
	;
	;   7--------6
	;  /    (89)/|		p8-p11 = circular cover (elliptical in perspective)
	; 3--------4 |		
	; |        | |
	; |        | 5 
	; |        |/
	; 1--------2
	;
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ thumbnailInfix "-MANHOLE-RECTANGULAR"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ thumbnailInfix "-TREKKEKUM-REKTANGULAER"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ thumbnailInfix "-REGARD-RECTANGULAIRE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ thumbnailInfix "-SCHACHT-REKTANGULAER"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ thumbnailInfix "-MANHOLE-RECTANGULAR"	)))
	)
	(setq
		p1  '(  0    0)
		p2  '( 99  -12)
		p5  '(136   21)
		p3  '(  0  100)
		p4  '( 99   88)
		p6  '(136  121)
		p7  '( 37  133)
		p8  '( 90  111)
		p9  '( 115 111)
		p10 '( 90  116)
		p11 '(115  116)
		dCover	8
	)
	(command 
		_POLYLINE_ p1 p3 p4 p2 _closedPolyline_
		_POLYLINE_ p3 p7 p6 p5 p2 _openPolyline_
		_LINE_ p4 p6 _ENTER_
		_ARC_setEllipseCenter_ p8 p9 dCover
		_ELLIPSE_ _setEllipseCenter_ p8 p9 dCover
		_ELLIPSE_ _setEllipseCenter_ p10 p11 dCover
	)
	(MoveLeft 65)
	(MoveDown 65)
	(ScaleAll 0.04) ; to 1 x 1 x 1 meter approx in 1:250 scale (4x4x4 m3 in 1:1000 or schematic)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
