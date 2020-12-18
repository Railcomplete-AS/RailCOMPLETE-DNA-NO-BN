;
; Mast.lsp
;
; 2020-01-20 CLFEY/JOSJO: Fjernet "-EKSISTERENDE" symboler, de skal iht TRV ikke vises. Bruk "...-NY" i stedet.
;
(defun C:ORDINAER-MAST (/)
	(BJELKEMAST)
	(GMBMAST)
	(GITTERMAST-H)
	(GITTERMAST-B)
	(BETONGMAST)
	(TREMAST)
)



(defun BJELKEMAST (/ blockName x y w1 w2 rad)
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BJELKEMAST"
		x (/ 3.0 2) 	;halvbredde symbol
		y (/ 3.0 2) 	;halvhøyde symbol
		w1 (/ 0.18 2) 	;halvbredde "livet"
		w2 0.20 		;hel tykkelse "tverr-beina"
		rad 0.15
	)
	(command
		"._PLINE"
			(list w1 0) 
			(list w1 (- y (+ w2 rad)))
			"A" "CE"
			(list center_x center_y) (list (+ w1 rad) (- y w2))
			"L" 
			(list x (- y w2))
			(list x y)
			(list 0 y)
			""
		"._MIRROR" "LAST" "" (list 0 0) (list 0 1) "N"
		"._MIRROR" "ALL" "" (list 0 0) (list 1 0) "N"
	)
	(drawHatchSelectPoint 0.03 (list 0 rad) (/ 3.14 4) 0)
	(newBlock blockName)
	blockName
)



(defun GMBMAST (/ blockName x y rad center_x center_y)
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GMBMAST"
		x 0.85 ;bredde
		y 3.00 ;høyde
		rad 1.713
		center_x 1.713
		center_y 1.520
	)
	(command
		"._PLINE"
			(list 0 (/ y 2)) (list 0 0) (list x 0) (list x y) 
			"A" "CE" 
			(list center_x center_y) (list 0 (/ y 2))
			""
	)
	(command "._MOVE" "ALL" "" (list (/ x 2) (/ y 2)) (list 0 0))
	(newBlock blockName)
	blockName
)



(defun GITTERMAST-H (/ blockName x y)
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-H"
		x (/ 3.0 2) ;halvbredde
		y (/ 3.0 2) ;halvhøyde
	)
	(command
		"._RECTANGLE" (list (- x) (- y)) (list x y)
	)
	(newBlock blockName)
	blockName
)



(defun GITTERMAST-B (/ blockName x y)
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-B"
		x (/ 1.05 2) ;halvbredde
		y (/ 3.0 2)  ;halvhøyde
	)
	(command
		"._RECTANGLE" (list (- x) (- y)) (list x y)
	)
	(newBlock blockName)
	blockName
)



(defun BETONGMAST (/ blockName x y)
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BETONGMAST"
		x 3
		y 3
	)
	(command
		"._RECTANGLE" (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2))
	)
	(drawHatch 0.13) 
	(newBlock blockName)
	blockName
)



(defun TREMAST (/ blockName r)
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-TREMAST"
		r	1.5
	)
	(command
		"._CIRCLE" (list 0 0) r
	)
	(newBlock blockName)
	blockName
)





