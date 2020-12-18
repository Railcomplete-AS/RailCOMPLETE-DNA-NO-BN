;
; Dvergsignal.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now included in main signal LISP routines.
;
(defun C:SIGNAL-DVERGSIGNAL-FRITTSTAAENDE ( / feste )
	(setq 
		feste nil
	)
	(repeat 2
		(Ds feste)
		(DsMKs feste)
		(Ds66 feste )
		(DsMKs66 feste)
		(setq feste "AAK")
	)
)



(defun Ds ( feste / blockName side poleDs yokePole topOfDs )
	; Note: drawLyingPole() must extend the line under the Ds head, in order for the following BREAK command in oldDrawDs() to work properly.
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS"
		side 4.0 ; Ds arc radius = vertical side = horizontal side
		poleDs 3.0
		yokePole 2.5
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
	)
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawLyingPole poleDs (+ topOfDs yokePole))
			(oldDrawDs)
			(command "._MOVE" "_ALL" "" (list (+ topOfDs yokePole) 0) "0,0")
		)
		(progn
			(drawLyingPole 0 topOfDs) 
			(oldDrawDs)
			(drawLyingHsBase)
		)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun DsMKs ( feste / blockName side poleDs topOfDs )
	(setq 
		blockName "_NO-BN-2D-JBTSI-SIGNAL-43-MKS-DS"
		side 4.0 ; Ds arc radius = vertical side = horizontal side
		poleDs 3.0
		yokePole 2.5
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
	)
	(oldDrawDsMKs)
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawLyingPole topOfDs (+ topOfDs yokePole))
			(command "._MOVE" "_ALL" "" (list (+ topOfDs yokePole) 0) "0,0")
		)
		(progn
			(drawLyingPole 0 poleDs)
			(drawLyingHsBase)
		)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun Ds66 ( feste / blockName pole66 yokePole letterSHeight side poleDs yokePole topOfDs offset )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-TSS-MKS-DS"
		pole66 8.0
		letterSHeight 4.0
		side 4.0 ; Ds arc radius = vertical side = horizontal side
		poleDs 3.0
		yokePole 2.5
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
		offset (* 0.9 (getMediumLanternRadius))
	)
	(drawLyingPole poleDs topOfDs) ; add short pole because oldDrawDs() needs it for the BREAK command
	(oldDrawDs)
	(drawLyingPole topOfDs (+ topOfDs offset)) ; add short pole to make space for the MKs lantern
	(drawLetterS (+ topOfDs offset)) ; add 'S' shape on top of short pole
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawLyingPole (+ topOfDs offset letterSHeight) (+ topOfDs offset letterSHeight yokePole))
			(command "._MOVE" "_ALL" "" (list (+ topOfDs offset letterSHeight yokePole) 0) "0,0")
		)
		(progn
			(drawLyingPole 0 poleDs)
			(drawLyingHsBase)
		)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun DsMKs66 ( feste / blockName pole66 letterSHeight side poleDs yokePole topOfDs offset )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS-MKS-66"
		pole66 8.0
		letterSHeight 4.0
		side 4.0 ; Ds arc radius = vertical side = horizontal side
		poleDs 3.0
		yokePole 2.5
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
		offset (* 0.9 (getMediumLanternRadius))
	)
	(oldDrawDsMKs)
	(drawLyingPole topOfDs (+ topOfDs offset)) ; add short pole to make space for the MKs lantern
	(drawLetterS (+ topOfDs offset)) ; add 'S' shape on top of short pole
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawLyingPole (+ topOfDs offset letterSHeight) (+ topOfDs offset letterSHeight yokePole))
			(command "._MOVE" "_ALL" "" (list (+ topOfDs offset letterSHeight yokePole) 0) "0,0")
		)
		(progn
			(drawLyingPole 0 poleDs)
			(drawLyingHsBase)
		)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)
