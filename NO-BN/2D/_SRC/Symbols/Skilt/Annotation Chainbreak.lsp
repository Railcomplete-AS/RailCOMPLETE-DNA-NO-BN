;
; Annotation Chainbreak.lsp
;
(defun ANNOTATIONS-CHAINBREAK (/ blockName description)
	(setq 
		blockName "NO-BN-2D-JBTSI-ANNOTATIONS-CHAINBREAK"
		description "Annotation Kjedebrudd"
	)
	(command
		"._PLINE" "-11,0" "9,0" "11,4" "-9,4" "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	  )
	(command "._LINE" "-10.876,0.248" "9.124,0.248" "")
	(command "._LINE" "-10.752,0.4961" "9.248,0.4961" "")
	(command "._LINE" "-9.248,3.5039" "10.752,3.5039" "")
	(command "._LINE" "-9.124,3.752" "10.876,3.752" "") 
	(addAtt "FRA_KM" "Fra km:" "15" "-1.0,-2.75" 1.8 0 "iso" "Right" 16)
	(addAtt "FRA_M" "Fra meter:" "422" "-1.0,-5.50" 1.8 0 "iso" "Right" 16)
	(addAtt "TIL_KM" "Til km:" "15" "1.0,-2.75" 1.8 0 "iso" "Left" 16)
	(addAtt "TIL_M" "Til meter:" "450" "1.0,-5.50" 1.8 0 "iso" "Left" 16)
	(addAtt "SPRANG" "Sprang:" "+28m" "0,5.5" 1.8 0 "iso" "MC" 16)
	(addText "KJEDEBRUDD" "0,2" 1.8 0 "iso" "MC")
	(command "._ROTATE" "ALL" "" "0,0" "-90")
	(command "._MOVE" "ALL" "" "Displacement" "23,0")
	(drawPole 0 23)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	(newBlock blockName)
	description
)