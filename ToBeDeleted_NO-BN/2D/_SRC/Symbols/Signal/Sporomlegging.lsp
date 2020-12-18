;
; Sporomlegging.lsp
;
(defun C:SPOROMLEGGING ()
	(SPORVEKSELDRIVMASKIN)
	(SPORSPERREDRIVMASKIN)
	(LOKALSTILLER)
)



(defun LOKALSTILLER ( / blockName rad1 rad2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-SIKRINGSANLEGG-LOKALSTILLER"
		rad1 1.0 rad2 0.5
	)
	(command
		"._CIRCLE" "0,0" rad1
		"._CIRCLE" "0,0" rad2
	)
	(drawHatchSelectPoint "0.05" "0,0" 0 0.12)
	(newBlock blockName)
)



(defun SPORVEKSELDRIVMASKIN ( / blockName rad1 rad2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORVEKSEL"
		rad1 1.0 rad2 0.5
	)
	(command
		"._CIRCLE" "0,0" rad1
		"._CIRCLE" "0,0" rad2
	)
	(newBlock blockName)
)



(defun SPORSPERREDRIVMASKIN ( / blockName rad1 rad2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORSPERRE"
		rad1 1.0 rad2 0.5
	)
	(command
		"._CIRCLE" "0,0" rad1
		"._CIRCLE" "0,0" rad2
	)
	(newBlock blockName)
)


