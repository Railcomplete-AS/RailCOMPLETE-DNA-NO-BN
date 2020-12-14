;
; Main.lsp
;
; Main Lisp file for generating Bane NOR 2D symbol library for use with RailCOMPLETE.
; This fils will be called from a script, "GenerateSymbolLibrary.scr" run under AutoCAD from a batch file.
; See scripts and batchfiles under Github\RailCOMPLETE\Customization\_TOOLBOX...
;
; Copyright RailCOMPLETE (r) 2016-2020. All rights reserved.
;
(vl-load-com)

(defun C:MAIN ()
	(command 
		"._LAYER"
   	    "Color" "white" "0"
		"New" "Defpoints"
		"Color" "white" "Defpoints"
		"Plot" "N" "Defpoints" ""
	)
	(purgeAll)
	(setCadSystemDefaults)
	(createIsoTextStyle)
	(setDwgPropertiesToByBlock)
	(INSERTION-POINT)
	(C:GENERATE-COMMON-OBJECTS)
	(C:GENERATE-BOARDS-AND-POLES-OBJECTS)
	(C:GENERATE-CIVIL-WORKS-OBJECTS)
	(C:GENERATE-TRACK-AND-EMBANKMENT-OBJECTS)
	(C:GENERATE-HIGH-VOLTAGE-OBJECTS)
	(C:GENERATE-SIGNALING-OBJECTS)
	(C:GENERATE-TELECOM-OBJECTS)
	(C:GENERATE-LOW-VOLTAGE-OBJECTS)
	(C:GENERATE-THUMBNAILS) ; Thumbnail icons when creating objects which are not point objects (area, alignment, table etc)
	;;;;;;;;;;;;;; (C:GENERATE-TABELL) ; Only for internal use - produce table showing all available 2D symbols.
	 *** The file name "2D.dwg" is expected by other batch files taking care of the results:
	(SAVE-RESULT "2D.dwg")
)



(defun SAVE-RESULT (adm / fname)
	(setq fname (strcat (getvar "dwgprefix") adm))
	(cond 
		((= (findfile fname) nil)
			; File does not exist
			(command ".SAVE" fname)
		)
		(T 
			; File exists already
			(command ".SAVE" fname "YES")
		)
	)
)


(defun INSERTION-POINT (/ radius blockName)
	(setq 
		radius 0.05
		blockName "NO-BN-2D-INSERTION-POINT" ;2D symbol block insertion point
	)
	(newLayer "JBTFE__INNSETTINGSPUNKT" 62 "Tverrfaglig - objektets eget innsettingspunkt")
	(command 
		"._LAYER" "SET" "JBTFE__INNSETTINGSPUNKT" ""
		"._COLOR" "ByLayer"
		"._CIRCLE" "0,0" radius
		"._LINE" (list (- radius) 0) (list radius 0) ""
		"._LINE" (list 0 (- radius)) (list 0 radius) ""
	)
	(newBlock blockName)
	blockName
)
