;
; SymbolLibraryHelpers.lsp
;
; Supporting Lisp routines, needed for generation of 2D symbol libraries.
;
(defun AddSupportPath (dir / tmp Cpath)
	(vl-load-com)
	(setq
		Cpath (getenv "ACAD") tmp 
		(strcat ";" dir ";")
	)
	(if (not (vl-string-search dir cpath)) (setenv "ACAD" (strcat Cpath ";" dir)))
	dir
)



(defun purgeAll()
	(command 
		"._LAYER" "Unlock" "*" ""
		"._ERASE" "ALL" ""
		"._LAYER" "SET" "0" ""
		"._PURGE" "All" "*" "No"
		"._PURGE" "All" "*" "No"
		"._PURGE" "All" "*" "No"
	)
	'purgeAll
)



(defun loadFolder (folderName)
	(if (findfile folderName)
		(mapcar 
			'(lambda (x)
				(if (wcmatch (strcase x) "*.LSP")
					(load (strcat (findfile folderName) "\\" x))
				)
			)
			(vl-directory-files (findfile folderName))
		)
		(print "NO .LSP FILES LOADED")
	)
	'loadFolder
)



(defun setCadSystemDefaults ()
	;(if (findfile "setCadSystemDefaults.scr")
    ;	(command "_script" "setCadSystemDefaults.scr")
    ;	(princ "No setCadSystemDefaults.scr file")
	;)
	(command
		LUNITS 2 ; Linear Units - 2=Decimal (xxx.yyyyyyy)
		LUPREC 3 ; Linear Unit Precision - 3 digits after comma
		AUNITS 0 ; Angular Units - 0=Decimal Degrees
		AUPREC 3 ; Angular Unit Precision - 3 digits after comma
		INSUNITS 6 ; Insertion unit - 6=meters
		LIGHTINGUNITS 2 ; 2=International
		DIMZIN 8 ; DIMZIN: Integer Saved in: Drawing Initial value: 0 (imperial) or 8 (metric) 
		; Values 0-3 affect feet-and-inch dimensions only: Value Description 0 
		; Suppresses zero feet and precisely zero inches 1 Includes zero feet and precisely zero inches 2 Includes zero feet and suppresses zero inches 3 Includes 
		; zero inches and suppresses zero feet 4 Suppresses leading zeros in decimal dimensions (for example, 0.5000 becomes .5000) 8 Suppresses trailing zeros in 
		; decimal dimensions (for example, 12.5000 becomes 12.5) 12 Suppresses both leading and trailing zeros (for example, 0.5000 becomes .5) DIMZIN also affects 
		; real-to-string conversions performed by the AutoLISP rtos and angtos functions.
	)
	(command
		"ERASE ALL" "" ""
		"SNAP" 1.0
		"OSNAP" "OFF"
		"OSNAPCOORD" 1
		"SNAPMODE" 0
		"GRID" "ON"
		"GRID" 0.1
		"UNITS" 2 4 1 4 0 "N"
		"INSUNITS" 6
		"AUNITS" 0
		"OSMODE" 0
		"COORDS" 1
		"PICKBOX" 5
		"DYNPICOORDS" 1
		"DYNPIFORMAT" 1
		"ORTHOMODE" 0
		"PICKFIRST" 1
		"PICKADD" 0
		"ATTREQ" 0
		"ATTDIA" 0
		"FILEDIA" 1
		"ZOOM" "-11,-5" "11,5"
	)
	'setCadSystemDefaults
)



(defun createIsoTextStyle ()
;
;   'Iso3098'will result in an error if not present in the appropriate AutoCAD folder.
;	Place preferred font file in the GitHub RailCOMPLETE folder "Symbol Library\Fonts".
;
	(command "._STYLE" "ISO" "iso3098" "Annotative" "No" "0" "1" "0" "No" "No" )
	'createIsoTextStyle
)



(defun setDwgPropertiesToByBlock ()
	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "byblock"
		"._LINETYPE" "Set" "byblock" ""
		"._LWEIGHT" "byblock"
		"._CETRANSPARENCY" "byblock"
	)
	'setDwgPropertiesToByBlock
)
  
  
  
(defun setPropertiesToByBlockForAllBlocks (/ doc)
	(vlax-for block 
		(vla-get-blocks 
			(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		)
		(vlax-for x block
			(progn
				(if (= (vla-get-layer x) "0")
					(vla-put-color x 0)
				)
				(vla-put-linetype x "ByBlock")
				(vla-put-lineweight x -2)
				(vla-put-entitytransparency x "ByBlock")
			)
		)
	)
	(vla-regen doc acActiveViewport)
	'setPropertiesToByBlockForAllBlocks
)



(defun drawHatch (scale)
	(command 
		"._-HATCH" "S" "L" "" "Properties" "ANSI31" scale "0" "ORIGIN" "SET" "0.0,0.01" "N" ""
	   "._EXPLODE" "L" ""
	)
	'drawHatch
)



(defun drawHatchOptions (scale ang offset style selection)
	(command 
		"._-HATCH" "S" selection "" "Properties" style scale ang "ORIGIN" "SET" (strcat "0.0," (rtos offset)) "N" ""
	   "._EXPLODE" "L" ""
	)
	'drawHatchOptions
)



(defun drawHatchOptionsColor (scale ang offset style selection color)
	(command
		"._-HATCH" "S" selection "" "color" color "" "Properties" style scale ang "ORIGIN" "SET" (strcat "0.0," (rtos offset)) "N" ""
		"._EXPLODE" "L" ""
	)
	'drawHatchOptionsColor
)



(defun drawHatchSelectPoint (scale pt ang offset)
	(command 
		"._-HATCH" pt "Properties" "ANSI31" scale ang "ORIGIN" "SET" (strcat "0.0," (rtos offset)) "N" ""
	   "._EXPLODE" "L" ""
	)
	'drawHatchSelectPoint
)



(defun drawHatchOptionsSelectPoint (scale pt ang offset style)
	(command
		"._-HATCH" pt "Properties" style scale ang "ORIGIN" "SET" (strcat "0.0," (rtos offset)) "N" ""
	   "._EXPLODE" "L" ""
	)
	'drawHatchOptionsSelectPoint
)



(defun newBlock (blockName) ;create block from present drawing
	; in the case when the blocks are nested
	(if (tblsearch "BLOCK" blockName)
		 ;If block exists already (such as 'NO-BN-2D-JBTSI-FILLED-nn' for switches / signaling symbols) which is generated for several switch types)
			;Redefine block definition:
			(command "._BLOCK" blockName "YES" "0,0" "ALL" "")
		; else just create first-time block:
			(command "._BLOCK" blockName 	   "0,0" "ALL" "") 
	)
	(setDwgPropertiesToByBlock)
	blockName
)



(defun newBlockPreSetLayer (blockName) ;create block from present drawing
	; in the case when the blocks are nested
	(if (tblsearch "BLOCK" blockName)
		(command "._BLOCK" blockName "YES" "0,0" "ALL" "") ;Yes to redefine block definition
        (command "._BLOCK" blockName 	   "0,0" "ALL" "")
	)
	'newBlockPreSetLayer
)



(defun newLayer (layerName color description)
	(command 
		"._LAYER" 
			"New" layerName 
			"Color" color layerName 
			"Description" description layerName 
			""
	)
	(command)  ; CLFEY: Why this line?
	'newLayer
)



(defun addAtt (tag prom val pos height rot textStyle justify aflags)
	(setq var (getvar 'AFLAGS))
	(setvar "AFLAGS" aflags)
	(createIsoTextStyle)
	(if (or (= 48 aflags) (= 32 aflags))
		(command 
			"._ATTDEF" "" tag prom val "" pos
				"Justify" justify
				"Style" textStyle
				"H" height
				"Rotation" rot
				"Width" 0
		)
		(if (= prom nil)
			(command "._ATTDEF" "" tag val "STYLE" textStyle "Justify" justify pos height rot)
			(command "._ATTDEF" "" tag prom val "STYLE" textStyle "Justify" justify pos height rot)
		)
    )
	(setvar "AFLAGS" var)
	'addAtt
)



(defun addText (text pos height rot textStyle justify)
	(createIsoTextStyle)
	(command "._TEXT" "STYLE" textStyle "JUSTIFY" justify pos height rot text)
	'addText
)



(defun addMText (text pos height width rot textStyle justify)
	(createIsoTextStyle)
	(command "._MTEXT" pos "S" textStyle  "H" height "J" justify "R" rot  "W" width text "")
   'addMText
)


; mirrorSelection()
; Mirror an object residing in the first quadrant (from 0 to 90 decimal degrees, 0 DD = East and 90 DD = North).
; quadrant = number 1..4, results in mirroring operations resulting from mirroring about X-axis (0 DD) and / or Y-axis (90 DD).
; 'selection' is typically "A" or ("All")
; 1 = no action
; 2 = mirror about the Y-axis
; 3 = mirror about the Y- then the X-axis (or X-axis then Y-axis)
; 4 = mirror about the X-axis
(defun mirrorSelection (quadrant selection / x) 
	(cond
		((= quadrant 1)
			(setq x +1)
			; No action
		)
		((= quadrant 2)
			(command "._MIRROR" selection "" "0,0" "0,1" "Y")
			(setq x -1)
		)
		((= quadrant 3)
			(command "._MIRROR" selection "" "0,0" "0,1" "Y")
			(command "._MIRROR" selection "" "0,0" "1,0" "Y")
			(setq x -1)
		)
		((= quadrant 4)
			(command "._MIRROR" selection "" "0,0" "1,0" "Y")
			(setq x +1)
		)
	)
	'mirrorSelection
)



