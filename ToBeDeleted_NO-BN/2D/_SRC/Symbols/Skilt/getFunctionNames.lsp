;
; getFunctionNames.lsp
;
;Function called by GENERERSKILT in order to call individual board generation routines one by one.
;Note: Stated names must confornm with the routine's name, but the name of that Lisp file does not matter.
;Use autocad 'DATAEXTRACTION' in acad existing symbol file to export symbol info,
;then add quotation marks and paste from excel
;NB: No spaces before/after block names.
(defun getFunctionNames	(/ functionNames f)		
	(setq 
		functionNames (list 
			"101-1" "101-2" "101-3" "101-4-NY" "101-4-GAMMEL" "101-7" "101-8" "101-9" "101-10"
			"102-1" "102-2" "102-3"
			"60A" "60B" "60C" "60D" "60D-10" "60D-40" "60E" "60F" "60G" "60H"
			"61A" "61B" "61C" 
			"62A"
			"63A" "63B" 
			"64A" "64B" "64C" "64D" "64E" "64F" 
			"65A" "65B" "65C" "65D" "65E" "65F" "65G-1" "65G-2" "65G-3" 
			"66A"
			"67A" "67B" "67C" "67D" "67E" "68A"
			"68B" "68C" "68D-1" "68D-2" "68D-3" "68F" "68G"
			"69A" "69B" 
			"70A"
			"72A" "72B" "73A"
			"74-1" "74-2" "75A-1"
			"75A-2" "75B" "75C-1" "75C-2" "75D-1" "75D-2"
			"BaliseSkilt"
			"SKILT-TREDJEPERSON-138-MOT-VEI"
			"SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR"
			"ANNOTATIONS-CHAINBREAK"
			"Skilt 1000V togvarmeanlegg"
			"Skilt Anleggsomraade begynner"
			"Skilt Anleggsomraade slutter"
			"Skilt ATC innkoplet"
			"Skilt ATC utkoplet"
			"Skilt Bevegelig kryss"
			"Skilt Kjedebrudd"
			"Skilt Noedtelefon"    
			"Skilt Sidespor"
			"Skilt Spornummer"
			"Skilt StoppSeOgLytt"
			"Skilt Underskilt meterangivelse"
			"Roemningsavstand H"
			"Roemningsavstand V"
			"Roemningsavstand VH"
			"Skilt_Arbeidsomraade_1-2"
			"Skilt_Arbeidsomraade_1-3"
			"Skilt_Arbeidsomraade_2-2"
			"Skilt_Arbeidsomraade_2-3"
			"Skilt_Arbeidsomraade_3"
		)
	)
		;fix strings, substitutes spaces by underscores, remove tabs
	(setq f (mapcar 'removeSpacesnTabs functionNames))
)



(defun drawSquare45 (side1)
					; square
  (setq ang (/ pi 4))			;45 degrees

	(command 
	"._PLINE"
			(list 0 0)
			(list (* side1 (cos ang)) (* side1 (sin ang)))
			(list 0 (* 2 side1 (sin ang)))
			(list (- (* side1 (cos ang))) (* side1 (sin ang)))
		"CLOSE"
	)
)



(defun drawTriangle (side1 / ang blockName)
	(setq ang (D->R 60))
	(command 
	"._PLINE"
		"0,0"
			(list (* side1 (cos ang)) (* side1 (sin ang)))
			(list (- (* side1 (cos ang))) (* side1 (sin ang)))
		"CLOSE"
	)
)



(defun getside () 
	(setq side 2.0)
)



(defun removeSpacesnTabs (f)
	(setq 
		f (vl-string-subst "_" " " f)
		f (vl-string-subst "_" " " f)
		f (vl-string-subst "_" " " f)
		f (vl-string-subst "_" " " f)
		f (vl-string-right-trim "\t" f)
		f (vl-string-left-trim "\t" f)
	)
)