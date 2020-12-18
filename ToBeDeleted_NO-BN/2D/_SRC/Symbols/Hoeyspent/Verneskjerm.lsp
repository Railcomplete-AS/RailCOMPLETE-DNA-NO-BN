;
; Verneskjerm.lsp
;
(defun C:VERNESKJERM (/)
	(VERNESKJERM)
)


; TODO CLFEY 2019-08-19 IKKE FERDIG ENNÅ - 
; "NO-BN-2D-JBTKL-JORDING-VERNESKJERM-1" står på den ene siden og "NO-BN-2D-JBTKL-JORDING-VERNESKJERM-2" på 
; den andre siden av en KL-mast (to verneskjermer benyttes for å dekke en utliggerbrakett med en eller flere utliggere på)
; Se Elkraftportalen, EH.70338 Beskyttelsesskjerm
(defun VERNESKJERM (/ blockName x y)
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-VERNESKJERM"
		x 0.900 ; Verneskjerm utvendig bredde (høyden er 1400)
		y 0.032 ; Rørdimensjon
		tube 0.032 ; Rør-ramme av rundstål Ø32
		earSide 0.065 ; 10mm flattstål 0.065 x 0.065 side-sveiset til rundstålet i senter stål høyde oppe og nede (6 ører totalt)
		earHole 0.022
		earDisplacement (+ (/ tube 2) 0.050)
		ear1 0.076
		ear2 (+ ear1 0.210)
		ear3 (+ ear2 0.240)
	)
	(command 
		"._RECT" (list (/ x -1) 0) (list 0 (- y)) ""
	)
	(strcat blockName "-1")
	(newBlock blockName)

	(command 
		"._RECT" (list 0 0) (list (/ x 1) (- y)) ""
	)
	(strcat blockName "-2")
	(newBlock blockName)
)

