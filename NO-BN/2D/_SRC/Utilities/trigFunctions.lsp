;=========================================================================================================================
;
; trigFunctions.lsp
;
; Adapted from CADalyst tip 442, by John Howard
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
;
;COS
(defun DDcos ( x / ) (cos (D->R x)))

;SIN
(defun DDsin ( x / ) (sin (D->R x)))

;CO-TANGENT
(defun cot (x)
	(cond
		((equal (sin x) 0.0 1.0e-16)
			(if (minusp x)
				-1.0e200
				1.0e200
			)
		)
		(T
			(/ (cos x) (sin x))
		)
	)
)
(defun DDcot ( x / ) (cot (D->R x)))

;CO-SECANT
(defun csc (x)
	(cond
		((equal (sin x) 0.0 1.0e-16)
			(if (minusp x)
				-1.0e200
				1.0e200
			)
		)
		(T
			(/ 1.0 (sin x))
		)
	)
)
(defun DDcsc ( x / ) (csc (D->R x)))

;SECANT
(defun sec (x)
	(cond
		((equal (sin x) 0.0 1.0e-16)
			(if (minusp x)
				-1.0e200
				1.0e200
			)
		)
		(T
			(/ 1.0 (cos x))
		)
	)
)
(defun DDsec ( x / ) (sec (D->R x)))

;TANGENT
(defun tan (x)
	(cond
		((equal (cos x) 0.0 1.0e-16)
			(if (minusp x)
				-1.0e200
				1.0e200
			)
		)
		(T
			(/ (sin x) (cos x))
		)
	)
)
(defun DDtan ( x / ) (tan (D->R x)))

;ARC COSECANT
(defun acsc (x)
	(cond
		((equal x 1.0 1.0e-16)
			(* pi 0.5)
		)
		((equal x -1.0 1.0e-16)
			(* pi -0.5)
		)
		((> (abs x) 1.0)
			(atan (/ (/ 1.0 x) (sqrt (- 1.0 (/ 1.0 (* x x))))))
		)
		(T
			(prompt "\n*ERROR* (abs x) < 1.0 from ACSC function\n")
		)
	)
)
(defun DDacsc ( x / ) (acsc (D->R x)))

;ARC COSINE
(defun acos (x)
	(cond
		((equal x 1.0 1.0e-16)
			0.0
		)
		((equal x -1.0 1.0e-16)
			pi
		)
		((< (abs x) 1.0)
			(- (* pi 0.5) (atan (/ x (sqrt (- 1.0 (* x x))))))
		)
		(T
			(prompt "\n*ERROR* (abs x) > 1.0 from ACOS function\n")
		)
	)
)
(defun DDacos ( x / ) (acos (D->R x)))

;ARC SECANT
(defun asec (x)
	(cond
		((equal x 1.0 1.0e-16)
			0.0
		)
		((equal x -1.0 1.0e-16)
			pi
		)
		((> (abs x) 1.0)
			(- (* pi 0.5) (atan (/ (/ 1.0 x) (sqrt (- 1.0 (/ 1.0 (* x x)))))))
		)
		(T
			(prompt "\n*ERROR* (abs x) < 1.0 from ASEC function\n")
		)
	)
)
(defun DDasec ( x / ) (asec (D->R x)))

;ARC SINE
(defun asin (x)
	(cond
		((equal x 1.0 1.0e-16)
			(* pi 0.5)
		)
		((equal x -1.0 1.0e-16)
			(* pi -0.5)
		)
		((< (abs x) 1.0)
			(atan (/ x (sqrt (- 1.0 (* x x)))))
		)
		(T
			(prompt "\n*ERROR* (abs x) > 1.0 from ASIN function\n")
		)
	)
)
(defun DDasin ( x / ) (asin (D->R x)))
