;Trig functions in AutoLISP
;adapted from CADalyst tip 442, by John Howard
;
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
;
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
;
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
;
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
;
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
;
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
;
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
;
(princ "\nCOT, CSC, SEC, TAN, ACSC, ACOS, ASEC, ASIN \n")
(prin1)