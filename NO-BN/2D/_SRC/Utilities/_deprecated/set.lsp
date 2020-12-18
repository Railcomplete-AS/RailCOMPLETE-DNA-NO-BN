;
; 2020-07-26 CLFEY: Moved to folder Utilities\_Deprecated because no-one uses these anymore.
;
(defun varget (lis newVar) ;(setq lis '("HIGHLIGHT" "BLIPMODE" "CMDECHO" "BLIPMODE" "OSMODE"))(setq newVar '(0 0 0 0 517))
	(setq oldVar (mapcar 'getvar lis))
	(setq no 0)

	(repeat (length lis)
		(setvar (nth no lis) (nth no newVar))
		(setq no (1+ no))
	);repeat
  oldVar
);defun
 
(defun varset (lis oldVar)
	(setq no 0)
	(repeat (length lis)
		(setvar (nth no lis) (nth no oldVar))
		(setq no (1+ no))
	);repeat
  (princ)
);defun