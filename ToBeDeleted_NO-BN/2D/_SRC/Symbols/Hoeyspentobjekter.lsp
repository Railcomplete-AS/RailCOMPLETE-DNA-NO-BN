;
; Hoeyspentobjekter.lsp
;
(loadFolder (findfile "Hoeyspent")) ; ! Cannot use special national characters in filename in a findfile LISP command!

(defun C:GENERATE-HIGH-VOLTAGE-OBJECTS (/)
  (setCadSystemDefaults)
  (C:AAK)
  (C:AVSPENNING)
  (C:BRYTER)
  (C:FORBINDELSE)
  (C:ORDINAER-MAST)
  (C:HENGEMAST) ; I tunnel og i åk
  (C:ISOLATOR)
  (C:JORDING)
  (C:KRAFTAVLASTING)
  (C:TRANSFORMATOR)
  (C:UTLIGGER)
  (C:VERNESKJERM)
)
