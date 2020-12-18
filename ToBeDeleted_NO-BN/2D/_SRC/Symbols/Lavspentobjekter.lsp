;
; Lavspentobjekter.lsp
;
(loadFolder (findfile "Lavspent"))

(defun C:GENERATE-LOW-VOLTAGE-OBJECTS ( / )
  (setCadSystemDefaults)
  (C:ARMATUR)
  (C:LAVSPENTSKAP)
  (C:TRAFO)
  (C:UPS-FORDELER)
 )