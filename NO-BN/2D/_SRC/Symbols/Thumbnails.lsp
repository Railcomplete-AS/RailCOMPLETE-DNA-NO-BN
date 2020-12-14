;
; Thumbnails.lsp
;
; Icons (small images) which are shown in the list of available object types which are NOT point objects, during the creation process RC-CreateXxxxxx
;

(loadFolder (findfile "Thumbnails")) 

(defun C:GENERATE-THUMBNAILS ( )
	(setCadSystemDefaults)
	(C:THUMBNAIL-BILVEI)
	(C:THUMBNAIL-GANGVEI)
	(C:THUMBNAIL-JORDLEDER)
	(C:THUMBNAIL-KABELKANAL)
	(C:THUMBNAIL-KONTAKTLEDNING)
	(C:THUMBNAIL-KONTAKTLEDNINGSOPPHENG)
	(C:THUMBNAIL-OMRAADE)
	(C:THUMBNAIL-ROEMNINGSVEI)
	(C:THUMBNAIL-ROERPAKKE)
	(C:THUMBNAIL-SPOR)
	(C:THUMBNAIL-SYKKELVEI)
	(C:THUMBNAIL-TABELL)
	(C:THUMBNAIL-TREKKEROER)
	(C:THUMBNAIL-UNSPECIFIED)
)
