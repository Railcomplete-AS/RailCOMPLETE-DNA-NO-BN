;=========================================================================================================================
;
; 10_Thumbnails.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail symbols top-level LISP routine

; Icons (small images) which are shown in the list of available object types which are NOT point objects, during the 
; object creation process (RC-CreateTable, RC-CreateAlignment, RC-CreateArea, RC-CreatePointObject).


(setq f (strcat rootFolder "\\Main\\Thumbnails"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 10_GENERATE-THUMBNAILS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(setq thumbnailInfix (strcat "THUMBNAIL"			))
	(TraceLevel2 "THUMBNAIL-UNSPECIFIED")					(THUMBNAIL-UNSPECIFIED)
	(TraceLevel2 "THUMBNAIL-TABLE")							(THUMBNAIL-TABLE)
	(TraceLevel2 "THUMBNAIL-AREA")							(THUMBNAIL-AREA)
	(TraceLevel2 "THUMBNAIL-ROAD")							(THUMBNAIL-ROAD)
	(TraceLevel2 "THUMBNAIL-PEDESTRIAN-LANE")				(THUMBNAIL-PEDESTRIAN-LANE)
	(TraceLevel2 "THUMBNAIL-BICYCLE-LANE")					(THUMBNAIL-BICYCLE-LANE)
	(TraceLevel2 "THUMBNAIL-CABLE-DUCT-OPEN")				(THUMBNAIL-CABLE-DUCT-OPEN)
	(TraceLevel2 "THUMBNAIL-CABLE-DUCT-FLEXIBLE-TUBE")		(THUMBNAIL-CABLE-DUCT-FLEXIBLE-TUBE)
	(TraceLevel2 "THUMBNAIL-CABLE-DUCT-CONCRETE-ENCASED")	(THUMBNAIL-CABLE-DUCT-CONCRETE-ENCASED)
	(TraceLevel2 "THUMBNAIL-CABLE-DUCT-RIGID-TUBE")			(THUMBNAIL-CABLE-DUCT-RIGID-TUBE)
	(TraceLevel2 "THUMBNAIL-MANHOLE")						(THUMBNAIL-MANHOLE)
	(TraceLevel2 "THUMBNAIL-RAILWAY-TRACK")					(THUMBNAIL-RAILWAY-TRACK)
	(TraceLevel2 "THUMBNAIL-RAIL-PROFILE")					(THUMBNAIL-RAIL-PROFILE)
	(TraceLevel2 "THUMBNAIL-CONTACT-WIRE-PROFILE")			(THUMBNAIL-CONTACT-WIRE-PROFILE)
	(TraceLevel2 "THUMBNAIL-CATENARY-WIRE-SYSTEM")			(THUMBNAIL-CATENARY-WIRE-SYSTEM)
	(TraceLevel2 "THUMBNAIL-EARTHING-WIRE")					(THUMBNAIL-EARTHING-WIRE)
	(TraceLevel2 "THUMBNAIL-ESCAPE-ROUTE")					(THUMBNAIL-ESCAPE-ROUTE)
	(TraceLevel2 "THUMBNAIL-PLATFORM-ELEMENT")				(THUMBNAIL-PLATFORM-ELEMENT)

	; Cleanup temp globals
	(setq thumbnailInfix nil)

	; Specific to this administration:
	; (nothing)
)
