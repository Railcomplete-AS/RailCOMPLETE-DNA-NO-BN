;=========================================================================================================================
;
; Thumbnails.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Thumbnail symbols top-level LISP routine

; Icons (small images) which are shown in the list of available object types which are NOT point objects, during the creation process RC-CreateXxxxxx

(loadFolder (findfile "Thumbnails")) 

(defun C:GENERATE-THUMBNAILS ( / )
	(setCadSystemDefaults)
	(C:THUMBNAIL-ROAD)
	(C:THUMBNAIL-PEDESTRIAN-LANE)
	(C:THUMBNAIL-EARTHING-WIRE)
	(C:THUMBNAIL-CABLE-DUCT)
	(C:THUMBNAIL-CONTACT-WIRE)
	(C:THUMBNAIL-CATENARY)
	(C:THUMBNAIL-AREA)
	(C:THUMBNAIL-ESCAPE-ROUTE)
	(C:THUMBNAIL-CLOSED-CABLE-CONDUIT)
	(C:THUMBNAIL-TRACK)
	(C:THUMBNAIL-RAIL-PROFILE)
	(C:THUMBNAIL-BICYCLE-LANE)
	(C:THUMBNAIL-TABLE)
	(C:THUMBNAIL-FLEXIBLE-TUBE)
	(C:THUMBNAIL-UNSPECIFIED)
)
