# The drawing script (`<RailwayInstallationDrawingScript>`)

The script turns RailCOMPLETE's per-object data into a sheet in model space. It runs in the script context (the same
API as RC-RunScript scripts (French: RC-ExecuterScript)) plus the installation-drawing globals below. Start from example-schema.xml.

## 1. When and how it runs

| Run | Trigger | Data | Differences |
|---|---|---|---|
| **Preview** | the export window renders a drawing | `_railwayInstallationDrawingSelectionData` = a list with **one** object; `_railwayInstallationDrawingPreviewMode = true` | Runs headless inside a transaction that is **rolled back**; only entities appended to **model space** are harvested and shown (never paper space). The script runs once per drawing, including background warm-up renders of up to 12 selected drawings, and again when settings change — any side effect happens many times. A script error shows the drawing as Error with a "Content preview" fallback. |
| **Export** | Export button of `_RC-AssistCreateInstallationDrawing`, after the window closes | the list of **all** selected objects that share this script; preview flag nil | Synchronous, inside the command (document locked). Entities stay in the drawing even if the script fails half-way. A Lua error goes to the command line, aborts the run for the whole group and skips RailCOMPLETE's dimension arrows and layouts. After a successful script, RailCOMPLETE draws the dimension arrows and creates layouts (if the script reported them, §3). |
| **Stand-alone** | `_RC-RunScript` on a copy of the script | no global | Call `getRailwayInstallationDrawingSelectionData("<schema Name>")`: it prompts for objects, always uses each object's frame 0 (there is no direction picker) and the drawing's saved settings. `PlaceDimensionsExternally` is false here. |

The **Cross Section Viewer never runs the drawing script.** It shows the projection, the schema components and the
dimensions only. Anything the script draws (sheet furniture, but also object-dependent geometry) is invisible where
users edit dimensions — see schema-reference.md §3 for the alternatives.

Rules that follow from this:

- **Handle a list.** One export run lays out every selected object (one insertion-point pick per run).
- **An object the script decides not to draw** still shows as OK or Warning in the preview list ("Skipped" means only
  "no frame for the direction" or "no content at all": no projection, rails, sleepers or components) and stays ticked for export; the preview shows it with the
  "Content preview – pre-layout" badge. Say why on the command line. RailCOMPLETE draws no arrows and makes no layout
  for a sheet the script did not report.
- **Prompts in preview**: only `askForPoint` (returns the default or the origin), `askForKeyword` (returns nil),
  `askForPointObjects` (returns an empty list) and `getRailwayInstallationDrawingSelectionData` are preview-safe.
  `askForDouble`, `askForInteger`, `askForString`, `askForAlignment`, `askForObject`, `askForPointObject`,
  `askForPath`, `askForFileName`, `askForFolderName`, `showMessage`, `getFileFromPrompt` **prompt during the preview**.
  Wrap them: `if not _railwayInstallationDrawingPreviewMode then … end`. Prefer schema components/parameters for
  user options.
- **Never call `runCommand()` or `beginUndoBufferItem()`** from a drawing script: the script runs synchronously
  inside a command, so `runCommand` freezes AutoCAD for about 11 s and then raises an error (and the command still runs
  after the export), and `beginUndoBufferItem` **hangs AutoCAD for good**, in export and preview alike. Library
  helpers built on `runCommand` (select-all, zoom, CAD-settings helpers in the DNA's `lib2.lua`) are off limits too.
- **`write()` output is normally lost** (it only reaches an RC Lua editor window if one happens to be open), and
  `lib2.show()` pops a blocking dialog during export. For messages the user must see, write to the command line:
  `pcall(function() DocumentData.Document.Editor:WriteMessage("\n" .. msg) end)` — no shipped script uses this yet,
  so ask the user to confirm the message appears; use `error("…")` for fatal problems (it aborts the whole run).
- `askForPoint` raises an error when the user cancels without a default; that is acceptable at the end of a run but
  not in the middle of one.
- **No side effects in preview**: file writes, message boxes and property assignments on RailCOMPLETE objects are not
  rolled back (assigning `someRcObject.Prop = x` saves the object). Guard them with the preview flag.
- **The Lua sandbox**: globals a script assigns live in a per-run sandbox and do not survive to the next run; keep
  variables `local` anyway and never modify shared tables such as `table` or `string`. Native Lua is limited to
  `string, math, table, utf8, assert, error, ipairs, next, pairs, pcall, select, tonumber, tostring, type, xpcall` —
  no `print`, `io`, `os`, `require` or `load`. No execution time limit exists — an endless loop hangs AutoCAD.
- Objects are grouped by identical script text; a script shared by two schemas cannot tell which schema produced the data.
- Compare enum-valued members defensively as strings: `tostring(pointObject.dir) == "up"`.

## 2. What the script receives: `RailwayInstallationDrawingObjectData`

One per selected object, for **one** frame (the one picked by the direction picker). .NET lists are 0-based;
depending on the access path, indexing past the end either raises or returns nil — use `table.select(list)` to get a
Lua table and test `.Count` before indexing.

| Member | Content |
|---|---|
| `PointObject` | the object: every property by name (`name`, `id`, `RcType` (type name), `dir`, `LateralOffset`, `Alignment`, DNA properties), relations, `Dimensions.DimensionList` |
| `PointObjectsInInstallationDrawing` | the host first, then every visible object inside the frame footprint (for tables of mounted equipment etc.) |
| `AllEntities` | everything to draw in the section: projected 3D geometry + rails + sleepers + all enabled components — put it in one block (§3) |
| `PointObjects` | the projected 3D geometry only |
| `Rails`, `Sleepers` | schema geometry |
| `Gauges`, `CatenaryOutlines`, `DiggingEdges`, `RailAnnotations`, `CustomComponents` | lists of `(entity, itemName, componentName)` — filter by component name to treat parts differently |
| `PointObjectPointsOfInterest` | list of `(point, name, ownerKey, fullName)` → `.Item1` (`.X`, `.Y`), `.Item2`, `.Item3`, `.Item4`; e.g. `CenterLine` with owner `OwnAlignment` or an alignment id |
| `GaugePointsOfInterest`, `RailPointsOfInterest` | extra anchors |
| `ExpandedDimensions` | resolved dimensions (`Name`, `FromPoint`, `ToPoint`, `Orientation`, `Offset`, `SetValueLuaScript`, …) — only needed if the script draws its own arrows |
| `PlaceDimensionsExternally` | true in export and preview: RailCOMPLETE draws the dimension arrows; the script must not |
| `DimensionExtents` | `MinX, MaxX, MinY, MaxY, Present` of RailCOMPLETE's arrows in frame-local coordinates, for sizing the sheet — or `nil` when there are none: test `data.DimensionExtents and data.DimensionExtents.Present` |
| `LayoutReport` | writable — how the script tells RailCOMPLETE where things ended up (§3) |
| `FrameDirection` | `"up"`, `"down"`, `"both"`, `"none"` or `"unknown"` — of the frame that was used |
| `AlignmentSnapshots` | one per track found through the object's first non-frozen frame (the object must project perpendicularly onto it) that the schema's `AppliesToAlignmentType` accepts; the object's own reference track, when it is among them, is kept even if `AppliesToAlignmentType` rejects it (no rails are drawn for it then): `Id`, `Name`, `AlignmentGauge`, `PointObjectDistanceAlong`, `Radius`, `Cant` (mm), `DesignSpeed`, `VerticalProfileRadius` — **may be empty** |
| `RasterImageFilePaths` | point-cloud backdrops for the section |
| `ResolutionWarnings` | dimension warnings (already written to the command line) |

Not available: the schema name, the frame's name/index/bounds, other frames of the object.

**Coordinates**: all entities and points are in the frame's display plane — metres, X across (viewer's right), Y up.
**Absolute positions are meaningless; only differences within one frame are.** Put `AllEntities` in a block, centre
the block on its bounds, and position everything else from points of interest converted with
`sheetPoint = sectionBlockPosition + point * scale`. The block's base point is the frame-local origin, and
`DimensionExtents` uses the same frame-local metres, so both can be merged before you scale and centre.

**Entity types**: the projection holds AutoCAD curves (lines, arcs, circles, polylines, splines) and regions from
AutoCAD's section generator — no hatches, no text, and nothing tells which object an entity came from. A schema
`Polyline2D` arrives as a `Polyline`, an `Annotation2D` as an `MText`.

## 3. The layout contract (use this)

This is what example-schema.xml implements. Order matters:

1. **Section block**: `cadInterface.insertBlock(name, table.select(data.AllEntities))` (older RailCOMPLETE:
   `cadInterface.createBlock`), `cadInterface.createBlockReference(name, rcPoint3D)`, centre it on its `Bounds`, set
   `ScaleFactors`. The section must stay an unrotated, uniformly scaled block reference.
2. **Dimensions**: if `data.PlaceDimensionsExternally` is true, draw none; use `data.DimensionExtents` to leave room.
3. **Report** (guard every write; writing a member that does not exist throws on older builds):
   - `LayoutReport.SectionOriginX/Y` = the block's position, `ScaleFactor`, `Reported = true`;
   - if `data.PlaceDimensionsExternally ~= nil`: `ScriptDrewDimensions = not data.PlaceDimensionsExternally`,
     `DimensionStyleReported = true` and the style values `DimensionTextHeight`, `DimensionArrowSize`,
     `DimensionExtensionLineExtension`, `DimensionExtensionLineOffset`, `DimensionTextGap`,
     `DimensionMeasurementScaleFactor` (1000), `DimensionDecimalPlaces`, `DimensionTextStyleName` — **forgetting this
     block exports the sheet without dimensions**;
   - if `LayoutReport.SheetReported ~= nil`: `ScriptCreatedLayouts = false`, `SheetReported = true`, `SheetName`,
     `SheetWidth`, `SheetHeight`, `SheetTopLeftX/Y` — and a **positive `ScaleFactor`**, or the sheet is silently
     skipped.
   All values are in pre-UCS sheet coordinates.
4. **Sheet content**: frame, cartouche, tables, notes — drawn around the block.
5. **UCS**: transform every entity by `DocumentData.Document.Editor.CurrentUserCoordinateSystem` as the last step
   (dimensions you draw yourself: transform their points and set `HorizontalRotation`), then
   `cadInterface.addEntitiesToModelSpace(entities)`.
6. **No layouts, no "create layouts?" prompt**: RailCOMPLETE creates one paper-space layout per reported sheet when the
   export window's "Create paper space layouts" box is ticked. The layout is named `SheetName` (empty: the object's
   identity, + " (up)"/" (down)" for an up/down frame), sanitised and de-duplicated within one run. A later export of a sheet with the same
   name reuses that layout and replaces its viewport; the earlier model-space sheet stays until the user deletes it.

What RailCOMPLETE-created layouts can do: one layout per object and frame, **portrait ISO A4…A0 only**, plot device
`DWG To PDF.pc3`, the smallest format that fits at 20 paper-mm per drawing unit ÷ ScaleFactor — the section always
plots at 1:50; ScaleFactor only enlarges the model-space sheet — export only, when the "Create paper space layouts"
box is ticked. The viewport is sized to the paper **height**, so the result
is right only for sheets with ISO portrait proportions (10.5 × 14.85, 14.85 × 21.0 …). **A landscape or strip sheet
reported this way gets a wrong layout, not an error.** Landscape, custom media (multi-A4 strips) or other scales need
script-created layouts (§6).

The FR-SR DNA's "PV d'implantation" script predates this contract (it draws its own arrows and creates its own layouts after a
prompt). Copy its *content* (SNCF sheet furniture, tables, gauges), not its contract.

## 4. Building entities (there is no `addLine`/`addText` API)

Every entity is an AutoCAD .NET object built by reflection. Type names are relative to `Autodesk.AutoCAD.`.
**Call `cadInterface.*` with a dot, never a colon.** Methods on the returned .NET objects use a colon
(`polyline:AddVertexAt(…)`).

| Need | Recipe |
|---|---|
| point | `cadInterface.createCadEntity("Geometry.Point3d", {x, y, 0})`; RC point: `getPoint3D(x, y)` |
| line | `cadInterface.createCadEntity("DatabaseServices.Line", {p1, p2})` (AutoCAD Point3d) |
| polyline | `cadInterface.createCadEntity("DatabaseServices.Polyline", {})`, `pl:AddVertexAt(i, cadInterface.createCadEntity("Geometry.Point2d", {x, y}), bulge, 0, 0)`, `pl.Closed = true` (bulge ≠ 0 draws arcs) |
| circle / arc | `cadInterface.createCadEntity("DatabaseServices.Circle", {centre, cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1}), r})`; `cadInterface.createCadEntity("DatabaseServices.Arc", {centre, r, startRad, endRad})` |
| text | `cadInterface.createCadEntity("DatabaseServices.MText", {})` then `.Contents` (`\P` new line, `{\L…}` underline), `.Location`, `.TextHeight`, `.Width`, `.Attachment = cadInterface.createCadEntity("DatabaseServices.AttachmentPoint", {"TopLeft"})`, `.TextStyleId = cadInterface.getTextStyleId("RC-STANDARD")`, `.ShowBorders`. `.ActualWidth`, `.ActualHeight`, `.Bounds` work before insertion — use them to size boxes |
| dimension you draw | `cadInterface.createCadEntity("DatabaseServices.AlignedDimension", {})` then `.XLine1Point`, `.XLine2Point`, `.DimLinePoint`, `.DimensionText`, overrides `.Dimtxt .Dimasz .Dimtad .Dimexe .Dimexo .Dimgap .Dimlfac .Dimdec .Dimse1 .Dimse2`. The preview keeps aligned and rotated (linear) dimensions live and draggable (a rotated one as an aligned copy); other dimension types are exploded |
| hatch | must be database-resident: create, add to a block or model space, then `cadInterface.runEntityMethodsWithTransaction(hatch, {SetHatchPattern = {…}, AppendLoop = {…}})`. The DNA helper library's `RC__CAD().createHatch` does this. Simpler: a closed polyline |
| block with attributes | `cadInterface.createCadEntity("DatabaseServices.AttributeDefinition", {pos, text, tag, prompt, styleId})`, `cadInterface.insertBlock(name, {…})`, `createBlockReference`, `addEntitiesToModelSpace({ref})`, then `cadInterface.setBlockReferenceAttributeValues(ref, {TAG = value})` (once). The preview may show attribute tags instead of values — MText in drawn boxes is the safer cartouche |
| a DNA 2D symbol already in the drawing | `if cadInterface.blockExist(name) then cadInterface.createBlockReference(name, p) end`; an object's own blocks: `pointObject:getBlockNames()`; copy a symbol's geometry: `cadInterface.getClonesOfNonTextEntitiesInBlock(name)` |
| layer / linetype / colour | `cadInterface.createCadEntity("DatabaseServices.LayerTableRecord", {})` + `.Name`, `.Color`, then `cadInterface.addLayers({layer})`; `cadInterface.loadLineType(name)`; `entity.Color = cadInterface.getCadColor("red")` |
| transform | `cadInterface.runEntityMethods(entity, {TransformBy = {matrix}})`; matrices via `runExternalLibraryFunction("Autodesk.AutoCAD.Geometry.Matrix3d", "Rotation", {angle, vector, point})` |
| logo / raster image | `local img = createExternalLibraryObject("RailCOMPLETE.Model.Cad.RasterImageInfo", {path, x, y, 0})` (all **four** arguments), `img.Width`, `img.Height` (drawing units), then include it in `addEntitiesToModelSpace({…})`; shown in the preview. Path: absolute, or `runExternalLibraryFunction("RailCOMPLETE.Common.FileLocator", "ReturnFullPath", {"Images\\logo.png"})` (relative to the administration folder `…\Adm\<DNA>\`). Caveats: the image stays an external file reference, and RailCOMPLETE tags it so that deleting it later offers to **delete the image file** — for a logo inside the DNA folder, copy the file next to the drawing first and insert the copy |
| block from an external DWG | no function; `cadInterface.getEntitiesInDatabase(path)` reads entities but inserting them is unreliable. Keep sheet furniture as Lua code, or as symbols in the DNA symbol library |

`createExternalLibraryObject(fullTypeName, args, {Prop = value})` and
`runExternalLibraryFunction(fullTypeName, method, args)` reach any loaded .NET type. They work but are brittle across
RailCOMPLETE versions — keep such calls in one helper and comment what they rely on.

## 5. Tables

Use RailCOMPLETE tables rather than drawing grids by hand:

```lua
local spec = createExternalLibraryObject("RailCOMPLETE.Model.Tables.ViewModel.TableLayoutViewModel", {})
spec.EnableCustomSettings = true
-- spec.Columns: list of createExternalLibraryObject("RailCOMPLETE.Model.Tables.ColumnSpecification", {}) with
--   .Width, .Data (a Lua expression evaluated per row object, e.g. "name" or "getBlockImage()"),
--   .Headers (list of RailCOMPLETE.Model.Tables.Header with .Name; several headers = header rows, top first;
--   equal adjacent header texts merge automatically)
-- Rows: spec.UseCustomCollectionLuaFilter = true; spec.CustomCollectionLuaFilter = "<Lua source returning the rows>"
local tableObject = insertTableObject(rctype_TableUserdefined, getPoint3D(x, y), spec)
```

- The table type is matched by `Name` and **must declare at least one `<InsertTable>`**, even when a specification is
  passed (`rctype_TableUserdefined` does in FR-SR and NO-BN); otherwise the call fails with "No valid InsertTableOptions
  found".
- The row filter is Lua *source text* evaluated later, outside the script — it cannot see script variables. Compute the
  rows in the script and embed them as literals (the FR-SR DNA's `serializeLuaTable` does this).
- The table is a persistent RailCOMPLETE object of the DNA's user-defined table type; `resizeTableObject(t, w, h)`
  (resize twice, smaller first, to work round a known bug), `cadInterface.getCadEntityFromRcObject(t)` for the
  AutoCAD table.
- Works in export and in the preview (the preview explodes tables and draws them monochrome). Always lands in model
  space, on the table type's layer (set `.Layer = "0"` if needed).
- Use the Lua row filter, not an Object Manager filter: the latter depends on the Object Manager palette having been
  opened in the session.
- Older name: `createTableObject` (deprecated).

## 6. Sheets and layouts beyond the contract

- **Script-created layout**: `cadInterface.createLayoutWithViewport(layoutName, sheetWidth, sheetHeight,
  sheetTopLeft, plotDeviceName, mediaName, portrait)` makes (or replaces) one layout with **one** viewport onto the
  model-space sheet rectangle, scale = paper height ÷ sheet height — so give the sheet the media's proportions. Any
  device and media work, including landscape (`"ISO full bleed A3 (420.00 x 297.00 MM)"`; the media decides the
  orientation — pass `portrait = true` to pin the rotation) and custom media from a `.pc3`/`.pmp` pair shipped in the
  DNA's `AutoCAD\Plotters` folder (an unknown media name raises "Could not find an '…' paper size on this device" —
  wrap the call in `pcall`). **Guard it with `if not _railwayInstallationDrawingPreviewMode`**, and leave
  `LayoutReport.ScriptCreatedLayouts` at its default (true) so RailCOMPLETE does not overwrite it. The export window's
  "Create paper space layouts" box is not visible to Lua; give users their own switch (a BooleanParameter under
  Gauges/CatenaryOutlines, or an export-time `askForKeyword`, which returns nil in preview).
- Several viewports on one layout, or entities in paper space: reachable only through fragile reflection
  (`DatabaseServices.Viewport`, `cadInterface.addEntitiesToBlock("*Paper_Space…", …)`), never previewed. Call
  `createLayoutWithViewport` first and add extra viewports after it: a later call on the same layout erases every
  viewport but the first. Paper-space entities are duplicated on re-export unless the script cleans up. Do not build a deliverable on it without the user testing it first.
- Not possible from a drawing script today: plotting to PDF (the user plots or publishes the created layouts), two
  frames of one object in one run.
  Keep sheet furniture (frame, cartouche, tables, logos, notes) in **model space** around the section — that is what
  the preview shows and what a one-viewport layout captures.
- Sheets for several objects: the export run gets all objects, so a combined sheet can be drawn — but the preview
  renders each object alone, and RailCOMPLETE's layouts/dimension arrows are one per object.

## 7. Querying the model

| Need | Function |
|---|---|
| objects of a type | `table.where(DocumentData.ObjectCollection, function(o) return o.RcType == rctype_Track end)` (every `rctype_*` LuaName is a global holding the type's Name string — the same string `obj.RcType` returns) |
| by id | `getObjectFromId(id)` |
| related objects | `getRelatedObjects("relation prompt text", obj)`; mounted/attached objects: `obj.AttachedElements` |
| near a point | `getNearbyPointObjects2D(rcType, false, point, distance)`, `getNearbyAlignments(point, rcType, distance)` |
| mileage / PK and track data at a point | `getAlignmentInfo(obj)` → `.Mileage`, `.ReferenceMileage`, `.DistanceToAlignment`, `.SideOfAlignment`, `.CurveRadius`, `.Cant`, `.Elevation`, `.AlignmentName` … |
| position from PK | `alignment:getPosFromMileage(m)`, then `alignment:getPoint(pos)` |
| sample a track in plan | `alignment.RcAlignment.HorizontalGeometry:GetPointAtPos(s)` (for schematic plans drawn by the script) |
| topology | `getUpObject/getDownObject`, `getUpObjectsWithPaths`, `getPathsToObject` |
| project data for a cartouche | `DocumentData.RailwayDocumentHeader` (`ProjectName`, `Stations`, `DnaName`, `DnaVersion`, …) |
| schema parameters | `getCurrentInstallationDrawingSchemaParameters("Gauges")` |
| external data | `getFileFromPath(FileType.Excel, path)` (sheets → rows keyed by header) |
| DNA helpers | any `<LuaFunction>` of the DNA; shared script libraries via `local lib = includeLuaFile("Lua\\Functions\\x.lua")` then `lib.fn()` — it returns a table of the file's globals (path relative to the administration folder `…\Adm\<DNA>\` — the deployed copy of the repository's `FR-SR`/`NO-BN` folder that holds `DNA`, `Lua` and `Images`) |

## 8. Choosing the host, and patterns for sheets that are not cross sections

Many administration documents (procès-verbaux, equipment sheets) are not a cut through the track. Decide the host
first:

| Document | Host | Why |
|---|---|---|
| Sheet per object built around a cross section or a front elevation of 3D solids | RID schema, RID-native content | frames, components, points of interest and editable dimensions all apply |
| Sheet per object whose content is plans, 2D elevations, tables (no usable projection) | RID schema **as a host** — the script draws everything from model queries and 2D blocks | same command, selection, preview and batch export as other drawings. Each object still needs a cross section frame (a small box is enough), or the export and the preview skip it; nothing else RID-native is used |
| A list or form over many objects (one row per object) | a DNA script in the Scripts menu (`<DNA>\Lua\Scripts\…`, run through `_RC-RunScript`), optionally with an RC table | RID prepares one data object and one preview per object, which fights a list; a plain script runs asynchronously, so prompts, `runCommand` and layouts all work there |

Patterns that recur:

- **Script body in a library.** Keep the embedded `<RailwayInstallationDrawingScript>` a thin wrapper that loads the
  real code: `local pv = includeLuaFile("Lua\\Functions\\PV\\my_pv.lua")`, then calls it with the selection data and
  the preview flag as arguments. `includeLuaFile` re-reads the file on every call and resolves the path relative to the
  deployed administration folder (`…\Adm\<DNA>\`, not its `DNA` subfolder), so a script edit needs no DNA rebuild — copy the file to the deployed folder and close/reopen
  the preview window (its cache is per window). The same library serves a Scripts-menu entry point for batch jobs.
  Keep libraries under `Lua\Functions`, since everything under `Lua\Scripts` is listed as a runnable script. (Confirm
  the first time that the included chunk behaves the same in preview and export.)
- **Script-only sheets and the preview status.** A drawing whose data has no content at all — no projected geometry
  and no rails, sleepers or components — gets the grey "Skipped" dot even when the script draws a full sheet for it.
  The dot is cosmetic: the drawing stays ticked and Export still hands it to the script. For a normal status, declare
  one tiny `<Rails>` polyline (e.g. two coincident points; `AppliesToAlignmentType` set to the track type): it gives
  the object content whenever an applicable track is in its first non-frozen frame. It is also part of
  `data.AllEntities` and is drawn in the Cross Section Viewer. Untested — confirm once.
- **Schematic plan de situation**: draw tracks as lines ordered by lateral offset from `getNearbyAlignments`, place
  objects by (distance along, distance across) from `getAlignmentInfo`, and write distances as texts — not to scale.
- **Elevation from 2D blocks**: a slot table per support type (which block at which height), filled from the objects
  mounted on the support (`AttachedElements` or relations); explicit `DimensionText` on every dimension (the preview
  rebuilds each dimension from its points, text, style and a few overrides; the script's `Dimlfac`/`Dimdec` overrides
  are dropped); no attributes inside the blocks (the preview explodes blocks, so attribute tags may show) and, until
  tested, no wipeouts either.
- **Plan of the real drawing at a scale**: a whole-sheet `createLayoutWithViewport` onto a model-space rectangle around
  the object (e.g. 42.0 × 29.7 m on an A3-landscape medium = 1:100). The viewport follows the current UCS, so the user
  sets a UCS along the track first (one direction per run). `createLayoutWithViewport` takes `sheetTopLeft` in
  current-UCS coordinates (it transforms the point by the UCS itself), so convert the object's world position into the
  UCS first (`cadInterface.runEntityMethods(p, {TransformBy = {DocumentData.Document.Editor.CurrentUserCoordinateSystem:Inverse()}})`),
  build the rectangle and the overlay in those coordinates, and transform the overlay by the UCS as the last step
  (§3 step 5). The script's overlay (dimensions, labels, frame) lives in
  model space at the object's real position — put it on a layer per object so it can be removed before the next
  export; the preview shows only the overlay, never the underlying drawing.

## 9. What an installation drawing can and cannot be

| Want | Status |
|---|---|
| cross section at an object, with rails, gauges, dimensions, cartouche, tables | yes — the core use |
| front elevation of equipment beside the track | yes — a frame yawed ±90° (cross-section-frames.md §7); no track needed; needs solid 3D models |
| equipment elevation drawn from 2D blocks with heights from properties | yes — in the script |
| schematic plan de situation drawn by the script from model queries (tracks, the object, distances, PK) | yes — the FR-SR DNA's PV d'implantation already draws one in its signal cartouche (signal symbol, track, PK, track-circuit joint dimension); a general helper is a few hundred lines once, reusable (§8) |
| the real drawing (survey, roads, symbols) as a whole sheet at a scale, with an overlay | yes — a script-created layout (§6, §8), outside the preview |
| the real drawing beside a separate cartouche or other views on one sheet | no — needs a second viewport or paper-space content, a RailCOMPLETE C# change (only fragile reflection today). A frame or cartouche drawn in model space over the viewed area works (§8), but the drawing shows through it |
| two views of one object on one sheet (face + side, or up + down) | no in one run. `cadInterface.getCrossSectionFrame(obj, false)` returns the old-style projection of **all** non-frozen frames of an object merged into one list (overlapping, no schema components, points keyed by frame name) — usable only for an object with exactly one frame, e.g. a neighbour |
| landscape or multi-A4 sheets with RailCOMPLETE-created layouts | no — script-created layouts only (§6) |
| tables inside a sheet | yes — an RC table built from a Lua spec with `insertTableObject` (§5), as the FR-SR DNA's PV d'implantation does for its "implantation longitudinale" table (it still calls the deprecated name `createTableObject`; use `insertTableObject`) |
| a document that is only a list over many objects (one row per object, e.g. a signal survey form) | better as a Scripts-menu script (RC-RunScript, §8): an installation drawing prepares one data object and one preview per object |
| PDF output | yes, through AutoCAD's own plot/PUBLISH on the created layouts; no automatic PDF from within the drawing script |
