---
name: installation-drawings
description: Use when creating or changing railway installation drawings in the DNA -- RailwayInstallationSchema, RailwayInstallationDrawingScript, cross section frames (CrossSectionFrames dynamic property), points of interest, DefaultDimensions and SetValueScripts -- or when an installation drawing, a frame, a dimension, the Cross Section Viewer or RC-ASSISTCREATEINSTALLATIONDRAWING does not behave as expected
---

# Railway installation drawings (RID)

A railway installation drawing is a per-object sheet that RailCOMPLETE generates from the model: a projection of the
3D surroundings of a placed object (usually a cross section at the object, or a front elevation of equipment), the
administration's rails, gauges and outlines, dimensions to points of interest, and whatever sheet furniture the DNA's
Lua script draws around it (cartouche, tables, notes). This skill is the contract between the DNA and RailCOMPLETE —
you do not have RailCOMPLETE's C# source, so do not guess behaviour it does not describe; flag it as a question for
the user to test.

$ARGUMENTS

## The five parts and where the user sees them

| DNA part | Declared in | What it does | Where the user meets it |
|---|---|---|---|
| **Cross section frame** | `<DynamicProperty Type="CrossSectionFrames" Subtype="CrossSectionFrame">` on object types | the camera box on each placed object: what is projected, from where, which tracks count | Properties palette → *Cross sections*; frame list in the Cross Section Viewer; direction picker in the export window |
| **Schema** | `<RailwayInstallationSchema>` in the DNA's single `<RailwayInstallationSchemaContainer>` | names a drawing type; which tracks get rails/sleepers/components | schema drop-down in the export window |
| **Components** | `<Rails>`, `<Sleepers>`, `<Gauges>`, `<CatenaryOutlines>`, `<DiggingEdges>`, `<RailAnnotations>`, `<CustomComponents>` | 2D geometry drawn per track from Lua formulas | always drawn (rails, sleepers) or check boxes/drop-downs in the export window |
| **Dimensions** | `<DefaultDimensions>` templates between points of interest | copied onto objects at insertion; resolved per drawing; optionally editable with a `SetValueScript` that moves the object | green-yellow editable dimensions in the Cross Section Viewer; dimension arrows on the sheet |
| **Drawing script** | `<RailwayInstallationDrawingScript>` (Lua) | lays out the sheet in model space and reports it back | the preview in the export window; the sheet and layouts after Export |

Data flow: user runs **`_RC-AssistCreateInstallationDrawing`** on selected objects → picks schema, direction and
components in the export window → for each object RailCOMPLETE takes **one** frame, projects the 3D objects inside it,
evaluates the schema's component formulas for the tracks in it, resolves dimensions → hands a list of per-object data to
the drawing script → the script draws sheets in model space → RailCOMPLETE adds dimension arrows and paper-space
layouts. The export window's preview runs the same script headless, one object at a time.

## Work in this order

1. **Is this an installation drawing at all?** It fits a sheet per object, built around a projection or drawn by the
   script from model queries (schematic plans, 2D elevations, tables inside the sheet). A list over many objects, the
   real surveyed drawing beside other views on one sheet, or two views of one object on one sheet do not fit — see
   drawing-script.md §8 (choosing the host) and §9 (limits) before promising anything.
2. **Frames** on the object types — cross-section-frames.md. Check the 3D models are solids and add named points where
   dimensions need them.
3. **Schema skeleton** — copy example-schema.xml into the single container; set `Name`, `AppliesToAlignmentType`.
4. **Components and dimensions** — schema-reference.md.
5. **Drawing script** — drawing-script.md; keep the layout contract of the example.
6. **Styles** — a `<DimensionStyle>` with `Dimlfac` 1000, text styles, linetypes you reference.
7. **Release notes**, then hand over for building and testing — testing-and-troubleshooting.md gives the user's test
   loop; you check what you can before that (XML, Lua compile, names).

## Rules that break drawings silently

1. One `<RailwayInstallationSchemaContainer>` only; a schema without a drawing script is not offered.
2. In component and POI formulas **`this` is the track, not the object**; the object is not passed in. Object-dependent
   geometry goes in the drawing script (export preview and sheet only — **the Cross Section Viewer never runs the
   script**), in the 3D model, or in a formula that finds the object from the track position (schema-reference.md §3).
3. Frames and template dimensions are created **when an object is inserted**. Existing objects need
   `_RC-MatchDynamicProperties` / "Restore missing dimensions from the DNA". Put frame logic in named
   `<LuaFunction>`s so improvements reach existing objects.
4. **One frame per object per export run.** The direction picker chooses it; the script cannot see other frames.
5. Only **solid/surface/region/body** geometry of 3D models is projected — mesh models are invisible (several library
   models are meshes; check). Object points of interest can only be added as marker blocks (layer
   `RC_PointsOfInterest`, attribute `POINTOFINTEREST_NAME`) in the 3D DWG.
6. Dimensions belong to **objects**, not drawing types: they are copied at insertion (SetValueScript text included)
   from the drawing's selected schema, and every schema's drawing shows every stored dimension of an object. Editing a
   dimension in the DNA does not reach placed objects. Keep dimension `Name`s unique across all schemas and distinct
   from the built-in schema's (`Lateral offset`, `Insertion height`, `Top height`, `Cant`) — prefix them.
7. A dimension is editable only if its `SetValueScript` contains `_dimensionValue`; values arrive in metres only if the
   dimension style has `Dimlfac` 1000; each `this.X = …` saves immediately, so compute first and assign last.
8. In the drawing script: handle a **list** of objects; only `askForPoint`, `askForKeyword`, `askForPointObjects` are
   safe in the preview; never `runCommand` or `beginUndoBufferItem` (freeze/hang); `write()` is not shown; test
   `.Count` before indexing .NET lists; `cadInterface.x(…)` with a dot; the preview re-runs the script many times,
   so no side effects there.
9. Script data has no fixed origin — anchor everything on the section block's bounds and on points of interest.
10. Report the section transform (with a positive `ScaleFactor`), dimension style and sheet through `LayoutReport`, or
    the export has no dimensions and no layouts. RailCOMPLETE's layouts are portrait ISO only; landscape or strip
    sheets need `cadInterface.createLayoutWithViewport`, outside the preview.
11. `BooleanParameter` `DefaultValue` is ignored; parameters are shown only for Gauges/CatenaryOutlines (elsewhere
    they are fixed at true) and only with an `ApplicableSubcategory`; Lua reads the drawing's *saved* settings of its
    selected schema.
12. Escape `<` and `&` in embedded Lua (or use a raw CDATA section, never `<xpp:cdata>`); an XML comment cannot
    contain `--`. Component formula errors go only to RailCOMPLETE's log file, and `table.select` silently drops
    elements whose function fails.

## Quick reference

| Thing | Value |
|---|---|
| Export command | `RC-AssistCreateInstallationDrawing`; ribbon: RailCOMPLETE tab → Assist panel slide-out → "Assist Create Installation Drawing..."; window title "Installation drawing preview". Command names are localized — type them with a leading underscore (`_RC-AssistCreateInstallationDrawing`) to work in any language |
| Viewer | right-click an object → **Open Cross Section Viewer**; or the eye button in the palette's *Cross sections* category (no typed command). Shows projection, components and dimensions — never the drawing script's output |
| Frame debugging | `_RC-Show2dProjectionBoxPreview`, `_RC-Show2dProjection3dSourcePreview`, `_RC-Show2dProjectionPreview` |
| Retrofit frames | `_RC-MatchDynamicProperties` |
| Load a rebuilt DNA into a test drawing | `_RC-AGENT-LoadDnaFromXml`, then `_RC-AGENT-ReplaceDnaInDrawing` (Agent licence); otherwise a new drawing, or `_RC-UpdateDnaWithMapping` |
| Units | metres everywhere; `Cant` in mm; dimension labels are always whole mm; the dimension style needs `Dimlfac` 1000 so typed values reach Lua in metres |
| Formula globals (components) | `this` = track, `_alignmentSnapshot`, `_position` |
| SetValueScript globals | `this` = object to change, `_dimensionValue`, `_previousDimensionValue` (m), `_alignmentSnapshots[id]` |
| Drawing-script globals | `_railwayInstallationDrawingSelectionData` (list), `_railwayInstallationDrawingPreviewMode` (true in preview), `cadInterface`, `DocumentData` |
| Object points | `My_InsertionPoint`, `My_TopCenter` (…9 box points), `My_3DGeometry-<name>` |
| Track points | `OwnAlignment_CenterLine`, `_LeftInnerRail`, `_RightInnerRail`, `_NearestRail`, `_LowestRail` |
| Cant-correct a formula point | `RC_ApplyLiftAndRotate(this, _alignmentSnapshot.PointObjectDistanceAlong, p, _alignmentSnapshot.Cant)` |
| Built-in gauge contours | `RC_GaugeContourG1/G2/GA/GB/GC(this, _alignmentSnapshot, margin)` |

## Files in this skill

| File | Read it when |
|---|---|
| example-schema.xml | starting a new drawing type — a complete schema with a script that follows the current contract |
| schema-reference.md | writing any schema XML: grammar, formula contexts, components and export-window controls, points of interest, dimensions |
| cross-section-frames.md | declaring or debugging frames, 3D models and named points, front elevations |
| drawing-script.md | writing the Lua that lays out the sheet: inputs, layout contract, entity recipes, tables, model queries, limits |
| testing-and-troubleshooting.md | before handing over, and whenever something does not show up |

## Common mistakes

| Mistake | Instead |
|---|---|
| Guessing command names or UI labels | use the names in this skill; ask the user when a label is not here |
| Declaring an object point of interest in XML | add a marker block to the 3D DWG (cross-section-frames.md §6) |
| A `Hatch2D` in the schema, or a zigzag polyline as "hatch" | a closed `Polyline2D`, or a real hatch drawn by the script |
| Reading `this.<object property>` in a component formula (`this` is the track) | draw object-dependent parts in the script (not visible in the viewer), or look the object up from the track position |
| Expecting existing objects to pick up new frames or dimensions | give the retrofit steps with the handover |
| `askForDouble` in the script for a user option | a component toggle or a BooleanParameter |
| Copying an older script's contract, such as the FR-SR DNA's PV d'implantation (own arrows, own layouts, layout prompt) | copy example-schema.xml's contract; reuse only the older script's content |
| Indexing `data.AlignmentSnapshots[0]` blindly | check `.Count`; decide what the sheet shows without a track |
| Reusing the built-in schema's dimension names (copied from example code) | prefix every dimension name per drawing type |
| Designing a dimension around a marker in a 3D model that does not exist yet | list the new or changed DWG as a deliverable for the user |
| Assuming a library 3D model will show in the projection | check it is made of solids; meshes are invisible |
| Promising the real drawing next to other views on one sheet, two views of one object on one sheet, landscape RailCOMPLETE-created layouts or a PDF written by the drawing script | these need RailCOMPLETE changes today — say so. A whole-sheet view of the real drawing, script-created landscape layouts and PDF through PUBLISH work |

## Repository specifics (NO-BN)

NO-BN has **no installation drawing schema yet**; users only see RailCOMPLETE's built-in one — which is therefore
the selected schema in every NO-BN drawing, so every object inserted with a current RailCOMPLETE already carries the
built-in dimensions `Lateral offset`, `Insertion height`, `Top height` and `Cant`. They will appear (or warn) in
NO-BN drawings: never reuse those names, and tell users to select the NO-BN schema and press Save settings in older
drawings.

- **Start the container**: create `NO-BN/DNA/_SRC/NO-BN-InstallationDrawings.xml` holding the DNA's one
  `<RailwayInstallationSchemaContainer>` and include it from `NO-BN-RootFile.xml`. All future schemas go inside it.
- **Frames**: exactly one exists — `rctype_Signal` ("JBTSA_SIG Signal") in `NO-BN-Signals.xml`, in the legacy spelling
  `Type="Projection2DCollection" Subtype="Projection2D"` (still loads as a cross section frame). It is only ±0.5 m deep,
  has no `RotationCenter` and yaws `dir == "up" and 0 or 180`. When touching it: switch to the new spelling, set
  `RotationCenter`, and consider a deeper box. Every other object type needs frames declared before it can be drawn
  (cross-section-frames.md; put the bounds logic in `NOBN_<discipline>_…` LuaFunctions).
- **Dimension style**: `NO-BN-StyleDefinitions.xml` has no `<DimensionStyle>`; add one with `Dimlfac` 1000 (see the
  example) and reference it from `DefaultDimensions`. Text styles `RC-STANDARD` and `RC-ARIAL` exist.
- Track type: `rctype_Track` "JBTKO_SPO Spor" (railML `eTrack`).
- Gauges: RailCOMPLETE's `RC_GaugeContourG1/G2/GA/GB/GC` and `RC_ApplyLiftAndRotate` work in NO-BN formulas as they are.
  `NO-BN-GaugeHalfProfiles.xml` holds Norwegian half profiles as `<DisplayGaugeSettings>` data for RailCOMPLETE's
  train display — not reachable from Lua; to draw one, copy its coordinates into a `NOBN_<discipline>_…` LuaFunction
  returning `getPoint3D` points (mirrored for the left half) passed through `RC_ApplyLiftAndRotate`.
- Natural first candidates (they have 3D models): the main signal, the OCS family (`rctype_OcsPole`,
  `rctype_Cantilever`, `rctype_OcsPortal`, drop arms) with their foundations as related objects. The main-signal models
  (`3D/STD-2026.1/SA/NO-BN-3D-SA-SIG-NSI63-HS*.dwg`, 144 files) are 3D solids (sampled), so they project, but the
  sampled ones have no `RC_PointsOfInterest` markers: a dimension to a point on the signal head needs a marker in
  every model the signal can resolve to. Other families are unchecked — ask the user.
- `rctype_Signal` defaults: `LateralOffset = RightSided and 3.5 or -3.5` (+ = right of the track), `VerticalOffset =
  PortalMounted and 8.0 or 0`. Frame Z bounds are relative to the insertion point, which includes `VerticalOffset`: a
  portal-mounted signal needs `Min.Z` below −8, or its frame finds no track.
- Table type for `insertTableObject`: `rctype_TableUserdefined` (`NO-BN-Tables.xml`). CAD helpers: `RC__CAD()` in
  `NO-BN-LuaCode-BasicCADFunctions.xml`.
- Repository conventions: Lua naming `RC__…` (generic), `NOBN_<discipline>_…`, `_OBJECTTYPE_…`; conventional
  commits (`feat: …`); release notes in `NO-BN/ReleaseNotes/`. Embedded Lua either XML-escaped (NO-BN's usual style)
  or in a raw `<![CDATA[ … ]]>` section, which XPPq passes through — never `<xpp:cdata>` around Lua (escaped Lua then
  arrives as `&lt;`).
- This skill's own policy: hand the DNA build and every AutoCAD test to the user, and write user-visible labels
  (component names, sheet texts) in Norwegian.

