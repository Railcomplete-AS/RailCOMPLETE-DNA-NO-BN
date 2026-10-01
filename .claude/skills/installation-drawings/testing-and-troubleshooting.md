# Testing and troubleshooting installation drawings

You (the agent) cannot run AutoCAD. Every change is verified by the user in AutoCAD, so make each test round cheap:
say exactly what to build, what to load, what to click and what they should see.

## 1. What you can check yourself before handing over

- The XML is well-formed and the preprocessor output contains your schema **inside the single container**.
- Every embedded Lua block compiles (extract `<Formula>`, `<SetValueScript>` and the drawing script, unescape
  `&lt; &gt; &amp;`, and compile each as RailCOMPLETE does: `load("return " .. src)` first and, only if that fails,
  `load(src)` — an expression formula such as `name` or `dir == "down" and 180 or 0` is valid only in the first form).
- Every point-of-interest name in `PointOfInterestA/B` follows the tables in schema-reference.md §5, and every
  `3DGeometry-<name>` really exists as a marker in the 3D models (ask the user if you cannot inspect the DWGs).
  Every `Geometry3D` model file you name exists — list any new DWG in the handover as a file the user must create.
- The 3D models of every object type the drawing shows are solids, not meshes (ask the user to check if unsure).
- Dimension names are prefixed per drawing type and do not reuse the built-in schema's names.
- Every `LuaName` in `AppliesToObjectTypes`/`PerspectiveType` exists in the DNA; every `DimensionStyle`, text style and
  linetype you name exists in `<StyleDefinitions>`.
- No preview-unsafe prompt, `runCommand`, `beginUndoBufferItem` or unguarded file write in the drawing script.

## 2. The user's test loop (give them these steps)

1. **Build and deploy the DNA** with the repository's build script (you must not run it yourself). RailCOMPLETE can
   load from both `%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle` and `%PROGRAMDATA%\…`; make sure the one being
   loaded is the new one.
2. **Get the new DNA into the test drawing.** A drawing carries its own copy of the DNA and re-loads it when it becomes
   active. For an existing test drawing: `_RC-AGENT-LoadDnaFromXml` (pick the built DNA file), then
   `_RC-AGENT-ReplaceDnaInDrawing` — both need an **Agent licence**. Without one: start a new drawing with the new
   DNA, or use `_RC-UpdateDnaWithMapping`.
3. **New or changed frames/dimensions only reach newly inserted objects.** Insert fresh test objects, or retrofit
   (`_RC-MatchDynamicProperties` from a freshly inserted source object, only frames ticked, mode Replace; delete old
   dimensions, then viewer canvas menu "Restore missing dimensions from the DNA").
4. **Frame check**: select an object, `_RC-Show2dProjectionBoxPreview` (is the box where you expect?), then right-click →
   **Open Cross Section Viewer** (right frame in the frame list, the expected objects, rails, dimensions).
5. **Export check**: `_RC-AssistCreateInstallationDrawing`, pick the schema in the drop-down, look at every item's
   status dot in the preview list (OK / Warning / Error / Skipped) and the preview itself; press **Save settings** if
   schema parameters were changed (it also makes the chosen schema the drawing's selected one, which newly inserted
   objects copy their dimensions from); then Export and inspect model space and the created layouts.
6. **Report back**: the command-line text, the preview status and its message, a screenshot of the preview and of the
   sheet. RailCOMPLETE's log file (`_RC-OpenLogFile`) holds formula errors and preview script errors that are shown
   nowhere else.

Build a small, permanent test drawing: one straight track and one curved, canted track; one object of each type the
schema targets, left and right of the track, up and down facing; one object away from any track.

## 3. Symptom → cause

| Symptom | Likely cause |
|---|---|
| Schema missing from the export window's drop-down | no or empty `<RailwayInstallationDrawingScript>`; schema placed in a second `RailwayInstallationSchemaContainer`; test drawing still carries the old DNA (§2 step 2); raw `<` in embedded Lua broke the DNA load |
| Object skipped ("Skipped …: no cross-section frame is declared for the selected direction.", grey status) | the object has no frame (inserted before the frame existed), or Up/Down picked and no frame matches that direction and none is non-directional |
| Viewer shows an empty canvas | object has no frames, or the selected frame is frozen |
| Object present but not drawn | 3D model made of meshes/lines (only solids, surfaces, regions, bodies are projected); object hidden; insertion point outside the box footprint; box Z range misses it |
| No rails/sleepers/gauges | the track is outside the first non-frozen frame's X/Y rectangle or Z range (objects at Z=0 next to tracks with real elevations); `AppliesToAlignmentType` does not match the track type (exact Name, or railML type such as `eTrack`) |
| One schema component missing, no error anywhere | its formula failed or returned the wrong type (see the log file); `this` used as if it were the object; a `Hatch2D` in the schema |
| Rails drawn in a front-elevation frame | the track is inside the yawed box; keep it out or use a schema without rail components |
| Dimension missing, warning "Dimension '…' skipped: point of interest '…' or '…' not found." | wrong POI name; `OwnAlignment_*` with no track in the frame; `3DGeometry-*` marker absent from that 3D model; related object not related to the host |
| Dimension present in the viewer but not on the exported sheet | the script did not report its layout (`Reported`), drew its own dimensions (`ScriptDrewDimensions` true), or did not report the dimension style (command line: "…the DNA script left the dimensions to RailCOMPLETE but reported no dimension style…"); `ScaleFactor` ≤ 0 ("…could not be drawn (zero measured length, or an unusable sheet scale)"); or it is a yellow proposed template instance, which is never exported |
| Dimensions from the wrong drawing type on an object | dimensions belong to objects; all schemas' stored dimensions show up — restrict with `AppliesToObjectTypes` |
| New template dimensions not on objects | only copied at insertion, from the schema selected in that drawing's export settings; use "Restore missing dimensions from the DNA" |
| Dimension not editable in the viewer | its SetValueScript does not contain the text `_dimensionValue` |
| Typing 1850 moves the object 1850 m | the dimension style lacks `Dimlfac 1000`, or `DefaultDimensions/@DimensionStyle` does not name it |
| Editing a value does nothing | a formula on a *linked* property recomputed it (e.g. `DistanceToAlignment` for `LateralOffset`) — clear that one with `"="`; the script wrote `Prop = …` without `this.`; the script errored (generic message on the command line, the Lua error in the log file); for a subject-bound dimension `this` is the related object, not the host |
| Preview shows "Content preview – pre-layout" | status Error: the drawing script raised an error — the message is only in RailCOMPLETE's log file (`_RC-OpenLogFile`); Export writes it to the command line. Status OK/Warning: the script ran but drew nothing for that object (e.g. it skipped a trackless object) |
| Preview hangs or pops up a prompt | a non-preview-safe `ask*` prompt, `runCommand` or `beginUndoBufferItem` in the script |
| Warnings from `write()` never appear | expected — use the command-line helper (`Editor:WriteMessage`) |
| Parameter change has no effect | parameter values are read from the saved settings of the drawing's selected schema: press Save settings, and reopen the preview window (already rendered drawings stay cached); a per-object schema choice does not change which parameters Lua reads. Parameter not shown at all → no `ApplicableSubcategory`, none of its components ticked, or it is not in Gauges/CatenaryOutlines |
| Changed a dimension or SetValueScript in the DNA, objects still behave the old way | dimensions (script text included) were copied onto the objects at insertion; delete the dimension and "Restore missing dimensions from the DNA", or re-insert |
| Dimensions of another drawing type appear, or a restore does nothing | dimension names collide across schemas, or the object was inserted while another schema was selected — prefix names per schema |
| Sheet without layouts | "Create paper space layouts" unticked, or the script did not report `SheetReported`; script-created layouts are skipped in preview by design |
| Everything shifted/rotated in the preview | entities not transformed by the current UCS as the last step, or the section block rotated/non-uniformly scaled |
| A DNA function change does not reach existing objects | the formula text was copied at insertion with the logic inlined — move logic into a named `<LuaFunction>` |
| Behaviour changed after a RailCOMPLETE update, no error | a renamed Lua API function (deprecated names keep working for a while); check the release notes and the command line for deprecation warnings |
