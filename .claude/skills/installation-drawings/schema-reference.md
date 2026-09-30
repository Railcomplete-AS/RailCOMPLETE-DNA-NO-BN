# RailwayInstallationSchema reference

Everything a DNA author writes inside `<RailwayInstallationSchemaContainer>`. There is no XSD for these elements;
this file is the grammar. Units are metres unless stated.

## 1. Placement

- Exactly **one** `<RailwayInstallationSchemaContainer>` directly under the DNA root `<RailwayObjectTypeDefinitions>`.
  RailCOMPLETE reads only the first one; a second container is silently ignored. With XPPq, let each drawing type's
  include file contribute only `<RailwayInstallationSchema>` elements that expand *inside* the one container.
- Any number of `<RailwayInstallationSchema>` elements inside it. Where the container sits in the root does not matter.
- A schema with an empty `<RailwayInstallationDrawingScript>` does not exist for RailCOMPLETE: not in the drop-down,
  not for dimension copying, not for Lua lookups by name. A script of only whitespace or comments counts as non-empty:
  the schema is listed and then draws nothing.
- `Name` is the identity; with duplicate names the first one wins everywhere. Never name a schema `Intrinsic` (the
  built-in schema's identity, found first).
- The drop-down lists the built-in schema ("Intrinsic", localized) and then the DNA's schemas. A drawing whose export
  settings have never been saved uses **the first DNA schema in document order** as its selected schema — so the
  include order of your schema files decides which schema new drawings start with (and whose dimensions get copied
  onto newly inserted objects, §6).

## 2. Element tree

```
<RailwayInstallationSchemaContainer>
  <RailwayInstallationSchema Name="…" Description="…">
    <AppliesToAlignmentType>…</AppliesToAlignmentType>          0..n
    <RailwayInstallationDrawingScript>lua</…>                     required in practice
    <Rails>                                                       0..1, always drawn
      <Section2DItemWrapper …>…</Section2DItemWrapper>            0..n
      <PointOfInterest><Name>…</Name><Formula>lua</Formula></PointOfInterest>   0..n (avoid, see §5)
    </Rails>
    <Sleepers>                                                    0..1, always drawn
      <Section2DItemWrapper …>…</Section2DItemWrapper>            0..n
    </Sleepers>
    <Gauges> | <CatenaryOutlines> | <DiggingEdges> | <RailAnnotations> | <CustomComponents>   each 0..1
      <ComponentSubcategory Name="…" Description="…">             0..n
        <PresentationComponent Name="…" Description="…">          0..n  (one on/off toggle each)
          <Section2DItemWrapper …>…</Section2DItemWrapper>        0..n
        </PresentationComponent>
      </ComponentSubcategory>
      <BooleanParameter Name="…" Description="…">                 0..n  (Gauges/CatenaryOutlines only, see §4)
        <ApplicableSubcategory>subcategory name</ApplicableSubcategory>   1..n (0 = never shown)
      </BooleanParameter>
    </…>
    <DefaultDimensions DimensionStyle="…">                        0..1
      <Dimension Name="…" PointOfInterestA="…" PointOfInterestB="…" Orientation="Horizontal|Vertical" Offset="…">
        <AppliesToObjectTypes><AppliesToObjectType>rctype_…</AppliesToObjectType></AppliesToObjectTypes>  0..1
        <PerspectiveType>rctype_…</PerspectiveType>              0..1
        <SetValueScript>lua</SetValueScript>                      0..1
      </Dimension>
    </DefaultDimensions>
  </RailwayInstallationSchema>
</RailwayInstallationSchemaContainer>

<Section2DItemWrapper Side="Both|Left|Right">   exactly one of <Polyline2D/>, <Annotation2D/>, <Hatch2D/>, then <Formula>
```

### Attributes of the schema

| Member | Form | Notes |
|---|---|---|
| `Name` | attribute | The schema's identity. It is stored in drawings (selected schema, saved settings) — renaming a shipped schema orphans users' saved settings. Also the argument of `getRailwayInstallationDrawingSelectionData(name)`. |
| `Description` | attribute | Tooltip of the schema drop-down in the export window. |
| `AppliesToAlignmentType` | repeated element | Which alignments get rails, sleepers and components. Matches the alignment type's `Name` (the `ObjectType/@Name` verbatim, case-sensitive), **or** (case-insensitive) its `DataType`, or the name of its railML data class or any base class: `eTrack`/`tTrack` = every track type in any DNA; `tElementWithAlignment` = every non-track alignment; `tElementWithIDAndName` = every alignment. The `LuaName` (`rctype_…`) and the ObjectType's `Class` attribute do **not** match. Empty = all alignments, including roads and cable routes. Has no effect on which *objects* can be exported. |

## 3. Section2D items and their formulas

A `Section2DItemWrapper` holds one 2D item and a Lua `<Formula>` that computes its geometry. The formula is evaluated
**once per applicable track in the frame**.

### Formula context (Rails, Sleepers, every PresentationComponent)

| Global | What it is |
|---|---|
| `this` | **the alignment (track), not the object being drawn.** The object is not passed in. For object-dependent geometry (its type, height, variant) choose: (a) draw it in the drawing script — shown in the export preview and on the sheet, **never in the Cross Section Viewer**, which does not run the script; (b) put it in the object's 3D model; or (c) find the object from the track position in the formula — no shipped DNA does this yet, so confirm once: `local here = getAlignmentInfo(_position).Point` (the point on this track at the object's station), then `getNearbyPointObjects2D(rctype_X, here, false, radius)`, keeping objects with `o.Alignment ~= nil and o.Alignment.id == _alignmentSnapshot.Id and math.abs(getAlignmentInfo(o).DistanceAlong - _alignmentSnapshot.PointObjectDistanceAlong) < tolerance`. `radius` must exceed the object's lateral offset; only visible objects with X, Y and Z are returned; if `here` is nil the search runs along the whole track. The formula runs once per track in the frame and, like every formula, is stopped after the Lua time limit (5 s by default). (c) shows in the viewer, preview and sheet alike but selects by position, so two candidates at one station are ambiguous. If the user must see the result while editing in the viewer, (a) is the wrong choice. |
| `_alignmentSnapshot` | `AlignmentGauge` (m, inner-rail distance), `PointObjectDistanceAlong` (m, the object's station on this track), `Radius` (m, **signed**: + = left-hand curve, `math.huge` on straights), `Cant` (**mm**, signed), `DesignSpeed` (km/h), `VerticalProfileRadius` (m, `math.huge` if none), `Name`, `Id`. Values may be the user's overrides from the export window. |
| `_position` | the position on the track at `PointObjectDistanceAlong` (`.Pos`) |
| DNA `<LuaFunction>`s and RC functions | all callable, e.g. `RC_ApplyLiftAndRotate`, `RC_GaugeContourG1…GC`, `getCurrentInstallationDrawingSchemaParameters` |

**Coordinates**: 2D in the track's cross-section plane. X = lateral from the track axis, **+ to the right** looking in
increasing mileage; Y = up from the rolling plane (uncanted). Author as if looking in increasing mileage — RailCOMPLETE
mirrors X itself when the frame looks the other way.

**Cant** is not applied for you. Pass every point through
`RC_ApplyLiftAndRotate(this, _alignmentSnapshot.PointObjectDistanceAlong, point, _alignmentSnapshot.Cant)` so the
export window's cant override is honoured. (`Parent="CCS"` applies the track's real cant instead and ignores the
override — prefer the default `Parent="ACS"` plus `RC_ApplyLiftAndRotate`.)

**Side** (`Left`/`Right`/`Both`, default `Both`): shows the item only when the drawn object stands on that side of the
track, looking in increasing mileage. For the object's own track that is its real side (the same test as `RightSided`).
For every other track in the frame nothing is measured: the object is assumed to be on the opposite side, as if it
stood between the two tracks. The frame direction does not change the test. An object without a reference track shows
every item. Ignored inside `<Rails>` and `<Sleepers>`.

**Errors are swallowed twice**: `table.select`, `table.where` and `table.firstOrNil` call their function under
`pcall`, so a vertex whose `RC_ApplyLiftAndRotate` call fails is silently dropped (a malformed polyline, no message);
and a formula that fails outright only writes to RailCOMPLETE's log file.

### Item types and return contracts

| Item | Formula must return | Useful attributes |
|---|---|---|
| `<Polyline2D>` | a table of `getPoint3D(x, y)` (straight segments only — no arcs; approximate circles with vertices) | `Name`, `Closed="true"`, `Layer`, `Color` (e.g. `ByLayer`, ACI `1`, `#RRGGBB`), `Linetype` (must exist in the DNA's StyleDefinitions or acad.lin), `LinetypeScale`, `Lineweight`, `Transparency`, `Parent` (`ACS` default / `CCS`) |
| `<Annotation2D>` | `{content, getPoint3D(x, y), rotationDegrees}` — content is MText (`\P` new line) | `Name`, `Justify` (`TopLeft` … `BottomRight`, default `BottomCenter`), `TextHeight` (0.2), `WidthFactor`, `Style`, `Layer`, `Color`, `RotationBeforeOffset` |
| `<Hatch2D>` | (not evaluated) | **Unusable in a schema**: its `HatchParentId` is looked up among the *track type's* own 2D overlays, not the schema's polylines, so it never finds a schema outline. Draw hatches in the drawing script instead, or use a closed `Polyline2D`. |

Errors: a formula that fails or returns the wrong type skips that one item and writes only to RailCOMPLETE's log file —
nothing appears on the command line or in the preview. If a component "does not show", suspect the formula first.
A wrapper with no recognised child element can break the whole object's drawing.

## 4. Component categories, toggles and parameters

| Category | Intended content | Default when first seen | Export-window control |
|---|---|---|---|
| `Rails`, `Sleepers` | rail and sleeper profiles | always drawn | none |
| `Gauges` | structure gauges, train outlines. Each component's polyline publishes `Left`/`Right` extreme points usable by dimensions (`OwnAlignment_<subcategory>_<component>_Left`) | **off** | one drop-down per subcategory (header = subcategory Name, items = component Names), plus its BooleanParameters |
| `CatenaryOutlines` | catenary/pantograph outlines, wire markers | **off** | as Gauges |
| `DiggingEdges` | excavation limits | on | flat check boxes (component Names; subcategory names not shown) |
| `RailAnnotations` | helper lines from track points | on | flat check boxes |
| `CustomComponents` | anything else | on | flat check boxes |

- A `PresentationComponent`'s `Name` is its label and its toggle key (keep it unique within its category);
  `Description` is the tooltip. Labels are shown as written — there is no translation layer, so write them in the
  users' language.
- The drawing script receives each category as its own list of `(entity, itemName, componentName)` tuples
  (`data.Gauges`, `data.CustomComponents`, …) and all of them merged in `data.AllEntities`.
- A category with no `ComponentSubcategory` shows nothing in the export window.
- **BooleanParameter** — a check box labelled with its `Name` (tooltip = `Description`), in the export window and in
  the viewer's *Edit settings* window. It appears only under `Gauges` and `CatenaryOutlines`, and **only while at least one component of
  one of its `ApplicableSubcategory`s is ticked**; with no `ApplicableSubcategory` it is never shown (it still exists
  and keeps its value). In the other three categories a parameter is stored but fixed at `true`. `DefaultValue` is
  **ignored**: a new parameter starts at the category default (off for Gauges/CatenaryOutlines).
- Read parameters from any Lua context with `getCurrentInstallationDrawingSchemaParameters("Gauges")["My parameter"]`
  (→ true/false; the category name is case-sensitive and anything else throws). It reads the drawing's **saved**
  settings of the **drawing's selected schema** — not unsaved toggles, not a per-object schema choice made in the
  export window. After a user toggles a parameter they must press **Save settings**, and drawings the preview has
  already rendered stay cached until the window is reopened.
- Component on/off states are saved per drawing, per schema name, with Save; Export uses the unsaved component edits
  (but, as above, not unsaved parameter edits).

If you need a user option in a category other than Gauges/CatenaryOutlines, model it as a **flag component**: a
component whose only item is an invisible marker, e.g. a `<Polyline2D>` whose formula returns
`{getPoint3D(0, 0), getPoint3D(0.001, 0)}`, and in the script test
`table.firstOrNil(table.select(data.CustomComponents), function(x) return x.Item3 == "<component Name>" end) ~= nil`
(`Item3` = component Name, `Item2` = item Name). The marker exists only when at least one applicable track is in the
frame, so without a track the option reads as off. Components in DiggingEdges, RailAnnotations and CustomComponents
start **on** in every existing drawing, so a new one changes every user's next export — say so in the release note.

## 5. Points of interest

Dimension endpoints and script anchors. Names are `<owner>_<point>`:

| Owner | Points |
|---|---|
| `My` (the object) | `InsertionPoint`; bounding-box points of its projected 3D model `TopLeft`, `TopCenter`, `TopRight`, `MiddleLeft`, `MiddleCenter`, `MiddleRight`, `BottomLeft`, `BottomCenter`, `BottomRight`; `3DGeometry-<name>` from marker blocks in its 3D DWG (see cross-section-frames.md §6) |
| `OwnAlignment` (the object's own track) | `CenterLine`, `LeftInnerRail`, `RightInnerRail`, `NearestRail` (inner rail edge nearest the object), `LowestRail` (the lower inner rail) — at rolling-plane height, with cant |
| another track in the frame | same points, owner = the alignment's id (use `OwnAlignment_*` in dimensions) |
| gauge components | `OwnAlignment_<subcategory>_<component>_Left` / `_Right` |
| a directly related object in the frame | the relation key = the relation's prompt as seen from the host (its `RelatesTo` prompt + a space + the target spaces, or its `ReverseRelatesTo` prompt + a space + the source spaces; several spaces joined with `/`), e.g. `Est support_signal pour signal_classique_sur_nacelle_BottomCenter`. A second or third object under the same key gets `_2`, `_3` on the key, before the point: `…signal_classique_sur_nacelle_2_BottomCenter`, numbered in the order RailCOMPLETE finds them. Prefer subject-bound templates, where `My_*` is the related object and `Host_*` the central one |
| an unrelated object in the frame | empty owner — all unrelated objects collide, do not use |

There is **no XML or Lua way to declare a point on an object**; only the automatic points and 3D-model marker blocks.
Names are matched exactly (case-sensitive). Object and marker points are published whatever the frame box — only
objects and tracks are tested against the box.
`<Rails><PointOfInterest>` exists but its point is not transformed into drawing space; no production DNA uses it.
(If you do: `Name` is a child element, `<PointOfInterest><Name>x</Name><Formula>…</Formula></PointOfInterest>`. The
formula runs once per applicable track with `this` = track and `_position`, but no `_alignmentSnapshot` and no
object; it must return one `getPoint3D(x, y)`; the point is published under its bare `Name`, once per track.)

## 6. Dimensions

### Template attributes

| Member | Meaning |
|---|---|
| `Name` | Needed for "Restore missing dimensions from the DNA" and required (unique) for subject-bound templates. Shown to users. |
| `PointOfInterestA` / `B` | From / to point names (§5). The offset is measured from A. |
| `Orientation` | `Horizontal` (measures X difference) or `Vertical` (measures Y difference) |
| `Offset` | metres from point A to the dimension line: along display Y for `Horizontal` (+ = up), along display X for `Vertical` (+ = to the viewer's right). The export and its preview use the sign as stored; the Cross Section Viewer flips **vertical** dimensions of left-sided objects to the other side, so the viewer and the sheet can disagree on the side (`Offset="0"` avoids that) |
| `AppliesToObjectTypes` | LuaNames (`rctype_…`) of object types that get this dimension; empty = all. Note the wrapper element. |
| `PerspectiveType` | Makes it a **subject-bound** template: one instance per related object of this LuaName (e.g. each nacelle signal on a portal). `My_*` then means the subject, `Host_*` the central object. |
| `SetValueScript` | Lua run when the user edits the value in the Cross Section Viewer. **The dimension is editable only if the script text contains `_dimensionValue`.** |
| `DefaultDimensions/@DimensionStyle` | Name of a `<DimensionStyle>` in `<StyleDefinitions>`. Must have **`Dimlfac` 1000** (drawing in metres, labels in mm) — otherwise a value typed as `1850` reaches the script as 1850 m. |

### Lifecycle — dimensions belong to objects, not to drawing types

- Plain templates are **copied onto an object when it is inserted** — every field, **including the SetValueScript
  text and the style name** — from the schema *selected in that drawing's export settings* (the first DNA schema if
  none was ever saved), filtered by `AppliesToObjectTypes`. Nothing re-copies them afterwards: **editing a dimension or
  its SetValueScript in the DNA does not change objects already placed.** To pick up DNA changes, delete the
  dimension on the object and use **Restore missing dimensions from the DNA** (viewer canvas menu: uses the drawing's
  selected schema; export preview: uses that drawing's schema, saved only when the user exports), or re-insert the
  object. There is no batch restore: one object at a time in the viewer, one drawing at a time in the preview.
- Every stored dimension of an object is resolved in every schema's drawing; `AppliesToObjectTypes` is not re-checked
  at that point. A dimension whose points do not resolve is dropped with a warning (preview status Warning,
  command-line message on export).
- **Dimension `Name`s must be unique across all schemas of the DNA, and differ from the built-in schema's
  `Lateral offset`, `Insertion height`, `Top height` and `Cant`** (objects inserted while the built-in schema was the
  drawing's schema already carry those): restoring and "already materialized" bookkeeping match by name only. Prefix
  names per drawing type (`PN_…`, `PT_…`).
- Subject-bound (`PerspectiveType`) templates are never copied at insertion. The viewer materializes a persistent
  instance per directly related subject each time it loads (and records it so a user's deletion sticks); the export
  and preview add transient instances (yellow proposals) for not-yet-materialized pairs, which RailCOMPLETE does not
  place on the exported sheet.
- Labels are whole millimetres of the metre distance.
- Viewer colours: **green-yellow with framed text = editable**; yellow semi-transparent in the export preview = a
  proposed template instance; others = dimension-style colour.

### SetValueScript context

| Global | Meaning |
|---|---|
| `this` | the object to change: the subject for subject-bound dimensions, else the object selected in the viewer. Its properties are also bare globals (`LateralOffset`, `RightSided`, `Alignment`, …). |
| `_dimensionValue` | the new value in **metres** (typed millimetres ÷ the style's Dimlfac; drags pass metres directly) |
| `_previousDimensionValue` | the previous value in metres |
| `_alignmentSnapshots` | snapshots keyed by alignment id: `_alignmentSnapshots[this.Alignment.id]` — can be nil, check it. They are the snapshots of the object open in the viewer (the host); for a subject-bound dimension the subject's track may be missing |

- **Every `this.Prop = value` assignment removes that property's own formula, sets the value and saves the object
  immediately.** Clear (`this.Other = "="`) only a formula on *another* property that would recompute `Prop` — e.g.
  `DistanceToAlignment` before setting `LateralOffset`. A bare `Prop = value` (without `this.`) changes nothing.
- Validate and compute everything first, assign last: if the script raises an error after an assignment, the earlier
  assignments stay saved (only AutoCAD UNDO reverts them).
- On an error the user sees only RailCOMPLETE's generic line ("The dimension value was not applied: the script that
  sets it reported an error…"; French "La valeur de cote n'a pas été appliquée…"); the Lua error text goes to
  RailCOMPLETE's log file. To tell the user why a value is refused, write the reason to the command line
  (`pcall(function() DocumentData.Document.Editor:WriteMessage("\n" .. msg) end)`) and then `return` without assigning.
- Return value ignored. The script that runs is the copy stored on the object's dimension (see lifecycle above).
- Only the Cross Section Viewer runs SetValueScripts; the export preview does not.

Reference recipe (copy the logic, give it your own prefixed name): lateral offset from the nearest inner rail with cant
correction — see `EX_LateralOffset` in example-schema.xml.

## 7. XML embedding of Lua

- Escape `<` and `&` as `&lt;` and `&amp;` inside `<Formula>`, `<SetValueScript>` and the drawing script (`>` may
  stay; `&gt;` is harmless), or wrap the Lua in a raw `<![CDATA[ … ]]>` section — XPPq passes it through unchanged (the
  built-in schema and FR-SR's catenary files use it). Do **not** use `<xpp:cdata>` around escaped Lua: it wraps the
  text as it stands, so the Lua receives `&lt;` literally. A raw `<` breaks the whole DNA with a confusing parser
  message.
- An XML comment cannot contain `--`: never comment out a `<Dimension>` or formula that holds Lua comments with
  `<!-- -->`.
- XmlSerializer booleans are lower case: `true`/`false`.
