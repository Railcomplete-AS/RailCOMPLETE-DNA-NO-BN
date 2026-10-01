# Cross section frames

A cross section frame is the "camera box" stored on each placed object. Everything an installation drawing shows
of the 3D world comes through one frame of one object: the objects projected, the tracks the schema draws rails and
gauges for, and the points of interest that dimensions hang on. No frame, no drawing: the export skips an object
that has no frame.

## 1. Geometry: what the numbers mean

The frame is a box in the object's **local** coordinate system:

| Axis | Direction | Origin |
|---|---|---|
| +Y | along the object's reference alignment, towards **increasing mileage** | the object's insertion point (Z included) |
| +X | to the right, seen looking along +Y | |
| +Z | up | Z bounds are relative to the object's insertion **elevation** |

```
            plan view (Rotation = 0 0 0)                      what the drawing shows
                                                         (view from the cut plane towards +Y)
   MaxBound.Y  +----------------------+
               |      visible depth   |                    display Y = local Z (up)
   insertion   |           o          |                         ^
   point       |                      |                         |
   MinBound.Y  +======================+  <- cut plane          -+--> display X = local X
             MinBound.X          MaxBound.X                (only differences are meaningful;
               ---- track direction (+Y) --->               there is no fixed absolute origin)
```

- **MinimumBound.Y is the cut plane**: the viewer stands there and looks towards MaximumBound.Y. Everything between
  the two Y bounds is projected as elevation. With Y from −2 to +2, the object sits 2 m behind the cut plane.
- **X** = horizontal extent of the drawing (Min.X left, Max.X right as the viewer sees it). **Z** = vertical extent
  below/above the insertion elevation.
- Keep Min < Max on every axis. Inverted bounds are read as a half turn by some code paths but make the track search
  find **no tracks**. For a reversed view use `Rotation.Z = 180`.
- The frame follows the **alignment tangent, not the object's `dir`**. A down-facing object still looks towards
  increasing mileage unless you add 180° yourself.
- An object with no reference alignment orients its frame by its block rotation.

**Rotation** = three angles in degrees, applied in `RotationOrder` (default `ZXY`) around `RotationCenter`
(default `InsertionPoint`):

| Angle | Effect |
|---|---|
| `Rotation.Z` (yaw) | turns the view horizontally. **±90 turns the view plane parallel to the track** (front elevation of equipment beside the track). +90: looks to the left of the track direction; −90: looks to the right (checked against the code and FR-SR's convention, not yet in AutoCAD); 180: looks towards decreasing mileage. |
| `Rotation.Y` (roll) | rotates the drawing in its own plane |
| `Rotation.X` (pitch) | tilts the view; 90 gives plan-like views (untested by any DNA) |

`RotationCenter = BoxCenter` turns the box in place. With `InsertionPoint` an asymmetric box swings round the
insertion point, so a 180° frame needs its X bounds mirrored (the FR-SR DNA's potence frames do this).

## 2. Declaring frames in the DNA

Frames are declared per object type (or per variant) with a dynamic property. **Each `<DynamicProperty>` adds one
frame**; frame index = declaration order, object-type-level ones first, then the variant's.

```xml
<DynamicProperty Type="CrossSectionFrames" Subtype="CrossSectionFrame">
	<!-- Keys are relative to the frame. Literal values, DotLiquid templates, or "=<lua>" formulas. -->
	<SetValue Key="RotationCenter" Value="BoxCenter"/>
	<SetValue Key="Direction" Value="up"/>
	<SetValue Key="Annotation.Margin" Value="1.000"/>
	<!-- Lua formulas, evaluated in the HOST OBJECT's context (LateralOffset, dir, name, RightSided, relations...). -->
	<LuaExpression Name="Name"><Formula>name</Formula></LuaExpression>
	<LuaExpression Name="MinimumBound"><Formula>"-4 -2 -2"</Formula></LuaExpression>   <!-- whole point: "x y z" string or {x, y, z} -->
	<LuaExpression Name="MaximumBound.Z"><Formula>10</Formula></LuaExpression>         <!-- one component -->
	<LuaExpression Name="Rotation.Z"><Formula>dir == "down" and 180 or 0</Formula></LuaExpression>
</DynamicProperty>
```

| Key | Type / values | Default | Notes |
|---|---|---|---|
| `Name` | string | "" | Shown in the viewer's frame list and the palette; the sheet name is usually derived from the object name, not this |
| `Frozen` | bool | false | Frozen frames show nothing in the viewer and give a degraded export. Do not freeze frames meant for drawings |
| `MinimumBound`, `MaximumBound` | point, metres | (−4, −1, −1), (4, 1, 7) | See §1 |
| `Rotation` | point, degrees | (0, 0, 0) | See §1 |
| `RotationCenter` | `BoxCenter` \| `InsertionPoint` | `InsertionPoint` | |
| `RotationOrder` | `XYZ` `XZY` `YXZ` `YZX` `ZXY` `ZYX` | `ZXY` | |
| `Direction` | `up` \| `down` \| `both` (lower case) | `both` | Which export direction picks this frame (§5) |
| `AlignmentDisplayOption` | `Adaptive` \| `All` \| `Representation3D` \| `CrossSectionOverlay` | `Adaptive` | Keep `Adaptive`: it is the only option with which the viewer uses the installation-drawing pipeline |
| `IncludeObjects`, `ExcludeObjects` | object references | empty | Force objects in or out regardless of position |
| `Annotation.*` (`Contents`, `Margin`, `Justify`, `TextHeight`, `Style`, `Rotation`) | | | Only used by the grid export RC-EXPORT2DPROJECTIONS, not by installation drawings |

Silent failures: an **unknown `SetValue` key is ignored without a message**; an unknown `Subtype` prints a message and
adds nothing; an unknown `Type` throws. Spell keys exactly as above.

Legacy spelling `Type="Projection2DCollection" Subtype="Projection2D"` still loads (it becomes a cross section frame).
Use the new spelling in new code.

### Put the frame logic in named `<LuaFunction>`s

The formula text is copied onto each object **when it is inserted**. A formula that only calls a named DNA function
(`<Formula>MYDNA_getFrameMinimumBound()</Formula>`) keeps improving when you ship a better function; a formula with
the logic inlined is frozen on every existing object. The FR-SR DNA's frame macros follow this pattern.

## 3. Existing objects do not get new frames

Frames (and their formulas) are created only when an object is **inserted**. After adding or changing a frame in the
DNA, objects already in drawings keep what they had. Tell users how to retrofit:

- **`_RC-MatchDynamicProperties`** (the reliable route): pick a freshly inserted object as the source, then the
  existing objects. It copies the source object's frames and their formulas (not the DNA's). By default it copies
  **every** dynamic property type — untick everything but cross section frames in its *Settings* — and its default
  mode **Prepend** puts the copies in front, shifting existing frames (frame 0 is the Default frame; the first
  non-frozen frame, normally frame 0, decides the tracks). Use *Replace* to swap the frames out.
- Properties palette, category **Cross sections**, **Add**: appends one frame with RailCOMPLETE's **built-in defaults,
  not the DNA's values** — only useful for manual experiments.
- Delete and re-insert the objects.

## 4. What a frame projection contains

- **Objects**: the host object always (even if hidden or excluded), plus every *visible* placed object whose
  **insertion point** lies inside the box's plan footprint (height is not tested, and the object's extent is not
  tested), plus visible `IncludeObjects`; `ExcludeObjects` wins over `IncludeObjects`. Plain AutoCAD entities that are
  not RailCOMPLETE objects are never projected.
- **Geometry**: each included object's 3D representation. Only **3D solids, surfaces, regions and bodies** survive;
  lines, polylines, 3D faces, **polyface meshes, polygon meshes and subdivision meshes** are dropped. All solids of all
  objects in the box are merged into one set. An object without a 3D representation contributes no geometry.
- **Check the 3D models before designing a drawing around them.** Many existing library models are meshes and
  therefore invisible in every projection (in FR-SR, for example, the telephone model is entirely polyface mesh and
  the mast of the signal on a mast is almost entirely mesh). Their nine bounding-box points still exist, so dimensions
  to `My_TopCenter` etc. resolve even though nothing is drawn. Fix: remodel or convert the model to 3D solids in
  AutoCAD (select the object, the Properties palette names its type: "3D Solid" is good, "Polyface Mesh" / "Mesh" is
  not).
- **Tracks for the schema** (rails, sleepers, components): alignments whose closest point to the object lies inside the
  frame's X/Y rectangle **and** whose elevation relative to the object is inside Min.Z…Max.Z, filtered by the schema's
  `AppliesToAlignmentType`. These are computed from the object's **first non-frozen frame**, whichever frame is being
  exported. Gotchas: the object's elevation includes its `VerticalOffset` — an object mounted 8 m above the track
  needs `Min.Z` ≤ −8 minus a margin; objects inserted at Z = 0 next to tracks with real elevations fail the Z test
  too; and a track needs a perpendicular projection of the object (an object beyond the end of a track gets no rails
  for it). An object with no elevation passes the Z test. For tracks, `IncludeObjects` wins over `ExcludeObjects`.
- **Points of interest**: see §6.
- **Point clouds** inside the frame come as a raster backdrop.

No track in the box is not an error: you still get the object's projection and its `My_*` points, but no rails, no
gauges, no `OwnAlignment_*` points (dimensions using them are dropped with a warning) and an empty
`AlignmentSnapshots` list in the drawing script.

## 5. How the export picks a frame

The export window has a direction picker: **Default (first frame) / Up / Down**.

- Default → frame index 0.
- Up/Down → the first frame whose `Direction` is exactly that, else the first frame whose `Direction` is neither up
  nor down, else the object is skipped ("no frame for direction").
- **Exactly one frame per object per run.** The drawing script only learns the chosen frame's direction
  (`data.FrameDirection`), not its name or index. A drawing that needs two views of one object (e.g. face + side, or
  up + down on one sheet) cannot get them in one run.
- Dimensions are not tied to frames: every stored dimension of the object is resolved in whichever frame is exported
  (subject-bound dimensions pointing the opposite way of an up/down frame are dropped).

## 6. Points of interest from objects

| Point | Name to use in a dimension |
|---|---|
| Insertion point | `My_InsertionPoint` |
| Corners/edges of the projected 3D model's bounding box | `My_TopLeft`, `My_TopCenter`, `My_TopRight`, `My_MiddleLeft`, `My_MiddleCenter`, `My_MiddleRight`, `My_BottomLeft`, `My_BottomCenter`, `My_BottomRight` |
| Named point in the 3D model | `My_3DGeometry-<name>` |
| Same points on a **directly related** object inside the frame | `<relation key>_<point>`; the key is the relation's prompt as seen from the host plus a space and the other side's space name(s), joined with `/`, e.g. `Est support_signal pour signal_classique_sur_nacelle_BottomCenter`; a second object under the same key gets `_2` on the key, before the point (`…nacelle_2_BottomCenter`) |
| Own track | `OwnAlignment_CenterLine`, `OwnAlignment_LeftInnerRail`, `OwnAlignment_RightInnerRail`, `OwnAlignment_NearestRail`, `OwnAlignment_LowestRail` |
| Another track crossing the frame | `<alignment id>_CenterLine` etc. (ids are per drawing — not usable in DNA templates) |

Names are matched exactly (case-sensitive); the first match wins. Unrelated objects in the frame all publish their
points under the same empty key, so they cannot be told apart. **To dimension to a neighbour, relate it to the host
object** — or, better, write a subject-bound template (`PerspectiveType`), where `My_*` means the related object and
`Host_*` the central one, so you never type relation keys.

### Adding a named point to a 3D model (the only way to declare an object point)

There is no XML or Lua way to declare an object point. It is authored in the 3D model DWG that the object type's
3D representation references:

1. Open the 3D model DWG in AutoCAD.
2. Insert a small block reference **at the top level of model space** (not nested inside another block), on layer
   **`RC_PointsOfInterest`**.
3. Give the block an attribute with tag **`POINTOFINTEREST_NAME`**; its value is the point name, e.g. `CentreCaisson`.
4. Save. The point is now `3DGeometry-CentreCaisson`, i.e. `My_3DGeometry-CentreCaisson` in a dimension.

**A point that must follow a property** (e.g. the bottom of a foundation whose depth is a property) cannot live in an
existing model. Add a `Geometry3D` slot to the object type whose offset is a formula of that property and whose model
is a new marker-only DWG (one marker block, nothing else). You cannot create DWG files: name the new file (folder,
file name, marker name, layer, attribute) as a deliverable the user must author, and make the slot's `Name` formula
return `""` until it exists. A `Geometry3D` whose file is missing breaks that object's 3D build, and every dimension on
the point is dropped with a warning. Like frames, a new slot reaches only objects inserted afterwards.

The marker block (and anything else on that layer) is removed from the drawn geometry. Marker points are published
whatever the frame box (only objects and tracks are box-tested). Budget for it: adding two points
to one structure family in FR-SR meant editing 138 DWG files.

## 7. Recipes

**Cross section of a wayside object** (the standard installation drawing). Box reaches from behind the object to past
the track, ±2 m deep, from 2 m below to 10 m above the insertion point:

```xml
<LuaFunction Name="MYDNA_getWaysideFrameMinimumBound()" ReturnType="String">
	<Signature>String MYDNA_getWaysideFrameMinimumBound()</Signature>
	<Formula>
function MYDNA_getWaysideFrameMinimumBound()
	local lateralOffset = LateralOffset or 0
	if lateralOffset ~= lateralOffset then lateralOffset = 0 end
	local towardsTrack = math.abs(lateralOffset) + 3.5
	return (lateralOffset >= 0 and -towardsTrack or -3.5) .. " -2 -2"
end
	</Formula>
</LuaFunction>
<!-- ...and a matching MaximumBound function, then on the object type: -->
<DynamicProperty Type="CrossSectionFrames" Subtype="CrossSectionFrame">
	<LuaExpression Name="Name"><Formula>name</Formula></LuaExpression>
	<SetValue Key="RotationCenter" Value="BoxCenter"/>
	<LuaExpression Name="Rotation.Z"><Formula>dir == "down" and 180 or 0</Formula></LuaExpression>
	<LuaExpression Name="MinimumBound"><Formula>MYDNA_getWaysideFrameMinimumBound()</Formula></LuaExpression>
	<LuaExpression Name="MaximumBound"><Formula>MYDNA_getWaysideFrameMaximumBound()</Formula></LuaExpression>
</DynamicProperty>
```

**One frame per traffic direction** (objects serving both directions): declare two frames, `Direction=up` with
`Rotation.Z = 0` and `Direction=down` with `Rotation.Z = 180`. Frame 0 is the up frame and decides the tracks for both.

**Front elevation of equipment beside the track** (cabinet face, pole with boxes, level-crossing light): yaw the view
so it looks at the equipment from the track.

```xml
<DynamicProperty Type="CrossSectionFrames" Subtype="CrossSectionFrame">
	<LuaExpression Name="Name"><Formula>name .. " - face"</Formula></LuaExpression>
	<SetValue Key="RotationCenter" Value="BoxCenter"/>
	<!-- Object right of the track: look to the right (-90). Left of the track: +90. -->
	<LuaExpression Name="Rotation.Z"><Formula>(LateralOffset or 0) >= 0 and -90 or 90</Formula></LuaExpression>
	<!-- After the yaw: X = along the track (drawing width), Y = depth towards/away from the track, Z = height. -->
	<SetValue Key="MinimumBound" Value="-1.5 -1 -0.5"/>
	<SetValue Key="MaximumBound" Value="1.5 1 3"/>
</DynamicProperty>
```

**Reaching the face frame at export**: Default always exports frame 0; Up/Down export the first frame whose
`Direction` is exactly `up`/`down`. So either make the face frame frame 0 (of the object type, or of a dedicated
variant), or give it `Direction` `up` while the frames before it keep `both`, and tell users to pick Up. Frame 0 still
decides the tracks.

Caveats for yawed frames: if the track falls inside the box, the schema draws rails and gauges as if it were a cross
section, laid into the wrong plane. Keep the track out of the box, or use a dedicated schema without rail/gauge
components for this drawing type. Lateral distances to rails are meaningless in this view; heights above
`OwnAlignment_LowestRail` still work if the track is in the box. The drawing script must not require a track.
Remember that the first non-frozen frame (normally frame 0) decides the tracks for every frame of the object.

**Object far from any track, object only**: a small box around the object (e.g. `-1 -1 -0.5` to `1 1 3`). The object
needs a solid 3D model; the script must tolerate an empty `AlignmentSnapshots`.

**Several items on one support** (boxes on a pole, plates on a frame): all objects whose insertion points fall in the
footprint are projected together. Dimensions to a specific item need a relation between the host (e.g. the support)
and the item. All projected solids reach the script as one merged, unlabelled set, so the script cannot style one item
differently from the others.

## 8. Where the user meets frames

| UI | What it shows / does |
|---|---|
| Properties palette, category **Cross sections** | one expandable row per frame (`0 Cross section frame`, …) with Name, Frozen, bounds, rotation, direction, include/exclude; **Add**; per-frame remove/copy/move/freeze (sun/snow icon); any field accepts `=formula` |
| Eye button on that category | opens the Cross Section Viewer for the selection |
| Right-click an object → **Open Cross Section Viewer** (there is no typed command and no ribbon button for it) | modeless viewer: object combobox, **frame combobox** (`<index>: <Name> (<Direction>)`, "(frozen)"), dimension editing, "Edit settings" |
| `_RC-Show2dProjectionBoxPreview` / `_RC-Show2dProjection3dSourcePreview` / `_RC-Show2dProjectionPreview` | transient in-drawing previews of the box, its 3D sources and the 2D result — the fastest way to debug bounds |
| Export window (ribbon: RailCOMPLETE tab → Assist panel slide-out → "Assist Create Installation Drawing..."; command `_RC-AssistCreateInstallationDrawing`) | direction picker Default/Up/Down picks the frame |

There are no grips for frames in the drawing.

## 9. Checklist

1. Frame declared on the object type (or variant), logic in named LuaFunctions.
2. Box reaches the track if the drawing needs rails/track points; Z range covers the track elevation.
3. `RotationCenter` explicit; reversed views by `Rotation.Z`, never by inverted bounds.
4. `AlignmentDisplayOption` left `Adaptive`, frame not frozen.
5. 3D models are solids/surfaces (not meshes — check each model); named points added where dimensions need them.
6. Neighbours you dimension to are related to the host.
7. Retrofit plan for existing drawings (`_RC-MatchDynamicProperties`).
8. Checked in the viewer with `_RC-Show2dProjectionBoxPreview` before writing the script.
