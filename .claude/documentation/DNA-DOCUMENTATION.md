# NO-BN DNA Source File Documentation

> **Version:** NO-BN 2026.1
> **Copyright:** 2015–2026 Railcomplete AS, Norway, NO916118503
> **License:** MIT — publicly available at [github.com/Railcomplete-AS/RailCOMPLETE-DNA-NO-BN](https://github.com/Railcomplete-AS/RailCOMPLETE-DNA-NO-BN)
> **Last updated:** 2026-03-04

---

## What is the DNA?

The **DNA (Definition of Network Assets)** is the configuration layer that makes RailCOMPLETE specific to Norway's railway system (Bane NOR). It defines:

- Every railway object type (signals, switches, OCS poles, balises, etc.)
- 2D drawing symbols and 3D models for each object
- Lua formulas that compute dynamic property values
- Model checks (automated validation rules)
- Object relationships (how objects are connected)
- Layer visibility groups
- Standard properties and text attributes

The DNA is consumed by the **RailCOMPLETE plugin for AutoCAD** (a separate C# codebase). Without a DNA, the plugin has no railway-specific content.

---

## Source File Location

All source files live in:
```
NO-BN\DNA\_SRC\
```

They are **preprocessed** by the XPPq tool and compiled into a single output file:
```
NO-BN\DNA\NO-BN-2026.1-DNA.xml
```

Source files use the `.xml` extension but are **not** standard XML — they contain XPPq preprocessor directives.

---

## The XPPq Preprocessor

XPPq (EPEIOS open-source tool) assembles the modular XML source files before they are loaded by RailCOMPLETE. Key directives:

| Directive | What it does |
|---|---|
| `<xpp:expand href="file.xml"/>` | Include another file's contents here |
| `<xpp:define name="MACRO">…</xpp:define>` | Define a reusable block of XML |
| `<xpp:expand select="MACRO"/>` | Insert a previously defined macro |
| `<xpp:set Name="VAR" value="VALUE"/>` | Set a preprocessor variable |
| `<xpp:ifeq select="VAR" value="V">…</xpp:ifeq>` | Conditional include |
| `<xpp:cdata>…</xpp:cdata>` | Wrap contents in a CDATA section |
| `<xpp:bloc>…</xpp:bloc>` | Group multiple elements (required in some macro contexts) |

The master file (`NO-BN-RootFile.xml`) includes all other partial files using `<xpp:expand href="…"/>`. This means you **edit the partial `_SRC` files**, never the compiled output.

---

## Complete File Index

### Entry Point

| File | Role |
|---|---|
| `NO-BN-RootFile.xml` | **Master assembly file.** Defines DNA identification, global settings (units, paths, fonts, train dimensions), and includes all other partial files in the correct order. Also contains the authoritative naming convention documentation. |

### Configuration & Framework Files

| File | Role |
|---|---|
| `NO-BN-StyleDefinitions.xml` | AutoCAD text styles, linetype definitions, dimension styles, and other drawing standards. |
| `NO-BN-ShowLayers.xml` | Defines named layer-visibility groups (used by the "Show Layers" command). Each group is a named set of layer patterns that can be toggled together. Covers all disciplines plus general layers (insertion points, symbol frames, etc.). |
| `NO-BN-GaugeHalfProfiles.xml` | Defines clearance gauge half-profiles for Norwegian railways. Used for sighting calculations and clearance checks. |
| `NO-BN-Relations.xml` | Declares all valid object-to-object relationship types (e.g., "Signal has BaliseGroup", "OCS Pole has Cantilever"). Each relation gets a forward and reverse Lua name constant. |
| `NO-BN-StandardProperties.xml` | Defines reusable XPPq macros for custom properties (discipline tag, earthing status, OCP area reference, etc.), and documents the three-tier property system (Intrinsic / Custom / Dynamic). |
| `NO-BN-StandardTextAttributes.xml` | Defines reusable XPPq macros for standard text attribute sets used across object types (code label, name label, mileage, content text, etc.). |
| `NO-BN-PositionToolSettings.xml` | Configures the on-screen position readout displayed while placing objects. Shows own and reference alignment name, mileage, distance-to-alignment, and elevation using Lua formatting functions. Implements ISO 19148:2012 linear referencing. |
| `NO-BN-Tables.xml` | Defines data tables (lookup tables referenced by Lua formulas). |
| `NO-BN-ModelChecks.xml` | Declares global model-check XPPq macros and reusable model-check Lua functions used by multiple object types (e.g., OCP area count check, distance-to-neighbour check). |

### Lua Libraries (Global Functions)

| File | Role |
|---|---|
| `NO-BN-General-Lua.xml` | **Generic RC functions** (prefix `RC__`). Contains functions that depend only on RailCOMPLETE intrinsics, not on any DNA content. Examples: `RC__COPYRIGHT_STATEMENT()`, `RC__DNA_VERSION()`, `RC__toInt()`, `RC__round()`, `RC__getAngleFromDir()`, `RC__flipAcsTuple()`, `RC__AcsToWcs()`, etc. These are candidates for promotion into the core C# API. |
| `NO-BN-National-Lua.xml` | **Norway/Bane NOR–specific shared functions** (prefix `NOBN_xxx_`). Contains functions shared across disciplines but specific to Norwegian conventions: mileage-increases-towards-left detection, layer/stage management, OCP code lookup, symbol frame state logic, label formatting. |
| `NO-BN-LuaCode-BasicCADFunctions.xml` | Lua wrappers for basic CAD geometry construction used in 3D model generation formulas (`RC__CAD().createPolyline()`, `.createCircle()`, `.createLine()`, etc.). |
| `NO-BN-LuaCode-Balloon.xml` | Lua code for generating balloon/callout annotation geometry. |
| `NO-BN-LuaCode-Cantilever.xml` | Lua code for OCS cantilever 3D geometry generation (arm angles, wire clamp positions, system height, stagger). |
| `NO-BN-LuaCode-Portal Beam.xml` | Lua code for OCS portal beam 3D geometry generation. |
| `NO-BN-LuaCode-Guy Wire.xml` | Lua code for guywire/tensioning-wire 3D geometry generation. |
| `_NO-BN-TemplateFunctionDeclaration.xml` | A blank-template file showing the correct XML structure for declaring a new `LuaFunction`. Use as a copy-paste starting point. |

### Object Definition Files — by Discipline

#### Common / General (FE — Felles)

| File | Contents |
|---|---|
| `NO-BN-CommonObjects.xml` | General-purpose objects used across disciplines: OCP areas, RcArea regions, annotation/label general types. |
| `NO-BN-Labels.xml` | Generic label object types (floating labels that can attach to any target object via the `rel_Label_AppliesTo_Anything` relation). |
| `NO-BN-Foulingpoint.xml` | **Fouling point** (sporvekselspiss / frisporingspunkt) — the critical track clearance point at a switch where two converging routes first become conflicted. |

#### Track & Substructure (KO/KU)

| File | Contents |
|---|---|
| `NO-BN-TrackConnections.xml` | Track connectivity objects: buffer stops, track endings, track continuation objects used to model where alignments start and terminate. |
| `NO-BN-TrackAndWaysideObjects.xml` | **Main track content:** Track alignments (`JBTKO_SPO Spor`), switch labels, horizontal and vertical geometry annotation object types, wayside markers, gradient and curvature annotations. Supports "live" geometry annotation (automated vertex/segment annotation objects along alignments). |
| `NO-BN-CivilWorks.xml` | **Substructure / civil engineering:** Cable ducts, foundations (OCS, signal, cabinet, telecom), tunnels, bridges, platforms, buildings, retaining walls. Includes `NOBN_sub_getFoundationCode()` which auto-derives a foundation's code from its associated structure. |

#### Overhead Catenary System / High Voltage (EH — Elektro Høyspent)

| File | Contents |
|---|---|
| `NO-BN-OcsWireSystem.xml` | **All wire-type alignments:** Contact wires, catenary/messenger wires, conductors, midpoint anchor lines, guywires, spanners. Also: wire change annotation objects, wire tension balancers (WTB), midpoint anchors, ground anchoring, tunnel wall fasteners, conductor continuations. |
| `NO-BN-OcsPoles.xml` | **OCS support poles:** Standard catenary poles (`JBTEH_MAS`), drop-arm poles under portals, drop-arm poles from bridge/tunnel ceilings. Contains `_JBTEH_MAS_getSpanlengths()` which computes 2D span lengths for each contact wire section at a pole. |
| `NO-BN-OcsCantilevers.xml` | **OCS cantilever assemblies:** Cantilever support brackets and individual cantilevers. Contains detailed geometry computation for contact wire clamp position (lateral offset/stagger, system height, top/lower console, registration arm). |
| `NO-BN-OcsSwitchesAndTransformers.xml` | **OCS sectioning and power equipment:** Section insulators, neutral sections, section switches, auto-transformers, power cables. |
| `NO-BN-OcsVariousObjects.xml` | **Miscellaneous OCS objects:** Earthing switches, OCS cabinets, lightning arresters, current collectors, and other catenary system accessories. |
| `NO-BN-Earthing.xml` | **Earthing system:** Earthing connections, transient earthing connectors, rail bonds. The global earthing block name for transient earthing is configured in the RootFile. |

#### Signalling (SA — Signalanlegg)

| File | Contents |
|---|---|
| `NO-BN-Signals.xml` | **Signal objects and Lua functions.** Main signals, distant signals, shunting signals, ERTMS marker boards, ERTMS electrification boards. Key Lua: `NOBN_sig_getSignalNumber()`, `NOBN_sig_getSignalLitra()`, `NOBN_sig_getSignalShortName()`, `NOBN_sig_getSignalFullName()`. |
| `NO-BN-SignallingObjects.xml` | **Non-signal signalling infrastructure:** Signal cabinets (apparatus, keylock, crank, cable), axle counters and their sensors, track circuits, treadles, block instruments. Key Lua: `NOBN_sig_getCabinetName()`. |
| `NO-BN-Balises.xml` | **Train detection — balise objects:** NSS balises and balise groups, ETCS balises and balise groups. Contains: `NOBN_sig_getBaliseGroupDirection()`, `NOBN_sig_chkBaliseDistanceToPreviousBalise()` (checks 2.35 m / 2.8 m / 3.2 m / 3.5 m / 10.5 m / 11.8 m distance thresholds per Bane NOR standard). |
| `NO-BN-Interlocking.xml` | **Interlocking synthesis configuration.** Defines filters for train route start/end points, shunting route start/end points, and via-points used by the RC interlocking export function. Uses signal type and variant to determine which objects are valid route endpoints. |
| `NO-BN-SignalSighting.xml` | **Signal sighting calculations.** Settings and Lua for computing whether a signal is visible from the driver's cab, based on track geometry and sighting distance rules. |

#### Telecommunications (TE — Telekommunikasjon)

| File | Contents |
|---|---|
| `NO-BN-Telecom.xml` | **Telecom objects:** Telecom racks (`JBTTE_RAC Telerack`), GSM-R antennas and equipment, radio blocks, intrusion detection, CCTV, PA systems, telephone posts. |

#### Low-Voltage Power (EL — Elektro Lavspent)

| File | Contents |
|---|---|
| `NO-BN-LowPower.xml` | **Low-voltage power supply objects:** Distribution boards, power outlets, lighting systems, power cables ≤1000V, UPS systems, battery chargers. |

#### Boards and Poles (SK — Skilt)

| File | Contents |
|---|---|
| `NO-BN-BoardsAndPoles.xml` | **Trackside signs and sign poles:** Speed boards, kilometre boards, warning boards, gradient boards, curve boards, general-purpose sign poles, signal notice boards. |

#### Gauges

| File | Contents |
|---|---|
| `NO-BN-GaugeHalfProfiles.xml` | Clearance gauge profiles used by the signal sighting and clearance commands. Defines the standard Norwegian 1435 mm (standard gauge) clearance envelope. |

---

## Core XML Elements Explained

### `<ObjectType>`

The main building block. Declares one kind of railway object.

```xml
<ObjectType DataType="tOrientedElement"
            Class="RailwayPlacedObject"
            LuaName="rctype_TelecomRack"
            Name="JBTTE_RAC Telerack"
            Layer="JBTTE_RAC"
            Color="ByLayer"
            Group="Tele/Stativer"
            AttMirrorY="{% if RightSided %}true{% else %}false{% endif %}">
```

| Attribute | Meaning |
|---|---|
| `DataType` | Geometry class: `tOrientedElement` (point, has direction), `tElementWithAlignment` (alignment/line), `RailCOMPLETELabel`, `tMarker`, `tDelimitedOrientedElement`, `RailCOMPLETESection` |
| `Class` | RC class: `RailwayPlacedObject` (most objects), `RailwayAlignment` (tracks, wires) |
| `LuaName` | The Lua constant name to refer to this type (e.g., `rctype_TelecomRack`) |
| `Name` | Full display name, formatted as `JBTXX_YYY Human readable name` |
| `Layer` | Default AutoCAD layer |
| `Color` | Default colour (usually `ByLayer`) |
| `Group` | Path in the object picker tree, using `/` as separator |

### `<Variants>`

Lists the selectable sub-types (variants) for an object. The user picks one variant when inserting the object. Each variant may have different 2D symbols.

```xml
<Variants DefaultValue="Seksjonsfelt annotering">
    <Variant Name="Seksjonsfelt annotering"/>
    <Variant Name="Vekslingsfelt annotering"/>
    <Variant Name="Sugefelt annotering"/>
    <Variant Name="Fritekst annotering"/>
</Variants>
```

### `<InsertPointObject>`

Defines one insertion mode for an object — how it snaps to alignments, what 2D block it shows, and what happens when it is placed.

```xml
<InsertPointObject Name="Telerack"
                   DisplayBlockName="NO-BN-2D-JBTTE_RAC-TELERACK-{{SymbolMode}}"
                   DefaultSnapMode="Alignment"
                   SnapToAlignment="true"
                   SnapDistance="4.0"
                   AskForAttachment="true">
    <OwnAlignmentTargetSpace>spor</OwnAlignmentTargetSpace>
    <JigSymbolAppearance RotateIfRightSideOfAlignment="true" AddAngle="0"/>
</InsertPointObject>
```

`{{SymbolMode}}` is a template placeholder that resolves to `Schematic` or `Geographic` at runtime.

### `<LuaFunction>`

Declares a named, reusable Lua function. Stored globally (not per-object).

```xml
<LuaFunction Name="NOBN_sig_getSignalNumber()"
             ReturnType="String"
             HideFromUser="false"
             Description="Returns the number extracted from the signal's Code.">
    <Signature>String NOBN_sig_getSignalNumber(tSignal s)</Signature>
    <Formula>
        function NOBN_sig_getSignalNumber(s)
            if s == nil then s = getObjectFromId(id) end
            -- implementation ...
        end
    </Formula>
</LuaFunction>
```

`HideFromUser="true"` suppresses the function from the user-facing Lua intellisense list (for internal helpers).

### `<LuaExpression>`

Binds a Lua formula to a specific property of an object type. Evaluated per-object when the property is read.

```xml
<!-- Simple property formula -->
<LuaExpression Name="name">
    <Formula>NOBN_sig_getSignalFullName()</Formula>
</LuaExpression>

<!-- Model check -->
<LuaExpression Name="mc_BaliseDistance" IsModelCheck="true">
    <Formula>NOBN_sig_chkBaliseDistanceToPreviousBalise()</Formula>
</LuaExpression>

<!-- 3D geometry binding -->
<LuaExpression Name="Geometry3D_0.Name">
    <Formula>"NO-BN-3D-JBTTE-TELERACK"</Formula>
</LuaExpression>
```

### `<CustomProperty>`

Declares a new property that doesn't exist in the base RailCOMPLETE product. Appears in the Property Manager.

```xml
<CustomProperty DataType="String" ReadOnly="true"
                Category="Model check"
                Name="mc_NumberOfOcpAreas"
                DisplayName="Antall OCP områder"
                Description="Number of OCP areas this object belongs to."/>
```

### `<Relations>` / `<Relation>`

Declares a valid connection type between two object types.

```xml
<Relations>
    <SourceSpace>kl-mast</SourceSpace>
    <Relation Name="Har utligger"
              ForwardLuaName="rel_OcsPole_Has_Cantilever"
              ReverseLuaName="rel_Cantilever_BelongsTo_OcsPole"
              Color="150">
        <TargetSpace>utligger</TargetSpace>
        <RelatesTo Prompt="Har utligger" Min="0" Max="inf"/>
        <ReverseRelatesTo Prompt="Tilhører mast" Min="0" Max="1"/>
    </Relation>
</Relations>
```

The `ForwardLuaName` becomes a Lua constant (e.g., `rel_OcsPole_Has_Cantilever`) usable in `getRelatedObjects()` calls.

### `<ShowLayer>`

Creates a named visibility group for the layer visibility command.

```xml
<ShowLayer Name="COM_ShowSymbolFramesAll"
           DisplayName="Generelt/Symbolrammer, alle typer"
           DefaultOff="false">
    <LayerSelection Type="PartialMatch" CaseSensitive="true">JBTRC$SYMBOLRAMMER_ERROR</LayerSelection>
    <LayerSelection Type="PartialMatch" CaseSensitive="true">JBTRC$SYMBOLRAMMER_WARNING</LayerSelection>
</ShowLayer>
```

---

## Discipline System

Every object name, layer, and Lua function prefix follows the discipline code system. The `JBT` prefix stands for "Jernbaneteknikk" (railway engineering).

| Discipline Code | Layer Prefix | Function Prefix | Norwegian | English |
|---|---|---|---|---|
| FE | `JBTFE_` | `NOBN_com_` | Felles | Common / General |
| KU | `JBTKU_` | `NOBN_sub_` | Underbygning | Substructure / Civil works |
| KO | `JBTKO_` | `NOBN_trk_` | Overbygning | Track superstructure |
| EH | `JBTEH_` | `NOBN_ocs_` | Elektro Høyspent | OCS / High-voltage |
| SA | `JBTSA_` | `NOBN_sig_` | Signalanlegg | Signalling |
| TE | `JBTTE_` | `NOBN_tel_` | Telekommunikasjon | Telecommunications |
| EL | `JBTEL_` | `NOBN_pow_` | Elektro Lavspent | Low-voltage power |
| SK | `JBTSK_` | `NOBN_bnp_` | Skilt | Boards and poles |
| RC | `JBTRC_` | `RC__` | (internal) | RailCOMPLETE internal |

**Object type name format:** `JBTXX_YYY Human readable name`
**2D symbol block name format:** `NO-BN-2D-JBTxx_DESCRIPTION-{{SymbolMode}}`

---

## Lua Naming Conventions

### Function Prefixes

| Prefix | Scope | Example |
|---|---|---|
| `RC__` (double underscore) | Generic — no DNA-specific content, no Norwegian text | `RC__round(x, 2)` |
| `NOBN_xxx_` | Norway/Bane NOR specific, discipline `xxx` | `NOBN_sig_getSignalNumber()` |
| `OBJECTTYPE_` | Global, shared by multiple related object types | `JBTEH_MAS_nonPortalPole_code()` |
| `_OBJECTTYPE_` (leading underscore) | Local to one object type | `_JBTEH_LBA_ocs_annotation_name()` |

### Function Name Patterns

| Pattern | Meaning |
|---|---|
| `get…` | Returns a computed value |
| `chk…` | Model check — returns `(message, objects, symbol)` |
| `set…` | Returns a value to be written into a property |
| `is…` / `has…` | Returns boolean |
| `UPPERCASE_NAME()` | "Constant" — a zero-argument function returning a fixed value |

### Property Name Patterns

| Pattern | Meaning |
|---|---|
| `camelCase` | railML-inherited property (e.g., `code`, `name`) |
| `PascalCase` | RC-specific or DNA-declared property (e.g., `Spanlength`, `RightSided`) |
| `mc_PascalCase` | Model check result property |

---

## Model Checks

A model check is a property + formula pair that validates one design rule. It appears in the Property Manager with a coloured status symbol.

### How to declare one

1. Add a `<CustomProperty>` with `ReadOnly="true"` and a name starting with `mc_`
2. Add a `<LuaExpression>` with `IsModelCheck="true"` calling a check function
3. Implement the check function as a `<LuaFunction>` with `ReturnType="String"` (or `"Tuple"`)

### Return value format

```lua
return "message", symbol
return "value: message", {relatedObject1, relatedObject2}, symbol
return "value: message", {relatedObjects}, symbol, _info("Extra tooltip text")
```

Message format convention: `<value>: <STATUS> - <Sentence ending with period.>`

### Status symbols

| Symbol | Visual | Meaning |
|---|---|---|
| `_ok` | Green checkmark | Valid / within tolerance |
| `_warning` | Yellow triangle | Marginal / outside recommended range |
| `_error` | Red cross | Invalid / rule violation |
| `_unfinished` | Blue question mark | Data incomplete — cannot evaluate |
| `_noSymbol` | (nothing) | Informational only |
| `_info("text")` | Tooltip | Additional detail shown on hover |

### Common global model check functions (from `NO-BN-ModelChecks.xml`)

| Function | What it checks |
|---|---|
| `NOBN_com_chkNumberOfOcpAreas()` | Whether the object belongs to exactly one OCP area |
| `NOBN_com_chkDistanceToClosestNeighbour()` | Distance to the nearest object of a given type in a given direction |
| `NOBN_com_triggerNeighbourObjectModelchecks()` | Forces neighbouring objects to refresh their model checks |

### Signalling model checks (from `NO-BN-Balises.xml`)

| Function | Rule |
|---|---|
| `NOBN_sig_chkBaliseDistanceToPreviousBalise()` | Checks balise separation against Bane NOR ATP distance rules (critical thresholds at 2.35 m, 2.8 m, 3.2 m, 3.5 m, 10.5 m, 11.8 m) |

---

## Standard Property Macros

Defined in `NO-BN-StandardProperties.xml`, these XPPq macros are reused across object types with `<xpp:expand select="MACRO_NAME"/>`:

| Macro Name | What it adds |
|---|---|
| `NOBN_com_STD_CUSTOMATTRIBUTES___VAR` | The `Var` (local variable / sequence number) property |
| `NOBN_com_DISCIPLINE___OCS` | Discipline tag set to OCS (EH) |
| `NOBN_com_DISCIPLINE___SIG` | Discipline tag set to Signalling (SA) |
| `NOBN_com_DISCIPLINE___TRK` | Discipline tag set to Track (KO) |
| `NOBN_com_DISCIPLINE___TEL` | Discipline tag set to Telecom (TE) |
| `NOBN_com_CHK_NUMBER_OF_OCP_AREAS` | Adds `mc_NumberOfOcpAreas` model check property + formula |
| `NOBN_com_SET_OCP_STATION_REFERENCE` | Adds OCP station code lookup |
| `NOBN_com_STD_LUAEXPRESSIONS___SYMBOLFRAME` | Symbol frame (error/warning/unfinished indicator) |
| `NOBN_com_STD_LUAEXPRESSIONS___EARTHED_TO_NONE` | Earthing status = none |
| `NOBN_com_STD_LUAEXPRESSIONS___EARTHED_TO_RAIL` | Earthing status = to rail |
| `NOBN_com_STD_LUAEXPRESSIONS___WAYSIDE_GENERAL_OBJECT` | Standard wayside object properties |
| `NOBN_com_STD_LUAEXPRESSIONS___TRACKBOUND_OBJECT` | Standard track-bound object properties |
| `NOBN_com_STD_CUSTOMATTRIBUTES___MILEAGE_INCREASES_TOWARDS_LEFT` | The `MileageIncreasesTowardsLeft` computed property |

---

## Standard Text Attribute Macros

Defined in `NO-BN-StandardTextAttributes.xml`. Applied with `<xpp:expand select="…"/>`:

| Macro Name | What text attributes it adds |
|---|---|
| `NOBN_com_STD_TEXTATTRIBUTES___OCS_NORMAL` | Standard OCS text (code + name, vertical) |
| `NOBN_com_STD_TEXTATTRIBUTES___SIG_NORMAL` | Standard signalling text |
| `NOBN_com_STD_TEXTATTRIBUTES___TEL_VERTICAL` | Telecom vertical text |
| `NOBN_com_STD_TEXTATTRIBUTES___CONTENT` | Free-text content label |
| `NOBN_com_STD_TEXTATTRIBUTES___HORIZONTAL_AND_VERY_FAR` | Horizontal text placed far from object |
| `NOBN_com_STD_TEXTATTRIBUTES___VERTICAL_AND_FAR` | Vertical text placed at offset |

---

## Position Tool Settings

Configured in `NO-BN-PositionToolSettings.xml`. Controls the live readout shown near the cursor when placing an object.

The following data lines appear on screen in this priority order:

| Priority | Info shown | Lua function |
|---|---|---|
| Top | Own alignment name | `NOBN_trk_PositionTransient_Set_AlignmentName_Own` |
| 1 | Own alignment mileage | `NOBN_trk_PositionTransient_Set_Mileage_Own` |
| 2 | Own alignment distance-along | `NOBN_trk_PositionTransient_Set_DistanceAlong_Own` |
| 3 | Own alignment elevation | `NOBN_trk_PositionTransient_Set_Elevation_Own` |
| 4 | Distance to alignment | `NOBN_trk_PositionTransient_Set_DistanceToAlignment_Own` |
| Top | Reference alignment name | `NOBN_trk_PositionTransient_Set_AlignmentName_Ref` |
| 2 | Reference alignment distance-along | `NOBN_trk_PositionTransient_Set_DistanceAlong_Ref` |
| 3 | Reference alignment elevation | `NOBN_trk_PositionTransient_Set_Elevation_Ref` |

---

## Global Configuration (from `NO-BN-RootFile.xml`)

### DNA Identity

```
Administration: NO-BN (Bane NOR SF)
Agent:          NO.0001 (Railcomplete AS)
Version:        2026.1
```

### Drawing Settings

| Setting | Value |
|---|---|
| Linear unit | Decimal, 3 decimal places |
| Angular unit | Decimal degrees, 3 places |
| Insertion scale | Meters |
| Default font | Arial |
| Default text style | RC-ARIAL |
| Default text height | 1.0 m |
| Initial screen width | 420 m |
| Right side of alignment | Positive X direction |
| Reference alignment projection | Direct (Bane NOR method) |

### Default Train Dimensions (for sighting calculations)

| Parameter | Value |
|---|---|
| Car width | 3.40 m |
| Axle separation | 18.0 m |
| Axle count | 2 |
| Nose length | 3.0 m |
| Tail length | 3.0 m |

### File Paths (relative to administration home folder)

| Resource | Path |
|---|---|
| Switch geometry | `NO-BN\DNA\Switches\NO-BN-2026.1-SwitchGeometries.xml` |
| 2D symbol library | `NO-BN\2D\NO-BN-2026.1-2D.dwg` |
| Symbol thumbnails | `NO-BN\2D\NO-BN-2026.1-SymbolThumbnails.rc` |
| 3D libraries | `NO-BN\3D\STD-2026.1\{FE,KO,KU,EH,EL,SA,TE,SK}` |
| 3D layer mappings | `NO-BN\3D\LayerMappings` |
| DNA mappings | `NO-BN\DNA\DnaMappings` |
| FAQ | `NO-BN\FAQ\NO-BN-2026.1-faq.xml` |
| Lua tooltip pages | `NO-BN\Lua\LuaTooltipPages` |

---

## Interlocking Export Configuration

Defined in `NO-BN-Interlocking.xml`. Tells the RC interlocking synthesis engine which objects are valid train route and shunting route endpoints.

| Filter | Criteria |
|---|---|
| Train route start | Signals with `MainSignal` = `Hs2` or `Hs3`, or ERTMS marker boards |
| Train route end | Signals with `MainSignal` = `Hs1`, `Hs2`, or `Hs3`; fictitious end points; ERTMS marker boards |
| Shunting route start | Signals where `DwarfSignal` = `"Ja"` |
| Shunting route end | Signals with any dwarf; fictitious end points |
| Via points | Fictitious "via point for control panel" objects |

---

## Key Lua Functions by Discipline

### Common (NOBN_com_)

| Function | Purpose |
|---|---|
| `NOBN_com_mileageIncreasesTowardsLeft()` | Returns true if km increases leftward in current UCS |
| `NOBN_com_setStage()` | Extracts `XXxx-YYyy` construction stage pattern from layer name |
| `NOBN_com_setLayer()` | Returns layer name with stage suffix, validated |
| `NOBN_com_getOcpCode()` | Returns the OCP station code for this object |
| `NOBN_com_chkNumberOfOcpAreas()` | Model check: object belongs to exactly 1 OCP area |

### Signalling (NOBN_sig_)

| Function | Purpose |
|---|---|
| `NOBN_sig_getSignalNumber()` | Extracts numeric part from signal code (e.g., `4313UL` → `4313`) |
| `NOBN_sig_getSignalLitra()` | Extracts letter part from signal code (e.g., `4313UL` → `UL`) |
| `NOBN_sig_getSignalShortName()` | Returns formatted short name (e.g., `UL(4313)`) |
| `NOBN_sig_getSignalFullName()` | Returns full formatted signal name per Bane NOR standard |
| `NOBN_sig_getCabinetName()` | Returns name for signal cabinet (apparatus, keylock, crank, cable) |
| `NOBN_sig_getBaliseGroupDirection()` | Returns balise group direction (`up`/`down`/`unknown`) |
| `NOBN_sig_chkBaliseDistanceToPreviousBalise()` | Model check: inter-balise distance vs. ATP rules |

### OCS (NOBN_ocs_ / JBTEH_MAS_)

| Function | Purpose |
|---|---|
| `JBTEH_MAS_nonPortalPole_code()` | Auto-generates pole code from reference mileage + sequence |
| `_JBTEH_MAS_getSpanlengths()` | Computes 2D span lengths for each contact wire at a pole |
| `NOBN_ocs_getWireClampPoint3D()` | Returns 3D position of the contact wire clamp on a cantilever |

### Substructure (NOBN_sub_)

| Function | Purpose |
|---|---|
| `NOBN_sub_getFoundationCode()` | Finds the code of an OCS/signal/cabinet foundation from nearby objects |

### Generic RC (RC__)

| Function | Purpose |
|---|---|
| `RC__COPYRIGHT_STATEMENT()` | Returns the DNA copyright string |
| `RC__DNA_VERSION()` | Returns the current DNA version IRI |
| `RC__toInt(x)` | Converts float to integer string |
| `RC__round(x, p)` | Rounds `x` to `p` decimal places |
| `RC__isNan(x)` | Returns true if `x` is NaN |
| `RC__getAngleFromDir(d, rs)` | Angle in degrees from direction (`up`/`down`/`both`) |
| `RC__flipAcsTuple(lat, long, obj)` | Flips ACS offsets based on object side and direction |
| `RC__AcsToWcs(lat, long, deg, obj)` | Converts alignment-relative coords to world coords |
| `RC__WcsToAcs(x, y, deg, obj)` | Converts world coords to alignment-relative coords |
| `RC__getDistance2D(p1, p2)` | Euclidean 2D distance between two points |
| `RC__getDistance3D(p1, p2)` | Euclidean 3D distance between two points |
| `RC__identify(obj)` | Returns best available identifier (name, code, or id) |

---

## Relation Constants

Defined in `NO-BN-Relations.xml`. Used in `getRelatedObjects(rel_XYZ)` calls:

| Constant | Meaning |
|---|---|
| `rel_OcsPole_Has_Cantilever` | OCS pole → its cantilevers |
| `rel_Cantilever_Holds_ContactWire` | Cantilever → contact wire alignment |
| `rel_CantileverOrWtb_HasNext_CantileverOrWtb` | Cantilever/WTB chain — sequential ordering |
| `rel_OcsWireChangeAnnotation_AppliesTo_ContactWire` | Wire change annotation → contact wire |
| `rel_NssBalise_BelongsTo_NssBaliseGroup` | NSS balise → its balise group |
| `rel_EtcsBalise_BelongsTo_EtcsBaliseGroup` | ETCS balise → its balise group |
| `rel_EtcsBaliseGroup_Contains_EtcsBalise` | ETCS balise group → its balises |
| `rel_Signal_Has_BaliseGroup` | Signal → associated balise group |
| `rel_Label_AppliesTo_Anything` | Label → any target object |
| `rel_OcsPole_HasNext_OcsPole` | OCS pole → next pole in sequence |

---

## How to Add a New Object Type — Checklist

1. **Choose the correct file** — pick the discipline file (e.g., `NO-BN-Signals.xml` for signalling).
2. **Declare the object type** using `<ObjectType>` with correct `DataType`, `Class`, `LuaName`, `Name`, `Layer`, `Group`.
3. **Set RelationSpace** — the space name used in relation declarations.
4. **Apply standard macros** — `<xpp:expand select="NOBN_com_STD_CUSTOMATTRIBUTES___VAR"/>`, discipline macro, symbol frame, earthing status, text attributes.
5. **Add custom properties** if needed — `<CustomProperty>`.
6. **Add model checks** — either via macro or custom `mc_` property + `LuaExpression`.
7. **Add Lua formulas** — for `name`, `code`, `Geometry3D_0.Name`, and any computed properties.
8. **Define Variants** — if the object has subtypes.
9. **Define InsertPointObject** — snap behaviour, 2D block name, alignment target space.
10. **Verify relation declarations** — ensure `NO-BN-Relations.xml` has the needed relations.
11. **Add 2D symbol** — matching block `NO-BN-2D-JBTxx_DESCRIPTION-Schematic` and `-Geographic` in the 2D LISP source.
12. **Add 3D model** — matching `.dwg` file in `NO-BN\3D\STD-2026.1\{discipline}`.

---

## How to Add a New Lua Function — Checklist

1. **Determine prefix** — `RC__` if generic, `NOBN_xxx_` if Norway/discipline-specific, `_OBJECTTYPE_` if local to one type.
2. **Choose the correct file** — `NO-BN-General-Lua.xml` for generic, `NO-BN-National-Lua.xml` for shared national, or inside the discipline object file.
3. **Use the template** from `_NO-BN-TemplateFunctionDeclaration.xml`.
4. **Always default `obj` to `this`**: `obj = obj or this`
5. **Check for nil** before accessing alignment or related objects.
6. **Collections are 0-indexed** — loop with `for i = 0, n-1 do`.
7. **For model checks**, return `message, {objects}, symbol`.
8. **Bind with `<LuaExpression>`** inside the relevant `<ObjectType>` block.

---

*This document was generated from the NO-BN DNA source files in `NO-BN\DNA\_SRC\`. For the authoritative source, always refer to the XML files directly and to `NO-BN-RootFile.xml` for naming conventions.*
