# NO-BN DNA Patch 13 — Reference Documentation

**File:** `NO-BN/DNA/NO-BN-2021.a-DNA-patch_13.xml`
**Base DNA version:** 2021.a
**Total lines:** 37,394
**Root element:** `<RailwayObjectTypeDefinitions>`

---

## What Changed from Patch 12

Patch 13 adds 400 lines over patch 12. The changes fall into four areas:

### Typography overhaul
- New text style **RC-ARIAL** (Arial font) added and set as the **default** text style, replacing the previous iso3098/iso defaults.
- All `TextAttribute` definitions across ~18 object types have been updated:
  - Attribute renamed: `MText` → `IsMtext` (capitalised).
  - `OBJEKTBESKRIVELSE` attribute changed to `IsMtext="true"` (enables multi-line description text).
  - New `LuaExpression TextAttribute_OBJEKTBESKRIVELSE.Justify` added per object type — dynamically positions the description label above or below based on the OBJEKTNAVN position.

### Track switch (JBTKO_SPV Sporveksel) text positioning
- Full redesign of text attribute positioning for switch objects.
- TextAttributes now carry an explicit `Style="RC-ARIAL"` attribute.
- New positioning uses the new `RC__flipAcsTuple()` function to correctly mirror label offsets when the switch is drawn left-hand or in the "down" direction.

### New Lua functions
| Function | Added / Removed | Purpose |
|---|---|---|
| `RC__flipAcsTuple()` | **Added** | Converts (lateral, longitudinal) ACS offsets, flipping signs based on object orientation and switch geometry |
| `_JBTEH_BAR_Offset3DZ()` | **Added** | Calculates Z-elevation for guy wires drawn in either direction (mast→footplate or footplate→mast) |
| `_JBTEH_MAS_VerticalOffset()` | **Added** | Returns Z = 0 to lock OCS mast to lowest-rail elevation (can be overridden per object) |
| `RC__getWcsVectorFromAcsVector()` | **Removed** | Superseded by `RC__flipAcsTuple()` |
| `RC__getAcsVectorFromWcsVector()` | **Removed** | Superseded by `RC__flipAcsTuple()` |

Net Lua function count: **137** (136 in patch 12, +3 added, −2 removed).

### New configuration
- **`DefaultLandxmlImportSettings`** added inside `<DefaultSettingsForCommands>`: specifies `JBTKO_SPO Spor` as the default object type when importing alignments from LandXML files.
- **`DnaIdentification`** name updated from `"NO-BN 2021.a (patch 11)"` to `"NO-BN 2021.a (patch 13)"`.

---

## Table of Contents

1. [Overview](#overview)
2. [File Structure](#file-structure)
3. [Style Definitions](#style-definitions)
4. [Object Types](#object-types)
5. [Lua Functions](#lua-functions)
6. [Lua Expressions](#lua-expressions)
7. [Dock Points and Dynamic Snap Points](#dock-points-and-dynamic-snap-points)
8. [Show Layers](#show-layers)
9. [Property Overrides](#property-overrides)
10. [Property Categories](#property-categories)
11. [Fouling Point Settings](#fouling-point-settings)
12. [Display Gauge Settings](#display-gauge-settings)
13. [Position Tool Settings](#position-tool-settings)
14. [Default Settings for Commands](#default-settings-for-commands)
15. [Default Interlocking Export Options](#default-interlocking-export-options)
16. [DNA Identification](#dna-identification)

---

## Overview

This file is a **cumulative DNA patch** that extends and overrides the base `NO-BN-2021.a-DNA.xml`. It is a complete self-contained DNA document covering all disciplines for the Norwegian railway administration (Bane NOR). It defines:

- 148 railway object types across all engineering disciplines
- 137 global Lua functions
- 163 AutoCAD layer definitions
- 94 show-layer display controls
- 51 property overrides and 27 property categories
- Style definitions, fouling point configurations, gauge profiles, and interlocking export options

---

## File Structure

The root element `<RailwayObjectTypeDefinitions>` contains these top-level sections in order:

| Section | Description |
|---|---|
| `<StyleDefinitions>` | Linetypes, text styles, layers, annotation config, symbol frames |
| `<PropertyOverride>` | 51 property display configurations |
| `<PropertyCategory>` | 27 property groupings for the UI |
| `<DefaultSettingsForCommands>` | Default sighting, train dimension, and LandXML import parameters |
| `<FoulingPointSetting>` | 4 fouling point calculation configurations |
| `<PositionToolSettings>` | Position display format configuration |
| `<ReferenceAlignmentProjectionMethod>` | Alignment projection method setting |
| `<DisplayGaugeSettings>` | 8 track clearance gauge profiles |
| `<ObjectType>` | 148 railway object type definitions |
| `<LuaExpression>` | Standalone Lua expressions |
| `<LuaFunction>` | 137 global Lua function definitions |
| `<ShowLayer>` | 94 layer visibility control entries |
| `<DnaIdentification>` | DNA version and path configuration |
| `<DefaultInterlockingExportOptions>` | Interlocking export filter scripts |

---

## Style Definitions

### Text Styles

| Name | Font | Height | Notes |
|---|---|---|---|
| RC-STANDARD | iso3098 | 0 (auto) | Legacy standard style |
| RC-ARIAL | Arial | 0 (auto) | **New in patch 13** |

**Default text style:** `RC-ARIAL` (new in patch 13, previously `iso`).

### Linetypes

| Name | Description |
|---|---|
| RC-SOLID | Solid continuous line |
| RC-DASHED | Basic dashed line (2/1 pattern) |
| RC-DASHED-01 | Longer dash, small gap |
| RC-DASHED-02 | Dash-dot pattern |
| RC-DASHED-03 | Dash-dot-dot pattern |
| RC-EARTHING-jl | Earthing line with "jl" label |
| RC-EARTHING-Cu50 | Copper 50mm² earthing conductor |
| RC-EARTHING-Cu70 | Copper 70mm² earthing conductor |
| RC-EARTHING-Cu95 | Copper 95mm² earthing conductor |
| RC-EARTHING-Cu120 | Copper 120mm² earthing conductor |
| RC-EARTHING-Cu150 | Copper 150mm² earthing conductor |
| RC-EARTHING-Alu120 | Aluminium 120mm² earthing conductor |
| RC-EARTHING-Alu240 | Aluminium 240mm² earthing conductor |
| RC-OCP | Operational control point boundary (50m pattern) |
| RC-Brukerdefinert | User-defined area boundary |
| RC-Arbeidsområde | Work area boundary |
| RC-Lokalområde | Local area boundary |
| RC-Parsellgrense | Section boundary |
| RC-Plattform | Platform edge line |
| RC-Jernbanebro | Railway bridge line |
| RC-Veibro | Road bridge line |
| RC-Tunnel | Tunnel line |
| RC-Viewport | Viewport boundary |
| RC-JordingsMaskenett | Earthing mesh network line |

### Layers

All 163 layers follow the naming convention `JBTxx_CODE` where `xx` is the discipline code. Layers use the `@` prefix for annotation/text attributes, `$` for component sub-layers, and `#` for transient annotation graphics.

#### RailCOMPLETE Internal Layers

| Layer | Color | Description |
|---|---|---|
| JBTRC_XRF | 62 | Referenced external DWG files (XRef) |
| JBTRC$SYMBOLRAMMER_HISTORIC | 62 | Symbol frames for historic objects |
| JBTRC$SYMBOLRAMMER_UNFINISHED | 62 | Symbol frames for unfinished objects |
| JBTRC$SYMBOLRAMMER_ERROR | 62 | Symbol frames for objects with model check errors |
| JBTRC$SYMBOLRAMMER_WARNING | 62 | Symbol frames for objects with model check warnings |
| JBTRC$SYMBOLRAMMER_DEFAULT | 62 | Default symbol frames |

#### Cross-discipline Text Layers

Each discipline has three shared text layers (Color 62):

| Pattern | Description |
|---|---|
| `JBTxx@OBJEKTNAVN` | Object name display |
| `JBTxx@OBJEKTID` | BaneData FDV Object-ID display |
| `JBTxx@OBJEKTBESKRIVELSE` | Object description display |

Applied to disciplines: FE, KU, KO, EH, SA, EL, TE.

#### Transient Annotation Layers (JBTFE#)

| Layer | Color | Description |
|---|---|---|
| JBTFE#ANNOTATIONS_GEOMETRY | 2 | Geometry transient graphics |
| JBTFE#ANNOTATIONS_ALIGNMENTNAME | 2 | Alignment name labels |
| JBTFE#ANNOTATIONS_MILEAGE | 2 | Mileage labels |
| JBTFE#ANNOTATIONS_MILEAGECHANGE | 2 | Chain discontinuity markers |
| JBTFE#ANNOTATIONS_VERTICALPROFILE | 2 | Vertical profile graphics |
| JBTFE#ANNOTATIONS_FOULINGPOINT | 2 | Fouling point markers |
| JBTFE#ANNOTATIONS_REFERENCEALIGNMENT | 2 | Reference alignment graphics |
| JBTFE#ANNOTATIONS_SECTION | 2 | Track usage / alignment segment graphics |
| JBTFE#ANNOTATIONS_RELATIONS | 7 | Relation visualisation |
| JBTFE#ANNOTATIONS_ALIGNMENT_POS_3D | 7 | 3D position relative to track |
| JBTFE#ANNOTATIONS_EARTHING | 90 | Earthing graphics |
| JBTFE#ANNOTATIONS_SIGHT_LINE | 2 | 2D sight lines (dashed) |
| JBTFE#ANNOTATIONS_SIGHT_AREA | 2 | 2D sight areas |
| JBTFE#ANNOTATIONS_SIGHT_BEAM | 2 | 2D sight beams |
| JBTFE#ANNOTATIONS_SIGHT_VOLUME | 2 | 2D sight volumes |

#### FE — Felles (Common/General)

| Layer | Color | Description |
|---|---|---|
| JBTFE_DIVERSE | 4 | Miscellaneous general objects |
| JBTFE_OMRÅDER | 1 | Areas |
| JBTFE_ETIKETTER | 4 | Labels |
| JBTFE_MARKØRER | 4 | Markers |
| JBTFE_WATCH | 4 | Watch objects |
| JBTFE_SPORBRUK | 4 | Track usage |
| JBTFE_SPORBRUKGRENSE | 4 | Track usage boundaries |
| JBTFE_HJL | 6 | Helper/construction lines |

#### KU — Underbygning (Substructure/Civil Works)

| Layer | Color | Description |
|---|---|---|
| JBTKU_SKT | 2 | Signs not belonging to a specific discipline |
| JBTKU_SPB | 2 | Special brackets |
| JBTKU_SKF | 2 | Sign foundations (not in substructure contract) |
| JBTKU_FUN | 2 | Large foundations requiring track re-stabilisation |
| JBTKU_KFK | 1 | Cable duct (with cable tap-offs) |
| JBTKU_KFR | 5 | Pipe crossings and encased pipe bundles |
| JBTKU_KFP | 6 | Steel pipes pressed through ground |
| JBTKU_KFT | 3 | Flexible conduit (low protection) |
| JBTKU_KFØ_TREKKEKUM | 2 | Pull pits (cable draw pits) |
| JBTKU_VEI | 5 | Roads (car, cycle, pedestrian, escape routes) |

#### KO — Overbygning (Track Superstructure)

| Layer | Color | Description |
|---|---|---|
| JBTKO_SPO | 2 | Track |
| JBTKO_SKI | 2 | Rails |
| JBTKO_SGB | 2 | Subgrade and ballast |
| JBTKO_PLF | 4 | Platform edges |
| JBTKO_SPV | 2 | Switches and crossings |
| JBTKO_SPF | 2 | Track continuations |
| JBTKO_TNG | 2 | Switch blades (tongues) |
| JBTKO_SKJ | 2 | Rail joints |
| JBTKO_SVI | 2 | Sleepers |
| JBTKO_MVS | 2 | Derailment beams |
| JBTKO_SST | 2 | Buffer stops |
| JBTKO_SKI_isolasjon | 2 | Single rail running edge and track isolation |
| JBTKO_HOS | 2 | Horizontal geometry segments |
| JBTKO_VES | 2 | Vertical profile segments |
| JBTKO_HOT | 2 | Horizontal geometry transition points |
| JBTKO_VET | 2 | Vertical profile transition points |
| JBTKO_KIL | 2 | Kilometre/hectometre/decametre markers |
| JBTKO_SKT | 2 | Signs (track discipline) |
| JBTKO$SPF | 62 | Component layer: track continuations |
| JBTKO$SPV | 62 | Component layer: switches |
| JBTKO@SPV_TYPE | 62 | Text attribute: switch type |

#### EH — Elektro Høyspent (High-Voltage / OCS)

| Layer | Color | Linetype | Description |
|---|---|---|---|
| JBTEH_KTL_Kjørbar | 7 | Continuous, 0.35mm | Catenary/contact wire, traversable |
| JBTEH_KTL_Ikke_kjørbar | 7 | RC-DASHED, 0.35mm | Catenary/contact wire, non-traversable |
| JBTEH_BAR | 2 | Continuous, 0.18mm | Stay wire (bardun) |
| JBTEH_STR | 2 | Continuous, 0.18mm | Strut (strever) |
| JBTEH_FUN | 2 | — | OCS mast foundations |
| JBTEH_JOR | 3 | RC-DASHED | Longitudinal earth conductor |
| JBTEH_JOF | 2 | — | OCS rail earthing connection |
| JBTEH_JSK | 2 | — | OCS earthing busbar |
| JBTEH_JEL | 2 | — | OCS earth electrode |
| JBTEH_XBE | 3 | — | High-voltage protection screen |
| JBTEH_LBA | 2 | — | Conductor change annotation |
| JBTEH_AAK | 2 | — | OCS yoke (åk) |
| JBTEH_MAS | 2 | — | OCS mast, free-standing |
| JBTEH_MAS_HGM_TUNNEL | 2 | — | OCS hanging mast for bridge/tunnel |
| JBTEH_MAS_HGM_ÅK | 2 | — | OCS hanging mast in yoke |
| JBTEH_UTL | 2 | — | OCS support bracket (utligger) |
| JBTEH_KLK | 2 | — | Contact wire clamp |
| JBTEH_AEH | 2 | — | Tensioning anchor (with console) |
| JBTEH_BAF | 2 | — | Stay wire anchor plate |
| JBTEH_STF | 2 | — | Strut anchor plate |
| JBTEH_KPM | 2 | — | Console on mast |
| JBTEH_FOR | 2 | — | Connection / current bridge |
| JBTEH_SVE | 2 | — | Floating crossing |
| JBTEH_SIL | 2 | — | Section insulator (traversable) |
| JBTEH_ISO | 2 | — | Line insulator (non-traversable) |
| JBTEH_ISS | 2 | — | Rod insulator (traversable) |
| JBTEH_BRY | 2 | — | High-voltage switch |
| JBTEH_SUG | 2 | — | Suction transformer |
| JBTEH_AUT | 2 | — | Auto-transformer |
| JBTEH_BKT | 2 | — | OCS sign (skilt) |
| JBTEH$SPV | 62 | — | Component layer: switches in OCS (off by default) |
| JBTEH@STREKKRAFT | 62 | — | Text attribute: tension force [kN] |
| JBTEH@SPENNLENGDE | 62 | — | Text attribute: span length [m] |
| JBTEH@SIKKSAKK | 62 | — | Text attribute: stagger/zigzag [cm] |
| JBTEH@UTL_LEDNING | 62 | — | Text attribute: wire name held by bracket |

#### SA — Signalanlegg (Signalling)

| Layer | Color | Description |
|---|---|---|
| JBTSA_ATC | 2 | NSS balise group (ATC2 / Ebicab 700) |
| JBTSA_ATB | 2 | NSS balise |
| JBTSA_ATK | 2 | NSS balise codes |
| JBTSA_ETC | 2 | ETCS balise group |
| JBTSA_ETB | 2 | ETCS balise (Eurobalise) |
| JBTSA_LEU | 2 | ETCS balise encoder (LEU) |
| JBTSA_TER | 2 | Technical buildings and rooms |
| JBTSA_APS | 2 | Equipment cabinets |
| JBTSA_AVI | 2 | Derailment indicator |
| JBTSA_SSP | 2 | Track trap / derail |
| JBTSA_DRV | 2 | Switch machine |
| JBTSA_SPD | 2 | Track-trap machine |
| JBTSA_LOK | 2 | Local point operator |
| JBTSA_SAM | 2 | Collective lock |
| JBTSA_SPF | 2 | Track circuit detection section |
| JBTSA_TEA | 2 | Axle counter detection section |
| JBTSA_TEL | 2 | Axle counter sensor point |
| JBTSA_TET | 2 | Axle counter tuning unit |
| JBTSA_SIG | 2 | Signal |
| JBTSA_ERT | 2 | ERTMS marker board |
| TFES_FIK | 2 | Fictitious signal (virtual route endpoint) |
| JBTSA_SIK | 2 | Interlocking system |
| JBTSA_FUN | 2 | Pole for S-lock, local operator, smaller signals |
| JBTSA_AVS | 2 | Distances |
| JBTSA_MSS | 2 | Signs, boards, and poles |

#### EL — Elektro Lavspent (Low-Voltage Power)

| Layer | Color | Description |
|---|---|---|
| JBTEL_LYA | 2 | Light fixture |
| JBTEL_LYS | 2 | Lighting point |
| JBTEL_FSP | 2 | Distribution cabinet |
| JBTEL_SVG | 2 | Group cabinet |
| JBTEL_TVP | 2 | Train heating post |
| JBTEL_TRF | 2 | Transformer |
| JBTEL_UPS | 2 | UPS distributor |

#### TE — Telekommunikasjon (Telecommunications)

| Layer | Color | Description |
|---|---|---|
| JBTTE_RAC | 2 | Telecom rack |
| JBTTE_TLT | 2 | Telephone |

### Symbol Frames

11 symbol frame types control the visual border drawn around object symbols:

| Name | Purpose |
|---|---|
| Symbolramme-R2.75 | Standard frame, radius 2.75 |
| Watch | Watch object frame |
| Frame1–Frame5 | Size-graded standard frames |
| Historic | Frame for historic/decommissioned objects |
| Unfinished | Frame for objects not yet fully specified |
| Error | Frame for objects with model check errors |
| Warning | Frame for objects with model check warnings |

---

## Object Types

148 ObjectType entries are defined, organized by discipline. The structure is unchanged from patch 12. The JBTKO_SPV Sporveksel object type received updated text attribute definitions (see [What Changed from Patch 12](#what-changed-from-patch-12)).

### Tables (14 types)

| Name | DataType | Description |
|---|---|---|
| FE-DIV Etikett | RailCOMPLETELabel | Generic label/annotation |
| FE-DIV Brukerdefinert tabell | — | User-defined table |
| FE-DIV Skiltplantabell | — | Sign plan table |
| FE-DIV Sporprosjekteringstabell | — | Track design table |
| FE-DIV Sporvekseltabell | — | Switch table |
| FE-DIV Horisontaltrasétabell | — | Horizontal alignment table |
| FE-DIV Vertikaltrasétabell | — | Vertical profile table |
| FE-DIV KL-tabell | — | OCS table |
| FE-DIV Jordingstabell | — | Earthing table |
| FE-DIV Fundamenttabell | — | Foundation table |
| FE-DIV Føringsveitabell | — | Cable route table |
| FE-DIV Forriglingstabell | — | Interlocking table |
| FE-DIV Signaltabell | — | Signal table |
| FE-DIV ATC kodetabell | — | ATC code table |

### FE — Felles (General) Object Types (10 types)

| Name | Class | Description |
|---|---|---|
| JBTFE_HJL Hjelpelinje | RailwayAlignment | Helper/construction alignment line |
| JBTFE_DIV OCP område | RailwayArea (eOcp) | Operational control point area |
| JBTFE_DIV Område | RailwayArea | Generic area |
| JBTFE_WCH Watch | RailwayPlacedObject | Watch object for custom monitoring |
| JBTFE_MRK Markør | RailwayPlacedObject | Generic marker |
| JBTFE_DIV Sporbruk | RailwayAlignment | Track usage definition |
| JBTFE_DIV Sporbrukgrense | RailwayPlacedObject | Track usage boundary marker |
| JBTFE_DIV Etikett for sporveksel | — | Switch label |
| JBTFE_DIV KOF-format koordinatliste | — | KOF-format coordinate list |
| JBTFE_DIV Cross section | — | Cross section |
| JBTFE_PRX Proxy | — | Proxy object |

### KO — Track Superstructure Object Types (20 types)

| Name | Description |
|---|---|
| JBTKO_SPO Spor | Track alignment |
| JBTKO_SPV Sporveksel | Switch (turnout) — text attributes redesigned in patch 13 |
| JBTKO_SPV Sporkryss | Diamond crossing |
| JBTKO_SPF Sporfortsettelse | Track continuation |
| JBTKO_TNG Sporvekseltunge | Switch blade/tongue |
| JBTKO_SKI Enkeltskinne kjørekant og isolasjon | Single rail running edge and track isolation |
| JBTKO_SKJ Skinneskjøt | Rail joint |
| JBTKO_SGB Subgrade and ballast | Subgrade and ballast |
| JBTKO_PLF Plattformkant | Platform edge |
| JBTKO_MVS Oppkjørsbjelke | Derailment beam |
| JBTKO_SST Sporstopper | Buffer stop |
| JBTKO_HOS Horisontalgeometrisegment | Horizontal geometry segment |
| JBTKO_VES Vertikalprofilsegment | Vertical profile segment |
| JBTKO_HOT Horisontalgeometripunkt | Horizontal geometry transition point |
| JBTKO_VET Vertikalprofilpunkt | Vertical profile transition point |
| JBTKO_SKT (multiple sign variants) | Track-side signs (Signal 63, 64, 66, 67, 68/69, 74, 75, etc.) |

### KU — Substructure Object Types (10 types)

| Name | Description |
|---|---|
| JBTKU_FUN Apparatskapfundament | Equipment cabinet foundation |
| JBTKU_FUN Signalfundament | Signal foundation |
| JBTKU_FUN Telefundament | Telecom foundation |
| JBTKU_KFK Kabelkanal | Cable duct |
| JBTKU_KFP Pressrør | Pressed steel pipe |
| JBTKU_KFR Rørpakke | Pipe bundle/crossing |
| JBTKU_KFT Trekkerør | Flexible conduit |
| JBTKU_KFØ Trekkekum | Cable pull pit |
| JBTKU_SKF Skiltfeste | Sign mounting foundation |
| JBTKU_VEI Vei | Road |

### EH — High-Voltage OCS Object Types (26+ types)

| Name | Description | Patch 13 notes |
|---|---|---|
| JBTEH_KTL Kontaktledning | Contact wire / catenary | |
| JBTEH_MAS KL-mast | OCS mast, free-standing | New `_JBTEH_MAS_VerticalOffset()` locks base elevation to Z=0 |
| JBTEH_MAS Hengemast for bru og tunnel | OCS hanging mast for bridge/tunnel | |
| JBTEH_MAS Hengemast i åk | OCS hanging mast in yoke | |
| JBTEH_UTL Utligger | OCS support bracket | |
| JBTEH_UTK Seksjonsutliggerkonsoll | Section bracket console | |
| JBTEH_AAK Åk | OCS yoke | |
| JBTEH_BAR Bardun | Stay wire | New `Offset3D.Z` expression using `_JBTEH_BAR_Offset3DZ()` |
| JBTEH_STR Strever | Strut | |
| JBTEH_FOR Forbindelse, strømbro | Current bridge / connection | |
| JBTEH_SVE Svevende kryss | Floating crossing | |
| JBTEH_AEH Avspenning | Tensioning anchor | |
| JBTEH_FUN KL-fundament | OCS mast foundation | |
| JBTEH_TUF Tunnelfeste | Tunnel mounting | |
| JBTEH_KPM Konsoll på mast | Console on mast | |
| JBTEH_BAF Bardunfeste | Stay wire anchor plate | |
| JBTEH_STF Streverfeste | Strut anchor plate | |
| JBTEH_KLK Kontakttrådklemme | Contact wire clamp | |
| JBTEH_SIL Seksjonsisolator | Section insulator (traversable) | |
| JBTEH_ISO Lineisolator | Line insulator (non-traversable) | |
| JBTEH_ISS Stavisolator | Rod insulator (traversable) | |
| JBTEH_BRY KL-bryter | OCS high-voltage switch | |
| JBTEH_SUG Sugetransformator | Suction transformer | |
| JBTEH_AUT Autotransformator | Auto-transformer | |
| JBTEH_JOR Jordleder | Longitudinal earth conductor | |
| JBTEH_JOF Jordforbinder | Rail earthing connection | |
| JBTEH_JSK Jordskinne | Earthing busbar | |
| JBTEH_JEL Jordelektrode | Earth electrode | |
| JBTEH_XBE Beskyttelsesskjerm | High-voltage protection screen | |
| JBTEH_DIV Ledningsbytte annotering | Conductor change annotation | |
| JBTEH_BKT Signal 65 Strømavtager | Current collector sign (Signal 65) | |

### SA — Signalling Object Types (27 types)

| Name | Description |
|---|---|
| JBTSA_SIG Signal | Main signal |
| JBTSA_SIG Togsporsignal | Shunting signal on track |
| JBTSA_SIG Høyt skiftesignal frittstående | Free-standing shunting signal |
| JBTSA_SIG Brusignal/Frostportsignal | Bridge/frost gate signal |
| JBTSA_SIG Veisignal | Road signal |
| JBTSA_SIG Planovergangssignal | Level crossing signal |
| JBTSA_SIG Rasvarslingssignal | Rockfall warning signal |
| JBTSA_ERT ERTMS-signal | ERTMS marker board |
| JBTSA_FIK Fiktivt punkt | Virtual route endpoint |
| JBTSA_MSS (multiple sign variants) | Reflectors, Signal 60 ATC, Signal 61, balise sign, ERTMS sign, metre markers, etc. |
| JBTSA_ATC NSS balisegruppe | NSS balise group (ATC2 / Ebicab 700) |
| JBTSA_ATB NSS balise | NSS individual balise |
| JBTSA_ETC ETCS balisegruppe | ETCS balise group |
| JBTSA_ETB ETCS balise | ETCS Eurobalise |
| JBTSA_APS Apparatskap | Equipment cabinet (large) |
| JBTSA_APS Lite skap og boks | Equipment cabinet (small) |
| JBTSA_TER Tekniske bygninger og rom | Technical buildings and rooms |
| JBTSA_AVI Avsporingsindikator | Derailment indicator |
| JBTSA_SSP Sporsperre | Track trap / derail |
| JBTSA_DRV Sporvekseldrivmaskin | Switch machine |
| JBTSA_SPD Sporsperredrivmaskin | Track-trap machine |
| JBTSA_LOK Lokalstiller | Local point operator |
| JBTSA_SAM S-lås | Collective lock (S-lock) |
| JBTSA_SPF Sporfelt | Track circuit |
| JBTSA_TEA Togdeteksjonsavsnitt for akseltellere | Axle counter detection section |
| JBTSA_TEL Akselteller sensor | Axle counter sensor point |
| JBTSA_TET Akselteller tuningenhet | Axle counter tuning unit |
| JBTSA_SIK Sikringsanlegg | Interlocking system |

### EL — Low-Voltage Object Types (7 types)

| Name | Description |
|---|---|
| JBTEL_LYS Lys | Lighting point |
| JBTEL_LYA Lysarmatur | Light fixture |
| JBTEL_FSP Fordelingsskap | Distribution cabinet |
| JBTEL_SVG Gruppeskap | Group cabinet |
| JBTEL_TVP Togvarmepost | Train heating post |
| JBTEL_TRF Transformator | Transformer |
| JBTEL_UPS UPS fordeler | UPS distributor |

### TE — Telecommunications Object Types (2 types)

| Name | Description |
|---|---|
| JBTTE_RAC Telerack | Telecom rack |
| JBTTE_TLT Telefon | Telephone |

### Signs / Boards Object Types (12 types)

Various sign types from the KO-SKT and SA-MSS groups:
- Anleggsområde (construction area sign)
- Rømningsavstand (escape distance board)
- Sidespor (siding sign)
- Skilt 1000V togvarmeanlegg (train heating sign)
- Skilt bevegelig kryss (moveable crossing sign)
- Skilt nødtelefon (emergency telephone sign)
- Skilt spornummer (track number sign)
- Stopp se og lytt (Stop, Look and Listen)
- Arbeidsområdeskilt (work area sign)
- Signal 101 Identifikasjonsskilt (identification sign)
- Signal 102 Pilskilt (arrow sub-sign)
- Trafikkskilt 138 Jernbaneovergang (road level crossing sign)

---

## Lua Functions

137 global Lua functions are defined. The table below covers all functions, with **new in patch 13** and **removed in patch 13** clearly marked.

### RC__ — Generic RailCOMPLETE Utilities

| Function | New/Changed | Description |
|---|---|---|
| `RC__toint()` | | Convert value to integer |
| `RC__getCollectionOfRelatedObjects()` | | Get collection of objects related via a named space |
| `RC__getAngleFromDir()` | | Calculate angle from direction vector |
| `RC__getMileageFromRelatedObject()` | | Get mileage value from a related object |
| `RC__getDistanceToAlignmentFromRelatedObject()` | | Get lateral distance from a related object |
| `RC__getRelativeElevationFromRelatedObject()` | | Get relative elevation from a related object |
| `RC__getAngularOffsetFromRelatedObject()` | | Get angular offset from a related object |
| `RC__getDirFromRelatedObject()` | | Get direction from a related object |
| `RC__acsVector2wcsVector()` | | Convert alignment-coordinate-system vector to world-coordinate-system vector |
| `RC__identify()` | | Return object identity string |
| `RC__isNan()` | | Check if a value is NaN |
| `RC__getUrlExtension()` | | Extract file extension from URL |
| `RC__getUrlDrive()` | | Extract drive letter from URL |
| `RC__getUrlDriveAndFolder()` | | Extract drive and folder from URL |
| `RC__getUrlDriveAndFolderAndFilename()` | | Extract drive, folder, and filename from URL |
| `RC__getUrlFilename()` | | Extract filename (no extension) from URL |
| `RC__getUrlFilenameAndExtension()` | | Extract filename with extension from URL |
| `RC__sub()` | | Substring operation |
| `RC__round()` | | Round a number |
| `RC__snap()` | | Snap a value to nearest step |
| `RC__getNearest3DStep()` | | Get nearest 3D geometry step |
| `RC__isMemberOf()` | | Check membership in a collection |
| `RC__sortTable()` | | Sort a Lua table |
| `RC__toKm()` | | Convert mileage to kilometre display string |
| `RC__getDistance2D()` | | Calculate 2D distance between points |
| `RC__getDistance3D()` | | Calculate 3D distance between points |
| `RC__flipAcsTuple()` | **NEW** | Converts (lateral, longitudinal) ACS offsets, flipping signs based on object orientation. For switch objects (`JBTKO_SPV`) uses `SwitchGeometry` + `dir`. For all others uses `RightSided` + `dir`. Replaces the removed WCS↔ACS vector functions. |
| ~~`RC__getWcsVectorFromAcsVector()`~~ | **REMOVED** | Superseded by `RC__flipAcsTuple()` |
| ~~`RC__getAcsVectorFromWcsVector()`~~ | **REMOVED** | Superseded by `RC__flipAcsTuple()` |

### RC_com_ — Common Utility Functions

| Function | Description |
|---|---|
| `RC_com_getNearestEntireKm()` | Snap to nearest whole kilometre |
| `RC_com_getNearestEntireKm2()` | Snap to nearest whole kilometre (variant) |
| `RC_com_getNearestHalfKm()` | Snap to nearest half kilometre |
| `RC_com_getNearestHectometricSnap()` | Snap to nearest hectometre |
| `RC_com_getNearestKilometricSnap()` | Snap to nearest kilometre |
| `RC_com_getLabelItem1/2/3()` | Get label display items 1, 2, and 3 |
| `RC_com_getAreasOfVariant()` | Get areas associated with an object variant |
| `RC_com_getOcpStationReference()` | Get OCP station reference |
| `RC_com_getOcpStationRef()` | Get OCP station reference (short form) |
| `RC_com_getOcpCode()` | Get OCP station code |

### NOBN_com_ — Bane NOR Common Functions

| Function | Description |
|---|---|
| `NOBN_com_Layer()` | Returns layer name including phase (fasekode) suffix |
| `NOBN_com_Name3D()` | Returns 3D model name for an object |
| `NOBN_com_Layer3D()` | Returns 3D layer name for an object |
| `NOBN_com_BaneNORBaneDataID()` | Returns BaneData object ID |
| `NOBN_com_assignEarthingLabelColor()` | Sets colour for earthing annotation labels |
| `NOBN_com_assistAssignEarthingAlignment()` | Helper: assigns earthing alignment |
| `NOBN_com_assistAssignEarthingObject()` | Helper: assigns earthing object |
| `NOBN_com_assistAssignEarthingLabelText()` | Helper: assigns earthing label text |
| `NOBN_com_assistAssignEarthingPosBias()` | Helper: assigns earthing position bias |
| `NOBN_com_chkNumberOfOcpAreas()` | Model check: validates number of OCP areas |
| `NOBN_com_chkTriggerPointObjectModelChecks()` | Model check: validates trigger point objects |
| `NOBN_com_getKof05Records()` | Returns KOF-05 format coordinate records |
| `NOBN_com_getRollFromCantInterpretedAsDecimalDegrees()` | Converts cant to roll angle in decimal degrees |

### NOBN_trk_ — Track Functions

| Function | Description |
|---|---|
| `NOBN_trk_getFoulingPointDistanceNewTrain()` | Fouling point distance: new track, train |
| `NOBN_trk_getFoulingPointDistanceNewShunting()` | Fouling point distance: new track, shunting |
| `NOBN_trk_getFoulingPointDistanceExistingTrain()` | Fouling point distance: existing track, train |
| `NOBN_trk_getFoulingPointDistanceExistingShunting()` | Fouling point distance: existing track, shunting |
| `NOBN_trk_getFoulingPointLocalContribution()` | Local geometry contribution to fouling point |
| `NOBN_trk_getFoulingPointMileage()` | Mileage of calculated fouling point |
| `NOBN_trk_getFoulingPointReferenceMileage()` | Reference mileage for fouling point |
| `NOBN_trk_getTrackUsage()` | Returns track usage type for the current alignment |
| `NOBN_trk_getYawFromDir()` | Calculate yaw angle from direction vector |
| `NOBN_trk_getRollFromCant()` | Calculate roll angle from track cant |
| `NOBN_trk_getLiftFromCant()` | Calculate vertical lift from cant |
| `NOBN_trk_getPitchFromGradient()` | Calculate pitch angle from track gradient |
| `NOBN_trk_getDistanceToAlignmentFromCantAndTrackPlaneDistance()` | Lateral distance accounting for cant |
| `NOBN_trk_getSwitchLabelItem1/2/3()` | Switch label display items |
| `NOBN_trk_chkConnectionAlignmentCompatibility()` | Check: alignment type compatibility at junction |
| `NOBN_trk_chkConnectionCantMatch()` | Check: cant continuity at junction |
| `NOBN_trk_chkConnectionContinuity()` | Check: track geometric continuity |
| `NOBN_trk_chkConnectionElevationMatch()` | Check: elevation continuity at junction |
| `NOBN_trk_chkConnectionGradientAndCantMatch()` | Check: gradient and cant at junction |
| `NOBN_trk_chkConnectionGradientMatch()` | Check: gradient continuity at junction |
| `NOBN_trk_chkConnectionTangentMatch()` | Check: tangent direction continuity at junction |
| `NOBN_trk_PositionTransient_Set_*()` | 10 functions setting transient position display properties |

### NOBN_sig_ — Signalling Functions

| Function | Description |
|---|---|
| `NOBN_sig_getSignalNumber()` | Returns signal number |
| `NOBN_sig_getSignalShortName()` | Returns short signal name |
| `NOBN_sig_getSignalFullName()` | Returns full signal name |
| `NOBN_sig_getSignalPartNames()` | Returns list of signal component names |
| `NOBN_sig_getSignalLitra()` | Returns signal litra (identifier letter) |
| `NOBN_sig_getCabinetName()` | Returns associated cabinet name |
| `NOBN_sig_getSignalSightingRequirement()` | Returns signal sighting distance requirement |
| `NOBN_sig_getBaliseGroupDirection()` | Returns balise group orientation direction |
| `NOBN_sig_chkBaliseDistanceToNextBalise()` | Check: distance to next balise in direction |
| `NOBN_sig_chkBaliseDistanceToPreviousBalise()` | Check: distance to previous balise |
| `NOBN_sig_chkBaliseDistanceToTrackSideways()` | Check: lateral distance from balise to track |
| `NOBN_sig_chkBaliseDistanceToTrackVertically()` | Check: vertical distance from balise to track |
| `NOBN_sig_chkDistanceFromSignalToTrack()` | Check: signal clearance from track (general) |
| `NOBN_sig_chkLeftSideDistanceFromSignalToTrack()` | Check: signal clearance on left side |
| `NOBN_sig_chkRightSideDistanceFromSignalToTrack()` | Check: signal clearance on right side |
| `NOBN_sig_chkSafetyDistanceFromSignalToOcsMast()` | Check: signal to OCS mast safety distance |

### NOBN_ocs_ — OCS Functions

| Function | Description |
|---|---|
| `NOBN_ocs_getTensioningStandardValues()` | Returns standard tensioning values for catenary |

### NOBN_bnp_ — Boards and Poles Functions

| Function | Description |
|---|---|
| `NOBN_bnp_getBoardOrPoleName()` | Returns board or pole name |
| `NOBN_bnp_getBoardOffset3dZ()` | Returns 3D Z offset for board placement |
| `NOBN_bnp_getBoardSightingRequirement()` | Returns sighting distance requirement for board |
| `NOBN_bnp_getPoleRoutingDefaultValue()` | Returns default pole routing value |
| `NOBN_bnp_getEscapeDistanceBoardTexts()` | Returns escape distance texts (both sides) |
| `NOBN_bnp_getEscapeDistanceBoardTextLeft()` | Returns escape distance text for left side |
| `NOBN_bnp_getEscapeDistanceBoardTextRight()` | Returns escape distance text for right side |

### NOBN_sub_ — Substructure Functions

| Function | Description |
|---|---|
| `NOBN_sub_getFoundationCode()` | Returns foundation type code |

### Formatting Functions

| Function | Description |
|---|---|
| `NOBN_ClothoidRadiusFormatting()` | Formats clothoid radius display |
| `NOBN_VerticalProfileElevationFormatting()` | Formats vertical profile elevation display |
| `NOBN_VerticalProfileMileageFormatting()` | Formats vertical profile mileage display |
| `NOBN_MileageAnnotationsMileageFormatting()` | Formats mileage annotation display |

### Object-Local Private Functions (underscore prefix)

Local functions bound to specific object types (not shared):

| Function | Object type | New in patch 13 | Description |
|---|---|---|---|
| `_JBTEH_MAS_SpanLength*()` (4 variants) | OCS mast | | Span length calculations |
| `_JBTEH_MAS_non_yoke_mast_code()` | OCS mast | | Returns mast code for non-yoke masts |
| `_JBTEH_MAS_VerticalOffset()` | OCS mast | **NEW** | Returns Z=0 to lock mast base to lowest rail. Override manually when a different elevation is needed. |
| `_JBTEH_UTL_*()` (5 functions) | OCS bracket | | Bracket-specific calculations |
| `_JBTEH_BAR_Offset3DZ()` | Guy wire | **NEW** | Calculates Z-elevation for a guy wire. Checks which end (mast or footplate) is the start point by comparing geometry to the related footplate position. Handles guy wires drawn in either direction. Author: CLFEY, 2025-10-08. |
| `_JBTSA_ATB_TextAbove/Below/code()` | NSS balise | | NSS balise text and code retrieval |
| `_JBTSA_ATC_code()` | NSS balise group | | NSS balise group code |
| `_JBTSA_ETB_TextAbove/Below/code()` | ETCS balise | | ETCS balise text and code retrieval |
| `_JBTSA_ETC_code()` | ETCS balise group | | ETCS balise group code |

---

## Lua Expressions

Standalone and per-object `<LuaExpression>` entries provide calculated property values. In addition to all expressions carried from patch 12, patch 13 adds:

| Expression | Scope | Description |
|---|---|---|
| `Offset3D.Z` | JBTEH_BAR (guy wire) | **New.** Calls `_JBTEH_BAR_Offset3DZ()` to place the guy wire at the correct elevation at each point along its length. |
| `TextAttribute_OBJEKTBESKRIVELSE.Justify` | ~18 ObjectTypes | **New.** Dynamically selects `"TopCenter"` or `"BottomCenter"` justification for the description label depending on whether the OBJEKTNAVN label is to the right of centre. |

Common existing expressions (unchanged from patch 12):

| Expression | Purpose |
|---|---|
| `Layer` | Layer name with construction phase (fasekode) suffix |
| `BaneNORBaneDataID` | BaneData object ID |
| `Name` | Object naming formula |
| `RelativeElevation` | Elevation relative to track |
| `DistanceToAlignment` | Lateral distance from object to its alignment |
| `SpanLength` | OCS span length between masts |
| Signal naming expressions | Signal number, litra, full name, short name |
| TextAttribute content expressions | Bindings for OBJEKTNAVN, OBJEKTID, OBJEKTBESKRIVELSE |
| Variant-specific expressions | Properties differing per object variant |

---

## Dock Points and Dynamic Snap Points

Object types expose **dock points** through `<DockPointDefinitions>` / `<SnapPoints>`. Each `<SnapPoint>` defines a position — in the object's local frame — that objects of a given `TargetSpace` can snap to.

- For an **alignment (line) object**, `X` is the distance measured **along the alignment** from its start point, `Y` is the lateral offset, and `Z` the vertical offset.
- For a **point object**, `X`/`Y`/`Z` are offsets from the object's insertion point.

```xml
<DockPointDefinitions>
    <SnapPoints>
        <SnapPoint X="0" Y="0" Z="0" TargetSpace="forankring" />
    </SnapPoints>
</DockPointDefinitions>
```

### Dynamic snap points (coordinate overrides)

Any axis of a `<SnapPoint>` can be computed at runtime with a Lua formula instead of a fixed attribute, using a `<XOverride>`, `<YOverride>`, or `<ZOverride>` child element wrapping a `<Formula>`. The formula returns the coordinate value and may reference object properties and `RcAlignment`. When an axis is overridden, omit its static attribute and supply the override child instead; un-overridden axes keep their attribute values.

```xml
<DockPointDefinitions>
    <SnapPoints>
        <SnapPoint X="0" TargetSpace="OhlAttachment">
            <YOverride>
                <Formula>return WireHeight</Formula>
            </YOverride>
        </SnapPoint>
    </SnapPoints>
</DockPointDefinitions>
```

This anchors a snap point to a value that varies per object — e.g. a wire's height, or the **end of a variable-length alignment**.

#### Usage in patch 13

The **JBTEH_BAR Bardun** (guy wire) and **JBTEH_STR Strever** (strut) each expose a dynamic dock point at their **foot end**, so their fastening footplates (festemekanismer — `JBTEH_BAF Bardunfeste` / `JBTEH_STF Streverfeste`, both in the `forankring` space) snap precisely to the end of the line regardless of its length:

```xml
<SnapPoint Y="0" Z="0" TargetSpace="forankring">
    <XOverride>
        <Formula>return RcAlignment.HorizontalGeometry.Length</Formula>
    </XOverride>
</SnapPoint>
```

---

## Show Layers

94 `<ShowLayer>` entries control layer group visibility from the RailCOMPLETE layer panel. Unchanged from patch 12.

### Common (COM_)
- Symbol frames (all types, unfinished, warning, error, historic)
- Historic objects
- Name, ID, and info display
- Reserved areas
- Track usage
- Schematic, cable, and isolation symbols
- Relation visualisation

### Track (TRK_)
- Geometry segments and transition points
- Mileage annotations
- Vertical profile
- Rail, ballast, and sleeper details

### OCS (OCS_)
- Mast and bracket details
- Contact wire segments and span information
- Tensioning and stagger (zigzag)
- Earthing systems and isolation zones

### Signalling (SIG_)
- Signals and balises
- Axle counters
- Sighting zones and braking curves
- Metal-free zones
- Toggle circles

### Signs/Boards (BNP_)
- Board and pole details
- Sighting requirement display
- Wipeout area display

### Low-Voltage & Telecom (POW_, TEL_)
- Name, ID, and info display

### All Disciplines (ALL_)
- Global name, ID, and info display toggle

---

## Property Overrides

51 property overrides configure how standard system properties are displayed and behave in the UI. Unchanged from patch 12.

**Identity and metadata:** `id`, `RcType`, `TimeStamp`, `additionalName`, `lang`, `Variant`, `Seq`, `code`, `name`, `Discipline`, `Stage`, `description`

**Alignment and position:** `Alignment`, `Mileage`, `ReferenceAlignment`, `ReferenceMileage`, `SideOfAlignment`, `dir`, `pos`, `DistanceToAlignment`, `RelativeElevation`, `TargetAlignment`

**Display:** `SymbolMode`, `DrawTail`, `DrawTailExtension`, `SymbolFrame`, `KmStigerMotVenstre`

**CAD properties:** `Color`, `Layer`, `Linetype`, `Lineweight`, `GlobalWidth`

**3D geometry:** `geoCoord`

**Variables:** `Var0` through `Var9`

**Formulas:** `LuaExpressions`

**railML properties:** `model`, `kind`, `derailSide`, `SwitchGeometry`, `ConnectionCourse`, `ContinuingWithBegin`, `ExtendingFromEnd`

---

## Property Categories

27 `<PropertyCategory>` entries group properties in the UI panel. Unchanged from patch 12.

| Category | Properties included |
|---|---|
| Document | File metadata |
| File | File path info |
| External Data Files | References to external data |
| Patch | Patch-specific properties |
| Basic | Core identity properties |
| General | Name, code, description |
| Aligned object | Alignment, mileage, side, distance, elevation |
| Custom properties | User-defined properties |
| Text Attributes | AutoCAD attribute text bindings |
| Model check | Model check results and symbol frame |
| Presentation | Symbol mode, tail, frame |
| CAD | Layer, colour, linetype, lineweight |
| 3D geometry | geoCoord and 3D positioning |
| Local variables | Var0–Var9 |
| Earthing | Earthing system properties |
| Watch text | Watch object display text |
| Relations | Object relationship properties |
| Attachment | Attachment/fastening properties |
| Sections | Alignment section properties |
| Formulas | LuaExpression assignments |
| Misc (railML and other) | railML and miscellaneous properties |
| Interlocking | Interlocking system properties |
| Alignment | Alignment-specific properties |
| Main signal (railML) | railML signal properties |
| Switch (railML) | railML switch properties |
| Connections | Track connection properties |

---

## Fouling Point Settings

4 configurations for fouling point (spormiddel) calculations. Unchanged from patch 12.

| # | Name | Scenario | Lua Function |
|---|---|---|---|
| 1 | Ny bane, tog/\* | New track, train operations | `NOBN_trk_getFoulingPointDistanceNewTrain()` |
| 2 | Ny bane, skift/skift | New track, shunting | `NOBN_trk_getFoulingPointDistanceNewShunting()` |
| 3 | Eks. bane, tog/\* | Existing track, train operations | `NOBN_trk_getFoulingPointDistanceExistingTrain()` |
| 4 | Eks. bane, skift/skift | Existing track, shunting | `NOBN_trk_getFoulingPointDistanceExistingShunting()` |

---

## Display Gauge Settings

8 track clearance gauge profiles. Unchanged from patch 12.

| Name | Description |
|---|---|
| Nye baner på fri linje | New lines, open track |
| Nye baner på stasjon | New lines, station area |
| A-85 | Rail profile standard A-85 |
| A-96 | Rail profile standard A-96 |
| A-96T | Rail profile standard A-96T (tunnel) |
| (additional variants) | Other standard profiles |

Each gauge is defined as a half-profile with a series of (X, Y) coordinate points representing the clearance envelope boundary.

---

## Position Tool Settings

Configuration for the position display panel. Unchanged from patch 12. Shows object position relative to:

**Own alignment:** Name, distance to alignment, relative elevation, mileage, position coordinates.

**Reference alignment:** Name, relative elevation, mileage, position coordinates.

Display items are prioritised, with the most relevant shown first.

---

## Default Settings for Commands

### Sighting Configuration (unchanged)
| Parameter | Value |
|---|---|
| Method | Line |
| Stop distance | Configured per signal type |
| Train window | Standard value |
| Precision | Standard precision parameters |

### Train Dimension Defaults (unchanged)
| Parameter | Value |
|---|---|
| Width | 3.4 m |
| Axle separation | 18 m |
| Nose length | 3 m |
| Tail length | 3 m |

### LandXML Import Defaults (new in patch 13)
| Parameter | Value |
|---|---|
| Default alignment type | `JBTKO_SPO Spor` |

When importing alignments from a LandXML file, `JBTKO_SPO Spor` (standard track) is used as the default object type.

---

## Default Interlocking Export Options

Unchanged from patch 12.

| Option | Description |
|---|---|
| Train route start/end filter | Lua expression filtering valid route endpoints |
| Shunting route filter | Lua expression filtering shunting route endpoints |
| Switch naming script | Script to assign names to switch objects in export |
| Extended route formatting | Formatting rules for extended route notation |
| Excluded conflicts | Handling of deliberately excluded route conflicts |
| Signal type filters | Lua filters for main signals, shunting signals, and ERTMS signals |

---

## DNA Identification

| Property | Value |
|---|---|
| DNA version | 2021.a (patch 13) |
| 2D symbol library | `..\NO-BN\2D\NO-BN-2021.a-2D.dwg` |
| 3D geometry folders | `STD-2021.a` and `STD-2025.a` |
| Layer mapping folder | `..\NO-BN\3D\LayerMappings` |
| Switch geometry file | `..\NO-BN\DNA\Switches\NO-BN-2021.a-SwitchGeometries.xml` |
| Default font | Arial (changed from iso3098 in patch 12) |
| Default style | RC-ARIAL (changed from iso in patch 12) |
