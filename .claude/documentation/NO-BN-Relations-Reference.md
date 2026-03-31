# NO-BN Relations Reference

> Source file: `NO-BN/DNA/_SRC/NO-BN-Relations.xml`
> Branch: `Demo-DNA` (condensed/translated version)

This document charts every relation defined in the DNA. Relations connect railway object types
to each other (e.g. a cantilever *belongs to* a pole, a balise *belongs to* a balise group).

---

## How to Read the Tables

| Column | Meaning |
|--------|---------|
| **Relation** | English display name shown to the user |
| **Forward Lua** | Lua constant for the forward direction (source -> target) |
| **Reverse Lua** | Lua constant for the reverse direction (target -> source) |
| **Source** | Source `RcType`(s) |
| **Target** | Target `RcType`(s) |
| **Cardinality** | `Fwd min–max / Rev min–max` — how many targets per source, and vice versa |

---

## 1. Switches

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 1 | Switch–switch coupling | `rel_Switch_IsCoupledWith_Switch` | *(same, symmetric)* | sporveksel | sporveksel | 0–1 / 0–1 |
| 2 | Single/double slip mechanically coupled blade set | `rel_Switch_HasTonguesBeingMechanicallyCoupledWithTonguesIn_Switch` | *(same, symmetric)* | sporveksel | sporveksel | 0–1 / 0–1 |
| 3 | Switch–derailer coupling | `rel_Switch_IsCoupledWith_Derailer` | `rel_Derailer_IsCoupledWith_Switch` | sporveksel | sporsperre | 0–1 / 0–1 |
| 4 | Switch board for movable nose | `rel_Switch_Has_BoardOfTypeMoveablePointFrog` | `rel_BoardOfTypeMoveablePointFrog_AppliesTo_Switch` | sporveksel | skilt | 0–3 / 0–1 |

---

## 2. Switch Tongues

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 5 | Switch blades belong to switch | `rel_SwitchTongues_BelongTo_Switch` | `rel_Switch_Has_SwitchTongues` | sporvekseltunger | sporveksel | 1–1 / 1–1 |

---

## 3. Snow-Clearing Protection

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 6 | Deflection bar protects axle counter | `rel_DeflectionBar_Protects_AxleCounter` | `rel_AxleCounter_IsProtectedBy_DeflectionBar` | oppkjørsbjelke | tellepunkt | 0–∞ / 0–1 |

---

## 4. High Voltage / Catenary — HV Switch

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 7 | OCS switch actuation | `rel_HvSwitch_Has_HvSwitchActuator` | `rel_HvSwitchActuator_Actuates_HvSwitch` | kl-bryter | manøvermaskin | 0–1 / 1–1 |
| 8 | OCS switch installed on pole | `rel_HvSwitch_IsInstalledOn_OcsPole` | `rel_OcsPole_IsInstallationTargetFor_HvSwitch` | kl-bryter | kl-mast | 1–1 / 0–∞ |
| 9 | OCS switch interrupts HV conductor | `rel_HvSwitch_Interrupts_HvConductor` | `rel_HvConductor_IsInterruptedBy_HvSwitch` | kl-bryter | høyspentledning | 0–2 / 0–1 |

---

## 5. Catenary — Anchoring / Consoles

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 10 | Console on pole installed on pole | `rel_ConsoleOnOcsPole_IsInstalledOn_OcsPole` | `rel_OcsPole_IsInstallationTargetFor_ConsoleOnOcsPole` | konsoll_på_mast | kl-mast | 1–1 / 0–2 |
| 11 | Console anchors guy wire / spanner / WTB | `rel_ConsoleOnOcsPole_Anchors_GuyWireOrSpannerOrWtb` | `rel_GuyWireOrSpannerOrWtb_IsAttachedTo_ConsoleOnOcsPole` | konsoll_på_mast | avspenning, kl-bardun, kl-strever | 1–∞ / 0–1 |
| 12 | Guy wire ground anchorage | `rel_GuyWireFootplate_Anchors_GuyWire` | `rel_GuyWire_IsAnchoredBy_GuywireFootplate` | bardun_fotplate | kl-bardun | 1–1 / 1–1 |
| 13 | Spanner ground anchorage | `rel_SpannerFootplate_Anchors_Spanner` | `rel_Spanner_IsAnchoredBy_SpannerFootplate` | strever_fotplate | kl-strever | 1–1 / 1–1 |
| 14 | Tunnel fastener anchors guy wire / spanner / WTB | `rel_TunnelFootplate_Anchors_GuyWireOrSpannerOrWtb` | `rel_GuyWireOrSpannerOrWtb_IsAnchoredBy_TunnelFootplate` | tunnelfeste | avspenning, kl-bardun, kl-strever | 1–1 / 0–1 |

---

## 6. Catenary — Cantilevers

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 15 | Cantilever belongs to pole | `rel_Cantilever_BelongsTo_OcsPole` | `rel_OcsPole_Has_Cantilever` | utligger | kl-mast | 1–1 / 0–6 |
| 16 | Cantilever holds contact wire | `rel_Cantilever_Holds_ContactWire` | `rel_ContactWire_IsHeldBy_Cantilever` | utligger | kontaktledning | 1–1 / 0–∞ |
| 17 | Cantilever / WTB sequence | `rel_CantileverOrWtb_HasPrevious_CantileverOrWtb` | `rel_CantileverOrWtb_HasNext_CantileverOrWtb` | utligger, avspenning | utligger, avspenning | 0–1 / 0–1 |

---

## 7. Catenary — Wire Clamps

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 18 | Wire clamp belongs to cantilever | `rel_OcsWireClamp_BelongsTo_Cantilever` | `rel_Cantilever_Has_OcsWireClamp` | kontakttrådklemme | utligger | 1–1 / 0–1 |
| 19 | Wire clamp holds contact wire | `rel_OcsWireClamp_Holds_ContactWire` | `rel_ContactWire_IsHeldBy_OcsWireClamp` | kontakttrådklemme | kontaktledning | 1–1 / 0–∞ |

---

## 8. Catenary — Multi-Cantilever Console

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 20 | Multi-cantilever console installed on pole | `rel_MultiCantileverConsole_IsInstalledOn_OcsPole` | `rel_OcsPole_IsInstallationTargetFor_MultiCantileverConsole` | seksjonsutliggerkonsoll | kl-mast | 1–1 / 0–2 |
| 21 | Multi-cantilever console mounts cantilever | `rel_MultiCantileverConsole_IsMountingSurfaceFor_Cantilever` | `rel_Cantilever_IsMountedOn_MultiCantileverConsole` | seksjonsutliggerkonsoll | utligger | 2–2 / 0–1 |

---

## 9. Catenary — Wire Tension Balancers (WTB)

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 22 | WTB tensions wire / line / HV conductor | `rel_Wtb_Tensions_ContactWireOrMidpointAnchoringLineOrHvConductor` | `rel_ContactWireOrMidpointAnchoringLineOrHvConductor_IsTensionedBy_Wtb` | avspenning | kontaktledning, fixline, høyspentledning | 1–1 / 1–3 |

---

## 10. Catenary — Midpoint Anchor

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 23 | Midpoint anchor locks cantilever | `rel_MidpointAnchor_Locks_Cantilever` | `rel_Cantilever_IsLockedBy_MidpointAnchor` | fixpunkt | utligger | 1–1 / 0–1 |
| 24 | Midpoint anchor anchored by line | `rel_MidpointAnchor_IsAnchoredBy_MidpointAnchoringLine` | `rel_MidpointAnchoringLine_Anchors_MidpointAnchor` | fixpunkt | fixline | 1–1 / 1–1 |

---

## 11. Catenary — OCS Poles

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 25 | Drop-arm pole under portal | `rel_OcsPole_IsDropArmUnder_OcsPortal` | `rel_OcsPortal_IsDropArmSupportFor_OcsPole` | kl-mast | åk | 0–1 / 0–∞ |
| 26 | OCS pole sequence | `rel_OcsPole_HasPrevious_OcsPole` | `rel_OcsPole_HasNext_OcsPole` | kl-mast | kl-mast | 0–∞ / 0–∞ |
| 27 | OCS portal pole support | `rel_OcsPole_IsSupportFor_OcsPortal` | `rel_OcsPortal_IsSupportedBy_OcsPole` | kl-mast | åk | 0–∞ / 0–2 |

---

## 12. Catenary — OCS Foundations

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 28 | Pole / anchorage has foundation | `rel_OcsPoleOrAnchoringFootplate_Has_OcsFoundation` | `rel_OcsFoundation_AppliesTo_OcsPoleOrAnchoringFootplate` | kl-mast, bardun_fotplate, strever_fotplate, tunnelfeste | kl-fundament | 0–1 / 1–1 |

---

## 13. Catenary — Contact Wire Annotation

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 29 | Wire change annotation applies to contact wire | `rel_OcsWireChangeAnnotation_AppliesTo_ContactWire` | `rel_ContactWire_Has_OcsWireChangeAnnotation` | kl-annotering | kontaktledning | 0–2 / 0–2 |

---

## 14. Signalling — Cabinet

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 30 | Cabinet houses wiring connections | `rel_Cabinet_HousesWiringConnectionsFor_Object` | `rel_Object_HasWiringConnectionsHousedBy_Cabinet` | apparatskap, kabelboks | signal, NSS_balise, ETCS_balise, sporvekseldrivmaskin, sporsperredrivmaskin, lokalstiller | 0–∞ / 0–1 |

---

## 15. Signalling — Signals

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 31 | Distant signal announces signal | `rel_Signal_IsAnnouncedBy_Signal` | `rel_Signal_Announces_Signal` | signal | signal | 0–∞ / 0–∞ |
| 32 | Track signal applies to signal | `rel_Signal_Has_TrackSignal` | `rel_TrackSignal_AppliesTo_Signal` | signal | signal | 0–1 / 0–∞ |
| 33 | Signal has foundation | `rel_Signal_Has_SignalFoundation` | `rel_SignalFoundation_AppliesTo_Signal` | signal | signalfundament | 0–1 / 1–1 |

---

## 16. Signalling — Axle Counters

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 34 | Axle counter at signal | `rel_AxleCounter_AppliesTo_Signal` | `rel_Signal_Has_AxleCounter` | tellepunkt | signal | 0–2 / 0–1 |
| 35 | Axle counter located by marker pole | `rel_AxleCounter_IsLocatedAt_MarkerPole` | `rel_MarkerPole_Locates_AxleCounter` | tellepunkt | stolpe | 0–1 / 0–∞ |

---

## 17. Signalling — Point Machines

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 36 | Point machine belongs to switch | `rel_PointMachine_BelongsTo_Switch` | `rel_Switch_Has_PointMachine` | sporvekseldrivmaskin | sporveksel | 1–1 / 1–9 |

---

## 18. Signalling — Derailer Machines

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 37 | Derailer machine belongs to derailer | `rel_DerailerMachine_BelongsTo_Derailer` | `rel_Switch_Has_DerailerMachine` | sporsperredrivmaskin | sporsperre | 1–1 / 0–1 |

---

## 19. Signalling — Local Control Panel

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 38 | Local control panel actuates switch/derailer | `rel_LocalControlPanel_Actuates_SwitchOrDerailer` | `rel_SwitchOrDerailer_IsActuatedBy_LocalControlPanel` | lokalstiller | sporveksel, sporsperre | 1–1 / 0–1 |

---

## 20. ETCS (European Train Control System)

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 39 | ETCS balise belongs to group | `rel_EtcsBalise_BelongsTo_EtcsBaliseGroup` | `rel_EtcsBaliseGroup_Contains_EtcsBalise` | ETCS_balise | ETCS_balisegruppe | 1–1 / 1–8 |
| 40 | ETCS balise controlled by LEU | `rel_EtcsBalise_IsControlledBy_EtcsLinesideElectronicUnit` | `rel_EtcsLinesideElectronicUnit_Controls_EtcsBalise` | ETCS_balise | ETCS_LEU | 0–1 / 1–4 |
| 41 | ETCS group gets control info from object | `rel_EtcsBaliseGroup_ObtainsControlInformationFrom_Object` | `rel_Object_ProvidesControlInformationTo_EtcsBaliseGroup` | ETCS_balisegruppe | signal, ertms_marker_board, ertms_shunting_signal, skilt, stolpe, sporveksel | 0–∞ / 0–∞ |
| 42 | ETCS group applies to object | `rel_EtcsBaliseGroup_AppliesTo_Object` | `rel_Object_Has_EtcsBaliseGroup` | ETCS_balisegruppe | signal, ertms_marker_board, ertms_shunting_signal, skilt, stolpe, sporveksel | 0–∞ / 0–∞ |
| 43 | ETCS linking chain | `rel_EtcsBaliseGroup_LinksTo_EtcsBaliseGroup` | `rel_EtcsBaliseGroup_IsLinkedToFrom_EtcsBaliseGroup` | ETCS_balisegruppe | ETCS_balisegruppe | 0–∞ / 0–∞ |
| 44 | ETCS braking curve | `rel_EtcsBaliseGroup_HasBrakingCurveTargetIn_EtcsBaliseGroup` | `rel_EtcsBaliseGroup_IsBrakingCurveTargetFor_EtcsBaliseGroup` | ETCS_balisegruppe | ETCS_balisegruppe | 0–∞ / 0–∞ |
| 45 | ETCS code table depends on group | `rel_EtcsCodeTable_DependsOn_EtcsBaliseGroup` | `rel_EtcsBaliseGroup_Affects_EtcsCodeTable` | ETCS_balisegruppe | ETCS_kodetabell | 0–1 / 1–∞ |

---

## 21. NSS (National Signalling System)

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 46 | NSS balise belongs to group | `rel_NssBalise_BelongsTo_NssBaliseGroup` | `rel_NssBaliseGroup_Contains_NssBalise` | NSS_balise | NSS_balisegruppe | 1–1 / 2–4 |
| 47 | NSS group position determined by balise | `rel_NssBaliseGroup_HasPositionDeterminedBy_NssBalise` | `rel_NssBalise_DeterminesPositionFor_NssBaliseGroup` | NSS_balisegruppe | NSS_balise | 1–1 / 0–1 |
| 48 | NSS group applies to object | `rel_NssBaliseGroup_AppliesTo_Object` | `rel_Object_Has_NssBaliseGroup` | NSS_balisegruppe | signal, skilt, stolpe, sporveksel | 1–∞ / 0–∞ |
| 49 | NSS group gets control info from object | `rel_NssBaliseGroup_ObtainsControlInformationFrom_Object` | `rel_Object_ProvidesControlInformationTo_NssBaliseGroup` | NSS_balisegruppe | signal, skilt, stolpe, sporveksel | 0–∞ / 0–∞ |
| 50 | NSS linking chain | `rel_NssBaliseGroup_LinksTo_NssBaliseGroup` | `rel_NssBaliseGroup_IsLinkedToFrom_NssBaliseGroup` | NSS_balisegruppe | NSS_balisegruppe | 0–∞ / 0–∞ |
| 51 | NSS braking curve | `rel_NssBaliseGroup_HasBrakingCurveTargetIn_NssBaliseGroup` | `rel_NssBaliseGroup_IsBrakingCurveTargetFor_NssBaliseGroup` | NSS_balisegruppe | NSS_balisegruppe | 0–∞ / 0–∞ |
| 52 | NSS code table depends on group | `rel_NssCodeTable_DependsOn_NssBaliseGroup` | `rel_NssBaliseGroup_Affects_NssCodeTable` | NSS_balisegruppe | NSS_kodetabell | 0–1 / 1–∞ |

---

## 22. Common / Generic Relations

| # | Relation | Forward Lua | Reverse Lua | Source | Target | Cardinality |
|---|----------|-------------|-------------|--------|--------|-------------|
| 53 | Marker applies to anything | `rel_Marker_AppliesTo_Anything` | `rel_Anything_Has_Marker` | markør | *(anything)* | 0–∞ / 0–∞ |
| 54 | Label applies to anything | `rel_Label_AppliesTo_Anything` | `rel_Anything_Has_Label` | etikett | *(anything)* | 0–∞ / 0–∞ |
| 55 | Watch applies to anything | `rel_Watch_AppliesTo_Anything` | `rel_Anything_Has_Watch` | watch | *(anything)* | 0–∞ / 0–∞ |
| 56 | Balloon relates to anything | `rel_Balloon_RelatesTo_Anything` | `rel_Anything_IsRelatedTo_Balloon` | ballong | *(anything)* | 0–∞ / 0–∞ |

---

## Commented-Out / Inactive Relations

These relations exist in the XML but are commented out. They are **not active** in the current DNA.

| # | Relation | Reason |
|---|----------|--------|
| A | Board "installed on" (skilt, ertms_board) | TODO — "Should this be included?" |
| B | Derailed axle indicator signals set to stop | Condensed DNA: `rctype_DerailedAxleIndicator` removed |
| C | Axle counter tuning unit | Condensed DNA: `rctype_AxleCounterTuningUnit` removed |
| D | FTGS bond at signal | Condensed DNA: `rctype_FtgsBond` removed |
| E | FTGS tuning unit | Condensed DNA: `rctype_FtgsTuningUnit` removed |
| F | ETCS group position determined by balise | Not needed — position is always the first balise |
| G | HV switch actuation / pole / conductor (3 relations) | Condensed DNA: `rctype_HvSwitch` and `rctype_HvSwitchActuator` removed |
| H | Guy wire ground anchorage | Condensed DNA: `rctype_GuywireFootplate` removed |
| I | Spanner ground anchorage | Condensed DNA: `rctype_SpannerFootplate` removed |
| J | Tunnel fastener anchorage | Condensed DNA: `rctype_TunnelWallFixing` removed |
| K | Wire clamp cantilever / wire (2 relations) | Condensed DNA: `rctype_OcsWireClamp` removed |
| L | WTB wire/line association | Condensed DNA: `rctype_WireTensioningBalancer` removed |
| M | Midpoint anchor cantilever / line (2 relations) | Condensed DNA: `rctype_MidpointAnchor` removed |
| N | Contact wire change annotation | Condensed DNA: `rctype_ContactWireChangeAnnotation` removed |
| O | ETCS balise LEU association | `ETCS_LEU` has no ObjectType definition |
| P | ETCS code table association | `ETCS_kodetabell` has no ObjectType definition |
| Q | NSS code table association | `NSS_kodetabell` has no ObjectType definition |

**Also modified (not fully commented out):**
- Console on pole: removed `avspenning` and `kl-strever` TargetSpaces (types removed)
- Cantilever sequence: removed `avspenning` from Source/TargetSpaces (type removed)
- OCS foundation: removed `bardun_fotplate`, `strever_fotplate`, `tunnelfeste` SourceSpaces (types removed)
- Cabinet association: removed `kabelboks` SourceSpace (type removed)

---

## Summary

| Category | Count |
|----------|-------|
| **Active relations** | 39 |
| **Commented-out relations** | 23 |
| **Sections** | 22 |
| Switches | 4 |
| Switch Tongues | 1 |
| Snow-Clearing | 1 |
| HV Switch | ~~3~~ 0 (commented out) |
| Catenary — Anchoring/Consoles | ~~5~~ 2 (3 blocks commented out, 2 modified) |
| Catenary — Cantilevers | ~~3~~ 2 (sequence modified) |
| Catenary — Wire Clamps | ~~2~~ 0 (commented out) |
| Catenary — Multi-Cantilever Console | 2 |
| Catenary — WTB | ~~1~~ 0 (commented out) |
| Catenary — Midpoint Anchor | ~~2~~ 0 (commented out) |
| Catenary — OCS Poles | 3 |
| Catenary — OCS Foundations | 1 (modified) |
| Catenary — Contact Wire Annotation | ~~1~~ 0 (commented out) |
| Signalling — Cabinet | 1 (modified) |
| Signalling — Signals | 3 |
| Signalling — Axle Counters | 2 |
| Signalling — Point Machines | 1 |
| Signalling — Derailer Machines | 1 |
| Signalling — Local Control Panel | 1 |
| ETCS | ~~7~~ 5 (LEU and code table commented out) |
| NSS | ~~7~~ 6 (code table commented out) |
| Common / Generic | 4 |
