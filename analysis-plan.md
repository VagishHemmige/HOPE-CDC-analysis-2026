# Analysis plan — HOPE CDC Analysis 2026

| | |
|---|---|
| **Version** | 0.2 (draft for author review) |
| **Date** | 2026-10-24 |
| **Reporting frameworks** | STROBE (cross-sectional) with RECORD extensions; crosswalk in [Appendix A](#appendix-a-strobe-and-record-crosswalk) |
| **Authors** | [CONFIRM] |

Text marked **[CONFIRM]** needs an author decision or verification. Text marked **[OPEN]** is a choice not yet made.

---

## 1. Background and rationale

People living with HIV (PLWH) can now receive solid-organ transplants with acceptable outcomes in selected recipients, but transplant experience with HIV is concentrated at a minority of centers. The HIV Organ Policy Equity (HOPE) Act of 2013 permitted transplantation of organs from donors with HIV into recipients with HIV under research protocols. A U.S. Department of Health and Human Services final rule, effective November 27, 2024, removed the research and institutional review board requirement for **kidney and liver** HIV-positive-donor transplants only. Heart, lung and pancreas remain under research criteria.

Where centers sit relative to where PLWH live is a structural determinant of access. This study will describe that geography for five organs at two time points.

## 2. Objectives

### 2.1 Co-primary objectives

Both use the 60-minute driving isochrone as the reach definition, for each organ (kidney, liver, heart, lung, pancreas) and index year (2017, 2022).

- **P1 (HIV status).** Estimate the percentage of PLWH, and of people without HIV, living within 60 minutes' drive of an active adult transplant center, and the percentage-point difference between them (PLWH minus people without HIV).
- **P2 (center experience).** Among PLWH, estimate the percentage living within 60 minutes' drive of (a) any active center, (b) an HIV R+ center and (c) a HOPE D+ center, and the percentage-point reductions from (a) to (b) and from (a) to (c).

### 2.2 Secondary objectives

- **S-reach.** Repeat P1 and P2 using 50, 100 and 200 mile straight-line buffers.
- **S-change.** Describe the change in each primary and secondary estimate from 2017 to 2022.

### 2.3 Exploratory objective

- **E1.** Among active centers, compare the estimated number of PLWH within each center's own catchment across center status groups (HOPE D+, HIV R+ only, no HIV R+ transplants).

## 3. Study design

A repeated cross-sectional, population-level (ecological) geospatial study at two index years.

There are two units of analysis:

- **Census tract** for the primary and secondary objectives, carrying estimated counts of PLWH and people without HIV.
- **Transplant center** for the exploratory objective and for defining catchments.

No individual-level linkage between transplant recipients and population counts will be made. SRTR records will be used only to classify centers.

## 4. Setting

| Element | Definition |
|---|---|
| Geography | 48 contiguous states and the District of Columbia. Alaska, Hawaii, Puerto Rico and the territories are excluded from both population data and the center list. [CONFIRM: Alaska centers are excluded too] |
| Index years | 2017 and 2022 (calendar years) |
| Center-experience windows | Five calendar years ending in the index year: 2013–2017 and 2018–2022 |
| Isochrone reference dates | First Monday in March: 2017-03-06 and 2022-03-07, departing 07:00 local time |

Rationale for the index years: [CONFIRM — for example, alignment with ACS 5-year periods 2013–2017 and 2018–2022, and pre- and post-COVID comparison].

## 5. Data sources

| Source | Vintage | Geography | Use | Access conditions |
|---|---|---|---|---|
| SRTR Standard Analysis Files: kidney, liver, heart, lung and kidney-pancreas transplant files; deceased donor file | SAF release [CONFIRM] | Recipient and donor records, center code | Classifying centers | Restricted; SRTR data use agreement. No patient-level data leave the analysis environment. |
| HRSA transplant center list | Fetch date recorded with each reported run | Center point coordinates, organ services | Center locations | Public |
| American Community Survey (ACS) 5-year, total population (`B01003_001`) | 2013–2017 (for 2017) and 2018–2022 (for 2022) | Tract and county; 2010 tract boundaries for 2017, 2020 boundaries for 2022 | Tract and county population | Public |
| CDC NCHHSTP AtlasPlus, "HIV prevalence" | 2017 and 2022 | County | County PLWH counts | Public |
| Mapbox Isochrone API, `driving-traffic` profile | Computation date recorded | Center points | 60-minute catchments | Commercial API |
| Census cartographic boundary files (states) | 2022 | State | Maps only | Public |

Notes for interpretation:

- AtlasPlus prevalence counts persons with diagnosed HIV aged 13 and older [CONFIRM against AtlasPlus technical notes]. ACS `B01003_001` counts all ages (see L4).
- The HRSA center list reflects the date it is fetched, not the index years (see L5 and check V4).
- Mapbox routes on its road network and traffic model as of computation, not as of 2017 [CONFIRM how Mapbox treats past departure times]. Differences between the 2017 and 2022 isochrones will therefore reflect mainly which centers are included.

## 6. Study populations

### 6.1 Transplant centers

All definitions apply separately to each organ and index year. Centers are identified by SRTR center code, matched to the HRSA center code.

| Group | Definition | SRTR fields and values |
|---|---|---|
| **Base list** | HRSA centers listing the organ among their services, in the study geography | `Service_Lst` [CONFIRM that the match for "Heart" and "Pancreas" captures the intended programs] |
| **Active** | At least one adult transplant of that organ in the index calendar year | Adult: `REC_AGE_IN_MONTHS_AT_TX >= 216` (18 years); year of `REC_TX_DT` equals the index year |
| **HIV R+** | Active in the index year, **and** at least one adult recipient with HIV in the five-year window | `REC_HIV_STAT == "P"`; unknown, missing and not-done values count as not HIV-positive [CONFIRM coding] |
| **HOPE D+** | At least one adult recipient with HIV in the five-year window whose deceased donor was HIV-positive by NAT or antibody. Activity in the index year is **not** required. | Recipient as above; donor `DON_HIV_NAT` or `DON_ANTI_HIV` positive |

Decisions and rationale:

- **HOPE D+ centers will not be restricted to active centers.** Rationale: [CONFIRM — for example, HOPE experience reflects durable institutional capacity, and HOPE volumes are too small for one inactive year to signal lost capability]. Sensitivity analysis SA5 applies the restriction.
- **Living HIV-positive donors** [CONFIRM: whether they are included in HOPE D+].
- **Organ files.** Pancreas uses the kidney-pancreas file, which includes kidney-pancreas and pancreas-alone transplants [CONFIRM]. Whether simultaneous kidney-pancreas transplants also count toward kidney activity: [CONFIRM].

### 6.2 Population groups

| Group | Definition |
|---|---|
| **PLWH** | AtlasPlus county HIV prevalence count, allocated to tracts in proportion to tract population: `tract PLWH = county PLWH × tract population / county population` |
| **People without HIV** | Tract population minus tract PLWH |

The allocation assumes uniform prevalence within each county. Tract counts are estimates; only their sums over large areas will be interpreted.

## 7. Linkage

| ID | Linkage | Key | Method | Quality report |
|---|---|---|---|---|
| K1 | SRTR active centers → HRSA locations | Center code | Exact match | Per organ and year: active SRTR centers, number matched, unmatched centers with their transplant volumes |
| K2 | SRTR recipients → deceased donors | Donor ID | Left join | HIV R+ recipients by donor type (deceased matched, living, unmatched) |
| K3 | AtlasPlus counties → ACS counties | County FIPS | Left join from ACS | Per year: unmatched counties and their case counts, in each direction. Connecticut 2022 needs a stated rule, because ACS uses planning regions as county equivalents [CONFIRM which units AtlasPlus 2022 uses]. |
| K4 | Tracts → counties | First five digits of tract GEOID | Deterministic | All tracts assigned |

## 8. Reach definitions

| Definition | Role | Parameters |
|---|---|---|
| **60-minute isochrone** | **Primary** | Mapbox `driving-traffic`; 60 minutes; departure 07:00 local time on the reference date; each center's local time zone determined from its coordinates |
| 50-mile buffer | Secondary | Straight-line, 80,467.2 m, on EPSG:5070 (CONUS Albers equal-area) |
| 100-mile buffer | Secondary | 160,934.4 m |
| 200-mile buffer | Secondary | 321,868 m |

Every center in a group will receive a catchment under every definition (check V5). Mapbox limits isochrones to 60 minutes, so longer driving times are outside this design.

## 9. Outcome definitions

For each organ × year × center group × reach definition:

1. **Catchment.** The catchments of all centers in the group are dissolved into one area, so overlapping catchments count once.
2. **Tract inclusion.** A tract is within reach if any part of it intersects the catchment. SA3 tests a stricter rule.
3. **Numerator.** Sum of PLWH (or people without HIV) in tracts within reach.
4. **Denominator.** Sum of PLWH (or people without HIV) over all tracts in the study geography for that year.
5. **Percentage.** 100 × numerator ÷ denominator, reported to one decimal place.
6. **Contrasts.** Percentage-point differences for P1, P2 and the 2017 → 2022 change.

People without HIV are analyzed for active centers only (P1).

**Empty center groups.** When a group has no centers (likely for HOPE D+ heart, lung and pancreas), the numerator is 0 and the cell is reported as "0% (no centers)" with a footnote. Every results cell reports the number of centers it is based on.

## 10. Statistical methods

### 10.1 Primary and secondary objectives

Estimates are descriptive population quantities. No hypothesis tests are planned for P1, P2, S-reach or S-change.

No confidence intervals will be reported. The inputs are a near-complete enumeration of centers and population-level surveillance and census estimates, not a sample. The main uncertainty is structural (allocation, reach definition, suppression) and is addressed by the sensitivity analyses. ACS margins of error will not be propagated. [OPEN: whether to report the range across sensitivity analyses alongside each headline estimate.]

### 10.2 Exploratory objective (E1)

- For each active center, sum PLWH over tracts intersecting that center's own catchment (not the dissolved catchment).
- Group centers by status in each index year: HOPE D+, HIV R+ only, no HIV R+ transplants. Only active centers enter this analysis, so HOPE D+ centers inactive in the index year are excluded.
- Display as beeswarm and violin plots with medians, by year.
- Compare groups with a Kruskal–Wallis test.

Interpretation constraints:

- The test is exploratory and descriptive. Center values are not independent: overlapping catchments share tracts, and the same centers appear in both years.
- Up to 40 tests (5 organs × 4 reach definitions × 2 years) will be run without multiplicity adjustment. P-values are descriptive only.
- No causal interpretation will be made about whether local PLWH population drives HIV transplant experience, or the reverse.

## 11. Missing, suppressed and invalid data

| Issue | Handling |
|---|---|
| **CDC-suppressed county counts** | Filled with best estimates through the planned `CDCAtlas` update. Report per year the number of suppressed counties and the estimated PLWH assigned to them. Zero-fill is SA2. |
| Unmatched counties (K3) | Reported with their counts; resolved case by case, with each resolution recorded |
| Zero-population tracts or counties | Zero-population tracts contribute zero. Confirm that no county has zero population. |
| Recipient HIV status missing or unknown | Treated as not HIV-positive; the number of such adult recipients reported per organ and window |
| Centers missing coordinates or not linked | Reported (K1) and excluded |
| Invalid catchment geometries | Checked and repaired before joining (V9) |

## 12. Sensitivity analyses

| ID | Question | Variation |
|---|---|---|
| SA1 | Does the reach definition change conclusions? | 50, 100 and 200 mile buffers (the secondary objectives) |
| SA2 | Does suppression handling matter? | Zero-fill instead of best-estimate fill |
| SA3 | Does the tract inclusion rule matter? | Tract centroid inside the catchment, instead of any intersection [OPEN: geometric or population-weighted centroid] |
| SA4 | Does the within-county allocation matter? | Population-weighted areal interpolation [OPEN] |
| SA5 | Does the HOPE D+ scope matter? | HOPE D+ centers restricted to those active in the index year |
| SA6 | Does the population comparison depend on age? | People without HIV restricted to ages 13 and older, to match AtlasPlus [OPEN] |

## 13. Anticipated limitations

- **L1. Ecological allocation.** Tract PLWH counts assume uniform prevalence within counties; within-county clustering is not represented.
- **L2. Diagnosed HIV only.** Undiagnosed PLWH are not counted.
- **L3. Residence, not candidacy.** The population is all PLWH, not those with end-stage organ disease or on a waiting list. Estimates describe potential geographic access, not need.
- **L4. Age mismatch.** PLWH counts cover ages 13 and older [CONFIRM]; people without HIV include all ages; centers are defined by adult activity. SA6 addresses this.
- **L5. Center list currency.** Current HRSA locations are applied to past years; centers that have since closed may be missing.
- **L6. Routing assumptions.** One departure time and day, the current road network for both years, and driving only. Transit, cost and referral pathways are not considered.
- **L7. Experience as a proxy.** At least one HIV R+ transplant in five years does not measure a program's current willingness or capacity to evaluate PLWH.
- **L8. SRTR HIV coding.** Recipient and donor HIV fields may be incomplete or misclassified.
- **L9. Boundary changes.** 2017 and 2022 use different tract vintages and, in Connecticut, different county equivalents. Cross-year comparisons are made at the national level only.

## 14. Validation and reconciliation checks

Reported in a supplementary table, each with its expected result.

| ID | Check | Expected result |
|---|---|---|
| V1 | Total tract PLWH vs. total county PLWH over matched counties, per year | Equal within rounding |
| V2 | Total tract PLWH + people without HIV vs. ACS total population, per year | Equal, apart from unmatched counties |
| V3 | County linkage (K3) | Unmatched counties listed and explained |
| V4 | Center linkage (K1) | Unmatched active centers listed with volumes |
| V5 | Catchments vs. centers, per group, organ, year and reach definition | One catchment per center |
| V6 | Groups with zero centers | Percentage 0, not 100 |
| V7 | Ordering | 50 ≤ 100 ≤ 200 mile within each group; HIV R+ ≤ active within each reach definition. HOPE D+ need not be ≤ active. |
| V8 | Results grid | 120 PLWH estimates and 40 estimates for people without HIV |
| V9 | Geometry validity | All catchments valid |

## 15. Planned outputs

- **Table 1.** Per organ and year: number of centers in each group; total PLWH and total population.
- **Center flow diagram** per organ and year: HRSA base list → linked to SRTR → active → HIV R+, with HOPE D+ shown separately.
- **Results tables.** P1 and P2 at the primary reach definition with contrasts; secondary reach definitions in supplementary tables.
- **Maps.** Paired 2017 and 2022 maps of catchments over a PLWH dot-density layer.
- **Exploratory figure.** Beeswarm plots for E1.
- **Supplementary tables.** Linkage quality (K1–K3), suppression (Section 11), validation checks (Section 14), sensitivity analyses (Section 12).

## 16. Reproducibility and data access

- **Code:** <https://github.com/VagishHemmige/HOPE-CDC-analysis-2026>. Each reported run cites its commit hash.
- **Investigator access:** [CONFIRM the analysts' level of access to SRTR data under the data use agreement].
- **Restricted data:** SRTR files are not shared. Only center-level aggregates are published.
- **Live inputs:** dated snapshots of the HRSA list and AtlasPlus data are saved with each reported run.
- **Cached inputs:** build dates of ACS pulls and Mapbox isochrones are recorded with each reported run.
- **Software:** R and package versions recorded with each reported run [OPEN: whether to adopt `renv`].

## 17. Amendments

Changes to this plan after version 1.0 are recorded here with date, change and reason.

| Date | Version | Change | Reason |
|---|---|---|---|
| | | | |

---

## Appendix A. STROBE and RECORD crosswalk

S = STROBE item; R = RECORD extension. The manuscript column is filled in at submission.

| Item | Topic | Plan section | Manuscript | Notes |
|---|---|---|---|---|
| S1a / S1b | Design in title; informative abstract | 3 | | |
| R1.1 | Type of data in title or abstract | 5 | | Name SRTR, AtlasPlus, ACS |
| R1.2 | Geographic region and time frame | 4 | | |
| R1.3 | Linkage stated | 7 | | |
| S2 | Background and rationale | 1 | | |
| S3 | Objectives | 2 | | |
| S4 | Study design | 3 | | |
| S5 | Setting, locations, dates | 4 | | |
| S6a | Eligibility and sources | 6 | | Centers and populations |
| R6.1 | Codes or algorithms to select the population | 6.1, 6.2 | | |
| R6.2 | Validation of those codes | 6.1, 13 (L8) | | [CONFIRM any published validation of SRTR HIV fields] |
| R6.3 | Linkage flow diagram | 7, 15 | | Center flow diagram |
| S7 | Variables | 2, 8, 9 | | Exposure: HIV status (P1) or center group (P2) |
| R7.1 | Complete list of codes and algorithms | 6.1, 8 | | |
| S8 | Data sources and measurement | 5 | | |
| S9 | Efforts to address bias | 11, 12, 13 | | Framed as threats to the estimate |
| S10 | Study size | 15 (Table 1) | | No power calculation; counts reported |
| S11 | Quantitative variables | 8, 9 | | |
| S12a | Statistical methods | 9, 10 | | |
| S12b | Subgroups and interactions | 2 | | By organ and year |
| S12c | Missing data | 11 | | |
| S12d | Sampling strategy | 3, 10.1 | | Not applicable: no sampling |
| S12e | Sensitivity analyses | 12 | | |
| R12.1 | Investigator access to the database | 16 | | |
| R12.2 | Data cleaning | 6, 11 | | |
| R12.3 | Linkage methods and quality | 7, 14 | | |
| S13a–c | Numbers at each stage; flow diagram | 15 | | Center flow |
| R13.1 | Selection, including data quality and linkage | 7, 14 | | |
| S14a | Characteristics of the study population | 15 (Table 1) | | |
| S14b | Missing data per variable | 11 | | |
| S15 | Outcome data | 9 | | |
| S16a | Estimates and precision | 9, 10.1 | | No adjustment; no confidence intervals, with reason |
| S16b–c | Category boundaries; absolute measures | 8 | | |
| S17 | Other analyses | 10.2, 12 | | |
| S18 | Key results | — | | |
| S19 | Limitations | 13 | | |
| R19.1 | Limitations of routinely collected data | 13 | | |
| S20 | Interpretation | 10, 13 | | |
| S21 | Generalisability | 13 | | |
| S22 | Funding | — | | [CONFIRM] |
| R22.1 | Access to protocol, data and code | 16 | | |

## References

- von Elm E, et al. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement. *PLoS Med.* 2007;4(10):e296.
- Benchimol EI, et al. The REporting of studies Conducted using Observational Routinely-collected health Data (RECORD) statement. *PLoS Med.* 2015;12(10):e1001885.
- HHS final rule removing clinical research and IRB requirements for HIV-positive-to-HIV-positive kidney and liver transplantation, effective November 27, 2024 [CONFIRM Federal Register citation].
