# Analysis plan — HOPE CDC Analysis 2026

**Status:** Draft reflecting research decisions agreed through October 24, 2026. Methodological commitments below do not establish that the corresponding code has been implemented or that analyses have been reproduced.

## 1. Background and rationale

People living with HIV (PLWH) may benefit from solid-organ transplantation, but HIV-related transplant experience varies across centers. The HIV Organ Policy Equity (HOPE) Act created a pathway for transplantation from donors with HIV to recipients with HIV. Regulatory requirements differ by organ and calendar period, and geographic proximity to a center does not establish clinical eligibility or program willingness to accept a candidate.

This study describes the geographic distribution of access to adult solid-organ transplant centers among PLWH and people without HIV, and distinguishes access to any active center from access to centers with documented HIV recipient or HIV-positive donor experience.

## 2. Objectives

### 2.1 Co-primary descriptive objectives

The primary geographic reach measure is a 60-minute driving isochrone. Each objective is evaluated separately for kidney, liver, heart, lung and pancreas, in 2017 and 2022.

- **P1 — Access by HIV status:** Estimate the percentage of PLWH and the percentage of people without HIV residing within the catchment of any active adult transplant center, and their difference in percentage points (PLWH minus people without HIV).
- **P2 — Access by center experience:** Among PLWH, estimate the percentages residing within reach of (a) any active adult center, (b) an active HIV R+ center and (c) an active HOPE D+ center, and the percentage-point differences between these categories.

### 2.2 Secondary objectives

- **S1 — Alternative reach distances:** Repeat P1 and P2 with 50-, 100- and 200-mile straight-line catchments.
- **S2 — Temporal comparison:** Describe 2017-to-2022 differences in each estimate **in percentage points**, not relative percentage change. The two years are snapshots, not a longitudinal trend series.
- **S3 — Population stratification:** Estimate geographic coverage by race/ethnicity, sex, age group and geographic region where comparable HIV surveillance counts and corresponding population denominators support meaningful estimates. Document unavailable or suppressed strata rather than treating missing values as zero. Stratified comparisons are descriptive and conditional on data feasibility and quality.
- **S4 — Potential center expansion:** Among active centers without documented HIV R+ transplantation in the relevant five-year window, estimate the incremental number and percentage of PLWH who would lie within reach if a candidate center began offering HIV transplant services. Identify high-yield candidates by additional population covered beyond the existing HIV R+ network, accounting for overlap. This models hypothetical expansion of service availability, not actual feasibility or uptake.

### 2.3 Exploratory objective

Describe the estimated number of PLWH within individual active centers' catchments according to center experience category: HOPE D+, HIV R+ without HOPE D+ experience, and no documented HIV R+ experience. This is descriptive, not inferential.

## 3. Study design and setting

Repeated cross-sectional, ecological geospatial analysis of the 48 contiguous states and Washington, DC. Alaska, Hawaii, Puerto Rico and other territories are outside the study geography for both population and center analyses.

The index years are 2017 and 2022. Historical center experience is defined within five calendar years ending in each index year (2013–2017 and 2018–2022). Population estimates correspond to each index year. Geographic coverage is attributed to residential areas, not to individual patients or observed transplant referrals. SRTR patient-level records are used only to classify centers and derive experience measures; there is no patient-level linkage to community HIV surveillance data.

## 4. Data sources and measurement periods

| Source | Time basis and intended contribution |
|---|---|
| SRTR transplant and relevant donor records | Historical activity and transplant experience for each organ and five-year window; restricted patient-level source |
| Historical transplant-center records, including HRSA/OPTN or comparable authoritative sources | Center identity, organ service and **location at the applicable index year**; historical reconstruction is required |
| CDC NCHHSTP AtlasPlus HIV prevalence | County counts of diagnosed PLWH corresponding to each index year; verify age coverage and compatible state totals |
| American Community Survey 5-year estimates | County and tract populations for 2017 and 2022; age 13+ for the primary analysis, and all ages for a sensitivity analysis |
| Mapbox or comparably specified road-routing source | Modeled 60-minute driving catchments using a common contemporary road-network methodology for both historical snapshots |
| Census geographic boundaries | Appropriate county/tract geographic vintages; changes in geographic definitions, including Connecticut, require reconciliation |

The primary analysis requires index-year center locations, but **does not require a historical road-network reconstruction**. Contemporary road-network and traffic assumptions applied consistently to the two years should be disclosed as a modeling limitation. Source vintages and retrieval assumptions must be sufficiently documented to interpret the findings.

## 5. Study populations and transplant-center definitions

### 5.1 Transplant-center cohorts

Definitions apply separately by organ and index year. All three analytic center categories are nested:

1. **Active:** A geographically eligible program with at least one qualifying adult transplant of that organ in the index calendar year (adult defined as age 18+ at transplant).
2. **HIV R+:** An active center with at least one qualifying adult recipient living with HIV transplanted in the five-year window ending in the index year.
3. **HOPE D+:** An active HIV R+ center with at least one qualifying adult HIV D+/R+ transplant using a **deceased donor with HIV** in the same five-year window.

Thus, **HOPE D+ ⊆ HIV R+ ⊆ active**. HIV-positive living-donor transplants are not included in the HOPE D+ classification. Because such donations are uncommon, omission is expected to have little impact on center classification, but the extent of any difference can be checked if reliably identifiable data are available. Confirm as a simple validation check whether restricting HOPE D+ centers to index-year-active programs excludes any programs; do not treat a null difference as a standalone sensitivity analysis.

**Organ definitions:** Kidney activity and experience include qualifying kidney-alone and simultaneous kidney-pancreas transplants. Pancreas activity and experience include qualifying pancreas-alone and simultaneous kidney-pancreas transplants. A simultaneous kidney-pancreas transplant may contribute to *both* organ-specific analyses, but must not appear twice within either organ-specific count. Determine the handling of other combined-organ procedures consistently with the source definitions and avoid double counting. Actual SRTR file contents and transplant-type coding must be verified during implementation.

Center HIV experience is a proxy for past experience, not a demonstration of current willingness, clinical capacity, or candidacy acceptance.

### 5.2 Geographic population groups

- **PLWH:** County prevalence counts of diagnosed HIV obtained from AtlasPlus, allocated to census tracts proportionally to their resident population aged 13+.
- **People without HIV:** ACS resident population aged 13+ minus the estimated tract PLWH count.

This primary age alignment is conditional on verifying that the selected AtlasPlus HIV prevalence indicator covers ages 13+. It is a **population-measure alignment**, not an assertion that adolescents are eligible for adult transplant programs. Adults and pediatric patients use different transplant systems; the population aged 13–17 is retained because reliably age-disaggregated county HIV counts may not support an 18+ estimate. The effect of using all-age ACS population is examined separately.

## 6. Population estimation and suppressed counts

### 6.1 County-to-tract allocation

For county *c* and tract *t* within that county:

\[
\widehat{H}_{t}=H_{c}\,\frac{P_{t,13+}}{P_{c,13+}}
\]

where \(H_c\) is the county diagnosed-PLWH count and \(P_{13+}\) is resident population aged 13+. The county and tract age denominators should refer to compatible geographic units and periods. Population-proportional allocation is an ecological estimate assuming uniform HIV prevalence within each county; the resulting tract counts are not measured tract-level HIV counts. Totals must reconcile at the county level within rounding. Zero-denominator and missing county cases require explicit handling.

### 6.2 Suppressed county estimates

The planned method will be implemented in the **CDCAtlas package**, not independently recreated in the analysis repository. Within each state, subtract the sum of reported nonsuppressed county HIV counts from the comparable state total; allocate the nonnegative residual among suppressed counties in proportion to their relevant populations. Preserve the original observed-versus-imputed status and reconcile the completed county totals to state totals, allowing for published rounding. If state and county totals differ in scope, age definition, geography or vintage—or the residual is inconsistent—the discrepancy must be resolved or reported before applying this method.

This is an agreed **planned method**, not a claim that CDCAtlas already implements it. An alternative zero-fill treatment of suppressed counties is a prespecified sensitivity analysis.

## 7. Historical center linkage and geography

Historical location reconstruction for **both** index years is required. SRTR supplies activity and center identifiers; index-year location and program identity should be established using historical authoritative records rather than automatically carrying back current coordinates. Differences due to relocation, opening, closing, or identifier changes matter insofar as they affect geographically modeled coverage.

Linkage quality should be checked in proportion to its analytic impact: summarize numbers matched and unmatched, flag ambiguous or materially consequential unresolved centers, and investigate those cases as needed. There is **no requirement for manual verification of every automatically matched center**, or for a published linkage-audit table. Do not silently interpret a missing historical center location as evidence that the center did not exist.

County-to-ACS joins, county-to-tract assignments and any altered county-equivalent geography (notably Connecticut) must be reconciled sufficiently to avoid loss or duplication of HIV counts. Different tract boundary vintages do not imply that individual tract observations can be directly compared across years; cross-year comparisons focus on aggregate coverage.

## 8. Geographic reach and outcomes

### 8.1 Catchments

- **Primary:** 60-minute modeled driving isochrone for each center, at **7:00 AM local time on a weekday**, with consistent routing assumptions across the 2017 and 2022 comparisons. The previously selected reference weekdays were March 6, 2017 and March 7, 2022; these are reference departure conventions, **not evidence of historical traffic conditions**.
- **Secondary:** 50-, 100- and 200-mile straight-line buffers, computed in an appropriate projected coordinate system with distance units converted consistently.

For any organ, year, reach measure and center category, combine center catchments into a union before estimating population coverage so that overlapping catchments do not double-count residents.

### 8.2 Primary tract inclusion rule and denominator

A tract is considered covered if **any part of its geometry intersects** the union of eligible center catchments. All of its estimated PLWH or non-HIV population then contributes to the numerator. The denominator is the corresponding estimated population over the entire study geography in that year. Coverage is reported as a percentage to one decimal place, with contrasts in **percentage points**. Any-intersection can overstate access, particularly in large rural tracts; a geometric centroid rule is a prespecified sensitivity analysis.

Non-HIV geographic coverage is evaluated for the *active* center group (P1). PLWH coverage is evaluated for all three nested groups (P2).

When a group has **no qualifying centers**, report 0% with a clear “no qualifying centers” label. When qualifying centers exist but the modeled population covered is zero, report 0% without that label. Missing or unresolved input data yield **undetermined**, not zero. Report qualifying center counts alongside coverage findings.

## 9. Analysis and interpretation

### 9.1 Stratified geographic access (S3)

For each feasible subgroup and organ/year, use appropriately matched subgroup HIV counts and population denominators to estimate coverage under the same catchment and tract-inclusion definitions as the main study. Evaluate suppression, geographic completeness and reconciliation before interpreting subgroup differences. Where fine-grained age bands or intersections of demographic characteristics cannot be estimated reliably, report the limitation and omit unsupported strata rather than fabricate precision. Geographic regions must be defined consistently for the comparison. Subgroup analyses are descriptive and should not be interpreted as individual-level disparities in transplant referral or receipt.

### 9.2 Potential expansion of HIV R+ services (S4)

Take the union of catchments for active HIV R+ centers as the existing service network. For each eligible active non-HIV R+ center, calculate the additional estimated PLWH covered by its catchment **outside** that existing union, using the same index-year population and reach definition. Report absolute newly covered PLWH and change in coverage in percentage points; a candidate's total catchment population is not its marginal gain. Individual candidate gains are not additive when candidate catchments overlap. If illustrating a sequence of multiple additions, recalculate marginal gain after each hypothetical addition instead of summing standalone gains. This is a geographic prioritization scenario, not evidence that a center will or can initiate an HIV transplant program. Center ranking is descriptive and does not equate geographic benefit with clinical readiness.

### 9.3 Interpretation and inference

The co-primary and secondary estimates are **descriptive population quantities**. Report the percentages and prespecified percentage-point contrasts; do not calculate confidence intervals, conduct hypothesis tests or report relative percentage changes. The major uncertainties concern spatial allocation, suppressed counts, center location and routing assumptions, rather than sampling variation. Prespecified sensitivity analyses evaluate selected structural assumptions, but do not exhaust all sources of uncertainty.

For the exploratory per-center comparison, estimate PLWH within each individual center's catchment and display distributions, medians and appropriate descriptive plots across mutually exclusive experience categories. Individual center catchments overlap and may share tracts; centers can also appear in both years. **Do not perform Kruskal–Wallis or other hypothesis tests** for these comparisons, and do not infer a causal effect of local HIV population on center experience or vice versa.

## 10. Missing data and validity requirements

- Distinguish missing, unknown or not-done HIV status from demonstrated positive status when classifying recipients and donors. Document the handling of such records.
- Do not silently equate a suppressed county HIV count with zero in the primary analysis; retain observed/imputed indicators and reconcile sums.
- Verify that county-level HIV counts allocated to tracts reconcile with their parent counties and that estimated PLWH plus people without HIV reconcile with the corresponding ACS population base.
- Check matching and inclusion of historical centers and identify unresolved records with material potential impact; confirm nested center-group counts and membership.
- Confirm eligible-center catchments, valid geometries, uniform geography exclusions and absence of double counting in catchment unions or organ-specific transplant counts.
- Verify logically ordered results: 50-mile coverage must not exceed 100-mile coverage, which must not exceed 200-mile coverage; HOPE D+ coverage must not exceed HIV R+, which must not exceed active coverage, for a shared organ, year and reach measure.
- Explicitly distinguish empty center groups, uncovered populations and data failures.
- For subgroup coverage, reconcile source totals and denominators where comparable, track suppression and geographic completeness, and identify strata that cannot support estimation.
- For center expansion, verify that candidates are active and not already HIV R+ centers, and that newly covered tracts are not already in the existing catchment; avoid summing overlapping candidate gains.

These checks are **internal analytic requirements**. They do not mandate a published validation appendix or manual line-by-line audit. Material unresolved discrepancies should be corrected or disclosed before results are treated as final.

## 11. Prespecified sensitivity analyses

All three analyses will be performed and documented. Material changes in interpretation should be included in the manuscript; results may otherwise appear in supplementary materials rather than the main text.

| Analysis | Alternative to primary method |
|---|---|
| **SA-suppression** | Fill suppressed county HIV counts with zero rather than state-residual, population-proportional estimates |
| **SA-tract** | Count a tract only if its **geometric centroid** lies within the unioned catchment, rather than using any-intersection |
| **SA-age** | Use all-age ACS population for geographic allocation and the non-HIV population comparison rather than an age-13+ base, consistent with the original analysis |

The 50-, 100- and 200-mile buffer analyses are **secondary objectives**, not additional sensitivity analyses. No separate sensitivity analysis is required for historical road networks, alternative departure times, alternative county-to-tract allocation, or unrestricted HOPE D+ centers, apart from the checks already described.

## 12. Limitations

1. **Ecological estimation:** HIV counts are observed at county level and allocated to tracts under uniform within-county prevalence; within-county clustering and partial-tract coverage are not directly measured.
2. **Diagnosed HIV only:** Undiagnosed HIV is not counted in the source HIV surveillance measure.
3. **Geographic potential rather than realized access:** Proximity does not establish clinical need, referral, ability to travel, insurance coverage, program acceptance, or receipt of transplant.
4. **Age/clinical-system mismatch:** The 13+ population measure includes adolescents, whereas center activity is defined by adult transplantation; adolescent HIV burden is expected to be small but is not necessarily separately identifiable in county data.
5. **Center experience as proxy:** Previous HIV R+ or deceased-donor HIV D+/R+ transplantation need not indicate contemporary willingness or capacity; living-donor HIV D+/R+ experience is not separately classified.
6. **Historical and contemporary measurement:** Historical center locations and populations are combined with contemporary road-network and traffic assumptions; estimates do not recover historical driving conditions.
7. **Geographic and time limitations:** Contiguous US and DC only; two cross-sectional years; changes in tract boundaries and county equivalents complicate small-area comparisons.
8. **Source data quality:** HIV classification, center linkage, suppression and revisions to surveillance or geographic data may alter results.
9. **Subgroup coverage:** Differential suppression, incompatible denominator categories and incomplete demographic stratification may limit comparisons across population groups.
10. **Expansion scenarios:** Marginal geographic coverage does not establish a center’s clinical readiness, willingness, staffing, referral patterns or patients’ realized access.

## 13. Required scientific results

The analysis must produce, for each organ and index year: counts of centers by nested experience group; relevant population totals; 60-minute geographic coverage estimates for both co-primary objectives; prespecified percentage-point contrasts; secondary distance-buffer coverage; 2017-to-2022 percentage-point differences; descriptive center-level catchment population comparisons; feasible, quality-assessed population subgroup comparisons; and a descriptive ranking of potential HIV R+ center expansion by incremental geographic coverage. Prespecified sensitivity analyses must be documented, with material effects reported.

Maps, tables, figures and manuscript ordering are presentation choices and are not fixed by this analysis plan.

## 14. Reproducibility and data access

The study relies on restricted SRTR records and publicly available population, surveillance and geographic sources, as well as modeled routing data. Restricted, patient-level SRTR information must not be published or transferred into public analysis outputs. The analysis should be reproducible within an authorized environment from defined data vintages and methodological assumptions. Repository-specific execution, version-recording, cache management and contributor instructions belong in `AGENTS.md`; the current implementation and data flow belong in `ARCHITECTURE.md`. Extensions using longer driving times or transit, and Bayesian county-to-tract modeling, are potential subsequent studies rather than requirements of this plan.

---

## Appendix A. STROBE and RECORD crosswalk

This is a planning aid, not a claim of reporting-guideline compliance. Manuscript locations can be supplied during manuscript preparation.

| Item | Topic | Plan section | Manuscript location |
|---|---|---|---|
| STROBE 1–3; RECORD 1.1–1.3 | Title, abstract, background, objectives and linkage description | 1–2, 4, 7 | — |
| STROBE 4–6; RECORD 6.1–6.3 | Design, setting, participants, center selection and coding | 3–5 | — |
| STROBE 7–8; RECORD 7.1 | Variables, sources and measurement | 4–8 | — |
| STROBE 9–12; RECORD 12.1–12.3 | Bias, study size, statistical methods, missing data and linkage quality | 6–11, 14 | — |
| STROBE 13–17; RECORD 13.1 | Descriptive results, study population, subgroup and expansion analyses, outcome estimates and sensitivity analyses | 9–11, 13 | — |
| STROBE 18–21; RECORD 19.1 | Interpretation, limitations and generalisability | 9, 12 | — |
| STROBE 22; RECORD 22.1 | Funding, protocol, data availability and access | 14; funding to be stated in manuscript | — |

## References

- von Elm E, et al. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement. *PLoS Med.* 2007;4(10):e296.
- Benchimol EI, et al. The REporting of studies Conducted using Observational Routinely-collected health Data (RECORD) statement. *PLoS Med.* 2015;12(10):e1001885.
