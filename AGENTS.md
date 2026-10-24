# AGENTS.md

Guidance for everyone working on this repository: the maintainer,
collaborators, and any AI coding agents working for them. It covers how to
work here: conventions, data handling, and checks.

Three files divide the documentation:

| File                    | Covers                                                                  |
|-------------------------|-------------------------------------------------------------------------|
| `analysis-plan.md`      | Study design and methods: what the analysis does and why                |
| `ARCHITECTURE.md`       | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks           |

Read all three before making changes. Each covers its own topic and does not
repeat the others.

## The project

This repository estimates what share of people living with HIV (PLWH) in the
contiguous United States live within reach of an adult solid-organ transplant
center, compared with people without HIV. It combines SRTR transplant records,
the HRSA center list, CDC AtlasPlus HIV prevalence, ACS population, and Mapbox
driving isochrones. The results are published as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** It covers center
  definitions, populations, reach definitions, outcomes, sensitivity analyses
  and validation checks. Code implements the plan; it does not redefine it.
  - If the code needs to depart from the plan, change the plan in the same
    pull request.
  - Do not settle an item the plan marks **[CONFIRM]** or **[OPEN]** in code.
    Raise it with the maintainer.
  - After the plan reaches version 1.0, record each change in its Amendments
    table (plan §17), with the date and reason.
- **This is an analysis project, not an R package**, even though the scripts
  live in `R/`. Do not add DESCRIPTION, NAMESPACE, roxygen, pkgdown, or CRAN
  conventions.
- **Branch:** the default branch is `master`, not `main`.

## R scripts

- **Running the analysis.** Open `HOPE CDC analysis 2026.Rproj` and run
  `R/main.R` from the project root. It sources the other scripts in dependency
  order and leaves every object in the global environment. `ARCHITECTURE.md`
  describes what each script does.
- **One job per script.** When you add a script:
  - source it from `R/main.R` at the right point
  - add its Quarto page and sidebar entry (see below)
  - update `ARCHITECTURE.md`, including its execution order and diagram
- **Header comment.** Each script begins with a comment saying what it does and
  which objects it needs from earlier scripts.
- **Libraries and constants live in `R/setup.R`.** Other scripts do not call
  `library()` or hard-code values a reader might want to change, such as years,
  organs, distances, dates or thresholds. Add new ones to `setup.R` as named
  constants.
- **Loops follow the analysis grid.** Use `organ_list`, `year_list` and
  `distance_list` rather than writing out organs, years or distances. Objects
  are nested lists in that order: `object[[organ]][[distance]][[year]]`.
- **Helper functions** go in `R/functions.R`. New functions take their inputs as
  arguments rather than reading global objects. Two existing helpers,
  `make_paired_plot()` and `plot_center_HIV_catchment()`, read globals;
  `ARCHITECTURE.md` lists what they need. Do not add more of these.
- **Paths are relative to the project root.** No `setwd()`, no absolute paths
  (the maintainer works on Windows, others may not), and no writes outside the
  project. SRTR files are located only through the `sRtr` package's own
  configuration.

## Chunk labels

Scripts are divided into labelled sections so Quarto pages can show the code
without copying it:

```r
## ---- county-tract-merge
```

- A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all
  scripts**. A duplicate label silently shows the wrong code on the site.
- Renaming or deleting a label breaks the page that uses it. Search
  `quarto/*.qmd` before you change one.
- Labels are comments. They must never change what the code does, and the
  scripts must still run end to end through `R/main.R`.

## Quarto website

- **Layout:** `_quarto.yml` and `index.qmd` at the root; one page per script in
  `quarto/`, named after the script (`quarto/load_data.qmd` for
  `R/load_data.R`); results pages `quarto/results_<organ>.qmd`.
- **Script pages show code; they never run it.** A page either loads its script
  with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk and shows each
  labelled section with `#| eval: false`, or shows the whole script with the
  `file:` chunk option. Each page links to its script on GitHub and to the
  scripts it depends on.
- **Results pages read saved outputs only.** They read
  `../cache/Results_*_gt.rds` and find figures in `../figures/<organ>/` by
  filename pattern. They must never load SRTR files or rerun analysis steps.
  Because `cache/`, `tables/` and `figures/` are gitignored, results pages
  render only on a machine that has run the pipeline.
- **Stray figures appear on the site.** Any leftover file in
  `figures/<organ>/` that matches a results page's pattern is shown. Clear old
  outputs before rendering when filenames change.
- **New scripts** get a page and a sidebar entry in `_quarto.yml` in the same
  pull request. The `render:` list in `_quarto.yml` controls what is built;
  keep it explicit so Markdown files at the root (such as this one) are not
  rendered as pages.
- **Rendering and publishing:** the site renders to `docs/`, which GitHub Pages
  serves. The maintainer renders and commits `docs/` after merging. Pull
  requests should not include changes to `docs/`, to avoid conflicts.
  [CONFIRM]

## Outputs and caches

- `R/tables.R` writes to `tables/<organ>/`, `R/figures.R` to
  `figures/<organ>/`, and both write the RDS objects the site reads to
  `cache/`. All three folders are gitignored.
- **Caches do not invalidate themselves.** ACS pulls and Mapbox isochrones are
  reused whenever a file with the expected name exists in `cache/`. If you
  change center definitions, the center list, reference dates or ACS
  vintages, delete the affected cache files deliberately and say so in the
  pull request. Otherwise results silently mix old and new inputs.
- **Live inputs.** The HRSA center list and AtlasPlus data are fetched fresh on
  every run, so two runs on different dates can differ. Record the fetch date
  with any reported result (plan §16).

## Restricted data (applies to everyone)

SRTR Standard Analysis Files are covered by a data use agreement (DUA). The
rules below apply to people and agents alike.

- **Never commit data.** That includes SRTR files, extracts, and any saved
  object derived from recipient- or donor-level records (`.rds`, `.RData`,
  `.sas7bdat`, `.parquet`, `.csv`, and the like). Check `.gitignore` before
  adding a new output type.
- **AI agents must not read SRTR data.** Anything an agent opens, prints or
  summarizes is sent to the model provider. Agents therefore must not open SRTR
  files, print rows or `head()` of `tx` or `donor_deceased`, or display
  recipient-level fields, unless your own DUA explicitly permits it. Agents may
  write and review code, and run it on **synthetic data** that mimics the
  column names and types. Results on real data are produced by an authorized
  person.
- **Synthetic data** for development and tests is built in code, labelled as
  synthetic, and never derived from real records.
- **Public data is not restricted.** HRSA, AtlasPlus, ACS, TIGER and Mapbox
  outputs, and the aggregated results in `cache/Results_*`, may be read by
  agents.
- **Disclosure.** Published outputs show center-group aggregates only. Do not
  publish center-level counts of recipients with HIV or HIV-positive donors.
  Check the small-cell rules in the governing SRTR DUA before any output leaves
  the analysis environment. [CONFIRM the threshold]

## Secrets and paid APIs

- **Keys live in your user-level `~/.Renviron`, never in the repository:**
  `MAPBOX_PUBLIC_TOKEN` for `mapboxapi` and `CENSUS_API_KEY` for `tidycensus`.
  Do not write them into scripts, `.qmd` files, or a project-level `.Renviron`.
- **Mapbox calls cost money.** Each isochrone cache miss makes one call per
  center for that organ, year and center group. Ask the maintainer before any
  run that will miss the isochrone cache.
- **Be gentle with public APIs.** Keep the existing pauses between HRSA calls,
  and do not add loops that refetch data the pipeline already has.

## Data and API facts

Check each against the current documentation before relying on it.

- **Center identifiers.** SRTR `REC_CTR_CD` is matched to the HRSA `OTC_CD`
  (renamed `OTCCode`). Unmatched centers drop out silently today (plan §7, K1).
- **SRTR coding.** Adult means `REC_AGE_IN_MONTHS_AT_TX >= 216`. Recipient HIV
  uses `REC_HIV_STAT == "P"`. Donor HIV fields appear as both `"P"` and
  `"P: Positive"` depending on file format; match both. Only the deceased-donor
  file is joined, so living donors are not represented.
- **AtlasPlus** prevalence counts people aged 13 and older with diagnosed HIV
  [CONFIRM]. Suppressed counties come back as `NA`.
- **ACS and boundaries.** 2017 uses 2010 tract boundaries and 2022 uses 2020
  boundaries. Connecticut changed county equivalents to planning regions in
  2022. Keep GEOIDs as zero-padded character strings when you add new joins.
- **Projection.** All distance work is on EPSG:5070 (meters). Buffers in other
  projections give wrong distances.
- **Isochrones.** Mapbox caps isochrones at 60 minutes. Each center's time zone
  comes from `lutz::tz_lookup()` and must appear in `timezones` in `setup.R`;
  a center in an unlisted zone receives no isochrone and is not reported.

## Coding conventions

- **Joins.** Use `join_by()` with explicit keys and, where you can, an explicit
  `relationship`. After any join that could multiply rows, check the row count.
  Decide deliberately what happens to unmatched rows, and report them where the
  plan asks (plan §7).
- **Spatial joins.** Use the dissolved (`_united`) catchments for any count
  that must not double count. Per-center catchments are for per-center
  summaries only. Check geometry validity before joining (plan §14, V9).
- **Empty groups.** A center group with no centers must give a numerator of 0,
  never a missing value or 100% (plan §9, V6).
- **Allocation.** Tract PLWH counts are estimates. Interpret only their sums
  over large areas, and check that tract totals reconcile with county totals
  (plan §14, V1–V2).
- **Definitions.** Check SRTR, CDC, HRSA and Census definitions against their
  documentation. Never invent variables, codes, outcomes or clinical
  definitions.
- **Style:** the tidyverse style guide. Put style changes in their own
  commits, separate from changes that alter results.

## Checks for any change

Before opening a pull request, check the items that apply.

**Centers**

- Do the active, HIV R+ and HOPE D+ definitions still match plan §6.1,
  including the five-year window and the adult age rule?
- Are unmatched SRTR centers and missing coordinates counted and reported?
- Does every center in each group get exactly one catchment under every reach
  definition (plan §14, V5)?

**Populations and allocation**

- Are county and tract GEOIDs matched without losing leading zeros?
- Do tract PLWH totals equal county totals over matched counties?
- How are suppressed counties handled, and does that match plan §11?
- Are the ACS vintage and tract boundaries right for each index year?

**Reach and aggregation**

- Is everything on EPSG:5070 before buffering or intersecting?
- Are results computed on dissolved catchments, so each tract counts once?
- Do results satisfy the ordering checks: 50 ≤ 100 ≤ 200 miles, and HIV R+ ≤
  active (plan §14, V7)?
- Does the results grid still have 120 PLWH rows and 40 rows for people without
  HIV (V8)?

**Structure and documentation**

- Does `ARCHITECTURE.md` still describe the scripts and data flow? Update its
  commit reference.
- Are chunk labels unique, and does each page still find every label it uses?
- Does every script have a page and a sidebar entry?
- Do figure filenames still match the patterns the results pages search for?
- Do figures have alt text?

**Execution**

- Does `R/main.R` run from a clean session?
- Are paths relative, with no absolute paths or data directories?
- Did you delete any cache files your change makes stale?
- Are package versions recorded for any reported result? [OPEN: `renv`]

## Current gaps between the code and the plan

A working list. Delete each item when it is resolved, or move these into
GitHub Issues.

- Suppressed AtlasPlus counties are zero-filled in `R/load_data.R`. The plan
  makes best-estimate fill primary and zero-fill a sensitivity analysis (SA2).
- The recipient-to-donor join in `R/classify_centers.R` has no explicit key or
  relationship check.
- None of the validation checks V1–V9 or linkage reports K1–K3 are
  implemented, and there is no center flow diagram.
- Centers in time zones missing from `timezones` (for example
  `America/Boise`) silently receive no isochrone. Not yet checked against the
  real center list.
- Two constants live outside `setup.R`: buffer distances in meters
  (`R/geographical_computations_for_centers.R`) and dots per case
  (`R/figures.R`).
- Sensitivity analyses SA2–SA6 are not implemented.
- `README.Rmd` has no reproduction steps or data-access description.

## Working with agents

- **Autonomy.** The person directing an agent decides how much it may do, and
  may give it stricter rules of their own; those take precedence over this
  section. By default an agent may:
  - edit code and documentation
  - add scripts, synthetic data and tests
  - run code on synthetic data and render script pages
  - commit to a branch and open a pull request
- **`master` is protected.** Changes reach it only through a pull request the
  maintainer reviews.
- **Every agent, under any level of autonomy:**
  - follows the restricted-data and secrets rules above
  - asks before any step that would read SRTR data, run the full pipeline,
    miss the isochrone cache, or make other paid or bulk API calls
  - never publishes the site or force-pushes
- **Pull requests** stay focused. Each says which plan section it implements,
  what was checked, and whether it was checked on synthetic data, on real data
  by an authorized person, or only by reading. Never describe synthetic results
  as results.
- **References:** use SRTR SAF documentation, CDC AtlasPlus technical notes,
  Census ACS documentation, Mapbox API documentation, and PubMed. Cite what you
  used.
