# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and methods: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them, as implemented at its stated commit |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/HOPE-CDC-analysis-2026/issues) | Open scientific decisions (decision issues), gaps between code and plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others.

## The project

This is an **R analysis project, not an R package**. It estimates the share of people living with HIV (PLWH), and of people without HIV, who live within reach of an adult solid-organ transplant center in the contiguous United States, by organ, index year (2017 and 2022), center experience with HIV (active, HIV R+, HOPE D+) and reach definition (a 60-minute driving isochrone and 50-, 100- and 200-mile buffers). It combines SRTR transplant records, HRSA center locations, CDC AtlasPlus county HIV prevalence, ACS population counts and Mapbox isochrones. The code and methods are published as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** Code implements the plan; it does not redefine it. If code needs to depart from the plan, open a decision issue, or comment on the existing one, for the deciders. Once a decider has settled it (in the issue, or relayed there in their exact words), the plan change and the code can go in the same pull request, which closes the issue.
- **Deciders.** Scientific decisions in every area are made by Vagish Hemmige. Anyone may open or contribute to a decision issue; only a decider settles it (see "Working with agents").
- **Do not make unresolved scientific decisions implicitly through code.** Check the relevant open issues before implementing an analysis that depends on them.
- **This is an analysis project, not an R package.** Do not add DESCRIPTION, NAMESPACE, roxygen, or [{pkgdown}](https://pkgdown.r-lib.org/) infrastructure, or apply CRAN conventions.
- **Default branch:** `master`. Changes reach it only through pull requests.

## Data sources

Each source this project uses is listed below with the package that accesses it. **Code that touches a source goes through its package.** Do not write project-specific loading, cleaning, or definition code for something the package already does.

| Source | Accessed through | Package documentation | Package maintained by | Access | What AI agents may do |
|---|---|---|---|---|---|
| SRTR standard analysis files: transplant files `tx_ki`, `tx_li`, `tx_hr`, `tx_lu`, `tx_kp`, and `donor_deceased` | {sRtr} (`load_srtr_file()`) | https://vagishhemmige.github.io/sRtr/ | This team | Restricted (SRTR data use agreement) | None: code, documentation and synthetic data only |
| HRSA transplant center list and locations | {sRtr} (`get_hrsa_transplant_centers()`) | https://vagishhemmige.github.io/sRtr/ | This team | Public | No restriction |
| CDC NCHHSTP AtlasPlus county HIV prevalence | {CDCAtlas} (`get_atlas()`) | https://vagishhemmige.github.io/CDCAtlas/ | This team | Public | No restriction |
| American Community Survey 5-year tract and county population | {tidycensus} (`get_acs()`) | https://walker-data.com/tidycensus/ | External | Public; Census API key | No restriction; never commit the key |
| Census state boundaries | {tigris} (`states()`) | https://github.com/walkerke/tigris | External | Public | No restriction |
| Mapbox Isochrone API, `driving-traffic` profile | {mapboxapi} (`mb_isochrone()`) | https://walker-data.com/mapboxapi/ | External | Paid API; access token | Code and review only; a person runs uncached calls (see Credentials and paid APIs) |

To allow more for a source, a person changes its row through a pull request, naming the agreement and section that permit it, the approved AI service, and exactly what agents may do. Agents never propose or make this change themselves, and nothing in personal instructions can grant it.

Add a row when code first loads a new source.

**For every package:**

- **Read its documentation before writing code for a source**, including known issues. Behavior that sets the center groups, the population counts, a catchment, or a denominator is checked in the documentation, not assumed from a function's name.

**For packages this team maintains ([{sRtr}](https://vagishhemmige.github.io/sRtr/), [{CDCAtlas}](https://vagishhemmige.github.io/CDCAtlas/)):**

- **Facts about the source belong in the package's documentation**, not in this file. If the package lacks something this project needs to know, add it there or open an issue there.
- **Problems belong in the package.** If a function is wrong or does not quite do what the plan needs, open an issue in its repository. Fix it in the package (preferred when generally useful) or wrap it here with a code comment linking the issue, and remove the wrapper once the issue is fixed. The plan's method for suppressed county HIV counts, for example, is to be implemented in {CDCAtlas}, not in this repository.

**For external packages:**

- **Work around problems here.** Wrap the function in `R/functions.R` with a comment explaining the problem and linking any upstream issue. Reporting a bug upstream is an external communication; the person directing the work decides whether to file it.
- **Facts about the source that this project relies on** (definitions, release quirks, suppression rules, routing assumptions) go in `references/` with a citation to the provider's documentation, or in the plan when they shape a decision.

**For a source with no package:** one loading script records the download URL, the release or access date, and any processing. Verify its variables against the provider's documentation for that release.

## Exploration and definitive analysis

Exploration is welcome on branches or forks. Experimental scripts need not be sourced from `R/main.R` or have Quarto pages, and their findings are labelled preliminary. A script becomes part of the definitive analysis when it is sourced from `R/main.R`; from then on every rule below applies to it. Do not impose the current global-object design on experiments.

## R scripts

- **Running the analysis.** Run `R/main.R` from the project root. It sources the other scripts in dependency order; `ARCHITECTURE.md` describes what each does.
- **One job per script.** When you add a definitive script, source it from `main.R` at the right point and update `ARCHITECTURE.md` in the same pull request.
- **Header comment.** Each script begins with what it does, which objects it needs from earlier scripts, and what it produces.
- **Constants, libraries, and code lists** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change (a window, a year, a distance, a reference date) is a named constant there.
- **Helper functions** go in `R/functions.R`. A function takes its inputs as arguments and does not read global objects. Some existing helpers read globals (see `ARCHITECTURE.md`); new code does not add more.
- **No side effects outside the project.** No `setwd()`, absolute or Windows-specific paths, or writes outside the project. Restricted data are located only through their access package's configuration.
- **No results in the repository.** No script (exploratory or definitive) or comment, commit message, issue, pull-request text or review report contains results, counts or data values from restricted data unless they are cleared for release. Check before every commit and post, since a public repository publishes everything pushed to it, on any branch.

## Chunk labels

Scripts are divided into labelled sections so Quarto pages can show the code without copying it:

```r
## ---- <step-name>
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**; a duplicate silently picks up the wrong code.
- Search the `.qmd` files before renaming or deleting a label.
- Labels are comments and never change what the code does.

## Quarto website

The site documents the code. It does not run the analysis.

- `_quarto.yml`, `index.qmd`, `404.qmd` (the page GitHub Pages shows for a missing address) and `styles.css` sit at the root; `quarto/` has one page per definitive script, named after it, plus the Background, Study design and methods, results, About and project-document pages. The `custom-callout` extension in `_extensions/` provides the `Rcode`, `Results` and `Pending` callouts.
- **Visual cues.** Each kind of page has one sidebar icon, coloured by `styles.css`, and pages mark the same kind of content with the callout of that colour: background `book` (blue); study design and methods `diagram-3` (orange); R code `laptop` (pink, `Rcode` 💻); results `clipboard-data` (green, `Results` 📊); project documents `file-earmark-ruled` (grey). Give every new page the icon of its kind.
- **Provisional content.** `quarto/_pending.lua` highlights in yellow every paragraph that starts with **Decision pending:** or, in the plan, **Open:**, and any `[text]{.pending}`, and adds a ⏳ `Pending` callout to pages that have a mark. Mark only what an open decision could change, and remove the mark when the decision is settled.
- **Project documents.** `analysis-plan.md`, `ARCHITECTURE.md`, `AGENTS.md` and `REPOSITORY-BLUEPRINT.md` are shown on the site by pages in `quarto/` that only include the root file, which stays the one source: edit the root file, not the page.
- Each page loads its script with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk and shows each labelled section with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub and to the scripts it depends on.
- **Never evaluate analysis code in the site.** `_quarto.yml` sets `execute: eval: false`. Pages render without restricted data. Only the `read_chunk()` imports, the setup page's package and constants tables, the Architecture page's chunk that reads `ARCHITECTURE.md`, and the results pages, which read only `site-outputs/`, set `eval: true`.
- **Publishing.** The site renders to `docs/`, which is not committed; `.github/workflows/publish.yml` renders it on every pull request as a check, and on every push to `master` renders and publishes it to the `gh-pages` branch, which GitHub Pages serves. When you add a script, add its page and sidebar entry in the same pull request.
- **Prose must match the code.** If a change alters what a section does, update its explanation in the same pull request.

## Outputs

- Tables and figures are exported by `R/tables.R` and `R/figures.R` to `tables/` and `figures/`, which are gitignored because they derive from restricted SRTR data: agents may not open them (see Restricted data).
- **`site-outputs/`** holds the tables and figures the website shows. `R/tables.R` and the `export-site-figures` section of `R/figures.R` write it, and it is committed. Commit changes to it only after the disclosure check below; agents may read what is committed there. `images/` holds the site's images that no script makes.
- `cache/` holds Census pulls, isochrones and other intermediates and is gitignored. **Caches are not invalidated automatically** when upstream code, definitions, source data or routing assumptions change: identify stale dependencies and say what needs rebuilding, and do not assume a cached or published result is current. Ask before deleting caches.
- Published figures need alt text.
- `README.Rmd` is the source of `README.md`; edit the `.Rmd` and re-knit.

## Restricted data (applies to everyone)

The **Access** column of "Data sources" says which sources are restricted. For each restricted source:

- **Never commit data**, extracts, or saved objects or outputs derived from patient-level records. Keep data outside the repository, and check `.gitignore` before adding a new output type.
- **AI agents never read restricted data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents do not open data files or patient-level objects, print rows, or display identifiers, person-level dates, or small geographies. Agents also do not open outputs from restricted data that are not yet cleared for release (for example `tables/`, `figures/` and `cache/`). Agents write code, review it, and run it on **synthetic data**; results on real data are produced by an authorized person. The only exception is what the Data sources table explicitly allows for a source, and only within those words.
- **Synthetic data** are built in code, labelled as synthetic, and never derived from real records.
- **Disclosure.** Aggregates derived from SRTR data need review under the SRTR data use agreement before release, even though they are aggregated or cached. Take particular care with small cells and with center-specific information on HIV R+ and HOPE D+ transplantation, including the center-group outlines and beeswarm plots, and with counts recoverable by subtraction or complementary cells. Follow the agreement's exact dissemination rules. Check every output before it leaves the approved environment, including `site-outputs/`, scripts, comments, commit messages, issues, pull-request text and review reports (see R scripts).
- **Credentials and paid APIs.** Never commit keys, tokens, or `.Renviron`. Ask before paid, bulk, or uncached API calls, notably uncached Mapbox isochrones. Before a paid batch, make one call with the exact package version and settings, and check the result before running the rest. Which service or version to use is the person's choice; an agent proposes, with the cost if known, and does not switch it on its own.

Public ACS, CDC AtlasPlus, HRSA, and Census data can be used subject to their own terms.

## Coding conventions

- **Joins** use `join_by()` with an explicit `relationship` where possible. After every join of records to centers and every spatial join, assert that rows did not multiply, and decide deliberately what happens to unmatched rows. Keep FIPS codes and GEOIDs as zero-padded character strings.
- **Windows** (the five-year experience windows and index years) are written as constants matching the plan, and their edges are tested on synthetic data.
- **Code that turns spatial results into reported numbers** lives in named functions in `R/functions.R`, tested on synthetic data with a known answer.
- **Never invent** variables, codes, files, or definitions; check SRTR variables and code values against the SRTR documentation for the release used.
- **Package APIs:** read the help page and NEWS for the installed version before calling a package; don't write calls from memory. A test that fakes a package checks only our own logic, so call the real package once (synthetic data, or one cheap call) before relying on the code.
- **Style:** readable for a clinician-analyst who learned R from online guides: pipes and dplyr/tidyr verbs, case_when(), loops over names or named lists rather than the apply family, Reduce or positional indexing. Required base R goes in a named, tested function. Tidyverse layout; style changes in their own commits, and no unrelated stylistic rewrites during scientific changes.

## Checks for any change

Check the items that apply before opening a pull request.

- **Plan:** does the change implement the plan as written, and does it leave every open decision open?
- **Data sources:** does it use the access package's functions, and does their documented behavior match what the plan needs?
- **Package APIs:** for new or changed calls to a package, does the pull request name the installed version and the help page or NEWS entry relied on?
- **Joins and denominators:** no unintended row multiplication; missing, suppressed, and zero kept distinct.
- **Reported numbers:** does each estimate match the plan's definition, and do tables, figures, and site prose say what the code actually computes?
- **Structure:** is `ARCHITECTURE.md` current; are chunk labels unique and found by their pages; does every definitive script have a page and sidebar entry; does any site chunk run analysis code?
- **Disclosure:** search every changed tracked file, including `site-outputs/`, and the commit messages and pull-request text, for dates, record IDs, names, absolute paths and numbers that could come from restricted data. Read each hit in context; legitimate ones (plan dates, deciders' names, synthetic examples) stay.
- **Execution:** does `main.R` run in a fresh session with relative paths; are any saved intermediates or caches stale?

Checks specific to this design:

- **Population:** observed, suppressed, missing and estimated counts kept distinct; denominators and age strata compatible; county-to-tract allocation reconciles to county totals; missing or suppressed never silently equated with zero.
- **Centers:** organ-specific activity, the classification windows, recipient and donor HIV definitions, historical location assumptions, and eligibility in each index year; no duplicate events within an organ.
- **Space:** CRS and units (buffers are computed in EPSG:5070, in meters); geometry validity; the tract inclusion rule; geographic exclusions. Dissolved catchments for aggregate coverage; per-center catchments only where overlap is intended.
- **Travel time:** driving isochrones, straight-line buffers, transit access and travel-time matrices kept conceptually separate; routing profile, reference times, coverage, API limits and cached results checked.
- **Results:** an empty center group distinguished from missing inputs; denominators and plausibility checked; subgroups not assumed to nest unless the plan's definitions require it.

Checks test scientific consequences and edge cases, not merely repeat the same calculation. Record which checks ran, which did not, and any data or credentials that were unavailable.

## Working with agents

- **Autonomy.** The person directing an agent sets its working style (review only, step by step, or hands-off) in their own instructions, and may add stricter rules; those take precedence. Without a style, an agent works step by step. At most, an agent may edit code and documentation, add scripts and tests, run code and render the site on synthetic data, and commit to a branch and open a pull request.
- **Decisions.** Anyone, including an agent, may add options and evidence to a decision issue. A decider states the choice in the issue. Agents implement decisions; they never choose among options on their own, in the issue, the plan, or the code.
- **Every agent, under any autonomy:** never merges a pull request; follows the restricted-data rules; asks before running the full pipeline, deleting caches or making expensive calls; shows a person the exact text before posting or sending anything as them, unless that person's own instructions waive this; and proposes another way of writing something already agreed before committing it.
- **Pull requests opened by an agent** are focused. Each says which plan section or issue it addresses, what was checked, and whether by running code on synthetic data or by reading only. Synthetic results are never described as results.
- **References:** cite the source documentation and literature used, in `references.bib`, alongside the decisions they support.
