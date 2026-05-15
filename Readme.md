# QProMS - Quantitative PROteomics Made Simple

QProMS is a Shiny app and R package for quantitative proteomics analysis. It supports dataset import, experimental design setup, preprocessing, exploratory analysis, ranking, differential analysis when available, network and enrichment analysis, interactive HTML reporting, and session restore.

The app can be accessed [here](https://bioserver.ieo.it/shiny/app/qproms) or run locally.

## Table of contents

- [Installation](#installation)
- [Getting started](#getting-started)
- [Home page](#home-page)
- [Design page](#design-page)
- [Preprocessing](#preprocessing)
- [Principal component analysis (PCA)](#principal-component-analysis-pca)
- [Correlation](#correlation)
- [Rank](#rank)
- [Volcano](#volcano)
- [Heatmap](#heatmap)
- [Network](#network)
- [Over-representation analysis (ORA)](#over-representation-analysis-ora)
- [Geneset enrichment analysis (GSEA)](#geneset-enrichment-analysis-gsea)
- [Choosing analysis inputs](#choosing-analysis-inputs)
- [Databases](#databases)
- [Export and reporting](#export-and-reporting)
- [Sharing and reproducibility](#sharing-and-reproducibility)
- [Settings](#settings)
- [Defaults and behavior](#defaults-and-behavior)

## Installation

QProMS can be installed directly from GitHub:

```r
remotes::install_github("ieoresearch/QProMS", ref = "package")
```

Launch QProMS as a package with:

```r
library(qproms)
run_qproms()
```

## Getting started

The app guides you through a typical proteomics workflow:

1. Start from `Home` by uploading a new intensity table, restoring a saved session, or loading an example dataset.
2. Use `Design` to validate the input table and define the experimental design.
3. Use `Preprocessing` to filter proteins, normalize values, inspect missingness, and perform imputation.
4. Explore the data in `PCA`, `Correlation`, and `Rank`.
5. If statistical analysis is available, `Volcano` and `Heatmap` are added to the workflow.
6. Continue with `Network`, `ORA`, and `GSEA`.
7. Export tables, an HTML report, or a saved analysis session from `Export`.

The top navigation includes:

- `Home`
- `Design`
- `Preprocessing`
- `PCA`
- `Correlation`
- `Rank`
- `Network`
- `ORA`
- `GSEA`
- `Export`
- `Settings`
- `Help`

`Volcano` and `Heatmap` appear after workflow initialization when statistical analysis is available.

## Home page

The `Home` page provides three entry points:

1. Upload a new dataset.
2. Restore a saved `QProMS_analysis_<date>.rds` session.
3. Load one of the bundled example datasets.

From the same page, you can also jump directly to `Settings` or `Help`.

## Design page

The `Design` page walks through:

1. input table preview,
2. table check,
3. automatic or manual column parsing,
4. experimental design editing,
5. experimental design validation,
6. workflow initialization.

QProMS automatically identifies the following input formats:

| Software | Summary |
|----------|---------|
| MaxQuant | Detected from required metadata columns and supported LFQ, iBAQ, or Intensity patterns. |
| ProteomeDiscoverer | Detected from accession and peptide-count metadata together with supported Abundance patterns. After upload, the intensity column type still needs to be selected in the interface. |
| FragPipe | Detected from `Gene` and `Protein ID` metadata together with `MaxLFQ Intensity` or `Intensity` columns. |
| DIA-NN | Detected from `Genes` and `Protein.Ids` metadata together with sample columns matching `.mzML` or `.raw`. |
| Spectronaut | Detected from `PG.Genes` and `PG.ProteinGroups` metadata together with `PG.Quantity`, `PG.MS1Quantity`, or `PG.MS2Quantity`. |
| AlphaPept | Detected from the `V1` annotation column together with `_LFQ` intensity columns. |

### ProteomeDiscoverer input

ProteomeDiscoverer tables are supported through automatic table identification, but QProMS still asks you to choose the intensity column type after upload. In practice, the workflow is:

1. Upload the ProteomeDiscoverer table in `Home`.
2. Open `Design` and let QProMS identify the table.
3. In `Table Check`, choose the intensity type detected in your export, for example `Abundance:` or `Abundances (Normalized):`.
4. Select the organism.
5. Continue to `Make Design Table`.

If automatic identification does not succeed, QProMS can work with custom tables after manual selection of:

- the gene/protein annotation column,
- the intensity columns,
- optional log transformation,
- the organism.

Supported organism choices are:

- Homo sapiens
- Mus musculus
- Drosophila melanogaster
- Saccharomyces cerevisiae
- Escherichia coli

Required metadata columns for supported software are:

| Software | Required metadata columns |
|----------|---------------------------|
| MaxQuant | `Gene names`, `Protein IDs`, `Peptides`, `Razor + unique peptides`, `Unique peptides`, `Only identified by site`, `Reverse`, `Potential contaminant` |
| ProteomeDiscoverer | `Accession`, `# Peptides`, `# Unique Peptides`, `# Razor Peptides` |
| FragPipe | `Gene`, `Protein ID` |
| DIA-NN | `Genes`, `Protein.Ids` |
| Spectronaut | `PG.Genes`, `PG.ProteinGroups` |
| AlphaPept | `V1` |

Supported intensity column patterns are:

| Software | Supported intensity columns or patterns |
|----------|-----------------------------------------|
| MaxQuant | `LFQ intensity `, `iBAQ `, `Intensity ` |
| ProteomeDiscoverer | `Abundance:`, `Abundances (Normalized):` |
| FragPipe | `MaxLFQ Intensity`, `Intensity` |
| DIA-NN | `.mzML`, `.raw` |
| Spectronaut | `PG.Quantity`, `PG.MS1Quantity`, `PG.MS2Quantity` |
| AlphaPept | `_LFQ` |

### Regex-based column selection

When a table is not identified automatically, QProMS provides a manual column-selection workflow.

In this mode, you can:

1. choose the gene or protein annotation column,
2. type a regex or keyword into `Filter intensity columns (regex or keyword)`,
3. review the filtered list of numeric columns,
4. select the columns that should be used as intensities.

This filter is helpful when sample columns share a common naming pattern. For example:

- `LFQ|Intensity` matches columns containing either `LFQ` or `Intensity`
- `Sample_` matches columns whose names contain `Sample_`
- `Abundance:` matches ProteomeDiscoverer abundance-style columns

After filtering, you still confirm the exact intensity columns manually before continuing.

After clicking `Make Design Table`, the experimental design table is created. The main columns are:

| Column name | Meaning |
|-------------|---------|
| `keep` | Whether a sample column should be included in the analysis. |
| `condition` | Experimental condition assigned to the sample. Replicates belonging to the same group must share the same condition name. |
| `key` | Internal unique sample identifier used by QProMS. |

After validation, QProMS adds sample labels and starts the downstream workflow.

## Preprocessing

The `Preprocessing` page combines filtering, normalization, imputation, and QC visualizations. It includes:

- valid-value filtering by group or total coverage,
- peptide-based filtering when relevant metadata are available,
- contaminant, reverse, and site filtering where supported,
- normalization settings,
- imputation settings,
- processed tables and QC views.

All preprocessing choices propagate through the downstream workflow. This means that filtering, normalization, and imputation settings affect later pages such as `PCA`, `Correlation`, `Rank`, `Volcano`, `Heatmap`, `ORA`, and `GSEA`.

The visualization panels include:

- protein counts,
- intensity distributions,
- coverage,
- intersection plot,
- coefficient of variation plot,
- missing data overview,
- imputed value distribution,
- processed table.

Protein filtering can be controlled by valid values in three ways:

- `In at least one group` keeps proteins observed above the threshold in at least one condition.
- `In each group` keeps proteins observed above the threshold in every condition.
- `In total` keeps proteins based on overall data completeness across the full dataset.

Normalization can be switched between `None` and `VSN`. Variance stabilizing normalization (VSN) is useful when you want intensity values to be made more comparable across samples while reducing mean-variance dependence.

Search-engine-specific filters are exposed when the required metadata are present:

- For `MaxQuant`, QProMS can filter by peptide count and can remove `Reverse`, `Potential contaminant`, and `Only identified by site` entries.
- For `ProteomeDiscoverer`, peptide-count-based filtering is available when the peptide metadata columns are present.
- For `FragPipe`, contaminant handling is based on the contaminant list used by the app, reflecting the FragPipe/Philosopher-style contaminant handling workflow.

For MaxQuant, these contaminant-related switches correspond to the standard MaxQuant annotation columns:

- `Reverse` removes reverse-sequence decoy identifications.
- `Potential contaminant` removes proteins flagged as contaminants.
- `Only identified by site` removes entries supported only by site evidence rather than standard protein-group identification.

For FragPipe, contaminants are filtered against the contaminant list bundled with QProMS, matching the idea of contaminant handling used in FragPipe and Philosopher workflows.

### Imputation

QProMS supports:

- `mixed` imputation,
- Perseus-style Gaussian imputation,
- `missForest` imputation,
- no imputation.

Mixed imputation separates missing-at-random (MAR) and missing-not-at-random (MNAR) values. In practice, values missing from only one replicate inside a condition are treated as MAR, while proteins completely missing from a condition are treated as MNAR.

Perseus-style imputation replaces missing values by sampling from a down-shifted Gaussian distribution. The `Down shift` parameter controls how far the imputed distribution is moved below the observed data, and `Scale` controls the width of that imputed distribution. This approach follows the Perseus-style missing-value workflow described in the original Perseus documentation: [coxdocs.org](https://cox-labs.github.io/coxdocs/replacemissingfromgaussian.html).

`missForest` uses iterative random-forest imputation and exposes the number of iterations and trees. This approach is based on Stekhoven and Buhlmann, 2012: [missForest paper](https://academic.oup.com/bioinformatics/article/28/1/112/219101).

A common workflow is to begin with the default mixed imputation, inspect the missing-data and imputed-distribution panels, and then compare against Perseus or `missForest` if needed.

## Principal component analysis (PCA)

The `PCA` page provides:

- a 2D PCA plot,
- a 3D PCA plot.

These plots are useful as quality control to ensure that replicas cluster with each other and separate across different experimental conditions, meaning that much of the variance is associated with the biological or experimental grouping of interest.

## Correlation

The `Correlation` page provides:

- a correlation heatmap,
- scatter plots,
- an interactive table.

Users can choose Pearson, Spearman, or Kendall correlation and filter scatter plots by sample selection.

### Highlighting proteins in scatter plots

The `Table` panel is linked to the scatter-plot view.

- Select one or more proteins in the table.
- Open the scatter-plot panel to see those proteins highlighted across the selected pairwise comparisons.

This is useful when you want to inspect how specific proteins behave across samples while keeping the full correlation structure in view.

## Rank

The `Rank` page visualizes protein abundance ranking. It includes:

- a protein rank plot,
- an interactive table,
- optional ranking by merged condition,
- top or bottom selection by percentage.

Proteins selected in the table are highlighted in the plot.

This tab is also a practical starting point for downstream functional analysis, because the top or bottom N% of ranked proteins can be passed directly to `Network` or `ORA`.

## Volcano

When statistical analysis is available, the `Volcano` page is added to the workflow. It provides:

- contrast selection,
- Welch's t-test,
- Student's t-test,
- moderated `limma` t-test,
- paired analysis option,
- fold change cutoff,
- multiple-testing correction,
- volcano or MA plots,
- profile plots,
- interactive result table.

### Choosing a statistical test

Three test types are available:

- `Welch's t-test` is the default choice for two-group comparisons and does not assume equal variance between groups.
- `Student's t-test` assumes equal variance between the two groups and is most appropriate when that assumption is reasonable.
- `Moderated t-test (limma)` borrows information across proteins to stabilize variance estimates and can be a good choice when replicate numbers are limited. For background on limma, see Ritchie et al., 2015: [Nucleic Acids Research](https://academic.oup.com/nar/article/43/7/e47/2414268).

The `Paired` option is off by default but can be toggled if a paired test is to be performed. This is desirable if the comparison is between two samples that are correlated.

`Fold change` is the minimum log2 fold change required for a protein to be marked as significant.

`Truncation` controls how p-values are adjusted for multiple testing. The available methods include:

- `BH`, the default Benjamini-Hochberg false discovery rate correction;
- `Bonferroni`, `Holm`, `Hochberg`, and `Hommel`, which are more conservative family-wise error control methods;
- `BY`, the Benjamini-Yekutieli correction for dependence-aware FDR control;
- `None`, which leaves raw p-values unadjusted.

Together, fold change, alpha, and truncation determine which proteins are marked as significant in the volcano or MA plot.

### Volcano plot versus MA plot

The `Plot type` control lets you switch between a volcano plot and an MA plot using the same statistical results.

- In the `Volcano` plot, the x-axis shows log2 fold change and the y-axis shows `-log10(p-value)`. This view emphasizes both effect size and statistical significance.
- In the `MA` plot, the x-axis shows mean abundance (`A`) and the y-axis shows log2 fold change (`M`). This view is useful for checking whether fold changes depend on signal intensity or whether low-abundance proteins behave differently from high-abundance proteins.

In QProMS, the MA plot is generated from the same differential-analysis table used for the volcano plot. For each protein, the app computes:

- `M` as the log2 fold change between the two groups being compared;
- `A` as the mean abundance across the two groups in that comparison.

The same significance calls and highlighting logic are reused in both views, so switching plot type changes the visual representation of the result, not the underlying test.

### Highlighting proteins and showing profile plots

The `Table` panel is linked to the volcano and profile views.

- Select one or more proteins in the results table to highlight them on the volcano or MA plot.
- The same table selection is also used to populate the `Profile Plot` panel.

This makes it easy to move from a significant hit in the table to its abundance pattern across the compared groups.

## Heatmap

When statistical analysis is available, the `Heatmap` page is added to the workflow. It provides:

- ANOVA-based feature selection,
- hierarchical clustering controls,
- cluster number selection,
- Z-score toggle,
- cluster profile plots,
- protein profile plot,
- interactive table.

### How proteins are selected for the heatmap

The heatmap is based on an ANOVA test across conditions. Proteins are first tested for differences in abundance across the experimental groups, and p-values are adjusted using the selected multiple-testing correction method.

Proteins with adjusted p-values below the selected alpha threshold are marked as significant and form the matrix used for clustering and heatmap visualization. Proteins that do not pass this threshold remain in the table and are labelled as `not_defined`, but they are not part of the clustered heatmap matrix.

### How clustering is done

After selecting significant proteins, QProMS builds a protein-by-sample matrix and clusters it with hierarchical clustering using the method chosen in the interface. Available methods include:

- `complete`
- `average`
- `ward.D2`
- `mcquitty`

The row dendrogram is cut into the selected number of clusters, and these clusters are labelled as `cluster_1`, `cluster_2`, and so on. If the `Z-score` option is enabled, each protein profile is scaled before clustering so that the heatmap emphasizes relative patterns across samples rather than absolute abundance.

When `Z-score` is enabled, the heatmap displays row-scaled abundance patterns, making it easier to compare relative changes across samples for proteins with very different overall intensities. When `Z-score` is disabled, the heatmap displays the raw abundance values that enter the ANOVA-derived matrix.

### Showing proteins in the protein profile panel

The `Table` panel is linked to the `Protein Profile` panel.

- Select one or more proteins in the heatmap table.
- The selected proteins are then displayed in the protein profile view, together with their assigned cluster.

The `Cluster Profile` panel shows the aggregate profile of each cluster, while the `Protein Profile` panel focuses on selected individual proteins.

## Network

The `Network` page builds interaction networks from:

- rank-based selection,
- volcano results,
- heatmap clusters.

Available options include:

- String and CORUM sources,
- score threshold,
- force or circular layout,
- node-name display,
- optional subnetwork creation from selected nodes.

If statistical analysis is not available, the network input is restricted to rank-based selection.

Use this tab when you want to move from a protein list to an interaction-level view.

## Over-representation analysis (ORA)

The `ORA` page performs enrichment analysis using proteins selected from:

- rank results,
- volcano contrasts,
- heatmap clusters,
- manual gene selection.

Supported databases are:

- Gene Ontology,
- KEGG,
- WikiPathways.

The page includes ontology selection for GO, simplify threshold, background selection, alpha, multiple-testing correction, bar plots, and result tables.

Use `ORA` when you already have a selected subset of proteins, for example top-ranked proteins, volcano hits, heatmap clusters, or a manually curated list.

### What "Include background" means

The `Background` option controls which universe is used for the enrichment test.

- If background is not included, enrichment is tested against the full annotation universe for the selected organism.
- If background is included, enrichment is tested against the proteins available in the corresponding QProMS result set, for example the proteins present in the statistical table, heatmap table, or rank-based selection context.

Including background is useful when you want enrichment to be interpreted relative to the proteins detected in your experiment rather than relative to the full organism-wide annotation space.

The ORA bar plot can be arranged by:

- fold enrichment,
- raw p-value,
- adjusted p-value,
- count.

This makes it possible to emphasize effect size, statistical significance, or the size of the contributing protein set depending on the question you are asking.

## Geneset enrichment analysis (GSEA)

The `GSEA` page performs enrichment analysis using ranked protein lists. Ranking can be based on:

- intensity,
- fold change,
- `-log10(p-value)`.

Supported databases are:

- Gene Ontology,
- KEGG,
- WikiPathways.

The page includes ontology selection for GO, simplify threshold, alpha, multiple-testing correction, bar plots, GSEA plots, and result tables.

Use `GSEA` when you want to analyse a ranked list rather than a predefined hit list.

The GSEA bar plot can be arranged by:

- normalized enrichment score (`NES`),
- raw p-value,
- adjusted p-value,
- set size.

This allows you to view the results either in terms of effect direction and magnitude (`NES`) or in terms of statistical support.

### Interpreting the GSEA enrichment plot

In addition to the summary bar chart, QProMS also generates a GSEA enrichment plot for the selected gene set. This plot is useful for understanding where the enrichment signal comes from along the ranked protein list.

The plot can be read in three parts:

- the running enrichment score curve shows how the enrichment score accumulates as you move through the ranked list;
- the vertical tick marks indicate where members of the selected gene set appear in that ranked list;
- the bottom ranking panel reflects the ranking metric used for the analysis.

The highest positive peak or lowest negative trough of the running curve corresponds to the enrichment score for that gene set. If the main peak is near the top of the ranked list and the enrichment score is positive, the gene set is associated with proteins enriched toward the top of the ranking. If the main trough is negative and located toward the bottom, the gene set is associated with proteins enriched toward the bottom of the ranking.

The leading-edge subset corresponds to the genes or proteins that contribute most strongly to the enrichment signal around that peak or trough. In practice, this plot complements the bar chart by showing not just whether a pathway is enriched, but also where the contributing members fall within the ranked list. A useful interpretation guide is available here: [MetwareBio GSEA guide](https://www.metwarebio.com/gsea-enrichment-analysis-guide/).

## Choosing analysis inputs

QProMS lets you reuse results from earlier steps as inputs for downstream functional analysis.

### Using Rank as input

In the `Rank` tab, you first define a ranked protein list by choosing:

- whether ranking should be based on individual samples or merged conditions,
- which sample or condition to rank,
- whether to take proteins from the top or bottom of the ranking,
- the percentage of proteins to include.

After pressing `PROCESS`, QProMS stores that ranked subset as the active rank-based selection. You do not need to re-select the proteins manually in downstream tabs. Instead, you can open another tab and choose `Rank` under `Inputs From`.

That rank-based selection can then be used in:

- `Network`,
- `ORA`.

In practice, this means:

- in `Network`, choosing `Rank` will build a protein-protein interaction network from the ranked subset defined in the `Rank` tab;
- in `ORA`, choosing `Rank` will test that same ranked subset for functional enrichment.

This is the main way to take the top N% or bottom N% of proteins from a sample or condition and pass them directly into downstream functional analysis.

### Using Volcano as input

When statistical analysis is available, `Volcano` results can be reused downstream.

The `Volcano` tab defines proteins through contrasts and significance filtering. Once a contrast has been processed, downstream tabs can reuse those results.

- `Network` accepts one or more contrasts and lets you choose `Up` and/or `Down` directions. It then builds a network from the proteins significantly changing in those selected directions.
- `ORA` accepts volcano-derived inputs in the form of `contrast_up` and `contrast_down`. This lets you test enriched proteins and depleted proteins separately.
- `GSEA` can rank proteins by fold change or `-log10(p-value)` from selected volcano contrasts, so it uses the whole ranked result rather than only the significant subset.

The practical distinction is:

- `Network` and `ORA` use volcano results as selected protein sets;
- `GSEA` uses volcano results as ranked protein lists.

### Using Heatmap as input

When `Heatmap` is available, downstream modules can use heatmap clusters as inputs.

- `Network` can build interaction graphs from one or more clusters.
- `ORA` can perform enrichment analysis on one or more clusters.

### Manual and intensity-based inputs

`ORA` also supports manual gene selection by entering gene names directly.

`GSEA` supports intensity-based ranking using selected samples or merged conditions, in addition to volcano-based ranking.

In practice, a simple analysis path is:

1. Use `Rank` to choose a sample or condition and define the top or bottom percentage.
2. Open `Network` to visualize interactions among those proteins.
3. Open `ORA` to test whether that same subset is enriched for pathways or functions.
4. Use `Volcano` when you want contrast-based hit lists or contrast-based ranking.
5. Use `GSEA` when you want to work from a full ranked list instead of a selected subset.

## Databases

QProMS uses different databases depending on the analysis tab.

### Network databases

- `String` is used to retrieve known and predicted protein-protein interactions and build interaction networks.
- `CORUM` is used to add curated mammalian protein complex relationships.

These sources can be selected together in the `Network` tab.

### Functional enrichment databases

`ORA` and `GSEA` support:

- `GO` for Gene Ontology enrichment across Biological Process, Molecular Function, and Cellular Component categories.
- `KEGG` for pathway-level enrichment focused on curated signaling and metabolic maps.
- `WikiPathways` for pathway enrichment using community-curated pathway collections.

In `ORA`, these databases are used to test whether selected proteins are over-represented in annotated functions or pathways.

In `GSEA`, these databases are used to test whether annotated functions or pathways are enriched at the top or bottom of a ranked protein list.

As a rule of thumb:

- choose `GO` for broad functional interpretation,
- choose `KEGG` for curated biochemical and signalling pathways,
- choose `WikiPathways` for additional pathway collections maintained by the community.

## Export and reporting

The `Export` page provides three types of output:

1. Export result tables in `.xlsx`, `.csv`, or `.tsv`.
2. Generate an interactive HTML report with full or custom section selection.
3. Save the full analysis session as `.rds`.

Available table exports include:

- `Filtered`
- `Normalized`
- `Imputed`
- `Ranked`
- `Volcano`
- `Heatmap`
- `Nodes`
- `Edges`
- `ORA`
- `GSEA`

The HTML report summarizes the selected analysis steps and can be shared with collaborators. The saved `.rds` session preserves analysis parameters and results so the workflow can be restored later from `Home`.

## Sharing and reproducibility

QProMS supports two complementary ways to share analyses.

### Interactive HTML report

The HTML report is designed for interactive reporting and communication. It can be shared with collaborators to review figures, summaries, and selected analysis sections in a browser-friendly format.

### `.rds` analysis session

The saved `.rds` session is designed for reproducibility and handoff. It preserves the analysis state, including parameters and results, so the workflow can be reopened from `Home` and continued or re-examined later.

Together, these outputs support both presentation and reproducible reloading of the analysis.

## Settings

The `Settings` page allows users to configure:

- color palette,
- plot text size,
- plot export format (`svg` or `png`/canvas).

## Defaults and behavior

Important default settings include:

- log2 transform enabled,
- normalization method: `None`,
- imputation method: `mixed`,
- correlation method: `pearson`,
- rank selection: top 10%,
- statistics defaults: Welch test, `BH` correction, fold change cutoff `1`,
- ORA defaults: database `GO`, ontology `BP`,
- GSEA defaults: database `GO`, ontology `BP`,
- plot format: `svg`,
- palette: `D`.

Several pages use a `PROCESS` button. Parameter changes are applied when processing is triggered for that page.
