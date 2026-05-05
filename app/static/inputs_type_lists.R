box::use(
  org.Hs.eg.db[org.Hs.eg.db],
  org.Mm.eg.db[org.Mm.eg.db],
  org.EcK12.eg.db[org.EcK12.eg.db],
  org.Dm.eg.db[org.Dm.eg.db],
  org.Sc.sgd.db[org.Sc.sgd.db],
)

#' @export
metadata_list <- list(
  "FragPipe" = c("Gene", "Protein ID"),
  "MaxQuant" = c(
    "Gene names",
    "Protein IDs",
    "Peptides",
    "Razor + unique peptides",
    "Unique peptides",
    "Only identified by site",
    "Reverse",
    "Potential contaminant"
  ),
  "DIA-NN" = c("Run", "Protein", "Sequence", "Precursor.Id", "Fragment.Sum"),
  "Spectronaut" = c("PG.Genes", "PG.ProteinGroups"),
  "ProteomeDiscoverer" = c("Accession", "# Peptides", "# Unique Peptides", "# Razor Peptides"),
  "AlphaPept" = "V1"
  
)

#' @export
intensity_list <- list(
  "FragPipe" = c("MaxLFQ Intensity", "Intensity"),
  "MaxQuant" = c("LFQ intensity ", "iBAQ ", "Intensity "),
  "DIA-NN" = c(".mzML", ".raw","Fragment.Sum"),
  "Spectronaut" = c("PG.Quantity", "PG.MS1Quantity", "PG.MS2Quantity"),
  "ProteomeDiscoverer" = c("Abundance:", "Abundances \\(Normalized\\):"),
  "AlphaPept" = "_LFQ"
)

#' @export
org_map <- list(
  human = list(
    orgdb = org.Hs.eg.db,
    kegg = "hsa",
    wiki = "Homo sapiens",
    tax_id = 9606
  ),
  mouse = list(
    orgdb = org.Mm.eg.db,
    kegg = "mmu",
    wiki = "Mus musculus",
    tax_id = 10090
  ),
  ecoli = list(
    orgdb = org.EcK12.eg.db,
    kegg = "ecoj",
    wiki = "Escherichia coli",
    tax_id = 562
  ),
  drosophila = list(
    orgdb = org.Dm.eg.db,
    kegg = "dme",
    wiki = "Drosophila melanogaster",
    tax_id = 7227
  ),
  buddingyeast = list(
    orgdb = org.Sc.sgd.db,
    kegg = "sce",
    wiki = "Saccharomyces cerevisiae",
    tax_id = 559292
  )
)