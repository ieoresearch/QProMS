library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(echarts4r)
library(trelliscope)
library(rbioapi)
library(org.Hs.eg.db)
library(clusterProfiler)

box::use(app/logic/R6Class_QProMS)
box::use(app/static/inputs_type_lists)

r6 <- R6Class_QProMS$QProMS$new()
# r6$loading_data(input_path = "app/static/proteinGroups.txt", input_name = "test")
r6$loading_data(input_path = "app/static/combined_protein.tsv", input_name = "test")
msg <- r6$identify_table_type()
r6$create_summary_table()
r6$make_expdesign("MaxLFQ Intensity")
a <- tibble(
  "condition" = c("xl", "xl", "xl", "non", "non", "non"),
  "key" = c(
    "XL_1 MaxLFQ Intensity",
    "XL_2 MaxLFQ Intensity",
    "XL_3 MaxLFQ Intensity",
    "nonXL_1 MaxLFQ Intensity",
    "nonXL_2 MaxLFQ Intensity",
    "nonXL_3 MaxLFQ Intensity"
  )
)
# a <- tibble(
#   "condition" = c("gel", "gel", "gel", "ist", "ist", "ist"),
#   "key" = c(
#     "LFQ intensity GEL_25kDa",
#     "LFQ intensity GEL_50kDa",
#     "LFQ intensity GEL_75kDa",
#     "LFQ intensity iST_50kDa_01_200ng",
#     "LFQ intensity iST_50kDa_02_200ng",
#     "LFQ intensity iST_50kDa_03_200ng"
#   )
# )
r6$validate_expdesign(a)
r6$add_replicate_and_label(a)
r6$preprocessing()
r6$protein_rank_target <- r6$expdesign$label[1]
r6$shiny_wrap_workflow()
r6$plot_pca(FALSE)
r6$organism <- "human"
r6$contrasts <- "xl_vs_non"
r6$stat_uni_test(test = "xl_vs_non", fc = 1, alpha = 0.05, p_adj_method = "BH", paired_test = FALSE, test_type = "welch")
a <- plot_volcano(tests = c("gel_vs_ist", "ist_vs_gel"), gene_names_marked = c("PRB3", "PRB1"), TRUE, TRUE)
trelliscope::view_trelliscope(a)
r6$clusters_number <- 3
r6$stat_anova(alpha = 0.05, p_adj_method = "BH")
r6$make_nodes(list_from = "univariate", focus = "xl_vs_non", "down")
r6$organism <- "human"
r6$make_edges("string")
r6$plot_heatmap(order_by_expdesing = FALSE)

a <- r6$imputed_data %>% 
  group_by(gene_names) %>%
  summarise(imputed = any(imputed), .groups = "drop") 

b <- r6$imputed_data %>% 
  pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity")

left_join(b, a, by = "gene_names")
