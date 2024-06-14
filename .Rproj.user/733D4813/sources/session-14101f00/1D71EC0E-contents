library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(echarts4r)
library(trelliscope)

box::use(app/logic/R6Class_QProMS)
box::use(app/static/inputs_type_lists)

r6 <- R6Class_QProMS$QProMS$new()
r6$loading_data(input_path = "app/static/proteinGroups.txt", input_name = "test")
msg <- r6$identify_table_type()
r6$create_summary_table()
r6$make_expdesign("LFQ intensity")
a <- tibble(
  "condition" = c("gel", "gel", "gel", "ist", "ist", "ist"),
  "key" = c(
    "LFQ intensity GEL_25kDa",
    "LFQ intensity GEL_50kDa",
    "LFQ intensity GEL_75kDa",
    "LFQ intensity iST_50kDa_01_200ng",
    "LFQ intensity iST_50kDa_02_200ng",
    "LFQ intensity iST_50kDa_03_200ng"
  )
)
r6$validate_expdesign(a)
r6$add_replicate_and_label(a)
r6$preprocessing()
r6$protein_rank_target <- r6$expdesign$label[1]
r6$shiny_wrap_workflow()
r6$plot_pca(FALSE)
r6$stat_t_test(test = c("gel_vs_ist", "ist_vs_gel"), fc = 1, alpha = 0.05, p_adj_method = "BH", paired_test = FALSE, test_type = "limma")
a <- plot_volcano(tests = c("gel_vs_ist", "ist_vs_gel"), gene_names_marked = c("PRB3", "PRB1"), TRUE, TRUE)
trelliscope::view_trelliscope(a)

r6$stat_anova(alpha = 0.05, p_adj_method = "BH")
r6$anova_table %>% View()


r6$plot_heatmap(z_score = TRUE, n_cluster = 2, clustering_method = "ward.D2", order_by_expdesing = FALSE)
r6$anova_table %>% View()
r6$anova_col_order
r6$clusters_number <- 2
plot_protein_profile(c("PRB3", "PRB1", "gltX", "pgi"))
plot_protein_profile("pgi")



