library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(echarts4r)
library(trelliscope)
library(rbioapi)

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
r6$stat_uni_test(test = "xl_vs_non", fc = 1, alpha = 0.05, p_adj_method = "BH", paired_test = FALSE, test_type = "welch")
a <- plot_volcano(tests = c("gel_vs_ist", "ist_vs_gel"), gene_names_marked = c("PRB3", "PRB1"), TRUE, TRUE)
trelliscope::view_trelliscope(a)
r6$stat_anova(alpha = 0.05, p_adj_method = "BH")
r6$make_nodes(list_from = "univariate", focus = "xl_vs_non", "down")
r6$organism <- "human"
r6$make_edges("string")
r6$plot_heatmap(order_by_expdesing = FALSE)

reactable_network = function(table, interactive) {
  if(is.null(table)){return(NULL)}
  sele <- NULL
  oncl <- NULL
  if(interactive){
    sele <- "multiple"
    oncl <- "select"
  } 
  t <- table %>% 
    reactable(
      searchable = TRUE,
      resizable = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      height = "auto",
      selection = sele,
      paginationType = "simple",
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(6, 12, 18, 24),
      defaultPageSize = 12,
      onClick = oncl,
      defaultColDef = colDef(align = "center", minWidth = 200)
    )
  return(t)
}

plotly_empty(type = "scatter", mode = "markers") %>%
  config(displayModeBar = FALSE) %>%
  layout(
    title = list(
      text = "Not enough significant genes to generate a heatmap.",
      yref = "paper",
      y = 0.5
    )
  )


