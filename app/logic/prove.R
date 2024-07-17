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

make_ora_list <- function(focus) {
  test <- stringr::str_remove(focus, pattern = "_up|_down")
  data <- r6$stat_table %>%
    dplyr::select(gene_names, dplyr::starts_with(test)) %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches(test)),
                     ~ stringr::str_remove(., paste0(test, "_"))) %>%
    dplyr::filter(significant)
  
  if(nrow(data) > 0) {
    data <- data %>%
      dplyr::mutate(direction = dplyr::if_else(fold_change > 0, paste0(test, "_up"), paste0(test, "_down"))) %>%
      dplyr::filter(direction == focus) %>% 
      dplyr::select(gene_names, direction) 
  } else {
    data <- tibble::tibble(
      gene_names = c("NO_Significant", "NO_Significant"),
      direction = c(paste0(test, "_up"), paste0(test, "_down"))
    )
  }
  return(data)
}
go_ora <- function(list_from, focus, ontology, simplify_thr, alpha, p_adj_method, background) {
  
  orgdb <- if (r6$organism == "human") org.Hs.eg.db else org.Mm.eg.db
  
  if (list_from == "univariate") {
    groupped_data <- map(focus, ~ make_ora_list(focus = .x)) %>% 
      purrr::reduce(dplyr::bind_rows) %>% 
      dplyr::group_by(direction)
    gene_vector <- groupped_data %>% 
      dplyr::group_map(~ dplyr::pull(.x, gene_names)) %>% 
      set_names(dplyr::group_keys(groupped_data) %>% dplyr::pull())
    uni <- r6$stat_table %>% dplyr::pull(gene_names)
  }
  
  if (list_from == "multivariate") {
    groupped_data <- r6$anova_table %>% 
      dplyr::filter(cluster %in% focus) %>% 
      dplyr::select(gene_names, cluster) %>% 
      dplyr::group_by(cluster)
    if (nrow(groupped_data) == 0) {
      gene_vector <- list(not_defined = "NO_Significant")
    } else {
      gene_vector <- groupped_data %>% 
        dplyr::group_map(~ dplyr::pull(.x, gene_names)) %>% 
        set_names(dplyr::group_keys(groupped_data) %>% dplyr::pull())
    }
    uni <- r6$anova_table %>% dplyr::pull(gene_names)
  } 
  
  if (list_from == "top_rank"){
    gene_vector <- list(r6$protein_rank_list) %>% 
      set_names(r6$protein_rank_target)
    uni <- r6$rank_data %>% dplyr::pull(gene_names)
  }
  
  if (list_from == "manual"){
    gene_vector <- list("manual" = focus) 
    uni <- NULL
  }
  
  if (!background) {
    uni <- NULL
  }
  
  r6$ora_result_list <- map(gene_vector, possibly(~ enrichGO(
    gene = .x,
    OrgDb = orgdb,
    keyType = 'SYMBOL',
    ont = ontology,
    pAdjustMethod = p_adj_method,
    universe = uni,
    readable = TRUE) %>% 
    filter(p.adjust < alpha) %>% 
    simplify(cutoff = simplify_thr), otherwise = NULL))

}


go_ora(
  list_from = "multivariate",
  focus = c("cluster_1", "cluster_2", "cluster_3"),
  ontology = "BP",
  simplify_thr = 1,
  alpha = 0.05,
  p_adj_method = "BH",
  background = FALSE
)

print_ora_table <- function(arranged_with) {
  r6$ora_table <- map(r6$ora_result_list, ~ pluck(.x, "result")) %>%
    list_rbind(names_to = "group") %>% 
    as_tibble() %>% 
    tidyr::separate(GeneRatio, into = c("a", "b"), sep = "/", remove = FALSE) %>%
    tidyr::separate(BgRatio, into = c("c", "d"), sep = "/", remove = FALSE) %>%
    mutate(
      fold_enrichment = (as.numeric(a) / as.numeric(b)) / (as.numeric(c) / as.numeric(d)),
      across(c("pvalue", "p.adjust", "qvalue"), ~ round(-log10(.), 2)),
      fold_enrichment = round(fold_enrichment, 2)
    ) %>%
    select(-c(a, b, c, d)) %>% 
    dplyr::relocate(ID) %>% 
    dplyr::relocate(geneID, .after = dplyr::last_col()) %>% 
    dplyr::relocate(Count, .after = fold_enrichment) %>% 
    arrange(desc(!!sym(arranged_with)))
}

print_ora_table("Count") %>% View()

ora_table_all %>%  
  dplyr::filter(group == "cluster_2") %>%
  dplyr::arrange(fold_enrichment) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::mutate(fold_enrichment = if_else(fold_enrichment < 3.5, fold_enrichment*(-1), fold_enrichment)) %>%
  dplyr::mutate(color = if_else(fold_enrichment > 0, "#67001f", "#053061")) %>%
  echarts4r::e_chart(ID, renderer = "svg") %>%
  echarts4r::e_bar(fold_enrichment, bind = Description) %>%
  echarts4r::e_flip_coords() %>%
  echarts4r::e_grid(containLabel = TRUE) %>%
  # echarts4r::e_color("green") %>%
  e_add_nested("itemStyle", color) %>%
  echarts4r::e_tooltip(
    formatter = htmlwidgets::JS(
      paste0("function(params){return('<strong>", "fold_enrichment", ": </strong>' + params.value[0])}")
    )
  ) %>%
  echarts4r::e_x_axis(
    name = "fold_enrichment",
    nameLocation = "center",
    nameTextStyle = list(
      fontWeight = "bold",
      fontSize = 16,
      lineHeight = 60
    )
  ) %>%
  echarts4r::e_y_axis(axisLabel = list(fontSize = 0)) %>%
  echarts4r::e_legend(show = FALSE) %>%
  echarts4r::e_labels(show = TRUE, formatter= '{b}', position = "insideLeft") %>%
  echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
  echarts4r::e_show_loading(text = "Loading...", color = "#35608D")
  
# ora_table_all %>% 
#   group_by(ID) %>%
#   filter(n() > 1) %>%
#   dplyr::arrange(ID) %>%
#   ungroup() %>% 
#   dplyr::slice_head(n = 20) %>%
#   dplyr::group_by(group) %>% 
#   echarts4r::e_chart(ID, renderer = "svg") %>%
#   echarts4r::e_bar(pvalue, bind = Description) %>%
#   echarts4r::e_flip_coords() %>%
#   echarts4r::e_grid(containLabel = TRUE) %>%
#   # echarts4r::e_color(color) %>%
#   echarts4r::e_tooltip(
#     formatter = htmlwidgets::JS(
#       paste0("function(params){return('<strong>", "fold_enrichment", ": </strong>' + params.value[0])}")
#     )
#   ) %>%
#   echarts4r::e_x_axis(
#     name = "fold_enrichment",
#     nameLocation = "center",
#     nameTextStyle = list(
#       fontWeight = "bold",
#       fontSize = 16,
#       lineHeight = 60
#     )
#   ) %>%
#   echarts4r::e_y_axis(axisLabel = list(fontSize = 0)) %>%
#   echarts4r::e_labels(show = TRUE, formatter= '{b}', position = "insideLeft") %>%
#   echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
#   echarts4r::e_show_loading(text = "Loading...", color = "#35608D")

print_ora_table = function(ontology = "BP", groups, value) {
  
  ora_table_all <- map(r6$ora_result_list_simplified, ~ pluck(.x, "result")) %>%
    list_rbind(names_to = "group") %>% 
    as_tibble()
  
  r6$ora_table_all_download <- ora_table_all %>% 
    tidyr::separate(GeneRatio, into = c("a", "b"), sep = "/", remove = FALSE) %>%
    tidyr::separate(BgRatio, into = c("c", "d"), sep = "/", remove = FALSE) %>%
    dplyr::mutate(fold_enrichment = (as.numeric(a)/as.numeric(b))/(as.numeric(c)/as.numeric(d))) %>% 
    dplyr::select(-c(a,b,c,d)) %>% 
    dplyr::mutate(dplyr::across(c("pvalue", "p.adjust", "qvalue"), ~ -log10(.))) %>%
    dplyr::mutate(dplyr::across(c("pvalue", "p.adjust", "qvalue", "fold_enrichment"), ~ round(., 2))) %>% 
    dplyr::relocate(ID) %>% 
    dplyr::relocate(geneID, .after = dplyr::last_col()) %>% 
    dplyr::relocate(Count, .after = fold_enrichment) 
  
  r6$ora_table_counts <- ora_table_all %>% 
    dplyr::group_by(ONTOLOGY) %>% 
    dplyr::summarise(count = dplyr::n())
  
  r6$ora_table <- ora_table_all %>% 
    dplyr::filter(ONTOLOGY == ontology) %>%
    dplyr::filter(group %in% groups) %>% 
    tidyr::separate(GeneRatio, into = c("a", "b"), sep = "/", remove = FALSE) %>%
    tidyr::separate(BgRatio, into = c("c", "d"), sep = "/", remove = FALSE) %>%
    dplyr::mutate(fold_enrichment = (as.numeric(a)/as.numeric(b))/(as.numeric(c)/as.numeric(d))) %>% 
    dplyr::select(-c(a,b,c,d)) %>% 
    dplyr::mutate(dplyr::across(c("pvalue", "p.adjust", "qvalue"), ~ -log10(.))) %>%
    dplyr::mutate(dplyr::across(c("pvalue", "p.adjust", "qvalue", "fold_enrichment"), ~ round(., 2))) %>% 
    dplyr::relocate(ID) %>% 
    dplyr::relocate(geneID, .after = dplyr::last_col()) %>% 
    dplyr::relocate(Count, .after = fold_enrichment) 
  
  if(value == "fold_enrichment") {
    r6$ora_table <- r6$ora_table %>% 
      dplyr::arrange(-fold_enrichment)
  } else {
    r6$ora_table <- r6$ora_table %>% 
      dplyr::arrange(-pvalue)
  }
  
}
