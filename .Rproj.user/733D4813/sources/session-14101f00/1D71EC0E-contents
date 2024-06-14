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
r6$anova_table %>% View()


r6$plot_heatmap(z_score = TRUE, n_cluster = 2, clustering_method = "ward.D2", order_by_expdesing = FALSE)



make_nodes = function(list_from, focus, direction) {
  if (list_from == "univariate") {
    nodes_table <- self$print_stat_table() %>% 
      select(gene_names, starts_with(focus)) %>% 
      rename_at(vars(matches(focus)), ~ str_remove(., paste0(focus, "_"))) %>%
      filter(significant) %>% 
      mutate(
        category = if_else(fold_change > 0, "up", "down"),
        size = p_val * 5,
        color = case_when(
          fold_change > 0 & fold_change <= 1.5 ~ "#fddbc7",
          fold_change > 1.5 & fold_change <= 2 ~ "#f4a582",
          fold_change > 2 & fold_change <= 2.5 ~ "#d6604d",
          fold_change > 2.5 & fold_change <= 3 ~ "#b2182b",
          fold_change > 3 ~ "#67001f",
          fold_change < 0 & fold_change >= -1.5 ~ "#d1e5f0",
          fold_change < -1.5 & fold_change >= -2 ~ "#92c5de",
          fold_change < -2 & fold_change >= -2.5 ~ "#4393c3",
          fold_change < -2.5 & fold_change >= -3 ~ "#2166ac",
          fold_change < -3 ~ "#053061"
        )
      )
    if (direction == "up") {
      nodes_table <- nodes_table %>% 
        filter(category == "up")
    } else if (direction == "down") {
      nodes_table <- nodes_table %>% 
        filter(category == "down")
    }
  } else if (list_from == "multivariate") {
    nodes_table <- self$print_anova_table() %>% 
      filter(significant) %>%
      filter(cluster %in% focus) %>%
      mutate(category = cluster, size = p_val * 2)
  } else {
    nodes_table <- tibble(gene_names = self$protein_rank_list) %>%
      mutate(
        category = self$protein_rank_target,
        p_val = 1,
        p_adj = 1,
        size = 10
      )
  }
  self$nodes_table <- nodes_table
  self$name_for_edges <- nodes_table %>%
    pull(gene_names)
}


make_edges = function(source) {
  edges_string_table <- NULL
  edges_corum_table <- NULL
  if(self$organism == "human"){tax_id <- 9606} else {tax_id <- 10090}
  if("string" %in% source) {
    edges_string_table <- self$name_for_edges %>%
      rba_string_interactions_network(species = tax_id, verbose = FALSE) %>%
      filter(escore != 0, dscore != 0) %>%
      unite("stringId", stringId_A:stringId_B, remove = TRUE) %>%
      distinct(stringId, .keep_all = TRUE) %>%
      ## string calculation for fisical score
      mutate(score1 = (escore - 0.041) * (1 - 0.041)) %>%
      mutate(score2 = (dscore - 0.041) * (1 - 0.041)) %>%
      mutate(score_combin = 1 - (1 - score1) * (1 - score2)) %>%
      mutate(score = score_combin + 0.041 * (1 - score_combin)) %>%
      ## end
      select(source = preferredName_A, target = preferredName_B, score) %>%
      filter(source != target) %>%
      mutate(
        complex = "not defined",
        color = "#999999",
        size = round(score * 10 / 2, 0),
        database = "String"
      )
    if (nrow(edges_string_table) == 0) {
      edges_string_table <- NULL
    }
  }
  if("corum" %in% source) {
    if(tax_id == 9606){
      raw_corum_table <-
        get_complex_genes(
          import_omnipath_complexes(resources = "CORUM"),
          self$name_for_edges,
          total_match = FALSE
        ) %>%
        unique() %>%
        select(name, components_genesymbols) %>%
        separate_rows(components_genesymbols, sep = "_") %>%
        filter(components_genesymbols %in% self$name_for_edges) %>%
        unique() %>% 
        group_by(name) %>%
        filter(n() > 1) %>%
        ungroup()
      if (nrow(raw_corum_table) != 0) {
        expand_nodes <- raw_corum_table %>%
          group_by(name) %>%
          group_map( ~ pull(.x, components_genesymbols))
        edges_corum_table <-
          map(.x = expand_nodes, .f = ~ as.data.frame(t(combn(.x, 2)))) %>%
          reduce(bind_rows) %>%
          rename(target = V1,  source = V2) %>%
          left_join(raw_corum_table, by = c("source" = "components_genesymbols"), relationship = "many-to-many") %>%
          rename(complex = name) %>%
          unique() %>% 
          mutate(score = 1, color = "#4daf4a") %>% 
          group_by(source, target, color) %>% 
          nest() %>% 
          unnest_wider(data, names_sep = "_") %>%
          ungroup() %>% 
          mutate(complex = map_chr(data_complex, str_flatten, collapse = "/")) %>% 
          rowwise() %>% 
          mutate(
            score = sum(data_score),
            size = if_else(score <= 5, score, 5),
            database = "Corum"
          ) %>% 
          select(source, target, complex, score, color, size, database)
      } else {
        edges_corum_table <- NULL
      }
    } else {
      edges_corum_table <- NULL
    }
  }
  if (is.null(edges_string_table) & is.null(edges_corum_table)) {
    self$edges_table <- NULL
  } else {
    self$edges_table <- edges_string_table %>%
      bind_rows(edges_corum_table)
  }
}