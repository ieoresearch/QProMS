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


r6$plot_heatmap(z_score = TRUE, n_cluster = 2, clustering_method = "ward.D2", order_by_expdesing = FALSE)

plot_ppi_network = function(list_from, score_thr, isolate_nodes, layout, show_names, selected, filtered) {
  edges <- self$edges_table %>%
    filter(score >= score_thr)
  
  nodes <- self$nodes_table
  
  if (!isolate_nodes) {
    final_list <- unique(c(edges$source, edges$target))
    nodes <- nodes %>%
      filter(gene_names %in% final_list)
  }
  
  if (filtered) {
    nodes <- nodes %>%
      filter(gene_names %in% selected)
  }
  
  if (nrow(nodes) == 0) {
    return(
      e_charts(renderer = self$plot_format) %>%
        e_text(
          "No nodes to display after filtering.",
          x = "center",
          y = "center",
          textStyle = list(fontSize = 20, color = "red")
        ) %>%
        e_show_loading(text = "Loading...", color = "#0d6efd")
    )
  }
  
  p <- e_charts(renderer = self$plot_format) %>%
    e_graph(
      roam = TRUE,
      layout = layout,
      zoom = 0.5,
      force = list(
        initLayout = "circular",
        repulsion = 800,
        edgeLength = 150,
        layoutAnimation = FALSE
      ),
      autoCurveness = TRUE,
      emphasis = list(focus = "adjacency")
    ) %>%
    e_graph_nodes(
      nodes = nodes,
      names = gene_names,
      value = p_val,
      size = size,
      category = category,
      legend = FALSE
    ) %>%
    e_graph_edges(
      edges = edges,
      source = source,
      target = target,
      value = score,
      size = size
    ) %>%
    e_tooltip() %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    e_show_loading(text = "Loading...", color = "#0d6efd")
  
  if (show_names) {
    p <- p %>%
      e_labels(fontSize = 10)
  }
  
  p$x$opts$series[[1]]$links <- purrr::map2(p$x$opts$series[[1]]$links, edges$color, ~ modifyList(.x, list(lineStyle = list(color = .y))))
  
  if (list_from == "univariate") {
    p$x$opts$series[[1]]$data <- purrr::map2(p$x$opts$series[[1]]$data, nodes$color, ~ modifyList(.x, list(itemStyle = list(color = .y))))
  }
  
  if (!is.null(selected) && !filtered) {
    p$x$opts$series[[1]]$data <- purrr::map(p$x$opts$series[[1]]$data, function(node) {
      if (node$name %in% selected) {
        node$itemStyle <- modifyList(node$itemStyle, list(borderColor = "gray20", borderWidth = 2, color = "#ffc107"))
        node$symbolSize <- 30
      }
      node
    })
  }
  return(p)
}

plot_empty_message = function(message) {
  e_chart(data.frame(x = "", y = ""), x, renderer = "svg") %>%
    e_bar(y) %>%
    e_legend(show = FALSE) %>%
    e_draft(
      text = message,
      size = "2rem",
      opacity = 1,
      color = "#555"
    ) %>%
    e_show_loading(text = "Loading...", color = "#0d6efd")
}
plot_empty_message("Ciao")

