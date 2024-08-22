box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, isolate, icon, observe, updateSelectInput, updateSelectizeInput, reactive, observeEvent, numericInput, conditionalPanel, selectizeInput],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, input_switch, tooltip, input_task_button],
  gargoyle[watch, trigger],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  reactable[reactableOutput, renderReactable, getReactableState],
  dplyr[pull, `%>%`],
  purrr[map]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        full_screen = TRUE, 
        nav_panel(
          "BarPlot",
          trelliscopeOutput(ns("bar_plot"), style = "height: 100%")
        ),
        nav_panel(
          "Table",
          reactableOutput(ns("table"))
        )
      )
    ),
    sidebar = sidebar(
      input_task_button(
        id = ns("update"),
        label = "UPDATE"
      ),
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("inputs"),
          selectInput(
            inputId = ns("strategy"),
            label = "Inputs From",
            choices = c(
              "Rank" = "top_rank",
              "Volcano" = "univariate",
              "Heatmap" = "multivariate",
              "Manual selection" = "manual"
            ), 
            selected = "univariate"
          ),
          conditionalPanel(
            condition = "input.strategy == 'top_rank'",
            ns = ns,
            input_switch(
              id = ns("by_cond_input"),
              label = tooltip(
                trigger = list(
                  "Merge Condition",
                  icon("info-circle")
                ),
                "If TRUE, use the Intensity mean of each condition."
              ),
              value = FALSE
            ),
            selectInput(
              inputId = ns("target"),
              label = "Genes from",
              choices = NULL,
              selected = NULL, 
              width = "auto"
            ),
            selectInput(
              inputId = ns("selections"),
              label = "Selection",
              choices = c("From top" = "top", "From bottom" = "bot"),
              selected = "top", 
              width = "auto"
            ),
            sliderInput(
              inputId = ns("top_n_slider"),
              label = "n % of proteins",
              min = 1,
              max = 50,
              value = 10,
              step = 1
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            selectInput(
              inputId = ns("volcano_input"),
              label = "Contrast",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            selectInput(
              inputId = ns("clusters_input"),
              label = "Clusters",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'manual'",
            ns = ns,
            selectizeInput(
              inputId = ns("gene_names_vector"),
              label = "Gene names",
              choices = "gene",
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = ns("ontology_input"),
            label = "Ontology",
            choices = c(
              "Biological Processes" = "BP",
              "Molecular Function" = "MF",
              "Cellular Components" = "CC"
            ),
            selected = "BP"
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          sliderInput(
            inputId = ns("simplify_thr"),
            label = "Simplify threshold",
            min = 0.1,
            max = 1,
            value = 1,
            step = 0.1
          ),
          input_switch(
            id = ns("background_input"),
            label = "Background",
            value = FALSE
          ),
          numericInput(
            inputId = ns("alpha_input"),
            label = "Alpha",
            value = 0.05,
            min = 0.01,
            max = 0.05,
            step = 0.01
          ),
          selectInput(
            inputId = ns("truncation_input"),
            label = "Truncation",
            choices = c(
              "Benjamini & Hochberg" = "BH",
              "Bonferroni" = "bonferroni",
              "Holm (1979)" = "holm",
              "Hochberg (1988)" = "hochberg",
              "Hommel (1988)" = "hommel",
              "Benjamini & Yekutieli" = "BY",
              "None" = "none"),
            selected = "BH"
          )
        ),
        accordion_panel(
          title = "Visual Parameters",
          id = ns("v_params"),
          selectInput(
            inputId = ns("arrenged"),
            label = "X axis",
            choices = c(
              "Fold Enrichment" = "fold_enrichment",
              "p.value" = "pvalue",
              "p.adjust" = "p.adjust",
              "Count" = "Count"
            ), 
            selected = "fold_enrichment"
          ),
          numericInput(
            inputId = ns("show_category"),
            label = "Show Category",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      watch("stat")
      chs <- unlist(map(r6$contrasts, ~ c(paste0(.x, "_up"), paste0(.x, "_down"))))
      updateSelectInput(inputId = "volcano_input", choices = chs, selected = chs[1])
    })

    observe({
      watch("heatmap")
      chh <- paste0("cluster_", 1:r6$clusters_number)
      updateSelectInput(inputId = "clusters_input", choices = chh, selected = chh[1])
    })
    
    observe({
      watch("genes")
      if(!is.null(r6$expdesign)) {
        if(input$by_cond_input){
          updateSelectInput(inputId = "target", choices = unique(r6$expdesign$condition))
        } else {
          updateSelectInput(inputId = "target", choices = r6$expdesign$label)
        }
      }
      updateSelectizeInput(inputId = "gene_names_vector", choices = r6$filtered_gene_vector, server = TRUE)
    })
    
    
    observeEvent(input$update ,{
      r6$go_ora_from_statistic <- input$strategy
      r6$go_ora_alpha <- as.double(input$alpha_input)
      r6$go_ora_p_adj_method <- input$truncation_input
      r6$go_ora_term <- input$ontology_input
      r6$go_ora_top_n <- input$show_category
      r6$go_ora_simplify_thr <- input$simplify_thr
      r6$go_ora_background <- input$background_input
      r6$go_ora_plot_arrenge <- input$arrenged
      
      if(r6$go_ora_from_statistic == "univariate") {
        r6$go_ora_focus <- input$volcano_input
      }
      
      if(r6$go_ora_from_statistic == "top_rank") {
        r6$protein_rank_target <- input$target
        r6$protein_rank_by_cond <- input$by_cond_input
        r6$protein_rank_selection <- input$selections
        r6$protein_rank_top_n <- as.numeric(input$top_n_slider) / 100
        if(!is.null(r6$data)) {
          r6$rank_protein(
            target = r6$protein_rank_target,
            by_condition = r6$protein_rank_by_cond,
            selection = r6$protein_rank_selection,
            n_perc = r6$protein_rank_top_n
          )
        }
        r6$go_ora_focus <- r6$protein_rank_target
      }
      
      if(r6$go_ora_from_statistic == "multivariate") {
        r6$go_ora_focus <- input$clusters_input
      }
      
      if(r6$go_ora_from_statistic == "manual") {
        r6$go_ora_focus <- input$gene_names_vector
      }
      
      r6$go_ora(
        list_from = r6$go_ora_from_statistic,
        focus = r6$go_ora_focus,
        ontology = r6$go_ora_term,
        simplify_thr = r6$go_ora_simplify_thr,
        alpha = r6$go_ora_alpha,
        p_adj_method = r6$go_ora_p_adj_method,
        background = r6$go_ora_background
      )
      r6$print_ora_table(r6$go_ora_plot_arrenge)
      trigger("plot")
    })
   
    output$bar_plot <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$ora_result_list)) {
        focus_plot <- r6$go_ora_focus
        if(r6$go_ora_from_statistic == "manual") {focus_plot <- "manual"}
        r6$plot_ora(focus_plot, r6$go_ora_plot_arrenge, r6$go_ora_top_n)
      }
    })
    
    output$table <- renderReactable({
      watch("plot")
      r6$reactable_functional_analysis(r6$ora_table)
    })
    
  })
}
