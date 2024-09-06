box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, isolate, icon, observe, updateSliderInput, updateSelectInput, reactive, observeEvent, conditionalPanel],
  bslib[page_sidebar, layout_columns, navset_card_underline, update_switch, nav_panel, sidebar, accordion, accordion_panel, input_switch, tooltip, input_task_button],
  gargoyle[watch, trigger],
  echarts4r[echarts4rOutput, renderEcharts4r],
  reactable[reactableOutput, renderReactable, getReactableState],
  dplyr[pull, `%>%`]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        full_screen = TRUE, 
        nav_panel(
          tooltip(
            trigger = list(
              "Network Plot",
              icon("info-circle")
            ),
            "Select proteins in the Nodes table to highlight them."
          ),
          echarts4rOutput(ns("network_plot"))
        ),
        nav_panel(
          "Nodes Table",
          reactableOutput(ns("table_nodes"))
        ),
        nav_panel(
          "Edges Table",
          reactableOutput(ns("table_edges"))
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
              "Heatmap" = "multivariate"
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
              label = tooltip(
                trigger = list(
                  "Selection",
                  icon("info-circle")
                ),
                "This selection is used for generate the network plot. Press 'UPDATE' to validate a new selection."
              ),
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
              inputId = ns("test_uni_input"),
              label = "Contrasts",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              inputId = ns("ui_direction_input"),
              label = "Directions",
              choices = c("Up" = "up", "Down" = "down"),
              selected = "up",
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
          selectInput(
            inputId = ns("db_source"),
            label = "Database",
            choices = c("String" = "string", "Corum" = "corum"),
            selected = "string", 
            multiple = TRUE
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          sliderInput(
            inputId = ns("score_thr"),
            label = "Score threshold",
            min = 0,
            max = 0.9,
            value = 0.4,
            step = 0.1
          )
        ),
        accordion_panel(
          title = "Visual Parameters",
          id = ns("v_params"),
          selectInput(
            inputId = ns("layout"),
            label = NULL,
            choices = c("force", "circular"),
            selected = "force"
          ),
          input_switch(
            id = ns("isolate_nodes_input"),
            label = "Keep isolate nodes",
            value = FALSE
          ),
          input_switch(
            id = ns("names_input"),
            label = "Show names",
            value = TRUE
          ),
          input_switch(
            id = ns("keep_selected"),
            label = tooltip(
              trigger = list(
                "Subset",
                icon("info-circle")
              ),
              "If TRUE, display network with only selected nodes."
            ),
            value = FALSE
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
      watch("genes")
      if(!is.null(r6$expdesign)) {
        if(input$by_cond_input){
          updateSelectInput(inputId = "target", choices = unique(r6$expdesign$condition))
        } else {
          updateSelectInput(inputId = "target", choices = r6$expdesign$label)
        }
      }
    })
    
    observe({
      watch("stat")
      updateSelectInput(inputId = "test_uni_input", choices = r6$contrasts, selected = r6$contrasts[1])
    })
    
    observe({
      watch("heatmap")
      ch <- paste0("cluster_", 1:r6$clusters_number)
      updateSelectInput(inputId = "clusters_input", choices = ch, selected = ch[1])
    })
    
    observe({
      watch("session")
      updateSelectInput(inputId = "strategy", selected = r6$network_from_statistic)
      update_switch(id = "by_cond_input", value = r6$protein_rank_by_cond)
      updateSelectInput(inputId = "target", selected = r6$protein_rank_target)
      updateSelectInput(inputId = "selections", selected = r6$protein_rank_selection)
      updateSliderInput(inputId = "top_n_slider", value = r6$protein_rank_top_n * 100)
      updateSelectInput(inputId = "test_uni_input", choices = r6$contrasts, selected = r6$contrasts[1])
      updateSelectInput(inputId = "ui_direction_input", selected = r6$network_uni_direction)
      updateSelectInput(inputId = "clusters_input", selected = r6$network_focus_multi)
      updateSelectInput(inputId = "db_source", selected = r6$pdb_database)
      updateSliderInput(inputId = "score_thr", value = r6$network_score_thr)
    })
    
    observeEvent(input$update ,{
      r6$network_from_statistic <- input$strategy
      r6$pdb_database <- input$db_source
      r6$network_uni_direction <- input$ui_direction_input
      r6$network_score_thr <- input$score_thr
      
      if(r6$network_from_statistic == "top_rank") {
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
        focus_net <- "top_rank"
      }
      
      if(r6$network_from_statistic == "univariate") {
        r6$network_focus_uni <- input$test_uni_input
        focus_net <- r6$network_focus_uni
      }
      
      if(r6$network_from_statistic == "multivariate") {
        r6$network_focus_multi <- input$clusters_input
        focus_net <- r6$network_focus_multi
      }
      
      r6$make_nodes(
        list_from = r6$network_from_statistic,
        focus = focus_net,
        direction = r6$network_uni_direction
      )
      r6$make_edges(source = r6$pdb_database)
      
      trigger("plot")
    })
    
    output$network_plot <- renderEcharts4r({
      watch("plot")
      
      if(!is.null(r6$nodes_table)) {
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = r6$network_score_thr
        )
        if(!is.null(nodes)) {
          highlights <- nodes[gene_selected(), ] %>% 
            pull(gene_names)
          fil <- isolate(input$keep_selected)
        } else {
          highlights <- NULL
        }
        
        if(length(highlights) == 0){
          highlights <- NULL
          fil <- FALSE
        }
        r6$plot_ppi_network(
          list_from = r6$network_from_statistic,
          score_thr = r6$network_score_thr,
          isolate_nodes = isolate(input$isolate_nodes_input),
          layout = isolate(input$layout),
          show_names = isolate(input$names_input),
          selected = highlights,
          filtered = fil
        )
      } else {
        # r6$plot_empty_message("No network to display.")
        NULL
      }
    })
    
    gene_selected <- reactive(getReactableState("table_nodes", "selected"))
    
    output$table_nodes <- renderReactable({
      watch("plot")
      table <- r6$print_nodes(
        isolate_nodes = isolate(input$isolate_nodes_input),
        score_thr = r6$network_score_thr
      )
      r6$reactable_network(table, TRUE)
    })
    
    output$table_edges <- renderReactable({
      watch("plot")
      nodes <- r6$print_nodes(
        isolate_nodes = isolate(input$isolate_nodes_input),
        score_thr = r6$network_score_thr
      )
      if(!is.null(nodes)) {
        highlights <- nodes[gene_selected(), ] %>% 
          pull(gene_names)
        table <- r6$print_edges(
          selected_nodes = highlights,
          score_thr = r6$network_score_thr
        )
        r6$reactable_network(table, FALSE)
      }
    })

  })
}
