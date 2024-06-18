box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, isolate, icon, observe, updateSelectInput, reactive, observeEvent, conditionalPanel],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, input_switch, tooltip, input_task_button],
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
        title = "Network",
        full_screen = TRUE, 
        nav_panel(
          "Plot",
          echarts4rOutput(ns("network_plot"))
        )
      ),
      navset_card_underline(
        title = "Tables",
        full_screen = TRUE, 
        nav_panel(
          "Nodes",
          reactableOutput(ns("table_nodes"))
        ),
        nav_panel(
          "Edges",
          reactableOutput(ns("table_edges"))
        )
      )
    ),
    sidebar = sidebar(
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
              "Statistics" = "univariate",
              "Heatmap" = "multivariate"
            ), 
            selected = "univariate"
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
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          selectInput(
            inputId = ns("db_source"),
            label = "Database",
            choices = c("String" = "string", "Corum" = "corum"),
            selected = "string", 
            multiple = TRUE
          ),
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
      ),
      input_task_button(
        id = ns("update"),
        label = "UPDATE"
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      watch("stat")
      updateSelectInput(inputId = "test_uni_input", choices = r6$contrasts)
    })
    
    observe({
      watch("heatmap")
      updateSelectInput(inputId = "clusters_input", choices = paste0("cluster_", 1:r6$clusters_number))
    })
    
    observeEvent(input$update ,{
      r6$network_from_statistic <- input$strategy
      r6$pdb_database <- input$db_source
      r6$network_uni_direction <- input$ui_direction_input
      r6$network_score_thr <- input$score_thr
      focus_net <- "top_rank"
      
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
      
      if (!is.null(r6$nodes_table)) {
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = r6$network_score_thr
        )
        highlights <- nodes[gene_selected(), ] %>% 
          pull(gene_names)
        fil <- isolate(input$keep_selected)
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
        r6$plot_empty_message("No network to display.")
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
      
      highlights <- nodes[gene_selected(), ] %>% 
        pull(gene_names)
      
      table <- r6$print_edges(
        selected_nodes = highlights,
        score_thr = r6$network_score_thr
      )
      r6$reactable_network(table, FALSE)
    })

  })
}
