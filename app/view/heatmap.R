box::use(
  shiny[moduleServer, NS, actionButton, br, selectInput, icon, div, numericInput, observe, updateSelectInput, observeEvent, req, isolate, reactive],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_select, nav_panel, sidebar, tooltip, input_switch, accordion, accordion_panel, input_task_button],
  gargoyle[watch, trigger, init],
  reactable[reactableOutput, renderReactable, getReactableState],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  plotly[plotlyOutput, renderPlotly],
  echarts4r[echarts4rOutput, renderEcharts4r],
  dplyr[pull, `%>%`]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        title = "Plots",
        id = ns("heatmap_plots_nav"),
        full_screen = TRUE, 
        nav_panel(
          title = "Heatmap",
          plotlyOutput(ns("heatmap_plot"))
        ), 
        nav_panel(
          "Protein Profile",
          echarts4rOutput(ns("profile_protein_plot"))
        ),
        nav_panel(
          "Cluster Profile",
          trelliscopeOutput(ns("plot_cluster_profile"), style = "height: 100%")
        )
      ),
      navset_card_underline(
        title = "Table",
        full_screen = TRUE, 
        nav_panel(
          "Results",
          reactableOutput(ns("table_anova"))
        )
      )
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("define"),
          numericInput(
            inputId = ns("n_cluster_input"),
            label = "N° of clusters",
            value = 1,
            min = 1,
            step = 1
          ),
          selectInput(
            inputId = ns("clust_method"),
            label = "HClust method",
            choices = c("complete", "single", "average", "ward.D2", "mcquitty"),
            selected = "complete"
          ),
          input_switch(
            id = ns("zscore_input"),
            label = "Z-score",
            value = TRUE
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          numericInput(
            inputId = ns("alpha_input_milti"),
            label = "Alpha",
            value = 0.05,
            min = 0.01,
            max = 0.05,
            step = 0.01
          ),
          selectInput(
            inputId = ns("truncation_input_milti"),
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
          input_switch(
            id = ns("order_by_expdesing"),
            label = "Order by expdesing",
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
    
    init("heatmap")
    
    observeEvent(input$update ,{
      r6$anova_alpha <- input$alpha_input_milti
      r6$anova_p_adj_method <- input$truncation_input_milti
      r6$z_score <- input$zscore_input
      r6$anova_clust_method <- input$clust_method
      r6$anova_manual_order <- input$order_by_expdesing
      r6$clusters_number <- input$n_cluster_input
      
      r6$stat_anova(
        alpha = r6$anova_alpha,
        p_adj_method = r6$anova_p_adj_method
      )
      
      trigger("plot", "heatmap")
    })
    output$heatmap_plot <- renderPlotly({
      watch("plot")
      if(!is.null(r6$anova_table)) {
        r6$plot_heatmap(order_by_expdesing = r6$anova_manual_order)
      }
      
    })
    output$plot_cluster_profile <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$anova_table)) {
        r6$plot_cluster_profile()
      }
    })
    
    output$table_anova <- renderReactable({
      watch("plot")
      r6$reactable_interactive(r6$print_anova_table())
    })
    
    gene_selected_anova <- reactive(getReactableState("table_anova", "selected"))
    
    output$profile_protein_plot <- renderEcharts4r({
      watch("plot")
      if(!is.null(r6$anova_table)) {
        table <- r6$print_anova_table()
        highlights <- table[gene_selected_anova(),] %>% 
          pull(gene_names)
        r6$plot_protein_profile(highlights) 
      }
    })
    
    
  })
}
