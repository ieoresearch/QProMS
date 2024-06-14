box::use(
  shiny[moduleServer, NS, selectInput, br, actionButton, observeEvent, updateSelectizeInput, observe, selectizeInput, isolate, icon],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, tooltip],
  echarts4r[echarts4rOutput, renderEcharts4r],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  gargoyle[watch, trigger],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        title = "Principal Component Analysis",
        full_screen = TRUE, 
        nav_panel(
          "2D",
          echarts4rOutput(ns("pca_2d_plot"))
        ),
        nav_panel(
          "3D",
          echarts4rOutput(ns("pca_3d_plot"))
        )
      ),
      navset_card_underline(
        title = "Correlation",
        full_screen = TRUE, 
        nav_panel(
          "Heatmap",
          echarts4rOutput(ns("correlation_plot"))
        ),
        nav_panel(
          title = tooltip(
            trigger = list(
              "Scatter plot",
              icon("info-circle")
            ),
            "Multiple plot visualization."
          ),
          value = "Scatter plot",
          trelliscopeOutput(ns("scatter_plot"), style = "height: 100%")
        )
      )
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("subset"),
          selectInput(
            inputId = ns("correlation_input"),
            label = "Correlation method",
            choices = c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
            selected = "pearson"
          ),
          selectizeInput(
            inputId = ns("gene_names_vector"),
            label = tooltip(
              trigger = list(
                "Highlights names",
                icon("info-circle")
              ),
              "Use this for the Scatter plot tab."
            ),
            choices = "gene",
            multiple = TRUE
          )
        )
      ),
      actionButton(
        inputId = ns("update"),
        label = "UPDATE",
        class = "bg-primary"
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observe({
        watch("genes")
        updateSelectizeInput(inputId = "gene_names_vector", choices = r6$filtered_gene_vector, server = TRUE)
    })
    
    observeEvent(input$update, {
      r6$cor_method <- input$correlation_input
      trigger("plot")
    })
    
    output$pca_2d_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_pca(view_3d = FALSE) 
    })
    
    output$pca_3d_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_pca(view_3d = TRUE) 
    })
    
    output$correlation_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_correlation() 
    })
    
    output$scatter_plot <- renderTrelliscope({
      watch("plot")
      r6$plot_multi_scatter(isolate(input$gene_names_vector))
    })

  })
}
