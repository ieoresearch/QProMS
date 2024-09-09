box::use(
  shiny[moduleServer, observe, downloadButton, updateSelectInput, downloadHandler, NS, conditionalPanel, radioButtons, selectInput, actionButton, hr, h3, h4, br, div, observeEvent, req, sliderInput, checkboxGroupInput, isolate],
  bslib[page_fillable, layout_columns, card, card_header, card_body, accordion, accordion_panel, nav_select, tooltip],
  gargoyle[init, watch, trigger],
  quarto[quarto_render],
  purrr[set_names, map],
  shinyalert[shinyalert],
  dplyr[`%>%`],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, -1, 10, -1, 12),
      row_heights = c(1,10,1),
      div(),
      card(
        card_header(h4(class = "text-center", "Download Results")),
        card_body(
          accordion(
            id = ns("accordion"),
            multiple = FALSE,
            accordion_panel(
              title = "Download Result Tables",
              id = ns("tables"),
              layout_columns(
                selectInput(
                  inputId = ns("select_table"),
                  label = "Select Table",
                  choices = c("Filtred", "Normalized", "Imputed", "Ranked", "Volcanos", "Heatmap", "Nodes", "Edges", "ORA", "GSEA"),
                  selected = "Filtred",
                  width = "100%"
                ),
                selectInput(
                  inputId = ns("table_extension"),
                  label = "File extension ",
                  choices = c(".xlsx", ".csv", ".tsv"),
                  selected = ".xlsx",
                  width = "100%"
                ),
                conditionalPanel(
                  condition = "input.select_table == 'Filtred' || input.select_table == 'Normalized' || input.select_table == 'Imputed' || input.select_table == 'Volcanos' || input.select_table == 'Heatmap'",
                  ns = ns,
                  selectInput(
                    inputId = ns("add_metadata"),
                    label = "Add Metadata Columns",
                    choices = NULL,
                    width = "100%",
                    multiple = TRUE
                  )
                ),
                downloadButton(
                  outputId = ns("download_table"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary mt-auto",
                  icon = NULL
                )
              )
            ),
            accordion_panel(
              title = "Define & Download Report",
              id = ns("report"),
              layout_columns(
                col_widths = c(9, 3),
                checkboxGroupInput(
                  inputId = ns("report_section"),
                  label = "Report Section",
                  choices = c("Preprocessing", "PCA", "Correlation", "Rank", "Volcano", "Heatmap", "Network", "ORA", "GSEA"),
                  inline = TRUE,
                  selected = c("Preprocessing", "PCA", "Correlation", "Rank", "Volcano", "Heatmap", "Network", "ORA", "GSEA")
                ),
                downloadButton(
                  outputId = ns("download_report"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary mt-auto",
                  icon = NULL
                )
              )
            ),
            accordion_panel(
              title = "Save Analysis Session",
              id = ns("parmas"),
              layout_columns(
                col_widths = c(9, 3),
                div(
                  class = "alert alert-info",
                  style = "white-space: pre-wrap;",
                  role = "alert",
                  "The analysis session with all the parameters used will be stored in a QProMS_analysis.rds file. You can reload this file into home page to continue the analysis."
                ),
                downloadButton(
                  outputId = ns("download_params"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary",
                  icon = NULL
                )
              )
            )
          )
        )
      ),
      div()
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      watch("genes")
      updateSelectInput(inputId = "add_metadata", choices = colnames(r6$raw_data_unique), selected = NULL)
    })
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste0(input$select_table, "_table_", Sys.Date(), input$table_extension)
      },
      content = function(file) {
       
        r6$download_table(
          handler_file = file,
          table_type = input$select_table,
          table_extension = input$table_extension,
          extra_columns = input$add_metadata
        )
       
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("QProMS_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        
        params <- c("Preprocessing", "PCA", "Correlation", "Rank",
                    "Volcano", "Heatmap", "Network", "ORA", "GSEA")
        
        shinyalert(
          title = "The report is rendering",
          text = "Wait the download window to pops, the report is rendering in background.",
          size = "m",
          closeOnClickOutside = TRUE,
          type = "info",
          showConfirmButton = FALSE,
          timer = 0
        )
        
        r6$download_parameters(handler_file = "app/logic/QProMS_session_internal.rds", r6class = r6)

        param_list <- map(params, ~ .x %in% isolate(input$report_section)) %>%
         set_names(params)
        
        quarto_render(
          "app/logic/Report_QProMS.qmd",
          execute_params = param_list,
          quiet = FALSE
        )
        
        file.copy("app/logic/Report_QProMS.html", file)
        file.remove("app/logic/Report_QProMS.html")
      }
    )
    
    output$download_params <- downloadHandler(
      filename = function() {
        paste0("QProMS_analysis_", Sys.Date(), ".rds")
      },
      content = function(file) {
        r6$new_session <- FALSE
        r6$download_parameters(handler_file = file, r6class = r6)
      }
    )
    
  })
}
