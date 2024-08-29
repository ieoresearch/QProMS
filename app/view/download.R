box::use(
  shiny[moduleServer, NS, radioButtons, selectInput, actionButton, hr, h3, h4, br, div, observeEvent, req, sliderInput, checkboxGroupInput],
  bslib[page_fillable, layout_columns, card, card_header, card_body, accordion, accordion_panel, nav_select, tooltip],
  gargoyle[init, watch, trigger],
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
                  choices = c("Filtred", "Normalized", "Imputed", "Volcanos", "Heatmap", "Nodes", "Edges", "ORA", "GSEA"),
                  selected = "Filtred",
                  width = "100%"
                ),
                selectInput(
                  inputId = ns("select_table"),
                  label = "Add Metadata Columns",
                  choices = NULL,
                  width = "100%",
                  multiple = TRUE
                ),
                actionButton(
                  inputId = ns("download_table"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary mt-auto"
                )
              )
            ),
            accordion_panel(
              title = "Define & Download Report",
              id = ns("report"),
              layout_columns(
                col_widths = c(8, 4),
                checkboxGroupInput(
                  inputId = ns("report_section"),
                  label = "Report Section",
                  choices = c("Prepocessing", "PCA", "Correlation", "Rank", "Volcano", "Heatmap", "Network", "ORA", "GSEA"),
                  inline = TRUE,
                  selected = c("Prepocessing", "PCA", "Correlation", "Rank", "Volcano", "Heatmap", "Network", "ORA", "GSEA")
                ),
                actionButton(
                  inputId = ns("download_report"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary mt-auto"
                )
              )
            ),
            accordion_panel(
              title = "Download Parameters",
              id = ns("parmas"),
              layout_columns(
                col_widths = c(8, 4),
                div(
                  class = "alert alert-success",
                  style = "white-space: pre-wrap;",
                  role = "alert",
                  "All parameters used during the analysis will be stored in the QProMS_parameters.yaml file."
                ),
                actionButton(
                  inputId = ns("download_params"),
                  label = "DOWNLOAD",
                  width = "100%",
                  class = "bg-primary"
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
    
    
  })
}
