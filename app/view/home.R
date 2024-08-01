box::use(
  shiny[moduleServer, NS, div, p, h1, h4, HTML, fileInput, textInput, req, tagList, passwordInput, updateSelectInput, selectInput, actionButton, observeEvent, isolate, br, observe, updateActionButton, uiOutput, renderUI, icon],
  bslib[page_fillable, layout_columns, card, accordion, accordion_panel, accordion_panel_close, accordion_panel_open, nav_select, tooltip, input_task_button],
  gargoyle[init, watch, trigger]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, -1, 5, 5, -1, 12),
      row_heights = c(1,10,1),
      div(),
      card(
        accordion(
          id = ns("accordion"),
          multiple = FALSE,
          accordion_panel(
            title = "Upload File",
            id = ns("upload"),
            fileInput(
              inputId = ns("upload_file"),
              label = tooltip(
                trigger = list(
                  "Intensity Table",
                  icon("info-circle")
                ),
                "Upload a intensity table. accept '.txt', '.tsv', '.csv' format."
              ),
              multiple = FALSE,
              accept = c(".txt", ".tsv", ".csv"),
              width = "100%"
            ),
            br(),
            actionButton(
              inputId = ns("confirm"),
              label = "START",
              width = "100%",
              class = "bg-primary"
            )
          ),
          accordion_panel(
            title = "Upload Parameters",
            id = ns("params"),
            fileInput(
              inputId = ns("upload_params"),
              label = tooltip(
                trigger = list(
                  "Parameters",
                  icon("info-circle")
                ),
                "Upload a QProMS parameters yaml for reproducibility. Is not necessary for new analysis."
              ),
              multiple = FALSE,
              placeholder = "QProMS_parameters.yaml",
              accept = ".yaml",
              width = "100%"
            ),
            br(),
            input_task_button(
              id = ns("start_with_params"),
              label = "START",
              style = "width: 100%;"
            )
          )
        )
      ),
      card(
        class = "bg-primary",
        div(
          class = "my-3 mx-2 py-3",
          h1(class = "text-center", HTML('<strong>Q</strong>uantitative <strong>PRO</strong>teomics ')),
          h1(class = "text-center mb-5", HTML('<strong>M</strong>ade <strong>S</strong>imple')),
          p(
            class = "px-5",
            paste(
              "Welcome to QProMS! ",
              "This Shiny app enables easy but powerful and reproducible ", 
              "analyses for label-free proteomics data. It works out of ",  
              "the box with major data-dependent and data-independent ",
              "search engine results (MaxQuant, FragPipe, Spectronaut, ",
              "DIA-NN) as well as custom result tables. It ",
              "can produce publication-quality figures and export HTML ", 
              "reports and parameter files for sharing and reproducing ",
              "results. "
            )
          )
        )
      ),
      div()
    )
  )
}

#' @export
server <- function(id, r6, main_session) {
  moduleServer(id, function(input, output, session) {
    
    init("expdesig")
    
    observeEvent(input$confirm, {
      req(input$upload_file)
      ## Load the data
      r6$loading_data(
        input_path = input$upload_file$datapath,
        input_name = input$upload_file$name
      )
      trigger("expdesig")
      nav_select("top_navigation", "Desing", session = main_session)
    })
    
    observeEvent(input$start_with_params, {
      req(input$upload_file)
      
      if(!is.null(input$upload_params)) {
        r6$loading_patameters(input_path = input$upload_params$datapath)
        r6$preprocessing()
        r6$shiny_wrap_workflow()
        nav_select("top_navigation", "Preprocessing", session = main_session)
      }
    })
  })
}
