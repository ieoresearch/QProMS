box::use(
  shiny[moduleServer, tags, NS, div, p, h1, h4, HTML, fileInput, textInput, req, tagList, passwordInput, updateSelectInput, selectInput, actionButton, observeEvent, isolate, br, observe, updateActionButton, uiOutput, renderUI, icon],
  bslib[page_fillable, layout_columns, navset_underline, nav_spacer, nav_remove, nav_panel, nav_insert, card, accordion, accordion_panel, accordion_panel_close, accordion_panel_open, nav_select, tooltip, input_task_button],
  gargoyle[init, watch, trigger],
  shinyalert[shinyalert],
)

box::use(
  app/view/statistics,
  app/view/heatmap,
)

panels <- list(
  Volcano = list(
    target = "Rank",
    title  = "Volcano",
    ui     = statistics$ui,
    ns     = "statistics"
  ),
  Heatmap = list(
    target = "Volcano",
    title  = "Heatmap",
    ui     = heatmap$ui,
    ns     = "heatmap"
  )
)

#' @export
ui <- function(id, primary_col) {
  ns <- NS(id)
  page_fillable(
    div(
      style = paste0("background-color: ",primary_col,"; width: 100%; height: calc(45vh - 28px); padding: 2rem; margin: 0; display: flex; position: relative;"),
      div(
        style = "width: 50%; display: flex; justify-content: center; align-items: center; padding: 2rem; margin: 2rem",
        h1("Quantitative Proteomics Made Simple",
           style = "color: #404040; font-size: 3.5em; font-weight: bold; text-align: center; font-family: 'Tomorrow', sans-serif;"
        )
      ),
      div(
        style = "width: 50%; display: flex; justify-content: center; align-items: center; position: relative;",
        tags$img(
          src = "static/immagine_home.png",
          style = "max-height: 110%; max-width: 100%; object-fit: contain; position: absolute; top: 20%; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);"
        )
      )
    ),
    layout_columns(
      col_widths = c(-2, 8, -2),
      style = "display: flex; justify-content: center; align-items: center; padding: 2rem; margin: 2rem",
      div(
        layout_columns(
          gap = "2rem",
          navset_underline(
            id = ns("start_nav"),
            nav_spacer(),
            nav_panel(
              title = "Upload File",
              fileInput(
                inputId = ns("upload_file"),
                label = tooltip(
                  trigger = list("Intensity Table", icon("info-circle")),
                  "Click on HELP button to a dedicated upload guide for each software."
                ),
                multiple = FALSE,
                accept = c(".txt", ".tsv", ".csv"),
                width = "100%"
              )
            ), 
            nav_panel(
              title = "Restore analysis session",
              fileInput(
                inputId = ns("upload_params"),
                label = tooltip(
                  trigger = list("Saved Analysis", icon("info-circle")),
                  "You MUST upload a QProMS_analysis.rds file to continue a previous analysis. Is not necessary for new analysis."
                ),
                multiple = FALSE,
                accept = ".rds",
                width = "100%"
              )
            ), 
            nav_panel(
              title = "Example Dataset",
              selectInput(
                inputId = ns("example"),
                label = "Datasets",
                choices = c("TURBO vs WT" = "turbo", "APEX"),
                width = "100%",
                selected = "turbo"
              )
            ), 
            nav_spacer()
          )
        ),
        br(),
        layout_columns(
          input_task_button(
            id = ns("start"),
            label = "START",
            width = "100%",
            class = "bg-primary"
          ),
          layout_columns(
            actionButton(
              inputId = ns("setting"),
              label = "SETTINGS",
              width = "100%",
              class = "bg-secondary"
            ),
            actionButton(
              inputId = ns("help"),
              label = "HELP",
              width = "100%",
              class = "bg-light"
            )
          )
        )
      )
    )
  )
}

validate_uploaded_file <- function(upload, allowed_extensions, label) {
  extension <- tolower(tools::file_ext(upload$name))
  allowed_extensions <- tolower(allowed_extensions)
  max_size_mb <- getOption("qproms.maxUploadSizeMb", 500)
  max_size_bytes <- max_size_mb * 1024^2

  if (!extension %in% allowed_extensions) {
    shinyalert(
      title = "Unsupported file type",
      text = paste0(
        "The ", label, " must use one of these extensions: ",
        paste(paste0(".", allowed_extensions), collapse = ", "), "."
      ),
      size = "m",
      closeOnClickOutside = TRUE,
      type = "error",
      showConfirmButton = FALSE,
      timer = 5000
    )
    return(FALSE)
  }

  if (!is.null(upload$size) && upload$size > max_size_bytes) {
    shinyalert(
      title = "File too large",
      text = paste0(
        "The ", label, " is larger than the configured ",
        max_size_mb, " MB upload limit."
      ),
      size = "m",
      closeOnClickOutside = TRUE,
      type = "error",
      showConfirmButton = FALSE,
      timer = 5000
    )
    return(FALSE)
  }

  TRUE
}


#' @export
server <- function(id, r6, main_session) {
  moduleServer(id, function(input, output, session) {
    
    init("expdesig", "session")
    ns <- session$ns
    
    observeEvent(input$setting, {
      nav_select("top_navigation", "Settings", session = main_session)
    })
    observeEvent(input$help, {
      nav_select("top_navigation", "Help", session = main_session)
    })
    
    observeEvent(input$start, {
      if(input$start_nav == "Upload File") {
        if(is.null(input$upload_file)) {
          shinyalert(
            title = "Missing Dataset!",
            text = "Before pressing START you MUST upload a dataset.",
            size = "m",
            closeOnClickOutside = TRUE,
            type = "info",
            showConfirmButton = FALSE,
            timer = 3500
          )
        }
        req(input$upload_file)
        if (!validate_uploaded_file(input$upload_file, c("txt", "tsv", "csv"), "dataset")) {
          return(invisible(NULL))
        }
        ## Load the data
        r6$loading_data(input_path = input$upload_file$datapath)
        trigger("expdesig")
        nav_select("top_navigation", "Design", session = main_session)
      }
      if(input$start_nav == "Restore analysis session") {
        if(is.null(input$upload_params)) {
          shinyalert(
            title = "Missing QProMS_analysis.rds file!",
            text = "Before pressing START you MUST upload a QProMS_analysis.rds file.",
            size = "m",
            closeOnClickOutside = TRUE,
            type = "info",
            showConfirmButton = FALSE,
            timer = 3500
          )
        }
        req(input$upload_params)
        if (!validate_uploaded_file(input$upload_params, "rds", "analysis session")) {
          return(invisible(NULL))
        }
        if(!is.null(input$upload_params)) {
          restore_ok <- tryCatch(
            {
              r6$loading_parameters(input_path = input$upload_params$datapath, r6)
              TRUE
            },
            error = function(e) {
              shinyalert(
                title = "Invalid session file",
                text = conditionMessage(e),
                size = "m",
                closeOnClickOutside = TRUE,
                type = "error",
                showConfirmButton = FALSE,
                timer = 7000
              )
              FALSE
            }
          )
          if (!restore_ok) {
            return(invisible(NULL))
          }
          trigger("session", "genes")
          nav_select("top_navigation", "Preprocessing", session = main_session)
          purrr::walk(names(panels), ~ nav_remove("top_navigation", target  = .x, session = main_session))
          if (r6$with_statistics) {
            purrr::walk(
              panels,
              ~ nav_insert(
                "top_navigation",
                target  = .x$target,
                select  = FALSE,
                session = main_session,
                nav_panel(
                  title = .x$title,
                  class = "html-fill-item html-fill-container bslib-gap-spacing",
                  style = "--bslib-navbar-margin:0; padding:0",
                  .x$ui(ns(.x$ns))
                )
              )
            )
          } 
        }
      }
      if(input$start_nav == "Example Dataset") {
        if(isolate(input$example == "turbo")) {
          r6$loading_parameters(input_path = "app/static/QProMS_example_turbo_vs_wt.rds", r6)
        } else {
          r6$loading_parameters(input_path = "app/static/QProMS_example_dataset_p62.rds", r6)
        }
        trigger("session", "genes")
        nav_select("top_navigation", "Preprocessing", session = main_session)
        purrr::walk(names(panels), ~ nav_remove("top_navigation", target  = .x, session = main_session))
        purrr::walk(
          panels,
          ~ nav_insert(
            "top_navigation",
            target  = .x$target,
            select  = FALSE,
            session = main_session,
            nav_panel(
              title = .x$title,
              class = "html-fill-item html-fill-container bslib-gap-spacing",
              style = "--bslib-navbar-margin:0; padding:0",
              .x$ui(ns(.x$ns))
            )
          )
        )
      }
    })
    statistics$server("statistics", r6 = r6, main_session = main_session)
    heatmap$server("heatmap", r6 = r6, main_session = main_session)
  })
}
