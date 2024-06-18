box::use(
  shiny[div, moduleServer, NS, renderUI, tags, uiOutput, showModal, removeModal, modalDialog, observeEvent, tagList, p, h1, actionButton, icon],
  bslib[page_navbar, page_sidebar, nav_panel, nav_item, sidebar, nav_spacer],
  reactable.extras[reactable_extras_dependency],
)

box::use(
  app/view/preprocessing,
  app/view/correlation,
  app/view/upload,
  app/view/rank,
  app/view/statistics,
  app/view/heatmap,
  app/view/network,
)

box::use(
  app/logic/R6Class_QProMS,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    id = "app-upload-top_navigation",
    title = "QProMS",
    sidebar = NULL,
    header = list(
      reactable_extras_dependency()
    ),
    nav_spacer(),
    nav_panel(title = "Upload", upload$ui(ns("upload"))),
    nav_panel(title = "Preprocessing", preprocessing$ui(ns("preprocessing"))),
    nav_panel(title = "PCA & Correlation", correlation$ui(ns("correlation"))),
    nav_panel(title = "Rank", rank$ui(ns("rank"))),
    nav_panel(title = "Statistics", statistics$ui(ns("statistics"))),
    nav_panel(title = "Heatmap", heatmap$ui(ns("heatmap"))),
    nav_panel(title = "Network", network$ui(ns("network"))),
    nav_panel(title = "Functional Analysis", page_sidebar(sidebar = sidebar(title = "sb5"))),
    nav_panel(title = "Report", page_sidebar(sidebar = sidebar(title = "sb6")))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Welcome banner that pop-up at the start of application.
    showModal(
      modalDialog(
        tagList(
          div(
            class = "modal-banner",
            h1("Welcome to QProMS App!")
          ),
          p(
            paste(
              "Welcome to Quantitative PROteomics Made Simple (QProMS). ",
              "This Shiny app enables easy but powerful and reproducible ", 
              "analyses for label-free proteomics data. It works out of ",  
              "the box with major data-dependent and data-independent ",
              "search engine results (MaxQuant, FragPipe, Spectronaut, ",
              "DIA-NN, AlphaPept) as well as custom result tables. It ",
              "can produce publication-quality figures and export HTML ", 
              "reports and parameter files for sharing and reproducing ",
              "results. It can handle multiple simultaneous comparisons ",
              "between different experimental conditions."
            )
          )
        ), 
        easyClose = F,
        size = "xl",
        footer = tagList(
          actionButton(session$ns("tutorial"), "Use Example Dataset", width = "250px"),
          actionButton(
            session$ns("show_dash"),
            "Show Dashboard",
            width = "250px",
            class = "bg-primary"
          )
        )
      )
    )
    observeEvent(input$show_dash, {
      removeModal()
    })
    ## Generate new object
    object <- R6Class_QProMS$QProMS$new()
    ## Load modules server
    upload$server("upload", r6 = object)
    preprocessing$server("preprocessing", r6 = object)
    correlation$server("correlation", r6 = object)
    rank$server("rank", r6 = object)
    statistics$server("statistics", r6 = object)
    heatmap$server("heatmap", r6 = object)
    network$server("network", r6 = object)
    
  })
}
