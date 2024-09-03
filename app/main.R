box::use(
  shiny[div, moduleServer, NS, strong, icon],
  bslib[page_navbar, page_sidebar, nav_panel, nav_item, sidebar, nav_spacer, page_fluid],
  reactable.extras[reactable_extras_dependency],
)

box::use(
  app/view/home,
  app/view/preprocessing,
  app/view/pca,
  app/view/correlation,
  app/view/upload,
  app/view/rank,
  app/view/statistics,
  app/view/heatmap,
  app/view/network,
  app/view/ora,
  app/view/gsea,
  app/view/settings,
  app/view/download,
)

box::use(
  app/logic/R6Class_QProMS,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    id = ns("top_navigation"),
    title = strong("QProMS"),
    sidebar = NULL,
    header = list(
      reactable_extras_dependency()
    ),
    nav_spacer(),
    nav_panel(title = "Home", home$ui(ns("home"))),
    nav_panel(title = "Design", upload$ui(ns("upload"))),
    nav_panel(title = "Preprocessing", preprocessing$ui(ns("preprocessing"))),
    nav_panel(title = "PCA", pca$ui(ns("pca"))),
    nav_panel(title = "Correlation", correlation$ui(ns("correlation"))),
    nav_panel(title = "Rank", rank$ui(ns("rank"))),
    nav_panel(title = "Volcano", statistics$ui(ns("statistics"))),
    nav_panel(title = "Heatmap", heatmap$ui(ns("heatmap"))),
    nav_panel(title = "Network", network$ui(ns("network"))),
    nav_panel(title = "ORA", ora$ui(ns("ora"))),
    nav_panel(title = "GSEA", gsea$ui(ns("gsea"))),
    nav_spacer(),
    nav_panel(title = "", value = "Save Results", icon = icon("download"), download$ui(ns("download"))),
    nav_panel(title = "", value = "Settings", icon = icon("gear"), settings$ui(ns("settings")))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Expand Shiny limits for upload
    options(shiny.maxRequestSize=10000*1024^2)
    ## Generate new object
    object <- R6Class_QProMS$QProMS$new()
    ## Load modules server
    home$server("home", r6 = object, main_session = session)
    upload$server("upload", r6 = object, main_session = session)
    preprocessing$server("preprocessing", r6 = object)
    pca$server("pca", r6 = object)
    correlation$server("correlation", r6 = object)
    rank$server("rank", r6 = object)
    statistics$server("statistics", r6 = object)
    heatmap$server("heatmap", r6 = object)
    network$server("network", r6 = object)
    ora$server("ora", r6 = object)
    gsea$server("gsea", r6 = object)
    download$server("download", r6 = object)
    settings$server("settings", r6 = object)
  })
}
