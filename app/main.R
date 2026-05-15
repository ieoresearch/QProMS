# =============================================
# QProMS - Main Application
# =============================================

box::use(
  shiny[moduleServer, NS, tags, icon],
  bslib[page_navbar, nav_panel, nav_item, nav_spacer, bs_theme]
)

box::use(
  home          = app/view/home,
  upload        = app/view/upload,
  preprocessing = app/view/preprocessing,
  pca           = app/view/pca,
  correlation   = app/view/correlation,
  rank          = app/view/rank,
  stats_mod     = app/view/statistics,
  heat_mod      = app/view/heatmap,
  network       = app/view/network,
  ora           = app/view/ora,
  gsea          = app/view/gsea,
  settings      = app/view/settings,
  download      = app/view/download,
  help          = app/view/help
)

box::use(
  QProMS_module = app/logic/R6Class_QProMS
)

#' @export
app_ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    id = ns("top_navigation"),
    title = tags$a("QProMS", href = "?", style = "text-decoration: none; color: inherit;"),
    sidebar = NULL,
    bg = "#6EC1E4",
    gap = "1rem",
    header = list(
      tags$head(
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Tomorrow:wght@400;700&display=swap",
          rel = "stylesheet"
        )
      )
    ),
    theme = bs_theme(version = 5, primary = "#6EC1E4"), 
    nav_spacer(),
    nav_panel(title = "Home", home$ui(ns("home"), primary_col = "#6EC1E4")),
    nav_panel(title = "Design", upload$ui(ns("upload"))),
    nav_panel(title = "Preprocessing", preprocessing$ui(ns("preprocessing"))),
    nav_panel(title = "PCA", pca$ui(ns("pca"))),
    nav_panel(title = "Correlation", correlation$ui(ns("correlation"))),
    nav_panel(title = "Statistics", stats_mod$ui(ns("statistics"))),
    nav_panel(title = "Heatmap", heat_mod$ui(ns("heatmap"))),
    nav_panel(title = "Rank", rank$ui(ns("rank"))),
    nav_panel(title = "Network", network$ui(ns("network"))),
    nav_panel(title = "ORA", ora$ui(ns("ora"))),
    nav_panel(title = "GSEA", gsea$ui(ns("gsea"))),
    nav_spacer(),
    nav_panel(title = "Export", value = "Save Results", download$ui(ns("download"))),
    nav_item(tags$a(href = "https://github.com/ieoresearch/QProMS", target = "_blank", icon("github"))),
    nav_panel(title = "", value = "Settings", icon = icon("gear"), settings$ui(ns("settings"))),
    nav_panel(title = "", value = "Help", icon = icon("question"), help$ui(ns("help")))
  )
}

#' @export
app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    object <- QProMS_module$QProMS$new()
    options(shiny.maxRequestSize = 10000 * 1024^2)
    
    home$server("home", r6 = object, main_session = session)
    upload$server("upload", r6 = object, main_session = session)
    preprocessing$server("preprocessing", r6 = object)
    pca$server("pca", r6 = object)
    correlation$server("correlation", r6 = object)
    rank$server("rank", r6 = object)
    network$server("network", r6 = object, main_session = session)
    ora$server("ora", r6 = object, main_session = session)
    gsea$server("gsea", r6 = object, main_session = session)
    stats_mod$server("statistics", r6 = object)
    heat_mod$server("heatmap", r6 = object)
    download$server("download", r6 = object)
    settings$server("settings", r6 = object, main_session = session)
    help$server("help", r6 = object, main_session = session)
  })
}

#' @export
ui <- function(request) {
  app_ui("app")
}

#' @export
server <- function(input, output, session) {
  app_server("app")
}