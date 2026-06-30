box::use(
  shiny[div, moduleServer, NS, strong, icon, tags],
  bslib[page_navbar, page_sidebar, nav_panel, nav_item, sidebar, nav_spacer, page_fluid, bs_theme],
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
  app/view/help,
)

box::use(
  app/logic/R6Class_QProMS,
)

object <- R6Class_QProMS$QProMS$new()

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    id = ns("top_navigation"),
    title = tags$a("QProMS", href = "?", style = "text-decoration: none; color: inherit;"),
    sidebar = NULL,
    bg = object$primary_color,
    gap = "1rem",
    header = list(
      tags$head(
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Tomorrow:wght@400;700&display=swap",
          rel = "stylesheet"
        )
      )
    ),
    theme = bs_theme(version = 5, primary = object$primary_color), 
    nav_spacer(),
    nav_panel(title = "Home", home$ui(ns("home"), primary_col = object$primary_color), style = "padding: 0 !important; margin: -1px;"),
    nav_panel(title = "Design", upload$ui(ns("upload"))),
    nav_panel(title = "Preprocessing", preprocessing$ui(ns("preprocessing"))),
    nav_panel(title = "PCA", pca$ui(ns("pca"))),
    nav_panel(title = "Correlation", correlation$ui(ns("correlation"))),
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Configure upload limits. Keep the limit explicit and overrideable so
    ## public deployments do not accept multi-GB anonymous uploads by default.
    max_upload_mb <- suppressWarnings(as.numeric(Sys.getenv("QPROMS_MAX_UPLOAD_MB", "500")))
    if (is.na(max_upload_mb) || max_upload_mb <= 0) {
      max_upload_mb <- 500
    }
    options(
      shiny.maxRequestSize = max_upload_mb * 1024^2,
      qproms.maxUploadSizeMb = max_upload_mb
    )
    ## Load modules server
    home$server("home", r6 = object, main_session = session)
    upload$server("upload", r6 = object, main_session = session)
    preprocessing$server("preprocessing", r6 = object)
    pca$server("pca", r6 = object)
    correlation$server("correlation", r6 = object)
    rank$server("rank", r6 = object)
    network$server("network", r6 = object, main_session = session)
    ora$server("ora", r6 = object, main_session = session)
    gsea$server("gsea", r6 = object, main_session = session)
    download$server("download", r6 = object)
    settings$server("settings", r6 = object, main_session = session)
    help$server("help", r6 = object, main_session = session)
  })
}
