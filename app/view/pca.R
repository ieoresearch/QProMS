box::use(
  shiny[moduleServer, NS],
  bslib[page_fillable, layout_columns, navset_card_underline, nav_panel],
  echarts4r[echarts4rOutput, renderEcharts4r],
  gargoyle[watch],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      navset_card_underline(
        full_screen = TRUE, 
        nav_panel(
          "2D",
          echarts4rOutput(ns("pca_plot_2d"))
        )
      ),
      navset_card_underline(
        full_screen = TRUE, 
        nav_panel(
          "3D",
          echarts4rOutput(ns("pca_plot_3d"))
        )
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {

    output$pca_plot_2d <- renderEcharts4r({
      watch("plot")
      r6$plot_pca(view_3d = FALSE) 
    })
    
    output$pca_plot_3d <- renderEcharts4r({
      watch("plot")
      r6$plot_pca(view_3d = TRUE) 
    })

  })
}
