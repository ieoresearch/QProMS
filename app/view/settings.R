box::use(
  shiny[moduleServer, NS, radioButtons, actionButton, hr, h3, br, div, observeEvent, req],
  bslib[page_fluid, layout_columns],
  esquisse[palettePicker],
  viridis[viridis],
  purrr[map, set_names],
  dplyr[`%>%`, filter, select],
  gargoyle[init, watch, trigger],
)

## metterli dentro uno script utility functions
names <- c("A", "B", "C", "D", "E", "F", "G", "H")
palette_choices <- map(
  names, ~ viridis(n = 10, direction = -1, end = 0.90, begin = 0.10, option = .x)
) %>% set_names(names)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    title = "Settings", 
    h3("Visual Setting"),
    br(),
    layout_columns(
      palettePicker(
        inputId = ns("palette"),
        label = "Color Palettes",
        choices = palette_choices,
        selected = "D"
      ),
      radioButtons(
        inputId = ns("plot_format"),
        label = "Plot extension",
        inline = TRUE,
        choices = c("svg" = "svg", "png" = "canvas"),
        selected = "svg"
      )
    ),
    hr(),
    actionButton(
      inputId = ns("update"),
      label = "UPDATE",
      class = "bg-primary",
      width = "300px"
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$update, {
      r6$plot_format <- input$plot_format
      r6$palette <- input$palette
      if(!is.null(r6$expdesign)) {
        r6$define_colors()
        trigger("plot")
      }
    })

  })
}
