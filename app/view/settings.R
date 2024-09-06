box::use(
  shiny[moduleServer, NS, radioButtons, actionButton, hr, updateSliderInput, updateRadioButtons, h3, h4, br, div, observeEvent, req, sliderInput, strong, observe],
  bslib[page_fillable, layout_columns, card, card_header, card_body, accordion, accordion_panel, accordion_panel_close, accordion_panel_open, nav_select, tooltip],
  esquisse[palettePicker, updatePalettePicker],
  viridis[viridis],
  purrr[map, set_names],
  dplyr[`%>%`, filter, select],
  gargoyle[init, watch, trigger],
  shinyalert[shinyalert],
)

## metterli dentro uno script utility functions
names <- c("A", "B", "C", "D", "E", "F", "G", "H")
palette_choices <- map(
  names, ~ viridis(n = 10, direction = -1, end = 0.90, begin = 0.10, option = .x)
) %>% set_names(names)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, -1, 10, -1, 12),
      row_heights = c(1,10,1),
      div(),
      card(
        card_header(h4(class = "text-center", "Settings")),
        card_body(
          layout_columns(
            col_widths = c(-2, 8, -2, -2, 6, 2, -2, 12, -4, 4, -4),
            gap = "2rem",
            row_heights = c(1, 1, 1, 1),
            palettePicker(
              inputId = ns("palette"),
              label = "Color Palettes",
              choices = palette_choices,
              selected = "D"
            ),
            sliderInput(
              inputId = ns("text_sixe"),
              label = "Plots text size",
              min = 4,
              max = 36,
              value = 16,
              step = 1,
              width = "100%"
            ),
            radioButtons(
              inputId = ns("plot_format"),
              label = "Plot extension",
              choices = c("svg" = "svg", "png" = "canvas"),
              selected = "svg"
            ),
            div(),
            actionButton(
              inputId = ns("update"),
              label = "UPDATE",
              class = "bg-primary",
              width = "100%"
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
      watch("session")
      updatePalettePicker(inputId = "palette", choices = palette_choices, selected = r6$palette)
      updateSliderInput(inputId = "text_sixe", value = r6$plot_font_size)
      updateRadioButtons(inputId = "plot_format", selected = r6$plot_format)
    })
    
    
    observeEvent(input$update, {
      r6$plot_format <- input$plot_format
      r6$palette <- input$palette
      r6$plot_font_size <- input$text_sixe
      if(!is.null(r6$expdesign)) {
        r6$define_colors()
        trigger("plot")
      }
      shinyalert(
        title = "Setting Updated!",
        text = "",
        size = "s",
        closeOnClickOutside = TRUE,
        type = "success",
        showConfirmButton = FALSE,
        timer = 1500
      )
      
    })

  })
}
