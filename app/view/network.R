box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, icon, observe, updateSelectInput, reactive, observeEvent, conditionalPanel],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, input_switch, tooltip],
  gargoyle[watch, trigger],
  echarts4r[echarts4rOutput, renderEcharts4r],
  reactable[reactableOutput, renderReactable, getReactableState],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        title = "Network",
        full_screen = TRUE, 
        nav_panel(
          "Plot",
          echarts4rOutput(ns("network_plot"))
        )
      ),
      navset_card_underline(
        title = "Tables",
        full_screen = TRUE, 
        nav_panel(
          "Nodes",
          "node"
        ),
        nav_panel(
          "Edges",
          "edge"
        )
      )
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("inputs"),
          selectInput(
            inputId = ns("strategy"),
            label = "Inputs From",
            choices = c(
              "Rank" = "top_rank",
              "Statistics" = "univariate",
              "Heatmap" = "multivariate"
            ), 
            selected = "univariate"
          ),
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            selectInput(
              inputId = ns("test_uni_input"),
              label = "Contrasts",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              inputId = ns("ui_direction_input"),
              label = "Directions",
              choices = c("Up" = "up", "Down" = "down", "Both" = "both"),
              selected = "up"
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            selectInput(
              inputId = ns("clusters_input"),
              label = "Clusters",
              choices = NULL,
              multiple = TRUE
            )
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          selectInput(
            inputId = ns("db_source"),
            label = "Database",
            choices = c("String" = "string", "Corum" = "corum"),
            selected = "string", 
            multiple = TRUE
          ),
          sliderInput(
            inputId = ns("score_thr"),
            label = "Score threshold",
            min = 0,
            max = 0.9,
            value = 0.4,
            step = 0.1
          )
        ),
        accordion_panel(
          title = "Visual Parameters",
          id = ns("v_params"),
          selectInput(
            inputId = ns("layout"),
            label = NULL,
            choices = c("force", "circular"),
            selected = "force"
          ),
          input_switch(
            id = ns("isolate_nodes_input"),
            label = "Keep isolate nodes",
            value = FALSE
          ),
          input_switch(
            id = ns("names_input"),
            label = "Show names",
            value = TRUE
          ),
          input_switch(
            id = ns("keep_selected"),
            label = tooltip(
              trigger = list(
                "Subset",
                icon("info-circle")
              ),
              "If TRUE, display network with only selected nodes."
            ),
            value = FALSE
          )
        )
      ),
      actionButton(
        inputId = ns("update"),
        label = "UPDATE",
        class = "bg-primary"
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {

  })
}
