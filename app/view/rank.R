box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, icon, observe, updateSelectInput, reactive, observeEvent],
  bslib[page_sidebar, layout_columns, card, card_header, card_body, sidebar, accordion, accordion_panel, input_switch, tooltip],
  gargoyle[watch, trigger],
  echarts4r[echarts4rOutput, renderEcharts4r],
  reactable[reactableOutput, renderReactable, getReactableState],
  dplyr[`%>%`, pull]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Protein Rank Plot"),
        card_body(echarts4rOutput(ns("protein_rank_plot")))
      ),
      card(
        full_screen = TRUE,
        card_header("Table"),
        card_body(reactableOutput(ns("table")))
      ),
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("inputs"),
          input_switch(
            id = ns("by_cond_input"),
            label = tooltip(
              trigger = list(
                "Merge Condition",
                icon("info-circle")
              ),
              "If TRUE, use the Intensity mean of each condition."
            ),
            value = FALSE
          ),
          selectInput(
            inputId = ns("target"),
            label = "Genes from",
            choices = NULL,
            selected = NULL, 
            width = "auto"
          ),
          selectInput(
            inputId = ns("selections"),
            label = tooltip(
              trigger = list(
                "Selection",
                icon("info-circle")
              ),
              "The current selection is saved for network and functional analysis. If you change inputs press 'UPDATE' to validate the new selection."
            ),
            choices = c("From top" = "top", "From bottom" = "bot"),
            selected = "top", 
            width = "auto"
          ),
          sliderInput(
            inputId = ns("top_n_slider"),
            label = "n % of proteins",
            min = 1,
            max = 50,
            value = 10,
            step = 1
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

    observe({
      watch("genes")
      if(!is.null(r6$expdesign)) {
        if(input$by_cond_input){
          updateSelectInput(inputId = "target", choices = unique(r6$expdesign$condition))
        } else {
          updateSelectInput(inputId = "target", choices = r6$expdesign$label)
        }
      }
    })
    
    observeEvent(input$update ,{
      r6$protein_rank_target <- input$target
      r6$protein_rank_by_cond <- input$by_cond_input
      r6$protein_rank_selection <- input$selections
      r6$protein_rank_top_n <- as.numeric(input$top_n_slider) / 100
      if(!is.null(r6$data)) {
        r6$rank_protein(
          target = r6$protein_rank_target,
          by_condition = r6$protein_rank_by_cond,
          selection = r6$protein_rank_selection,
          n_perc = r6$protein_rank_top_n
        )
        trigger("plot")
      }
    })
    
    output$table <- renderReactable({
      watch("plot")
      r6$reactable_interactive(r6$print_rank_table())
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    output$protein_rank_plot <- renderEcharts4r({
      watch("plot")
      if(!is.null(r6$rank_data)) {
        highlights <- r6$rank_data[gene_selected(),] %>%
          pull(gene_names)
        r6$plot_protein_rank(highlights_names = highlights)
      }
    })
    
  })
}
