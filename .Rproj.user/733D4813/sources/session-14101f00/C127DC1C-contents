box::use(
  shiny[moduleServer, NS, selectInput, sliderInput, br, actionButton, observeEvent, icon, observe, req],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, input_switch, accordion_panel_remove, tooltip],
  echarts4r[echarts4rOutput, renderEcharts4r],
  gargoyle[watch, trigger],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  reactable[reactableOutput, renderReactable],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        title = "Subset",
        full_screen = TRUE, 
        nav_panel(
          "Counts",
          echarts4rOutput(ns("protein_counts_plot"))
        ),
        nav_panel(
          "Distribution",
          echarts4rOutput(ns("distribution_plot"))
        ),
        nav_panel(
          "Upset Plot",
          echarts4rOutput(ns("valid_values_plot"))
        ),
        nav_panel(
          "CV",
          echarts4rOutput(ns("cv_plot"))
        ),
        nav_panel(
          "Table",
          reactableOutput(ns("subset_table"))
        )
      ),
      navset_card_underline(
        title = "Missing Data",
        full_screen = TRUE, 
        nav_panel(
          "Counts",
          echarts4rOutput(ns("missing_data_counts_plot"))
        ),
        nav_panel(
          title = tooltip(
            trigger = list(
              "Distribution",
              icon("info-circle")
            ),
            "Multiple plot visualization."
          ),
          value = "Distribution",
          trelliscopeOutput(ns("missval_distribution_plot"), style = "height: 100%")
        ),
        # nav_panel(
        #   "Not Imputed",
        #   echarts4rOutput(ns("pre_imputation_plot"))
        # ),
        nav_panel(
          "Imputed",
          echarts4rOutput(ns("post_imputation_plot"))
        ),
        nav_panel(
          "Table",
          reactableOutput(ns("imputed_table"))
        )
      )
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Subset Missing Data",
          id = ns("subset"),
          selectInput(
            inputId = ns("valid_values_input"),
            label = tooltip(
              trigger = list(
                "Method",
                icon("info-circle")
              ),
              "This filter remove missing data base on the valid values grouping method selected."
            ),
            choices = c("In at least one group" = "alog", "In each group" = "each_grp", "In total" = "total"),
            selected = "alog"
          ),
          sliderInput(
            inputId = ns("valid_values_slider"),
            label = tooltip(
              trigger = list(
                "Percentage",
                icon("info-circle")
              ),
              "Select the percentage of valid values."
            ),
            min = 50,
            max = 100,
            value = 100,
            step = 5
          )
        ),
        accordion_panel(
          title = "Subset Peptides",
          id = ns("peptides"),
          selectInput(
            inputId = ns("peptides_input"),
            label = tooltip(
              trigger = list(
                "Column Type",
                icon("info-circle")
              ),
              "This filter applies only for MaxQuant proteingGroups.txt files."
            ),
            choices = c("Peptides" = "peptides", "Unique peptides" = "unique", "Razor peptides" = "razor"),
            selected = "peptides"
          ),
          sliderInput(
            inputId = ns("peptides_slider"),
            label = tooltip(
              trigger = list(
                "Minimum number",
                icon("info-circle")
              ),
              "This filter applies only for MaxQuant proteingGroups.txt files."
            ),
            min = 0,
            max = 10,
            value = 2,
            step = 1
          )
        ),
        accordion_panel(
          title = "Remove Contaminants",
          id = ns("contaminants"),
          input_switch(
            id = ns("rev"),
            label = tooltip(
              trigger = list(
                "Reverse",
                icon("info-circle")
              ),
              "If TRUE will be removed."
            ),
            value = TRUE
          ),
          input_switch(
            id = ns("cont"),
            label = tooltip(
              trigger = list(
                "Contaminant",
                icon("info-circle")
              ),
              "If TRUE will be removed."
            ),
            value = TRUE
          ),
          input_switch(
            id = ns("oibs"),
            label = tooltip(
              trigger = list(
                "Only identify by site",
                icon("info-circle")
              ),
              "If TRUE will be removed."
            ),
            value = TRUE
          )
        ),
        accordion_panel(
          title = "Normalization",
          id = ns("normalization"),
          selectInput(
            inputId = ns("normalization_input"),
            label = "Normalization",
            choices = c("None", "VSN"),
            selected = "None"
          )
        ),
        accordion_panel(
          title = "Imputation",
          id = ns("imputation"),
          selectInput(
            inputId = ns("imputation_input"),
            label = "Method",
            choices = c("Mixed" = "mixed", "Perseus" = "perseus", "None" = "none"),
            selected = "mixed"
          ),
          sliderInput(
            inputId = ns("shift_slider"),
            label = "Down shift",
            min = 1.6,
            max = 2,
            value = 1.8,
            step = 0.1
          ),
          sliderInput(
            inputId = ns("scale_slider"),
            label = "Scale",
            min = 0.1,
            max = 0.5,
            value = 0.3,
            step = 0.1
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
    
    observeEvent(input$update, {

      r6$valid_val_filter <- input$valid_values_input
      r6$valid_val_thr <- as.numeric(input$valid_values_slider) / 100
      r6$pep_filter <- input$peptides_input
      r6$pep_thr <- input$peptides_slider
      r6$rev <- input$rev
      r6$cont <- input$cont
      r6$oibs <- input$oibs
      r6$norm_methods <- input$normalization_input
      r6$imp_methods <- input$imputation_input
      r6$imp_shift <- input$shift_slider
      r6$imp_scale <- input$scale_slider
      
      if(!is.null(r6$data)) {
        r6$shiny_wrap_workflow()
        trigger("plot", "genes")
      }
    })
    output$protein_counts_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_protein_counts() 
    })
    output$distribution_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_distribution() 
    })
    output$valid_values_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_protein_coverage() 
    })
    output$cv_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_cv() 
    })
    output$missing_data_counts_plot <- renderEcharts4r({
      watch("plot")
      r6$plot_missing_data()
    })
    output$missval_distribution_plot <- renderTrelliscope({
      watch("plot")
      r6$plot_missval_distribution() 
    })
    # output$pre_imputation_plot <- renderEcharts4r({
    #   watch("plot")
    #   r6$plot_imputation(data = r6$normalized_data, imp_visualization = FALSE) 
    # })
    output$post_imputation_plot <- renderEcharts4r({
      watch("plot")
      if(r6$imp_methods == "none"){
        r6$plot_imputation(data = r6$normalized_data, imp_visualization = FALSE) 
      }else{
        r6$plot_imputation(data = r6$imputed_data, imp_visualization = TRUE) 
      }
    })
    output$subset_table <- renderReactable({
      watch("plot")
      r6$print_table(r6$normalized_data)
    })
    output$imputed_table <- renderReactable({
      watch("plot")
      if(r6$imp_methods == "none"){
        r6$print_table(r6$normalized_data)
      }else{
        r6$print_table(r6$imputed_data)
      }
    })
  })
}
