box::use(
  shiny[moduleServer, NS, selectInput, sliderInput, isolate, numericInput, updateSelectInput, updateSliderInput, br, actionButton, observeEvent, icon, observe, req, conditionalPanel, reactiveVal, renderPlot, plotOutput],
  bslib[page_sidebar, input_task_button, layout_columns, navset_card_underline, nav_panel, update_switch, sidebar, accordion, accordion_panel, input_switch, accordion_panel_remove, tooltip, nav_hide, nav_show],
  echarts4r[echarts4rOutput, renderEcharts4r],
  gargoyle[watch, trigger, init],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  reactable[reactableOutput, renderReactable],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
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
          "Coverage",
          echarts4rOutput(ns("valid_values_plot"))
        ),
        nav_panel(
          "Intersection",
          plotOutput(ns("upset_plot"))
        ),
        nav_panel(
          title = tooltip(
            trigger = list(
              "CV",
              icon("info-circle")
            ),
            "Coefficient of Variation Plot."
          ),
          echarts4rOutput(ns("cv_plot"))
        ),
        nav_panel(
          "Missing Data",
          echarts4rOutput(ns("missing_data_counts_plot"))
        ),
        nav_panel(
          title = "Imputed",
          value = "Distribution",
          trelliscopeOutput(ns("missval_distribution_plot"), style = "height: 100%")
        ),
        nav_panel(
          "Table",
          reactableOutput(ns("imputed_table"))
        )
      )
    ),
    sidebar = sidebar(
      input_task_button(
        id = ns("update"),
        label = "PROCESS",
        class = "bg-primary"
      ),
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Analysis Level",
          id = ns("analysis_level_panel"),
          selectInput(
            inputId = ns("analysis_level"),
            label = "Feature Level",
            choices = c("Protein" = "protein", "Peptide" = "peptide"),
            selected = "protein"
          )
        ),
        accordion_panel(
          title = "Subset by Valid Values",
          id = ns("subset"),
          selectInput(
            inputId = ns("valid_values_input"),
            label = tooltip(
              trigger = list(
                "Method",
                icon("info-circle")
              ),
              "Filter missing data according to the selected valid values grouping method."
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
              "Amount of valid valued in the group."
            ),
            min = 0,
            max = 100,
            value = 100,
            step = 5
          )
        ),
        accordion_panel(
          title = "Subset by Peptides",
          id = ns("peptides"),
          # value = "test",
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteingGroups.txt files."
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteingGroups.txt files."
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteingGroups.txt files."
            ),
            value = TRUE
          )
        ),
        accordion_panel(
          title = "Normalization",
          id = ns("normalization"),
          selectInput(
            inputId = ns("normalization_input"),
            label = tooltip(
              trigger = list(
                "Normalization",
                icon("info-circle")
              ),
              "VSN normalization: Applies a variance-stabilizing transformation to make intensity values comparable across samples"
            ),
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
            choices = c("Mixed" = "mixed", "Perseus" = "perseus", "missForest" = "missforest", "None" = "none"),
            selected = "mixed"
          ),
          conditionalPanel(
            condition = "input.imputation_input == 'mixed' || input.imputation_input == 'perseus'",
            ns = ns,
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
          ),
          conditionalPanel(
            condition = "input.imputation_input == 'mixed'",
            ns = ns,
            sliderInput(
              inputId = ns("mar_mnar_thr"),
              label = "MAR/MNAR threshold",
              min = 0.25,
              max = 1,
              value = 0.75,
              step = 0.05
            )
          ),
          conditionalPanel(
            condition = "input.imputation_input == 'missforest'",
            ns = ns,
            numericInput(
              inputId = ns("maxiter"),
              label = tooltip(
                trigger = list(
                  "Number of iterations",
                  icon("info-circle")
                ),
                "Maximum number of iterations unless the stopping criterion is met earlier (maxiter)."
              ),
              min = 1,
              max = 20,
              value = 1,
              step = 1
            ),
            numericInput(
              inputId = ns("ntree"),
              label = tooltip(
                trigger = list(
                  "Number of trees",
                  icon("info-circle")
                ),
                "Number of trees to grow in each per-variable forest (ntree)."
              ),
              min = 10,
              max = 100,
              value = 10,
              step = 10
            ),
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    only_first_time_trigger <- reactiveVal(TRUE)
    
    observe({
      watch("session")
      updateSelectInput(inputId = "valid_values_input", selected = r6$valid_val_filter)
      updateSelectInput(inputId = "valid_values_slider", selected = r6$valid_val_thr*100)
      updateSelectInput(inputId = "peptides_input", selected = r6$pep_filter)
      updateSelectInput(inputId = "normalization_input", selected = r6$norm_methods)
      updateSelectInput(inputId = "imputation_input", selected = r6$imp_methods)
      updateSliderInput(inputId = "peptides_slider", value = r6$pep_thr)
      updateSliderInput(inputId = "shift_slider", value = r6$imp_shift)
      updateSliderInput(inputId = "scale_slider", value = r6$imp_scale)
      update_switch(id = "rev", value = r6$rev)
      update_switch(id = "cont", value = r6$cont)
      update_switch(id = "oibs", value = r6$oibs)
    })
    
    observe({
      watch("genes")
      output$protein_counts_plot <- renderEcharts4r({
        if (!is.null(r6$filtered_data) && only_first_time_trigger()) {
          r6$plot_empty_message("Press Process Button")
        } else if (!is.null(r6$filtered_data)) {
          r6$plot_protein_counts()
        }
      })
      if(!is.null(r6$input_type)) {
        if(!r6$input_type %in% c("MaxQuant", "ProteomeDiscoverer")) {
          accordion_panel_remove("accordion", "Subset by Peptides", session = session)
        }
        if(r6$input_type != "MaxQuant") {
          accordion_panel_remove("accordion", "Remove Contaminants", session = session)
        }
      }
    })
  
    observeEvent(input$update, {
      only_first_time_trigger(FALSE)
      r6$analysis_level <- input$analysis_level
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
      r6$mar_mnar_thresh <- input$mar_mnar_thr
      r6$missforest_ntree <- input$ntree
      r6$missforest_niter <- input$maxiter
      
      if(!is.null(r6$data)) {
        r6$shiny_wrap_workflow()
        trigger("genes")
        output$protein_counts_plot <- renderEcharts4r({
          r6$plot_protein_counts() 
        })
        output$distribution_plot <- renderEcharts4r({
          r6$plot_distribution() 
        })
        output$valid_values_plot <- renderEcharts4r({
          r6$plot_protein_coverage() 
        })
        output$upset_plot <- renderPlot({
          r6$plot_protein_coverage_intersections() 
        })
        output$cv_plot <- renderEcharts4r({
          r6$plot_cv() 
        })
        output$missval_distribution_plot <- renderTrelliscope({
          r6$plot_missval_distribution() 
        })
        output$missing_data_counts_plot <- renderEcharts4r({
          r6$plot_missing_data()
        })
        output$post_imputation_plot <- renderEcharts4r({
          if(r6$imp_methods == "none"){
            r6$plot_imputation(data = r6$normalized_data, imp_visualization = FALSE) 
          }else{
            r6$plot_imputation(data = r6$imputed_data, imp_visualization = TRUE) 
          }
        })
        output$imputed_table <- renderReactable({
          if(r6$imp_methods == "none"){
            r6$print_table(r6$normalized_data)
          }else{
            r6$print_table(r6$imputed_data)
          }
        })
      }
    })
  })
}
