box::use(
  shiny[moduleServer, NS, actionButton, br, selectInput, icon, div, numericInput, observe, updateSelectInput, observeEvent, req, isolate, reactive],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, tooltip, input_switch, accordion, accordion_panel, input_task_button],
  gargoyle[watch, trigger, init],
  reactable[reactableOutput, renderReactable, getReactableState],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  plotly[plotlyOutput, renderPlotly],
  echarts4r[echarts4rOutput, renderEcharts4r],
  dplyr[pull, `%>%`]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        title = "Plots",
        full_screen = TRUE, 
        nav_panel(
          title = "Volcano",
          trelliscopeOutput(ns("volcano_plot"), style = "height: 100%")
        ), 
        nav_panel(
          title = "Profile",
          trelliscopeOutput(ns("profile_plot_uni"), style = "height: 100%")
        )
      ),
      navset_card_underline(
        title = "Table",
        full_screen = TRUE, 
        nav_panel(
          "Results",
          reactableOutput(ns("table_uni"))
        )
      )
    ),
    sidebar = sidebar(
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("define"),
          selectInput(
            inputId = ns("contrast_input"),
            label = "Contrasts",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("test_input"),
            label = "Test type",
            choices = c(
              "Welch's T-test" = "welch",
              "Student's T-test" = "student",
              "limma"
              # "Wilcoxon test" = "wilcox" # remove beacuse not work properly.
            ), 
            selected = "welch" 
          ),
          input_switch(
            id = ns("paider_input"),
            label = "Paired",
            value = FALSE
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          numericInput(
            inputId = ns("fc_input"),
            label = "Fold change",
            value = 1,
            min = 0,
            step = 0.5
          ),
          numericInput(
            inputId = ns("alpha_input"),
            label = "Alpha",
            value = 0.05,
            min = 0.01,
            max = 0.05,
            step = 0.01
          ),
          selectInput(
            inputId = ns("truncation_input"),
            label = "Truncation",
            choices = c(
              "Benjamini & Hochberg" = "BH",
              "Bonferroni" = "bonferroni",
              "Holm (1979)" = "holm",
              "Hochberg (1988)" = "hochberg",
              "Hommel (1988)" = "hommel",
              "Benjamini & Yekutieli" = "BY",
              "None" = "none"),
            selected = "BH"
          )
        ),
        accordion_panel(
          title = "Visual Parameters",
          id = ns("v_params"),
          input_switch(
            id = ns("same_y_input"),
            label = "Share same Y axis",
            value = TRUE
          ),
          input_switch(
            id = ns("same_x_input"),
            label = "Share same X axis",
            value = FALSE
          )
        )
      ),
      input_task_button(
        id = ns("update"),
        label = "UPDATE"
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("stat")
    
    observe({
      watch("genes")
      updateSelectInput(inputId = "contrast_input", choices = r6$all_test_combination)
    })
    
    observeEvent(input$update ,{

      req(input$contrast_input)
      
      r6$univariate_test_type <- input$test_input
      r6$univariate_paired <- input$paider_input
      r6$fold_change <- as.double(input$fc_input)
      r6$univariate_alpha <- as.double(input$alpha_input)
      r6$univariate_p_adj_method <- input$truncation_input
      r6$contrasts <- input$contrast_input
      
      r6$stat_uni_test(
        test = r6$contrasts,
        fc = r6$fold_change,
        alpha = r6$univariate_alpha,
        p_adj_method = r6$univariate_p_adj_method,
        paired_test = r6$univariate_paired,
        test_type = r6$univariate_test_type
      )
      trigger("plot", "stat")
    })
    
    gene_selected <- reactive(getReactableState("table_uni", "selected"))
    
    output$volcano_plot <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$stat_table)) {
        table <- r6$print_stat_table()
        highlights <- table[gene_selected(),] %>% 
          pull(gene_names)
        r6$plot_volcano(
          r6$contrasts,
          highlights,
          isolate(input$same_x_input),
          isolate(input$same_y_input)
        )
      }
    })
    
    output$profile_plot_uni <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$stat_table)) {
        table <- r6$print_stat_table()
        highlights <- table[gene_selected(),] %>% 
          pull(gene_names)
        r6$plot_stat_profile(tests = r6$contrasts, genes = highlights)
      }
    })
    
    output$table_uni <- renderReactable({
      watch("plot")
      r6$reactable_interactive(r6$print_stat_table())
    })

  })
}
