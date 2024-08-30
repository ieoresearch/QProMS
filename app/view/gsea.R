box::use(
  shiny[moduleServer, NS, selectInput, br, sliderInput, actionButton, isolate, icon, observe, updateSelectInput, updateSelectizeInput, reactive, observeEvent, numericInput, conditionalPanel, selectizeInput],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, input_switch, tooltip, input_task_button],
  gargoyle[watch, trigger],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  reactable[reactableOutput, renderReactable, getReactableState],
  dplyr[pull, `%>%`],
  purrr[map]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_columns(
      navset_card_underline(
        full_screen = TRUE, 
        nav_panel(
          "BarPlot",
          trelliscopeOutput(ns("bar_plot"), style = "height: 100%")
        ),
        nav_panel(
          "Table",
          reactableOutput(ns("table"))
        )
      )
    ),
    sidebar = sidebar(
      input_task_button(
        id = ns("update"),
        label = "UPDATE"
      ),
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Inputs",
          id = ns("inputs"),
          selectInput(
            inputId = ns("rank_with"),
            label = "Rank with",
            choices = c("Fold change" = "fc", "Intensity" = "intensity"),
            selected = "fc", 
            width = "auto"
          ),
          conditionalPanel(
            condition = "input.rank_with == 'fc'",
            ns = ns,
            selectInput(
              inputId = ns("volcano_input"),
              label = "Contrast",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.rank_with == 'intensity'",
            ns = ns,
            input_switch(
              id = ns("by_cond_input"),
              label = tooltip(
                trigger = list(
                  "Merge Condition",
                  icon("info-circle")
                ),
                "If TRUE, use the Intensity mean of each condition."
              ),
              value = TRUE
            ),
            selectInput(
              inputId = ns("target"),
              label = "Genes from",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = "auto"
            )
          ),
          selectInput(
            inputId = ns("ontology_input"),
            label = "Ontology",
            choices = c(
              "Biological Processes" = "BP",
              "Molecular Function" = "MF",
              "Cellular Components" = "CC"
            ),
            selected = "BP"
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          sliderInput(
            inputId = ns("simplify_thr"),
            label = "Simplify threshold",
            min = 0.1,
            max = 1,
            value = 1,
            step = 0.1
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
              "BH (Default)" = "BH",
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
          selectInput(
            inputId = ns("arrenged"),
            label = "X axis",
            choices = c(
              "NES" = "NES",
              "p.value" = "pvalue",
              "p.adjust" = "p.adjust",
              "Set Size" = "setSize"
            ), 
            selected = "NES"
          ),
          numericInput(
            inputId = ns("show_category"),
            label = "Show Category",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          )
        )
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
          ch <- unique(r6$expdesign$condition)
          updateSelectInput(inputId = "target", choices = ch, selected = ch[1])
        } else {
          updateSelectInput(inputId = "target", choices = r6$expdesign$label, selected = r6$expdesign$label[1])
        }
      }
    })
    
    observe({
      watch("stat")
      updateSelectInput(inputId = "volcano_input", choices = r6$contrasts, selected = r6$contrasts[1])
    })

    observeEvent(input$update ,{
      
      r6$go_gsea_rank_with <- input$rank_with
      r6$go_gsea_alpha <- input$alpha_input
      r6$go_gsea_p_adj_method <- input$truncation_input
      r6$go_gsea_term <- input$ontology_input
      r6$go_gsea_top_n <- input$show_category
      r6$go_gsea_simplify_thr <- input$simplify_thr
      r6$go_gsea_plot_arrenge <- input$arrenged
     
      if(r6$go_gsea_rank_with == "fc") {
        r6$go_gsea_focus <- input$volcano_input
        r6$go_gsea_by_cond <- FALSE
      }

      if(r6$go_gsea_rank_with == "intensity") {
        r6$go_gsea_focus <- input$target
        r6$go_gsea_by_cond <- input$by_cond_input
      }

      r6$go_gsea(
        test = r6$go_gsea_focus,
        rank_type = r6$go_gsea_rank_with,
        by_condition = r6$go_gsea_by_cond,
        ontology = r6$go_gsea_term,
        simplify_thr = r6$go_gsea_simplify_thr,
        alpha = r6$go_gsea_alpha,
        p_adj_method = r6$go_gsea_p_adj_method
      )
      r6$print_gsea_table(r6$go_gsea_plot_arrenge)
      trigger("plot")
    })

    output$bar_plot <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$gsea_result_list)) {
        r6$plot_gsea(r6$go_gsea_focus, r6$go_gsea_plot_arrenge, r6$go_gsea_top_n)
      }
    })

    output$table <- renderReactable({
      watch("plot")
      r6$reactable_functional_analysis(r6$gsea_table)
    })
    
  })
}
