box::use(
  shiny[moduleServer, NS, conditionalPanel, actionButton, br, selectInput, icon, div, numericInput, observe, updateSelectInput, observeEvent, req, isolate, reactive],
  bslib[page_sidebar, layout_columns, navset_card_underline, nav_panel, sidebar, tooltip, input_switch, accordion, accordion_panel, input_task_button],
  gargoyle[watch, trigger],
  reactable[reactableOutput, renderReactable, getReactableState],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  plotly[plotlyOutput, renderPlotly],
  echarts4r[echarts4rOutput, renderEcharts4r],
  dplyr[pull, arrange, desc, `%>%`, across, ends_with, mutate, where]
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
          title = div(
            conditionalPanel(condition = "input.strategy == 'univariate'", ns = ns, "Volcano"),
            conditionalPanel(condition = "input.strategy == 'multivariate'", ns = ns, "Heatmap")
          ),
          value = "Main",
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            trelliscopeOutput(ns("volcano_plot"), style = "height: 100%")
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            plotlyOutput(ns("heatmap_plot"), height = "700px")
          )
        ), 
        nav_panel(
          "Profile",
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            trelliscopeOutput(ns("profile_plot_uni"), style = "height: 100%")
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            conditionalPanel(
              condition = "input.profile_type == 'protein'",
              ns = ns,
              echarts4rOutput(ns("profile_protein_plot"), height = "700px")
            ),
            conditionalPanel(
              condition = "input.profile_type == 'cluster'",
              ns = ns,
              trelliscopeOutput(ns("plot_cluster_profile"), style = "height: 100%")
            )
          )
        )
      ),
      navset_card_underline(
        title = "Table",
        full_screen = TRUE, 
        nav_panel(
          "Results",
          conditionalPanel(condition = "input.strategy == 'univariate'",
                           ns = ns,
                           reactableOutput(ns("table_uni"))), 
          conditionalPanel(condition = "input.strategy == 'multivariate'", 
                           ns = ns, 
                           reactableOutput(ns("table_anova")))
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
            inputId = ns("strategy"),
            label = "Strategy",
            choices = c("Univariate" = "univariate", "Multivariate" = "multivariate"),
            selected = "univariate"
          ),
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            selectInput(
              inputId = ns("contrast_input"),
              label = "Contrast",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            numericInput(
              inputId = ns("n_cluster_input"),
              label = "N° of clusters",
              value = 2,
              min = 0,
              step = 1
            ),
            selectInput(
              inputId = ns("profile_type"),
              label = "Profile plot",
              choices = c("Protein" = "protein", "Cluster" = "cluster"),
              selected = "protein"
            )
          )
        ),
        accordion_panel(
          title = "Parameters",
          id = ns("params"),
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
            selectInput(
              inputId = ns("test_input"),
              label = "Test type",
              choices = c("Welch's T-test" = "welch", "Student's T-test" = "student", "limma", "Wilcoxon test" = "wilcox"),
              selected = "welch" 
            ),
            input_switch(
              id = ns("paider_input"),
              label = "Paired",
              value = FALSE
            ),
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
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            input_switch(
              id = ns("zscore_input"),
              label = "Z-score",
              value = TRUE
            ),
            selectInput(
              inputId = ns("clust_method"),
              label = "Hierarchical Clustering method",
              choices = c("complete", "single", "average", "ward.D2", "mcquitty"),
              selected = "complete"
            ),
            numericInput(
              inputId = ns("alpha_input_milti"),
              label = "Alpha",
              value = 0.05,
              min = 0.01,
              max = 0.05,
              step = 0.01
            ),
            selectInput(
              inputId = ns("truncation_input_milti"),
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
          )
        ),
        accordion_panel(
          title = "Visual Parameters",
          id = ns("v_params"),
          conditionalPanel(
            condition = "input.strategy == 'univariate'",
            ns = ns,
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
          ),
          conditionalPanel(
            condition = "input.strategy == 'multivariate'",
            ns = ns,
            input_switch(
              id = ns("order_by_expdesing"),
              label = "Order by expdesing",
              value = FALSE
            )
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
    
    observe({
      watch("genes")
      updateSelectInput(inputId = "contrast_input", choices = r6$all_test_combination)
    })
    
    observeEvent(input$update ,{

      if(input$strategy == "univariate") {
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
      } else {
        r6$anova_alpha <- input$alpha_input_milti
        r6$anova_p_adj_method <- input$truncation_input_milti
        r6$z_score <- input$zscore_input
        r6$anova_clust_method <- input$clust_method
        r6$anova_manual_order <- input$order_by_expdesing
        r6$clusters_number <- input$n_cluster_input
        
        r6$stat_anova(
          alpha = r6$anova_alpha,
          p_adj_method = r6$anova_p_adj_method
        )
        
        output$heatmap_plot <- renderPlotly({
          r6$plot_heatmap(
            z_score = r6$z_score,
            n_cluster = r6$clusters_number,
            clustering_method = r6$anova_clust_method,
            order_by_expdesing = r6$anova_manual_order
          )
        })
      }
      trigger("plot")
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
    
    output$table_anova <- renderReactable({
      watch("plot")
      r6$reactable_interactive(r6$print_anova_table())
    })
    
    gene_selected_anova <- reactive(getReactableState("table_anova", "selected"))
    
    output$profile_protein_plot <- renderEcharts4r({
      watch("plot")
      if(!is.null(r6$anova_table)) {
        table <- r6$print_anova_table()
          highlights <- table[gene_selected_anova(),] %>% 
            pull(gene_names)
          r6$plot_protein_profile(highlights) 
      }
    })
    output$plot_cluster_profile <- renderTrelliscope({
      watch("plot")
      if(!is.null(r6$anova_table)) {
        r6$plot_cluster_profile()
      }
    })

  })
}
