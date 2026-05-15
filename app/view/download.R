box::use(
  shiny[moduleServer, observe, downloadButton, fluidPage, p, updateCheckboxGroupInput, span, uiOutput, checkboxInput, updateSelectInput, downloadHandler, NS, conditionalPanel, withProgress, incProgress, radioButtons, selectInput, actionButton, hr, h3, h4, br, div, observeEvent, req, sliderInput, checkboxGroupInput, isolate],
  bslib[page_fillable, layout_columns, card, card_header, card_body, accordion, accordion_panel, nav_select, tooltip],
  gargoyle[init, watch, trigger],
  quarto[quarto_render],
  purrr[set_names, map],
  shinyalert[shinyalert],
  dplyr[`%>%`],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    layout_columns(
      col_widths = c(12, 12, 12),
      gap = "1rem",
      card(
        card_header(
          h3("📥 Export results")
        ),
        card_body(
          p(
            class = "text-muted",
            "Download processed tables, generate a comprehensive analysis report, ",
            "or save the current analysis session to continue later."
          )
        )
      ),
      card(
        style = "overflow: visible;",
        card_header(
          h4("📊 Export result tables")
        ),
        card_body(
          style = "overflow: visible;",
          layout_columns(
            col_widths = c(6, 3, 3),
            selectInput(
              ns("select_table"),
              "Table",
              choices = list(
                "Preprocessing" = c("Filtered", "Normalized", "Imputed"),
                "Statistical analysis" = c("Ranked", "Volcano"),
                "Visualization data" = c("Heatmap", "Nodes", "Edges"),
                "Functional enrichment" = c("ORA", "GSEA")
              ),
              width = "100%"
            ),
            selectInput(
              ns("table_extension"),
              "File format",
              choices = c(".xlsx", ".csv", ".tsv"),
              selected = ".xlsx",
              width = "100%"
            ),
            div(
              style = "margin-top: 2.3rem;",
              checkboxInput(
                ns("include_metadata"),
                "Include sample metadata",
                value = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.include_metadata === true &&
                          ['Filtered','Normalized','Imputed','Volcano','Heatmap']
                          .includes(input.select_table)",
              ns = ns,
              selectInput(
                ns("add_metadata"),
                "Metadata columns",
                choices = NULL,
                multiple = TRUE,
                width = "100%"
              )
            )
          ),
          downloadButton(
            ns("download_table"),
            label = "⬇ Download table",
            class = "btn-primary w-100"
          )
        )
      ),
      card(
        card_header(
          h4("📄 Generate analysis report")
        ),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            radioButtons(
              ns("report_preset"),
              "Report preset",
              choices = c("Full report" = "full", "Custom" = "custom"),
              selected = "full"
            ),
            conditionalPanel(
              condition = "input.report_preset === 'custom'",
              ns = ns,
              checkboxGroupInput(
                ns("report_section"),
                "Included sections",
                inline = TRUE,
                choices = list(
                  "Preprocessing" = c("Preprocessing"),
                  "Exploratory analysis" = c("PCA", "Correlation"),
                  "Differential analysis" = c("Rank", "Volcano"),
                  "Visualization" = c("Heatmap", "Network"),
                  "Functional analysis" = c("ORA", "GSEA")
                )
              )
            )
          ),
          div(
            class = "mt-3",
            downloadButton(
              ns("download_report"),
              label = "📄 Generate report (.html)",
              class = "btn-primary w-100"
            ),
            span(
              class = "text-muted small",
              "Report generation may take up to some minutes."
            )
          )
        )
      ),
      card(
        card_header(
          h4("💾 Save analysis session")
        ),
        card_body(
          p(
            "Save the entire analysis state, including parameters and results, ",
            "to resume your work later."
          ),
          p(
            class = "text-muted",
            "The session will be saved as a .rds file and can be reloaded ",
            "from the Home page."
          ),
          downloadButton(
            ns("download_params"),
            label = "💾 Save session (.rds)",
            class = "btn-outline-primary w-100"
          )
        )
      )
    )
  )
}


#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    get_table_from_r6 <- function(table_type) {
      switch(table_type,
             "Filtered"   = r6$filtered_data,    
             "Normalized" = r6$normalized_data,
             "Imputed"    = r6$imputed_data,
             "Ranked"     = r6$rank_data,
             "Volcano"    = r6$stat_table,
             "Heatmap"    = r6$anova_table,
             "Nodes"      = r6$nodes_table,
             "Edges"      = r6$edges_table,
             "ORA"        = r6$ora_table,        
             "GSEA"       = r6$gsea_table,
             NULL
      )
    }
    
    observe({
      watch("genes")
      updateSelectInput(
        inputId = "add_metadata",
        choices = colnames(r6$raw_data_unique),
        selected = NULL
      )
      if (!r6$with_statistics) {
        updateCheckboxGroupInput(
          inputId = "report_section",
          choices = c("Preprocessing", "PCA", "Correlation", "Rank", "Network", "ORA", "GSEA"),
          selected = c("Preprocessing", "PCA", "Correlation", "Rank", "Network", "ORA", "GSEA")
        )
      } else {
        updateCheckboxGroupInput(
          inputId = "report_section",
          choices = c(
            "Preprocessing", "PCA", "Correlation", "Rank",
            "Volcano", "Heatmap", "Network", "ORA", "GSEA"
          ),
          selected = c(
            "Preprocessing", "PCA", "Correlation", "Rank",
            "Volcano", "Heatmap", "Network", "ORA", "GSEA"
          )
        )
      }
    })
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste0(input$select_table, "_table_", Sys.Date(), input$table_extension)
      },
      content = function(file) {
        
        if (is.null(r6$data)) {
          shinyalert(
            title = "No data loaded",
            text  = "Please load your data before downloading a table.",
            type  = "warning"
          )
          return(invisible(NULL))
        }
        
        selected_table <- input$select_table
        table_data     <- get_table_from_r6(selected_table)
        
        if (is.null(table_data)) {
          shinyalert(
            title = "Table not available",
            text  = paste0(
              "The '", selected_table, "' table has not been generated yet. ",
              "Please complete the corresponding analysis step first."
            ),
            type  = "warning"
          )
          return(invisible(NULL))
        }
        
        # --- Tutto ok: procedi con il download ---
        r6$download_table(
          handler_file    = file,
          table_type      = selected_table,
          table_extension = input$table_extension,
          extra_columns   = input$add_metadata
        )
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("QProMS_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        if (!is.null(r6$data)) {
          withProgress(message = "The report is rendering", value = 0, {
            incProgress(1/5, message = "Loading Parameters")
            Sys.sleep(1)
            params <- c("Preprocessing", "PCA", "Correlation", "Rank",
                        "Volcano", "Heatmap", "Network", "ORA", "GSEA")
            incProgress(1/5, message = "Save session")
            Sys.sleep(1)
            r6$download_parameters(handler_file = "app/logic/QProMS_session_internal.rds", r6class = r6)
            incProgress(1/5, message = "Use only selected Section")
            Sys.sleep(1)
            if (isolate(input$report_preset) == "custom") {
              param_list <- map(params, ~ .x %in% isolate(input$report_section)) %>%
                set_names(params)
            } else {
              param_list <- map(params, ~ .x %in% params) %>%
                set_names(params)
            }
            print(param_list)
            incProgress(1/5, message = "Render Report", detail = "This operation can take some time..")
            quarto_render(
              "app/logic/Report_QProMS.qmd",
              execute_params = param_list,
              quiet = FALSE
            )
            incProgress(1/5, message = "Finish!")
            file.copy("app/logic/Report_QProMS.html", file)
            file.remove("app/logic/Report_QProMS.html")
          })
        }
      }
    )
    
    output$download_params <- downloadHandler(
      filename = function() {
        paste0("QProMS_analysis_", Sys.Date(), ".rds")
      },
      content = function(file) {
        if (!is.null(r6$data)) {
          r6$new_session <- FALSE
          r6$download_parameters(handler_file = file, r6class = r6)
        }
      }
    )
    
  })
}