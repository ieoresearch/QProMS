box::use(
  shiny[moduleServer, observe, downloadButton, fluidPage, p, updateCheckboxGroupInput, span, uiOutput, checkboxInput, updateSelectInput, downloadHandler, NS, conditionalPanel, withProgress, incProgress, radioButtons, selectInput, actionButton, hr, h3, h4, br, div, observeEvent, req, sliderInput, checkboxGroupInput, isolate, showNotification, reactiveVal, renderUI],
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
              "Tables",
              choices = list(
                "Preprocessing" = c("Filtered", "Normalized", "Imputed"),
                "Statistical analysis" = c("Ranked", "Volcano"),
                "Visualization data" = c("Heatmap", "Nodes", "Edges"),
                "Functional enrichment" = c("ORA", "GSEA")
              ),
              multiple = TRUE,
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
                          Array.isArray(input.select_table) &&
                          input.select_table.some(x => ['Filtered','Normalized','Imputed','Volcano','Heatmap','Ranked'].includes(x))",
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
          uiOutput(ns("report_controls"))
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
    
    report_file <- reactiveVal(NULL)
    
    cleanup_generated_report <- function(path) {
      if (is.null(path) || !nzchar(path)) {
        return(invisible(NULL))
      }

      report_dir <- dirname(path)
      temp_root <- normalizePath(tempdir(), mustWork = TRUE)
      report_dir_norm <- normalizePath(report_dir, mustWork = FALSE)
      is_temp_child <- startsWith(report_dir_norm, paste0(temp_root, .Platform$file.sep))

      if (is_temp_child && dir.exists(report_dir_norm)) {
        unlink(report_dir_norm, recursive = TRUE, force = TRUE)
      } else if (file.exists(path)) {
        unlink(path, force = TRUE)
      }

      invisible(NULL)
    }

    reset_report_file <- function() {
      cleanup_generated_report(report_file())
      report_file(NULL)
    }

    session$onSessionEnded(function() {
      cleanup_generated_report(report_file())
    })

    output$report_controls <- renderUI({
      generated_report <- report_file()
      report_ready <- !is.null(generated_report) && file.exists(generated_report)
      
      if (!report_ready) {
        div(
          class = "mt-3",
          actionButton(
            session$ns("generate_report"),
            label = "📄 Generate report",
            class = "btn-primary w-100"
          ),
          span(
            class = "text-muted small",
            "Generate the report first. The download button will appear when the report is ready."
          )
        )
      } else {
        div(
          class = "mt-3",
          actionButton(
            session$ns("generate_report"),
            label = "📄 Generate report again",
            class = "btn-primary w-100"
          ),
          br(),
          br(),
          downloadButton(
            session$ns("download_report"),
            label = "⬇ Download generated report (.html)",
            class = "btn-outline-primary w-100"
          ),
          span(
            class = "text-muted small",
            "Report generated successfully. You can now download it."
          )
        )
      }
    })
    
    observeEvent(
      list(input$report_preset, input$report_section),
      {
        reset_report_file()
      },
      ignoreInit = TRUE
    )
    
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
    
    observeEvent(input$select_table, {
      if (length(input$select_table) > 1 && !identical(input$table_extension, ".xlsx")) {
        updateSelectInput(inputId = "table_extension", selected = ".xlsx")
        showNotification("Multiple tables are exported together as an .xlsx workbook.", type = "message")
      }
    }, ignoreNULL = TRUE)
    
    output$download_table <- downloadHandler(
      filename = function() {
        selected_tables <- isolate(input$select_table)
        if (is.null(selected_tables) || length(selected_tables) == 0) {
          return(paste0("QProMS_tables_", Sys.Date(), ".xlsx"))
        }
        if (length(selected_tables) == 1) {
          return(paste0(selected_tables[[1]], "_table_", Sys.Date(), input$table_extension))
        }
        paste0("QProMS_tables_", Sys.Date(), ".xlsx")
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
        
        selected_tables <- input$select_table
        if (is.null(selected_tables) || length(selected_tables) == 0) {
          shinyalert(
            title = "No tables selected",
            text  = "Please select at least one table to export.",
            type  = "warning"
          )
          return(invisible(NULL))
        }
        
        ok <- r6$download_tables(
          handler_file = file,
          table_types = selected_tables,
          table_extension = input$table_extension,
          extra_columns = input$add_metadata
        )
        
        if (identical(ok, "multiple_requires_xlsx")) {
          showNotification("Exporting multiple tables at once is available only as an .xlsx workbook.", type = "warning")
          return(invisible(NULL))
        }
        
        if (!isTRUE(ok)) {
          if (length(selected_tables) == 1 && is.null(get_table_from_r6(selected_tables[[1]]))) {
            shinyalert(
              title = "Table not available",
              text  = paste0(
                "The '", selected_tables[[1]], "' table has not been generated yet. ",
                "Please complete the corresponding analysis step first."
              ),
              type  = "warning"
            )
          } else {
            showNotification("No selected tables are available for export.", type = "warning")
          }
          return(invisible(NULL))
        }
      }
    )
    
    observeEvent(input$generate_report, {
      reset_report_file()
      
      if (is.null(r6$data)) {
        shinyalert(
          title = "No data loaded",
          text  = "Please load your data before generating a report.",
          type  = "warning"
        )
        return(invisible(NULL))
      }
      
      withProgress(message = "The report is rendering", value = 0, {
        incProgress(1/5, message = "Loading parameters")
        
        params <- c(
          "Preprocessing", "PCA", "Correlation", "Rank",
          "Volcano", "Heatmap", "Network", "ORA", "GSEA"
        )
        
        incProgress(1/5, message = "Saving session")
        
        app_root <- normalizePath(getwd(), mustWork = TRUE)
        report_qmd <- file.path(app_root, "app/logic/Report_QProMS.qmd")
        session_file <- tempfile("QProMS_session_", fileext = ".rds")
        on.exit(unlink(session_file, force = TRUE), add = TRUE)

        r6$download_parameters(
          handler_file = session_file,
          r6class = r6
        )
        
        incProgress(1/5, message = "Selecting report sections")
        
        if (isolate(input$report_preset) == "custom") {
          param_list <- map(params, ~ .x %in% isolate(input$report_section)) %>%
            set_names(params)
        } else {
          param_list <- map(params, ~ .x %in% params) %>%
            set_names(params)
        }
        
        param_list <- c(param_list, list(session_file = session_file))
        print(param_list)
        
        incProgress(
          1/5,
          message = "Rendering report",
          detail = "This operation can take some time."
        )
        
        render_dir <- tempfile("QProMS_report_")
        dir.create(render_dir)
        render_committed <- FALSE
        on.exit({
          if (!isTRUE(render_committed)) {
            unlink(render_dir, recursive = TRUE, force = TRUE)
          }
        }, add = TRUE)
        
        output_file <- paste0(
          "QProMS_report_",
          Sys.getpid(),
          "_",
          as.integer(Sys.time()),
          ".html"
        )
        
        quarto_render(
          input = report_qmd,
          output_file = output_file,
          output_dir = render_dir,
          execute_params = param_list,
          quiet = FALSE,
          execute_dir = app_root
        )
        
        incProgress(1/5, message = "Finalizing report")
        
        generated_report <- file.path(render_dir, output_file)
        
        if (!file.exists(generated_report)) {
          stop("Report HTML was not created: ", generated_report)
        }
        
        report_file(generated_report)
        render_committed <- TRUE
        
        showNotification(
          "Report generated successfully. You can now download it.",
          type = "message",
          duration = 8
        )
      })
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("QProMS_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        generated_report <- report_file()
        
        if (is.null(generated_report) || !file.exists(generated_report)) {
          stop("Report has not been generated yet. Please click 'Generate report' first.")
        }
        
        ok <- file.copy(generated_report, file, overwrite = TRUE)
        
        if (!isTRUE(ok)) {
          stop("Failed to copy generated report to download file.")
        }
      },
      contentType = "text/html"
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