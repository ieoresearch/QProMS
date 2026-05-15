box::use(
  shiny[moduleServer, NS, tags, selectInput, br, actionButton, selectizeInput, updateSelectizeInput, fileInput, radioButtons, observeEvent, observe, div, icon, req, uiOutput, renderUI, updateSelectInput, removeUI, textInput],
  bslib[page_fillable, layout_columns, layout_sidebar, tooltip, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, nav_select, input_switch, toggle_sidebar, nav_remove, input_task_button, nav_insert, layout_column_wrap],
  reactable[reactableOutput, renderReactable, reactable, colDef],
  rhandsontable[rHandsontableOutput, renderRHandsontable, hot_to_r],
  purrr[map, set_names, imap, keep_at, flatten_chr, discard_at, walk],
  stringr[word, str_remove],
  dplyr[`%>%`, filter, select],
  gargoyle[init, watch, trigger],
)

box::use(
  app/view/statistics,
  app/view/heatmap,
)

panels <- list(
  Volcano = list(
    target = "Rank",
    title  = "Volcano",
    ui     = statistics$ui,
    ns     = "statistics"
  ),
  Heatmap = list(
    target = "Volcano",
    title  = "Heatmap",
    ui     = heatmap$ui,
    ns     = "heatmap"
  )
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    navset_card_underline(
      id = ns("upload_container"),
      full_screen = TRUE, 
      nav_panel(
        "Input Table",
        reactableOutput(ns("raw_input_table"))
      ),
      nav_panel(
        title = "Table Check",
        layout_sidebar(
          uiOutput(ns("alert_message")),
          reactableOutput(ns("raw_summary_table")),
          sidebar = sidebar(
            width = 300,
            actionButton(
              inputId = ns("confirm2"),
              label = "Make Design Table",
              class = "bg-primary"
            ),
            uiOutput(ns("gene_column"))
          )
        )
      ),
      nav_panel(
        title = "Experimental Design",
        layout_sidebar(
          rHandsontableOutput(ns("exp_design")),
          sidebar = sidebar(
            width = 300,
            actionButton(
              inputId = ns("verify"),
              label = "Verify Design Table",
              class = "bg-primary"
            )
          )
        )
      ),
      nav_panel(
        title = "Experimental Design Check",
        layout_sidebar(
          uiOutput(ns("alert_message2")),
          reactableOutput(ns("complete_expdesign")),
          sidebar = sidebar(
            width = 300,
            uiOutput(ns("define_action_button"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, r6, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    init("genes")
    
    observe({
      watch("session")
      if(!r6$new_session){
        output$raw_input_table <- renderReactable({r6$table_raw_data()})
      }
    })
    
    observe({
      watch("expdesig")
      
      if(!is.null(r6$raw_data)) {
        output$raw_input_table <- renderReactable({
          r6$table_raw_data()
        })
        
        nav_select("upload_container", "Table Check")
        ## render message based of the table identification
        msg <- r6$identify_table_type()
        output$alert_message <- renderUI({
          imap(msg[[2]],
               .f = ~ div(
                 class = paste0("alert alert-", msg$status),
                 style = "white-space: pre-wrap;",
                 role = "alert",
                 .x
               ))
        })
        if (msg$status == "success") {
          output$raw_summary_table <- renderReactable({
            r6$create_summary_table()
          })
          output$gene_column <- renderUI({
            div(
              selectInput(
                inputId = ns("intensity_type"),
                label = "Select Intensity type",
                choices = r6$check_intensity_regex()
              ),
              selectInput(
                inputId = ns("organism"),
                label = "Organism",
                choices = c("Homo Sapiens" = "human",
                            "Mus Musculus" = "mouse",
                            "Drosophila Melanogaster" = "drosophila",
                            "Saccharomyces Cerevisiae" = "buddingyeast",
                            "Escherichia Coli" = "ecoli"),
                selected = "human"
              )
            )
          })
        } else {
          output$raw_summary_table <- renderReactable({NULL})
          output$gene_column <- renderUI({
            div(
              selectInput(
                ns("metadata_column"),
                "Select Gene Column",
                choices = colnames(r6$raw_data)[sapply(r6$raw_data, is.character)]
              ),
              textInput(
                ns("intensity_pattern"),
                "Filter intensity columns (regex or keyword)",
                placeholder = "e.g. LFQ|Intensity|Sample_"
              ),
              selectizeInput(
                ns("intensity_columns"),
                "Intensity columns",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  placeholder = "Select one or more intensity columns",
                  maxItems = NULL,
                  hideSelected = TRUE,
                  dropdownParent = 'body'
                )
              ),
              input_switch(
                id = ns("log_transform"),
                label = "Perform log Transformation?",
                value = TRUE
              ),
              selectInput(
                inputId = ns("organism"),
                label = "Organism",
                choices = c(
                  "Homo Sapiens" = "human",
                  "Mus Musculus" = "mouse",
                  "Drosophila Melanogaster" = "drosophila",
                  "Saccharomyces Cerevisiae" = "buddingyeast",
                  "Escherichia Coli" = "ecoli"
                ),
                selected = "human"
              )
            )
          })
        }
      }
      
    })
    
    observeEvent(r6$raw_data, {
      req(r6$raw_data)
      updateSelectizeInput(
        session,
        "intensity_columns",
        choices = colnames(r6$raw_data)[sapply(r6$raw_data, is.numeric)],
        server = TRUE
      )
    })
    observeEvent(input$intensity_pattern, {
      req(r6$raw_data)
      
      all_numeric <- colnames(r6$raw_data)[sapply(r6$raw_data, is.numeric)]
      
      if (nzchar(input$intensity_pattern)) {
        filtered <- grep(
          input$intensity_pattern,
          all_numeric,
          value = TRUE,
          ignore.case = TRUE
        )
      } else {
        filtered <- all_numeric
      }
      
      updateSelectizeInput(
        session,
        "intensity_columns",
        choices = filtered,
        selected = intersect(input$intensity_columns, filtered),
        server = TRUE
      )
    })
    
    observeEvent(input$confirm2, {
      if(!is.null(r6$raw_data) & r6$new_session) {
        nav_select("upload_container", "Experimental Design")
        output$exp_design <- renderRHandsontable({
          if(r6$identify_table_status == "success") {
            r6$make_expdesign(input$intensity_type)
          } else {
            r6$external_genes_column <- input$metadata_column
            r6$make_expdesign(input$intensity_columns)
          }
        })
      }
    })
    observeEvent(input$verify, {
      if(!is.null(r6$raw_data) & r6$new_session) {
        des <- hot_to_r(input$exp_design) %>% 
          filter(keep) %>% 
          select(-keep)
        result <- r6$validate_expdesign(des)
        nav_select("upload_container", "Experimental Design Check")
        output$alert_message2 <- renderUI({
          imap(
            discard_at(result, "validation_status"),
            .f = ~ div(
              class = paste0("alert alert-", word(.x, 1)),
              style = "white-space: pre-wrap;",
              role = "alert",
              str_remove(.x, word(.x, 1))
            )
          )
        })
        if(result$validation_status){
          output$complete_expdesign <- renderReactable({
            r6$add_replicate_and_label(des)
          })
        } else {
          output$complete_expdesign <- renderReactable({NULL})
        }
        output$define_action_button <- renderUI({
          if(result$validation_status){
            bslib::layout_column_wrap(
              width = 1,
              input_task_button(
                id = ns("start"),
                label = "START",
                width = "100%"
              )
            )
          } else {
            actionButton(
              inputId = ns("back"),
              label = "BACK",
              width = "100%",
              class = "bg-primary"
            )
          }
          
        })
      }
    })
    
    observeEvent(input$back, {
      nav_select("upload_container", "Experimental Design")
    })
    
    observeEvent(input$start, {
      if(!is.null(input$log_transform) && r6$identify_table_status == "info") {
        r6$log_transform <- input$log_transform
      } else {
        r6$log_transform <- TRUE
      }
      r6$organism <- input$organism
      r6$protein_rank_target <- r6$expdesign$label[1]
      r6$preprocessing()
      if(is.null(r6$data)) return(NULL)
      r6$shiny_wrap_workflow()
      trigger("genes")
      nav_select("top_navigation", "Preprocessing", session = main_session)
      purrr::walk(names(panels), ~ nav_remove("top_navigation", target  = .x, session = main_session))
      if (r6$with_statistics) {
        purrr::walk(
          panels,
          ~ nav_insert(
            "top_navigation",
            target  = .x$target,
            select  = FALSE,
            session = main_session,
            nav_panel(
              title = .x$title,
              class = "html-fill-item html-fill-container bslib-gap-spacing",
              style = "--bslib-navbar-margin:0; padding:0",
              .x$ui(ns(.x$ns))
            )
          )
        )
      } 
    })
    
    statistics$server("statistics", r6 = r6, main_session = main_session)
    heatmap$server("heatmap", r6 = r6, main_session = main_session)
  })
}
