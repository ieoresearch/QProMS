box::use(
  shiny[moduleServer, NS, selectInput, br, actionButton, fileInput, radioButtons, observeEvent, observe, div, icon, req, uiOutput, renderUI, updateSelectInput, removeUI],
  bslib[page_sidebar, layout_columns, layout_sidebar, tooltip, navset_card_underline, nav_panel, sidebar, accordion, accordion_panel, nav_select, input_switch, toggle_sidebar, input_task_button],
  esquisse[palettePicker],
  viridis[viridis],
  reactable[reactableOutput, renderReactable, reactable, colDef],
  rhandsontable[rHandsontableOutput, renderRHandsontable, hot_to_r],
  purrr[map, set_names, imap, keep_at, flatten_chr, discard_at],
  stringr[word, str_remove],
  dplyr[`%>%`, filter, select],
  gargoyle[init, watch, trigger],
)

## metterli dentro uno script utility functions
names <- c("A", "B", "C", "D", "E", "F", "G", "H")
palette_choices <- purrr::map(
  names, ~ viridis(n = 6, direction = -1, end = 0.90, begin = 0.10, option = .x)
  ) %>% purrr::set_names(names)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
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
              label = "Make ExpDesign Table",
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
              label = "Verify ExpDesign Table",
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
    ),
    sidebar = sidebar(
      id = ns("main_sidebar"),
      width = 375,
      accordion(
        id = ns("accordion"),
        multiple = FALSE,
        accordion_panel(
          title = "Upload File",
          id = ns("upload"),
          fileInput(
            inputId = ns("upload_file"),
            label = tooltip(
              trigger = list(
                "Input Table",
                icon("info-circle")
              ),
              "Upload a intensity table. accept '.txt', '.tsv', '.csv' format."
            ),
            multiple = FALSE,
            accept = c(".txt", ".tsv", ".csv")
          ),
          br(),
          actionButton(
            inputId = ns("confirm1"),
            label = "CONFIRM",
            class = "bg-primary"
          )
        ),
        accordion_panel(
          title = "Upload Parameters",
          id = ns("params"),
          fileInput(
            inputId = ns("upload_params"),
            label = tooltip(
              trigger = list(
                "Parameters",
                icon("info-circle")
              ),
              "Upload a QProMS parameters yaml for reproducibility. Is not necessary for new analysis."
            ),
            multiple = FALSE,
            placeholder = "QProMS_parameters.yaml",
            accept = ".yaml"
          ),
          br(),
          input_task_button(
            id = ns("start_with_params"),
            label = "START"
          )
        ),
        accordion_panel(
          title = "Visual Settings",
          id = ns("settings"),
          radioButtons(
            inputId = ns("plot_format"),
            label = "Plot extension",
            choices = c("svg" = "svg", "png" = "canvas"),
            selected = "svg"
          ), 
          palettePicker(
            inputId = ns("palette"),
            label = "Palettes:",
            choices = palette_choices,
            selected = "D"
          ),
          br(),
          actionButton(
            inputId = ns("update"),
            label = "UPDATE",
            class = "bg-primary"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    init("plot", "genes")
    
    observeEvent(input$start_with_params, {
      req(input$upload_file)
      
      if(!is.null(input$upload_params)) {
        r6$loading_patameters(input_path = input$upload_params$datapath)
        r6$preprocessing()
        r6$shiny_wrap_workflow()
        nav_select("top_navigation", "Preprocessing")
      }
    })
    
    observeEvent(input$confirm1, {
      req(input$upload_file)
      ## Load the data
      r6$loading_data(
        input_path = input$upload_file$datapath,
        input_name = input$upload_file$name
      )
      ## Show data in the relative tab
      output$raw_input_table <- renderReactable({
        r6$table_raw_data()
      })
      
      toggle_sidebar(id = "main_sidebar", open = "toggle", session = session)
      nav_select("upload_container", "Table Check")
      ## render message based of the table identification
      msg <- r6$identify_table_type()
      # r6$identify_table_status <- msg$status # da mettere dento la funzione sopra
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
              choices = c("Homo Sapiens" = "human", "Mus Musculus" = "mouse"),
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
            selectInput(
              ns("intensity_columns"),
              "Select Intensity Columns",
              colnames(r6$raw_data)[sapply(r6$raw_data, is.numeric)],
              multiple = TRUE
            ),
            input_switch(
              id = ns("log_transform"),
              label = "Perform log Transformation?",
              value = TRUE
            ),
            selectInput(
              inputId = ns("organism"),
              label = "Organism",
              choices = c("Homo Sapiens" = "human", "Mus Musculus" = "mouse"),
              selected = "human"
            )
          )
        })
      }
    })
    observeEvent(input$confirm2, {
      req(input$upload_file)
      nav_select("upload_container", "Experimental Design")
      output$exp_design <- renderRHandsontable({
        if(r6$identify_table_status == "success") {
          r6$make_expdesign(input$intensity_type)
        } else {
          r6$external_genes_column <- input$metadata_column
          r6$make_expdesign(input$intensity_columns)
        }
      })
    })
    observeEvent(input$verify, {
      req(input$upload_file)
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
          input_task_button(
            id = ns("start"),
            label = "START"
          )
        } else {
          actionButton(
            inputId = ns("back"),
            label = "BACK",
            class = "bg-primary"
          )
        }
        
      })
    })
    observeEvent(input$back, {
      nav_select("upload_container", "Experimental Design")
    })
    observeEvent(input$start, {
      if(!is.null(input$log_transform) && r6$identify_table_status == "danger") {
        r6$log_transform <- input$log_transform
      } else {
        r6$log_transform <- TRUE
      }
      r6$organism <- input$organism
      r6$protein_rank_target <- r6$expdesign$label[1]
      r6$preprocessing()
      r6$shiny_wrap_workflow()
      trigger("plot", "genes")
      nav_select("top_navigation", "Preprocessing")
    })
    
    observeEvent(input$update, {
      r6$plot_format <- input$plot_format
      r6$palette <- input$palette
      r6$define_colors()
      trigger("plot")
    })
    
  })
}
