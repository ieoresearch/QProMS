
box::use(
  shiny[moduleServer, tags, NS, div, p, h1, h4, HTML, fileInput, textInput, req, tagList, passwordInput, updateSelectInput, selectInput, actionButton, observeEvent, isolate, br, observe, updateActionButton, uiOutput, renderUI, icon, checkboxGroupInput, updateCheckboxGroupInput],
  bslib[page_fillable, layout_columns, navset_underline, nav_spacer, nav_remove, nav_panel, nav_insert, nav_select, tooltip, input_task_button],
  gargoyle[init, watch, trigger],
  shinyalert[shinyalert],
)

box::use(
  stats_view = app/view/statistics,
  heat_view = app/view/heatmap,
)

panels <- list(
  Volcano = list(target = "Rank", title = "Volcano", ui = stats_view$ui, ns = "statistics"),
  Heatmap = list(target = "Volcano", title = "Heatmap", ui = heat_view$ui, ns = "heatmap")
)

validate_annotation <- function(ann, sample_col, group_cols) {
  # Check if columns exist
  if (!all(c(sample_col, group_cols) %in% colnames(ann))) {
    missing_cols <- setdiff(c(sample_col, group_cols), colnames(ann))
    return(list(
      valid = FALSE, 
      error = paste("Selected columns not found:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Convert all columns to character
  ann[[sample_col]] <- as.character(ann[[sample_col]])
  
  # Safely convert group columns to character
  if (length(group_cols) > 0) {
    for (col in group_cols) {
      ann[[col]] <- as.character(ann[[col]])
    }
  }
  
  # Replace NA and empty strings with "Unknown"
  for (col in group_cols) {
    ann[[col]] <- trimws(ann[[col]])  # Remove leading/trailing whitespace
    ann[[col]][is.na(ann[[col]]) | ann[[col]] == "" | ann[[col]] == "NA"] <- "Unknown"
  }
  
  # Also clean sample IDs
  ann[[sample_col]] <- trimws(ann[[sample_col]])
  ann[[sample_col]][is.na(ann[[sample_col]])] <- "Unknown"
  
  return(list(valid = TRUE, data = ann, error = NULL))
}

# ================= UI =================
#' @export
ui <- function(id, primary_col) {
  ns <- NS(id)
  
  page_fillable(
    layout_columns(
      col_widths = c(-2, 8, -2),
      
      div(
        navset_underline(
          id = ns("start_nav"),
          
          nav_panel(
            title = "Upload File",
            
            fileInput(
              ns("upload_file"),
              "Intensity Table",
              accept = c(".txt", ".tsv", ".csv", ".parquet")
            ),
            
            br(),
            
            fileInput(
              ns("annotation_file"),
              "Annotation File (optional)",
              accept = c(".csv", ".txt", ".tsv")
            ),
            
            # dynamic UI
            selectInput(ns("annotation_sample_col"), "Sample ID column", choices = NULL),
            
            selectInput(
              ns("annotation_match_mode"),
              "Match mode",
              choices = c("Auto" = "auto", "Exact" = "exact", "Remove .raw" = "basename", "Extract suffix" = "extract_suffix"),
              selected = "auto"
            ),
            
            uiOutput(ns("annotation_group_cols_ui"))
          ),
          
          nav_panel(
            title = "Example Dataset",
            selectInput(ns("example"), "Datasets", choices = c("turbo"))
          )
        ),
        
        br(),
        
        input_task_button(ns("start"), "START")
      )
    )
  )
}

# ================= SERVER =================
#' @export
server <- function(id, r6, main_session) {
  moduleServer(id, function(input, output, session) {
    
    init("expdesig", "session")
    
    # ===== Load annotation and populate dropdown =====
    observeEvent(input$annotation_file, {
      req(input$annotation_file)
      
      ann <- tryCatch(
        utils::read.delim(input$annotation_file$datapath, sep = ";", header = TRUE),
        error = function(e) {
          shinyalert("Error", e$message, type = "error")
          return(NULL)
        }
      )
      
      req(!is.null(ann))
      
      cols <- colnames(ann)
      
      updateSelectInput(session, "annotation_sample_col", choices = cols, selected = cols[1])
    })
    
    # ===== Dynamic grouping checkboxes =====
    output$annotation_group_cols_ui <- renderUI({
      req(input$annotation_file)
      req(input$annotation_sample_col)
      
      ann <- tryCatch(
        utils::read.delim(input$annotation_file$datapath, sep = ";", header = TRUE),
        error = function(e) NULL
      )
      
      req(!is.null(ann))
      
      cols <- setdiff(colnames(ann), input$annotation_sample_col)
      
      checkboxGroupInput(
        session$ns("annotation_group_cols"),
        "Columns for grouping",
        choices = cols,
        selected = if(length(cols) > 0) cols[1] else NULL  # Select first column by default
      )
    })
    
    # ===== MAIN START BUTTON =====
    observeEvent(input$start, {
      
      req(input$upload_file)
      
      # Load main dataset
      r6$loading_data(input$upload_file$datapath, input$upload_file$name)
      r6$groupby_columns <- input$annotation_group_cols
      r6$annotation_group_cols <- input$annotation_group_cols
      
      # ===== Annotation logic =====
      if (!is.null(input$annotation_file)) {
        
        ann <- tryCatch(
          utils::read.delim(input$annotation_file$datapath, sep = ";", header = TRUE),
          error = function(e) {
            shinyalert("Annotation error", e$message, type = "error")
            return(NULL)
          }
        )
        
        req(!is.null(ann))
        
        sample_col <- input$annotation_sample_col
        group_cols <- input$annotation_group_cols
        match_mode <- input$annotation_match_mode
        
        # Validate input selections
        if (is.null(sample_col) || !(sample_col %in% colnames(ann))) {
          shinyalert("Error", "Please select a valid sample column", type = "error")
          return()
        }
        
        if (is.null(group_cols) || length(group_cols) == 0) {
          shinyalert("Error", "Please select at least one grouping column", type = "error")
          return()
        }
        
        # Validate and clean annotation data
        validation <- validate_annotation(ann, sample_col, group_cols)
        
        if (!validation$valid) {
          shinyalert("Error", validation$error, type = "error")
          return()
        }
        
        ann <- validation$data
        
        # Create condition column
        if (length(group_cols) == 1) {
          ann$condition <- ann[[group_cols]]
        } else {
          ann$condition <- apply(
            ann[, group_cols, drop = FALSE],
            1,
            function(x) paste(x, collapse = "_")
          )
        }
        
        # Validate condition column
        if (any(is.na(ann$condition)) || any(ann$condition == "")) {
          shinyalert(
            "Warning",
            "The selected grouping columns contain missing or empty values. These rows will be excluded from analysis.",
            type = "warning"
          )
          ann <- ann[!is.na(ann$condition) & ann$condition != "", ]
        }
        
        if (nrow(ann) == 0) {
          shinyalert(
            "Error", 
            "No valid samples remain after removing missing conditions. Please check your annotation file.",
            type = "error"
          )
          return()
        }
        
        # Trim whitespace from condition
        ann$condition <- trimws(ann$condition)
        ann$sample_id <- ann[[sample_col]]
        
        # Match samples to intensity table columns
        raw_cols <- colnames(r6$raw_data)
        
        normalize_keys <- function(keys, mode) {
          base <- basename(keys)
          no_raw <- sub("\\.raw$", "", base)
          
          if (mode == "exact") return(keys)
          if (mode == "basename") return(no_raw)
          
          if (mode == "extract_suffix") {
            return(sub("^.*?([A-Za-z0-9]+_col[0-9]+_[0-9]+)$", "\\1", no_raw))
          }
          
          return(no_raw)
        }
        
        raw_ids <- normalize_keys(raw_cols, match_mode)
        
        matched <- sapply(ann$sample_id, function(id) {
          id <- trimws(id)
          hits <- raw_cols[raw_ids == id]
          if (length(hits) == 1) hits else NA
        })
        
        ann$key <- matched
        
        # Count matched/unmatched samples
        n_matched <- sum(!is.na(ann$key))
        n_total <- nrow(ann)
        
        if (n_matched == 0) {
          shinyalert(
            "Error",
            "No samples from annotation file could be matched to intensity table columns. Please check your match mode.",
            type = "error"
          )
          return()
        }
        
        if (n_matched < n_total) {
          unmatched <- ann$sample_id[is.na(ann$key)]
          shinyalert(
            "Warning",
            paste(
              "Only", n_matched, "of", n_total, "samples were matched.",
              "Unmatched samples will be excluded:\n",
              paste(unmatched[1:min(5, length(unmatched))], collapse = ", "),
              if(length(unmatched) > 5) paste("... and", length(unmatched) - 5, "more")
            ),
            type = "warning"
          )
        }
        
        ann <- ann[, c("key", "condition")]
        ann <- ann[!is.na(ann$key), ]
        
        # apply to expdesign
        r6$add_replicate_and_label(ann)
      }
      
      trigger("expdesig")
      nav_select("top_navigation", "Design", session = main_session)
    
      })
    
  })
}
      




