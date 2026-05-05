box::use(
  shiny[moduleServer, NS, selectInput, sliderInput, isolate, numericInput, updateSelectInput, updateSliderInput, br, actionButton, observeEvent, icon, observe, req, conditionalPanel, reactiveVal, renderPlot, plotOutput, fileInput, renderText, verbatimTextOutput],
  bslib[page_sidebar, input_task_button, layout_columns, navset_card_underline, nav_panel, update_switch, sidebar, accordion, accordion_panel, input_switch, accordion_panel_remove, tooltip, nav_hide, nav_show],
  echarts4r[
    echarts4rOutput, renderEcharts4r,
    e_charts, e_bar, e_line, e_tooltip, e_legend,
    e_x_axis, e_y_axis, e_title, e_grid, e_add_nested, e_color
  ],
  plotly[plotlyOutput, renderPlotly, ggplotly, layout],
  gargoyle[watch, trigger, init],
  stats[stats_reorder = reorder],
  trelliscope[trelliscopeOutput, renderTrelliscope],
  reactable[reactable, reactableOutput, renderReactable, getReactableState], 
  ggplot2[
    ggplot, geom_col, coord_flip, labs, theme_minimal, element_text,
    aes, theme_void, geom_text, margin_ggplo = margin, theme,
    element_blank, facet_grid, facet_wrap, element_rect,
    geom_boxplot, geom_jitter, scale_fill_brewer
  ],
  dplyr[
    `%>%`, select, filter, arrange, desc, slice_head, rename,
    all_of, any_of, summarise, group_by, left_join, mutate,
    across, n_distinct
  ],  tidyr[separate],
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
          plotlyOutput(ns("protein_counts_plot"), height = "650px")
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
          plotlyOutput(ns("cv_plot"), height = "650px")
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
        ),
        nav_panel(
          "Missed Cleavages",
          echarts4rOutput(ns("missed_cleavages_plot"))
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
          title = "Exclusion criteria",
          id = ns("exclusion_criteria"),
          
          input_switch(
            id = ns("apply_exclusion"),
            label = "Exclude samples based on protein count",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.apply_exclusion",
            ns = ns,
            numericInput(
              inputId = ns("excl_protein_threshold"),
              label = "Minimum number of proteins",
              value = 4000,
              min = 0,
              step = 100
            )
          ),
          
          input_switch(
            id = ns("excl_cv"),
            label = "Exclude by CV > threshold",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.excl_cv",
            ns = ns,
            numericInput(
              inputId = ns("excl_cv_threshold"),
              label = "CV threshold",
              value = 0.3,
              min = 0,
              step = 0.05
            )
          ),
          
          input_switch(
            id = ns("excl_missed_cleavages"),
            label = "Exclude by missed cleavages %",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.excl_missed_cleavages",
            ns = ns,
            numericInput(
              inputId = ns("excl_mc_threshold"),
              label = "Missed cleavages % threshold",
              value = 5,
              min = 0,
              max = 100
            )
          ),
          
          shiny::tags$h5("Exclusion Summary:"),
          verbatimTextOutput(ns("exclusion_summary")),
          
          shiny::tags$hr(),
          shiny::tags$h5("Excluded Samples:"),
          reactableOutput(ns("excluded_samples_table")),
          
          shiny::tags$hr(),
          shiny::tags$h5("Sample Intensity Profile:"),
          
          shiny::tags$p(
            "Click on a sample row above to visualize its protein intensities",
            style = "font-size: 0.9em; color: #666;"
          ),
          verbatimTextOutput(ns("sample_info")),
          plotOutput(ns("sample_intensity_plot"), height = "500px")
          
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
          selectInput(
            inputId = ns("peptides_input"),
            label = tooltip(
              trigger = list(
                "Column Type",
                icon("info-circle")
              ),
              "This filter applies only for MaxQuant proteinGroups.txt files in protein mode."
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
              "This filter applies only for MaxQuant proteinGroups.txt files in protein mode."
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteinGroups.txt files."
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteinGroups.txt files."
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
              "If TRUE will be removed. This filter applies only for MaxQuant proteinGroups.txt files."
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
            )
          )
        )
        

    )
  ))}


#' @export
server <- function(id, r6) {
  workflow_done <- reactiveVal(0)
  moduleServer(id, function(input, output, session) {
    
    only_first_time_trigger <- reactiveVal(TRUE)
    selected_sample <- reactiveVal(NULL)
    
    observe({
      watch("session")
      updateSelectInput(inputId = "analysis_level", selected = r6$analysis_level)
      updateSelectInput(inputId = "valid_values_input", selected = r6$valid_val_filter)
      updateSliderInput(inputId = "valid_values_slider", value = r6$valid_val_thr * 100)
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
      
      # output$protein_counts_plot <- renderEcharts4r({
      #   if (!is.null(r6$filtered_data) && only_first_time_trigger()) {
      #     r6$plot_empty_message("Press Process Button")
      #   } else if (!is.null(r6$filtered_data)) {
      #     r6$plot_protein_counts()
      #   }
      # })
      
      if (!is.null(r6$input_type)) {
        if (r6$analysis_level == "peptide" || !r6$input_type %in% c("MaxQuant", "ProteomeDiscoverer")) {
          accordion_panel_remove("accordion", "Subset by Peptides", session = session)
        }
        if (r6$analysis_level == "peptide" || r6$input_type != "MaxQuant") {
          accordion_panel_remove("accordion", "Remove Contaminants", session = session)
        }
      }
    })
    
    observe({
      selected_row <- getReactableState("excluded_samples_table", "selected")
      
      if (!is.null(selected_row) && length(selected_row) > 0) {
        if (!is.null(r6$excluded_samples) && selected_row <= nrow(r6$excluded_samples)) {
          sample_key <- r6$excluded_samples$key[selected_row]
          selected_sample(sample_key)
        }
      }
    })
    
    observeEvent(input$update, {
      only_first_time_trigger(FALSE)
      
      r6$min_proteins_pct <- input$min_proteins_pct / 100
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
      
      r6$excl_protein_threshold <- if (isTRUE(input$apply_exclusion)) {
        input$excl_protein_threshold
      } else {
        NULL
      }
      
      r6$excl_cv_threshold <- if (isTRUE(input$excl_cv)) {
        input$excl_cv_threshold
      } else {
        NULL
      }
      
      r6$excl_mc_threshold <- if (isTRUE(input$excl_missed_cleavages)) {
        input$excl_mc_threshold
      } else {
        NULL
      }
      
      print(r6$excl_protein_threshold)
      print(r6$excl_cv_threshold)
      print(r6$excl_mc_threshold)
      
      if (!is.null(r6$data)) {
        r6$shiny_wrap_workflow()
        
        
        output$exclusion_summary <- renderText({
          if (is.null(r6$exclusion_summary)) {
            return("No exclusion applied")
          }
          
          excl_summary <- r6$exclusion_summary
          
          paste0(
            "Total samples: ", excl_summary$total_samples, "\n",
            "Excluded: ", excl_summary$excluded_count, "\n",
            "Remaining: ", excl_summary$remaining_samples
          )
        })
        
        output$excluded_samples_table <- renderReactable({
          if (is.null(r6$excluded_samples) || nrow(r6$excluded_samples) == 0) {
            return(NULL)
          }
          
          reactable::reactable(
            r6$excluded_samples,
            striped = TRUE,
            compact = TRUE,
            resizable = TRUE,
            onClick = "select",
            selection = "single",
            columns = list(
              label = reactable::colDef(name = "Sample"),
              condition = reactable::colDef(name = "Condition"),
              replicate = reactable::colDef(name = "Replicate"),
              count = reactable::colDef(name = "Count"),
              cv = reactable::colDef(name = "CV"),
              mc_percent = reactable::colDef(name = "Missed Cleavages %"),
              exclusion_reason = reactable::colDef(name = "Reason", minWidth = 300)
            )
          )
        })
        
        
        # ===== NEW: Render sample info =====
        output$sample_info <- renderText({
          sample <- selected_sample()
          if (is.null(sample)) {
            return("Click on a sample row to view its intensity profile")
          }
          
          sample_data <- r6$excluded_samples %>%
            filter(key == sample)
          
          if (nrow(sample_data) == 0) {
            return("Sample not found")
          }
          
          paste(
            "Selected Sample:", sample_data$label,
            "| Condition:", sample_data$condition,
            "| Replicate:", sample_data$replicate,
            "| Proteins: ", sample_data$count,
            "| Exclusion Reason:", sample_data$exclusion_reason,
            sep = " "
          )
        })
        
        # ===== NEW: Render intensity plot for selected sample =====
        output$sample_intensity_plot <- renderPlot({
          sample <- selected_sample()
          
          if (is.null(sample)) {
            return(ggplot() +
                     theme_void() +
                     geom_text(
                       aes(x = 0.5, y = 0.5, label = "Select a sample to view intensities"),
                       size = 5
                     ) +
                     labs(title = ""))
          }
          
          # Get the intensities for this sample from raw_data
          # Assuming the first column is the feature ID
          intensity_data <- tryCatch({
            if (r6$input_type == "DIA-NN") {
              r6$data %>%
                filter(key == sample) %>%
                filter(!is.na(intensity), intensity > 0) %>%
                select(feature_id = feature_names, intensity) %>%
                arrange(desc(intensity)) %>%
                slice_head(n = 30)
            } else {
              r6$raw_data_unique %>%
                select(feature_id = feature_names, all_of(sample)) %>%
                rename(intensity = !!sample) %>%
                filter(!is.na(intensity), intensity > 0) %>%
                arrange(desc(intensity)) %>%
                slice_head(n = 30)
            }
          }, error = function(e) {
            message("sample_intensity_plot ERROR: ", e$message)
            return(NULL)
          })
          
          if (is.null(intensity_data) || nrow(intensity_data) == 0) {
            return(ggplot() +
                     theme_void() +
                     geom_text(
                       aes(x = 0.5, y = 0.5, label = "No intensity data available for this sample"),
                       size = 5
                     ) +
                     labs(title = ""))
          }
          
          # Create bar plot
          ggplot2::ggplot(intensity_data, aes(x = stats_reorder(feature_id, intensity), y = intensity)) +
            geom_col(fill = "#6EC1E4", width = 0.7) +
            coord_flip() +
            labs(
              title = paste("Top 30 Protein Intensities:", sample),
              x = "Protein ID",
              y = "Intensity (log scale)"
            ) +
            theme_minimal() +
            theme(
              axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, face = "bold", margin = ggplot2::margin(b = 10)),
              axis.title = element_text(size = 10),
              panel.grid.minor = element_blank()
            )
        }, res = 96)
        
        trigger("genes")
        
        output$protein_counts_plot <- renderPlotly({
          workflow_done()
          req(r6$filtered_data)
          req(r6$expdesign)
          req(r6$annotation_group_cols)
          
          group_cols <- r6$annotation_group_cols
          
          counts_data <- r6$filtered_data %>%
            dplyr::select(-dplyr::any_of(c("condition", "replicate"))) %>%
            dplyr::left_join(
              r6$expdesign %>%
                dplyr::select(
                  label,
                  condition,
                  replicate,
                  dplyr::any_of(group_cols)
                ),
              by = "label"
            ) %>%
            dplyr::filter(!is.na(intensity), !is.na(condition)) %>%
            dplyr::group_by(
              label,
              condition,
              replicate,
              dplyr::across(dplyr::any_of(group_cols))
            ) %>%
            dplyr::summarise(
              count = dplyr::n_distinct(feature_names),
              .groups = "drop"
            )
          
          if (length(group_cols) >= 2) {
            
            col_group <- group_cols[1]  # e.g. PMethod
            row_group <- group_cols[2]  # e.g. Input
            
            if (!all(c(col_group, row_group) %in% base::colnames(counts_data))) {
              counts_data <- counts_data %>%
                tidyr::separate(
                  condition,
                  into = group_cols,
                  sep = "_",
                  remove = FALSE,
                  extra = "merge",
                  fill = "right"
                )
            }
            
            stats_data <- counts_data %>%
              dplyr::group_by(
                dplyr::across(dplyr::all_of(c(col_group, row_group)))
              ) %>%
              dplyr::summarise(
                mean_count = base::mean(count, na.rm = TRUE),
                median_count = stats::median(count, na.rm = TRUE),
                sd_count = stats::sd(count, na.rm = TRUE),
                min_count = base::min(count, na.rm = TRUE),
                max_count = base::max(count, na.rm = TRUE),
                n_samples = dplyr::n(),
                .groups = "drop"
              )
            
            counts_data <- counts_data %>%
              dplyr::left_join(
                stats_data,
                by = c(col_group, row_group)
              ) %>%
              dplyr::mutate(
                hover_text = paste0(
                  col_group, ": ", .data[[col_group]], "<br>",
                  row_group, ": ", .data[[row_group]], "<br>",
                  "Sample: ", label, "<br>",
                  "Replicate: ", replicate, "<br>",
                  "Count: ", count, "<br><br>",
                  "<b>Group statistics</b><br>",
                  "Mean count: ", base::round(mean_count, 1), "<br>",
                  "Median count: ", base::round(median_count, 1), "<br>",
                  "SD count: ", base::round(sd_count, 1), "<br>",
                  "Min count: ", min_count, "<br>",
                  "Max count: ", max_count, "<br>",
                  "n samples: ", n_samples
                )
              )
            
            p <- suppressWarnings(
              ggplot2::ggplot(
                counts_data,
                ggplot2::aes(
                  x = "Samples",
                  y = count,
                  fill = .data[[col_group]],
                  text = hover_text
                )
              ) +
                ggplot2::geom_boxplot(
                  width = 0.45,
                  alpha = 0.75,
                  outlier.shape = NA
                ) +
                ggplot2::geom_jitter(
                  ggplot2::aes(text = hover_text),
                  width = 0.08,
                  size = 2,
                  alpha = 0.75
                ) +
                ggplot2::facet_grid(
                  stats::as.formula(paste(row_group, "~", col_group))
                ) +
                ggplot2::labs(
                  title = paste("Feature counts by", row_group, "and", col_group),
                  x = NULL,
                  y = "Feature Count"
                ) +
                ggplot2::theme_minimal(base_size = 13) +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(
                    size = 16,
                    face = "bold",
                    hjust = 0.5,
                    margin = ggplot2::margin(b = 15)
                  ),
                  axis.text.x = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.text.y = ggplot2::element_text(size = 10),
                  axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
                  strip.text = ggplot2::element_text(size = 12, face = "bold"),
                  strip.background = ggplot2::element_rect(fill = "grey90", color = "grey50"),
                  panel.grid.minor = ggplot2::element_blank(),
                  legend.position = "none"
                )
            )
            
            suppressWarnings(
              plotly::ggplotly(p, tooltip = "text") %>%
                plotly::layout(
                  title = list(x = 0.5),
                  margin = list(l = 90, r = 40, t = 80, b = 50)
                )
            )
            
          } else {
            
            counts_data <- counts_data %>%
              dplyr::arrange(dplyr::desc(count)) %>%
              dplyr::mutate(
                hover_text = paste0(
                  "Sample: ", label, "<br>",
                  "Condition: ", condition, "<br>",
                  "Replicate: ", replicate, "<br>",
                  "Count: ", count
                )
              )
            
            p <- ggplot2::ggplot(
              counts_data,
              ggplot2::aes(
                x = stats::reorder(label, -count),
                y = count,
                text = hover_text
              )
            ) +
              ggplot2::geom_col(width = 0.75, fill = "#5470C6") +
              ggplot2::labs(
                title = "Feature Count by Sample",
                x = "Sample",
                y = "Feature Count"
              ) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(
                  size = 16,
                  face = "bold",
                  hjust = 0.5
                ),
                axis.text.x = ggplot2::element_text(
                  angle = 45,
                  hjust = 1,
                  size = 8
                ),
                panel.grid.minor = ggplot2::element_blank()
              )
            
            suppressWarnings(
              plotly::ggplotly(p, tooltip = "text") %>%
                plotly::layout(
                  title = list(x = 0.5),
                  margin = list(l = 90, r = 30, t = 80, b = 100)
                )
            )
          }
        })
        
        output$distribution_plot <- renderEcharts4r({
          workflow_done()
          r6$plot_distribution()
        })
        
        output$valid_values_plot <- renderEcharts4r({
          workflow_done()
          r6$plot_protein_coverage()
        })
        
        output$upset_plot <- renderPlot({
          workflow_done()
          r6$plot_protein_coverage_intersections()
        })
        
        output$cv_plot <- renderPlotly({
          req(r6$imputed_data)
          req(r6$expdesign)
          req(r6$annotation_group_cols)
          
          group_cols <- r6$annotation_group_cols
          
          plot_data <- r6$imputed_data %>%
            dplyr::select(-dplyr::any_of(c("condition", "replicate"))) %>%
            dplyr::left_join(
              r6$expdesign %>%
                dplyr::select(
                  label,
                  condition,
                  replicate,
                  dplyr::any_of(group_cols)
                ),
              by = "label"
            ) %>%
            dplyr::filter(!is.na(intensity), !is.na(condition)) %>%
            dplyr::group_by(
              label,
              condition,
              replicate,
              dplyr::across(dplyr::any_of(group_cols))
            ) %>%
            dplyr::summarise(
              cv = stats::sd(intensity, na.rm = TRUE) / abs(base::mean(intensity, na.rm = TRUE)),
              n_features = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              cv = base::replace(cv, base::is.infinite(cv) | base::is.nan(cv), 0)
            )
          
          if (length(group_cols) >= 2) {
            
            col_group <- group_cols[1]  # e.g. PMethod
            row_group <- group_cols[2]  # e.g. Input
            
            if (!all(c(col_group, row_group) %in% base::colnames(plot_data))) {
              plot_data <- plot_data %>%
                tidyr::separate(
                  condition,
                  into = group_cols,
                  sep = "_",
                  remove = FALSE,
                  extra = "merge",
                  fill = "right"
                )
            }
            
            stats_data <- plot_data %>%
              dplyr::group_by(
                dplyr::across(dplyr::all_of(c(col_group, row_group)))
              ) %>%
              dplyr::summarise(
                mean_cv = base::mean(cv, na.rm = TRUE),
                median_cv = stats::median(cv, na.rm = TRUE),
                sd_cv = stats::sd(cv, na.rm = TRUE),
                min_cv = base::min(cv, na.rm = TRUE),
                max_cv = base::max(cv, na.rm = TRUE),
                n_samples = dplyr::n(),
                .groups = "drop"
              )
            
            plot_data <- plot_data %>%
              dplyr::left_join(
                stats_data,
                by = c(col_group, row_group)
              ) %>%
              dplyr::mutate(
                panel_label = paste0(.data[[col_group]], " / ", .data[[row_group]]),
                hover_text = paste0(
                  col_group, ": ", .data[[col_group]], "<br>",
                  row_group, ": ", .data[[row_group]], "<br>",
                  "Sample: ", label, "<br>",
                  "Replicate: ", replicate, "<br>",
                  "Sample CV: ", base::round(cv, 4), "<br><br>",
                  "<b>Group statistics</b><br>",
                  "Mean CV: ", base::round(mean_cv, 4), "<br>",
                  "Median CV: ", base::round(median_cv, 4), "<br>",
                  "SD CV: ", base::round(sd_cv, 4), "<br>",
                  "Min CV: ", base::round(min_cv, 4), "<br>",
                  "Max CV: ", base::round(max_cv, 4), "<br>",
                  "n samples: ", n_samples
                )
              )
            
            p <- suppressWarnings(
              ggplot2::ggplot(
                plot_data,
                ggplot2::aes(
                  x = "Samples",
                  y = cv,
                  fill = .data[[col_group]],
                  text = hover_text
                )
              ) +
                ggplot2::geom_boxplot(
                  width = 0.45,
                  alpha = 0.75,
                  outlier.shape = NA
                ) +
                ggplot2::geom_jitter(
                  ggplot2::aes(text = hover_text),
                  width = 0.08,
                  size = 2,
                  alpha = 0.75
                ) +
                ggplot2::facet_grid(
                  stats::as.formula(paste(row_group, "~", col_group))
                ) +
                ggplot2::labs(
                  title = paste("CV distribution by", row_group, "and", col_group),
                  x = NULL,
                  y = "Coefficient of Variation"
                ) +
                ggplot2::theme_minimal(base_size = 13) +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(
                    size = 16,
                    face = "bold",
                    hjust = 0.5,
                    margin = ggplot2::margin(b = 15)
                  ),
                  axis.text.x = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.text.y = ggplot2::element_text(size = 10),
                  axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
                  strip.text = ggplot2::element_text(size = 12, face = "bold"),
                  strip.background = ggplot2::element_rect(fill = "grey90", color = "grey50"),
                  panel.grid.minor = ggplot2::element_blank(),
                  legend.position = "none"
                )
            )
            
            suppressWarnings(
              plotly::ggplotly(p, tooltip = "text") %>%
                plotly::layout(
                  title = list(x = 0.5),
                  margin = list(l = 90, r = 40, t = 80, b = 50)
                )
            )
          } else {
            
            plot_data <- plot_data %>%
              dplyr::arrange(dplyr::desc(cv)) %>%
              dplyr::mutate(
                hover_text = paste0(
                  "Sample: ", label, "<br>",
                  "Condition: ", condition, "<br>",
                  "Replicate: ", replicate, "<br>",
                  "CV: ", base::round(cv, 4), "<br>",
                  "Features: ", n_features
                )
              )
            
            p <- ggplot2::ggplot(
              plot_data,
              ggplot2::aes(
                x = stats::reorder(label, -cv),
                y = cv,
                text = hover_text
              )
            ) +
              ggplot2::geom_col(width = 0.75, fill = "#5470C6") +
              ggplot2::labs(
                title = "Coefficient of Variation by Sample",
                x = "Sample",
                y = "CV"
              ) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(
                  size = 16,
                  face = "bold",
                  hjust = 0.5
                ),
                axis.text.x = ggplot2::element_text(
                  angle = 45,
                  hjust = 1,
                  size = 8
                ),
                panel.grid.minor = ggplot2::element_blank()
              )
            
            plotly::ggplotly(p, tooltip = "text") %>%
              plotly::layout(
                title = list(x = 0.5),
                margin = list(l = 90, r = 30, t = 80, b = 100)
              )
          }
        })
        

        output$missed_cleavages_plot <- renderEcharts4r({
          workflow_done()
          r6$plot_missed_cleavages()
        })
        
        output$missval_distribution_plot <- renderTrelliscope({
          workflow_done()
          r6$plot_missval_distribution()
        })
        
        output$missing_data_counts_plot <- renderEcharts4r({
          workflow_done()
          r6$plot_missing_data()
        })
        
        output$imputed_table <- renderReactable({
          workflow_done()
          req(r6$normalized_data)
          
          df <- if (r6$imp_methods == "none") {
            r6$normalized_data
          } else {
            r6$imputed_data
          }
          
          req(df)
          
          reactable::reactable(
            head(df, 100),
            striped = TRUE,
            compact = TRUE,
            resizable = TRUE
          )
        })
      }
    })
  })
}