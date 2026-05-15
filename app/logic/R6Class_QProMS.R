box::use(
  R6[R6Class],
  arrow[read_parquet],
  data.table[fread],
  utils[head, combn, modifyList, write.csv, write.table],
  dplyr[`%>%`, n_distinct, group_by, summarise, n, filter, mutate, ungroup, row_number, select, across, where, all_of, na_if, left_join, rename, if_else, case_when, full_join, relocate, inner_join, distinct, count, everything, pull, arrange, slice_head, slice_tail, last_col, rename_with, ends_with, desc, rename_at, vars, bind_rows, group_map, rowwise, if_all, group_keys, sym, slice_max, c_across],
  tidyr[drop_na, pivot_longer, pivot_wider, expand_grid, unite, separate_rows, unnest_wider, nest, separate],
  purrr[map, map2, set_names, imap, keep_at, flatten_chr, reduce, map_chr, map_dbl, possibly, list_rbind, pluck, compact, flatten],
  stringr[str_detect, word, str_replace_all, str_extract, str_replace, str_split_1, str_remove, str_which, str_flatten],
  tibble[tibble, as_tibble, column_to_rownames, rownames_to_column, enframe, deframe],
  vsn[vsn2, predict],
  limma[lmFit, eBayes, topTable],
  missForest[missForest],
  stats[sd, runif, rnorm, prcomp, cor, na.omit, t.test, p.adjust, wilcox.test, model.matrix, aov, hclust, dist, cutree, as.dendrogram, median, qt],
  rbioapi[rba_string_interactions_network],
  OmnipathR[get_complex_genes, import_omnipath_complexes],
  clusterProfiler[enrichGO, simplify, gseGO, enrichKEGG, bitr, enrichWP, gseKEGG, gseWP],
  enrichplot[gseaplot2],
  viridis[viridis],
  htmlwidgets[JS],
  reactable[reactable, colDef],
  rhandsontable[rhandsontable, hot_col],
  trelliscope[panel_lazy, as_trelliscope_df, set_default_layout, add_trelliscope_resource_path],
  heatmaply[heatmaply],
  UpSetR[upset],
  ggplot2[ggplot, theme_void, aes, geom_text, xlab],
  plotly[plotly_empty, config, layout],
  rlist[list.save, list.load],
  openxlsx[createStyle, createWorkbook, addWorksheet, writeDataTable, setColWidths, addStyle, saveWorkbook, read.xlsx],  echarts4r[e_charts, e_bar, e_x_axis, e_y_axis, e_tooltip, e_legend, e_grid, e_color, e_toolbox_feature, e_show_loading, e_boxplot, e_histogram, e_data, e_scatter, e_scatter_3d, e_x_axis_3d, e_y_axis_3d, e_z_axis_3d, e_correlations, e_visual_map, e_title, e_mark_point, e_add_nested, e_group, e_line, e_band2, e_graph, e_graph_nodes, e_graph_edges, e_labels, e_draft, e_flip_coords, e_lm]
)

box::use(
  app/static/inputs_type_lists,
  app/static/contaminants
)

# ============================================================================
# HELPER FUNCTIONS FOR REACTABLE CONFIGURATIONS
# ============================================================================

#' Create reactable with configuration list
.reactable_from_config <- function(data, config, coldef = NULL) {
  args <- config
  args$data <- data
  if (!is.null(coldef)) {
    args$defaultColDef <- coldef
  }
  do.call(reactable, args)
}

#' Configuration for raw data table
.get_raw_data_table_config <- function() {
  list(
    wrap = FALSE,
    striped = TRUE,
    resizable = TRUE,
    compact = TRUE,
    bordered = TRUE,
    height = "auto",
    paginationType = "simple",
    showPageInfo = FALSE,
    defaultPageSize = 15
  )
}

#' Configuration for imputed data table
.get_imputed_table_config <- function() {
  list(
    wrap = FALSE,
    striped = TRUE,
    resizable = TRUE,
    compact = TRUE,
    height = "auto",
    paginationType = "simple"
  )
}

#' @export
QProMS <- R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    annotation_data = NULL,
    annotation_sample_col = NULL,
    annotation_group_cols = NULL,
    annotation_match_mode = "auto",
    groupby_columns = NULL,
    excl_protein_threshold = NULL,
    excl_cv_threshold = NULL,
    excl_mc_threshold = NULL,
    exclusion_summary = NULL,
    excluded_samples = NULL,
    raw_data = NULL,
    raw_data_unique = NULL,
    new_session = TRUE,
    identify_table_status = NULL,
    data = NULL,
    input_type = NULL,
    intensity_type = NULL,
    external_genes_column = NULL,
    log_transform = TRUE,
    analysis_level = "protein",
    feature_id_col = NULL,
    feature_names = NULL,
    organism = NULL,
    expdesign = NULL,
    plot_format = "svg",
    plot_font_size = 16,
    palette = "D",
    color_palette = NULL,
    primary_color = "#6EC1E4",
    enable_exclusion = FALSE,
    min_proteins_pct = 0.7,
    #################################
    # parameters for data wrangling #
    filtered_data = NULL,
    filtered_gene_vector = NULL,
    valid_val_filter = "alog",
    valid_val_thr = 1,
    pep_filter = "peptides",
    pep_thr = 2,
    rev = TRUE,
    cont = TRUE,
    oibs = TRUE,
    ################################
    # parameters for normalization #
    normalized_data = NULL,
    norm_methods = "None",
    is_norm = FALSE,
    #############################
    # parameters for imputation #
    imp_methods_used = NULL, 
    imputed_data = NULL,
    imp_methods = "missforest",
    mar_mnar_thresh = 0.75,
    imp_shift = 1.8,
    imp_scale = 0.3,
    missforest_ntree = 10,
    missforest_niter = 1,
    cor_method = "pearson",
    is_mixed = NULL,
    is_imp = FALSE,
    ################
    # protein rank #
    rank_data = NULL,
    protein_rank_target = NULL,
    protein_rank_by_cond = FALSE,
    protein_rank_selection = "top",
    protein_rank_top_n = 0.1,
    protein_rank_list = NULL,
    #############################
    # parameters For Statistics #
    with_statistics = FALSE,
    not_test_cond = NULL,
    all_test_combination = NULL,
    contrasts = NULL,
    univariate = NULL,
    univariate_test_type = "welch",
    univariate_paired = FALSE,
    stat_table = NULL,
    univariate_alpha = 0.05,
    univariate_p_adj_method = "BH",
    fold_change = 1,
    univariate_same_y = TRUE,
    univariate_same_x = FALSE,
    anova_table = NULL,
    anova_matrix = NULL,
    row_den = NULL,
    col_den = NULL,
    anova_alpha = 0.05,
    anova_p_adj_method = "BH",
    anova_clust_method = "complete",
    z_score = TRUE,
    anova_manual_order = FALSE,
    anova_col_order = NULL,
    clusters_number = 1,
    ######################
    # parameters For ORA #
    ora_result_list = NULL,
    ora_table = NULL,
    go_ora_from_statistic = NULL,
    go_ora_focus = NULL,
    go_ora_database = "GO",
    go_ora_alpha = 0.05,
    go_ora_p_adj_method = "BH",
    go_ora_term = "BP",
    go_ora_top_n = 10,
    go_ora_simplify_thr = 1,
    go_ora_min_gs_size = 10,
    go_ora_max_gs_size = 500,
    go_ora_background = FALSE,
    go_ora_plot_arrenge = "fold_enrichment",
    #######################
    # parameters for GSEA #
    gsea_result_list = NULL,
    go_gsea_by_cond = FALSE,
    gsea_table = NULL,
    go_gsea_rank_with = "fc",
    go_gsea_database = "GO",
    go_gsea_tested_condition = NULL,
    go_gsea_alpha = 0.05,
    go_gsea_p_adj_method = "BH",
    go_gsea_term = "BP",
    go_gsea_focus = NULL,
    go_gsea_top_n = 10,
    go_gsea_simplify_thr = 1,
    go_gsea_min_gs_size = 10,
    go_gsea_max_gs_size = 500,
    go_gsea_plot_arrenge = "NES",
    ##########################
    # parameters for network #
    nodes_table = NULL,
    edges_table = NULL,
    name_for_edges = NULL,
    network_from_statistic = NULL,
    network_score_thr = NULL,
    network_focus_uni = NULL,
    network_focus_multi = NULL,
    network_uni_direction = NULL,
    selected_nodes = NULL,
    pdb_database = NULL,
    ###########
    # Methods #
    loading_data = function(input_path, input_name = NULL) {

      message("🔥 NEW LOADER ACTIVE 🔥")

      if (is.null(input_name) || is.na(input_name) || input_name == "") {
        input_name <- input_path
      }

      message("FILE NAME: ", input_name)

      ext <- tools::file_ext(input_name)
      if (ext == "") {
        ext <- tools::file_ext(input_path)
      }

      ext <- tolower(ext)

      message("EXTENSION DETECTED: ", ext)

      if (ext == "parquet") {
        message("USING read_parquet()")
        self$raw_data <- arrow::read_parquet(input_path) %>%
          as.data.frame()
      } else {
        message("USING fread()")
        self$raw_data <- fread(input = input_path)
      }
    },
    loading_parameters = function(input_path, self, print = FALSE) {
      parameters_list <- list.load(input_path)
      imap(parameters_list, ~ {self[[.y]] <- .x})
      invisible(self)
      if(print){return(parameters_list)}
    },
    load_annotation_file = function(path) {
      ext <- tolower(tools::file_ext(path))

      ann <- if (ext %in% c("xlsx", "xls")) {
        openxlsx::read.xlsx(path)
      } else {
        data.table::fread(path) %>% as.data.frame()
      }

      ann <- tibble::as_tibble(ann)

      if (ncol(ann) == 0) {
        stop("Annotation file is empty.")
      }

      self$annotation_data <- ann
      invisible(ann)
    },
    get_annotation_columns = function() {
      if (is.null(self$annotation_data)) return(character(0))
      colnames(self$annotation_data)
    },
    get_summary_data_by_groups = function(group_vars = NULL) {

      df <- self$filtered_data

      # fallback if nothing specified
      if (is.null(group_vars)) {
        group_vars <- intersect(c("PMethod", "FAIMS"), colnames(df))
      }

      df %>%
        dplyr::mutate(
          feature_names = ifelse(
            self$analysis_level == "peptide",
            peptide_names,
            gene_names
          )
        ) %>%
        dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
        dplyr::summarise(
          n_proteins = dplyr::n_distinct(feature_names),
          .groups = "drop"
        )
    },
    get_current_keys = function(intensity_type = NULL) {
      if (self$input_type == "DIA-NN") {
        return(unique(self$raw_data$Run))
      }

      if (self$identify_table_status == "success") {
        return(grep(intensity_type, colnames(self$raw_data), value = TRUE, ignore.case = FALSE))
      }

      character(0)
    },
    normalize_keys_for_matching = function(keys, mode = "auto") {
      if (mode == "exact") {
        return(keys)
      }

      base <- basename(keys)
      no_raw <- sub("\\.raw$", "", base, ignore.case = TRUE)

      if (mode == "basename") {
        return(no_raw)
      }

      if (mode == "extract_suffix") {
        return(sub("^.*?([A-Za-z0-9]+_col[0-9]+_[0-9]+)$", "\\1", no_raw))
      }

      # auto
      extracted <- sub("^.*?([A-Za-z0-9]+_col[0-9]+_[0-9]+)$", "\\1", no_raw)
      ifelse(extracted == no_raw, no_raw, extracted)
    },
    make_expdesign_from_annotation = function(intensity_type,
                                              sample_col,
                                              group_cols,
                                              match_mode = "auto") {
      if (is.null(self$annotation_data)) {
        stop("No annotation file loaded.")
      }

      if (!sample_col %in% colnames(self$annotation_data)) {
        stop("Selected sample ID column not found in annotation file.")
      }

      if (length(group_cols) == 0) {
        stop("Please select at least one grouping column.")
      }

      keys <- self$get_current_keys(intensity_type)

      if (length(keys) == 0) {
        stop("Could not determine matrix sample keys.")
      }

      ann <- self$annotation_data
      ann_ids <- ann[[sample_col]] %>% as.character()
      key_ids <- self$normalize_keys_for_matching(keys, mode = match_mode)

      exp_tbl <- tibble::tibble(
        key = keys,
        match_id = key_ids
      ) %>%
        dplyr::left_join(
          ann %>%
            dplyr::mutate(match_id = as.character(.data[[sample_col]])),
          by = "match_id"
        ) %>%
        dplyr::mutate(
          condition = apply(
            dplyr::select(., dplyr::all_of(group_cols)),
            1,
            function(x) paste(x[!is.na(x) & x != ""], collapse = "_")
          ),
          keep = TRUE
        ) %>%
        dplyr::mutate(
          condition = ifelse(condition == "", NA_character_, condition)
        ) %>%
        dplyr::select(keep, condition, key)

      exp_tbl <- exp_tbl %>%
        dplyr::group_by(condition) %>%
        dplyr::mutate(replicate = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(label = paste(condition, replicate, sep = "_"))

      self$annotation_sample_col <- sample_col
      self$annotation_group_cols <- group_cols
      self$annotation_match_mode <- match_mode
      self$expdesign <- exp_tbl

      invisible(exp_tbl)
    },
    table_raw_data = function() {
      w <- self$raw_data %>% colnames() %>% nchar() %>% max()
      config <- .get_raw_data_table_config()
      t <- self$raw_data %>%
        head(15) %>%
        .reactable_from_config(
          config,
          colDef(minWidth = w * 10)
        )
      return(t)
    },
    check_required_columns = function(data, required_columns) {
      missing_columns <- vector("list", length(required_columns))
      names(missing_columns) <- required_columns

      for (col in required_columns) {
        if (!col %in% colnames(data)) {
          missing_columns[[col]] <- paste("The column", col, "is missing.")
        } else {
          missing_columns[[col]] <- NULL
        }
      }

      missing_columns <- Filter(Negate(is.null), missing_columns)

      if (length(missing_columns) > 0) {
        return(list(status = FALSE, message = paste(unlist(missing_columns), collapse = "\n")))
      } else {
        return(list(status = TRUE, message = "All required columns are present."))
      }
    },
    check_intensity_columns = function(data, intensity_patterns) {
      intensity_regex <- paste(intensity_patterns, collapse = "|")

      if (any(str_detect(colnames(data), intensity_regex))) {
        return(list(status = TRUE, message = "Intensity columns are present."))
      } else {
        return(list(status = FALSE, message = "No Intensity columns found."))
      }
    },
    identify_table_type = function() {
      data <- self$raw_data
      required_columns_list <- inputs_type_lists$metadata_list
      intensity_patterns_list <- inputs_type_lists$intensity_list
      table_names <- names(required_columns_list)

      error_messages <- list()

      for (i in seq_along(required_columns_list)) {
        table_name <- table_names[i]
        required_columns <- required_columns_list[[i]]
        intensity_patterns <- intensity_patterns_list[[i]]

        # Check required columns
        required_columns_check <- self$check_required_columns(data, required_columns)

        # Check intensity columns
        intensity_columns_check <- self$check_intensity_columns(data, intensity_patterns)

        # If both checks pass, return the table type
        if (required_columns_check$status && intensity_columns_check$status) {
          self$identify_table_status <- "success"
          self$input_type <- table_name
          return(list(status = "success", message = paste("Table identified:", table_name)))
        } else {
          # Collect error messages
          error_messages[[table_name]] <- paste(
            "Table type", table_name, "check failed:",
            required_columns_check$message, intensity_columns_check$message, sep = "\n"
          )
        }
      }

      # If no table type matches, return the collected error messages
      self$identify_table_status <- "info"
      self$input_type <- "External"
      return(list(status = "info", messages = error_messages))
    },
    check_intensity_regex = function() {
      regex_vec <- flatten_chr(inputs_type_lists$intensity_list %>% keep_at(self$input_type))

      found_regex <- sapply(regex_vec, function(regex) {
        any(str_detect(colnames(self$raw_data), regex))
      })

      matching_regex <- regex_vec[found_regex]
      return(matching_regex)
    },
    create_summary_table = function() {

      data <- self$raw_data
      required_columns <- inputs_type_lists$metadata_list[[self$input_type]][1]
      intensity_patterns <- self$check_intensity_regex()

      num_rows <- nrow(data)

      non_unique_counts <- sapply(required_columns, function(col) {
        if (col %in% colnames(data)) {
          sum(duplicated(data[[col]]))
        } else {
          NA
        }
      })

      missing_values_counts <- sapply(intensity_patterns, function(pattern) {

        intensity_cols <- grep(pattern, colnames(data), value = TRUE)

        if (length(intensity_cols) == 0) return(NA_real_)

        values <- data[, intensity_cols, drop = FALSE]

        total_values <- length(unlist(values))
        missing_values <- sum(is.na(values) | values == 0 | values == "")

        round((missing_values / total_values) * 100, 1)
      })

      summary_table <- data.frame(
        Metric = c(
          "N° of Proteins",
          paste("N° of duplicate or missing", required_columns),
          paste("Missing data '%' in:", intensity_patterns)
        ),
        Value = c(num_rows, non_unique_counts, missing_values_counts)
      )

      summary_table <- summary_table[!is.na(summary_table$Value), ]
      rownames(summary_table) <- NULL

      reactable(summary_table)
    },
    make_expdesign = function(intensity_type) {

      if (!is.null(self$expdesign) && nrow(self$expdesign) > 0) {

        table <- self$expdesign %>%
          rhandsontable(width = "100%", stretchH = "all", height = 500) %>%
          hot_col("key", readOnly = TRUE)

        return(table)
      }

      if (self$input_type == "DIA-NN") {
        intensity_cols <- unique(self$raw_data$Run)
      } else if (self$identify_table_status == "success") {
        intensity_cols <- grep(intensity_type, colnames(self$raw_data), value = TRUE, ignore.case = FALSE)
      } else {
        if (is.null(intensity_type)) intensity_type <- ""
        intensity_cols <- intensity_type
      }

      table <- tibble("keep" = TRUE, "condition" = "", "key" = intensity_cols) %>%
        rhandsontable(width = "100%", stretchH = "all", height = 500) %>%
        hot_col("key", readOnly = TRUE)

      return(table)
    },
    validate_expdesign = function(data) {
      results <- list()
      validation_status <- TRUE

      if (any(is.na(data$condition)) || any(data$condition == "")) {
        results$condition_check <- "danger The 'condition' column contains missing or empty values."
        validation_status <- FALSE
      } else {
        # Controlla che la colonna "condition" contenga almeno 2 gruppi con almeno 3 componenti per gruppo
        condition_groups <- data %>%
          group_by(condition) %>%
          summarise(count = n())

        groups_with_3_or_more <- condition_groups %>%
          filter(count >= 3) %>%
          nrow()

        if (groups_with_3_or_more >= 2) {
          results$condition_check <- "success The 'condition' column contains at least 2 groups with at least 3 replicates each."
          results$condition_check2 <- "info All the statistics pages will be available."
          self$with_statistics <- TRUE
        } else {
          results$condition_check <- "warning The 'condition' column does not contains at least 2 groups with at least 3 replicates each."
          results$condition_check2 <- "info The statistics pages will NOT be available."
          self$with_statistics <- FALSE
        }

        groups_with_less_than_3 <- condition_groups %>%
          filter(count < 3)

        if (nrow(groups_with_less_than_3) > 0) {
          self$not_test_cond <- groups_with_less_than_3 %>% pull(condition)
          results$condition_warning <- paste("warning The following groups have less than 3 replicates:",
                                             paste(groups_with_less_than_3$condition, collapse = ", "))
        } else {
          self$not_test_cond <- NULL
        }
      }

      results$validation_status <- validation_status
      return(results)
    },
    add_replicate_and_label = function(data) {

      data <- data %>%
        mutate(keep = TRUE) %>%
        group_by(condition) %>%
        mutate(replicate = row_number()) %>%
        ungroup()

      table <- data %>%
        mutate(label = paste(condition, replicate, sep = "_")) %>%
        select(keep, condition, key, replicate, label)

      self$expdesign <- table

      config <- .get_imputed_table_config()
      rtable <- table %>%
        select(-key) %>%
        .reactable_from_config(
          config,
          colDef(align = "center")
        )

      return(rtable)
    },
    make_unique_genes = function(genes, protein_ids) {
      # Sostituire i valori mancanti o vuoti con il corrispondente valore di protein_id
      genes <- ifelse(genes == "" | is.na(genes), protein_ids, genes)
      # Rendere unici i nomi dei geni
      genes <- make.unique(genes, sep = "_")
      return(genes)
    },

    count_missed_cleavages = function(seq) {

      if (is.na(seq) || seq == "") return(NA_integer_)

      # remove modifications / symbols, keep amino-acid letters only
      seq <- gsub("[^A-Z]", "", seq)

      if (nchar(seq) < 2) return(0L)

      chars <- strsplit(seq, "")[[1]]

      mc <- 0L

      # internal positions only
      for (i in seq_len(length(chars) - 1)) {
        if (chars[i] %in% c("K", "R") && chars[i + 1] != "P") {
          mc <- mc + 1L
        }
      }

      return(mc)
    },

    define_colors = function() {
      n_of_color <- max(self$expdesign %>% distinct(condition) %>% nrow())
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
    },
    define_tests = function() {
      conditions <- unique(self$expdesign$condition)
      if(!is.null(self$not_test_cond)) {
        conditions <- conditions[! conditions %in% self$not_test_cond]
      }
      self$all_test_combination <-
        expand_grid(cond1 = conditions, cond2 = conditions) %>%
        filter(cond1 != cond2) %>%
        mutate(test = paste0(cond1, "_vs_", cond2)) %>%
        pull(test)
    },
    get_feature_label = function() {
      if (self$analysis_level == "peptide") {
        return("Peptide")
      } else {
        return("Protein")
      }},
      get_counts_by_condition = function() {

        if (is.null(self$filtered_data) || is.null(self$expdesign)) {
          return(NULL)
        }

        counts_by_replicate <- self$filtered_data %>%
          filter(bin_intensity == 1) %>%
          group_by(label) %>%
          summarise(
            counts = n_distinct(!!sym(self$feature_id_col)),
            .groups = "drop"
          ) %>%
          inner_join(self$expdesign, by = "label")

        counts_summary <- counts_by_replicate %>%
          group_by(condition) %>%
          summarise(
            mean_count = round(mean(counts, na.rm = TRUE), 1),
            sd_count   = round(sd(counts, na.rm = TRUE), 1),
            n_reps     = dplyr::n(),
            summary    = paste0(mean_count, " +/- ", ifelse(is.na(sd_count), 0, sd_count)),
            .groups = "drop"
          ) %>%
          rename(
            Condition = condition,
            Replicates = n_reps,
            Mean = mean_count,
            SD = sd_count,
            Mean_SD = summary
          )




        reactable(counts_summary)
      },
    apply_sample_exclusion = function() {
      
      # ====== DEFENSIVE CHECK: Verify normalized_data exists ======
      if (is.null(self$normalized_data) || nrow(self$normalized_data) == 0) {
        warning("normalized_data is NULL or empty. Cannot apply sample exclusion.")
        self$excluded_samples <- data.frame()
        self$exclusion_summary <- list(
          total_samples = 0,
          excluded_count = 0,
          remaining_samples = nrow(self$expdesign),
          sample_table = data.frame()
        )
        return(self$exclusion_summary)
      }
      
      # Return early if no exclusion criteria are set
      has_protein_threshold <- !is.null(self$excl_protein_threshold) && self$excl_protein_threshold > 0
      has_cv_threshold <- !is.null(self$excl_cv_threshold) && self$excl_cv_threshold > 0
      has_mc_threshold <- !is.null(self$excl_mc_threshold) && self$excl_mc_threshold > 0
      
      if (!has_protein_threshold && !has_cv_threshold && !has_mc_threshold) {
        # No exclusion - return summary of all samples
        self$excluded_samples <- data.frame()
        self$exclusion_summary <- list(
          total_samples = nrow(self$expdesign),
          excluded_count = 0,
          remaining_samples = nrow(self$expdesign),
          sample_table = data.frame()
        )
        return(self$exclusion_summary)
      }
      
      # Initialize tracking dataframe (WITHOUT pre-creating metric columns to avoid join conflicts)
      sample_metrics <- self$expdesign %>%
        mutate(
          exclusion_reason = NA_character_,
          excluded = FALSE
        )
      
      # ====== CRITERION 1: Protein/Feature Count Threshold ======
      if (has_protein_threshold) {
        
        # Count features per sample (WITH error handling)
        count_by_sample <- tryCatch({
          self$normalized_data %>%
            group_by(label) %>%
            summarise(
              count = n_distinct(feature_names),
              .groups = "drop"
            )
        }, error = function(e) {
          warning(sprintf("Error computing count_by_sample: %s", e$message))
          return(data.frame(label = character(), count = integer()))
        })
        
        # Only join if count_by_sample is not empty
        if (nrow(count_by_sample) > 0) {
          sample_metrics <- sample_metrics %>%
            left_join(count_by_sample, by = "label")
        } else {
          # Add empty count column to maintain structure
          sample_metrics <- sample_metrics %>%
            mutate(count = NA_integer_)
        }
        
        # Mark samples below threshold (with NA safety check)
        if ("count" %in% colnames(sample_metrics)) {
          below_threshold <- !is.na(sample_metrics$count) & 
            sample_metrics$count < self$excl_protein_threshold
          
          sample_metrics$excluded[below_threshold] <- TRUE
          sample_metrics$exclusion_reason[below_threshold] <- 
            paste0("Count (", sample_metrics$count[below_threshold], ") < ", self$excl_protein_threshold)
        }
      } else {
        # Add empty count column if not computed
        sample_metrics <- sample_metrics %>%
          mutate(count = NA_integer_)
      }
      
      # ====== CRITERION 2: CV Threshold ======
      if (has_cv_threshold) {
        
        # Compute CV for each sample (WITH error handling)
        cv_by_sample <- tryCatch({
          self$normalized_data %>%
            filter(!is.na(intensity)) %>%
            group_by(label) %>%
            summarise(
              cv = sd(intensity, na.rm = TRUE) / abs(mean(intensity, na.rm = TRUE)),
              .groups = "drop"
            ) %>%
            mutate(cv = replace(cv, is.infinite(cv) | is.nan(cv), NA_real_))
        }, error = function(e) {
          warning(sprintf("Error computing cv_by_sample: %s", e$message))
          return(data.frame(label = character(), cv = numeric()))
        })
        
        # Only join if cv_by_sample is not empty
        if (nrow(cv_by_sample) > 0) {
          sample_metrics <- sample_metrics %>%
            left_join(cv_by_sample, by = "label")
        } else {
          # Add empty cv column to maintain structure
          sample_metrics <- sample_metrics %>%
            mutate(cv = NA_real_)
        }
        
        # Mark samples with high CV (with NA safety check)
        if ("cv" %in% colnames(sample_metrics)) {
          high_cv <- !is.na(sample_metrics$cv) & 
            sample_metrics$cv > self$excl_cv_threshold
          
          sample_metrics$excluded[high_cv] <- TRUE
          
          existing_reason <- sample_metrics$exclusion_reason[high_cv]
          new_reason <- paste0("CV (", round(sample_metrics$cv[high_cv], 4), ") > ", self$excl_cv_threshold)
          sample_metrics$exclusion_reason[high_cv] <- 
            ifelse(
              is.na(existing_reason),
              new_reason,
              paste(existing_reason, new_reason, sep = "; ")
            )
        }
      } else {
        # Add empty cv column if not computed
        sample_metrics <- sample_metrics %>%
          mutate(cv = NA_real_)
      }
      
      # ====== CRITERION 3: Missed Cleavages Percentage ======
      if (has_mc_threshold) {
        
        # Try to find missed cleavages column in raw data
        mc_col <- NULL
        possible_cols <- c("missed_cleavages_pct", "Missed.Cleavages", "missed_cleavages", 
                           "Missed.Cleavages.count", "MissedCleavages")
        
        for (col_name in possible_cols) {
          if (col_name %in% colnames(self$raw_data)) {
            mc_col <- col_name
            break
          }
        }
        
        if (!is.null(mc_col) && !is.null(self$raw_data) && nrow(self$raw_data) > 0) {
          # Compute mean MC percentage per sample (WITH error handling)
          mc_by_sample <- tryCatch({
            self$raw_data %>%
              group_by(across(all_of(self$expdesign$key[1]))) %>%
              summarise(
                mc_percent = mean(as.numeric(.data[[mc_col]]), na.rm = TRUE) * 
                  (if (max(as.numeric(.data[[mc_col]]), na.rm = TRUE) <= 1) 100 else 1),
                .groups = "drop"
              )
          }, error = function(e) {
            warning(sprintf("Error computing mc_by_sample: %s", e$message))
            return(data.frame(key = character(), mc_percent = numeric()))
          })
          
          if (nrow(mc_by_sample) > 0) {
            # Match to expdesign
            colnames(mc_by_sample)[1] <- "key"
            
            sample_metrics <- sample_metrics %>%
              left_join(mc_by_sample, by = "key")
            
            # Mark samples with high MC (with NA safety check)
            if ("mc_percent" %in% colnames(sample_metrics)) {
              high_mc <- !is.na(sample_metrics$mc_percent) & 
                sample_metrics$mc_percent > self$excl_mc_threshold
              
              sample_metrics$excluded[high_mc] <- TRUE
              
              existing_reason <- sample_metrics$exclusion_reason[high_mc]
              new_reason <- paste0("MC% (", round(sample_metrics$mc_percent[high_mc], 2), ") > ", self$excl_mc_threshold)
              sample_metrics$exclusion_reason[high_mc] <- 
                ifelse(
                  is.na(existing_reason),
                  new_reason,
                  paste(existing_reason, new_reason, sep = "; ")
                )
            }
          } else {
            sample_metrics <- sample_metrics %>%
              mutate(mc_percent = NA_real_)
          }
        } else {
          sample_metrics <- sample_metrics %>%
            mutate(mc_percent = NA_real_)
        }
      } else {
        # Add empty mc_percent column if not computed
        sample_metrics <- sample_metrics %>%
          mutate(mc_percent = NA_real_)
      }
      
      # ====== FINAL SAFETY CHECK: Ensure all required columns exist ======
      required_cols <- c("label", "condition", "replicate", "key", "count", "cv", "mc_percent", "exclusion_reason")
      missing_cols <- setdiff(required_cols, colnames(sample_metrics))
      
      if (length(missing_cols) > 0) {
        warning(sprintf("Missing columns in sample_metrics: %s. Adding as NA.", paste(missing_cols, collapse = ", ")))
        
        # Add any missing columns as NA
        for (col in missing_cols) {
          if (col %in% c("count")) {
            sample_metrics[[col]] <- NA_integer_
          } else if (col %in% c("cv", "mc_percent")) {
            sample_metrics[[col]] <- NA_real_
          } else {
            sample_metrics[[col]] <- NA_character_
          }
        }
      }
      
      # ====== APPLY EXCLUSIONS ======
      
      # Store excluded samples with their metrics and reasons
      self$excluded_samples <- sample_metrics %>%
        filter(excluded) %>%
        select(all_of(required_cols))
      
      # Get indices of samples to keep
      remaining_indices <- which(!sample_metrics$excluded)
      remaining_labels <- sample_metrics$label[remaining_indices]
      
      # Filter normalized_data to only keep remaining samples
      if (!is.null(self$filtered_data) && nrow(self$filtered_data) > 0) {
        self$filtered_data <- self$filtered_data %>%
          filter(label %in% remaining_labels)
      }
      
      if (!is.null(self$normalized_data) && nrow(self$normalized_data) > 0) {
        self$normalized_data <- self$normalized_data %>%
          filter(label %in% remaining_labels)
      }
      
      self$expdesign <- sample_metrics[remaining_indices, ] %>%
        select(-all_of(c("count", "cv", "mc_percent", "exclusion_reason", "excluded")))
      
      # Store summary
      self$exclusion_summary <- list(
        total_samples = nrow(sample_metrics),
        excluded_count = nrow(self$excluded_samples),
        remaining_samples = nrow(self$expdesign),
        sample_table = sample_metrics
      )
      
      if (nrow(self$expdesign) == 0) {
        warning("No samples remaining after exclusion criteria applied!")
      }
      
      invisible(self$exclusion_summary)
    },
    preprocessing = function() {

      intensity_cols <- self$expdesign$key
      self$define_colors()
      self$define_tests()

      if (self$input_type == "DIA-NN") {
        initial_table <- self$raw_data

        if (self$log_transform) {
          initial_table <- initial_table %>%
            mutate(`Fragment.Sum` = log2(`Fragment.Sum`)) %>%
            mutate(`Fragment.Sum` = na_if(`Fragment.Sum`, -Inf))
        }

      } else if (self$log_transform) {
        initial_table <- self$raw_data %>%
          mutate(across(all_of(intensity_cols), log2)) %>%
          mutate(across(all_of(intensity_cols), ~ na_if(., -Inf)))
      } else {
        initial_table <- self$raw_data
      }

      if (self$input_type == "External") {
        required_columns <- self$external_genes_column
        gene_col <- self$external_genes_column

        initial_table <- initial_table %>%
          rename(gene_names := !!gene_col) %>%
          filter(gene_names != "") %>%
          drop_na(gene_names) %>%
          mutate(gene_names = str_extract(gene_names, "[^;]*")) %>%
          mutate(gene_names = make.unique(gene_names, sep = "_"))

        if ("Stripped.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Stripped.Sequence"]])
        } else if ("Modified.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Modified.Sequence"]])
        } else {
          initial_table <- initial_table %>% mutate(peptide_names = NA_character_)
        }

        initial_table <- initial_table %>%
          mutate(
            missed_cleavages = vapply(
              peptide_names,
              self$count_missed_cleavages,
              integer(1)
            )
          )

        if (self$analysis_level == "peptide" && any(!is.na(initial_table$peptide_names))) {
          initial_table <- initial_table %>% mutate(feature_names = peptide_names)
          self$feature_id_col <- "peptide_names"
        } else {
          initial_table <- initial_table %>% mutate(feature_names = gene_names)
          self$feature_id_col <- "gene_names"
        }

      } else if (self$input_type == "ProteomeDiscoverer") {
        org_map <- inputs_type_lists$org_map
        org_info <- org_map[[self$organism]]
        if (is.null(org_info)) return(NULL)
        orgdb <- org_info$orgdb

        g_names <- tryCatch({
          bitr(
            initial_table$Accession,
            fromType = "UNIPROT",
            toType = "SYMBOL",
            OrgDb = orgdb
          ) %>%
            dplyr::rename(Accession = UNIPROT)
        }, error = function(e) {
          shiny::showNotification("Organism selected non compatible with Uniprot ID.", type = "error")
          NULL
        })
        if (is.null(g_names)) return(NULL)

        gene_col <- "SYMBOL"
        protein_col <- "Accession"
        required_columns <- c(gene_col, inputs_type_lists$metadata_list[[self$input_type]])

        initial_table <- initial_table %>%
          left_join(g_names, by = "Accession") %>%
          mutate(!!gene_col := self$make_unique_genes(.data[[gene_col]], .data[[protein_col]])) %>%
          rename(gene_names := !!gene_col)

        if ("Stripped.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Stripped.Sequence"]])
        } else if ("Modified.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Modified.Sequence"]])
        } else {
          initial_table <- initial_table %>% mutate(peptide_names = NA_character_)
        }

        if (self$analysis_level == "peptide" && any(!is.na(initial_table$peptide_names))) {
          initial_table <- initial_table %>% mutate(feature_names = peptide_names)
          self$feature_id_col <- "peptide_names"
        } else {
          initial_table <- initial_table %>% mutate(feature_names = gene_names)
          self$feature_id_col <- "gene_names"
        }

      } else if (self$input_type == "AlphaPept") {
        gene_col <- "gene_symbol"
        protein_col <- "uniprot_id"
        required_columns <- gene_col

        initial_table <- initial_table %>%
          separate(
            V1,
            into = c("db", "uniprot_id", "gene"),
            sep = "\\|",
            fill = "right",
            extra = "merge"
          ) %>%
          mutate(gene_symbol = sub("_.*$", "", gene)) %>%
          filter(!stringr::str_detect(db, "REV_")) %>%
          mutate(!!gene_col := self$make_unique_genes(.data[[gene_col]], .data[[protein_col]])) %>%
          rename(gene_names := !!gene_col) %>%
          mutate(peptide_names = NA_character_)

        initial_table <- initial_table %>% mutate(feature_names = gene_names)
        self$feature_id_col <- "gene_names"


      }  else if (self$input_type == "DIA-NN") {
        required_columns <- c("Run", "Protein", "Sequence", "Precursor.Id", "Fragment.Sum")

        initial_table <- initial_table %>%
          rename(
            gene_names = Protein,
            key = Run,
            intensity = `Fragment.Sum`
          ) %>%
          filter(!is.na(key), key != "") %>%
          filter(!is.na(intensity))

        if ("Stripped.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>%
            mutate(peptide_names = .data[["Stripped.Sequence"]])
        } else if ("Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>%
            mutate(peptide_names = .data[["Sequence"]])
        } else if ("Precursor.Id" %in% colnames(initial_table)) {
          initial_table <- initial_table %>%
            mutate(peptide_names = .data[["Precursor.Id"]])
        } else {
          initial_table <- initial_table %>%
            mutate(peptide_names = NA_character_)
        }

        initial_table <- initial_table %>%
          mutate(
            missed_cleavages = vapply(
              peptide_names,
              self$count_missed_cleavages,
              integer(1)
            )
          )

        if (self$analysis_level == "peptide" && any(!is.na(initial_table$peptide_names))) {
          initial_table <- initial_table %>% mutate(feature_names = peptide_names)
          self$feature_id_col <- "peptide_names"
        } else {
          initial_table <- initial_table %>% mutate(feature_names = gene_names)
          self$feature_id_col <- "gene_names"
        }

        self$raw_data_unique <- initial_table

        data <- initial_table %>%
          left_join(self$expdesign, by = "key") %>%
          mutate(bin_intensity = if_else(is.na(intensity), 0, 1)) %>%
          mutate(imputed = FALSE)

        self$data <- data
        return(invisible(NULL))
      } else {
        required_columns <- inputs_type_lists$metadata_list[[self$input_type]]
        gene_col <- required_columns[1]
        protein_col <- required_columns[2]

        initial_table <- initial_table %>%
          mutate(!!gene_col := str_extract(.data[[gene_col]], "[^;]*")) %>%
          mutate(!!protein_col := str_extract(.data[[protein_col]], "[^;]*")) %>%
          mutate(!!gene_col := self$make_unique_genes(.data[[gene_col]], .data[[protein_col]])) %>%
          rename(gene_names := !!gene_col)

        if ("Stripped.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Stripped.Sequence"]])
        } else if ("Modified.Sequence" %in% colnames(initial_table)) {
          initial_table <- initial_table %>% mutate(peptide_names = .data[["Modified.Sequence"]])
        } else {
          initial_table <- initial_table %>% mutate(peptide_names = NA_character_)
        }

        if (self$analysis_level == "peptide" && any(!is.na(initial_table$peptide_names))) {
          initial_table <- initial_table %>% mutate(feature_names = peptide_names)
          self$feature_id_col <- "peptide_names"
        } else {
          initial_table <- initial_table %>% mutate(feature_names = gene_names)
          self$feature_id_col <- "gene_names"}}



      self$raw_data_unique <- initial_table
      optional_cols <- intersect(c("peptide_names"), colnames(initial_table))
      required_keep <- intersect(required_columns, colnames(initial_table))

      data <- initial_table %>%
        select(all_of(c("gene_names", "feature_names","missed_cleavages", intensity_cols, optional_cols, required_keep))) %>%
        pivot_longer(cols = all_of(intensity_cols), names_to = "key", values_to = "intensity") %>%
        left_join(self$expdesign, by = "key") %>%
        mutate(bin_intensity = if_else(is.na(intensity), 0, 1)) %>%
        mutate(imputed = FALSE)

      self$data <- data
    },
    subset_missing_data = function(valid_val_filter, valid_val_thr) {

      ## Different strategies for filtering missing data:
      ## c("alog", "each_grp", "total")
      ## alog -> at least one group

      data <- self$data

      filtered_data <- data %>%
        {if(valid_val_filter == "total")
          group_by(., feature_names)
          else
            group_by(., feature_names, condition)
        } %>%
        mutate(
          miss_val = n() - sum(bin_intensity, na.rm = TRUE),
          n_size = n()
        ) %>%
        ungroup() %>%
        group_by(feature_names) %>%
        ## Range compreso tra 0 e 100% espresso in valori tra 0 e 1
        {if(valid_val_filter == "alog")
          filter(., any(miss_val <= round(n_size * (1 - valid_val_thr), 0)))
          else
            filter(., all(miss_val <= round(n_size * (1 - valid_val_thr), 0)))
        } %>%
        ungroup() %>%
        select(-c(miss_val, n_size))

      self$filtered_data <- filtered_data
    },
    subset_peptides = function(pep_filter, pep_thr) {

      data <- self$filtered_data

      # Only apply in protein mode
      if (!(self$analysis_level == "protein" &&
            self$input_type %in% c("MaxQuant", "ProteomeDiscoverer"))) {
        return(invisible(NULL))
      }

      # Example logic (adapt to your actual peptide column)
      if ("Peptides" %in% colnames(data)) {

        if (pep_filter == "peptides") {
          filtered_data <- data %>%
            filter(Peptides >= pep_thr)
        } else {
          filtered_data <- data
        }

      } else {
        # No peptide count column → do nothing
        filtered_data <- data
      }

      self$filtered_data <- filtered_data
    },
    subset_contaminant = function(rev, cont, oibs, rescue_cont = NULL) {

      data <- self$filtered_data

      if(self$input_type == "MaxQuant") {
        cleaned_data <- data %>%
          # Mutate potential_contaminant based on gene_names and rescue_cont
          mutate(`Potential contaminant` = case_when(
            gene_names %in% rescue_cont ~ "",
            TRUE ~ `Potential contaminant`
          )) %>%
          # Remove reverse, potential contaminant and only identified by site based on user input
          {if (rev) filter(., `Reverse` != "+") else .} %>%
          {if (cont) filter(., `Potential contaminant` != "+") else .} %>%
          {if (oibs) filter(., `Only identified by site` != "+") else .}
      } else {
        cleaned_data <- data %>%
          filter(!gene_names %in% contaminants$contaminant_list)
      }
      self$filtered_data <- cleaned_data
    },
    normalization = function(norm_methods) {

      data <- self$filtered_data

      if (norm_methods == "None" | nrow(distinct(data, feature_names)) < 42) {
        self$is_norm <- FALSE
        self$normalized_data <- data
      } else {
        self$is_norm <- TRUE

        raw_matrix <- data %>%
          pivot_wider(id_cols = feature_names, names_from = label, values_from = intensity) %>%
          column_to_rownames("feature_names") %>%
          as.matrix()

        set.seed(11)
        vsn_fit <- vsn2(2 ^ raw_matrix, verbose = FALSE)
        norm_matrix <- predict(vsn_fit, 2 ^ raw_matrix)

        normalized_data <- norm_matrix %>%
          as_tibble(rownames = "feature_names") %>%
          pivot_longer(cols = -feature_names, names_to = "label", values_to = "norm_intensity") %>%
          full_join(data, by = c("feature_names", "label")) %>%
          mutate(intensity = norm_intensity) %>%
          select(-norm_intensity) %>%
          relocate(intensity, .after = last_col())

        self$normalized_data <- normalized_data
      }
    },
    imputation = function(imp_methods, shift, scale, unique_visual = FALSE) {
      
      # ====== INITIALIZATION (MOVED TO TOP) ======
      # Initialize data from normalized_data FIRST (not buried in conditionals)
      data <- self$normalized_data %>%
        group_by(feature_names) %>%
        mutate(
          bin_intensity_pct = mean(bin_intensity, na.rm = TRUE),
          imputed = if_else(bin_intensity == 1, FALSE, TRUE)
        ) %>%
        ungroup()
      
      # Store the method used
      self$imp_methods_used <- imp_methods
      
      # ====== BRANCH ON IMPUTATION METHOD ======
      # Check if this is "No imputation"
      no_imputation_selected <- imp_methods %in% c("none", "No imputation", "")
      
      if (no_imputation_selected) {
        # ====== NO IMPUTATION ======
        self$is_imp <- FALSE
        self$is_mixed <- FALSE
        self$imputed_data <- data %>%
          mutate(imputed = FALSE)  # Mark all as not imputed
        
      } else if (imp_methods %in% c("mixed", "perseus")) {
        # ====== MIXED / PERSEUS IMPUTATION ======
        self$is_mixed <- imp_methods == "mixed"
        self$is_imp   <- TRUE
        set.seed(11)
        
        if (self$is_mixed) {
          # ---- MIXED IMPUTATION ----
          data <- data %>%
            mutate(
              for_mean_imp = bin_intensity == 0 & (bin_intensity_pct >= self$mar_mnar_thresh)
            )
          
          # Generate random imputation values
          data <- data %>%
            group_by(label) %>%
            mutate(
              mean = mean(intensity, na.rm = TRUE),
              sd   = sd(intensity, na.rm = TRUE),
              random_imp = rnorm(n(), mean = mean - (shift * sd), sd = scale * sd)
            ) %>%
            ungroup()
          
          # Apply imputation
          data <- data %>%
            mutate(
              imp_intensity = case_when(
                bin_intensity == 0 & for_mean_imp ~ random_imp,
                TRUE ~ as.numeric(intensity)
              )
            ) %>%
            mutate(intensity = imp_intensity) %>%
            select(-c(for_mean_imp, random_imp, imp_intensity, mean, sd))
          
          self$imputed_data <- data
          
        } else {
          # ---- PERSEUS-STYLE RANDOM NORMAL IMPUTATION ----
          set.seed(11)
          self$imputed_data <- data %>%
            group_by(label) %>%
            mutate(
              mean = mean(intensity, na.rm = TRUE),
              sd   = sd(intensity, na.rm = TRUE),
              n    = sum(!is.na(intensity)),
              total = nrow(data) - n
            ) %>%
            ungroup() %>%
            mutate(
              imp_intensity = case_when(
                bin_intensity == 0 ~ rnorm(n(), mean = mean - (shift * sd), sd = scale * sd),
                TRUE ~ intensity
              )
            ) %>%
            mutate(intensity = imp_intensity) %>%
            select(-c(mean, sd, n, total, imp_intensity))
        }
        
      } else if (imp_methods == "KNN") {
        # ====== KNN IMPUTATION ======
        self$is_imp <- TRUE
        
        # KNN imputation using existing logic
        # [PASTE YOUR EXISTING KNN IMPUTATION CODE HERE]
        # ...
        
      } else if (imp_methods == "missForest") {
        # ====== MISSFOREST IMPUTATION ======
        self$is_imp <- TRUE
        
        # missForest imputation using existing logic
        # [PASTE YOUR EXISTING MISSFOREST IMPUTATION CODE HERE]
        # ...
        
      }
      
      # ====== VALIDATION CHECKS ======
      if (!is.null(self$imputed_data) && nrow(self$imputed_data) > 0) {
        
        # Check row count consistency
        if (nrow(self$imputed_data) != nrow(self$normalized_data)) {
          warning(
            sprintf(
              "Row count mismatch: imputed_data has %d rows, normalized_data has %d rows",
              nrow(self$imputed_data),
              nrow(self$normalized_data)
            )
          )
        }
        
        # Check that intensity column exists and has proper type
        if (!"intensity" %in% names(self$imputed_data)) {
          stop("imputed_data is missing 'intensity' column after imputation")
        }
      }
      
      invisible(self)
    },
    rank_protein = function(target, by_condition, selection, n_perc) {
      if (by_condition) {
        data <- self$imputed_data %>%
          filter(condition == target) %>%
          group_by(feature_names) %>%
          summarise(mean_intensity = mean(intensity), .groups = "drop") %>%
          arrange(-mean_intensity) %>%
          mutate(rank = rank(-mean_intensity)) %>%
          rename(intensity = mean_intensity)
      } else {
        data <- self$imputed_data %>%
          filter(label == target) %>%
          select(feature_names, intensity) %>%
          arrange(-intensity) %>%
          mutate(rank = rank(-intensity))
      }

      if (selection == "top") {
        selected_list <- data %>% slice_head(prop = n_perc) %>% pull(feature_names)
      } else {
        selected_list <- data %>% slice_tail(prop = n_perc) %>% pull(feature_names)
      }

      self$rank_data <- data %>%
        mutate(highlighted = feature_names %in% selected_list)

      self$protein_rank_list <- selected_list
    },
    shiny_wrap_workflow = function() {
      self$preprocessing()
      
      self$subset_missing_data(
        valid_val_filter = self$valid_val_filter,
        valid_val_thr = self$valid_val_thr
      )
      
      if (self$analysis_level == "protein" &&
          self$input_type %in% c("MaxQuant", "ProteomeDiscoverer")) {
        self$subset_peptides(
          pep_filter = self$pep_filter,
          pep_thr = self$pep_thr
        )
      }
      
      self$subset_contaminant(
        rev = self$rev,
        cont = self$cont,
        oibs = self$oibs
      )
      
      self$normalization(norm_methods = self$norm_methods)
      
      # Sample exclusion AFTER normalization
      if (
        (!is.null(self$excl_protein_threshold) && self$excl_protein_threshold > 0) ||
        (!is.null(self$excl_cv_threshold) && self$excl_cv_threshold > 0) ||
        (!is.null(self$excl_mc_threshold) && self$excl_mc_threshold > 0)
      ) {
        self$apply_sample_exclusion()
      } else {
        self$excluded_samples <- NULL
        self$exclusion_summary <- NULL
      }
      
      self$imputation(
        imp_methods = self$imp_methods,
        shift = self$imp_shift,
        scale = self$imp_scale,
        unique_visual = FALSE
      )
       
      self$rank_protein(
        target = self$protein_rank_target,
        by_condition = self$protein_rank_by_cond,
        selection = self$protein_rank_selection,
        n_perc = self$protein_rank_top_n
      )
      
      self$filtered_gene_vector <- self$filtered_data %>%
        distinct(feature_names) %>%
        pull(feature_names)
    },
    reactable_interactive = function(table, sel = "multiple") {
      if(is.null(table)){return(NULL)}
      t <- table %>%
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          wrap = FALSE,
          height = "auto",
          selection = sel,
          paginationType = "simple",
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(6, 12, 18, 24),
          defaultPageSize = 12,
          onClick = "select",
          defaultColDef = colDef(align = "center", minWidth = 200),
          columns = list(gene_names = colDef(
            name = "Gene names",
            sticky = "left",
            style = list(borderRight  = "1px solid #eee")
          ))
        )
      return(t)
    },
    plot_empty_message = function(message) {
      e_charts(data.frame(x = "", y = ""), x, renderer = self$plot_format) %>%
        e_bar(y) %>%
        e_legend(show = FALSE) %>%
        e_draft(
          text = message,
          size = "2rem",
          opacity = 1,
          color = "#555"
        ) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)
    },
    plot_missed_cleavages = function() {

      if (is.null(self$raw_data_unique)) {
        return(NULL)
      }

      if (!"missed_cleavages" %in% colnames(self$raw_data_unique)) {
        return(self$plot_empty_message("No missed cleavages available"))
      }

      intensity_cols <- self$expdesign$key

      data_mc <- self$raw_data_unique %>%
        select(any_of(c("feature_names", "peptide_names", "gene_names", "missed_cleavages", intensity_cols))) %>%
        pivot_longer(
          cols = all_of(intensity_cols),
          names_to = "key",
          values_to = "intensity"
        ) %>%
        filter(!is.na(intensity)) %>%
        left_join(self$expdesign, by = "key")

      mc_summary <- data_mc %>%
        filter(!is.na(missed_cleavages)) %>%
        group_by(condition, missed_cleavages) %>%
        summarise(count = n(), .groups = "drop_last") %>%
        mutate(
          total = sum(count),
          percentage = round((count / total) * 100, 2),
          missed_cleavages = factor(missed_cleavages,
                                    levels = c("0", "1", "2", "non-tryptic"),
                                    ordered = TRUE)
        ) %>%
        ungroup()

      p <- mc_summary %>%
        e_charts(condition, renderer = self$plot_format) %>%
        e_bar(
          percentage,
          stack = "grp",
          colorBy = "data"
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_y_axis(
          name = "Missed cleavages (%)",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(
          name = "Condition",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_protein_counts = function(group_vars = NULL) {
      
      if (is.null(self$filtered_data) || nrow(self$filtered_data) == 0) {
        return(self$plot_empty_message("No data to plot yet.<br>Press PROCESS button."))
      }
      
      # Safe feature ID
      fid <- self$feature_id_col
      if (is.null(fid) || !fid %in% colnames(self$filtered_data)) {
        fid <- "feature_names"
        if (!"feature_names" %in% colnames(self$filtered_data)) {
          fid <- colnames(self$filtered_data)[1]
        }
      }
      
      # Use annotation_group_cols if available, else fallback
      if (is.null(group_vars) || length(group_vars) == 0) {
        group_vars <- self$annotation_group_cols %||% c("condition")
      }
      group_vars <- intersect(as.character(group_vars), colnames(self$filtered_data))
      
      if (length(group_vars) == 0) {
        group_vars <- "condition"
      }
      
      # Build summary table
      df_plot <- self$filtered_data %>%
        filter(bin_intensity == 1) %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(count = n_distinct(!!sym(fid)), .groups = "drop")
      
      # === CRITICAL FIX: Rename columns to safe names for echarts4r ===
      colnames(df_plot)[1] <- "x_axis"   # First column becomes x_axis
      
      # If we have 2 grouping columns, use the second for color
      if (ncol(df_plot) >= 3) {
        color_var <- colnames(df_plot)[2]
        colnames(df_plot)[2] <- "group_color"
        
        p <- df_plot %>%
          e_charts(x_axis) %>%
          e_bar(count, bind = group_color) %>%
          e_color(self$color_palette) %>%
          e_x_axis(name = group_vars[1], axisLabel = list(rotate = 45, fontSize = 11)) %>%
          e_y_axis(name = "Protein Count") %>%
          e_legend(textStyle = list(fontSize = 12)) %>%
          e_tooltip() %>%
          e_grid(containLabel = TRUE)
      } else {
        p <- df_plot %>%
          e_charts(x_axis) %>%
          e_bar(count) %>%
          e_color(self$primary_color) %>%
          e_x_axis(name = group_vars[1], axisLabel = list(rotate = 45, fontSize = 11)) %>%
          e_y_axis(name = "Protein Count") %>%
          e_tooltip() %>%
          e_grid(containLabel = TRUE)
      }
      
      return(p)
    },
    plot_protein_coverage = function() {

      if(is.null(self$filtered_data)){return(NULL)}

      p <- self$filtered_data %>%
        group_by(feature_names) %>%
        summarise(counts = sum(bin_intensity)) %>%
        ungroup() %>%
        select(counts) %>%
        table() %>%
        as_tibble() %>%
        rename(occurrence = n) %>%
        e_charts(counts, renderer = self$plot_format) %>%
        e_bar(occurrence) %>%
        e_tooltip(trigger = "item") %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$primary_color) %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(axisLabel = list(fontSize = self$plot_font_size)) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_protein_coverage_intersections = function() {

      if(is.null(self$filtered_data)){return(NULL)}
      upset_df <- self$filtered_data %>%
        group_by(label) %>%
        summarise(genes = list(bin_intensity)) %>%
        deframe() %>%
        as.data.frame()
      if(ncol(upset_df) < 2){return(NULL)}

      p <- upset(
        upset_df,
        nsets = ncol(upset_df),
        order.by = "freq",
        main.bar.color = self$primary_color,
        text.scale = 1.5
      )
      return(p)
    },
    plot_cv = function() {

      if(is.null(self$normalized_data)){return(NULL)}

      condition_groups <- self$expdesign %>%
        group_by(condition) %>%
        summarise(count = n()) %>%
        filter(count >= 2) %>%
        nrow()
      if(condition_groups<1){return(self$plot_empty_message("not enough replicates to compute CV"))}

      p <- self$normalized_data %>%
        group_by(feature_names, condition) %>%
        summarise(
          mean = mean(if (self$log_transform) 2^intensity else intensity, na.rm = TRUE),
          sd = sd(if (self$log_transform) 2^intensity else intensity, na.rm = TRUE),
          CV = round(sd / mean, 3)
        ) %>%
        ungroup() %>%
        group_by(condition) %>%
        e_charts(renderer = self$plot_format) %>%
        e_boxplot(
          CV,
          colorBy = "data",
          outliers = FALSE,
          itemStyle = list(borderWidth = 3)
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(show = FALSE) %>%
        e_y_axis(
          name = "Coefficient of variation",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(axisLabel = list(fontSize = self$plot_font_size)) %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_missing_data = function() {

      if(is.null(self$filtered_data)){return(NULL)}

      p <- self$filtered_data %>%
        group_by(label) %>%
        mutate(bin_intensity = if_else(bin_intensity == 1, "Valid", "Missing")) %>%
        count(bin_intensity) %>%
        pivot_wider(id_cols = label, names_from = bin_intensity, values_from = n) %>%
        {if(ncol(.) == 2) mutate(., Missing = 0)else . } %>%
        ungroup() %>%
        mutate(total = Valid + Missing) %>%
        mutate(perc_present = paste0(round(Valid*100/total, 1), "%")) %>%
        mutate(perc_missing = paste0(round(Missing*100/total, 1), "%")) %>%
        e_charts(label, renderer = self$plot_format) %>%
        e_bar(Valid, stack = "grp", bind = perc_present) %>%
        e_bar(Missing, stack = "grp", bind = perc_missing) %>%
        e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45, fontSize = self$plot_font_size)) %>%
        e_tooltip(trigger = "item") %>%
        e_color(c(self$primary_color, "#6c757d")) %>%
        e_grid(containLabel = TRUE) %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_missval_distribution_internal = function(labels) {

      if(is.null(self$imputed_data)){return(NULL)}

      if(labels == "total") {
        data <- self$imputed_data
      } else {
        data <- self$imputed_data %>%
          filter(label == labels)
      }

      if(!self$is_imp) {
        data <- data %>%
          filter(!imputed)
      }

      p <- data %>%
        mutate(missing_value = if_else(imputed, "Imputed", "Valid")) %>%
        mutate(missing_value = factor(missing_value, levels = c("Valid", "Imputed"))) %>%
        group_by(missing_value) %>%
        e_charts(renderer = self$plot_format) %>%
        e_histogram(intensity, breaks = pretty(0:40, n = 100)) %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          min = 10,
          max = 40,
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_color(c(self$primary_color, "#bc3754")) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_missval_distribution = function() {

      if(is.null(self$imputed_data)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)

      p <- tibble("labels" = c("total", self$expdesign$label)) %>%
        mutate(plots_panel = panel_lazy(self$plot_missval_distribution_internal)) %>%
        as_trelliscope_df(name = "Distribution",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 2)


      return(p)
    },
    print_table = function(data, df = FALSE) {

      if (is.null(data)) return(NULL)

      table <- data %>%
        select(feature_names, label, intensity) %>%
        mutate(intensity = round(intensity, 2)) %>%
        pivot_wider(id_cols = feature_names, names_from = label, values_from = intensity)

      if (any(data$imputed)) {
        imp_information <- data %>%
          group_by(feature_names) %>%
          summarise(imputed = any(imputed), .groups = "drop")

        table <- table %>%
          left_join(imp_information, by = "feature_names")
      } else {
        table <- table %>%
          mutate(imputed = FALSE)
      }

      if (!df) {
        table <- table %>%
          reactable(
            searchable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            compact = TRUE,
            wrap = FALSE,
            paginationType = "simple",
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(6, 12, 18, 24),
            defaultPageSize = 12,
            height = "auto",
            defaultColDef = colDef(align = "center", minWidth = 200),
            columns = list(
              feature_names = colDef(
                name = paste(self$get_feature_label(), " names"),
                sticky = "left",
                style = list(borderRight = "1px solid #eee")
              )
            )
          )
      }

      return(table)
    },
    plot_pca = function(view_3d = FALSE) {

      if(is.null(self$imputed_data)){return(NULL)}
      if(nrow(self$expdesign) < 3){return(self$plot_empty_message("not enough samples to compute PC"))}

      ## generate a matrix from imputed intensiy
      mat <- self$imputed_data %>%
        select(feature_names, label, intensity) %>%
        pivot_wider(id_cols = "feature_names",
                    names_from = "label",
                    values_from = "intensity") %>%
        column_to_rownames("feature_names") %>%
        as.matrix()

      ## perform PCA
      pca <- prcomp(t(mat), center = TRUE, scale = TRUE)

      ## calculate persentage of each PC
      pca_var <- pca$sdev^2
      pca_var_perc <- round(pca_var/sum(pca_var)*100, 1)
      ## create a data.frame for the first 3 PC
      pca_table <- data.frame(
        label = rownames(pca$x),
        x = pca$x[, 1],
        y = pca$x[, 2],
        z = pca$x[, 3]
      ) %>%
        left_join(self$expdesign, by = "label")

      ## generate plot
      if(!view_3d){
        p <- pca_table %>%
          group_by(condition) %>%
          e_charts(x, renderer = self$plot_format) %>%
          e_scatter(y, symbol_size = c(10, 10), bind = replicate) %>%
          e_tooltip(
            trigger = "item",
            formatter = JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
          e_x_axis(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            axisLabel = list(fontSize = self$plot_font_size),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = self$plot_font_size,
              lineHeight = 4 * self$plot_font_size
            )
          ) %>%
          e_y_axis(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            axisLabel = list(fontSize = self$plot_font_size),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = self$plot_font_size,
              lineHeight = 6 * self$plot_font_size
            )
          ) %>%
          e_color(self$color_palette) %>%
          e_grid(containLabel = TRUE) %>%
          e_toolbox_feature(feature = c("saveAsImage", "restore")) %>%
          e_show_loading(text = "Loading...", color = self$primary_color)
      }else{
        p <- pca_table %>%
          group_by(condition) %>%
          e_charts(x) %>%
          e_scatter_3d(y, z, symbol_size = c(10, 10), bind = replicate) %>%
          e_tooltip(
            trigger = "item",
            formatter = JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
          e_x_axis_3d(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            axisLabel = list(fontSize = self$plot_font_size),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = self$plot_font_size,
              lineHeight = 50
            )
          ) %>%
          e_y_axis_3d(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            axisLabel = list(fontSize = self$plot_font_size),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = self$plot_font_size,
              lineHeight = 50
            )
          ) %>%
          e_z_axis_3d(
            name = paste0("PC3 - ", pca_var_perc[3], " %"),
            nameLocation = "center",
            axisLabel = list(fontSize = self$plot_font_size),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = self$plot_font_size,
              lineHeight = 50
            )
          ) %>%
          e_legend() %>%
          e_color(self$color_palette) %>%
          e_grid(containLabel = TRUE) %>%
          e_toolbox_feature(feature = c("saveAsImage", "restore")) %>%
          e_show_loading(text = "Loading...", color = self$primary_color)
      }
      return(p)
    },
    plot_correlation = function() {
      if(is.null(self$normalized_data)){return(NULL)}
      if(nrow(self$expdesign) == 1){return(self$plot_empty_message("Not enough samples to compute the correlation matrix."))}

      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }

      color <- viridis(n = 3, direction = -1, end = 0.90, begin = 0.10, option = self$palette)

      mat <- data %>%
        #select(gene_names, label, intensity) %>%
        #pivot_wider(id_cols = gene_names, names_from = label, values_from = intensity) %>%
        #filter(if_all(.cols = everything(), .fns = ~ !is.na(.x))) %>%
        #column_to_rownames("gene_names") %>%
        select(feature_names, label, intensity) %>%
        pivot_wider(id_cols = feature_names, names_from = label, values_from = intensity) %>%
        filter(if_all(.cols = everything(), .fns = ~ !is.na(.x))) %>%
        column_to_rownames("feature_names") %>%
        cor(method = self$cor_method) %>%
        round(digits = 2)

      p <- mat %>%
        e_charts(renderer = self$plot_format) %>%
        e_correlations(order = "hclust", visual_map = FALSE) %>%
        e_x_axis(axisLabel = list(
          interval = 0,
          rotate = 45,
          fontSize = self$plot_font_size
        )) %>%
        e_y_axis(
          axisLabel = list(
            interval = 0,
            rotate = 0,
            fontSize = self$plot_font_size
          ),
          position = "right"
        ) %>%
        e_tooltip(trigger = "item", formatter = JS("
          function(params){
          return('X: ' + params.value[0] + '<br />Y: ' + params.value[1] + '<br />Value: ' + params.value[2])
          }")) %>%
        e_visual_map(
          min = min(mat),
          max = 1,
          bottom = 150,
          precision = 2,
          inRange = list(color = color),
          textStyle = list(fontSize = self$plot_font_size)
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_show_loading(text = "Loading...", color = self$primary_color) %>%
        e_toolbox_feature(feature = c("saveAsImage"))
      return(p)
    },
    plot_single_scatter = function(x, y, highlights_names) {

      if(is.null(self$normalized_data)){return(NULL)}

      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }

      data_scatter <- data %>%
        filter(label %in% c(x, y)) %>%
        select(feature_names, label, intensity) %>%
        pivot_wider(id_cols = feature_names, names_from = "label", values_from = "intensity") %>%
        select(feature_names, x = !!x, y = !!y)

      min_plot <- round(min(data_scatter %>% select(-feature_names), na.rm = TRUE) - 1, 0)
      max_plot <- round(max(data_scatter %>% select(-feature_names), na.rm = TRUE) + 1, 0)
      value <- round(cor(data_scatter$x, data_scatter$y), 2)

      p <- data_scatter %>%
        e_charts(x, dispose = FALSE) %>%
        e_scatter(y, legend = FALSE, symbol_size = 5, bind = feature_names) %>%
        e_x_axis(min = min_plot, max = max_plot) %>%
        e_y_axis(min = min_plot, max = max_plot) %>%
        e_color(self$primary_color) %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_tooltip(
          formatter = JS("
            function(params){
              return('<strong>' + params.name + '</strong>');
            }
          ")
        ) %>%
        e_y_axis(
          name = x,
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(
          name = y,
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_title(
          paste0("correlation: ", value),
          left = "left",
          textStyle = list(fontSize = self$plot_font_size)
        ) %>%
        e_toolbox_feature(feature = "saveAsImage")

      p <- p %>%
        e_lm(
          formula = y ~ x,
          name = "Regression",
          smooth = FALSE,
          lineStyle = list(width = 2, type = "solid"),
          itemStyle = list(color = "#1a1a1a")
        )

      if (highlights_names != "") {
        for (name in str_split_1(highlights_names, ":")) {
          highlights_name <- data_scatter %>%
            filter(feature_names == name) %>%
            select(xAxis = x,
                   yAxis = y,
                   value = feature_names) %>%
            as.list()

          p <- p %>%
            e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(color = "black", fontWeight = "normal", fontSize = 16),
              itemStyle = list(color = self$primary_color,  borderColor = self$primary_color, borderWidth = 0.2)
            )
        }
      }
      return(p)
    },
    plot_multi_scatter = function(gene_names_h, x_filter, y_filter) {

      if(is.null(self$normalized_data)){return(NULL)}
      if(nrow(self$expdesign) == 1){return(self$plot_empty_message("Not enough samples to compute the plot."))}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      combinations <- t(combn(self$expdesign %>% pull(label), 2))
      colnames(combinations) <- c("x", "y")
      combinations <- as_tibble(combinations)
      subset <- combinations %>%
        filter(x %in% x_filter) %>%
        filter(y %in% y_filter)
      if(is.null(gene_names_h)){
        names <- ""
      } else {
        names <- paste(gene_names_h, collapse = ":")
      }
      p <- subset %>%
        mutate(highlights_names = names) %>%
        mutate(plots_panel = panel_lazy(self$plot_single_scatter)) %>%
        as_trelliscope_df(name = "Scatter",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 2)

      return(p)
    },
    plot_protein_rank = function(highlights_names = NULL) {

      if(is.null(self$rank_data)){return(NULL)}
      p <- self$rank_data %>%
        mutate(color = if_else(gene_names %in% self$protein_rank_list, self$primary_color, "#6c757d")) %>%
        e_charts(rank, renderer = self$plot_format) %>%
        e_scatter(intensity,
                  legend = FALSE,
                  symbol_size = 5,
                  bind = feature_names) %>%
        e_add_nested("itemStyle", color) %>%
        e_tooltip(formatter = JS(
          "
            function(params){
              return('<strong>' + params.name + '</strong>');
            }
          "
        )) %>%
        e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) %>%
        e_y_axis(
          name = paste(self$get_feature_label(), " Rank (log2)"),
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(
          name = paste(self$get_feature_label(), " Rank"),
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_title(
          self$protein_rank_target,
          left = "center",
          textStyle = list(fontSize = self$plot_font_size)
        ) %>%
        e_grid(containLabel = TRUE)

      if (!is.null(highlights_names)) {
        for (name in highlights_names) {
          highlights_name <- self$rank_data %>%
            filter(gene_names == name) %>%
            select(xAxis = rank,
                   yAxis = intensity,
                   value = gene_names) %>%
            as.list()

          p <- p %>%
            e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(
                color = "black",
                fontWeight = "normal",
                fontSize = 16
              ),
              itemStyle = list(
                color = self$primary_color,
                borderColor = self$primary_color,
                borderWidth = 0.2
              )
            )
        }
      }
      return(p)
    },
    print_rank_table = function() {
      if(is.null(self$rank_data)){return(NULL)}
      t <- self$rank_data %>%
        mutate(intensity = round(intensity, 3))
      return(t)
    },
    stat_uni_test_single = function(data, test, fc, alpha, p_adj_method, paired_test, test_type) {


      conds <- str_split_1(test, "_vs_")
      cond_1 <- conds[1]
      cond_2 <- conds[2]

      mean_abundance_table <- data %>%
        filter(condition %in% c(cond_1, cond_2)) %>%
        pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>%
        rowwise() %>%
        mutate(mean_abundance = mean(c(
          mean(c_across(starts_with(cond_1)), na.rm = TRUE),
          mean(c_across(starts_with(cond_2)), na.rm = TRUE)
        ), na.rm = TRUE)) %>%
        ungroup() %>%
        select(gene_names, mean_abundance)

      if(nrow(filter(self$expdesign, condition == cond_1)) == nrow(filter(self$expdesign, condition == cond_2))) {
        paired_test <- FALSE
      }

      cond_order <- self$expdesign %>%
        filter(condition == cond_2) %>%
        pull(label)

      var_equal <- test_type == "student"
      self$univariate <- TRUE

      mat <- data %>%
        filter(condition %in% c(cond_1, cond_2)) %>%
        pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>%
        column_to_rownames("gene_names") %>%
        relocate(all_of(cond_order), .after = last_col()) %>%
        na.omit() %>%
        as.matrix()

      if(test_type == "limma") {
        cond_design <- str_remove(colnames(mat), "_[^_]*$")
        group_list <- factor(cond_design, levels = unique(cond_design))
        design <- model.matrix(~group_list)
        fit <- lmFit(mat, design) %>% eBayes()

        stat_data <- topTable(fit, number = nrow(mat), adjust.method = p_adj_method) %>%
          rownames_to_column("gene_names") %>%
          mutate(
            p_val = P.Value,
            fold_change = logFC*(-1),# questo perchè limma inverte destra e sinistra nel test
            p_adj = adj.P.Val,
            significant = abs(fold_change) >= fc & p_adj <= alpha
          ) %>%
          select(gene_names, p_val, fold_change, p_adj, significant) %>%
          rename_with(~paste0(cond_1, "_vs_", cond_2, "_", .), c("p_val", "fold_change", "p_adj", "significant"))
      } else {
        a <- self$expdesign %>%
          filter(condition == cond_1) %>%
          mutate(precise_label = paste0("^", label, "$")) %>%
          pull(precise_label) %>%
          map_dbl( ~ str_which(colnames(mat), .x))
        b <- self$expdesign %>%
          filter(condition == cond_2) %>%
          mutate(precise_label = paste0("^", label, "$")) %>%
          pull(precise_label) %>%
          map_dbl( ~ str_which(colnames(mat), .x))

        if(test_type == "wilcox"){
          p_values_vec <- apply(mat, 1, function(x) wilcox.test(x[a], x[b], paired = paired_test)$p.value)
        }else{
          p_values_vec <- apply(mat, 1, function(x) t.test(x[a], x[b], paired = paired_test, var.equal = var_equal)$p.value)
        }

        stat_data <- tibble(
          gene_names = rownames(mat),
          p_val = unname(p_values_vec),
          fold_change = unname(rowMeans(mat[, a, drop = FALSE]) - rowMeans(mat[, b, drop = FALSE])),
          p_adj = unname(p.adjust(p_values_vec, method = p_adj_method))
        ) %>%
          mutate(significant = abs(fold_change) >= fc & p_adj <= alpha) %>%
          rename_with(~paste0(cond_1, "_vs_", cond_2, "_", .), c("p_val", "fold_change", "p_adj", "significant"))
      }
      stat_data <- stat_data %>%
        left_join(mean_abundance_table) %>%
        rename_with(~paste0(cond_1, "_vs_", cond_2, "_", .), c("mean_abundance"))
      return(stat_data)
    },
    stat_uni_test = function(test, fc, alpha, p_adj_method, paired_test, test_type) {

      data <- if (self$is_imp) self$imputed_data else self$normalized_data

      stat_table_list <- map(
        test,
        ~ self$stat_uni_test_single(
          data = data,
          test = .x,
          fc = fc,
          alpha = alpha,
          p_adj_method = p_adj_method,
          paired_test = paired_test,
          test_type = test_type
        )
      ) %>%
        reduce(full_join, by = "gene_names")

      joint_stat_table <- data %>%
        pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>%
        left_join(stat_table_list, by = "gene_names")

      if (self$is_imp) {
        imp_information <- data %>%
          group_by(feature_names) %>%
          summarise(imputed = any(imputed), .groups = "drop")
        joint_stat_table <- joint_stat_table %>%
          left_join(imp_information, by = "gene_names")
      }

      self$stat_table <- joint_stat_table
    },
    print_stat_table = function() {
      if(is.null(self$stat_table)){return(NULL)}
      t <- self$stat_table %>%
        mutate(across(ends_with(c("p_val", "p_adj")), ~ -log10(.))) %>%
        mutate(across(where(is.numeric), ~ round(., 3))) %>%
        arrange(across(ends_with("p_val"), desc))
      return(t)
    },
    plot_volcano_single = function(test, highlights_names, same_x, same_y) {
      # Calcolare i limiti degli assi
      max_y_plot <- self$stat_table %>%
        select(ends_with("p_val")) %>%
        mutate(across(everything(), ~ -log10(.))) %>%
        max(na.rm = TRUE) %>%
        ceiling()

      min_x_plot <- self$stat_table %>%
        select(ends_with("fold_change")) %>%
        min(na.rm = TRUE) %>%
        floor()

      max_x_plot <- self$stat_table %>%
        select(ends_with("fold_change")) %>%
        max(na.rm = TRUE) %>%
        ceiling()

      # Preparare la tabella dei dati
      table <- self$stat_table %>%
        select(gene_names, starts_with(test)) %>%
        rename_at(vars(matches(test)), ~ str_remove(., paste0(test, "_")))

      min_thr <- table %>%
        filter(significant) %>%
        pull(p_val) %>%
        max(na.rm = TRUE)

      # Creare le linee di soglia per il grafico
      left_line <- tibble(
        p_val = c(-log10(min_thr), -log10(min_thr), max(-log10(table$p_val), na.rm = TRUE)),
        fold_change = c(min(table$fold_change, na.rm = TRUE), -self$fold_change, -self$fold_change)
      )

      right_line <- tibble(
        p_val = c(max(-log10(table$p_val), na.rm = TRUE), -log10(min_thr), -log10(min_thr)),
        fold_change = c(self$fold_change, max(table$fold_change, na.rm = TRUE), self$fold_change)
      )

      # Preparare il grafico
      p <- table %>%
        mutate(color = case_when(
          fold_change > 0 & significant ~ "#67001f",
          fold_change < 0 & significant ~ "#053061",
          TRUE ~ "#e9ecef"
        )) %>%
        mutate(
          fold_change = round(fold_change, 3),
          p_val = round(-log10(p_val), 3)
        ) %>%
        e_charts(fold_change, renderer = self$plot_format) %>%
        e_scatter(p_val, legend = FALSE, bind = gene_names, symbol_size = 5) %>%
        e_tooltip(formatter = JS("
      function(params) {
        return('<strong>' + params.name + '</strong><br />FC: ' + params.value[0] + '<br />p.val: ' + params.value[1])
      }
    ")) %>%
        e_add_nested("itemStyle", color) %>%
        e_data(left_line, fold_change) %>%
        e_line(p_val, legend = FALSE, color = "#000", symbol = "none", lineStyle = list(type = "dashed", width = .8)) %>%
        e_data(right_line, fold_change) %>%
        e_line(p_val, legend = FALSE, color = "#000", symbol = "none", lineStyle = list(type = "dashed", width = .8)) %>%
        e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) %>%
        e_x_axis(
          name = "Difference (fold change)",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_y_axis(
          name = "-log10 p-value",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_group("grp") %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      # Aggiungere punti di evidenziazione
      if (highlights_names != "") {
        for (name in str_split_1(highlights_names, ":")) {
          highlights_name <- table %>%
            filter(gene_names == name) %>%
            mutate(p_val = -log10(p_val)) %>%
            select(xAxis = fold_change,
                   yAxis = p_val,
                   value = gene_names) %>% as.list()

          p <- p %>%
            e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(color = "black", fontWeight = "normal", fontSize = 16),
              itemStyle = list(color = self$primary_color,  borderColor = self$primary_color, borderWidth = 0.2)
            )
        }
      }

      # Configurare gli assi
      if (same_x) {
        p <- p %>%
          e_x_axis(min = min_x_plot - 1, max = max_x_plot + 1)
      }

      if (same_y) {
        p <- p %>%
          e_y_axis(min = 0, max = max_y_plot + 1)
      }

      return(p)
    },
    plot_ma_single = function(test, highlights_names, same_x, same_y) {

      max_y_plot <- self$stat_table %>%
        select(ends_with("fold_change")) %>%
        max(na.rm = TRUE) %>%
        ceiling()

      min_y_plot <- self$stat_table %>%
        select(ends_with("fold_change")) %>%
        min(na.rm = TRUE) %>%
        floor()

      min_x_plot <- self$stat_table %>%
        select(ends_with("mean_abundance")) %>%
        min(na.rm = TRUE) %>%
        floor()

      max_x_plot <- self$stat_table %>%
        select(ends_with("mean_abundance")) %>%
        max(na.rm = TRUE) %>%
        ceiling()

      table <- self$stat_table %>%
        select(gene_names, starts_with(test)) %>%
        rename_at(vars(matches(test)), ~ str_remove(., paste0(test, "_")))

      p <- table %>%
        mutate(color = case_when(
          fold_change > 0 & significant ~ "#67001f",
          fold_change < 0 & significant ~ "#053061",
          TRUE ~ "#e9ecef"
        )) %>%
        mutate(
          fold_change = round(fold_change, 3),
          mean_abundance = round(mean_abundance, 3)
        ) %>%
        e_charts(mean_abundance, renderer = self$plot_format) %>%
        e_scatter(
          fold_change,
          legend = FALSE,
          bind = gene_names,
          symbol_size = 5
        ) %>%
        e_tooltip(formatter = htmlwidgets::JS("
      function(params) {
        return('<strong>' + params.name + '</strong><br />A: ' +
               params.value[0] + '<br />LFC: ' + params.value[1])
      }
    ")) %>%
        e_add_nested("itemStyle", color) %>%
        e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) %>%
        e_x_axis(
          name = "Mean abundance (A)",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_y_axis(
          name = "Log2 fold change (M)",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_group("grp") %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      if (highlights_names != "") {
        for (name in str_split_1(highlights_names, ":")) {
          highlights_name <- table %>%
            filter(gene_names == name) %>%
            select(
              xAxis = mean_abundance,
              yAxis = fold_change,
              value = gene_names
            ) %>% as.list()

          p <- p %>%
            e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(color = "black", fontWeight = "normal", fontSize = 16),
              itemStyle = list(
                color = self$primary_color,
                borderColor = self$primary_color,
                borderWidth = 0.2
              )
            )
        }
      }

      if (same_x) {
        p <- p %>%
          e_x_axis(min = min_x_plot - 1, max = max_x_plot + 1)
      }

      if (same_y) {
        p <- p %>%
          e_y_axis(min = min_y_plot - 1, max = max_y_plot + 1)
      }
      return(p)
    },
    plot_volcano = function(tests, gene_names_marked, all_same_x, all_same_y) {

      if(is.null(self$stat_table)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)

      if(is.null(gene_names_marked)){
        names <- ""
      } else {
        names <- paste(gene_names_marked, collapse = ":")
      }
      contrasts_table <- tibble(
        test = tests,
        highlights_names = names,
        same_x = all_same_x,
        same_y = all_same_y
      )
      p <- contrasts_table %>%
        mutate(plots_panel = panel_lazy(self$plot_volcano_single)) %>%
        as_trelliscope_df(name = "Volcano Plots",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 1)

      return(p)
    },
    plot_ma = function(tests, gene_names_marked, all_same_x, all_same_y) {

      if(is.null(self$stat_table)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)

      if(is.null(gene_names_marked)){
        names <- ""
      } else {
        names <- paste(gene_names_marked, collapse = ":")
      }
      contrasts_table <- tibble(
        test = tests,
        highlights_names = names,
        same_x = all_same_x,
        same_y = all_same_y
      )
      p <- contrasts_table %>%
        mutate(plots_panel = panel_lazy(self$plot_ma_single)) %>%
        as_trelliscope_df(name = "MA Plots",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 1)

      return(p)
    },
    plot_stat_profile_single = function(contrast, gene) {

      if(gene == "NO_GENE_SELECTED"){return(self$plot_empty_message("No genes selected."))}
      data <- if (self$is_imp) self$imputed_data else self$normalized_data
      cond <- unique(str_split_1(contrast, "_vs_"))

      data_p <- data %>%
        filter(gene_names == gene) %>%
        filter(condition %in% cond) %>%
        group_by(condition) %>%
        drop_na()

      min_plot <- round(min(data_p %>% pull(intensity), na.rm = TRUE) - 1, 0)
      max_plot <- round(max(data_p %>% pull(intensity), na.rm = TRUE) + 1, 0)

      p <- data_p %>%
        e_charts(renderer = self$plot_format) %>%
        e_boxplot(
          intensity,
          colorBy = "data",
          outliers = FALSE,
          itemStyle = list(borderWidth = 2)
        ) %>%
        e_y_axis(min = min_plot, max = max_plot) %>%
        e_legend(show = FALSE) %>%
        e_title(
          gene,
          left = "center",
          textStyle = list(fontSize = self$plot_font_size)
        ) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_x_axis(
          axisLabel = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size
          )) %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom"))

      return(p)
    },
    plot_stat_profile = function(tests, genes) {
      if(is.null(genes) || length(genes) == 0){
        genes <- "NO_GENE_SELECTED"
      }
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      x_all_genes <- rep(tests, each = length(genes))
      x_all_contrast <- rep(genes, length(tests))
      table <- tibble(
        contrast = x_all_genes,
        gene = x_all_contrast
      )
      p <- table %>%
        mutate(plots_panel = panel_lazy(self$plot_stat_profile_single)) %>%
        as_trelliscope_df(name = "Profile Plots",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 2)

      return(p)
    },
    stat_anova = function(alpha, p_adj_method) {
      if(is.null(self$imputed_data)){return(NULL)}
      data <- if (self$is_imp) self$imputed_data else self$normalized_data

      # Rimuovere i geni con valori NA
      data <- data %>%
        group_by(feature_names) %>%
        filter(!any(is.na(intensity))) %>%
        ungroup()

      # Calcolare i p-values
      p_values_vec <- data %>%
        split(.$gene_names) %>%
        map_dbl(~ summary(aov(intensity ~ condition, .x))[[1]][["Pr(>F)"]][[1]])

      # Creare tibble con p-values e p-values aggiustati
      p_values <- tibble(gene_names = names(p_values_vec), p_val = p_values_vec)
      p_ajusted <- tibble(gene_names = names(p_values_vec), p_adj = p.adjust(p_values_vec, method = p_adj_method))

      # Combinare i dati e aggiungere colonne di significatività e cluster
      stat_data <- data %>%
        pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>%
        full_join(p_values, by = "gene_names") %>%
        full_join(p_ajusted, by = "gene_names") %>%
        mutate(significant = if_else(p_adj <= alpha, TRUE, FALSE),
               cluster = "not_defined")

      # perform operation for heatmap
      mat_base <- stat_data %>%
        filter(significant) %>%
        select(-c(p_val, p_adj, significant, cluster)) %>%
        column_to_rownames("gene_names") %>%
        as.matrix()

      if(self$clusters_number < 1) {
        self$clusters_number <- 1
      }

      if (nrow(mat_base) <= self$clusters_number) {
        self$anova_matrix <- NULL
        self$anova_table <- stat_data
        return()
      }

      if (self$z_score) {
        mat <- t(apply(mat_base, 1, scale))
        colnames(mat) <- colnames(mat_base)
      } else {
        mat <- mat_base
      }

      self$row_den <- hclust(dist(mat), method = self$anova_clust_method)
      self$col_den <- hclust(dist(t(mat)), method = self$anova_clust_method)

      clusters <- cutree(self$row_den, k = self$clusters_number) %>%
        enframe(name = "gene_names", value = "cluster") %>%
        mutate(cluster = paste0("cluster_", cluster))

      self$anova_table <- stat_data %>%
        select(-cluster) %>%
        left_join(clusters, by = "gene_names") %>%
        mutate(cluster = if_else(is.na(cluster), "not_defined", cluster))

      self$anova_matrix <- mat
    },
    print_anova_table = function() {
      if(is.null(self$anova_table)){return(NULL)}
      t <- self$anova_table %>%
        select(gene_names, p_val, p_adj, significant, cluster) %>%
        arrange(p_val, -significant) %>%
        mutate(across(c("p_val", "p_adj"), ~ -log10(.))) %>%
        mutate(across(c("p_val", "p_adj"), ~ round(., 3)))
      return(t)
    },
    plot_heatmap = function(order_by_expdesing) {

      if(is.null(self$anova_matrix)){
        p <- plotly_empty(type = "scatter", mode = "markers") %>%
          config(displayModeBar = FALSE) %>%
          layout(
            title = list(
              text = "Not enough significant genes to generate a heatmap.",
              yref = "paper",
              y = 0.5
            )
          )
        return(p)
      }

      if (self$z_score) {
        mat_name <- "z-score"
      } else {
        mat_name <- "log2(Intensity)"
      }

      col_side_colors <- self$expdesign %>%
        select(label, condition) %>%
        deframe()

      col_side_palette <- tibble(
        levels = unique(col_side_colors),
        color = self$color_palette) %>%
        deframe()

      # Creating heatmap with heatmaply
      h <- heatmaply(
        self$anova_matrix,
        Rowv = as.dendrogram(self$row_den),
        k_row = self$clusters_number,
        Colv = if (!order_by_expdesing) as.dendrogram(self$col_den) else NA,
        column_text_angle = 45,
        plot_method = "plotly",
        showticklabels = c(TRUE, FALSE),
        fontsize_row = self$plot_font_size,
        fontsize_col = self$plot_font_size,
        col_side_colors = col_side_colors,
        col_side_palette = col_side_palette,
        label_names = c("Gene", "Sample", mat_name),
        row_dend_left = TRUE,
        seriate = "none"
      ) %>% config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Heatmap',
          height = 1000,
          width = 1200,
          scale = 1
        )
      )

      # Update column order in anova_col_order
      self$anova_col_order <- h$x$layout$xaxis2$ticktext

      return(h)
    },
    plot_protein_profile = function(gene) {

      if(is.null(self$anova_table)){return(NULL)}
      if(is.null(gene) || length(gene) == 0){return(self$plot_empty_message("No Protein selected."))}

      clu <- self$anova_table %>%
        filter(gene_names %in% gene) %>%
        distinct(cluster) %>%
        pull(cluster)

      if("not_defined" %in% clu) {
        alpha_cols <- viridis(n = length(clu) - 1, option = self$palette)
        alpha_cols <- str_replace(alpha_cols, pattern = "FF", replacement = "E6")
        sub_alpha_cols <- c(alpha_cols, "#22262980")
      } else {
        alpha_cols <- viridis(n = length(clu), option = self$palette)
        sub_alpha_cols <- str_replace(alpha_cols, pattern = "FF", replacement = "E6")
      }

      p <- self$anova_table %>%
        pivot_longer(
          !c(gene_names, p_val, p_adj, significant, cluster),
          names_to = "label",
          values_to = "intensity"
        ) %>%
        filter(gene_names %in% gene) %>%
        mutate(intensity = round(intensity, 2)) %>%
        group_by(cluster, feature_names) %>%
        arrange(factor(label, levels = self$anova_col_order)) %>%
        e_charts(label, renderer = self$plot_format) %>%
        e_line(intensity, bind = gene_names) %>%
        e_color(sub_alpha_cols) %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_x_axis(
          name = "",
          axisLabel = list(
            interval = 0,
            rotate = 45,
            fontSize = self$plot_font_size
          )
        ) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)

      return(p)
    },
    plot_cluster_profile_single = function(clust_name, clust_color) {
      if(is.null(self$anova_table)){return(NULL)}
      p <- self$anova_table %>%
        pivot_longer(
          !c(gene_names, p_val, p_adj, significant, cluster),
          names_to = "label",
          values_to = "intensity"
        ) %>%
        filter(cluster == clust_name) %>%
        group_by(label) %>%
        summarise(
          n = n(),
          sd = sd(intensity, na.rm = TRUE),
          median = median(intensity, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          error = qt(0.975, df = n - 1) * sd / sqrt(n),
          lower_bound = median - error,
          upper_bound = median + error
        ) %>%
        arrange(factor(label, levels = self$anova_col_order)) %>%
        e_charts(label, renderer = self$plot_format) %>%
        e_line(median,
               symbol = "none",
               color = "#222629",
               name = "Median") %>%
        e_band2(lower_bound,
                upper_bound,
                itemStyle = list(borderWidth = 0),
                name = "95% CI") %>%
        e_color(clust_color) %>%
        e_legend(textStyle = list(fontSize = self$plot_font_size)) %>%
        e_x_axis(
          name = "",
          axisLabel = list(
            interval = 0,
            rotate = 45,
            fontSize = self$plot_font_size
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 6 * self$plot_font_size
          )
        ) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)
      return(p)
    },
    plot_cluster_profile = function() {
      if(is.null(self$anova_table)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)

      clusters <- self$anova_table %>% distinct(cluster) %>% filter(cluster != "not_defined") %>% pull()
      colors <- viridis(n = length(clusters), option = self$palette)
      alpha_colors <- str_replace(colors, pattern = "FF", replacement = "E6")
      table <- tibble(clust_name = clusters, clust_color = alpha_colors)
      p <- table %>%
        mutate(plots_panel = panel_lazy(self$plot_cluster_profile_single)) %>%
        as_trelliscope_df(name = "Profile Plot",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = 2)

      return(p)
    },
    make_nodes = function(list_from, focus, direction) {
      if (list_from == "univariate") {
        if(is.null(focus)){return(NULL)}
        if (is.null(self$stat_table) ||
            nrow(filter(self$stat_table, if_all(ends_with("significant"), ~ . == TRUE))) == 0) {
          self$nodes_table <- NULL
          return(NULL)
        }
        nodes_table <- self$print_stat_table() %>%
          select(gene_names, starts_with(focus)) %>%
          rename_at(vars(matches(focus)), ~ str_remove(., paste0(focus, "_"))) %>%
          filter(significant) %>%
          mutate(
            category = if_else(fold_change > 0, "up", "down"),
            size = p_val * 5,
            color = case_when(
              fold_change > 0 & fold_change <= 1.5 ~ "#fddbc7",
              fold_change > 1.5 & fold_change <= 2 ~ "#f4a582",
              fold_change > 2 & fold_change <= 2.5 ~ "#d6604d",
              fold_change > 2.5 & fold_change <= 3 ~ "#b2182b",
              fold_change > 3 ~ "#67001f",
              fold_change < 0 & fold_change >= -1.5 ~ "#d1e5f0",
              fold_change < -1.5 & fold_change >= -2 ~ "#92c5de",
              fold_change < -2 & fold_change >= -2.5 ~ "#4393c3",
              fold_change < -2.5 & fold_change >= -3 ~ "#2166ac",
              fold_change < -3 ~ "#053061"
            )
          )
        if(length(direction) < 2) {
          if ("up" %in% direction) {
            nodes_table <- nodes_table %>%
              filter(category == "up")
          }
          if ("down" %in% direction) {
            nodes_table <- nodes_table %>%
              filter(category == "down")
          }
        }
      } else if (list_from == "multivariate") {
        if(is.null(self$anova_table) ||
           nrow(filter(self$anova_table, significant)) == 0){
          self$nodes_table <- NULL
          return(NULL)
        }
        nodes_table <- self$print_anova_table() %>%
          filter(significant) %>%
          filter(cluster %in% focus) %>%
          mutate(category = cluster, size = p_val * 2)
      } else {
        nodes_table <- tibble(gene_names = self$protein_rank_list) %>%
          mutate(
            category = self$protein_rank_target,
            p_val = 1,
            p_adj = 1,
            size = 10
          )
      }
      self$nodes_table <- nodes_table
      self$name_for_edges <- nodes_table %>%
        pull(gene_names)
    },
    make_edges = function(source) {
      edges_string_table <- NULL
      edges_corum_table <- NULL
      if(is.null(self$nodes_table)){return(NULL)}
      org_map <- inputs_type_lists$org_map
      org_info <- org_map[[self$organism]]
      if (is.null(org_info)) return(NULL)
      tax_id <- org_info$tax_id
      if("string" %in% source) {
        edges_string_table <- self$name_for_edges %>%
          rba_string_interactions_network(species = tax_id, verbose = FALSE) %>%
          filter(escore != 0, dscore != 0) %>%
          unite("stringId", stringId_A:stringId_B, remove = TRUE) %>%
          distinct(stringId, .keep_all = TRUE) %>%
          ## string calculation for fisical score
          mutate(score1 = (escore - 0.041) * (1 - 0.041)) %>%
          mutate(score2 = (dscore - 0.041) * (1 - 0.041)) %>%
          mutate(score_combin = 1 - (1 - score1) * (1 - score2)) %>%
          mutate(score = score_combin + 0.041 * (1 - score_combin)) %>%
          ## end
          select(source = preferredName_A, target = preferredName_B, score) %>%
          filter(source != target) %>%
          mutate(
            complex = "not defined",
            color = "#999999",
            size = round(score * 10 / 2, 0),
            database = "String"
          )
        if (nrow(edges_string_table) == 0) {
          edges_string_table <- NULL
        }
      }
      if("corum" %in% source) {
        if(tax_id == 9606){
          raw_corum_table <-
            get_complex_genes(
              import_omnipath_complexes(resources = "CORUM"),
              self$name_for_edges,
              total_match = FALSE
            ) %>%
            unique() %>%
            select(name, components_genesymbols) %>%
            separate_rows(components_genesymbols, sep = "_") %>%
            filter(components_genesymbols %in% self$name_for_edges) %>%
            unique() %>%
            group_by(name) %>%
            filter(n() > 1) %>%
            ungroup()
          if (nrow(raw_corum_table) != 0) {
            expand_nodes <- raw_corum_table %>%
              group_by(name) %>%
              group_map( ~ pull(.x, components_genesymbols))
            edges_corum_table <-
              map(.x = expand_nodes, .f = ~ as.data.frame(t(combn(.x, 2)))) %>%
              reduce(bind_rows) %>%
              rename(target = V1,  source = V2) %>%
              left_join(
                raw_corum_table,
                by = c("source" = "components_genesymbols"),
                relationship = "many-to-many"
              ) %>%
              rename(complex = name) %>%
              unique() %>%
              mutate(score = 1, color = "#4daf4a") %>%
              group_by(source, target, color) %>%
              nest() %>%
              unnest_wider(data, names_sep = "_") %>%
              ungroup() %>%
              mutate(complex = map_chr(data_complex, str_flatten, collapse = "/")) %>%
              rowwise() %>%
              mutate(
                score = sum(data_score),
                size = if_else(score <= 5, score, 5),
                database = "Corum"
              ) %>%
              select(source, target, complex, score, color, size, database)
          } else {
            edges_corum_table <- NULL
          }
        } else {
          edges_corum_table <- NULL
        }
      }
      if (is.null(edges_string_table) & is.null(edges_corum_table)) {
        self$edges_table <- NULL
      } else {
        self$edges_table <- edges_string_table %>%
          bind_rows(edges_corum_table)
      }
    },
    plot_ppi_network = function(list_from, score_thr, isolate_nodes, layout, show_names, selected, filtered) {
      if(is.null(self$nodes_table) | is.null(self$edges_table)) {
        return(self$plot_empty_message("No network to display."))
      }
      edges <- self$edges_table %>%
        filter(score >= score_thr)

      nodes <- self$nodes_table

      if (!isolate_nodes) {
        final_list <- unique(c(edges$source, edges$target))
        nodes <- nodes %>%
          filter(gene_names %in% final_list)
      }

      if (filtered) {
        nodes <- nodes %>%
          filter(gene_names %in% selected)
      }

      if (nrow(nodes) == 0) {
        return(self$plot_empty_message("Nodes table is empty."))
      }

      p <- e_charts(renderer = self$plot_format) %>%
        e_graph(
          roam = TRUE,
          layout = layout,
          zoom = 0.5,
          force = list(
            initLayout = "circular",
            repulsion = 800,
            edgeLength = 150,
            layoutAnimation = FALSE
          ),
          autoCurveness = TRUE,
          emphasis = list(focus = "adjacency")
        ) %>%
        e_graph_nodes(
          nodes = nodes,
          names = gene_names,
          value = p_val,
          size = size,
          category = category,
          legend = FALSE
        ) %>%
        e_graph_edges(
          edges = edges,
          source = source,
          target = target,
          value = score,
          size = size
        ) %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore"))

      if (show_names) {
        p <- p %>%
          e_labels(fontSize = self$plot_font_size)
      }

      p$x$opts$series[[1]]$links <- map2(p$x$opts$series[[1]]$links, edges$color, ~ modifyList(.x, list(lineStyle = list(color = .y))))

      if (list_from == "univariate") {
        p$x$opts$series[[1]]$data <- map2(p$x$opts$series[[1]]$data, nodes$color, ~ modifyList(.x, list(itemStyle = list(color = .y))))
      }

      if (!is.null(selected) && !filtered) {
        p$x$opts$series[[1]]$data <- map(p$x$opts$series[[1]]$data, function(node) {
          if (node$name %in% selected) {
            node$itemStyle$color <- "#198754"
            node$symbolSize <- 30
          }
          node
        })
      }
      return(p)
    },
    print_nodes = function(isolate_nodes, score_thr) {
      if(is.null(self$nodes_table) | is.null(self$edges_table)){return(NULL)}
      edges <- self$edges_table %>%
        filter(score >= score_thr)
      nodes <- self$nodes_table
      if(!isolate_nodes) {
        edge_source <- edges %>% pull(source)
        edge_target <- edges %>% pull(target)
        list <- c(edge_source, edge_target)
        final_list <- unique(list)
        nodes <- nodes %>%
          filter(gene_names %in% final_list)
      }
      nodes_tab <- nodes %>%
        select(c(gene_names, category, p_val, p_adj))

      return(nodes_tab)
    },
    get_summary_data = function() {

      df <- self$filtered_data

      df %>%
        mutate(
          feature_names = ifelse(
            self$analysis_level == "peptide",
            peptide_names,
            gene_names
          )
        ) %>%
        group_by(PMethod, FAIMS) %>%
        summarise(
          n_proteins = n_distinct(feature_names),
          .groups = "drop"
        )
    },
    print_edges = function(selected_nodes, score_thr) {
      if(is.null(self$nodes_table) | is.null(self$edges_table)){return(NULL)}
      edges_tab <- self$edges_table %>%
        filter(score >= score_thr) %>%
        filter(if (length(selected_nodes) != 0) source %in% selected_nodes | target %in% selected_nodes else TRUE) %>%
        select(-color, -size) %>%
        mutate(score = if_else(complex == "not defined", round(score, 2), 1)) %>%
        separate_rows(complex, sep = "/") %>%
        relocate(complex, .after = database)
      return(edges_tab)
    },
    reactable_network = function(table, interactive) {
      if(is.null(table)){return(NULL)}
      sele <- NULL
      oncl <- NULL
      w <- TRUE
      if(interactive){
        sele <- "multiple"
        oncl <- "select"
        w <- FALSE
      }
      t <- table %>%
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          wrap = w,
          height = "auto",
          selection = sele,
          paginationType = "simple",
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(6, 12, 18, 24),
          defaultPageSize = 12,
          onClick = oncl,
          defaultColDef = colDef(align = "center", minWidth = 100)
        )
      return(t)
    },
    make_ora_list_internal = function(focus) {
      if((is.null(self$stat_table) || is.null(focus))){return(NULL)}
      test <- str_remove(focus, pattern = "_up|_down")
      data <- self$stat_table %>%
        select(gene_names, starts_with(test)) %>%
        rename_at(vars(matches(test)), ~ str_remove(., paste0(test, "_"))) %>%
        filter(significant)
      if(nrow(data) > 0) {
        data <- data %>%
          mutate(direction = if_else(fold_change > 0, paste0(test, "_up"), paste0(test, "_down"))) %>%
          filter(direction == focus) %>%
          select(gene_names, direction)
      } else {
        data <- tibble(
          gene_names = c("NO_Significant", "NO_Significant"),
          direction = c(paste0(test, "_up"), paste0(test, "_down"))
        )
      }
      return(data)
    },
    go_ora = function(list_from, focus, database, ontology, simplify_thr, alpha, p_adj_method, min_gs_size, max_gs_size, background) {
      if(is.null(focus)){return(NULL)}
      org_map <- inputs_type_lists$org_map
      org_info <- org_map[[self$organism]]
      if (is.null(org_info)) return(NULL)

      orgdb     <- org_info$orgdb
      kegg_org  <- org_info$kegg
      wiki_org  <- org_info$wiki

      if (list_from == "univariate") {
        groupped_data <- map(focus, ~ self$make_ora_list_internal(focus = .x)) %>%
          reduce(bind_rows) %>%
          group_by(direction)
        gene_vector <- groupped_data %>%
          group_map(~ pull(.x, gene_names)) %>%
          set_names(group_keys(groupped_data) %>% pull())
        uni <- self$stat_table %>% pull(gene_names)
      }

      if (list_from == "multivariate") {
        if(is.null(self$anova_table)){return(NULL)}
        groupped_data <- self$anova_table %>%
          filter(cluster %in% focus) %>%
          select(gene_names, cluster) %>%
          group_by(cluster)
        if (nrow(groupped_data) == 0) {
          gene_vector <- list(not_defined = "NO_Significant")
        } else {
          gene_vector <- groupped_data %>%
            group_map(~ pull(.x, gene_names)) %>%
            set_names(group_keys(groupped_data) %>% pull())
        }
        uni <- self$anova_table %>% pull(gene_names)
      }

      if (list_from == "top_rank"){
        gene_vector <- list(self$protein_rank_list) %>%
          set_names(self$protein_rank_target)
        uni <- self$rank_data %>% pull(gene_names)
      }

      if (list_from == "manual"){
        gene_vector <- list("manual" = focus)
        uni <- NULL
      }

      if (!background) {
        uni <- NULL
      }

      if (database == "GO") {
        self$ora_result_list <- map(gene_vector, possibly(~ enrichGO(
          gene = .x,
          OrgDb = orgdb,
          keyType = 'SYMBOL',
          ont = ontology,
          pAdjustMethod = p_adj_method,
          universe = uni,
          minGSSize = min_gs_size,
          maxGSSize = max_gs_size,
          readable = TRUE) %>%
            clusterProfiler::filter(p.adjust < alpha) %>%
            simplify(cutoff = simplify_thr), otherwise = NULL))
      }

      if (database == "KEGG") {
        entrez <- map(
          gene_vector,
          ~ bitr(
            .x,
            fromType = "SYMBOL",
            toType   = "ENTREZID",
            OrgDb    = orgdb
          ) %>% pull(ENTREZID)
        )
        self$ora_result_list <- map(entrez, possibly(~ enrichKEGG(
          gene = .x,
          organism = kegg_org,
          keyType = 'kegg',
          pAdjustMethod = p_adj_method,
          universe = uni,
          minGSSize = min_gs_size,
          maxGSSize = max_gs_size) %>%
            clusterProfiler::filter(p.adjust < alpha), otherwise = NULL))
      }

      if (database == "WikiPathways") {
        entrez <- map(
          gene_vector,
          ~ bitr(
            .x,
            fromType = "SYMBOL",
            toType   = "ENTREZID",
            OrgDb    = orgdb
          ) %>% pull(ENTREZID)
        )
        self$ora_result_list <- map(entrez, possibly(~ enrichWP(
          gene = .x,
          organism = wiki_org,
          pAdjustMethod = p_adj_method,
          universe = uni,
          minGSSize = min_gs_size,
          maxGSSize = max_gs_size) %>%
            clusterProfiler::filter(p.adjust < alpha), otherwise = NULL))
      }

    },
    print_ora_table = function(arranged_with) {
      if(length(compact(self$ora_result_list)) == 0){
        self$ora_table <- NULL
        return(NULL)
      }
      empty <- map(self$ora_result_list, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>%
        as_tibble() %>%
        nrow()
      if(empty == 0) {
        self$ora_table <- NULL
        return(NULL)
      }
      self$ora_table <- map(self$ora_result_list, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>%
        as_tibble() %>%
        separate(GeneRatio, into = c("a", "b"), sep = "/", remove = FALSE) %>%
        separate(BgRatio, into = c("c", "d"), sep = "/", remove = FALSE) %>%
        mutate(
          fold_enrichment = (as.numeric(a) / as.numeric(b)) / (as.numeric(c) / as.numeric(d)),
          across(c("pvalue", "p.adjust", "qvalue"), ~ round(-log10(.), 3)),
          fold_enrichment = round(fold_enrichment, 3)
        ) %>%
        select(-c(a, b, c, d)) %>%
        relocate(ID) %>%
        relocate(geneID, .after = last_col()) %>%
        relocate(Count, .after = fold_enrichment) %>%
        arrange(desc(!!sym(arranged_with)))
    },
    plot_ora_single = function(focus, arrange, show_category) {
      if(is.null(self$ora_table) || is.null(focus)){return(self$plot_empty_message("No enrichment results."))}
      data <- self$ora_table %>%
        filter(group == focus)
      if (nrow(data) == 0) {
        return(self$plot_empty_message("No enrichment results."))
      }
      p <- data %>%
        rename(value := !!arrange) %>%
        slice_max(abs(value), n = show_category, with_ties = FALSE) %>%
        arrange(value) %>%
        e_charts(ID, renderer = self$plot_format) %>%
        e_bar(value, bind = Description) %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$primary_color) %>%
        e_tooltip(
          formatter = JS(
            paste0("function(params){return('<strong>", arrange, ": </strong>' + params.value[0])}")
          )
        ) %>%
        e_x_axis(
          name = arrange,
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_y_axis(axisLabel = list(fontSize = 0)) %>%
        e_legend(show = FALSE) %>%
        e_labels(show = TRUE, formatter= '{b}', position = "insideLeft") %>%
        e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)
      return(p)
    },
    plot_ora = function(groups, arrange_with, show_n_category) {
      if(is.null(groups)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      if(length(groups) == 1){n_col = 1}else{n_col = 2}
      table <- tibble(
        focus = groups,
        arrange = arrange_with,
        show_category = show_n_category
      )
      p <- table %>%
        mutate(plots_panel = panel_lazy(self$plot_ora_single)) %>%
        as_trelliscope_df(name = "BarPlot",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = n_col)

      return(p)
    },
    reactable_functional_analysis = function(table) {
      if(is.null(table)){return(NULL)}
      t <- table %>%
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          wrap = FALSE,
          height = "auto",
          paginationType = "simple",
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(6, 12, 18, 24),
          defaultPageSize = 12,
          defaultColDef = colDef(align = "center", minWidth = 200),
          columns = list(
            gene_names = colDef(
              name = "ID",
              sticky = "left",
              style = list(borderRight  = "1px solid #eee")
            ),
            geneID = colDef(minWidth = 1500, align = "left"),
            Description = colDef(minWidth = 500, align = "left")
          )
        )
      return(t)
    },
    go_gsea_rank_vector = function(test, rank, cond, db, org_db) {
      if(is.null(test)){return(NULL)}
      if (rank == "fc") {
        fc <- paste0(test, "_fold_change")
        gsea_vec <- self$stat_table %>%
          select(all_of(c("gene_names", fc))) %>%
          arrange(desc(!!sym(fc))) %>%
          select(gene_names, score = !!sym(fc))
      } else if (rank == "p_val") {
        p_val <- paste0(test, "_p_val")
        fc <- paste0(test, "_fold_change")
        gsea_vec <- self$stat_table %>%
          select(all_of(c("gene_names", p_val, fc))) %>%
          mutate(across(ends_with(c("p_val")), ~ -log10(.))) %>%
          mutate(across(ends_with(c("fold_change")), ~ sign(.))) %>%
          mutate(score = !!sym(p_val) * !!sym(fc)) %>%
          arrange(desc(score)) %>%
          select(gene_names, score)
      } else {
        if (cond) {
          gsea_vec <- self$imputed_data %>%
            filter(condition == test) %>%
            group_by(feature_names) %>%
            summarise(mean_intensity = mean(intensity), .groups = "drop") %>%
            arrange(desc(mean_intensity)) %>%
            select(gene_names, score = mean_intensity)
        } else {
          gsea_vec <- self$imputed_data %>%
            filter(label == test) %>%
            arrange(desc(intensity)) %>%
            select(gene_names, score = intensity)
        }
      }
      if(db == "GO") {
        gsea_vec_final <- gsea_vec %>%
          deframe() %>%
          list()
      } else {
        gsea_vec$idx <- seq_len(nrow(gsea_vec))
        conv <- bitr(
          gsea_vec$gene_names,
          fromType = "SYMBOL",
          toType   = "ENTREZID",
          OrgDb    = org_db
        )
        conv <- conv[!duplicated(conv$SYMBOL), ]
        gsea_vec$ENTREZID <- conv$ENTREZID[
          match(gsea_vec$gene_names, conv$SYMBOL)
        ]
        gsea_vec <- gsea_vec[!is.na(gsea_vec$ENTREZID), ]
        gsea_vec[order(gsea_vec$idx), ]
        gsea_vec <- gsea_vec[, c("ENTREZID", "score", "idx")]
        gsea_vec <- do.call(
          rbind,
          lapply(
            split(gsea_vec, gsea_vec$ENTREZID),
            function(x) x[which.max(abs(x$score)), ]
          )
        )
        gsea_vec <- gsea_vec[order(gsea_vec$idx), c("ENTREZID", "score")]
        rownames(gsea_vec) <- NULL
        gsea_vec_final <- gsea_vec %>%
          deframe() %>%
          list()
      }
      gsea_list_vec <- set_names(gsea_vec_final, test)
      return(gsea_list_vec)
    },
    go_gsea = function(test, rank_type, by_condition, database, ontology, simplify_thr, alpha, p_adj_method, min_gs_size, max_gs_size) {
      if(is.null(test)){return(NULL)}
      org_map <- inputs_type_lists$org_map
      org_info <- org_map[[self$organism]]
      if (is.null(org_info)) return(NULL)

      orgdb     <- org_info$orgdb
      kegg_org  <- org_info$kegg
      wiki_org  <- org_info$wiki

      list_of_gesa_vector <- map(
        .x = test,
        .f = ~ self$go_gsea_rank_vector(
          test = .x,
          rank = rank_type,
          cond = by_condition,
          db = database,
          org_db = orgdb
        )
      ) %>% flatten()

      if (database == "GO") {
        self$gsea_result_list <- map(
          .x = list_of_gesa_vector,
          .f = possibly(
            ~ gseGO(
              geneList      = .x,
              OrgDb         = orgdb,
              ont           = ontology,
              keyType       = 'SYMBOL',
              pAdjustMethod = p_adj_method,
              minGSSize     = min_gs_size,
              maxGSSize     = max_gs_size,
              verbose       = FALSE,
              nPermSimple   = 10000,
              eps           = 0
            )  %>%
              clusterProfiler::filter(p.adjust < alpha) %>%
              simplify(cutoff = simplify_thr),
            otherwise = NULL
          )
        )
      }
      if (database == "KEGG") {
        self$gsea_result_list <- map(
          .x = list_of_gesa_vector,
          .f = possibly(
            ~ gseKEGG(
              geneList  = .x,
              organism = kegg_org,
              keyType = 'kegg',
              pAdjustMethod = p_adj_method,
              minGSSize = min_gs_size,
              maxGSSize = max_gs_size,
              verbose       = FALSE,
              nPermSimple   = 10000,
              eps           = 0
            ) %>%
              clusterProfiler::filter(p.adjust < alpha),
            otherwise = NULL
          )
        )
      }
      if (database == "WikiPathways") {
        self$gsea_result_list <- map(
          .x = list_of_gesa_vector,
          .f = possibly(
            ~ gseWP(
              geneList = .x,
              organism = wiki_org,
              pAdjustMethod = p_adj_method,
              minGSSize = min_gs_size,
              maxGSSize = max_gs_size,
              verbose       = FALSE,
              nPermSimple   = 10000,
              eps           = 0) %>%
              clusterProfiler::filter(p.adjust < alpha),
            otherwise = NULL
          )
        )
      }
    },
    print_gsea_table = function(arranged_with) {
      if(length(compact(self$gsea_result_list)) == 0){
        self$gsea_table <- NULL
        return(NULL)
      }
      empty <- map(self$gsea_result_list, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>%
        as_tibble() %>%
        nrow()
      if(empty == 0) {
        self$gsea_table <- NULL
        return(NULL)
      }
      self$gsea_table <- map(self$gsea_result_list, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>%
        as_tibble() %>%
        mutate(
          across(c("pvalue", "p.adjust", "qvalue"), ~ round(-log10(.), 3)),
          across(c("enrichmentScore", "NES"), ~ round(., 3))
        ) %>%
        relocate(ID) %>%
        rename(geneID = core_enrichment) %>%
        relocate(geneID, .after = last_col()) %>%
        select(-leading_edge) %>%
        arrange(desc(!!sym(arranged_with)))
    },
    plot_gsea_single = function(focus, arrange, show_category) {
      if(is.null(self$gsea_table) || is.null(focus)){return(self$plot_empty_message("No enrichment results."))}
      data <- self$gsea_table %>%
        filter(group == focus)
      if (nrow(data) == 0) {
        return(self$plot_empty_message("No enrichment results."))
      }
      p <- data %>%
        mutate(color = if_else(NES > 0, "#67001f", "#053061")) %>%
        rename(value := !!arrange) %>%
        slice_max(abs(value), n = show_category, with_ties = FALSE) %>%
        arrange(value) %>%
        e_charts(ID, renderer = self$plot_format) %>%
        e_bar(value, bind = Description) %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE) %>%
        e_add_nested("itemStyle", color) %>%
        e_tooltip(
          formatter = JS(
            paste0("function(params){return('<strong>", arrange, ": </strong>' + params.value[0])}")
          )
        ) %>%
        e_x_axis(
          name = arrange,
          nameLocation = "center",
          axisLabel = list(fontSize = self$plot_font_size),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = self$plot_font_size,
            lineHeight = 4 * self$plot_font_size
          )
        ) %>%
        e_y_axis(axisLabel = list(fontSize = 0)) %>%
        e_legend(show = FALSE) %>%
        e_labels(show = TRUE, formatter= '{b}', position = "insideLeft") %>%
        e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
        e_show_loading(text = "Loading...", color = self$primary_color)
      return(p)
    },
    plot_gsea = function(groups, arrange_with, show_n_category) {
      if(is.null(groups)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      if(length(groups) == 1){n_col = 1}else{n_col = 2}
      table <- tibble(
        focus = groups,
        arrange = arrange_with,
        show_category = show_n_category
      )
      p <- table %>%
        mutate(plots_panel = panel_lazy(self$plot_gsea_single)) %>%
        as_trelliscope_df(name = "BarPlot",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>%
        set_default_layout(ncol = n_col)

      return(p)
    },
    plot_gseaplot = function(focus, gene_set_ID) {
      if(is.null(self$gsea_table) || is.null(focus) || length(gene_set_ID) == 0){return(
        ggplot() +
          theme_void() +
          geom_text(aes(0,0,label='Select a term in the table.'), size = 16) +
          xlab(NULL)
      )}
      data <- self$gsea_table %>%
        filter(group == focus)
      if (nrow(data) == 0) {
        return(NULL)
      }
      plot_title <- data %>%
        filter(ID == gene_set_ID) %>%
        pull(Description) %>%
        unique()
      gseaplot2(
        self$gsea_result_list[[focus]],
        geneSetID = gene_set_ID,
        title = plot_title
      )
    },
    download_excel = function(table, name, handler) {
      header_style <- createStyle(
        fontSize = 12,
        fontColour = "#0f0f0f",
        fgFill = "#faf2ca",
        halign = "center",
        border = "TopBottomLeftRight")
      body_style <- createStyle(
        halign = "center",
        border = "TopBottomLeftRight")
      excel <- createWorkbook()
      addWorksheet(excel, sheetName = name, gridLines = F)
      writeDataTable(excel, sheet = name, x = table, keepNA = T, na.string = "NaN")
      n_row <- table %>% nrow() + 1
      n_col <- table %>% ncol()
      setColWidths(excel, sheet = name, cols = 1:n_col, widths = 21)
      addStyle(excel, sheet = name, style = header_style, rows = 1, cols = 1:n_col, gridExpand = T)
      addStyle(excel, sheet = name, style = body_style, rows = 2:n_row, cols = 1:n_col, gridExpand = T)
      saveWorkbook(excel, handler, overwrite = T)
    },
    download_table = function(handler_file, table_type, table_extension, extra_columns) {
      table <- switch(
        table_type,
        "Filtered" = self$print_table(self$filtered_data, df = TRUE),
        "Normalized" = self$print_table(self$normalized_data, df = TRUE),
        "Imputed" = self$print_table(self$imputed_data, df = TRUE),
        "Ranked" = self$print_rank_table(),
        "Volcano" = self$print_stat_table(),
        "Heatmap" = self$print_anova_table(),
        "Nodes" = self$print_nodes(isolate_nodes = FALSE, score_thr = 0),
        "Edges" = self$print_edges(selected_nodes = NULL, score_thr = 0),
        "ORA" = self$ora_table,
        "GSEA" = self$gsea_table
      )
      if(is.null(table)) {return(NULL)}
      if(table_type %in% c("Nodes", "Edges", "ORA", "GSEA")) {extra_columns <- NULL}
      if(!is.null(extra_columns)) {
        extra <- self$raw_data_unique %>%
          select(gene_names, all_of(extra_columns))
        table <- left_join(table, extra, by = "gene_names")
      }
      switch(
        table_extension,
        ".xlsx" = self$download_excel(table, table_type, handler_file),
        ".csv" = write.csv(table, handler_file),
        ".tsv" = write.table(table, handler_file, sep = "\t", row.names = FALSE, quote = FALSE)
      )
    },
    download_parameters = function(handler_file, r6class) {
      tmp <- sapply(r6class, class)
      slots <- tmp[!tmp %in% c("environment", "function")]
      params <- imap(slots, ~ {r6class[[.y]]})
      list.save(params, handler_file)
    }
  ))

