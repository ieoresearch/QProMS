box::use(
  R6[R6Class],
  data.table[fread],
  utils[head, combn, modifyList],
  dplyr[`%>%`, n_distinct, group_by, summarise, n, filter, mutate, ungroup, row_number, select, across, where, all_of, na_if, left_join, rename, if_else, case_when, full_join, relocate, inner_join, distinct, count, everything, pull, arrange, slice_head, slice_tail, last_col, rename_with, ends_with, desc, rename_at, vars, bind_rows, group_map, rowwise, if_all],
  tidyr[drop_na, pivot_longer, pivot_wider, expand_grid, unite, separate_rows, unnest_wider, nest],
  purrr[map, map2, set_names, imap, keep_at, flatten_chr, reduce, map_chr, map_dbl],
  stringr[str_detect, word, str_replace_all, str_extract, str_replace, str_split_1, str_remove, str_which, str_flatten],
  tibble[tibble, as_tibble, column_to_rownames, rownames_to_column, enframe, deframe],
  vsn[vsn2, predict],
  limma[lmFit, eBayes, topTable],
  stats[sd, runif, rnorm, prcomp, cor, na.omit, t.test, p.adjust, wilcox.test, model.matrix, aov, hclust, dist, cutree, as.dendrogram, median, qt],
  rbioapi[rba_string_interactions_network],
  OmnipathR[get_complex_genes, import_omnipath_complexes],
  viridis[viridis],
  htmlwidgets[JS],
  reactable[reactable, colDef],
  rhandsontable[rhandsontable, hot_col],
  trelliscope[panel_lazy, as_trelliscope_df, set_default_layout, add_trelliscope_resource_path],
  heatmaply[heatmaply],
  plotly[plotly_empty, config, layout],
  echarts4r[e_charts, e_bar, e_x_axis, e_y_axis, e_tooltip, e_legend, e_grid, e_color, e_toolbox_feature, e_show_loading, e_boxplot, e_histogram, e_data, e_scatter, e_scatter_3d, e_x_axis_3d, e_y_axis_3d, e_z_axis_3d, e_correlations, e_visual_map, e_title, e_mark_point, e_add_nested, e_group, e_line, e_band2, e_graph, e_graph_nodes, e_graph_edges, e_labels, e_draft]
)

box::use(
  app/static/inputs_type_lists
)

#' @export
QProMS <- R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    raw_data = NULL,
    identify_table_status = NULL,
    parameters_loaded = FALSE,
    input_file_name = NULL,
    path = NULL,
    data = NULL,
    input_type = NULL,
    intensity_type = NULL,
    external_genes_column = NULL,
    log_transform = TRUE,
    organism = NULL, 
    expdesign = NULL,
    plot_format = "svg",
    palette = "D",
    color_palette = NULL,
    is_ok = TRUE, #
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
    imputed_data = NULL,
    imp_methods = "mixed",
    imp_shift = 1.8,
    imp_scale = 0.3,
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
    all_test_combination = NULL, 
    contrasts = NULL,
    primary_condition = NULL,#
    additional_condition = NULL,#
    univariate = NULL,
    univariate_test_type = NULL,
    univariate_paired = NULL,
    stat_table = NULL,
    univariate_alpha = 0.05,
    univariate_p_adj_method = "BH",
    fold_change = 1,
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
    ora_result_list_simplified = NULL,
    ora_table = NULL,
    ora_table_all_download = NULL,
    ora_table_counts = NULL,
    go_ora_from_statistic = NULL,
    go_ora_tested_condition = NULL,
    go_ora_alpha = 0.05,
    go_ora_p_adj_method = "BH",
    go_ora_term = "BP",
    go_ora_focus = NULL,
    go_ora_top_n = 10,
    go_ora_simplify_thr = NULL,
    go_ora_plot_value = "fold_enrichment",
    #######################
    # parameters for GSEA #
    gsea_result_list = NULL,
    gsea_result_list_simplified = NULL,
    protein_rank_by_cond_gsea = FALSE,
    protein_rank_target_gsea = NULL,
    gsea_table = NULL,
    gsea_table_all_download = NULL,
    gsea_table_counts = NULL,
    go_gsea_tested_condition = NULL,
    go_gsea_alpha = 0.05,
    go_gsea_p_adj_method = "BH",
    go_gsea_term = "BP",
    go_gsea_focus = NULL,
    go_gsea_top_n = 10,
    go_gsea_common_terms = FALSE,
    go_gsea_simplify_thr = 0.7,
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
    loading_data = function(input_path, input_name) {
      
      self$raw_data <- fread(input = input_path) 
      
      self$path <- input_path
      
      self$input_file_name <- input_name
    },
    loading_patameters = function(input_path) {
      
      parameters_list <- read_yaml(file = input_path)
      
      self$expdesign <- as_tibble(parameters_list$expdesign)
      self$input_file_name <- parameters_list$input_file_name
      ## for wrangling data page
      self$valid_val_filter <- parameters_list$valid_val_filter
      self$valid_val_thr <- parameters_list$valid_val_thr
      self$norm_methods <- parameters_list$norm_methods
      self$pep_filter <- parameters_list$pep_filter
      self$pep_thr <- parameters_list$pep_thr
      self$rev <- parameters_list$rev
      self$cont <- parameters_list$cont
      self$oibs <- parameters_list$oibs
      ## for missing data page
      self$imp_methods <- parameters_list$imp_methods
      self$imp_shift <- parameters_list$imp_shift
      self$imp_scale <- parameters_list$imp_scale
      ## for protein rank page
      self$protein_rank_target <- parameters_list$protein_rank_target
      self$protein_rank_by_cond <- parameters_list$protein_rank_by_cond
      self$protein_rank_selection <- parameters_list$protein_rank_selection
      self$protein_rank_top_n <- parameters_list$protein_rank_top_n
      ## for univariate page
      self$univariate_test_type <- parameters_list$univariate_test_type
      self$univariate_paired <- parameters_list$univariate_paired
      self$fold_change <- parameters_list$fold_change
      self$univariate_alpha <- parameters_list$univariate_alpha
      self$univariate_p_adj_method <- parameters_list$univariate_p_adj_method
      self$primary_condition <- parameters_list$primary_condition
      self$additional_condition <- parameters_list$additional_condition
      ## for multivariate page
      self$anova_alpha <- parameters_list$anova_alpha
      self$z_score <- parameters_list$z_score
      self$anova_p_adj_method <- parameters_list$anova_p_adj_method
      self$anova_clust_method <- parameters_list$anova_clust_method
      self$clusters_number <- parameters_list$clusters_number
      ## for network page
      self$pdb_database <- parameters_list$pdb_database
      self$network_score_thr <- parameters_list$network_score_thr
      
      self$parameters_loaded <- TRUE
      
    },
    table_raw_data = function() {
      t <- self$raw_data %>% 
        head(15) %>% 
        reactable(
          wrap = FALSE,
          striped = TRUE,
          resizable = TRUE,
          compact = TRUE,
          height = "auto",
          paginationType = "simple",
          showPageInfo = FALSE,
          defaultPageSize = 15
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
      self$identify_table_status <- "danger"
      self$input_type <- "External"
      return(list(status = "danger", messages = error_messages))
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
        
        total_values <- length(unlist(data[, ..intensity_cols]))
        missing_values <- sum(is.na(data[, ..intensity_cols]) | data[, ..intensity_cols] == 0 | data[, ..intensity_cols] == "")
        
        percent_missing <- round((missing_values / total_values) * 100, 1)
        return(percent_missing)
      })
      
      summary_table <- data.frame(
        Metric = c("N° of Proteins", paste("N° of duplicate or missing", required_columns), paste("Missing data '%' in:", intensity_patterns)),
        Value = c(num_rows, non_unique_counts, missing_values_counts)
      )
      
      # Rimuovere le righe con valori NA
      summary_table <- summary_table[!is.na(summary_table$Value), ]
      
      # Rimuovere i nomi delle righe
      rownames(summary_table) <- NULL
      
      return(reactable(summary_table))
    },
    make_expdesign = function(intensity_type) {
      
      if(self$identify_table_status == "success") {
        intensity_cols <- grep(intensity_type, colnames(self$raw_data), value = TRUE, ignore.case = FALSE)
      } else {
        if(is.null(intensity_type)){intensity_type <- ""}
        intensity_cols <- intensity_type
      }
      
      table <- tibble::tibble("keep" = TRUE, "condition" = "", "key" = intensity_cols) %>% 
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
        } else {
          results$condition_check <- "danger The 'condition' column does not contains at least 2 groups with at least 3 replicates each."
          validation_status <- FALSE
        }
        
        groups_with_less_than_3 <- condition_groups %>%
          filter(count < 3)
        
        if (nrow(groups_with_less_than_3) > 0) {
          results$condition_warning <- paste("warning The following groups have less than 3 replicates:", 
                                             paste(groups_with_less_than_3$condition, collapse = ", "))
        }
      }
      
      results$validation_status <- validation_status
      return(results)
    },
    add_replicate_and_label = function(data) {
      # Aggiungi la colonna replicate con numeri crescenti per ogni gruppo di condition
      data <- data %>%
        group_by(condition) %>%
        mutate(replicate = row_number()) %>%
        ungroup()
      
      # Aggiungi la colonna label combinando condition e replicate con un trattino
      table <- data %>%
        mutate(label = paste(condition, replicate, sep = "_"))
      
      self$expdesign <- table
      rtable <- table %>% 
        select(-key) %>% 
        reactable(
          wrap = FALSE,
          striped = TRUE,
          resizable = TRUE,
          compact = TRUE,
          height = "auto",
          paginationType = "simple",
          defaultColDef = colDef(align = "center"),
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
    define_colors = function() {
      n_of_color <- max(self$expdesign %>% distinct(condition) %>% nrow())
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
    },
    define_tests = function() {
      conditions <- unique(self$expdesign$condition)
      
      self$all_test_combination <-
        expand_grid(cond1 = conditions, cond2 = conditions) %>%
        filter(cond1 != cond2) %>%
        mutate(test = paste0(cond1, "_vs_", cond2)) %>%
        pull(test)
    },
    preprocessing = function() {
      
      intensity_cols <- self$expdesign$key
      self$define_colors()
      self$define_tests()
      
      # Trasformare le colonne di intensità con log2()
      if(self$log_transform) {
        initial_table <- self$raw_data %>%
          mutate(across(all_of(intensity_cols), log2)) %>% 
          mutate(across(all_of(intensity_cols), ~ na_if(.,-Inf)))
      } else {
        initial_table <- self$raw_data
      }

      if(self$input_type == "External") {
        required_columns <- self$external_genes_column
        gene_col <-self$external_genes_column
        
        # Rendere unica la colonna dei gene name
        initial_table <- initial_table %>%
          filter(.data[[gene_col]] != "") %>% 
          drop_na(all_of(gene_col)) %>% 
          mutate(!!gene_col := str_extract(.data[[gene_col]], "[^;]*")) %>% 
          mutate(!!gene_col := make.unique(.data[[gene_col]], sep = "_"))
      } else {
        required_columns <- inputs_type_lists$metadata_list[[self$input_type]]
        gene_col <- required_columns[1]
        protein_col <- required_columns[2]
        
        # Rendere unica la colonna dei gene name
        initial_table <- initial_table %>%
          mutate(!!gene_col := str_extract(.data[[gene_col]], "[^;]*")) %>% 
          mutate(!!protein_col := str_extract(.data[[protein_col]], "[^;]*")) %>% 
          mutate(!!gene_col := self$make_unique_genes(.data[[gene_col]], .data[[protein_col]]))
      }
      
      # Trasformare la tabella in formato long
      data <- initial_table %>%
        select(all_of(c(required_columns, intensity_cols))) %>% 
        pivot_longer(cols = all_of(intensity_cols), names_to = "key", values_to = "intensity") %>% 
        left_join(self$expdesign, by = "key") %>% 
        rename(gene_names := !!gene_col) %>% 
        mutate(bin_intensity = if_else(is.na(intensity), 0, 1))
      
      self$data <- data
    },
    subset_missing_data = function(valid_val_filter, valid_val_thr) {
      
      ## Different strategies for filtering missing data:
      ## c("alog", "each_grp", "total") 
      ## alog -> at least one group
      
      data <- self$data
      
      filtered_data <- data %>%
        {if(valid_val_filter == "total") 
          group_by(., gene_names) 
          else 
            group_by(., gene_names, condition)
        } %>%
        mutate(
          miss_val = n() - sum(bin_intensity, na.rm = TRUE),
          n_size = n()
        ) %>%
        ungroup() %>%
        group_by(gene_names) %>%
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
      
      filtered_data <- data %>%
        {if (pep_filter == "peptides") 
          filter(., `Peptides` >= pep_thr)
          else if (pep_filter == "unique") 
            filter(., `Unique peptides` >= pep_thr)
          else 
            filter(., `Razor + unique peptides` >= pep_thr)
        }
      
      self$filtered_data <- filtered_data
    },
    subset_contaminant = function(rev, cont, oibs, rescue_cont = NULL) {
      
      data <- self$filtered_data
      
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
      
      self$filtered_data <- cleaned_data
    },
    normalization = function(norm_methods) {
      
      data <- self$filtered_data
      
      if (norm_methods == "None" | nrow(distinct(data, gene_names)) < 42) {
        self$is_norm <- FALSE
        self$normalized_data <- data # questo è da verificare
      } else {
        self$is_norm <- TRUE
        
        # Convert tibble data into a matrix
        raw_matrix <- data %>%
          pivot_wider(id_cols = gene_names, names_from = label, values_from = intensity) %>%
          column_to_rownames("gene_names") %>%
          as.matrix()
        
        set.seed(11)
        # Variance stabilization transformation on matrix
        vsn_fit <- vsn2(2 ^ raw_matrix, verbose = FALSE)
        norm_matrix <- predict(vsn_fit, 2 ^ raw_matrix)
        
        # Return a table with QProMS object format
        normalized_data <- norm_matrix %>%
          as_tibble(rownames = "gene_names") %>%
          pivot_longer(cols = -gene_names, names_to = "label", values_to = "norm_intensity") %>%
          full_join(data, by = c("gene_names", "label")) %>%
          mutate(intensity = norm_intensity) %>%
          select(-norm_intensity) %>%
          relocate(intensity, .after = last_col())
        
        self$normalized_data <- normalized_data
      }
    },
    imputation = function(imp_methods, shift, scale, unique_visual = FALSE) {
      
      data <- self$normalized_data %>% 
        mutate(imputed = if_else(bin_intensity == 1, FALSE, TRUE))
      
      if(imp_methods == "mixed"){
        self$is_mixed <- TRUE
      }else{
        self$is_mixed <- FALSE
      }
      
      if(imp_methods == "mixed" | imp_methods == "perseus"){
        self$is_imp <- TRUE
        
        if(self$is_mixed){
          set.seed(11)
          data_mixed <- data %>%
            rownames_to_column() %>% 
            group_by(gene_names, condition) %>%
            mutate(for_mean_imp = if_else((sum(bin_intensity) / n()) >= 0.75, TRUE, FALSE)) %>%
            filter(for_mean_imp) %>% 
            mutate(random_imp = runif(
              n = 1,
              min = min(intensity, na.rm = TRUE),
              max = max(intensity, na.rm = TRUE)
            )) %>% 
            ungroup() %>% 
            select(rowname, for_mean_imp, random_imp)
          
          data_mixed_final <- data %>% 
            rownames_to_column() %>% 
            left_join(data_mixed, by = "rowname") %>% 
            mutate(
              imp_intensity = case_when(
                bin_intensity == 0 & for_mean_imp ~ random_imp,
                TRUE ~ as.numeric(intensity)
              )
            ) %>%
            mutate(intensity = imp_intensity) %>%
            select(-c(rowname, for_mean_imp, random_imp, imp_intensity))
          
          data <- data_mixed_final
        }
        
        if(unique_visual){
          data_unique <- data %>%
            group_by(gene_names, condition) %>%
            mutate(miss_val = n() - sum(bin_intensity)) %>%
            mutate(n_size = n()) %>%
            ungroup() %>%
            group_by(gene_names) %>%
            filter(any(miss_val <= 0)) %>%
            ungroup() %>%
            filter(miss_val == n_size) %>% 
            mutate(intensity = min(data$intensity, na.rm = TRUE), unique = TRUE) %>% 
            select(-c(bin_intensity, miss_val, n_size)) %>% 
            rename(unique_intensity = intensity) 
          
          data <- data %>% 
            left_join(data_unique, by = c("gene_names", "label", "condition", "replicate")) %>% 
            mutate(unique = if_else(is.na(unique), FALSE, unique)) %>% 
            mutate(intensity = if_else(unique, unique_intensity, intensity)) %>% 
            mutate(bin_intensity = if_else(unique, 1, bin_intensity)) %>% 
            select(-c(unique_intensity, unique)) 
        }
        ## this funcion perform classical Perseus imputation
        ## sice use random nomral distibution i will set a set.seed()
        set.seed(11)
        
        imputed_data <- data %>%
          group_by(label) %>%
          # Define statistic to generate the random distribution relative to sample
          mutate(
            mean = mean(intensity, na.rm = TRUE),
            sd = sd(intensity, na.rm = TRUE),
            n = sum(!is.na(intensity)),
            total = nrow(data) - n
          ) %>%
          ungroup() %>%
          # Impute missing values by random draws from a distribution
          # which is left-shifted by parameter 'shift' * sd and scaled by parameter 'scale' * sd.
          mutate(imp_intensity = case_when(
            is.na(intensity) ~ rnorm(total, mean = (mean - shift * sd), sd = sd * scale),
            TRUE ~ intensity
          )) %>%
          mutate(intensity = imp_intensity) %>%
          select(-c(mean, sd, n, total, imp_intensity))
        
        self$imputed_data <- imputed_data
        
      }else{
        self$is_imp <- FALSE
      }
    },
    rank_protein = function(target, by_condition, selection, n_perc) {
      if(by_condition) {
        data <- self$imputed_data %>%
          filter(condition == target) %>%
          group_by(gene_names) %>%
          summarise(mean_intenisty = mean(intensity)) %>%
          ungroup() %>%
          arrange(-mean_intenisty) %>%
          mutate(rank = rank(-mean_intenisty)) %>% 
          rename(intensity = mean_intenisty) %>% 
          select(gene_names, intensity, rank)
      } else {
        data <- self$imputed_data %>%
          filter(label == target) %>%
          arrange(-intensity) %>%
          mutate(rank = rank(-intensity)) %>% 
          select(gene_names, intensity, rank)
      }
      if(selection == "top") {
        selected_list <- data %>% 
          slice_head(prop = n_perc) %>%
          pull(gene_names)
      } else {
        selected_list <- data %>% 
          slice_tail(prop = n_perc) %>%
          pull(gene_names)
      } 
      self$rank_data <- data
      self$protein_rank_list <- selected_list
    },
    shiny_wrap_workflow = function() {
      self$subset_missing_data(
        valid_val_filter = self$valid_val_filter,
        valid_val_thr = self$valid_val_thr
      )
      if(self$input_type == "MaxQuant") {
        self$subset_peptides(
          pep_filter = self$pep_filter,
          pep_thr = self$pep_thr
        )
        self$subset_contaminant(
          rev = self$rev,
          cont = self$cont,
          oibs = self$oibs
        )
      }
      self$normalization(norm_methods = self$norm_methods)
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
      ## forse questo va fuori dalla funzione
      self$filtered_gene_vector <- self$filtered_data %>% 
        distinct(gene_names) %>% 
        pull(gene_names)
    },
    reactable_interactive = function(table) {
      if(is.null(table)){return(NULL)}
      t <- table %>% 
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          wrap = FALSE,
          height = "auto",
          selection = "multiple",
          paginationType = "simple",
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(6, 12, 18, 24),
          defaultPageSize = 12,
          onClick = "select",
          defaultSelected = 1,
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
        e_show_loading(text = "Loading...", color = "#0d6efd")
    },
    plot_protein_counts = function() {
      
      if(is.null(self$filtered_data)){return(NULL)}
      
      p <- self$filtered_data %>%
        group_by(label) %>%
        summarise(counts = sum(bin_intensity)) %>%
        ungroup() %>%
        inner_join(., self$expdesign, by = "label") %>%
        mutate(replicate = as.factor(replicate)) %>%
        group_by(condition) %>%
        e_charts(replicate, renderer = self$plot_format) %>%
        e_bar(counts, emphasis = list(focus = "series")) %>%
        e_tooltip(trigger = "item") %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        e_x_axis(
          name = "Replicate",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>% 
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
      return(p)
    },
    plot_distribution = function() {
      
      if(is.null(self$normalized_data)){return(NULL)}
      intervals <- length(unique(self$expdesign$replicate))
      
      p <- self$normalized_data %>%
        mutate(intensity = round(intensity, 2)) %>%
        group_by(condition, label) %>%
        e_charts(renderer = self$plot_format) %>%
        e_boxplot(
          intensity,
          colorBy = "data",
          layout = 'horizontal',
          outliers = FALSE,
          itemStyle = list(borderWidth = 3)
        ) %>%
        e_tooltip(trigger = "item") %>%
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        e_x_axis(axisLabel = list(interval = intervals)) %>% 
        e_toolbox_feature(feature = "saveAsImage") %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
      return(p)
    },
    plot_protein_coverage = function() {
      
      if(is.null(self$filtered_data)){return(NULL)}

      p <- self$filtered_data %>%
        group_by(gene_names) %>%
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
        e_color(self$color_palette) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
      return(p)
    },
    plot_cv = function() {
      
      if(is.null(self$normalized_data)){return(NULL)}

      p <- self$normalized_data %>% 
        group_by(gene_names, condition) %>% 
        summarise(
          mean = mean(intensity, na.rm = TRUE),
          sd = sd(intensity, na.rm = TRUE),
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
        e_y_axis(
          name = "Density",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>% 
        e_toolbox_feature(feature = "saveAsImage") %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
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
        e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        e_y_axis(name = "Counts") %>%
        e_tooltip(trigger = "item") %>%
        e_color(c("#0d6efd", "#6c757d")) %>% 
        e_grid(containLabel = TRUE) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
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
      
      p <- data %>%
        mutate(missing_value = if_else(imputed, "Missing", "Valid")) %>%
        mutate(missing_value = factor(missing_value, levels = c("Valid", "Missing"))) %>%
        group_by(missing_value) %>%
        e_charts(renderer = self$plot_format) %>%
        e_histogram(intensity, breaks = pretty(0:40, n = 100)) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%  
        e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          min = 10, 
          max = 40,
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_color(c("#0d6efd", "#6c757d")) %>% 
        e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
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
        set_default_layout(ncol = 1)
      
      
      return(p)
    },
    plot_imputation = function(data, imp_visualization = FALSE) {
      
      if(is.null(data)){return(NULL)}

      alpha_cols <- str_replace(self$color_palette, pattern = "FF", replacement = "B3")
      br <- pretty(0:40, n = 100)
      p <- data %>%
        group_by(condition) %>%
        e_charts(renderer = self$plot_format) %>%
        e_histogram(intensity, breaks = br) %>%
        e_color(alpha_cols) %>%
        e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%  
        e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          min = 10, max = 40,
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
      if(imp_visualization){
        imputed_dist <- self$imputed_data %>% 
          filter(imputed) %>% 
          group_by(condition)
        
        p <- data %>%
          group_by(condition) %>%
          e_charts(renderer = self$plot_format) %>%
          e_histogram(intensity, breaks = br) %>%
          e_color(c(alpha_cols, "#bc3754")) %>%
          e_data(imputed_dist, intensity) %>% 
          e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          e_histogram(intensity, name = "Imputed", breaks = br) %>%
          e_legend(selected = list('Imputed'= TRUE)) %>% 
          e_y_axis(
            name = "Counts",
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 16,
              lineHeight = 60
            )
          ) %>%  
          e_x_axis(
            name = "log2 Intensity",
            nameLocation = "center",
            min = 10, max = 40,
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          e_grid(containLabel = TRUE) %>%
          e_show_loading(text = "Loading...", color = "#0d6efd")
      }
      return(p)
    },
    print_table = function(data, df = FALSE) {
     
      if(is.null(data)){return(NULL)}
      
      table <- data %>% 
        select(gene_names, label, intensity) %>% 
        mutate(intensity = round(intensity, 2)) %>% 
        pivot_wider(id_cols = gene_names, names_from = label, values_from = intensity)
      
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
              gene_names = colDef(
                name = "Gene names",
                sticky = "left",
                style = list(borderRight  = "1px solid #eee"
                )
              )
            )
          )
      } 
      return(table)
    },
    plot_pca = function(view_3d = FALSE) {
      
      if(is.null(self$imputed_data)){return(NULL)}
      
      ## generate a matrix from imputed intensiy
      mat <- self$imputed_data %>%
        select(gene_names, label, intensity) %>%
        pivot_wider(id_cols = "gene_names",
                    names_from = "label",
                    values_from = "intensity") %>%
        column_to_rownames("gene_names") %>%
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
          e_x_axis(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          e_y_axis(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>% 
          e_color(self$color_palette) %>% 
          e_grid(containLabel = TRUE) %>%
          e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          e_show_loading(text = "Loading...", color = "#0d6efd")
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
          e_x_axis_3d(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          e_y_axis_3d(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          e_z_axis_3d(
            name = paste0("PC3 - ", pca_var_perc[3], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          e_legend() %>%
          e_color(self$color_palette) %>% 
          e_grid(containLabel = TRUE) %>%
          e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          e_show_loading(text = "Loading...", color = "#0d6efd")
      }
      return(p)
    },
    plot_correlation = function() {
      
      if(is.null(self$normalized_data)){return(NULL)}
      
      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }
      
      color <- viridis(n = 3, direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      mat <- data %>%
        select(gene_names, label, intensity) %>%
        pivot_wider(id_cols = gene_names, names_from = label, values_from = intensity) %>%
        filter(if_all(.cols = everything(), .fns = ~ !is.na(.x))) %>%
        column_to_rownames("gene_names") %>%
        cor(method = self$cor_method) %>% 
        round(digits = 2)
      
      p <- mat %>% 
        e_charts(renderer = self$plot_format) %>%
        e_correlations(order = "hclust", visual_map = FALSE) %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
        e_y_axis(axisLabel = list(interval = 0, rotate = 0), position = "right") %>%
        e_tooltip(trigger = "item", formatter = JS("
          function(params){
          return('X: ' + params.value[0] + '<br />Y: ' + params.value[1] + '<br />Value: ' + params.value[2])
          }")) %>%
        e_visual_map(
          min = min(mat),
          max = 1,
          bottom = 150,
          precision = 2,
          inRange = list(color = color)
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_show_loading(text = "Loading...", color = "#0d6efd") %>% 
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
        select(gene_names, label, intensity) %>%
        pivot_wider(id_cols = gene_names, names_from = "label", values_from = "intensity") %>% 
        select(gene_names, x = !!x, y = !!y)
      
      min_plot <- round(min(data_scatter %>% select(-gene_names), na.rm = TRUE) - 1, 0)
      max_plot <- round(max(data_scatter %>% select(-gene_names), na.rm = TRUE) + 1, 0)
      value <- round(cor(data_scatter$x, data_scatter$y), 2)
      
      p <- data_scatter %>%
        e_charts(x, dispose = FALSE) %>%
        e_scatter(y, legend = FALSE, symbol_size = 5, bind = gene_names) %>%
        e_x_axis(min = min_plot, max = max_plot) %>%
        e_y_axis(min = min_plot, max = max_plot) %>%
        e_color(self$color_palette) %>%
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
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        e_x_axis(
          name = y,
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_title(paste0("correlation: ", value), left = "center") %>%  
        e_toolbox_feature(feature = "saveAsImage")
      
      if (highlights_names != "") {
        for (name in str_split_1(highlights_names, ":")) {
          highlights_name <- data_scatter %>%
            filter(gene_names == name) %>%
            select(xAxis = x,
                   yAxis = y,
                   value = gene_names) %>% 
            as.list()
          
          p <- p %>%
            e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(color = "black", fontWeight = "normal", fontSize = 16),
              itemStyle = list(color = "#0d6efd",  borderColor = "#0d6efd", borderWidth = 0.2)
            )
        }
      }
      return(p)
    },
    plot_multi_scatter = function(gene_names_h) {
      
      if(is.null(self$normalized_data)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      combinations <- t(combn(self$expdesign %>% pull(label), 2))
      colnames(combinations) <- c("x", "y")
      combinations <- as_tibble(combinations)
      if(is.null(gene_names_h)){
        names <- ""
      } else {
        names <- paste(gene_names_h, collapse = ":")
      }
      p <- combinations %>% 
        mutate(highlights_names = names) %>%
        mutate(plots_panel = panel_lazy(self$plot_single_scatter)) %>% 
        as_trelliscope_df(name = "Scatter",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>% 
        set_default_layout(ncol = 1)
      
      return(p)
    },
    plot_protein_rank = function(highlights_names = NULL) {
      
      if(is.null(self$rank_data)){return(NULL)}
      colors <- viridis(n = 2 , direction = 1, end = 0.90, begin = 0.10, option = self$palette)
      
      p <- self$rank_data %>%
        mutate(color = if_else(gene_names %in% self$protein_rank_list, colors[2], colors[1])) %>%
        e_charts(rank, renderer = self$plot_format) %>%
        e_scatter(intensity,
                  legend = FALSE,
                  symbol_size = 5,
                  bind = gene_names) %>%
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
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        e_x_axis(
          name = "Protein Rank",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        e_title(self$protein_rank_target, left = "center") %>%
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
                color = "#0d6efd",
                borderColor = "#0d6efd",
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
        e_x_axis(name = "Difference (fold change)", nameLocation = "center", nameTextStyle = list(fontWeight = "bold", fontSize = 15, lineHeight = 50)) %>%
        e_y_axis(name = "-log10 p-value", nameLocation = "center", nameTextStyle = list(fontWeight = "bold", fontSize = 15, lineHeight = 50)) %>%
        e_grid(containLabel = TRUE) %>%
        e_group("grp") %>%
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
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
              itemStyle = list(color = "#0d6efd",  borderColor = "#0d6efd", borderWidth = 0.2)
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
        as_trelliscope_df(name = "Volcanos",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>% 
        set_default_layout(ncol = 1)
      
      return(p)
    },
    plot_stat_profile_single = function(contrast, gene) {
      
      data <- if (self$is_imp) self$imputed_data else self$normalized_data
      cond <- unique(str_split_1(contrast, "_vs_"))
      
      p <- data %>% 
        filter(gene_names == gene) %>% 
        filter(condition %in% cond) %>%
        group_by(condition) %>%
        drop_na() %>% 
        e_charts(renderer = self$plot_format) %>%
        e_boxplot(
          intensity,
          colorBy = "data",
          outliers = FALSE,
          itemStyle = list(borderWidth = 2)
        ) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        e_x_axis(
          axisLabel = list(
            fontWeight = "bold",
            fontSize = 16
          )) %>% 
        e_grid(containLabel = TRUE) %>%
        e_color(self$color_palette) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) 
      
      return(p)
    },
    plot_stat_profile = function(tests, genes) {
      if(is.null(genes)){return(NULL)}
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
        as_trelliscope_df(name = "Profile Plot",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>% 
        set_default_layout(ncol = 1)
      
      return(p)
    },
    stat_anova = function(alpha, p_adj_method) {
      data <- if (self$is_imp) self$imputed_data else self$normalized_data
      
      # Rimuovere i geni con valori NA
      data <- data %>% 
        group_by(gene_names) %>%
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
      
      if (nrow(mat_base) < self$clusters_number) {
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
        group_by(cluster, gene_names) %>%
        arrange(factor(label, levels = self$anova_col_order)) %>%
        e_charts(label, renderer = self$plot_format) %>%
        e_line(intensity, bind = gene_names) %>%
        e_color(sub_alpha_cols) %>%
        e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        e_grid(containLabel = TRUE) %>%
        e_tooltip() %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      
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
        e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        e_grid(containLabel = TRUE) %>%
        e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>% 
        e_show_loading(text = "Loading...", color = "#0d6efd")
      return(p)
    },
    plot_cluster_profile = function() {
      if(is.null(self$anova_table)){return(NULL)}
      ## create the resouce path for trelliscope
      tr_dir <- tempfile()
      dir.create(tr_dir)
      add_trelliscope_resource_path("trelliscope", tr_dir)
      
      clusters <- self$anova_table %>% distinct(cluster) %>% filter(cluster != "not_defined") %>% pull()
      colors <- viridis::viridis(n = length(clusters), option = self$palette)
      alpha_colors <- str_replace(colors, pattern = "FF", replacement = "E6")
      table <- tibble(clust_name = clusters, clust_color = alpha_colors)
      p <- table %>% 
        mutate(plots_panel = panel_lazy(self$plot_cluster_profile_single)) %>% 
        as_trelliscope_df(name = "Profile Plot",
                          path = file.path(tr_dir, "test"),
                          jsonp = FALSE) %>% 
        set_default_layout(ncol = 1)
      
      return(p)
    },
    make_nodes = function(list_from, focus, direction) {
      if (list_from == "univariate") {
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
      if(self$organism == "human"){tax_id <- 9606} else {tax_id <- 10090}
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
        e_toolbox_feature(feature = "saveAsImage") 
      
      if (show_names) {
        p <- p %>%
          e_labels(fontSize = 10)
      }
      
      p$x$opts$series[[1]]$links <- map2(p$x$opts$series[[1]]$links, edges$color, ~ modifyList(.x, list(lineStyle = list(color = .y))))
      
      if (list_from == "univariate") {
        p$x$opts$series[[1]]$data <- map2(p$x$opts$series[[1]]$data, nodes$color, ~ modifyList(.x, list(itemStyle = list(color = .y))))
      }
      
      if (!is.null(selected) && !filtered) {
        p$x$opts$series[[1]]$data <- map(p$x$opts$series[[1]]$data, function(node) {
          if (node$name %in% selected) {
            node$itemStyle$borderColor <- "gray20"
            node$itemStyle$color <- "#ffc107"
            node$itemStyle$borderWidth <- 2
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
    }
  )
)