#' Extract Raw Data from Stat Object
#'
#' This function extracts the 'raw.data' slot from an object of class 'Stat'.
#' If the object is not of class 'Stat' or does not contain a 'raw.data' slot,
#' it will return NULL.
#'
#' @param object An object of class 'Stat' which contains a slot named 'raw.data'.
#'               This should be a valid Stat object.
#'
#' @returns Returns the 'raw.data' slot of the Stat object if it exists.
#'          If the object does not have a 'raw.data' slot or the slot is empty,
#'          it returns NULL.
#'
#' @export
#'
#' @examples
#' # Assuming 'stat_object' is a valid Stat object
#' raw_data <- ExtractRawData(stat_object)
#'
#' # If the object does not have raw.data, it will return NULL
#' missing_data <- ExtractRawData(non_stat_object)
ExtractRawData <- function(object) {
  data <- tryCatch(slot(object, "raw.data"), error = function(e) NULL)
  return(data)
}



#' Diagnose Variable Types in Data
#'
#' This function analyzes a data frame to classify variables into numeric,
#' categorical, and variables that need encoding based on their unique values.
#' It also ensures that the specified grouping column (if provided) is excluded from
#' the analysis.
#'
#' @param data A data frame that contains the data to be analyzed. Each column represents a variable.
#' @param group_col A character string specifying the name of the grouping column (default is "group").
#'                  This column will be excluded from the analysis.
#' @param max_unique_values A numeric value specifying the maximum number of unique values a column can have
#'                           to be considered as categorical. Columns with fewer unique values than this threshold
#'                           will be treated as categorical variables (default is 5).
#'
#' @returns A list containing three elements:
#'   - `numeric_vars`: A character vector of numeric variables.
#'   - `categorical_vars`: A character vector of categorical variables.
#'   - `vars_to_encode`: A character vector of categorical variables that have more than 2 unique values
#'                       and should be encoded.
#'
#' @export
#'
#' @examples
#' # Example 1: Diagnose variables in a data frame
#' result <- diagnose_variable_type(data_frame, group_col = "group", max_unique_values = 5)
#' print(result)
#'
#' # Example 2: Diagnose variables without a grouping column
#' result_no_group <- diagnose_variable_type(data_frame, group_col = NULL)
diagnose_variable_type <- function(data,
                                   group_col = "group",
                                   max_unique_values = 5) {
  numeric_vars <- vector("list")
  categorical_vars <- vector("list")
  vars_to_encode <- vector()
  is_group_col_present <- !is.null(group_col) && group_col %in% names(data)
  for (col in names(data)) {
    if (!is_group_col_present || col != group_col) {
      unique_values <- length(unique(data[[col]]))
      if (unique_values <= max_unique_values) {
        categorical_vars[[col]] <- col
        if (unique_values > 2) {
          vars_to_encode <- c(vars_to_encode, col)
        }
      } else if (is.numeric(data[[col]])) {
        numeric_vars[[col]] <- col
      }
    }
  }
  numeric_vars <- unlist(numeric_vars)
  categorical_vars <- unlist(categorical_vars)
  return(list(numeric_vars = numeric_vars,
              categorical_vars = categorical_vars,
              vars_to_encode = vars_to_encode))
}

#' Diagnose Variable Types for 'Stat' Objects or Data Frames
#'
#' This function analyzes the variable types (numeric, categorical, and those needing encoding)
#' of a data frame or an object of class "Stat". If the input is a "Stat" object, it extracts
#' the raw data and group column from the object. If the input is a data frame, it directly uses
#' the provided data for diagnosis. It updates the "Stat" object with the diagnosed variable types.
#'
#' @param object An object of class "Stat" or a data frame. If the object is of class "Stat",
#'               the raw data and group column will be extracted from the object.
#'               If it is a data frame, the function directly operates on the data.
#' @param group_col A character string specifying the name of the grouping column (default is "group").
#'                  This column will be excluded from the analysis if present.
#' @param max_unique_values A numeric value specifying the maximum number of unique values a column can have
#'                           to be considered as categorical. Columns with fewer unique values than this threshold
#'                           will be treated as categorical variables (default is 5).
#'
#' @returns If the input is a "Stat" object, the updated object with the diagnosed variable types
#'          in the "variable.types" slot. If the input is a data frame, a list containing:
#'   - `numeric_vars`: A character vector of numeric variables.
#'   - `categorical_vars`: A character vector of categorical variables.
#'   - `vars_to_encode`: A character vector of categorical variables that have more than 2 unique values
#'                       and should be encoded.
#'
#' @export
#'
#' @examples
#' # Example 1: Diagnose variables in a "Stat" object
#' stat_obj <- stat_diagnose_variable_type(stat_object, group_col = "group")
#' print(stat_obj)
#'
#' # Example 2: Diagnose variables in a data frame
#' result <- stat_diagnose_variable_type(data_frame)
#' print(result)
stat_diagnose_variable_type <- function(object,
                                        group_col = "group",
                                        max_unique_values = 5) {

  if (inherits(object, "Stat")) {
    group_col = slot(object, "group_col")
    if (length(group_col) == 0) {
      group_col <- NULL
    }
    data <- slot(object, "raw.data")
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }


  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)


  cat("Diagnosed variable types:\n")
  cat("Numeric variables:", length(variable_types$numeric_vars), "\n")
  cat("Categorical variables:", length(variable_types$categorical_vars), "\n")

  if (length(variable_types$numeric_vars) == 0 && length(variable_types$categorical_vars) == 0) {
    stop("No valid variables found after variable type diagnosis")
  }


  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")

    object@variable.types <- variable_types
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'variable.types' slot updated.\n")
    return(object)
  }

  return(variable_types)
}

#' Remove Variables and Samples with Excessive Missing Data
#'
#' This function removes variables and samples from the input data frame where the percentage of missing
#' values exceeds the specified threshold. Variables with missing data above the threshold will be removed,
#' and the same applies to samples (rows) where the percentage of missing data exceeds the threshold.
#'
#' @param data A data frame containing the dataset to be processed.
#' @param miss_threshold A numeric value between 0 and 100 specifying the threshold (in percentage) for
#'                       missing data. Variables or samples with missing data above this threshold will
#'                       be removed (default is 20).
#'
#' @returns A data frame with variables and samples removed if their missing data exceeds the given threshold.
#'
#' @export
#'
#' @examples
#' # Example 1: Remove variables and samples with more than 20% missing data
#' cleaned_data <- remove_high_missing(my_data, miss_threshold = 20)
#'
#' # Example 2: Remove variables and samples with more than 10% missing data
#' cleaned_data_10 <- remove_high_missing(my_data, miss_threshold = 10)
remove_high_missing <- function(data, miss_threshold = 20) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  if (!is.numeric(miss_threshold) || miss_threshold < 0 || miss_threshold > 100) {
    stop("miss_threshold must be a number between 0 and 100.")
  }
  cat("Removing variables and samples with missing data above the threshold...\n")

  data[data == '<NA>'] <- NA
  var_missing_percentage <- colMeans(is.na(data)) * 100
  sample_missing_percentage <- rowMeans(is.na(data)) * 100


  high_missing_vars <- names(var_missing_percentage[var_missing_percentage >= miss_threshold])
  high_missing_samples <- which(sample_missing_percentage >= miss_threshold)


  original_rownames <- rownames(data)

  data <- data[, !names(data) %in% high_missing_vars, drop = FALSE]
  data <- data[-high_missing_samples, , drop = FALSE]

  if (nrow(data) > 0) {
    rownames(data) <- original_rownames[-high_missing_samples][1:nrow(data)]
  } else {
    rownames(data) <- NULL
  }

  cat("Remaining data after high missing values removal: ", nrow(data), " samples, ", ncol(data), " variables\n")

  return(data)
}



#' Impute Missing Values in a Data Frame
#'
#' This function imputes missing values in a data frame using either multiple imputation via MICE or
#' simple median and mode imputation. The method to be used is specified by the `impute_method` parameter.
#' Numeric and categorical variables are handled accordingly, with numeric values imputed using the median
#' and categorical values imputed using the mode.
#'
#' @param data A data frame containing the dataset to be imputed.
#' @param group_col A string specifying the name of the grouping column (default is "group"). This column is
#'                  excluded from the imputation process.
#' @param impute_method A string specifying the imputation method to be used. Options are:
#'                      "mice" (multiple imputation using MICE) or "median_mode" (simple median for numeric
#'                      and mode for categorical variables). Default is "mice".
#' @param m An integer specifying the number of imputed datasets when using the MICE method (default is 5).
#' @param max_unique_values An integer specifying the maximum number of unique values a variable can have
#'                           to be considered categorical. Default is 5.
#'
#' @returns A data frame with missing values imputed according to the selected method.
#'
#' @export
#'
#' @examples
#' # Example 1: Impute missing values using the MICE method with 5 imputed datasets
#' imputed_data <- impute_missing_values(my_data, impute_method = "mice", m = 5)
#'
#' # Example 2: Impute missing values using the median and mode imputation method
#' imputed_data <- impute_missing_values(my_data, impute_method = "median_mode")
impute_missing_values <- function(data,
                                  group_col = "group",
                                  impute_method = "mice",
                                  m = 5,
                                  max_unique_values = 5) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")

  impute_method <- match.arg(impute_method)

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  numeric_vars <- variable_types$numeric_vars
  categorical_vars <- variable_types$categorical_vars

  calculate_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  mic_median_impute <- function(data, m) {
    cat("Performing multiple imputation using MICE...\n")

    imp_data <- mice(data, m = m, method = 'pmm', maxit = 5, seed = 123)
    imp_data_data <- complete(imp_data, action = 3)

    cat("Imputed data (MICE) generated. Now performing median imputation...\n")

    imp_data_data[] <- lapply(imp_data_data, function(x) {
      if (is.numeric(x)) {
        ifelse(is.na(x), median(x, na.rm = TRUE), x)
      } else {
        ifelse(is.na(x), calculate_mode(x), x)
      }
    })

    for (col in categorical_vars) {
      imp_data_data[[col]] <- as.factor(imp_data_data[[col]])
    }

    return(imp_data_data)
  }

  if (impute_method == "mice") {
    cat("Using MICE imputation method...\n")
    data <- mic_median_impute(data, m)
  } else if (impute_method == "median_mode") {
    cat("Using simple median and mode imputation method...\n")

    for (col in categorical_vars) {
      mode_val <- calculate_mode(data[[col]])
      data[[col]][is.na(data[[col]])] <- mode_val
    }

    for (col in numeric_vars) {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }
  } else {
    stop("Invalid impute_method. Choose either 'mice' or 'median_mode'.")
  }

  cat("Imputation completed. Returning the processed data...\n")

  rownames(data) <- rownames(data)

  return(data)
}



#' Process Missing Values in a Data Frame or 'Stat' Object
#'
#' This function processes missing values in a data frame or 'Stat' object by first removing variables and
#' samples with excessive missing data, then imputing the remaining missing values using the selected method.
#' It supports both multiple imputation via MICE and simple median and mode imputation. The function updates
#' the 'Stat' object if the input is of class 'Stat' or returns the processed data frame.
#'
#' @param object A 'Stat' object or a data frame containing the data to be processed.
#' @param m An integer specifying the number of imputed datasets when using the MICE method (default is 5).
#' @param impute_method A string specifying the imputation method to be used. Options are:
#'                      "mice" (multiple imputation using MICE) or "median_mode" (simple median for numeric
#'                      and mode for categorical variables). Default is "mice".
#' @param group_col A string specifying the name of the grouping column (default is "group"). This column is
#'                  excluded from the imputation process.
#' @param miss_threshold A numeric value specifying the missing data threshold for variables and samples
#'                       (default is 20%). Variables or samples with missing data above this threshold are
#'                       removed.
#' @param max_unique_values An integer specifying the maximum number of unique values a variable can have to
#'                           be considered categorical. Default is 5.
#'
#' @returns A processed data frame with missing values handled, or a 'Stat' object with the updated clean data.
#'
#' @export
#'
#' @examples
#' # Example 1: Process missing values in a data frame using MICE with 5 imputed datasets
#' processed_data <- stat_miss_processed(my_data, impute_method = "mice", m = 5)
#'
#' # Example 2: Process missing values in a data frame using median and mode imputation
#' processed_data <- stat_miss_processed(my_data, impute_method = "median_mode")
stat_miss_processed <- function(object,
                                m = 5,
                                impute_method ="mice",
                                group_col = "group",
                                miss_threshold = 20,
                                max_unique_values = 5) {
  impute_method <- match.arg(impute_method)

  if (inherits(object, "Stat")) {
    data <- slot(object, "raw.data")
    group_col <- slot(object, "group_col")
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }


  clean_drop_data <- remove_high_missing(data, miss_threshold)

  if (ncol(clean_drop_data) == 0 || nrow(clean_drop_data) == 0) {
    stop("No data remains after removing high-missing variables/samples")
  }

  # Diagnose variable types (numeric and categorical)
  variable_types <- diagnose_variable_type(clean_drop_data, group_col = group_col, max_unique_values = max_unique_values)
  if (length(variable_types$numeric_vars) == 0 && length(variable_types$categorical_vars) == 0) {
    stop("No valid variables found after variable type diagnosis")
  }


  clean_miss_data <- impute_missing_values(clean_drop_data,
                                           group_col = group_col,
                                           impute_method = impute_method,  # Now passing the method as a string
                                           m = m)

  if (ncol(clean_miss_data) == 0 || nrow(clean_miss_data) == 0) {
    stop("No data remains after missing value imputation")
  }

  rownames(clean_miss_data) <- rownames(clean_drop_data)[rownames(clean_drop_data) %in% rownames(clean_miss_data)]


  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")
    object@clean.data <- clean_miss_data
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'clean.data' slot updated.\n")
    return(object)
  }

  return(clean_miss_data)
}

#' Detect and Mark Outliers in Data
#'
#' This function detects outliers in numeric variables of a given data frame. It identifies outliers using
#' the IQR (Interquartile Range) method and marks them in the data. The function generates boxplots to visualize
#' the detected outliers and saves these plots if specified.
#'
#' @param data A data frame containing the data to be processed. The data must include numeric variables.
#' @param group_col A string specifying the name of the grouping column (default is "group"). This column is
#'                  used to color the boxplots.
#' @param palette_name A string specifying the palette name for coloring the boxplots (default is "lisa::SalvadorDali").
#' @param save_plots A logical value indicating whether to save the generated plots (default is TRUE).
#' @param save_dir A string specifying the directory to save the plots (default is "StatObject").
#' @param plot_display_num An integer specifying how many variables to display per plot (default is 5).
#' @param sub_var A vector of variable names to restrict the outlier detection and plotting to (default is NULL,
#'                meaning all variables with outliers will be processed).
#' @param plot_width A numeric value specifying the width of the saved plot images (default is 5).
#' @param plot_height A numeric value specifying the height of the saved plot images (default is 5).
#' @param base_size A numeric value specifying the base size for the plot's text elements (default is 14).
#'
#' @returns A list containing two elements:
#'   - `data_marked`: The original data frame with a new column for each detected outlier variable,
#'     indicating whether each observation is an outlier.
#'   - `plots`: A list of ggplot objects representing the boxplots for visualizing outliers.
#'
#' @export
#'
#' @examples
#' # Example 1: Detect and mark outliers, save plots to the default directory
#' result <- detect_and_mark_outliers(data = my_data)
#'
#' # Example 2: Detect outliers and generate plots for specific variables only
#' result <- detect_and_mark_outliers(data = my_data, sub_var = c("var1", "var2"))
detect_and_mark_outliers <- function(data,
                                     group_col = "group",
                                     palette_name = "lisa::SalvadorDali",
                                     save_plots = TRUE,
                                     save_dir = here("StatObject"),
                                     plot_display_num = 5,
                                     sub_var = NULL,
                                     plot_width = 5,
                                     plot_height = 5,
                                     base_size = 14) {
  stopifnot(is.data.frame(data))

  original_rownames <- rownames(data)

  numeric_data <- data %>% dplyr::select_if(is.numeric)

  detect_outliers <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(x < lower_bound | x > upper_bound)
  }

  outlier_vars <- numeric_data %>%
    summarise(across(everything(), ~ any(detect_outliers(.), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "has_outliers") %>%
    filter(has_outliers) %>%
    pull(variable)

  if (length(outlier_vars) == 0) {
    cat("No variables with outliers found.")
    return(NULL)
  }

  if (!is.null(sub_var)) {
    outlier_vars <- intersect(outlier_vars, sub_var)
  }

  cat("Detected outlier variables:", paste(outlier_vars, collapse = ","), "\n")

  for (var in outlier_vars) {
    data <- data %>%
      mutate(!!paste0(var, "_outlier") := detect_outliers(!!sym(var)))
  }

  if (!is.null(group_col)) {
    data[[group_col]] <- as.factor(data[[group_col]])
  }

  colors <- as.vector(paletteer::paletteer_d(palette_name))

  data_with_outliers <- data %>% dplyr::select(all_of(outlier_vars), all_of(group_col))

  melted_data <- pivot_longer(data_with_outliers, cols = all_of(outlier_vars), names_to = "variable", values_to = "value")

  all_plots <- list()

  for (i in seq(1, length(outlier_vars), by = plot_display_num)) {
    selected_vars <- outlier_vars[i:min(i + plot_display_num - 1, length(outlier_vars))]

    if (!is.null(group_col)) {
      p <- ggplot(melted_data %>% filter(variable %in% selected_vars), aes(x = variable, y = value, fill = !!sym(group_col))) +
        geom_boxplot(outlier.shape = 19, outlier.colour = "red", outlier.size = 1) +
        scale_fill_manual(values = colors) +
        labs(title = "Boxplot of Variables with Outliers",
             x = "Variable",
             y = "Value") +
        theme_prism(base_size = base_size) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank()
        )
    } else {
      p <- ggplot(melted_data %>% filter(variable %in% selected_vars), aes(x = variable, y = value)) +
        geom_boxplot(outlier.shape = 19, outlier.colour = "red", outlier.size = 1, fill = colors[1]) +
        labs(title = "Boxplot of Variables with Outliers",
             x = "Variable",
             y = "Value") +
        theme_prism(base_size = base_size) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)
        )
    }

    all_plots[[length(all_plots) + 1]] <- p
  }
  cat("Generated ", length(all_plots), " plots for outlier visualization.\n")
  print(all_plots[[1]])

  if (save_plots) {
    outlier_dir <- file.path(save_dir)

    if (!is.null(sub_var) && length(sub_var) > 0) {
      sub_var_name <- paste(sub_var, collapse = "_")
      sub_dir <- file.path(outlier_dir, paste("sub_var_", sub_var_name, sep = ""))
      if (!dir.exists(sub_dir)) {
        dir.create(sub_dir, recursive = TRUE)
      }
      outlier_dir <- sub_dir
    } else {
      if (!dir.exists(outlier_dir)) {
        dir.create(outlier_dir, recursive = TRUE)
      }
    }

    for (i in 1:length(all_plots)) {
      ggsave(filename = file.path(outlier_dir, paste0("boxplot_outliers_batch_", i, ".png")),
             plot = all_plots[[i]],
             width = plot_width,
             height = plot_height)
    }
    cat("Saved plots to: ", outlier_dir, "\n")
  }

  rownames(data) <- original_rownames

  return(list(data_marked = data, plots = all_plots))
}


#' Detect and Mark Outliers in a Data Set
#'
#' @param object Input object, which can either be a `Stat` object or a data frame.
#' @param save_outliers Boolean indicating whether to save outliers information (default is TRUE).
#' @param nthreads Number of threads to use for parallel processing (default is 1).
#' @param group_col Name of the grouping column for categorical variables (default is "group").
#' @param palette_name Color palette name used for visualization (default is "lisa::SalvadorDali").
#' @param handle_method Method for handling outliers. Options include "replace", "remove", "keep", and "capping".
#' @param lower_quantile The lower quantile used to identify outliers (default is 0.05).
#' @param upper_quantile The upper quantile used to identify outliers (default is 0.95).
#' @param save_plots Boolean indicating whether to save plots of outlier visualization (default is TRUE).
#' @param save_dir Directory where the plots will be saved (default is "StatObject/pre_outlier_info").
#' @param plot_display_num Number of plots to display in one batch (default is 5).
#' @param sub_var A subset of variables to process (default is NULL, meaning all variables).
#' @param plot_width Width of the generated plots (default is 5).
#' @param plot_height Height of the generated plots (default is 5).
#' @param base_size Base font size for plots (default is 14).
#'
#' @returns Returns a modified object with marked outliers, either as a `Stat` object or as a data frame.
#' @export
#'
#' @examples
#' # Example 1: Using a Stat object
#' stat_object <- Stat$new(clean.data = your_data_frame, group_col = "group")
#' result <- stat_detect_and_mark_outliers(stat_object,
#'                                         save_outliers = TRUE,
#'                                         nthreads = 2,
#'                                         group_col = "group",
#'                                         save_plots = TRUE)
#' # The function updates the 'Stat' object and returns it with marked outliers.

#' # Example 2: Using a data frame directly
#' data_frame <- data.frame(
#'   gene1 = c(1, 2, 3, 100, 5),
#'   gene2 = c(2, 4, 6, 100, 8),
#'   group = c("A", "A", "B", "B", "A")
#' )
#' result_df <- stat_detect_and_mark_outliers(data_frame,
#'                                           save_outliers = TRUE,
#'                                           plot_display_num = 2,
#'                                           group_col = "group")
#' # The function returns a data frame with outliers marked.
stat_detect_and_mark_outliers <- function(object,
                                          save_outliers = TRUE,
                                          nthreads = 1,
                                          group_col = "group",
                                          palette_name = "lisa::SalvadorDali",
                                          handle_method = c("replace", "remove", "keep", "capping"),
                                          lower_quantile = 0.05,
                                          upper_quantile = 0.95,
                                          save_plots = TRUE,
                                          save_dir = here("StatObject","pre_outlier_info"),
                                          plot_display_num = 5,
                                          sub_var = NULL,
                                          plot_width = 5,
                                          plot_height = 5,
                                          base_size = 14) {

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- object@group_col
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  cat("Processing data with ", nrow(data), " rows.\n")

  original_rownames <- rownames(data)

  re_outliers <- detect_and_mark_outliers(data,
                                          group_col = group_col,
                                          palette_name = palette_name,
                                          save_plots = save_plots,
                                          save_dir = save_dir,
                                          plot_display_num = plot_display_num,
                                          sub_var = sub_var,
                                          plot_width = plot_width,
                                          plot_height = plot_height,
                                          base_size = base_size)

  rownames(re_outliers$data_marked) <- original_rownames

  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")

    object@outlier.marked[["pre_outliers_info"]] <- re_outliers
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'outlier.marked' slot updated.\n")
    return(object)
  }

  return(re_outliers$data_marked)
}


#' Remove Rows with Outliers from the Data Set
#'
#' This function removes rows from the provided data frame that are marked as outliers.
#' Outlier identification is based on columns with names ending in "_outlier".
#' The rows with any `TRUE` values in these columns will be excluded from the output.
#'
#' @param data A data frame containing outlier flags (columns with names ending in "_outlier").
#'
#' @returns A data frame with rows containing outliers removed.
#' @export
#'
#' @examples
#' # Example usage
#' data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
#'                    gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'                    gene2 = c(2, 4, 6, 100, 8),
#'                    gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
#' cleaned_data <- handle_outliers_remove(data)
#' # cleaned_data will contain the rows without outliers.
handle_outliers_remove <- function(data) {
  cleaned_data <- data %>%
    filter(across(ends_with("_outlier"), ~ . == FALSE))
  return(cleaned_data)
}

#' Replace Outliers with Median Values
#'
#' This function identifies columns in the data that are marked as outliers (those ending with "_outlier").
#' For each of these columns, the corresponding variable values are replaced by the median value of the
#' original variable (excluding outliers). The outlier indicator columns are then removed from the data.
#'
#' @param data A data frame containing outlier flags (columns with names ending in "_outlier").
#'
#' @returns A data frame with outliers replaced by the median values and outlier columns removed.
#' @export
#'
#' @examples
#' # Example usage
#' data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
#'                    gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'                    gene2 = c(2, 4, 6, 100, 8),
#'                    gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
#' cleaned_data <- handle_outliers_replace(data)
#' # cleaned_data will contain the outliers replaced by the median value for each gene.
handle_outliers_replace <- function(data) {
  for (var in colnames(data)) {
    if (grepl("_outlier$", var)) {
      original_var <- sub("_outlier$", "", var)
      median_value <- median(data[[original_var]], na.rm = TRUE)
      data[[original_var]][data[[var]]] <- median_value
    }
  }
  cleaned_data <- data %>% dplyr::select(-ends_with("_outlier"))
  return(cleaned_data)
}

#' Keep Outliers in the Data
#'
#' This function simply returns the input data without any modifications,
#' preserving outlier values marked in the dataset.
#'
#' @param data A data frame containing outlier flags (columns with names ending in "_outlier").
#'
#' @returns The original data frame, with outlier columns preserved.
#' @export
#'
#' @examples
#' # Example usage
#' data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
#'                    gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'                    gene2 = c(2, 4, 6, 100, 8),
#'                    gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
#' result_data <- handle_outliers_keep(data)
#' # result_data will be the same as the original data, with outliers unchanged.
handle_outliers_keep <- function(data) {
  return(data)
}


#' Cap Outliers to a Defined Range
#'
#' This function caps outliers in the data by replacing values outside of the
#' specified quantile range with the nearest quantile values. It uses the
#' `lower_quantile` and `upper_quantile` arguments to define the bounds.
#'
#' @param data A data frame containing the original data with outlier columns
#' (columns with names ending in "_outlier").
#' @param lower_quantile Numeric value representing the lower quantile for capping
#' (default is 0.05).
#' @param upper_quantile Numeric value representing the upper quantile for capping
#' (default is 0.95).
#'
#' @returns A data frame with outlier values capped to the specified quantile bounds.
#' @export
#'
#' @examples
#' # Example usage
#' data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
#'                    gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'                    gene2 = c(2, 4, 6, 100, 8),
#'                    gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
#' result_data <- handle_outliers_capping(data, lower_quantile = 0.1, upper_quantile = 0.9)
#' # result_data will have outliers capped based on the specified quantiles.
handle_outliers_capping <- function(data, lower_quantile = 0.05, upper_quantile = 0.95) {
  for (var in colnames(data)) {
    if (grepl("_outlier$", var)) {
      original_var <- sub("_outlier$", "", var)
      lower_bound <- quantile(data[[original_var]], lower_quantile, na.rm = TRUE)
      upper_bound <- quantile(data[[original_var]], upper_quantile, na.rm = TRUE)
      data[[original_var]] <- pmax(pmin(data[[original_var]], upper_bound), lower_bound)
    }
  }
  cleaned_data <- data %>% dplyr::select(-ends_with("_outlier"))
  return(cleaned_data)
}


#' Handle Outliers in the Data
#'
#' This function allows the user to handle outliers in a data frame using various
#' methods, including removing, replacing, keeping, or capping outliers. It operates
#' on columns with names ending in "_outlier".
#'
#' @param data A data frame containing columns with outlier flags (columns ending in "_outlier").
#' @param handle_method A character string specifying the method for handling outliers.
#' Options include "remove", "replace", "keep", and "capping" (default is "replace").
#' @param lower_quantile Numeric value representing the lower quantile for capping
#' outliers (default is 0.05).
#' @param upper_quantile Numeric value representing the upper quantile for capping
#' outliers (default is 0.95).
#'
#' @returns A data frame with outliers handled according to the selected method.
#' @export
#'
#' @examples
#' # Example 1: Remove outliers
#' data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
#'                    gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'                    gene2 = c(2, 4, 6, 100, 8),
#'                    gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
#' cleaned_data <- handle_outliers(data, handle_method = "remove")
#' # cleaned_data will have outliers removed.
#'
#' # Example 2: Replace outliers with the median value
#' cleaned_data <- handle_outliers(data, handle_method = "replace")
#' # cleaned_data will have outliers replaced by the median values.
#'
#' # Example 3: Keep outliers
#' cleaned_data <- handle_outliers(data, handle_method = "keep")
#' # cleaned_data will keep the outliers as they are.
#'
#' # Example 4: Cap outliers based on quantiles
#' cleaned_data <- handle_outliers(data, handle_method = "capping",
#'                                 lower_quantile = 0.1, upper_quantile = 0.9)
#' # cleaned_data will have outliers capped to the specified quantile range.
handle_outliers <- function(data,
                            handle_method = c("replace", "remove", "keep", "capping"),
                            lower_quantile = 0.05,
                            upper_quantile = 0.95) {

  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  if (!any(grepl("_outlier$", colnames(data)))) {
    stop("No '_outlier' columns found in the data.")
  }

  handle_method <- match.arg(handle_method)

  if (handle_method == "remove") {
    data <- handle_outliers_remove(data)
  } else if (handle_method == "replace") {
    data <- handle_outliers_replace(data)
  } else if (handle_method == "keep") {
    data <- handle_outliers_keep(data)
  } else if (handle_method == "capping") {
    data <- handle_outliers_capping(data,
                                    lower_quantile = lower_quantile,
                                    upper_quantile = upper_quantile)
  } else {
    stop("Unknown method!")
  }

  return(data)
}



#' Extract Outlier Data from a Stat Object
#'
#' This function extracts the marked outlier data from a 'Stat' object. The function
#' retrieves outlier information from the specified slot, allowing access to the
#' data that has been processed and marked for outliers.
#'
#' @param object An object of class 'Stat', containing outlier-marked data.
#' @param slot_name The name of the slot from which to extract the outlier data
#' (default is "outlier.marked").
#'
#' @returns A data frame containing the outlier-marked data extracted from the specified slot.
#' @export
#'
#' @examples
#' # Example 1: Extract outlier data from a Stat object
#' stat_object <- Stat$new(clean.data = your_data_frame, group_col = "group")
#' outlier_data <- extract_outlier_data(stat_object)
#' # outlier_data will contain the data with marked outliers.
#'
#' # Example 2: Extract outlier data from a specific slot
#' outlier_data <- extract_outlier_data(stat_object, slot_name = "outlier.marked")
#' # outlier_data will contain the data from the specified slot.
extract_outlier_data <- function(object,
                                 slot_name = "outlier.marked") {
  if (!inherits(object, "Stat")) {
    stop("Input must be an object of class 'Stat'.")
  }

  if (!slot_name %in% slotNames(object)) {
    stop(paste("Slot", slot_name, "does not exist in the object."))
  }

  outlier_info <- slot(object, slot_name)[["pre_outliers_info"]]

  if (is.null(outlier_info) || !("data_marked" %in% names(outlier_info))) {
    stop("No marked outlier data found.")
  }

  data_marked <- outlier_info[["data_marked"]]
  return(data_marked)
}

#' Handle Outliers in a Stat Object or Data Frame
#'
#' This function handles outliers in either a 'Stat' object or a data frame. It applies the
#' chosen outlier handling method (e.g., removing, replacing, keeping, or capping outliers)
#' to the data and returns the cleaned data. If the input is a 'Stat' object, the function
#' updates the 'Stat' object with the cleaned data.
#'
#' @param object An object of class 'Stat' or a data frame containing the data to process.
#' @param save_cleaned A boolean indicating whether to save the cleaned data in the 'Stat' object (default is TRUE).
#' @param handle_method A method to handle outliers. Options include "replace", "remove", "keep", and "capping" (default is "replace").
#' @param lower_quantile The lower quantile used for capping outliers (default is 0.05).
#' @param upper_quantile The upper quantile used for capping outliers (default is 0.95).
#' @param group_col The name of the grouping column in the data (default is "group").
#'
#' @returns If the input is a 'Stat' object, the updated 'Stat' object with cleaned data is returned.
#'          If the input is a data frame, the cleaned data frame is returned.
#' @export
#'
#' @examples
#' # Example 1: Using a Stat object
#' stat_object <- Stat$new(clean.data = your_data_frame, group_col = "group")
#' cleaned_stat <- stat_handle_outliers(stat_object,
#'                                      save_cleaned = TRUE,
#'                                      handle_method = "remove",
#'                                      lower_quantile = 0.05,
#'                                      upper_quantile = 0.95)
#' # The 'Stat' object is updated with the cleaned data.
#'
#' # Example 2: Using a data frame
#' data_frame <- data.frame(
#'   gene1 = c(1, 2, 3, 100, 5),
#'   gene2 = c(2, 4, 6, 100, 8),
#'   group = c("A", "A", "B", "B", "A")
#' )
#' cleaned_data <- stat_handle_outliers(data_frame,
#'                                      handle_method = "capping",
#'                                      lower_quantile = 0.05,
#'                                      upper_quantile = 0.95)
#' # The cleaned data frame is returned with outliers capped.
stat_handle_outliers <- function(object,
                                 save_cleaned = TRUE,
                                 handle_method = c("replace", "remove", "keep", "capping"),
                                 lower_quantile = 0.05,
                                 upper_quantile = 0.95,
                                 group_col = "group"
) {

  handle_method <- match.arg(handle_method, choices = c("replace", "remove", "keep", "capping"))

  if (inherits(object, "Stat")) {
    cat("'object' is of class 'Stat'.\n Extracting outlier data...\n")
    data <- extract_outlier_data(object)
    group_col <- object@group_col
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    cat(" 'object' is a data frame.\n")
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }

  cat("Handling outliers with method: ", handle_method,"\n")
  original_rownames <- rownames(data)
  cleaned_data <- handle_outliers(data,
                                  handle_method = handle_method,
                                  lower_quantile = lower_quantile,
                                  upper_quantile = upper_quantile)

  rownames(cleaned_data) <- original_rownames
  cat("Outlier handling completed. Cleaned data has ", nrow(cleaned_data), " rows.","\n")

  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")
    object@outlier.marked[["cleaned_data"]] <- cleaned_data
    object@clean.data <- cleaned_data
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'outlier.marked' slot updated.\n")
    cat("- 'clean.data' slot updated.\n")

    return(object)
  }

  return(cleaned_data)
}




#' Gaze Analysis for Group Comparison
#'
#' This function performs gaze analysis for group comparison using the specified formula,
#' method, and settings. It provides a detailed result, optionally saving it as a Word document.
#' The gaze analysis can be performed based on group columns in the data, and various settings
#' are available to customize the output.
#'
#' @param data A data frame containing the data to analyze.
#' @param formula A formula specifying the model to fit. If NULL, the formula is automatically created
#' based on the provided group columns (default is NULL).
#' @param group_cols A vector of column names representing the grouping variables (default is NULL).
#' @param digits The number of digits to display for the result (default is 1).
#' @param show.p A logical value indicating whether to display p-values in the output (default is TRUE).
#' @param gaze_method An integer between 1 and 5 representing the gaze analysis method to use (default is 3).
#' @param save_word A logical value indicating whether to save the results to a Word document (default is TRUE).
#' @param save_dir The directory to save the Word document (default is the "PrognosiX/gaze_baseline" folder).

#' @returns A data frame or matrix with the gaze analysis results. If `save_word` is TRUE, the results
#' are also saved as a Word document in the specified directory.
#' @export
#'
#' @examples
#' # Example 1: Performing gaze analysis with a formula and custom settings
#' result <- gaze_analysis(data = my_data,
#'                         formula = ~ group + age + gender,
#'                         digits = 2,
#'                         show.p = TRUE,
#'                         gaze_method = 3,
#'                         save_word = TRUE,
#'                         save_dir = "path/to/save")
#'
#' # Example 2: Using the default formula based on group columns
#' result <- gaze_analysis(data = my_data,
#'                         group_cols = c("group"),
#'                         digits = 1,
#'                         show.p = FALSE,
#'                         gaze_method = 1)
gaze_analysis <- function(data,
                          formula = NULL,
                          group_cols = NULL,
                          digits = 1,
                          show.p = TRUE,
                          gaze_method = 3,
                          save_word = TRUE,
                          save_dir = here("PrognosiX", "gaze_baseline")) {

  if (!is.data.frame(data)) stop("The input 'data' must be a data frame.")

  if (is.null(formula)) {
    if (!is.null(group_cols)) {
      if (length(group_cols) == 1 && group_cols %in% colnames(data)) {
        formula <- as.formula(paste(group_cols, "~ ."))
        cat("Using formula with one group:", deparse(formula), "\n")
      } else if (length(group_cols) > 1 && all(group_cols %in% colnames(data))) {
        formula <- as.formula(paste(paste(group_cols, collapse = " + "), "~ ."))
        cat("Using formula with multiple groups:", deparse(formula), "\n")
      } else {
        stop("Group columns not found in data.")
      }
    } else {
      formula <- as.formula("~ .")
      cat("Using default formula: ~ .\n")
    }
  } else if (!inherits(formula, "formula")) {
    stop("The input 'formula' must be a valid formula.")
  }

  if (!is.numeric(digits) || digits < 0 || digits != as.integer(digits))
    stop("The input 'digits' must be a non-negative integer.")

  if (!is.logical(show.p))
    stop("The input 'show.p' must be a logical value (TRUE or FALSE).")

  if (!is.numeric(gaze_method) || gaze_method < 1 || gaze_method > 5 || gaze_method != as.integer(gaze_method))
    stop("The input 'gaze_method' must be an integer between 1 and 5.")

  tryCatch({
    cat("Running gaze analysis with method:", gaze_method, "\n")
    result <- gaze(formula, data, digits = digits, show.p = show.p, method = gaze_method)

    cat("Result type:", class(result), "\n")
    if (is.data.frame(result) || is.matrix(result)) {
      result <- myft(result)
      cat("Gaze analysis completed successfully.\n")

      if (save_word) {
        doc <- read_docx()
        doc <- doc %>%
          body_add_flextable(result) %>%
          body_add_par("Gaze Analysis Results", style = "heading 1")

        if (!dir.exists(save_dir)) {
          dir.create(save_dir, recursive = TRUE)
        }

        word_filename <- file.path(save_dir, "gaze_analysis.docx")
        print(doc, target = word_filename)
        cat("Word file saved to:", word_filename, "\n")
      }

      return(result)
    } else {
      stop("The result is not a data frame or matrix.")
    }
  }, error = function(e) {
    stop("An error occurred while performing the gaze analysis: ", e$message)
  })
}

#' Statistical Gaze Analysis
#'
#' This function performs gaze analysis on a given dataset or `Stat` object, providing results for
#' group comparisons, statistical significance, and optionally saving the results as a Word document
#' and plots as images. It supports customized formulas and gaze methods for the analysis.
#'
#' @param object An object of class 'Stat' or a data frame containing the data to analyze.
#' @param formula A formula specifying the model to fit. If NULL, a default formula is used (default is NULL).
#' @param group_col The column name representing the grouping variable (default is "group").
#' @param digits The number of digits to display for the result (default is 1).
#' @param show.p A logical value indicating whether to display p-values in the output (default is TRUE).
#' @param gaze_method An integer between 1 and 5 representing the gaze analysis method to use (default is 3).
#' @param save_word A logical value indicating whether to save the results to a Word document (default is TRUE).
#' @param save_plots A logical value indicating whether to save the gaze analysis plots (default is TRUE).
#' @param save_dir The directory to save the Word document and plot images (default is the "StatObject/gaze_baseline" folder).
#'
#' @returns An updated 'Stat' object with the gaze analysis results if the input is of class 'Stat',
#'         or a data frame containing the results otherwise.
#' @export
#'
#' @examples
#' # Example 1: Performing gaze analysis on a Stat object with a custom formula and saving results
#' stat_object <- Stat$new(clean.data = your_data_frame, group_col = "group")
#' updated_stat_object <- stat_gaze_analysis(stat_object,
#'                                          formula = ~ group + age + gender,
#'                                          digits = 2,
#'                                          show.p = TRUE,
#'                                          gaze_method = 3,
#'                                          save_word = TRUE,
#'                                          save_plots = TRUE,
#'                                          save_dir = "path/to/save")
#'
#' # Example 2: Performing gaze analysis on a data frame
#' result <- stat_gaze_analysis(your_data_frame,
#'                              formula = ~ group + age + gender,
#'                              save_word = FALSE,
#'                              save_plots = TRUE)
stat_gaze_analysis <- function(object,
                               formula = NULL,
                               group_col = "group",
                               digits = 1,
                               show.p = TRUE,
                               gaze_method = 3,
                               save_word = TRUE,
                               save_plots = TRUE,
                               save_dir = here("StatObject", "gaze_baseline")) {

  cat("Starting stat_gaze_analysis function...\n")

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0)
    stop("No valid data found in the input.")

  if (!is.null(group_col) && !group_col %in% colnames(data))
    group_col <- NULL

  cat("Data prepared for gaze analysis. Number of rows:", nrow(data), "\n")

  result <- gaze_analysis(data,
                          formula = formula,
                          group_cols = group_col,
                          digits = digits,
                          show.p = show.p,
                          gaze_method = gaze_method,
                          save_word = save_word,
                          save_dir = save_dir)

  print(result)

  if (save_word) {
    cat("Saving results as Word document...\n")
    doc <- read_docx()
    doc <- doc %>%
      body_add_flextable(result) %>%
      body_add_par("Gaze Analysis Results", style = "heading 1")

    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }

    word_filename <- file.path(save_dir, "gaze_analysis.docx")
    print(doc, target = word_filename)
    cat("Word file saved to:", word_filename, "\n")
  }

  if (save_plots) {
    cat("Saving plots...\n")
    html_file <- tempfile(fileext = ".html")
    save_as_html(result, path = html_file)

    img_filename <- file.path(save_dir, "gaze_analysis.png")
    webshot(html_file, file = img_filename, vwidth = 800, vheight = 600)
    cat("Plot saved to:", img_filename, "\n")
  }

  if (inherits(object, "Stat")) {
    object@baseline.table <- result
    cat("Updating 'Stat' object...\n")
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'baseline.table' slot updated.\n")
    return(object)
  }

  return(result)
}



#' Compute Descriptive Statistics
#'
#' This function computes various descriptive statistics for the input dataset, including counts for categorical variables,
#' and summary statistics (mean, median, standard deviation, min, max) for numeric variables. It also checks for normality
#' of numeric variables and computes normality tests (Shapiro-Wilk or Anderson-Darling test), based on the number of unique
#' values and sample size.
#'
#' @param data A data frame containing the dataset to analyze.
#' @param count_feature A logical value indicating whether to compute counts for categorical variables (default is TRUE).
#' @param group_col The column name representing the grouping variable (default is "group").
#' @param max_unique_values The maximum number of unique values a variable can have to be considered categorical (default is 5).
#'
#' @returns A list containing the following components:
#' \item{Group_Counts}{The counts of each group if a `group_col` is provided.}
#' \item{Count_Results}{A list of counts for each categorical variable.}
#' \item{Num_Results}{A list of descriptive statistics for numeric variables, including separate statistics for normal and non-normal distributions.}
#' \item{Normality_Test}{A list of p-values and normality test results for numeric variables.}
#'
#' @export
#'
#' @examples
#' # Example 1: Compute descriptive statistics for a data frame
#' result <- compute_descriptive_stats(data = my_data, count_feature = TRUE, group_col = "group")
#'
#' # Example 2: Compute overall descriptive statistics without considering group
#' result <- compute_descriptive_stats(data = my_data, count_feature = FALSE)
compute_descriptive_stats <- function(data,
                                      count_feature = TRUE,
                                      group_col = "group",
                                      max_unique_values = 5) {
  if (length(group_col) == 0) group_col <- NULL

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  result <- list()

  if (count_feature && !is.null(variable_types) && !is.null(variable_types$categorical_vars)) {
    count_col <- variable_types$categorical_vars
    cat("Categorical variables identified:", count_col, "\n")

    if (group_col %in% colnames(data)) {
      data[[group_col]] <- as.factor(data[[group_col]])
      group_counts <- table(data[[group_col]])
      result$Group_Counts <- group_counts
      cat("Group counts computed for column:", group_col, "\n")
      num_cols <- setdiff(names(data), c(count_col, group_col))
    } else {
      num_cols <- setdiff(names(data), count_col)
    }

    cat("Counting categorical values...\n")
    count_results <- lapply(count_col, function(col) table(data[[col]]))
    names(count_results) <- count_col
    result$Count_Results <- count_results

    normal_vars <- character()
    non_normal_vars <- character()

    for (col in num_cols) {
      if (is.numeric(data[[col]])) {
        cat("Checking normality for numeric column:", col, "\n")
        if (length(unique(data[[col]])) <= 2) {
          non_normal_vars <- c(non_normal_vars, col)
        } else {
          if (sum(!is.na(data[[col]])) <= 5000) {
            is_normal <- shapiro.test(data[[col]])$p.value > 0.05
          } else {
            is_normal <- nortest::ad.test(data[[col]])$p.value > 0.05
          }
          if (is_normal) {
            normal_vars <- c(normal_vars, col)
          } else {
            non_normal_vars <- c(non_normal_vars, col)
          }
        }
      }
    }

    cat("Computing descriptive statistics for numeric columns...\n")
    all_stats <- apply(data[, num_cols, drop = FALSE], 2, function(x) {
      c(Mean = mean(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
    })

    normal_stats <- if (length(normal_vars) > 0) {
      apply(data[, normal_vars, drop = FALSE], 2, function(x) {
        c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
      })
    } else {
      NULL
    }

    non_normal_stats <- if (length(non_normal_vars) > 0) {
      apply(data[, non_normal_vars, drop = FALSE], 2, function(x) {
        if (sum(!is.na(x)) <= 5000) {
          ad_test <- shapiro.test(x)
        } else {
          ad_test <- nortest::ad.test(x)
        }
        c(AD_p_value = ad_test$p.value, Median = median(x, na.rm = TRUE), IQR = IQR(x, na.rm = TRUE))
      })
    } else {
      NULL
    }

    result$Num_Results <- list(All = all_stats, Normal = normal_stats, Non_Normal = non_normal_stats)
    result$Normality_Test <- lapply(data[, num_cols, drop = FALSE], function(x) {
      if (is.numeric(x)) {
        if (length(unique(x)) <= 2) {
          return(NULL)
        } else {
          if (sum(!is.na(x)) <= 5000) {
            test_result <- shapiro.test(x)
          } else {
            test_result <- nortest::ad.test(x)
          }
          return(list(p_value = test_result$p.value, is_normal = test_result$p.value > 0.05))
        }
      } else {
        return(NULL)
      }
    })

    cat("Descriptive statistics computation completed.\n")
  } else {
    cat("Computing overall descriptive statistics...\n")
    stats_compute <- apply(data, 2, function(x) c(Mean = mean(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
    result$Stats <- stats_compute
    cat("Overall statistics computed.\n")
  }

  return(result)
}



#' Compute Descriptive Statistics for 'Stat' Object or Data Frame
#'
#' This function computes descriptive statistics for numeric and categorical variables in the input data. It can handle both
#' `Stat` objects and standard data frames. The function computes counts for categorical variables, and summary statistics
#' (mean, median, standard deviation, min, max) for numeric variables, along with normality tests for numeric variables.
#'
#' @param object An object of class 'Stat' or a data frame containing the dataset to analyze.
#' @param count_feature A logical value indicating whether to compute counts for categorical variables (default is TRUE).
#' @param group_col The column name representing the grouping variable (default is "group").
#' @param max_unique_values The maximum number of unique values a variable can have to be considered categorical (default is 5).
#'
#' @returns If the input is a 'Stat' object, the function returns the updated 'Stat' object with a new slot `compute.descriptive`
#'          containing the results. If the input is a data frame, it returns the descriptive statistics as a list.
#'
#' @export
#'
#' @examples
#' # Example 1: Compute descriptive statistics for a 'Stat' object
#' updated_stat <- stat_compute_descriptive(object = my_stat, count_feature = TRUE, group_col = "group")
#'
#' # Example 2: Compute descriptive statistics for a data frame
#' result <- stat_compute_descriptive(object = my_data, count_feature = TRUE)
stat_compute_descriptive <- function(
    object,
    count_feature = TRUE,
    group_col = "group",
    max_unique_values = 5
) {

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")
    if (length(group_col) == 0 || !group_col %in% colnames(data)) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)

  if (length(variable_types$numeric_vars) == 0 && length(variable_types$categorical_vars) == 0) {
    stop("No valid variables found after variable type diagnosis")
  }

  result <- list()

  result <- compute_descriptive_stats(data = data, count_feature = count_feature)

  if (inherits(object, "Stat")) {
    object@compute.descriptive <- result
    cat("Updating 'Stat' object...\n")
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'compute.descriptive' slot updated.\n")

    return(object)
  }

  return(result)
}



#' Plot Categorical Descriptive Statistics for 'Stat' Object or Data Frame
#'
#' This function generates bar plots displaying the distribution of categorical variables and their percentages across different groups.
#' It can handle both `Stat` objects and standard data frames. The function also supports saving plots to a specified directory.
#' The plots are created using `ggplot2` and the colors are customized using a specified color palette.
#'
#' @param object An object of class 'Stat' or a data frame containing the dataset to analyze.
#' @param group_col The column name representing the grouping variable (default is "group").
#' @param palette_name The name of the color palette to use (default is "lisa::SalvadorDali").
#' @param save_plots A logical value indicating whether to save the plots (default is TRUE).
#' @param save_dir The directory where the plots will be saved (default is "StatObject/categorical_descriptive").
#' @param sub_var A vector of column names to subset the variables to plot (default is NULL, meaning all categorical variables will be plotted).
#' @param plot_width The width of the saved plot (default is 5).
#' @param plot_height The height of the saved plot (default is 5).
#' @param base_size The base font size for the plots (default is 14).
#'
#' @returns If the input is a 'Stat' object, the function returns the updated 'Stat' object with a new slot `compute.descriptive$count_plots`
#'          containing the generated plots. If the input is a data frame, it returns a list of the generated plots.
#'
#' @export
#'
#' @examples
#' # Example 1: Plot categorical descriptive statistics for a 'Stat' object
#' updated_stat <- plot_categorical_descriptive(object = my_stat, group_col = "group")
#'
#' # Example 2: Plot categorical descriptive statistics for a data frame
#' plot_results <- plot_categorical_descriptive(object = my_data, group_col = "group")
plot_categorical_descriptive <- function(
    object,
    group_col = "group",
    palette_name = "lisa::SalvadorDali",
    save_plots = TRUE,
    save_dir = here("StatObject", "categorical_descriptive"),
    sub_var = NULL,
    plot_width = 5,
    plot_height = 5,
    base_size = 14
) {

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")

    if (length(group_col) == 0 || !group_col %in% colnames(data)) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame.\n")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.\n")
  }

  colors <- as.vector(paletteer::paletteer_d(palette_name))

  if (inherits(object, "Stat")) {
    stats <- slot(object, "compute.descriptive")
  } else {
    stats <- compute_descriptive_stats(data, count_feature = TRUE, group_col = group_col)
  }

  if (is.null(stats$Count_Results)) {
    cat("No valid count results to display. Please check the data and parameters.\n")
    return(object)
  }

  count_plots <- list()

  for (col in names(stats$Count_Results)) {
    cat(paste("Processing column:", col, "\n"))

    if (!is.null(sub_var) && !(col %in% sub_var)) {
      cat(paste("Skipping column:", col, "not in sub_var.\n"))
      next
    }

    if (is.null(group_col)) {
      group_col <- "temp_group"
      data[[group_col]] <- "All"
    }

    data[[group_col]] <- as.factor(data[[group_col]])

    plot_data <- data %>%
      group_by(!!sym(col), !!sym(group_col)) %>%
      summarise(n = n(), .groups = 'drop') %>%
      mutate(perc = n / sum(n) * 100)

    if (nrow(plot_data) == 0) {
      cat(paste("No data available for column:", col, "\n"))
      next
    }

    p <- ggplot(plot_data, aes_string(x = col, y = "perc", fill = group_col)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = scales::percent(perc / 100)),
                position = position_dodge(width = 0.9), size = 4, vjust = -0.5, hjust = 0.5) +
      labs(title = paste("Bar plot of", col, "with percentage"), y = "Percentage") +
      theme_prism(base_size = base_size)

    if (length(unique(plot_data[[group_col]])) > length(colors)) {
      warning("Not enough colors provided for unique groups.\n")
      colors <- rep(colors, length(unique(plot_data[[group_col]])))
    }

    p <- p + scale_fill_manual(values = colors[1:length(unique(plot_data[[group_col]]))])

    count_plots[[col]] <- p

    if (save_plots) {
      ggsave(here(save_dir, paste0(col, "_plot.png")), plot = p, width = plot_width, height = plot_height)
      cat(paste("Saved plot for column:", col, "\n"))
    }
  }

  plot_count <- length(count_plots)
  cat(paste("A total of", plot_count, "plots were generated.\n"))

  if (plot_count > 0) {
    grid.arrange(grobs = count_plots, nrow = 1)
    print(count_plots[[1]])
  } else {
    cat("No valid count plots to display.\n")
  }

  if (save_plots) {
    cat("Saved plots to:", save_dir, "\n")
  }

  if (inherits(object, "Stat")) {
    object@compute.descriptive[["count_plots"]] <- count_plots
    cat("Updating 'Stat' object...\n")
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'compute.descriptiv' slot updated.\n")
    return(object)
  }
  return(count_plots)
}


#' Violin Plots for Numeric Variables
#'
#' This function generates violin plots for numeric variables in the dataset,
#' with boxplots and jittered points for enhanced visualization. The plots
#' are grouped by a specified column and saved as PNG files, if desired.
#' It handles large numbers of variables by splitting them into multiple
#' plots, each containing a set of variables (defined by the `vars_per_plot`
#' argument). The function also allows for customization of plot appearance
#' and save locations.
#'
#' @param data A data frame containing the data to plot.
#' @param vars_per_plot Integer. The number of variables (columns) to include
#'   in each individual plot. Default is 5.
#' @param save_dir String. The directory to save the plots in. Default is
#'   the directory `"StatObject"`.
#' @param palette_name String. The name of the color palette to use. The
#'   default is `"lisa::SalvadorDali"`, which uses a predefined color scheme.
#' @param group_col String. The name of the column in the data used to group
#'   the samples. Default is `"group"`.
#' @param max_unique_values Integer. The maximum number of unique values
#'   allowed for categorical variables. Defaults to 5. Used for filtering
#'   variables when diagnosing their types.
#' @param sub_var Character vector. A subset of variable names (columns)
#'   to include in the plot. If `NULL`, all numeric variables will be included.
#' @param save_plots Logical. If `TRUE`, the plots will be saved as PNG
#'   files in the specified `save_dir`. Default is `TRUE`.
#' @param plot_width Numeric. The width of each saved plot (in inches).
#'   Default is 5.
#' @param plot_height Numeric. The height of each saved plot (in inches).
#'   Default is 5.
#' @param base_size Numeric. The base font size for the plot. Default is 14.
#'
#' @returns A list of ggplot objects containing the generated violin plots.
#'   If `save_plots` is `TRUE`, the plots are also saved as PNG files in
#'   the specified directory.
#' @export
#'
#' @examples
#' violin_plots(data = my_data,
#'              vars_per_plot = 4,
#'              save_dir = "path/to/save_plots",
#'              palette_name = "lisa::SalvadorDali",
#'              group_col = "group",
#'              save_plots = TRUE)
violin_plots <- function(data,
                         vars_per_plot = 5,
                         save_dir = here::here("StatObject"),
                         palette_name = "lisa::SalvadorDali",
                         group_col = "group",
                         max_unique_values = 5,
                         sub_var = NULL,
                         save_plots = TRUE,
                         plot_width = 5,
                         plot_height = 5,
                         base_size = 14) {

  if (!is.null(group_col) && !group_col %in% names(data)) {
    stop(paste("Column", group_col, "not found in data"))
  }

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  num_cols <- variable_types$numeric_vars

  if (!is.null(sub_var)) {
    num_cols <- intersect(num_cols, sub_var)
  }

  if (length(num_cols) == 0) {
    stop("No numeric variables found after filtering with sub_var")
  }

  melted_data <- melt(data, id.vars = group_col, measure.vars = num_cols)

  if (is.null(group_col) || !group_col %in% names(melted_data)) {
    melted_data$group <- "Default Group"
  } else {
    melted_data$group <- melted_data[[group_col]]
  }

  num_vars <- length(num_cols)
  num_groups <- ceiling(num_vars / vars_per_plot)
  var_groups <- split(num_cols, rep(1:num_groups, each = vars_per_plot, length.out = num_vars))

  colors <- as.vector(paletteer::paletteer_d(palette_name))
  plot_list <- list()

  for (i in seq_along(var_groups)) {
    group_vars <- var_groups[[i]]
    group_data <- melted_data[melted_data$variable %in% group_vars, ]

    if (nrow(group_data) > 0) {
      p <- ggplot(group_data, aes(x = as.factor(group), y = value)) +
        geom_violin(aes(fill = after_stat(density)), scale = "area", alpha = 0.5) +
        geom_boxplot(position = position_dodge(width = 0.2), width = 0.1, size = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.3, color = "black", size = 0.5) +
        scale_fill_gradientn(colours = colorRampPalette(colors)(100),
                             limits = c(0, 1)) +
        facet_wrap(~variable, scales = "free_y", ncol = 1) +
        theme_prism(base_size = base_size) +
        labs(title = paste("Violin Plots - Part", i),
             x = "Group",
             y = "Value")

      plot_list[[i]] <- p

      if (save_plots) {
        if (!dir.exists(save_dir)) {
          dir.create(save_dir, recursive = TRUE)
        }

        ggsave(paste0(save_dir, "/violin_plot_part_", i, ".png"),
               plot = p,
               width = plot_width,
               height = plot_height)
      }
    }
  }

  return(plot_list)
}


#' Density Ridge Plots for Numeric Variables
#'
#' This function generates density ridge plots for numeric variables in the dataset,
#' with optional grouping by a specified column. It handles large numbers of variables
#' by splitting them into multiple plots, each containing a set of variables (defined
#' by the `vars_per_plot` argument). The function also allows for customization of plot
#' appearance and save locations.
#'
#' @param data A data frame containing the data to plot.
#' @param vars_per_plot Integer. The number of variables (columns) to include
#'   in each individual plot. Default is 5.
#' @param save_dir String. The directory to save the plots in. Default is
#'   the directory `"StatObject"`.
#' @param palette_name String. The name of the color palette to use. The
#'   default is `"lisa::SalvadorDali"`, which uses a predefined color scheme.
#' @param group_col String. The name of the column in the data used to group
#'   the samples. Default is `"group"`.
#' @param max_unique_values Integer. The maximum number of unique values
#'   allowed for categorical variables. Defaults to 5. Used for filtering
#'   variables when diagnosing their types.
#' @param sub_var Character vector. A subset of variable names (columns)
#'   to include in the plot. If `NULL`, all numeric variables will be included.
#' @param save_plots Logical. If `TRUE`, the plots will be saved as PNG
#'   files in the specified `save_dir`. Default is `TRUE`.
#' @param plot_width Numeric. The width of each saved plot (in inches).
#'   Default is 5.
#' @param plot_height Numeric. The height of each saved plot (in inches).
#'   Default is 5.
#' @param base_size Numeric. The base font size for the plot. Default is 14.
#'
#' @returns A list of ggplot objects containing the generated density ridge plots.
#'   If `save_plots` is `TRUE`, the plots are also saved as PNG files in
#'   the specified directory.
#' @export
#'
#' @examples
#' density_ridge_plots(data = my_data,
#'                     vars_per_plot = 4,
#'                     save_dir = "path/to/save_plots",
#'                     palette_name = "lisa::SalvadorDali",
#'                     group_col = "group",
#'                     save_plots = TRUE)
#'
density_ridge_plots <- function(data,
                                vars_per_plot = 5,
                                save_dir = here("StatObject"),
                                palette_name = "lisa::SalvadorDali",
                                group_col = "group",
                                max_unique_values = 5,
                                sub_var = NULL,
                                save_plots = TRUE,
                                plot_width = 5,
                                plot_height = 5,
                                base_size = 14) {

  if (length(group_col) == 0 || is.null(group_col) || !group_col %in% names(data)) {
    group_col <- NULL
  }

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  num_cols <- variable_types$numeric_vars

  if (!is.null(sub_var)) {
    sub_var <- intersect(sub_var, num_cols)

    if (length(sub_var) == 0) {
      stop("No valid variables in sub_var for plotting.")
    }

    num_cols <- sub_var
  }

  if (length(num_cols) == 0) {
    stop("No numeric variables found after filtering with sub_var\n")
  }

  melted_data <- melt(data, id.vars = group_col, measure.vars = num_cols)

  if (!is.null(group_col)) {
    melted_data$group <- melted_data[[group_col]]
  } else {
    melted_data$group <- "Default Group"
  }

  num_vars <- length(num_cols)
  num_groups <- ceiling(num_vars / vars_per_plot)
  var_groups <- split(num_cols, rep(1:num_groups, each = vars_per_plot, length.out = num_vars))

  colors <- as.vector(paletteer::paletteer_d(palette_name))
  plot_list <- list()

  for (i in seq_along(var_groups)) {
    group_vars <- var_groups[[i]]
    group_data <- melted_data[melted_data$variable %in% group_vars, ]

    if (nrow(group_data) > 0) {
      p <- ggplot(group_data, aes(x = value, y = group, fill = after_stat(density))) +
        geom_density_ridges_gradient(scale = 3, rel_min_height = 0.00) +
        scale_fill_gradientn(colours = colorRampPalette(colors)(100),
                             limits = c(0, 1)) +
        facet_wrap(~variable, scales = "free_x", ncol = 1) +
        theme_prism(base_size = base_size) +
        labs(title = paste("Density Ridge Plots  - Part", i),
             x = "Value",
             y = ifelse(!is.null(group_col), group_col, "Group"))

      plot_list[[i]] <- p

      if (save_plots) {
        if (!dir.exists(save_dir)) {
          dir.create(save_dir, recursive = TRUE)
        }

        ggsave(paste0(save_dir, "/density_ridge_plot_part_", i, ".png"),
               plot = p,
               width = plot_width,
               height = plot_height)
      }
    }
  }

  return(plot_list)
}


#' Numeric Descriptive Plots (Violin or Ridge Density)
#'
#' This function generates either violin plots or ridge density plots for numeric variables
#' in the given dataset (either a `Stat` object or a data frame). The function allows
#' customizing the number of variables per plot, palette style, and grouping columns, and
#' saves the plots if required. It provides a flexible approach to visualizing the distribution
#' of numeric data and comparing different groups.
#'
#' @param object An object of class `Stat` or a data frame containing numeric data.
#'   If the object is of class `Stat`, the clean data is extracted from the `Stat` object.
#' @param vars_per_plot Integer. The number of variables (columns) to include in each
#'   individual plot. Default is 5.
#' @param save_dir String. The directory where the plots will be saved. Default is
#'   `"StatObject/numeric_descriptive"`.
#' @param palette_name String. The name of the color palette to use. Default is
#'   `"lisa::SalvadorDali"`.
#' @param group_col String. The name of the column in the data to group the samples by.
#'   Default is `"group"`. If not provided, no grouping is performed.
#' @param max_unique_values Integer. The maximum number of unique values allowed for
#'   categorical variables. Default is 5. Used for filtering variables when diagnosing
#'   their types.
#' @param plot_type String. The type of plot to generate. Options are `"violin"` or
#'   `"ridge"`. Default is `"violin"`.
#' @param save_plots Logical. If `TRUE`, the plots will be saved as PNG files in
#'   the specified `save_dir`. Default is `TRUE`.
#' @param plot_width Numeric. The width of each saved plot (in inches). Default is 5.
#' @param plot_height Numeric. The height of each saved plot (in inches). Default is 5.
#' @param base_size Numeric. The base font size for the plot. Default is 14.
#' @param sub_var Character vector. A subset of variable names (columns) to include in
#'   the plot. If `NULL`, all numeric variables are included.
#'
#' @returns The input object (either a `Stat` object or a data frame) with the
#'   updated plots added to the appropriate slot. If the input is a `Stat` object,
#'   the `compute.descriptive` slot is updated with the generated plots.
#' @export
#'
#' @examples
#' plot_numeric_descriptive(object = my_stat_object,
#'                           vars_per_plot = 4,
#'                           save_dir = "path/to/save_plots",
#'                           palette_name = "lisa::SalvadorDali",
#'                           group_col = "group",
#'                           plot_type = "violin",
#'                           save_plots = TRUE)
#'
plot_numeric_descriptive <- function(
    object,
    vars_per_plot = 5,
    save_dir = here("StatObject", "numeric_descriptive"),
    palette_name = "lisa::SalvadorDali",
    group_col = "group",
    max_unique_values = 5,
    plot_type = "violin",
    save_plots = TRUE,
    plot_width = 5,
    plot_height = 5,
    base_size = 14,
    sub_var = NULL) {


  if (inherits(object, "Stat")) {
    cat("Input is an object of class 'Stat'. Extracting data...\n")
    data <- object@clean.data
    group_col <- object@group_col
    if (length(group_col) == 0) {
      group_col <- NULL
      cat("No group_col found in Stat object. Using NULL.\n")
    }
  } else if (is.data.frame(object)) {
    cat("Input is a data frame. Using it directly.\n")
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame\n")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input\n")
  }


  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)


  if (length(variable_types$numeric_vars) == 0) {
    stop("No valid numeric variables found after diagnosis\n")
  }

  if (!is.null(sub_var)) {
    cat("Filtering numeric variables based on sub_var:", sub_var, "\n")
    numeric_vars <- intersect(variable_types$numeric_vars, sub_var)
  } else {
    numeric_vars <- variable_types$numeric_vars
  }

  if (length(numeric_vars) == 0) {
    stop("No valid numeric variables found after filtering with sub_var\n")
  }


  plots_list <- list()

  if (plot_type == "violin") {
    cat("Generating violin plots...\n")
    plots_list <- violin_plots(data,
                               vars_per_plot = vars_per_plot,
                               save_dir = save_dir,
                               palette_name = palette_name,
                               group_col = group_col,
                               max_unique_values = max_unique_values,
                               save_plots = save_plots,
                               plot_width = plot_width,
                               plot_height = plot_height,
                               base_size = base_size,
                               sub_var = numeric_vars)
  } else if (plot_type == "ridge") {
    cat("Generating ridge density plots...\n")
    plots_list <- density_ridge_plots(data,
                                      vars_per_plot = vars_per_plot,
                                      save_dir = save_dir,
                                      palette_name = palette_name,
                                      group_col = group_col,
                                      max_unique_values = max_unique_values,
                                      save_plots = save_plots,
                                      plot_width = plot_width,
                                      plot_height = plot_height,
                                      base_size = base_size,
                                      sub_var = numeric_vars)
  } else {
    stop("Invalid plot type. Choose either 'violin' or 'ridge'.\n")
  }

  if (length(plots_list) > 0) {
    print(plots_list[[1]])
  }

  total_plots <- length(plots_list)
  cat("A total of", total_plots, "plots were generated.\n")

  if (inherits(object, "Stat")) {
    object@compute.descriptive[[paste0(plot_type, "_plots")]] <- plots_list
    cat("Updating 'Stat' object...\n")
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'compute.descriptive' slot updated.\n")
  }


  return(object)
}

#' Convert Variables to Numeric or Factor Based on Their Types
#'
#' This function converts the variables (columns) in the input data frame to either
#' numeric or factor types based on the information provided in `variable_types`.
#' Numeric variables are converted to numeric data type, and non-numeric variables
#' are converted to factors. The function uses `variable_types` to determine which
#' variables should be treated as numeric and which should be treated as factors.
#'
#' @param data A data frame containing the variables to be converted.
#' @param variable_types A list containing the variable types. This should have a
#'   component `numeric_vars`, which is a character vector of variable names that
#'   should be converted to numeric.
#'
#' @returns A data frame with the variables converted to the appropriate types
#'   (numeric or factor).
#' @export
#'
#' @examples
#' # Example of usage
#' data <- data.frame(a = c(1, 2, 3), b = c("low", "medium", "high"))
#' variable_types <- list(numeric_vars = c("a"))
#' converted_data <- convert_variables(data, variable_types)
#'
convert_variables <- function(data, variable_types) {
  stopifnot(is.data.frame(data))
  for (col in names(data)) {
    if (col %in% variable_types$numeric_vars) {
      data[[col]] <- as.numeric(data[[col]])
      cat("Converted ", col, " to numeric.\n")
    } else {
      data[[col]] <- factor(data[[col]])
      cat("Converted", col, "to factor.\n")
    }
  }
  return(data)
}

#' Convert Variables in a 'Stat' Object or Data Frame
#'
#' This function converts variables in a given object (either of class 'Stat' or a
#' data frame) to numeric or factor types based on the information provided by
#' `diagnose_variable_type`. If the input is a 'Stat' object, the conversion will
#' update its `clean.data` slot. If the input is a data frame, it will directly
#' return the converted data frame.
#'
#' @param object An object of class 'Stat' or a data frame. If a 'Stat' object is
#'   provided, the function will update its `clean.data` slot.
#' @param group_col A string representing the column name that groups the data.
#'   Default is "group". This column is used to determine the type of variables.
#' @param max_unique_values The maximum number of unique values allowed for a variable
#'   to be considered as numeric. Default is 5.
#'
#' @returns If the input is a 'Stat' object, it returns the updated 'Stat' object.
#'   If the input is a data frame, it returns the converted data frame.
#' @export
#'
#' @examples
#' # Example of usage
#' data <- data.frame(a = c(1, 2, 3), b = c("low", "medium", "high"))
#' stat_object <- Stat$new(clean.data = data, group_col = "group")
#' updated_stat_object <- stat_convert_variables(stat_object)
#'
stat_convert_variables <- function(object, group_col = "group", max_unique_values = 5) {
  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)

  row_names <- rownames(data)
  data <- convert_variables(data, variable_types)
  rownames(data) <- row_names

  if (inherits(object, "Stat")) {
    object@clean.data <- data
    cat("Updating 'Stat' object...\n")
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'clean.data' slot updated.\n")

    return(object)
  }

  return(data)
}


#' Log Transformation of Input Data
#'
#' This function applies a log transformation to the input vector `x`. The function
#' adds a small constant (0.00000001) to each value before applying the logarithm to
#' avoid taking the log of zero. It checks that the input does not contain negative
#' values, as log transformation is not valid for negative values.
#'
#' @param x A numeric vector to be log-transformed. The function assumes that the
#'   input contains non-negative values.
#'
#' @returns A list containing:
#'   - `scaled_data`: A numeric vector of log-transformed values.
#'   - `normalize_method`: A string indicating the normalization method used (log transformation).
#'   - `normalize_info`: An empty list that can be used to store additional normalization details.
#'
#' @export
#'
#' @examples
#' # Example of using log_transform
#' x <- c(1, 2, 3, 10)
#' result <- log_transform(x)
#' result$scaled_data  # The log-transformed values
#'
log_transform <- function(x) {
  if (any(x < 0)) {
    stop("Input contains negative values. Log transformation is not possible.")
  }
  scaled_data <- log(x + 0.00000001)
  return(list(scaled_data = scaled_data,
              normalize_method = "log_transform",
              normalize_info = list()))
}

#' Min-Max Scaling of Input Data
#'
#' This function performs Min-Max scaling on the input data. It normalizes each
#' column by subtracting the minimum value and dividing by the range (max - min)
#' for that column. It checks if any columns have constant values, as Min-Max scaling
#' cannot be performed on columns where the minimum and maximum values are the same.
#'
#' @param x A data frame or matrix to be scaled. Each column will be scaled independently.
#'
#' @returns A list containing:
#'   - `scaled_data`: A data frame or matrix with Min-Max scaled values.
#'   - `normalize_method`: A string indicating the normalization method used (Min-Max scaling).
#'   - `normalize_info`: A list containing the `min_vals` and `max_vals` for each column.
#'
#' @export
#'
#' @examples
#' # Example of using min_max_scale
#' x <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' result <- min_max_scale(x)
#' result$scaled_data  # The scaled values
#' result$normalize_info  # The min and max values used for scaling
#'
min_max_scale <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x)
  min_vals <- apply(x, 2, min)
  max_vals <- apply(x, 2, max)
  if (any(max_vals == min_vals)) {
    stop("Some columns have constant values. Min-Max scaling is not possible.")
  }
  scaled_data <- as.data.frame(sweep(sweep(x, 2, min_vals, "-"), 2, (max_vals - min_vals), "/"))
  return(list(scaled_data = scaled_data,
              normalize_method = "min_max_scale",
              normalize_info = list(min_vals = min_vals,
                                    max_vals = max_vals)))
}

#' Z-Score Standardization of Input Data
#'
#' This function performs Z-score standardization on the input data. It normalizes each
#' column by subtracting the mean and dividing by the standard deviation for that column.
#' If the standard deviation is zero (i.e., constant values), it sets the standard deviation
#' to 1 to avoid division by zero.
#'
#' @param x A data frame or matrix to be standardized. Each column will be standardized independently.
#'
#' @returns A list containing:
#'   - `scaled_data`: A data frame or matrix with Z-score standardized values.
#'   - `normalize_method`: A string indicating the normalization method used (Z-score standardization).
#'   - `normalize_info`: A list containing the `mean_vals` and `sd_vals` for each column.
#'
#' @export
#'
#' @examples
#' # Example of using z_score_standardize
#' x <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' result <- z_score_standardize(x)
#' result$scaled_data  # The standardized values
#' result$normalize_info  # The mean and SD values used for standardization
#'
z_score_standardize <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x)
  mean_vals <- apply(x, 2, mean)
  sd_vals <- apply(x, 2, sd)
  sd_vals[sd_vals == 0] <- 1
  standardized_data <- as.data.frame(sweep(sweep(x, 2, mean_vals, "-"), 2, sd_vals, "/"))
  return(list(scaled_data = standardized_data,
              normalize_method = "z_score_standardize",
              normalize_info = list(mean_vals = mean_vals,
                                    sd_vals = sd_vals)))
}

#' Center Data (Zero Mean)
#'
#' This function centers the input data by subtracting the mean from each column.
#' The data is transformed such that each column has a mean of zero. The scale argument
#' is set to `FALSE`, meaning no scaling (division by standard deviation) is applied,
#' only centering (shifting values to zero mean).
#'
#' @param data A data frame or matrix to be centered. Each column will be centered independently.
#'
#' @returns A list containing:
#'   - `scaled_data`: A data frame or matrix with centered values (mean subtracted).
#'   - `normalize_method`: A string indicating the normalization method used (center_data).
#'   - `normalize_info`: A list with additional information (empty in this case).
#'
#' @export
#'
#' @examples
#' # Example of using center_data
#' data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' result <- center_data(data)
#' result$scaled_data  # The centered values
#' result$normalize_method  # Method used for normalization
#'
center_data <- function(data) {
  centered_data <- scale(data, center = TRUE, scale = FALSE)
  scaled_data <- as.data.frame(centered_data)
  return(list(scaled_data = scaled_data,
              normalize_method = "center_data",
              normalize_info = list()))
}

#' Scale Data (Standardization)
#'
#' This function standardizes the input data by dividing each column by its standard deviation.
#' Columns with zero standard deviation will be handled by replacing their standard deviation with 1,
#' ensuring no division by zero errors occur. The data is transformed such that each column has unit variance.
#'
#' @param x A data frame or matrix to be standardized. Each column will be scaled independently.
#'
#' @returns A list containing:
#'   - `scaled_data`: A data frame or matrix with standardized values (scaled by the standard deviation of each column).
#'   - `normalize_method`: A string indicating the normalization method used (scale_data).
#'   - `normalize_info`: A list containing the scaling values (standard deviations for each column).
#'
#' @export
#'
#' @examples
#' # Example of using scale_data
#' data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' result <- scale_data(data)
#' result$scaled_data  # The scaled values
#' result$normalize_method  # Method used for normalization
#'
scale_data <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x)
  scale_vals <- apply(x, 2, sd)
  scale_vals[scale_vals == 0] <- 1
  scaled_data <- as.data.frame(sweep(x, 2, scale_vals, "/"))
  return(list(scaled_data = scaled_data,
              normalize_method = "scale_data",
              normalize_info = list(scale_vals = scale_vals)))
}


#' Normalize Data with Various Methods
#'
#' This function normalizes the numeric columns in the provided data frame using a selected method.
#' The available methods include log transformation, min-max scaling, Z-score standardization,
#' centering, and scaling. It also ensures that non-numeric columns (e.g., categorical) are preserved
#' during normalization.
#'
#' @param data A data frame containing the data to be normalized. Numeric columns will be transformed
#'             based on the selected normalization method.
#' @param normalize_method A string specifying the normalization method. One of: "log_transform",
#'                         "min_max_scale", "z_score_standardize", "max_abs_scale", "center_data",
#'                         or "scale_data". Default is "min_max_scale".
#' @param group_col A string specifying the column that contains group information. This is used for
#'                  diagnostic purposes and is optional. Default is "group".
#' @param max_unique_values The maximum number of unique values to classify a column as categorical.
#'                           Default is 5. Used in `diagnose_variable_type` to classify variables.
#'
#' @returns A list containing:
#'   - `scaled_data`: A data frame with normalized values for the numeric columns.
#'   - `normalize_method`: The method used for normalization (as a string).
#'   - `normalize_info`: A list containing additional information related to the normalization process.
#'
#' @export
#'
#' @examples
#' # Example of using normalize_data
#' data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), group = c("X", "Y", "X"))
#' result <- normalize_data(data, normalize_method = "z_score_standardize")
#' result$scaled_data  # The normalized data
#' result$normalize_method  # Method used for normalization
#'
normalize_data <- function(data,
                           normalize_method = "min_max_scale",
                           group_col = "group",
                           max_unique_values = 5) {
  stopifnot(is.data.frame(data))

  if (length(group_col) == 0 || is.null(group_col) || !group_col %in% colnames(data)) {
    group_col <- NULL
  }

  valid_methods <- c("log_transform", "min_max_scale", "z_score_standardize",
                     "max_abs_scale", "center_data", "scale_data")
  stopifnot(normalize_method %in% valid_methods)

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  num_cols <- variable_types$numeric_vars
  count_cols <- setdiff(names(data), num_cols)

  if (length(num_cols) > 0) {
    cat("Normalizing the following numeric columns:", num_cols, "\n")
    data[, num_cols] <- abs(data[, num_cols])

    row_names <- rownames(data)

    normalized_result <- switch(
      normalize_method,
      "log_transform" = {
        cat("Applying log transformation...\n")
        log_transform(data[, num_cols])
      },
      "min_max_scale" = {
        cat("Applying min-max scaling...\n")
        min_max_scale(data[, num_cols])
      },
      "z_score_standardize" = {
        cat("Applying Z-score standardization...\n")
        z_score_standardize(data[, num_cols])
      },
      "center_data" = {
        cat("Centering data...\n")
        center_data(data[, num_cols])
      },
      "scale_data" = {
        cat("Scaling data...\n")
        scale_data(data[, num_cols])
      }
    )

    normalized_result$scaled_data <- cbind(normalized_result$scaled_data, data[, count_cols, drop = FALSE])
    rownames(normalized_result$scaled_data) <- row_names
    return(normalized_result)
  } else {
    stop("No numeric columns to normalize.")
  }
}

#' Normalize Data in 'Stat' Object or Data Frame
#'
#' This function normalizes the numeric columns in the provided 'Stat' object or data frame using a selected method.
#' It checks for the existence of valid data and applies the chosen normalization method to the numeric columns.
#' The normalized data can either be returned as a data frame or integrated back into the 'Stat' object.
#'
#' @param object An object of class 'Stat' or a data frame containing the data to be normalized.
#'               The 'Stat' object should contain a slot 'clean.data' for data and an optional slot
#'               'group_col' for grouping information.
#' @param normalize_method A string specifying the normalization method. One of: "log_transform",
#'                         "min_max_scale", "z_score_standardize", "max_abs_scale", "center_data",
#'                         or "scale_data". Default is "log_transform".
#' @param group_col A string specifying the column that contains group information. This is used for
#'                  diagnostic purposes and is optional. Default is "group".
#' @param max_unique_values The maximum number of unique values to classify a column as categorical.
#'                           Default is 5. Used in `diagnose_variable_type` to classify variables.
#'
#' @returns A 'Stat' object with updated 'scale.data' slot if the input is a 'Stat' object,
#'          or a normalized data frame if the input is a data frame.
#'
#' @export
#'
#' @examples
#' # Example of using stat_normalize_process with a 'Stat' object
#' stat_obj <- Stat$new(clean.data = data, group_col = "group")
#' normalized_stat_obj <- stat_normalize_process(stat_obj, normalize_method = "z_score_standardize")
#' normalized_stat_obj@scale.data  # The normalized data
#'
#' # Example of using stat_normalize_process with a data frame
#' data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), group = c("X", "Y", "X"))
#' normalized_data <- stat_normalize_process(data, normalize_method = "min_max_scale")
#' normalized_data  # The normalized data frame
#'
stat_normalize_process <- function(object,
                                   normalize_method = "log_transform",
                                   group_col = "group",
                                   max_unique_values = 5) {
  cat("Input object class:", class(object), "\n")

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")

    if (length(group_col) == 0 || is.null(group_col) || !group_col %in% colnames(data)) {
      cat("Group column is not valid, setting to NULL.\n")
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  cat("Starting normalization process...\n")

  nm_result <- normalize_data(data,
                              normalize_method = normalize_method,
                              group_col = group_col,
                              max_unique_values = max_unique_values)

  nmdat <- nm_result$scaled_data
  if (inherits(object, "Stat")) {
    if (!is.null(slotNames(object)) && "scale.data" %in% slotNames(object)) {
      object@scale.data <- nmdat
      cat("Updating 'Stat' object...\n")
      cat("The 'Stat' object has been updated with the following slots:\n")
      cat("- 'scale.data' slot updated.\n")
    } else {
      stop("The 'Stat' object does not have a 'scale.data' slot.")
    }
    return(object)
  }

  cat("Normalization complete, returning normalized data frame.\n")
  return(nmdat)
}


#' One-Hot Encoding for Categorical Variables
#'
#' This function performs one-hot encoding on categorical variables in the provided data frame.
#' It identifies categorical variables based on the number of unique values and encodes them
#' into binary columns where each unique value of the categorical variable becomes a separate column.
#' If a group column is provided, it is moved to the end of the data frame.
#'
#' @param data A data frame containing the data to be encoded.
#' @param group_col A string specifying the column that contains group information. This is optional and,
#'                  if provided, will be moved to the end of the encoded data frame. Default is "group".
#' @param max_unique_values The maximum number of unique values a variable can have to be considered categorical.
#'                           Default is 5. This is used in `diagnose_variable_type` to determine which columns
#'                           should be one-hot encoded.
#'
#' @returns A data frame where categorical variables are replaced by one-hot encoded columns.
#'          The group column, if provided, is placed at the end of the data frame.
#'
#' @export
#'
#' @examples
#' # Example of one-hot encoding on a data frame with a categorical variable
#' data <- data.frame(
#'   group = c("A", "B", "A", "B", "C"),
#'   category = c("X", "Y", "X", "Z", "Y")
#' )
#' encoded_data <- one_hot_encode(data, group_col = "group")
#' encoded_data
#'
one_hot_encode <- function(data, group_col = "group", max_unique_values = 5) {
  if (!is.data.frame(data)) stop("Input must be a data frame")

  if (length(group_col) == 0 || !is.character(group_col) || !(group_col %in% colnames(data))) {
    cat("Group column is not valid, setting to NULL.\n")
    group_col <- NULL
  }

  if (!is.numeric(max_unique_values) || max_unique_values <= 0) {
    stop("max_unique_values must be a positive numeric value")
  }

  variable_types <- diagnose_variable_type(data, group_col = group_col, max_unique_values = max_unique_values)
  vars_to_encode <- variable_types$vars_to_encode

  encoded_data <- data
  row_names <- rownames(data)

  for (var in vars_to_encode) {
    unique_values <- unique(data[!is.na(data[, var]), var])
    cat("Encoding variable:", var, "with unique values:", unique_values, "\n")
    for (value in unique_values) {
      col_name <- paste(var, value, sep = "_")
      encoded_data[, col_name] <- as.integer(data[, var] == value)
    }
    encoded_data[, var] <- NULL
  }

  if (!is.null(group_col) && group_col %in% names(encoded_data)) {
    group <- encoded_data[[group_col]]
    encoded_data[[group_col]] <- NULL
    encoded_data <- cbind(encoded_data, group)
    colnames(encoded_data)[ncol(encoded_data)] <- group_col
  }

  rownames(encoded_data) <- row_names
  return(encoded_data)
}

#' Perform One-Hot Encoding on a Data Object
#'
#' This function applies one-hot encoding to the categorical variables in the input object, which can either
#' be a `Stat` object or a data frame. It utilizes the `one_hot_encode` function to perform the encoding
#' and updates the `Stat` object with the encoded data, or returns the encoded data frame if the input is a data frame.
#'
#' @param object An object of class 'Stat' or a data frame containing the data to be encoded.
#' @param method A method for encoding (not currently used, but kept for consistency with other functions).
#' @param group_col A string specifying the column that contains group information. This is optional, and if provided,
#'                  the group column will be moved to the end of the encoded data frame. Default is "group".
#' @param max_unique_values The maximum number of unique values a variable can have to be considered categorical.
#'                           Default is 5. This is used in `diagnose_variable_type` to determine which columns
#'                           should be one-hot encoded.
#'
#' @returns The input `Stat` object with its data updated with one-hot encoded values in the 'clean.data' slot,
#'          or a data frame containing the one-hot encoded data if the input was a data frame.
#'
#' @export
#'
#' @examples
#' # Example usage with a data frame
#' data <- data.frame(
#'   group = c("A", "B", "A", "B", "C"),
#'   category = c("X", "Y", "X", "Z", "Y")
#' )
#' encoded_data <- stat_onehot_encode(data, group_col = "group")
#' encoded_data
#'
#' # Example usage with a 'Stat' object (assuming 'stat_object' is a valid Stat object)
#' stat_object <- stat_onehot_encode(stat_object, group_col = "group")
#'
stat_onehot_encode <- function(object, method = 1, group_col = "group", max_unique_values = 5) {
  cat("Input object class:", class(object), "\n")

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")
    group_col <- slot(object, "group_col")

    if (length(group_col) == 0 || is.null(group_col)) {
      cat("Group column is not valid, setting to NULL.\n")
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  cat("Starting one-hot encoding on data...\n")

  onehot_data <- one_hot_encode(data, group_col = group_col, max_unique_values = max_unique_values)

  if (inherits(object, "Stat")) {
    if (!is.null(slotNames(object)) && "clean.data" %in% slotNames(object)) {
      object@clean.data <- onehot_data
      cat("Updating 'Stat' object...\n")
      cat("The 'Stat' object has been updated with the following slots:\n")
      cat("- 'clean.data' slot updated.\n")
    } else {
      stop("The 'Stat' object does not have a 'clean.data' slot.")
    }
    return(object)
  }

  cat("One-hot encoding complete, returning encoded data frame.\n")
  return(onehot_data)
}

