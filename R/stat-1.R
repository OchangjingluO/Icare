#' Modify column names of a data frame
#'
#' This function modifies the column names of a data frame by performing the following:
#' 1. Converts all column names to lowercase.
#' 2. Replaces spaces with underscores.
#' 3. Removes any non-alphanumeric characters except for underscores.
#' 4. Ensures all column names are unique by appending numbers to duplicated names.
#'
#' @param data A data frame whose column names need to be modified.
#'
#' @returns A data frame with modified column names.
#' @export
#'
#' @examples
#' data <- data.frame("First Name" = c("John", "Jane"), "Age" = c(25, 30))
#' modified_data <- modify_column_names(data)
#' print(modified_data)
modify_column_names <- function(data) {
  if (is.null(names(data))) {
    stop("Column names of the data frame cannot be NULL!")
  }

  new_names <- names(data)
  new_names <- tolower(new_names)
  new_names <- gsub(" ", "_", new_names)
  new_names <- gsub("[^[:alnum:]_]", "", new_names)

  if (anyDuplicated(new_names)) {
    warning("Duplicate column names found; they have been made unique.")
    new_names <- make.unique(new_names)
  }

  names(data) <- new_names
  return(data)
}


#' Class to store statistical analysis results
#'
#' This class is designed to store and manage various components of statistical analysis.
#' It includes raw and cleaned data, processed results, descriptive statistics, correlation results,
#' variable types, and metadata related to the analysis.
#'
#' @slot raw.data A data.frame containing the raw input data.
#' @slot clean.data A data.frame containing the cleaned data after preprocessing.
#' @slot info.data A data.frame containing additional information related to the data.
#' @slot scale.data A data.frame containing the scaled version of the data.
#' @slot meta.featurename A character vector containing metadata feature names.
#' @slot outlier.marked A list marking outliers detected in the data.
#' @slot group_col The grouping column in the data, which can be of any type.
#' @slot baseline.table A table that stores baseline information, can be of any type.
#' @slot processe.info A list containing information about the processing steps.
#' @slot compute.descriptive A list containing results of descriptive statistics computations.
#' @slot corr.result A list containing the results of correlation analysis.
#' @slot var.result A list containing the results of variance analysis.
#' @slot variable.types A list specifying the types of the variables in the dataset.
#'
#' @returns An object of class 'Stat' containing the slots mentioned above.
#' @export
#'
#' @examples
#' stat_obj <- new("Stat", raw.data = my_raw_data, clean.data = my_clean_data)
Stat <- setClass(
  Class = 'Stat',
  slots = c(
    raw.data = 'data.frame',
    clean.data = 'data.frame',
    info.data = 'data.frame',
    scale.data = 'data.frame',
    meta.featurename = 'character',
    outlier.marked = 'list',
    group_col = 'ANY',
    baseline.table = 'ANY',
    processe.info = 'list',
    compute.descriptive = 'list',
    corr.result = 'list',
    var.result = 'list',
    variable.types = 'list'
  ),
  prototype = list(
    raw.data = data.frame(),
    clean.data = data.frame(),
    info.data = data.frame(),
    scale.data = data.frame(),
    outlier.marked = list(),
    meta.featurename = character(),
    group_col = NULL,
    baseline.table = NULL,
    processe.info = list(),
    compute.descriptive = list(),
    corr.result = list(),
    var.result = list(),
    variable.types = list()
  )
)


#' Create a Stat object for statistical analysis
#'
#' This function creates a `Stat` object, which is used to store and manage various components
#' of statistical analysis, such as raw data, cleaned data, additional metadata, and processing
#' information. It performs basic checks and preparation on the input data before creating the object.
#'
#' @param raw.data A data.frame containing the raw data for analysis. Defaults to an empty data frame.
#' @param clean.data A data.frame containing the cleaned data for analysis. Defaults to an empty data frame.
#' @param info.data A data.frame containing additional metadata related to the data. Defaults to an empty data frame.
#' @param group_col A character string specifying the name of the column used for grouping. Default is `"group"`.
#' @param ... Additional arguments passed to methods (not used in this function).
#'
#' @returns An object of class `Stat`, which contains the processed data and metadata as slots.
#' @export
#'
#' @examples
#' # Creating a Stat object with example data
#' stat_obj <- CreateStatObject(raw.data = example_raw_data, clean.data = example_clean_data)
#'
#' # Creating a Stat object with metadata
#' stat_obj <- CreateStatObject(raw.data = example_raw_data, clean.data = example_clean_data, info.data = example_info_data)
CreateStatObject <- function(
    raw.data = data.frame(),
    clean.data = data.frame(),
    info.data = data.frame(),
    group_col = "group",
    ...
) {

  # Ensure data are data.frames
  raw.data <- as.data.frame(raw.data)
  clean.data <- as.data.frame(clean.data)
  info.data <- as.data.frame(info.data)

  # Check if at least one of raw.data or clean.data is non-empty
  if (nrow(raw.data) == 0 && nrow(clean.data) == 0) {
    stop("At least one of 'raw.data' or 'clean.data' must be provided and not empty.")
  }

  # Check if row names of info.data match the row names of raw.data or clean.data
  if (nrow(info.data) > 0) {
    if (nrow(raw.data) > 0 && !all(rownames(info.data) == rownames(raw.data))) {
      stop("Row names in 'info.data' do not match 'raw.data'.")
    }
    if (nrow(clean.data) > 0 && !all(rownames(info.data) == rownames(clean.data))) {
      stop("Row names in 'info.data' do not match 'clean.data'.")
    }
  }

  # Prepare the data by ensuring unique row and column names
  prepare_data <- function(data, data_name) {
    if (nrow(data) == 0) {
      return(data)
    }

    if (anyDuplicated(rownames(data))) {
      warning(paste("Duplicate row names found in", data_name, "; they have been made unique."))
      rownames(data) <- make.unique(rownames(data))
    }

    if (anyDuplicated(colnames(data))) {
      warning(paste("Duplicate column names found in", data_name, "; they have been made unique."))
      colnames(data) <- make.unique(colnames(data))
    }

    if (is.null(colnames(data))) {
      stop(paste(data_name, "is missing column names."))
    }

    data <- modify_column_names(data)

    cat(paste("Data prepared for", data_name, "\n"))

    return(data)
  }

  # Prepare raw and clean data
  raw.data <- prepare_data(raw.data, "raw.data")
  clean.data <- prepare_data(clean.data, "clean.data")

  # Set meta feature names based on the columns of the data
  if (nrow(raw.data) > 0) {
    meta.featurename <- as.character(colnames(raw.data))
  } else if (nrow(clean.data) > 0) {
    meta.featurename <- as.character(colnames(clean.data))
  } else {
    meta.featurename <- character()
  }

  # Handle info.data when it's missing
  if (nrow(info.data) == 0) {
    if (nrow(clean.data) > 0) {
      info.data <- data.frame(row.names = row.names(clean.data))
    } else if (nrow(raw.data) > 0) {
      info.data <- data.frame(row.names = row.names(raw.data))
    }
  } else {
    if (nrow(clean.data) > 0) {
      rownames(info.data) <- rownames(clean.data)
    } else if (nrow(raw.data) > 0) {
      rownames(info.data) <- rownames(raw.data)
    }
  }

  cat("Final info.data prepared.\n")

  # Create the Stat object
  Stat <- new(
    Class = 'Stat',
    raw.data = raw.data,
    info.data = info.data,
    clean.data = clean.data,
    meta.featurename = meta.featurename,
    processe.info = list(),
    group_col = group_col
  )

  cat("Stat object created.\n")

  return(Stat)
}



#' Plot Missing Data Distribution
#'
#' This function generates density plots to visualize the distribution of missing data in the given dataset.
#' It provides a variable-wise and sample-wise missing data distribution plot, as well as a combined plot.
#' The function also saves the plots as PNG files if specified and returns a summary of missing data statistics.
#'
#' @param data A data.frame containing the data to be analyzed. Missing values should be represented as `<NA>` or `NA`.
#' @param palette_name A character string specifying the palette name for the plot colors. Default is `"lisa::SalvadorDali"`.
#' @param alpha A numeric value between 0 and 1 specifying the transparency level of the density plots. Default is `0.9`.
#' @param save_plots A logical value indicating whether to save the plots as PNG files. Default is `TRUE`.
#' @param save_dir A character string specifying the directory where the plots will be saved. Default is `"here('StatObject')"`.
#' @param plot_width A numeric value specifying the width of the saved plots. Default is `5`.
#' @param plot_height A numeric value specifying the height of the saved plots. Default is `5`.
#' @param base_size A numeric value specifying the base size for text and elements in the plot. Default is `14`.
#'
#' @returns A list containing the following:
#'   - `var_plot_obj`: The ggplot object for the variable-wise missing data distribution plot.
#'   - `sample_plot_obj`: The ggplot object for the sample-wise missing data distribution plot.
#'   - `combined_plot`: The ggplot object for the combined missing data distribution plot.
#'   - `total_missing_values`: The total number of missing values in the data.
#'   - `total_variables`: The total number of variables (columns) in the data.
#'   - `total_samples`: The total number of samples (rows) in the data.
#'   - `variables_with_missing`: The number of variables with at least one missing value.
#'   - `samples_with_missing`: The number of samples with at least one missing value.
#'   - `max_missing_variable`: The highest percentage of missing values in any variable.
#'   - `min_missing_variable`: The lowest percentage of missing values in any variable.
#'   - `max_missing_sample`: The highest percentage of missing values in any sample.
#'   - `min_missing_sample`: The lowest percentage of missing values in any sample.
#'
#' @export
#'
#' @examples
#' # Generate missing data plots for a dataset
#' missing_info <- plot_missing_data(data = example_data, save_plots = TRUE)
#'
#' # Generate missing data plots with customized parameters
#' missing_info <- plot_missing_data(data = example_data, palette_name = "lisa::Sunset", alpha = 0.7, plot_width = 6, plot_height = 6)
plot_missing_data <- function(data,
                              palette_name = "lisa::SalvadorDali",
                              alpha = 0.9,
                              save_plots = TRUE,
                              save_dir = here("StatObject"),
                              plot_width = 5,
                              plot_height = 5,
                              base_size = 14) {
  colors <- as.vector(paletteer::paletteer_d(palette_name))

  primary_color <- colors[1]
  secondary_color <- colors[2]

  # Replace '<NA>' with actual NA values in the data
  data[data == '<NA>'] <- NA

  # Calculate missing percentage for variables and samples
  var_missing_percentage <- colMeans(is.na(data)) * 100
  sample_missing_percentage <- rowMeans(is.na(data)) * 100

  # If no missing data, return early
  if (all(var_missing_percentage == 0) && all(sample_missing_percentage == 0)) {
    cat("No missing values in the data.")
    return(list(var_plot_obj = NULL, sample_plot_obj = NULL, combined_plot = NULL))
  }

  # If missing data is low, print a message
  if (mean(var_missing_percentage) < 5 && mean(sample_missing_percentage) < 5) {
    cat("The percentage of missing data is low (below 5%).")
  }

  # Prepare data for plotting
  var_missing_df <- data.frame(Variable = names(var_missing_percentage), Missing_Percentage = var_missing_percentage)
  sample_missing_df <- data.frame(Sample = 1:nrow(data), Missing_Percentage = sample_missing_percentage)
  # Create density plot for variable-wise missing data
  var_plot_obj <- ggplot(data = var_missing_df, aes(x = Missing_Percentage, fill = "Variable-wise")) +
    geom_density(alpha = alpha, color = NA) +
    labs(x = "Missing Percentage", y = "Density", title = "Distribution of Missing Data (Variable-wise)") +
    scale_fill_manual(values = primary_color) +
    theme_prism(base_size = base_size) +
    theme(legend.position = "top")

  # Create density plot for sample-wise missing data
  sample_plot_obj <- ggplot(data = sample_missing_df, aes(x = Missing_Percentage, fill = "Sample-wise")) +
    geom_density(alpha = alpha, color = NA) +
    labs(x = "Missing Percentage", y = "Density", title = "Distribution of Missing Data (Sample-wise)") +
    scale_fill_manual(values = secondary_color) +
    theme_prism(base_size = base_size) +
    theme(legend.position = "none")

  # Combine both plots into one plot
  combined_plot <- var_plot_obj +
    geom_density(data = sample_missing_df, aes(x = Missing_Percentage, fill = "Sample-wise"), alpha = alpha, color = NA) +
    labs(y = "Density", title = "Overall Missing Data Distribution") +
    scale_fill_manual(values = c(primary_color, secondary_color)) +
    guides(fill = guide_legend(title = NULL)) +
    theme_prism(base_size = base_size)

  # Save plots if specified
  if (save_plots) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    ggsave(filename = file.path(save_dir, "variable_missing_data_plot.png"), plot = var_plot_obj, width = plot_width, height = plot_height)
    ggsave(filename = file.path(save_dir, "sample_missing_data_plot.png"), plot = sample_plot_obj, width = plot_width, height = plot_height)
    ggsave(filename = file.path(save_dir, "combined_missing_data_plot.png"), plot = combined_plot, width = plot_width, height = plot_height)
  }

  # Return a summary of missing data information
  missing_info <- list(
    var_plot_obj = var_plot_obj,
    sample_plot_obj = sample_plot_obj,
    combined_plot = combined_plot,
    total_missing_values = sum(is.na(data)),
    total_variables = ncol(data),
    total_samples = nrow(data),
    variables_with_missing = sum(var_missing_percentage > 0),
    samples_with_missing = sum(sample_missing_percentage > 0),
    max_missing_variable = max(var_missing_percentage),
    min_missing_variable = min(var_missing_percentage),
    max_missing_sample = max(sample_missing_percentage),
    min_missing_sample = min(sample_missing_percentage)
  )

  # Print the combined plot
  print(combined_plot)

  return(missing_info)
}



#' Plot Missing Data for a Stat Object or Data Frame
#'
#' This function generates and saves missing data plots (variable-wise and sample-wise distributions) for
#' an object of class 'Stat' or a data frame. It also updates the 'Stat' object with missing data information
#' and returns the object or the missing data summary, depending on the input type.
#'
#' @param object An object of class 'Stat' or a data frame. If it is a 'Stat' object, the function uses the
#'               `raw.data` slot for missing data analysis. If it is a data frame, it directly performs the analysis.
#' @param palette_name A character string specifying the palette name for the plot colors. Default is `"lisa::BridgetRiley"`.
#' @param alpha A numeric value between 0 and 1 specifying the transparency level of the density plots. Default is `0.9`.
#' @param save_plots A logical value indicating whether to save the plots as PNG files. Default is `TRUE`.
#' @param save_dir A character string specifying the directory where the plots will be saved. Default is `"here('StatObject', 'missing_info')"` (within the `StatObject` folder).
#' @param plot_width A numeric value specifying the width of the saved plots. Default is `5`.
#' @param plot_height A numeric value specifying the height of the saved plots. Default is `5`.
#'
#' @returns If the input is a 'Stat' object, returns the updated 'Stat' object with the missing data information stored
#'          in the `processe.info` slot. If the input is a data frame, returns the missing data summary as a list.
#'
#' @export
#'
#' @examples
#' # Generate missing data plots for a Stat object
#' updated_stat <- state_plot_missing_data(stat_object, save_plots = TRUE)
#'
#' # Generate missing data plots for a data frame
#' missing_info <- state_plot_missing_data(data_frame, save_plots = FALSE)
state_plot_missing_data <- function(
    object,
    palette_name = "lisa::BridgetRiley",
    alpha = 0.9,
    save_plots = TRUE,
    save_dir = here("StatObject", "missing_info"),
    plot_width = 5,
    plot_height = 5) {

  # Check if the input is a 'Stat' object or a data frame
  if (inherits(object, "Stat")) {
    data <- slot(object, "raw.data")
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame.")
  }

  # Ensure that the data is valid
  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }

  # Plot missing data
  missing_info <- plot_missing_data(data,
                                    palette_name = palette_name,
                                    alpha = alpha,
                                    save_plots = save_plots,
                                    save_dir = save_dir,
                                    plot_width = plot_width,
                                    plot_height = plot_height)
  cat("Saved plots to: ", save_dir, "\n")

  # Print the missing data summary
  print(missing_info)

  # If input is a 'Stat' object, update the object and return it
  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")
    object@processe.info <- missing_info
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'processe.info' slot updated.\n")
    return(object)
  }

  # If input is a data frame, return the missing data summary
  return(missing_info)
}

