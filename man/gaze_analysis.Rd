% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{gaze_analysis}
\alias{gaze_analysis}
\title{Gaze Analysis for Group Comparison}
\usage{
gaze_analysis(
  data,
  formula = NULL,
  group_cols = NULL,
  digits = 1,
  show.p = TRUE,
  gaze_method = 3,
  save_word = TRUE,
  save_dir = here("PrognosiX", "gaze_baseline")
)
}
\arguments{
\item{data}{A data frame containing the data to analyze.}

\item{formula}{A formula specifying the model to fit. If NULL, the formula is automatically created
based on the provided group columns (default is NULL).}

\item{group_cols}{A vector of column names representing the grouping variables (default is NULL).}

\item{digits}{The number of digits to display for the result (default is 1).}

\item{show.p}{A logical value indicating whether to display p-values in the output (default is TRUE).}

\item{gaze_method}{An integer between 1 and 5 representing the gaze analysis method to use (default is 3).}

\item{save_word}{A logical value indicating whether to save the results to a Word document (default is TRUE).}

\item{save_dir}{The directory to save the Word document (default is the "PrognosiX/gaze_baseline" folder).}
}
\value{
A data frame or matrix with the gaze analysis results. If \code{save_word} is TRUE, the results
are also saved as a Word document in the specified directory.
}
\description{
This function performs gaze analysis for group comparison using the specified formula,
method, and settings. It provides a detailed result, optionally saving it as a Word document.
The gaze analysis can be performed based on group columns in the data, and various settings
are available to customize the output.
}
\examples{
# Example 1: Performing gaze analysis with a formula and custom settings
result <- gaze_analysis(data = my_data,
                        formula = ~ group + age + gender,
                        digits = 2,
                        show.p = TRUE,
                        gaze_method = 3,
                        save_word = TRUE,
                        save_dir = "path/to/save")

# Example 2: Using the default formula based on group columns
result <- gaze_analysis(data = my_data,
                        group_cols = c("group"),
                        digits = 1,
                        show.p = FALSE,
                        gaze_method = 1)
}
