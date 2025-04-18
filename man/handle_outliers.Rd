% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{handle_outliers}
\alias{handle_outliers}
\title{Handle Outliers in the Data}
\usage{
handle_outliers(
  data,
  handle_method = c("replace", "remove", "keep", "capping"),
  lower_quantile = 0.05,
  upper_quantile = 0.95
)
}
\arguments{
\item{data}{A data frame containing columns with outlier flags (columns ending in "_outlier").}

\item{handle_method}{A character string specifying the method for handling outliers.
Options include "remove", "replace", "keep", and "capping" (default is "replace").}

\item{lower_quantile}{Numeric value representing the lower quantile for capping
outliers (default is 0.05).}

\item{upper_quantile}{Numeric value representing the upper quantile for capping
outliers (default is 0.95).}
}
\value{
A data frame with outliers handled according to the selected method.
}
\description{
This function allows the user to handle outliers in a data frame using various
methods, including removing, replacing, keeping, or capping outliers. It operates
on columns with names ending in "_outlier".
}
\examples{
# Example 1: Remove outliers
data <- data.frame(gene1 = c(1, 2, 3, 100, 5),
                   gene1_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE),
                   gene2 = c(2, 4, 6, 100, 8),
                   gene2_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE))
cleaned_data <- handle_outliers(data, handle_method = "remove")
# cleaned_data will have outliers removed.

# Example 2: Replace outliers with the median value
cleaned_data <- handle_outliers(data, handle_method = "replace")
# cleaned_data will have outliers replaced by the median values.

# Example 3: Keep outliers
cleaned_data <- handle_outliers(data, handle_method = "keep")
# cleaned_data will keep the outliers as they are.

# Example 4: Cap outliers based on quantiles
cleaned_data <- handle_outliers(data, handle_method = "capping",
                                lower_quantile = 0.1, upper_quantile = 0.9)
# cleaned_data will have outliers capped to the specified quantile range.
}
