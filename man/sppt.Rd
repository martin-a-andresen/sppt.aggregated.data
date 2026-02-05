\name{sppt}
\alias{sppt}
\title{Spatial Pattern Point Test for Aggregated Data}
\usage{
sppt(
  data,
  group_col = "group",
  count_col,
  B = 200,
  new_col = NULL,
  seed = NULL,
  conf_level = 0.95,
  check_overlap = FALSE,
  fix_base = FALSE,
  use_percentages = TRUE,
  create_maps = TRUE,
  export_maps = FALSE,
  export_dir = NULL,
  map_dpi = 300,
  export_results = FALSE,
  export_format = "shp",
  export_results_dir = NULL
)
}
\arguments{
\item{data}{A data frame or sf object containing the aggregated count data}

\item{group_col}{Character string specifying the column name for spatial group identifiers (default: "group")}

\item{count_col}{Character vector of column name(s) containing count data. If multiple columns provided, first is treated as base variable}

\item{B}{Integer specifying number of bootstrap samples (default: 200)}

\item{new_col}{Character vector of new column name(s) for output. If NULL, uses count_col names (default: NULL)}

\item{seed}{Integer for random seed to ensure reproducibility (default: NULL)}

\item{conf_level}{Numeric confidence level for intervals (default: 0.95)}

\item{check_overlap}{Logical indicating whether to check for interval overlap between variables (default: FALSE)}

\item{fix_base}{Logical indicating whether to fix the base (first) variable without bootstrapping (default: FALSE)}

\item{use_percentages}{Logical indicating whether to use percentages (TRUE) or counts (FALSE) (default: TRUE)}

\item{create_maps}{Logical indicating whether to create maps for bivariate case (default: TRUE)}

\item{export_maps}{Logical indicating whether to export generated maps (default: FALSE)}

\item{export_dir}{Character string specifying directory for map exports. If NULL, uses working directory (default: NULL)}

\item{map_dpi}{Integer specifying DPI for exported maps (default: 300)}

\item{export_results}{Logical indicating whether to export results (default: FALSE)}

\item{export_format}{Character string specifying export format: "shp", "csv", "txt", "rds", or "gpkg" (default: "shp")}

\item{export_results_dir}{Character string specifying directory for results export. If NULL, uses working directory (default: NULL)}
}
\value{
A data frame or sf object with added confidence interval columns and overlap statistics (if requested)
}
\description{
Performs bootstrap-based spatial pattern point tests on aggregated count data.
Compares spatial distributions between variables and calculates S-Index metrics
to quantify spatial pattern overlap.
}
\details{
The function performs bootstrap resampling to estimate confidence intervals for spatial
distributions of count data. When comparing two variables, it can calculate:
\itemize{
  \item S-Index: Proportion of observations with overlapping confidence intervals
  \item Robust S-Index: S-Index excluding observations where all variables are zero
  \item SIndex_Bivariate: -1 (base > test), 0 (overlap), 1 (test > base)
}
}
\examples{
\dontrun{
# Load spatial data
library(sf)
data <- st_read("your_shapefile.shp")

# Basic usage
result <- sppt(data, 
               group_col = "ID", 
               count_col = c("Population_2010", "Population_2020"))

# With overlap checking and map export
result <- sppt(data,
               group_col = "ID",
               count_col = c("Base", "Test"),
               check_overlap = TRUE,
               create_maps = TRUE,
               export_maps = TRUE,
               export_dir = "output/")

# Export results as CSV
result <- sppt(data,
               group_col = "ID",
               count_col = c("Base", "Test"),
               export_results = TRUE,
               export_format = "csv")
}
}
