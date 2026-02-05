# sppt.aggregated.data

<!-- badges: start -->
<!-- badges: end -->

The `sppt.aggregated.data` package performs spatial pattern point tests on aggregated count data using bootstrap resampling. It compares spatial distributions between variables and calculates S-Index metrics to quantify spatial pattern overlap.

## Installation

You can install the development version of sppt.aggregated.data from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("martin-a-andresen/sppt.aggregated.data", build_vignettes = TRUE)
```

After installation, view the comprehensive tutorial vignette:

``` r
vignette("introduction", package = "sppt.aggregated.data")
```

## Key Features

- **Bootstrap resampling**: Generate confidence intervals for spatial distributions
- **S-Index metrics**: Quantify spatial pattern overlap between variables
- **Flexible output**: Export results as shapefiles, CSV, RDS, or GeoPackage
- **Visualization**: Create and export maps showing spatial patterns
- **Multiple comparison modes**: Compare percentages or absolute counts

## Basic Usage

```r
library(sppt.aggregated.data)
library(sf)

# Load your spatial data
data <- st_read("your_shapefile.shp")

# Basic analysis
result <- sppt(data, 
               group_col = "DAUID",
               count_col = c("Base", "Test"),
               B = 200,
               seed = 123)

# Analysis with overlap statistics
result <- sppt(data,
               group_col = "DAUID",
               count_col = c("Base", "Test"),
               B = 200,
               check_overlap = TRUE,
               seed = 123)

# Export results and maps
result <- sppt(data,
               group_col = "DAUID",
               count_col = c("TFV", "TOV"),
               B = 200,
               check_overlap = TRUE,
               create_maps = TRUE,
               export_maps = TRUE,
               export_dir = "output/",
               export_results = TRUE,
               export_format = "shp",
               seed = 123)
```

## Parameters

### Core Parameters
- `data`: Data frame or sf object with aggregated count data
- `group_col`: Column name for spatial group identifiers
- `count_col`: Column name(s) for count data (vector for multiple variables)
- `B`: Number of bootstrap samples (default: 200)
- `seed`: Random seed for reproducibility

### Analysis Options
- `use_percentages`: Use percentages (TRUE) or counts (FALSE) (default: TRUE)
- `fix_base`: Fix first variable without bootstrapping (default: FALSE)
- `check_overlap`: Calculate overlap statistics (default: FALSE)
- `conf_level`: Confidence level for intervals (default: 0.95)

### Visualization Options
- `create_maps`: Create maps for bivariate case (default: TRUE)
- `export_maps`: Export generated maps (default: FALSE)
- `export_dir`: Directory for map exports
- `map_dpi`: DPI for exported maps (default: 300)

### Export Options
- `export_results`: Export results to file (default: FALSE)
- `export_format`: Format for export: "shp", "csv", "txt", "rds", "gpkg" (default: "shp")
- `export_results_dir`: Directory for results export

## Output

The function returns the input data with added columns:
- `{variable}_L`: Lower confidence bound
- `{variable}_U`: Upper confidence bound
- `intervals_overlap`: Binary indicator of overlap (if `check_overlap = TRUE`)
- `SIndex_Bivariate`: Spatial pattern comparison (-1, 0, 1) (if two variables)

When `check_overlap = TRUE`, the function also prints:
- **S-Index**: Proportion of observations with overlapping intervals
- **Robust S-Index**: S-Index excluding zero-count observations

## Example: Crime Data Analysis

```r
library(sppt.aggregated.data)
library(sf)

# Load Vancouver crime data
data <- st_read("Vancouver_2021_DAs.shp")

# Compare theft from vehicle (TFV) vs theft of vehicle (TOV)
result <- sppt(
  data = data,
  group_col = "DAUID",
  count_col = c("TFV", "TOV"),
  B = 200,
  conf_level = 0.95,
  check_overlap = TRUE,
  create_maps = TRUE,
  export_maps = TRUE,
  export_dir = "output/",
  map_dpi = 600,
  export_results = TRUE,
  export_format = "gpkg",
  seed = 171717,
  use_percentages = TRUE,
  fix_base = FALSE
)

# View S-Index statistics
cat("S-Index:", attr(result, "s_index"), "\n")
cat("Robust S-Index:", attr(result, "robust_s_index"), "\n")

# Map shows:
# - Gray: TFV > TOV
# - White: No significant difference
# - Black: TOV > TFV
```

## Citation

If you use this package in your research, please cite:

```
Andresen, M.A. (2026). sppt.aggregated.data: Spatial Point Pattern Test for Aggregated Data. R package version (0.1.0). URL: https://github.com/martin-a-andresen/sppt.aggregated.data
```

## License

MIT License - see LICENSE file for details

## Contact

- GitHub: [martin-a-andresen](https://github.com/martin-a-andresen)
- Issues: [Report a bug](https://github.com/martin-a-andresen/sppt.aggregated.data/issues)
