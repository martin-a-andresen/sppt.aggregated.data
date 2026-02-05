# Quick Reference Examples

## Installation

```r
# Install from GitHub
install.packages("devtools")
devtools::install_github("yourusername/sppt.aggregated.data")

# Load package
library(sppt.aggregated.data)
library(sf)
```

## Example 1: Basic Usage

```r
# Load your data
data <- st_read("your_data.shp")

# Run basic analysis
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Variable1", "Variable2"),
  B = 200,
  seed = 123
)

# View results
head(result)
```

## Example 2: With Overlap Statistics

```r
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Base", "Test"),
  B = 200,
  check_overlap = TRUE,
  seed = 123
)

# Access S-Index values
s_index <- attr(result, "s_index")
robust_s_index <- attr(result, "robust_s_index")

cat("S-Index:", s_index, "\n")
cat("Robust S-Index:", robust_s_index, "\n")
```

## Example 3: Export Results as CSV

```r
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Base", "Test"),
  B = 200,
  check_overlap = TRUE,
  export_results = TRUE,
  export_format = "csv",
  export_results_dir = "output/",
  seed = 123
)
```

## Example 4: Create and Export Maps

```r
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Base", "Test"),
  B = 200,
  check_overlap = TRUE,
  create_maps = TRUE,
  export_maps = TRUE,
  export_dir = "maps/",
  map_dpi = 600,
  seed = 123
)
```

## Example 5: Use Counts Instead of Percentages

```r
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Base", "Test"),
  B = 200,
  use_percentages = FALSE,  # Use absolute counts
  check_overlap = TRUE,
  seed = 123
)
```

## Example 6: Fixed Base Variable

```r
# Fix the base variable, only bootstrap the test variable
result <- sppt(
  data = data,
  group_col = "ID",
  count_col = c("Base", "Test"),
  B = 200,
  fix_base = TRUE,  # Don't bootstrap Base variable
  check_overlap = TRUE,
  seed = 123
)
```

## Example 7: Complete Analysis with All Exports

```r
result <- sppt(
  data = data,
  group_col = "DAUID",
  count_col = c("TFV", "TOV"),
  B = 200,
  conf_level = 0.95,
  check_overlap = TRUE,
  fix_base = FALSE,
  use_percentages = TRUE,
  create_maps = TRUE,
  export_maps = TRUE,
  export_dir = "output/maps/",
  map_dpi = 600,
  export_results = TRUE,
  export_format = "gpkg",
  export_results_dir = "output/data/",
  seed = 171717
)

# The output includes:
# - Confidence intervals for each variable
# - intervals_overlap column (0 or 1)
# - SIndex_Bivariate column (-1, 0, or 1)
# - Printed S-Index statistics
# - Map showing spatial patterns (if exported)
# - GeoPackage file with all results
```

## Understanding the Output

### Column Naming
- `{variable}_L`: Lower bound of confidence interval
- `{variable}_U`: Upper bound of confidence interval
- `intervals_overlap`: 1 if intervals overlap, 0 otherwise
- `SIndex_Bivariate`: -1 (Base > Test), 0 (overlap), 1 (Test > Base)

### Interpreting S-Index
- **S-Index = 1.0**: Perfect overlap (no spatial difference)
- **S-Index = 0.0**: No overlap (complete spatial difference)
- **Robust S-Index**: Same as S-Index but excludes zero-count areas

### Map Colors (Bivariate)
- **Gray**: Base variable greater than Test variable
- **White**: No significant difference (intervals overlap)
- **Black**: Test variable greater than Base variable

## Tips for Better Results

1. **Choose appropriate B**: 
   - Small datasets: B = 100-200
   - Large datasets: B = 500-1000

2. **Set a seed**: Always use `seed` parameter for reproducibility

3. **Export formats**:
   - Use "gpkg" for large spatial datasets (better than shapefiles)
   - Use "rds" to preserve all attributes and structure
   - Use "csv" for non-spatial analysis in other software

4. **Percentages vs Counts**:
   - Use percentages to compare spatial distributions
   - Use counts when absolute numbers matter

5. **Memory considerations**: For very large datasets, consider:
   - Reducing B
   - Processing subsets separately
   - Using "csv" export to save memory

## Troubleshooting

### "Package not found" error
```r
# Make sure devtools is installed
install.packages("devtools")

# Try again
devtools::install_github("yourusername/sppt.aggregated.data")
```

### "Could not find function" error
```r
# Make sure package is loaded
library(sppt.aggregated.data)

# Check that function exists
?sppt
```

### Large dataset taking too long
```r
# Reduce bootstrap samples
result <- sppt(data, count_col = c("A", "B"), B = 100)  # Instead of 200

# Or process a sample first
sample_data <- data[sample(nrow(data), 100), ]
result <- sppt(sample_data, count_col = c("A", "B"))
```

## Getting Help

```r
# View function documentation
?sppt

# View package help
help(package = "sppt.aggregated.data")

# Report issues
# Visit: https://github.com/yourusername/sppt.aggregated.data/issues
```
