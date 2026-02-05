#' Spatial Pattern Point Test for Aggregated Data
#'
#' Performs bootstrap-based spatial pattern point tests on aggregated count data.
#' Compares spatial distributions between variables and calculates S-Index metrics
#' to quantify spatial pattern overlap.
#'
#' @param data A data frame or sf object containing the aggregated count data
#' @param group_col Character string specifying the column name for spatial group identifiers (default: "group")
#' @param count_col Character vector of column name(s) containing count data. If multiple columns provided, first is treated as base variable
#' @param B Integer specifying number of bootstrap samples (default: 200)
#' @param new_col Character vector of new column name(s) for output. If NULL, uses count_col names (default: NULL)
#' @param seed Integer for random seed to ensure reproducibility (default: NULL)
#' @param conf_level Numeric confidence level for intervals (default: 0.95)
#' @param check_overlap Logical indicating whether to check for interval overlap between variables (default: FALSE)
#' @param fix_base Logical indicating whether to fix the base (first) variable without bootstrapping (default: FALSE)
#' @param use_percentages Logical indicating whether to use percentages (TRUE) or counts (FALSE) (default: TRUE)
#' @param create_maps Logical indicating whether to create maps for bivariate case (default: TRUE)
#' @param export_maps Logical indicating whether to export generated maps (default: FALSE)
#' @param export_dir Character string specifying directory for map exports. If NULL, uses working directory (default: NULL)
#' @param map_dpi Integer specifying DPI for exported maps (default: 300)
#' @param export_results Logical indicating whether to export results (default: FALSE)
#' @param export_format Character string specifying export format: "shp", "csv", "txt", "rds", or "gpkg" (default: "shp")
#' @param export_results_dir Character string specifying directory for results export. If NULL, uses working directory (default: NULL)
#'
#' @return A data frame or sf object with added confidence interval columns and overlap statistics (if requested)
#'
#' @details
#' The function performs bootstrap resampling to estimate confidence intervals for spatial
#' distributions of count data. When comparing two variables, it can calculate:
#' \itemize{
#'   \item S-Index: Proportion of observations with overlapping confidence intervals
#'   \item Robust S-Index: S-Index excluding observations where all variables are zero
#'   \item SIndex_Bivariate: -1 (base > test), 0 (overlap), 1 (test > base)
#' }
#'
#' @examples
#' \dontrun{
#' # Load spatial data
#' library(sf)
#' data <- st_read("your_shapefile.shp")
#'
#' # Basic usage
#' result <- sppt(data, 
#'                group_col = "ID", 
#'                count_col = c("Population_2010", "Population_2020"))
#'
#' # With overlap checking and map export
#' result <- sppt(data,
#'                group_col = "ID",
#'                count_col = c("Base", "Test"),
#'                check_overlap = TRUE,
#'                create_maps = TRUE,
#'                export_maps = TRUE,
#'                export_dir = "output/")
#'
#' # Export results as CSV
#' result <- sppt(data,
#'                group_col = "ID",
#'                count_col = c("Base", "Test"),
#'                export_results = TRUE,
#'                export_format = "csv")
#' }
#'
#' @export
#' @importFrom Matrix sparseMatrix
#' @importFrom dplyr left_join mutate rowwise select ungroup
#' @importFrom tidyr replace_na uncount
#' @importFrom rlang sym
#' @importFrom sf st_drop_geometry st_write
#' @importFrom stats quantile rmultinom setNames
sppt <- function(data,
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
                 export_results_dir = NULL) {
  
  # Check required packages
  required <- c("Matrix", "dplyr", "tidyr", "rlang")
  missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Required packages missing: ", paste(missing, collapse = ", "))
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # Handle multiple count columns
  if (length(count_col) > 1) {
    # If new_col not specified, use count_col names
    if (is.null(new_col)) {
      new_col <- count_col
    } else if (length(new_col) != length(count_col)) {
      stop("new_col must have the same length as count_col when both are vectors")
    }
    
    # Process each count column independently
    result <- data
    for (i in seq_along(count_col)) {
      # Skip bootstrapping for Base variable if fix_base is TRUE
      if (fix_base && i == 1) {
        # For Base variable, calculate percentage or keep counts
        if (use_percentages) {
          total_count <- sum(result[[count_col[i]]], na.rm = TRUE)
          if (total_count > 0) {
            result[[paste0(new_col[i], "_L")]] <- (result[[count_col[i]]] / total_count) * 100
            result[[paste0(new_col[i], "_U")]] <- (result[[count_col[i]]] / total_count) * 100
          } else {
            result[[paste0(new_col[i], "_L")]] <- 0
            result[[paste0(new_col[i], "_U")]] <- 0
          }
        } else {
          result[[paste0(new_col[i], "_L")]] <- result[[count_col[i]]]
          result[[paste0(new_col[i], "_U")]] <- result[[count_col[i]]]
        }
      } else {
        # Bootstrap as normal for Test variable (or all variables if fix_base = FALSE)
        result <- sppt(
          data = result,
          group_col = group_col,
          count_col = count_col[i],
          B = B,
          new_col = new_col[i],
          seed = if (!is.null(seed)) seed + i - 1 else NULL,
          conf_level = conf_level,
          check_overlap = FALSE,
          fix_base = FALSE,
          use_percentages = use_percentages,
          create_maps = FALSE,
          export_maps = FALSE
        )
      }
    }
    
    # Check for overlap after all variables are processed
    if (check_overlap && length(count_col) > 1) {
      # Define column names outside of rowwise
      lower_cols <- paste0(new_col, "_L")
      upper_cols <- paste0(new_col, "_U")
      
      if (fix_base) {
        # When fix_base is TRUE, check if Base value falls within Test interval
        base_lower <- paste0(new_col[1], "_L")
        base_upper <- paste0(new_col[1], "_U")
        test_lower <- paste0(new_col[2], "_L")
        test_upper <- paste0(new_col[2], "_U")
        
        result <- result |>
          dplyr::mutate(
            intervals_overlap = as.integer(
              .data[[base_lower]] >= .data[[test_lower]] & 
                .data[[base_upper]] <= .data[[test_upper]]
            )
          )
      } else {
        # Original overlap calculation
        result <- result |>
          dplyr::rowwise() |>
          dplyr::mutate(
            intervals_overlap = {
              # Get all lower and upper bounds
              lowers <- dplyr::c_across(dplyr::all_of(lower_cols))
              uppers <- dplyr::c_across(dplyr::all_of(upper_cols))
              
              # Check if all intervals overlap
              max_lower <- max(lowers, na.rm = TRUE)
              min_upper <- min(uppers, na.rm = TRUE)
              
              as.integer(max_lower <= min_upper)
            }
          ) |>
          dplyr::ungroup()
      }
      
      # Add SIndex_Bivariate for exactly two variables
      if (length(count_col) == 2) {
        base_var <- count_col[1]
        test_var <- count_col[2]
        
        result <- result |>
          dplyr::mutate(
            SIndex_Bivariate = dplyr::case_when(
              intervals_overlap == 1 ~ 0,
              .data[[test_var]] > .data[[base_var]] ~ 1,
              .data[[test_var]] < .data[[base_var]] ~ -1,
              TRUE ~ 0
            )
          )
      }
      
      # Calculate S-Index and Robust S-Index
      total_obs <- nrow(result)
      sum_overlap <- sum(result$intervals_overlap, na.rm = TRUE)
      
      # S-Index: proportion of all observations with overlapping intervals
      s_index <- sum_overlap / total_obs
      
      # Robust S-Index: proportion excluding observations where all count_col variables are zero
      count_data <- result |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(count_col))
      
      nonzero_mask <- count_data |>
        dplyr::rowwise() |>
        dplyr::mutate(has_nonzero = sum(dplyr::c_across(dplyr::everything()) > 0, na.rm = TRUE) > 0) |>
        dplyr::pull(has_nonzero)
      
      nonzero_obs <- sum(nonzero_mask)
      sum_overlap_nonzero <- sum(result$intervals_overlap[nonzero_mask], na.rm = TRUE)
      
      robust_s_index <- if (nonzero_obs > 0) {
        sum_overlap_nonzero / nonzero_obs
      } else {
        NA_real_
      }
      
      # Print the statistics
      cat("\n========================================\n")
      cat("Spatial Pattern Overlap Statistics\n")
      if (fix_base) {
        cat("Mode: Fixed Base (Test randomized)\n")
      }
      if (use_percentages) {
        cat("Using: Percentages (spatial distribution)\n")
      } else {
        cat("Using: Counts (absolute values)\n")
      }
      cat("========================================\n")
      cat("S-Index:          ", round(s_index, 4), "\n")
      cat("Robust S-Index:   ", round(robust_s_index, 4), "\n")
      cat("----------------------------------------\n")
      cat("Total observations:                ", total_obs, "\n")
      cat("Observations with overlap:         ", sum_overlap, "\n")
      cat("Observations with non-zero counts: ", nonzero_obs, "\n")
      cat("========================================\n\n")
      
      # Store statistics as attributes
      attr(result, "s_index") <- s_index
      attr(result, "robust_s_index") <- robust_s_index
      attr(result, "fix_base") <- fix_base
      attr(result, "use_percentages") <- use_percentages
      
      # Create map for bivariate case if requested and data is sf object
      if (create_maps && inherits(result, "sf") && length(count_col) == 2) {
        cat("Creating map...\n")
        
        tryCatch({
          # Determine export directory
          if (export_maps) {
            if (is.null(export_dir)) {
              export_dir <- getwd()
              cat("Exporting map to current working directory:", export_dir, "\n")
            } else {
              if (!dir.exists(export_dir)) {
                dir.create(export_dir, recursive = TRUE)
                cat("Created export directory:", export_dir, "\n")
              }
            }
          }
          
          # Calculate dimensions for specified DPI
          width_px <- 8 * map_dpi
          height_px <- 6 * map_dpi
          
          # Reset plot layout
          par(mfrow = c(1, 1))
          
          # Get variable names for legend
          base_name <- count_col[1]
          test_name <- count_col[2]
          
          # Create map
          if (export_maps) {
            png(filename = file.path(export_dir, "map_bivariate_s_index.png"),
                width = width_px, height = height_px, res = map_dpi)
          }
          
          plot(result["SIndex_Bivariate"], 
               pal = c("gray80", "white", "black"),
               breaks = c(-1.5, -0.5, 0.5, 1.5),
               main = "S-Index Bivariate",
               key.pos = NULL,
               reset = FALSE)
          
          legend("topleft",
                 legend = c(paste0(base_name, " > ", test_name), 
                            "Insignificant change", 
                            paste0(test_name, " > ", base_name)),
                 fill = c("gray80", "white", "black"),
                 border = "black",
                 title = "Spatial Pattern",
                 cex = 0.8)
          
          if (export_maps) {
            dev.off()
            cat("Map exported successfully to:", export_dir, "\n")
            cat("  - map_bivariate_s_index.png\n\n")
          } else {
            cat("Map created successfully.\n\n")
          }
        }, error = function(e) {
          if (export_maps && dev.cur() > 1) dev.off()
          cat("Note: Could not create/export map. Error:", e$message, "\n\n")
        })
      }
    }
    
    # Export results if requested
    if (export_results && length(count_col) > 1) {
      # Determine export directory for results
      if (is.null(export_results_dir)) {
        export_results_dir <- getwd()
        cat("Exporting results to current working directory:", export_results_dir, "\n")
      } else {
        if (!dir.exists(export_results_dir)) {
          dir.create(export_results_dir, recursive = TRUE)
          cat("Created export directory:", export_results_dir, "\n")
        }
      }
      
      # Create filename from count_col variables
      var_names <- paste(count_col, collapse = "_")
      base_filename <- paste0("sppt_output_", var_names)
      
      tryCatch({
        if (tolower(export_format) == "shp") {
          # Export as shapefile (requires sf object)
          if (inherits(result, "sf")) {
            filepath <- file.path(export_results_dir, paste0(base_filename, ".shp"))
            sf::st_write(result, filepath, delete_dsn = TRUE, quiet = TRUE)
            cat("Results exported as shapefile:", filepath, "\n")
          } else {
            warning("Cannot export as shapefile: data is not an sf object")
          }
        } else if (tolower(export_format) %in% c("csv", "txt")) {
          # Export as CSV/TXT
          result_export <- if (inherits(result, "sf")) {
            sf::st_drop_geometry(result)
          } else {
            result
          }
          
          filepath <- file.path(export_results_dir, paste0(base_filename, ".", tolower(export_format)))
          write.csv(result_export, filepath, row.names = FALSE)
          cat("Results exported as", toupper(export_format), ":", filepath, "\n")
        } else if (tolower(export_format) == "rds") {
          # Export as RDS (preserves all R object attributes including sf geometry)
          filepath <- file.path(export_results_dir, paste0(base_filename, ".rds"))
          saveRDS(result, filepath)
          cat("Results exported as RDS:", filepath, "\n")
        } else if (tolower(export_format) == "gpkg") {
          # Export as GeoPackage (alternative to shapefile, better for large data)
          if (inherits(result, "sf")) {
            filepath <- file.path(export_results_dir, paste0(base_filename, ".gpkg"))
            sf::st_write(result, filepath, delete_dsn = TRUE, quiet = TRUE)
            cat("Results exported as GeoPackage:", filepath, "\n")
          } else {
            warning("Cannot export as GeoPackage: data is not an sf object")
          }
        } else {
          warning("Unsupported export format: ", export_format, 
                  ". Supported formats: shp, csv, txt, rds, gpkg")
        }
      }, error = function(e) {
        warning("Failed to export results: ", e$message)
      })
    }
    
    return(result)
  }
  
  # Single column processing
  if (is.null(new_col)) {
    new_col <- count_col
  }
  
  # Store original group column type
  original_group_type <- class(data[[group_col]])[1]
  
  # Expand counts
  event <- data |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(c(group_col, count_col))) |>
    tidyr::uncount(!!rlang::sym(count_col))
  
  n <- nrow(event)
  if (n == 0) {
    data[[paste0(new_col, "_L")]] <- 0
    data[[paste0(new_col, "_U")]] <- 0
    return(data)
  }
  
  # Progress indicator for large datasets
  if (B > 100 && n > 1000) {
    message("Running ", B, " bootstrap samples on ", n, " events for ", count_col, "...")
  }
  
  # Convert to numeric factor for faster operations
  event[[group_col]] <- as.factor(event[[group_col]])
  groups <- levels(event[[group_col]])
  G <- length(groups)
  g_idx <- as.integer(event[[group_col]])
  
  # Sparse matrix bootstrap
  onehot <- Matrix::sparseMatrix(i = seq_len(n), j = g_idx, x = 1, dims = c(n, G))
  W <- stats::rmultinom(B, size = n, prob = rep(1 / n, n))
  group_counts <- as.matrix(Matrix::t(onehot) %*% W)
  
  # Convert to percentages if requested
  if (use_percentages) {
    group_values <- apply(group_counts, 2, function(col) {
      col_sum <- sum(col)
      if (col_sum > 0) {
        (col / col_sum) * 100
      } else {
        rep(0, length(col))
      }
    })
  } else {
    group_values <- group_counts
  }
  
  # Calculate quantiles
  alpha <- 1 - conf_level
  lower_prob <- alpha / 2
  upper_prob <- 1 - alpha / 2
  
  lower_values <- apply(group_values, 1, stats::quantile, probs = lower_prob)
  upper_values <- apply(group_values, 1, stats::quantile, probs = upper_prob)
  
  df_stat <- data.frame(
    group = groups,
    lower = lower_values,
    upper = upper_values,
    stringsAsFactors = FALSE
  )
  names(df_stat) <- c("group", paste0(new_col, "_L"), paste0(new_col, "_U"))
  
  # Convert group column back to original type
  if (original_group_type %in% c("numeric", "integer", "double")) {
    df_stat$group <- as.numeric(df_stat$group)
  }
  
  dplyr::left_join(data, df_stat, by = stats::setNames("group", group_col)) |>
    dplyr::mutate(
      !!paste0(new_col, "_L") := tidyr::replace_na(.data[[paste0(new_col, "_L")]], 0),
      !!paste0(new_col, "_U") := tidyr::replace_na(.data[[paste0(new_col, "_U")]], 0)
    )
}
