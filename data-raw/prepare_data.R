# Data Preparation Script
# This script prepares the Vancouver crime data for the package

library(sf)

# Read the shapefile
vancouver_crime_2021 <- st_read(
  "inst/extdata/Vancouver_DAs_Crime_2021.shp",
  quiet = TRUE
)

# Save as .rda file for the package
usethis::use_data(vancouver_crime_2021, overwrite = TRUE)

# Done!
cat("vancouver_crime_2021 dataset created successfully!\n")
