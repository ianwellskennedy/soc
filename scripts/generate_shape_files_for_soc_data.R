# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "fredr", "openxlsx", "conflicted", "arcgisbinding", "sf")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
invisible(sapply(packages, install_if_missing))

# Load the packages
library(tidyverse)
library(openxlsx)
library(conflicted)
library(arcgisbinding)
library(sf)

# Prefer the dplyr package for a few functions
conflicts_prefer(dplyr::filter, dplyr::lag)

rm(install_if_missing, packages)

# File paths ----

# Update the year_of_analysis according to the input file path you have chosen
year_of_analysis <- 2024

input_file_path <- paste0("data-output/soc_data_summarized_by_census_division_", year_of_analysis, ".xlsx")

census_division_shape_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/shapefiles/2023/Census Divisions/cb_2023_us_division_20m.shp"

# Read in data ----

share_by_primary_ext_wall_material <- read.xlsx(input_file_path, sheet = 'Prop by prim. ext. mat.')
share_by_secondary_ext_wall_material <- read.xlsx(input_file_path, sheet = 'Prop by sec. ext. mat.')


census_division_shape_file <- st_read(census_division_shape_file_path)
census_division_shape_file <- census_division_shape_file %>%
  select(NAME, geometry) %>%
  rename(DIV = NAME)

# Join data to shape file ----

recode_census_division <- function(data) {
  data <- data %>%
    mutate(DIV = case_when(
      DIV == 'east_north_central' ~ 'East North Central',
      DIV == 'east_south_central' ~ 'East South Central',
      DIV == 'mid_atlantic' ~ 'Middle Atlantic',
      DIV == 'mountain' ~ 'Mountain',
      DIV == 'new_england' ~ 'New England',
      DIV == 'pacific' ~ 'Pacific',
      DIV == 'south_atlantic' ~ 'South Atlantic',
      DIV == 'west_north_central' ~ 'West North Central',
      DIV == 'west_south_central' ~ 'West South Central',
      T ~ DIV
    )) 
}

prep_spatial_file <- function(data) {
  
  data <- data %>%
    mutate(across(.cols = -DIV, ~if_else(is.na(.), 0, .))) %>%
    left_join(census_division_shape_file, by = 'DIV') %>%
    st_as_sf()
  
}

share_by_primary_ext_wall_material <- recode_census_division(share_by_primary_ext_wall_material) 
share_by_secondary_ext_wall_material <- recode_census_division(share_by_secondary_ext_wall_material)

share_by_primary_ext_wall_material <- share_by_primary_ext_wall_material %>%
  pivot_wider(names_from = WAL1, values_from = prop, id_cols = DIV) %>%
  prep_spatial_file()

share_by_secondary_ext_wall_material <- share_by_secondary_ext_wall_material %>%
  pivot_wider(names_from = WAL2, values_from = prop, id_cols = DIV) %>%
  prep_spatial_file()


# Output files ----

arc.check_product()

arc.write(share_by_primary_ext_wall_material , path = "shapefiles/share_by_primary_ext_wall_material.shp", overwrite = T, validate = T)
arc.write(share_by_secondary_ext_wall_material , path = "shapefiles/share_by_secondary_ext_wall_material.shp", overwrite = T, validate = T)
