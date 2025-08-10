# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "fredr", "openxlsx", "lubridate", "xts", "httr", "readxl", "conflicted")

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
library(fredr)
library(openxlsx)
library(lubridate)
library(xts)
library(httr)
library(readxl)
library(conflicted)

# Prefer the dplyr package for a few functions
conflicts_prefer(dplyr::filter, dplyr::lag, dpylr::select)

rm(install_if_missing, packages)

# File paths ----

# Set the FRED API Key
fredr_set_key(key = 'cbebc48b543b6420b4aa3ff9bd7a9878')

# Import Census Data 
soc_data_folder <- "data/"

output_file_path <- "data-output/soc_starts_by_price_point_2010_to_2024.xlsx"

# Read in CPI data ----

cpi_data <- fredr(series_id = 'CPIAUCNS', observation_start = as.Date('2010-01-01'), observation_end = Sys.Date(), frequency = 'm')

cpi_data <- cpi_data %>%
  rename(cpi = value) %>%
  select(date, cpi)

# Get current CPI
may_2025_cpi <- cpi_data %>%
  filter(date == '2025-05-01') %>%
  select(cpi)

may_2025_cpi <- may_2025_cpi$cpi

cpi_data <- cpi_data %>%
  mutate(adjustment_rate_to_may_2025_dollars = may_2025_cpi / cpi)

# Set the variables for data aggregation ----

# List of required variables
variables_post_2010 <- c("STRT", "SLPR", "WEIGHT", "DIV", "SALE", "CAT", "COMP",
                       "CONPR", "FCONPR", "FSLPR", "PVALU", "AUTH", "DET", "LOTV", "ID")

# Import 2010 through 2023 data ----

soc_2010 <- read_excel(paste0(soc_data_folder, "soc10.xls"))

soc_2010_to_2024 <- soc_2010 %>%
  select(any_of(variables_post_2010)) %>%
  mutate(YEAR = 2010)

# Not every year has an ID column, create pseudo-ID
soc_2010_to_2024$ID <- paste("PID", sample(900000:10000000, nrow(soc_2010_to_2024), replace = TRUE))

for (i in 11:23){
  temp_data   <- read_excel(paste0(soc_data_folder, "soc", i, ".xls"))
  temp_data   <- temp_data %>%
    select(any_of(variables_post_2010)) %>%
    mutate(YEAR = as.numeric(paste0(20, i)))
  
  # Not every year has an ID column, create pseudo-ID if needed
  if (!("ID" %in% colnames(temp_data))){
    temp_data$ID <- paste("PID", sample(900000:10000000, nrow(temp_data), replace = TRUE))
  }
  
  soc_2010_to_2024 <- rbind(soc_2010_to_2024, temp_data)
}

# Import 2024 data ----

soc_2024 <- read.xlsx(paste0(soc_data_folder, "soc24.xlsx"))

soc_2024 <- soc_2024 %>%
  select(any_of(variables_post_2010)) %>%
  mutate(YEAR = 2024)

soc_2024$ID <- paste("PID", sample(900000:10000000, nrow(soc_2024), replace = TRUE))

soc_2010_to_2024 <- rbind(soc_2010_to_2024, soc_2024)

# Clean SOC data ----

# Create new "value" variable which is final sales price < final contract price < sales price < contract price < permit value
soc_2010_to_2024$value <- NA

# 2010 and Post
soc_2010_to_2024 <- soc_2010_to_2024 %>%
  mutate(value = case_when(
    FSLPR > 0 ~ FSLPR,
    SLPR > 0 ~ SLPR,
    FCONPR > 0 ~ FCONPR,
    CONPR > 0 ~ CONPR,
    LOTV > 0 ~ LOTV # Not including permit value any more
  ))

soc_2010_to_2024 <- soc_2010_to_2024 %>%
  select(DET, AUTH, CAT, STRT, DIV, WEIGHT, value, YEAR)

# Data with only relevant variables
soc_2010_to_2024_cleaned <- soc_2010_to_2024

# Rename variables
soc_2010_to_2024_cleaned <- soc_2010_to_2024_cleaned %>%
  rename(detached = DET,
         authorization_date = AUTH,
         category = CAT,
         start_date = STRT,
         census_division = DIV,
         weight = WEIGHT)

# Clean date
soc_2010_to_2024_cleaned <- soc_2010_to_2024_cleaned %>%
  mutate(start_date = as.Date(paste0(start_date, "01"), format = "%Y%m%d")) %>%
  mutate(authorization_date = as.Date(paste0(authorization_date, "01"), format = "%Y%m%d"))

# If start date is missing, assume it is one month later than the authorization date
soc_2010_to_2024_cleaned <- soc_2010_to_2024_cleaned %>%
  mutate(start_date = ifelse(is.na(start_date), authorization_date %m+% months(1), start_date)) %>%
  mutate(start_date = as.Date(start_date))

# Remove rows with missing values
soc_2010_to_2024_cleaned <- soc_2010_to_2024_cleaned %>%
  filter(!is.na(value)) %>%
  filter(!is.na(start_date))

# Clean Census division
soc_2010_to_2024_cleaned <- soc_2010_to_2024_cleaned %>%
  mutate(
    census_division = as.numeric(census_division),
    category = as.numeric(category),
    census_division = case_when(
      census_division == '1' ~ 'New England',
      census_division == '2' ~ 'Mid Atlantic',
      census_division == '3' ~ 'East North Central',
      census_division == '4' ~ 'West North Central',
      census_division == '5' ~ 'South Atlantic',
      census_division == '6' ~ 'East South Central',
      census_division == '7' ~ 'West South Central',
      census_division == '8' ~ 'Mountain',
      census_division == '9' ~ 'Pacific'),
    category = case_when(
      category == 1 ~ 'Built for Sale',
      category == 2 ~ 'Contractor-Built',
      category == 3 ~ 'Owner-Built',
      category == 4 ~ 'Built for Rent')
  ) 

# Adjust SOC data by the CPI ----

# Adjust by CPI
soc_2010_to_2024_real <- soc_2010_to_2024_cleaned %>%
  left_join(cpi_data, by = c("start_date" = "date")) %>%
  mutate(value_may_2025_usd = value * adjustment_rate_to_may_2025_dollars) %>%
  select(-c(cpi, adjustment_rate_to_may_2025_dollars)) 

# Create over $2MM flag
soc_2010_to_2024_real <- soc_2010_to_2024_real %>%
  mutate(
    over_1.25MM = ifelse(value_may_2025_usd >= 1250000, 1, 0),
    one_to_1.25_million = ifelse(value_may_2025_usd >= 1000000 & value_may_2025_usd < 1250000, 1, 0),
    over_750K = ifelse(value_may_2025_usd >= 750000 & value_may_2025_usd < 1000000, 1, 0),
    over_500K = ifelse(value_may_2025_usd >= 500000 & value_may_2025_usd < 750000, 1, 0),
    under_500K = ifelse(value_may_2025_usd < 500000, 1, 0)
  )

# Create year and month variable
soc_2010_to_2024_real <- soc_2010_to_2024_real %>%
  mutate(
    year = year(start_date),
    month = month(start_date)
    )

rm(cpi_data, mayt_2025_cpi, temp_data, i, variables_post_2010, soc_2010_to_2024)

# National: Over 1.25MM ----

monthly_results_national <- soc_2010_to_2024_real %>%
  group_by(year, month, over_1.25MM, category) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup() %>%
  mutate(census_division = 'National')

# If a month has no starts over $1.25MM, add a row with 0 starts
monthly_results_national <- monthly_results_national %>%
  complete(year, month, over_1.25MM, category, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) %>%
  mutate(census_division = 'National')

# Transpose to wide format and get total starts
monthly_results_national <- monthly_results_national %>%
  select(-census_division) %>%
  spread(over_1.25MM, total_starts) 
  
  # Transpose to wide format and get total starts
  monthly_results_national <- monthly_results_national %>%
  rename(under_1.25MM = `0`, over_1.25MM = `1`) %>%
  mutate(total_starts = under_1.25MM + over_1.25MM) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each category
monthly_results_national <- monthly_results_national %>%
  pivot_wider(names_from = category, values_from = c(under_1.25MM, over_1.25MM, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 1.25MM, over 1.25MM and total
monthly_results_national <- monthly_results_national %>% 
  mutate(across(starts_with("under_1.25MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_1.25MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_national <- monthly_results_national %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_1.25MM_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_1.25MM_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts_"))

monthly_results_national_over1.25MM <- monthly_results_national

# National: 1MM - 1.25MM ----

monthly_results_national <- soc_2010_to_2024_real %>%
  group_by(year, month, over_1MM, category) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup() %>%
  mutate(census_division = 'National')

# If a month has no starts over $1MM, add a row with 0 starts
monthly_results_national <- monthly_results_national %>%
  complete(year, month, over_1MM, category, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) %>%
  mutate(census_division = 'National')

# Transpose to wide format and get total starts
monthly_results_national <- monthly_results_national %>%
  select(-census_division) %>%
  spread(over_1MM, total_starts) %>%
  rename(under_1MM = `0`, over_1MM = `1`) %>%
  mutate(total_starts = under_1MM + over_1MM) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each category
monthly_results_national <- monthly_results_national %>%
  pivot_wider(names_from = category, values_from = c(under_1MM, over_1MM, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 1MM, over 1MM and total
monthly_results_national <- monthly_results_national %>% 
  mutate(across(starts_with("under_1MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_1MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_national <- monthly_results_national %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_1MM_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_1MM_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts_"))
monthly_results_national_over1MM <- monthly_results_national

# National: 750K - 1MM ----

monthly_results_national <- soc_2010_to_2024_real %>%
  group_by(year, month, over_750K, category) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup() %>%
  mutate(census_division = 'National')

# If a month has no starts over $1.5MM, add a row with 0 starts
monthly_results_national <- monthly_results_national %>%
  complete(year, month, over_750K, category, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) %>%
  mutate(census_division = 'National')

# Transpose to wide format and get total starts
monthly_results_national <- monthly_results_national %>%
  select(-census_division) %>%
  spread(over_750K, total_starts) %>%
  rename(under_750K = `0`, over_750K = `1`) %>%
  mutate(total_starts = under_750K + over_750K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each category
monthly_results_national <- monthly_results_national %>%
  pivot_wider(names_from = category, values_from = c(under_750K, over_750K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 750K, over 750K and total
monthly_results_national <- monthly_results_national %>% 
  mutate(across(starts_with("under_750K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_750K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_national <- monthly_results_national %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_750K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_750K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts_"))

monthly_results_national_over750K <- monthly_results_national

# National: 500K - 750K ----

monthly_results_national <- soc_2010_to_2024_real %>%
  group_by(year, month, over_500K, category) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup() %>%
  mutate(census_division = 'National')

# If a month has no starts over $750K, add a row with 0 starts
monthly_results_national <- monthly_results_national %>%
  complete(year, month, over_500K, category, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) %>%
  mutate(census_division = 'National')

# Transpose to wide format and get total starts
monthly_results_national <- monthly_results_national %>%
  select(-census_division) %>%
  spread(over_500K, total_starts) %>%
  rename(under_500K = `0`, over_500K = `1`) %>%
  mutate(total_starts = under_500K + over_500K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each category
monthly_results_national <- monthly_results_national %>%
  pivot_wider(names_from = category, values_from = c(under_500K, over_500K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 500K, over 500K and total
monthly_results_national <- monthly_results_national %>% 
  mutate(across(starts_with("under_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_national <- monthly_results_national %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_500K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_500K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts_"))
monthly_results_national_over500K <- monthly_results_national

rm(monthly_results_national)
# National: Under 500K ----

monthly_results_national <- soc_2010_to_2024_real %>%
  group_by(year, month, under_500K, category) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup() %>%
  mutate(census_division = 'National')

# If a month has no starts over $750K, add a row with 0 starts
monthly_results_national <- monthly_results_national %>%
  complete(year, month, under_500K, category, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) %>%
  mutate(census_division = 'National')

# Transpose to wide format and get total starts
monthly_results_national <- monthly_results_national %>%
  select(-census_division) %>%
  spread(under_500K, total_starts) %>%
  rename(over_500K = `0`, under_500K = `1`) %>%
  mutate(total_starts = over_500K + under_500K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each category
monthly_results_national <- monthly_results_national %>%
  pivot_wider(names_from = category, values_from = c(over_500K, under_500K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 500K, over 500K and total
monthly_results_national <- monthly_results_national %>% 
  mutate(across(starts_with("over_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("under_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_national <- monthly_results_national %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_500K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_500K_")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts_"))
monthly_results_national_under500K <- monthly_results_national

rm(monthly_results_national)

# Regional: Over 1.25MM ----

monthly_results_regional <- soc_2010_to_2024_real %>%
  group_by(year, month, over_1.25MM, census_division) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup()

# If a month has no starts over $1.25MM, add a row with 0 starts
monthly_results_regional <- monthly_results_regional %>%
  complete(year, month, over_1.25MM, census_division, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3))

# Transpose to wide format and add total starts
monthly_results_regional <- monthly_results_regional %>%
  spread(over_1.25MM, total_starts) %>%
  rename(under_1.25MM = `0`, over_1.25MM = `1`) %>%
  mutate(total_starts = under_1.25MM + over_1.25MM) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each census division
monthly_results_regional <- monthly_results_regional %>%
  pivot_wider(names_from = census_division, values_from = c(under_1.25MM, over_1.25MM, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 1.25MM, over 1.25MM and total
monthly_results_regional <- monthly_results_regional %>% 
  mutate(across(starts_with("under_1.25MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_1.25MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_regional <- monthly_results_regional %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_1.25MM")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_1.25MM")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts"))

monthly_results_regional_over1.25MM <- monthly_results_regional

# Regional: 1MM - 1.25MM ----

monthly_results_regional <- soc_2010_to_2024_real %>%
  group_by(year, month, over_1MM, census_division) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup()

# If a month has no starts over $1MM, add a row with 0 starts
monthly_results_regional <- monthly_results_regional %>%
  complete(year, month, over_1MM, census_division, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3))

# Transpose to wide format and add total starts
monthly_results_regional <- monthly_results_regional %>%
  spread(over_1MM, total_starts) %>%
  rename(under_1MM = `0`, over_1MM = `1`) %>%
  mutate(total_starts = under_1MM + over_1MM) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each census division
monthly_results_regional <- monthly_results_regional %>%
  pivot_wider(names_from = census_division, values_from = c(under_1MM, over_1MM, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 1MM, over 1MM and total
monthly_results_regional <- monthly_results_regional %>% 
  mutate(across(starts_with("under_1MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_1MM"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_regional <- monthly_results_regional %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_1MM")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_1MM")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts"))

monthly_results_regional_over1MM <- monthly_results_regional

# Regional: 750K - 1MM ----

monthly_results_regional <- soc_2010_to_2024_real %>%
  group_by(year, month, over_750K, census_division) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup()

# If a month has no starts over $750K, add a row with 0 starts
monthly_results_regional <- monthly_results_regional %>%
  complete(year, month, over_750K, census_division, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3))

# Transpose to wide format and add total starts
monthly_results_regional <- monthly_results_regional %>%
  spread(over_750K, total_starts) %>%
  rename(under_750K = `0`, over_750K = `1`) %>%
  mutate(total_starts = under_750K + over_750K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each census division
monthly_results_regional <- monthly_results_regional %>%
  pivot_wider(names_from = census_division, values_from = c(under_750K, over_750K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 750K, over 750K and total
monthly_results_regional <- monthly_results_regional %>% 
  mutate(across(starts_with("under_750K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_750K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_regional <- monthly_results_regional %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_750K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_750K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts"))

monthly_results_regional_over750K <- monthly_results_regional

# Regional: 500K - 750K ----

monthly_results_regional <- soc_2010_to_2024_real %>%
  group_by(year, month, over_500K, census_division) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup()

# If a month has no starts over $500K, add a row with 0 starts
monthly_results_regional <- monthly_results_regional %>%
  complete(year, month, over_500K, census_division, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3)) 

# Transpose to wide format and add total starts
monthly_results_regional <- monthly_results_regional %>%
  spread(over_500K, total_starts) %>%
  rename(under_500K = `0`, over_500K = `1`) %>%
  mutate(total_starts = under_500K + over_500K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each census division
monthly_results_regional <- monthly_results_regional %>%
  pivot_wider(names_from = census_division, values_from = c(under_500K, over_500K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 500K, over 500K and total
monthly_results_regional <- monthly_results_regional %>% 
  mutate(across(starts_with("under_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("over_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_regional <- monthly_results_regional %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_500K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_500K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts"))

monthly_results_regional_over500K <- monthly_results_regional

# Regional: Under 500K ----

monthly_results_regional <- soc_2010_to_2024_real %>%
  group_by(year, month, under_500K, census_division) %>%
  summarize(total_starts = sum(weight)) %>%
  ungroup()

# If a month has no starts under $500K, add a row with 0 starts
monthly_results_regional <- monthly_results_regional %>%
  complete(year, month, under_500K, census_division, fill = list(total_starts = 0)) %>%
  filter(!(year == 2025 & month >3))

# Transpose to wide format and add total starts
monthly_results_regional <- monthly_results_regional %>%
  spread(under_500K, total_starts) %>%
  rename(over_500K = `0`, under_500K = `1`) %>%
  mutate(total_starts = over_500K + under_500K) %>%
  filter(year >= 2010) %>%
  select(-`<NA>`)

# Pivot wider so only one row per month and a column for each census division
monthly_results_regional <- monthly_results_regional %>%
  pivot_wider(names_from = census_division, values_from = c(over_500K, under_500K, total_starts),
              names_sep = "_")

# Create 3 month trailing average for under 500K, over 500K and total
monthly_results_regional <- monthly_results_regional %>% 
  mutate(across(starts_with("over_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("under_500K"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right"))) %>%
  mutate(across(starts_with("total_starts"), ~rollapply(.,3, mean, partial = TRUE, fill = NA, align = "right")))

# Rename
monthly_results_regional <- monthly_results_regional %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("over_500K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("under_500K")) %>%
  rename_with(~paste0(., "_3mo_avg"), .cols = starts_with("total_starts"))

monthly_results_regional_under500K <- monthly_results_regional

# Save to Excel ----

dataset_list <- list(
  "National - Over 1.25MM" = monthly_results_national_over1.25MM,
  "National - Over 1MM" = monthly_results_national_over1MM,
  "National - Over 750K" = monthly_results_national_over750K,
  "National - Over 500K" = monthly_results_national_over500K,
  "National - Under 500K" = monthly_results_national_under500K,
  
  "Regional - Over 1.25MM" = monthly_results_regional_over1.25MM,
  "Regional - Over 1MM" = monthly_results_regional_over1MM,
  "Regional - Over 750K" = monthly_results_regional_over750K,
  "Regional - Over 500K" = monthly_results_regional_over500K,
  "Regional - Under 500K" = monthly_results_regional_under500K
)

write.xlsx(dataset_list, output_file_path)
