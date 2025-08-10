# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "fredr", "openxlsx", "lubridate", "xts", "conflicted", "readxl")

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
library(conflicted)
library(readxl)

# Prefer the dplyr package for a few functions
conflicts_prefer(dplyr::filter, dplyr::lag)

rm(install_if_missing, packages)

# File paths ----

# Update the input_file_path
input_file_path <- "data/soc24.xls"

# Update the year_of_analysis according to the input file path you have chosen
year_of_analysis <- 2024

ouput_file_path_for_cleaned_microdata <- paste0("cleaned-data/soc_", year_of_analysis, "_cleaned.xlsx")
ouput_file_path_for_cleaned_summary_file <- paste0("data-output/soc_data_summarized_by_census_division_", year_of_analysis, ".xlsx")

# Note: This script will not work prior to 2017 as microdata before 2017 did not feature the 'ID' column (which is included in the custom reclassiy_soc_data function below)
# Data cleaning ----

data <- read_excel(input_file_path)

if (year_of_analysis < 2023) {
  data <- data %>%
    mutate(across(c(COMP:WEIGHT, CONPR:AUTH), as.numeric))
} else {
  message("No transformation applied: year_of_analysis is 2023 or later.")
}

reclassiy_soc_data <- function(data) {
  data <- data %>%
    mutate(
      
      ACS = case_when(
        ACS == '1' ~ 'yes',
        ACS == '2' ~ 'no',
        ACS == '0' ~ 'not_reported',
      ),
      
      AGER = case_when(
        AGER == '1' ~ 'yes',
        AGER == '2' ~ 'no',
        AGER == '0' ~ 'not_reported',
      ),
      
      ASSOC = case_when(
        ASSOC == '1' ~ 'yes',
        ASSOC == '2' ~ 'no',
        ASSOC == '0' ~ 'not_reported',
      ),
      
      BASE = case_when(
        BASE == '01' ~ 'full_or_partial_basement',
        BASE == '02' ~ 'crawl_space',
        BASE == '03' ~ 'slab',
        BASE == '04' ~ 'other',
        BASE == '00' ~ 'not_reported',
      ),
      
      CAT = case_when(
        CAT == '1' ~ 'built_for_sale',
        CAT == '2' ~ 'contractor_built',
        CAT == '3' ~ 'owner_built',
        CAT == '4' ~ 'built_for_rent'
      ),
      
      CLOS = case_when(
        CLOS == '1' ~ 'yes',
        CLOS == '2' ~ 'no',
        CLOS == '0' ~ 'not_available_or_not_reported'
      ),
      
      CON = case_when(
        CON == '1' ~ 'yes',
        CON == '2' ~ 'no',
        CON == '0' ~ 'not_reported'
      ),
      
      DECK = case_when(
        DECK == '1' ~ 'yes',
        DECK == '2' ~ 'no',
        DECK == '0' ~ 'not_reported'
      ),
      
      DET = case_when(
        DET == '1' ~ 'detached',
        DET == '2' ~ 'attached',
        DET == '0' ~ 'not_reported'
      ),
      
      DIV = case_when(
        DIV == '1' ~ 'new_england',
        DIV == '2' ~ 'mid_atlantic',
        DIV == '3' ~ 'east_north_central',
        DIV == '4' ~ 'west_north_central',
        DIV == '5' ~ 'south_atlantic',
        DIV == '6' ~ 'east_south_central',
        DIV == '7' ~ 'west_south_central',
        DIV == '8' ~ 'mountain',
        DIV == '9' ~ 'pacific'
      ),
      
      FINC = case_when(
        FINC == '01' ~ 'conventional',
        FINC == '02' ~ 'fha',
        FINC == '03' ~ 'va',
        FINC == '04' ~ 'cash',
        FINC == '05' ~ 'other',
        FINC == '00' ~ 'not_reported'
      ),
      
      FNBS = case_when(
        FNBS == '1' ~ 'yes',
        FNBS == '2' ~ 'no',
        FNBS == '0' ~ 'not_applicable_or_not_reported'
      ),
      
      FOYER = case_when(
        FOYER == '1' ~ 'yes',
        FOYER == '2' ~ 'no',
        FOYER == '0' ~ 'not_reported'
      ),
      
      FRAME = case_when(
        FRAME == '1' ~ 'wood',
        FRAME == '2' ~ 'steel',
        FRAME == '3' ~ 'concrete_or_masonry_other_than_concrete_forms',
        FRAME == '4' ~ 'insulated_concrete_forms',
        FRAME == '0' ~ 'not_reported'
      ),
      
      GAR = case_when(
        GAR == '1' ~ '1_car_garage',
        GAR == '2' ~ '2_car_garage',
        GAR == '3' ~ '3_or_more_car_garage',
        GAR == '4' ~ 'other',
        GAR == '0' ~ 'not_reported'
      ),
      
      HEAT = case_when(
        HEAT == '01' ~ 'air_source_or_ground_source_heat_pump',
        HEAT == '02' ~ 'forced_air_furnace_without_heat_pump',
        HEAT == '03' ~ 'hot_water_or_steam_system',
        HEAT == '04' ~ 'other_or_no_heat',
        HEAT == '00' ~ 'not_reported'
      ),
      
      HEAT2 = case_when(
        HEAT2 == '01' ~ 'air_source_or_ground_source_heat_pump',
        HEAT2 == '02' ~ 'forced_air_furnace_without_heat_pump',
        HEAT2 == '03' ~ 'hot_water_or_steam_system',
        HEAT2 == '04' ~ 'other_or_no_heat',
        HEAT2 == '00' ~ 'not_reported_or_no_second_heating_system'
      ),
      
      LNDR = case_when(
        LNDR == '1' ~ 'basement',
        LNDR == '2' ~ 'first_floor',
        LNDR == '3' ~ 'second_floor_or_higher',
        LNDR == '4' ~ 'garage_or_carport',
        LNDR == '5' ~ 'no_connection_planned',
        LNDR == '6' ~ 'multiple_locations',
        LNDR == '0' ~ 'not_reported'
      ),
      
      MFGS = case_when(
        MFGS == '1' ~ 'modular',
        MFGS == '2' ~ 'panelized_precut',
        MFGS == '3' ~ 'site_built',
        MFGS == '0' ~ 'not_reported'
      ),
      
      PATI = case_when(
        PATI == '1' ~ 'yes',
        PATI == '2' ~ 'no',
        PATI == '0' ~ 'not_reported'
      ),
      
      PRCH = case_when(
        PRCH == '1' ~ 'yes',
        PRCH == '2' ~ 'no',
        PRCH == '0' ~ 'not_reported'
      ),
      
      SEWER = case_when(
        SEWER == '1' ~ 'public_sewers',
        SEWER == '2' ~ 'individual_septic_system',
        SEWER == '3' ~ 'other'
      ),
      
      STOR = case_when(
        STOR == '1' ~ '1',
        STOR == '2' ~ '2',
        STOR == '3' ~ '3_or_more',
        STOR == '0' ~ 'not_reported'
      ),
      
      WAL1 = case_when(
        WAL1 == '01' ~ 'wood_or_wood_products',
        WAL1 == '02' ~ 'brick_or_brick_veneer',
        WAL1 == '03' ~ 'aluminum_siding',
        WAL1 == '04' ~ 'stucco',
        WAL1 == '05' ~ 'vinyl_siding',
        WAL1 == '06' ~ 'concrete_block',
        WAL1 == '07' ~ 'stone_rock_or_other_stone_materials',
        WAL1 == '08' ~ 'fiber_cement_siding',
        WAL1 == '09' ~ 'other',
        WAL1 == '00' ~ 'not_reported'
      ),
      
      WAL2 = case_when(
        WAL2 == '01' ~ 'wood_or_wood_products',
        WAL2 == '02' ~ 'brick_or_brick_veneer',
        WAL2 == '03' ~ 'aluminum_siding',
        WAL2 == '04' ~ 'stucco',
        WAL2 == '05' ~ 'vinyl_siding',
        WAL2 == '06' ~ 'concrete_block',
        WAL2 == '07' ~ 'stone_rock_or_other_stone_materials',
        WAL2 == '08' ~ 'fiber_cement_siding',
        WAL2 == '09' ~ 'other',
        WAL2 == '00' ~ 'not_reported_or_no_secondary_wall_material'
      ),
      
      WALS = case_when(
        WALS == '1' ~ 'yes',
        WALS == '2' ~ 'no',
        WALS == '0' ~ 'not_reported'
      ),
      
      WATER = case_when(
        WATER == '1' ~ 'public_water',
        WATER == '2' ~ 'individual_well',
        WATER == '3' ~ 'other',
        WATER == '0' ~ 'not_reported'
      ),
      
      AREA = if_else(AREA == 0, -99, AREA),
      
      BEDR = case_when(
        BEDR == '2' ~ '2_bedrooms_or_less',
        BEDR == '3' ~ '3_bedrooms',
        BEDR == '4' ~ '4_bedrooms',
        BEDR == '5' ~ '5_bedrooms_or_more',
        BEDR == '0' ~ 'not_reported'
      ),
      
      # Edited this (used to be 'not_completed')
      COMP = if_else(COMP == 0, -99, COMP),
      
      FNSQ = if_else(FNSQ == 0, -99, FNSQ),
      
      FFNSQ = if_else(FFNSQ == 0, -99, FFNSQ),
      
      FPLS = case_when(
        FPLS == '0' ~ 'none',
        FPLS == '1' ~ '1_fireplace',
        FPLS == '2' ~ '2_or_more_fireplaces',
        FPLS == '9' ~ 'not_reported'
      ),
      
      FULB = case_when(
        FULB == '1' ~ '1_bathroom_or_less',
        FULB == '2' ~ '2_bathrooms',
        FULB == '3' ~ '3_bathrooms',
        FULB == '4' ~ '4_bathrooms_or_more',
        FULB == '9' ~ 'not_reported'
      ),
      
      HAFB = case_when(
        HAFB == '0' ~ '0_half_bathrooms',
        HAFB == '1' ~ '1_half_bathroom',
        HAFB == '2' ~ '2_half_bathrooms_or_more',
        HAFB == '9' ~ 'not_reported'
      ),
      
      LOTV = if_else(LOTV == 0, -99, LOTV),
      
      PVALU = if_else(PVALU == 0, -99, PVALU),
      
      # Edited this (used to be 'not_sold')
      SALE = if_else(SALE == 0, -99, SALE),
      
      FSQFS = if_else(FSQFS == 0, -99, FSQFS),
      
      # Edited this (used to be 'not_started')
      STRT = if_else(STRT == 0, -99, STRT),
      
      CONPR = if_else(CONPR == 0, -99, CONPR),
      
      SLPR = if_else(SLPR == 0, -99, SLPR),
      
      SQFS = if_else(SQFS == 0, -99, SQFS),
      
      FUEL = case_when(
        FUEL == '01' ~ 'electricity',
        FUEL == '02' ~ 'natural_gas',
        FUEL == '03' ~ 'bottled_or_liquified_petroleum_gas',
        FUEL == '04' ~ 'oil',
        FUEL == '05' ~ 'other_or_no_heat',
        FUEL == '00' ~ 'not_reported'
      ),
      
      FUEL2 = case_when(
        FUEL2 == '01' ~ 'electricity',
        FUEL2 == '02' ~ 'natural_gas',
        FUEL2 == '03' ~ 'bottled_or_liquified_petroleum_gas',
        FUEL2 == '04' ~ 'oil',
        FUEL2 == '05' ~ 'other_or_no_heat',
        FUEL2 == '00' ~ 'not_reported_or_no_second_heating_system'
      ),
      
      FCONPR = if_else(FCONPR == 0, -99, FCONPR),
      
      FSLPR = if_else(FSLPR == 0, -99, FSLPR),
      
      AUTH = as.yearmon(
        paste0(
          substr(AUTH, 1, 4), "-", 
          substr(AUTH, 5, 6), "-01"
        )
      )
    )
}

data <- reclassiy_soc_data(data)

data <- data %>%
  select(-ends_with('_F')) %>%
  select(ID, AUTH, everything())

write.xlsx(data, ouput_file_path_for_cleaned_microdata)

# Prop by financing source ----

prop_by_financing <- data %>%
  filter(FINC != 'not_reported') %>%
  group_by(FINC, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by construction method ----

prop_by_construction_method <- data %>%
  filter(MFGS != 'not_reported') %>%
  group_by(MFGS, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by framing material ----

prop_by_framing_material <- data %>%
  filter(FRAME != 'not_reported') %>%
  group_by(FRAME, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by exterior wall material ----

prop_by_exterior_wall_material <- data %>%
  filter(WAL1 != 'not_reported') %>%
  group_by(WAL1, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup() 

# Prop by secondary exterior wall material ----

prop_by_secondary_exterior_wall_material <- data %>%
  filter(WALS != 'not_reported') %>%
  mutate(WAL2 = if_else(WAL2 == 'not_reported_or_no_secondary_wall_material', 'no_secondary_wall_material', WAL2)) %>%
  group_by(WAL2, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# prop_by_exterior_wall_material %>%
#   ggplot(aes(x = factor(1), y = prop * 100, fill = WAL1)) +
#   geom_bar(stat = "identity", width = .25) +
#   coord_polar(theta = "y", direction = -1) +
#   theme_jbrec(base_text_size = 10) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   labs(title = 'Share of SF Starts by Primary Exterior Wall Material, by Census Division') +
#   # geom_text(aes(label = scales::percent(round(prop, 2), suffix = "%")),
#   #           position = position_stack(vjust = 0.5), size = 4) +
#   scale_fill_jbrec(discrete = TRUE) +
#   facet_wrap(~DIV, scales = "free_y", nrow = 3) +
#   theme(
#     axis.text.y = element_blank(),  # This will remove the x-axis text
#     axis.ticks.y = element_blank(),  # This will remove the x-axis ticks
#     axis.title.y = element_blank(),
#     axis.title.x = element_blank(),
#     #axis.text.x = element_blank(),  # This will remove the x-axis text
#     axis.ticks.x = element_blank(),
#     #axis.lines.x = element_blank(),
#     panel.border.x = element_blank(),
#     legend.key.width = unit(.5, "cm"))

# Prop by heating system ----

prop_by_heating_system <- data %>%
  filter(HEAT != 'not_reported') %>%
  group_by(HEAT, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by heating fuel ----

prop_by_heating_fuel <- data %>%
  filter(FUEL != 'not_reported') %>%
  group_by(FUEL, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by water supply ----

prop_by_water_supply <- data %>%
  filter(WATER != 'not_reported') %>%
  group_by(WATER, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by sewage disposal system ----

prop_by_sewage_system <- data %>%
  filter(!is.na(SEWER)) %>%
  group_by(SEWER, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop by stories ----

prop_by_stories <- data %>%
  filter(STOR != 'not_reported') %>%
  group_by(STOR, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop with a deck ----

prop_with_a_deck <- data %>%
  filter(DECK != 'not_reported') %>%
  group_by(DECK, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()

# Prop with a patio ----

prop_with_a_patio <- data %>%
  filter(PATI != 'not_reported') %>%
  group_by(PATI, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()


# Prop by parking ----

# “Other” includes carport, other off‐street parking (including a driveway with no garage or carport), and other parking facilities. 
prop_by_parking <- data %>%
  filter(GAR != 'not_reported') %>%
  group_by(GAR, DIV) %>%
  summarize(starts = sum(WEIGHT, na.rm = T)) %>%
  ungroup() %>%
  group_by(DIV) %>%
  mutate(prop = starts/sum(starts)) %>%
  ungroup()


# Output Data ----

dataset_list <- list(
                     'Prop by financing' = prop_by_financing,
                     'Prop by con. meth.' = prop_by_construction_method,
                     'Prop by framing mat.' = prop_by_framing_material,
                     'Prop by prim. ext. mat.' = prop_by_exterior_wall_material,
                     'Prop by sec. ext. mat.' = prop_by_secondary_exterior_wall_material,
                     'Prop by heating system' = prop_by_heating_system,
                     'Prop by heating fuel' = prop_by_heating_fuel,
                     'Prop by water supply' = prop_by_water_supply,
                     'Prop by sewage system' = prop_by_sewage_system,
                     'Prop by stories' = prop_by_stories,
                     'Prop with a deck' = prop_with_a_deck,
                     'Prop with a patio' = prop_with_a_patio,
                     'Prop by parking' = prop_by_parking
                     )

write.xlsx(dataset_list, ouput_file_path_for_cleaned_summary_file)
