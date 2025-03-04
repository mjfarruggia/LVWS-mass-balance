#source scripts

source("scripts/00_functions.R")
source("scripts/00_libraries.R")
source("scripts/01.1_LochO_data_munging.R")
source("scripts/01.2_Bear_Lake_SNOTEL.R")
source("scripts/01.3_NADP_CO98.R")



NADP_trim <- NADP %>%
  select(waterYear, weekofyear, subppt_mm, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL, NO3_mgL, SO4_mgL, Na_mgL) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  mutate(rainwater_m3 = (subppt_mm/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+3, # Convert  concentration from mg/L to kg/m³
         deposition_kg = rainwater_m3 * deposition_value_kg_m3, #kg across the time period (a week in this case)
         deposition_kg_per_ha = deposition_kg/660) 


#If I sum this up annually, do the numbers make sense?

N_only <- NADP_trim %>%
  filter(deposition_name=="NO3_mgL") %>%
  distinct(waterYear, weekofyear, .keep_all = TRUE) %>%
  group_by(waterYear, deposition_name) %>%
  summarize(deposition_kg_per_ha_per_year = sum(deposition_kg_per_ha, na.rm=TRUE))

#interpolated data
N_only_impute <- NADP %>%
  select(waterYear, weekofyear, subppt_mm, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL, NO3_mgL, SO4_mgL, Na_mgL) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  filter(deposition_name=="NO3_mgL") %>%
  distinct(waterYear, weekofyear, .keep_all = TRUE) %>%
  arrange(waterYear, weekofyear) %>%
  as_tsibble(., key = waterYear, index = weekofyear) %>% #time series tibble
  fill_gaps() %>%
  #impute missing data
  mutate(subppt_mm = imputeTS::na_interpolation(subppt_mm, maxgap = 4),
         deposition_value_mg_L = imputeTS::na_interpolation(deposition_value_mg_L, maxgap = 4),
         rainwater_m3 = (subppt_mm/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+3, # Convert  concentration from mg/L to kg/m³
         deposition_kg = rainwater_m3 * deposition_value_kg_m3, #kg across the time period (a week in this case)
         deposition_kg_per_ha = deposition_kg/660)
  

as.data.frame() %>%
  group_by(waterYear, deposition_name) %>%
  summarize(deposition_kg_per_ha_per_year = sum(deposition_kg_per_ha_impute, na.rm=TRUE))

