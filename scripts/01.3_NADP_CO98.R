source("scripts/00_functions.R")
source("scripts/00_libraries.R")

NADP <- read_csv(here("data/NTN-CO98-1984-2024.csv")) %>%
  mutate(dateTimeOn = mdy_hm(dateOn),
         dateTimeOff = mdy_hm(dateOff)) %>%
  select(-c(dateOn, dateOff)) %>%
  rename(subppt_mm = subppt,
         Ca_mgL = Ca,
         Mg_mgL = Mg,
         K_mgL = K,
         Na_mgL = na,
         NH4_mgL = NH4,
         NO3_mgL = NO3,
         Cl_mgL = Cl,
         SO4_mgL = SO4) %>%
  replace_with_na_all(condition = ~.x == -9) %>%
  replace_with_na_all(condition = ~.x == -9.990) %>%
  mutate(dateOn = date(dateTimeOn),
         dateOff = date(dateTimeOff),
         # Calculate the midpoint
         midpoint = dateOn + as.duration((dateOff - dateOn) / 2),
         start_date_plus_4 = dateOn + days(4),
         waterYear = calcWaterYear(start_date_plus_4),
         weekofyear = week(start_date_plus_4),
         year = year(start_date_plus_4))
         

as.duration((NADP$dateOff - NADP$dateOn) / 2)

#These came from Bret.Schichtel@colostate.edu
NADP_bret <- read_csv(here("data/WeeklyData_exp_subPrism22.csv")) %>%
  mutate(date=ymd(AvgDate),
         weekofyear=week(date),
         waterYear = calcWaterYear(date)) %>%
  rename(ppt_mm_bret=`weekly prec mm`,
         NO3_mgL_bret = `NO3 mg/l`,
         NH4_mgL_bret = `NH4 mg/l`,
         IN_mgL_bret = `IN mg/l`)

compare <- NADP_bret %>% 
  select(waterYear, date, weekofyear, ppt_mm_bret, NO3_mgL_bret) %>%
  left_join(NADP %>% select(midpoint,start_date_plus_4,  waterYear, dateOn, weekofyear, subppt_mm, NO3_mgL), by=c("waterYear","weekofyear"))


#How different?
compare %>%
  ggplot(aes(x=ppt_mm_bret, y=subppt_mm))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)

#How different?
compare %>%
  ggplot(aes(x=NO3_mgL_bret, y=NO3_mgL))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)
# Okay, so NO3 concentrations are largely, unchanged, it is ppt that is often underestimated, thus messing with deposition.
# I'll go ahead and use the ppt he used, and that'll also filled a few gaps in the N species

NADP <- NADP_bret %>%
  select(date, weekofyear, ppt_mm_bret, NO3_mgL_bret, NH4_mgL_bret, IN_mgL_bret) %>%
  left_join(., NADP, by=c("date"="start_date_plus_4","weekofyear"))


#Annual dep
annual_dep <- NADP %>%
  select(waterYear, weekofyear, ppt_mm_bret, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL_bret, NO3_mgL_bret, IN_mgL_bret, SO4_mgL, Na_mgL) %>%
  mutate(cations_mgL = Ca_mgL + Mg_mgL + Na_mgL + K_mgL) %>%
  select(-c(Ca_mgL, Mg_mgL, Na_mgL, K_mgL)) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  # filter(deposition_name=="NO3_mgL") %>%
  distinct(deposition_name, waterYear, weekofyear, .keep_all = TRUE) %>%
  arrange(deposition_name,waterYear, weekofyear) %>%
  as_tsibble(., key = c(deposition_name,waterYear), index = weekofyear) %>% #time series tibble
  fill_gaps() %>%
  #impute missing data
  mutate(deposition_name = case_match(
    deposition_name,
    "cations_mgL" ~ "cations",
    "NH4_mgL_bret" ~ "NH4-N",
    "NO3_mgL_bret" ~ "NO3-N",
    "SO4_mgL" ~ "SO4-S",
    "IN_mgL_bret" ~ "Inorganic N",
    .default = deposition_name
  )) %>%
  mutate(deposition_value_mg_L = case_when(deposition_name == "NO3-N" ~ deposition_value_mg_L * 0.226,
                                           deposition_name == "NH4-N" ~ deposition_value_mg_L * 0.777,
                                           deposition_name == "SO4-S" ~ deposition_value_mg_L * 0.333,
                                           TRUE ~ deposition_value_mg_L)) %>%
  mutate(ppt_mm_bret = imputeTS::na_interpolation(ppt_mm_bret, maxgap = 2),
         deposition_value_mg_L = imputeTS::na_interpolation(deposition_value_mg_L, maxgap = 2),
         rainwater_m3 = (ppt_mm_bret/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+3, # Convert  concentration from mg/L to kg/m³
         deposition_kg = rainwater_m3 * deposition_value_kg_m3, #kg across the time period (a week in this case)
         deposition_kg_per_ha = deposition_kg/660) %>%
  as.data.frame() %>%
  group_by(waterYear, deposition_name) %>%
  summarize(deposition_kg_per_ha_impute = sum(deposition_kg_per_ha, na.rm=TRUE))

#Monthly dep (June-Aug)
monthly_dep <- NADP %>%
  select(waterYear, weekofyear, ppt_mm_bret, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL_bret, NO3_mgL_bret, IN_mgL_bret, SO4_mgL, Na_mgL) %>%
  mutate(cations_mgL = Ca_mgL + Mg_mgL + Na_mgL + K_mgL) %>%
  select(-c(Ca_mgL, Mg_mgL, Na_mgL, K_mgL)) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  # filter(deposition_name=="NO3_mgL") %>%
  distinct(deposition_name, waterYear, weekofyear, .keep_all = TRUE) %>%
  arrange(deposition_name,waterYear, weekofyear) %>%
  as_tsibble(., key = c(deposition_name,waterYear), index = weekofyear) %>% #time series tibble
  fill_gaps() %>%
  #impute missing data
  mutate(deposition_name = case_match(
    deposition_name,
    "cations_mgL" ~ "cations",
    # "inorganicN_mgL" ~ "inorganic N",
    "NH4_mgL_bret" ~ "NH4-N",
    "NO3_mgL_bret" ~ "NO3-N",
    "SO4_mgL" ~ "SO4-S",
    "IN_mgL_bret" ~ "Inorganic N",
    .default = deposition_name
  )) %>%
  mutate(deposition_value_mg_L = case_when(deposition_name == "NO3-N" ~ deposition_value_mg_L * 0.226,
                                           deposition_name == "NH4-N" ~ deposition_value_mg_L * 0.777,
                                           deposition_name == "SO4-S" ~ deposition_value_mg_L * 0.333,
                                           TRUE ~ deposition_value_mg_L)) %>%
  mutate(ppt_mm_bret = imputeTS::na_interpolation(ppt_mm_bret, maxgap = 2),
         deposition_value_mg_L = imputeTS::na_interpolation(deposition_value_mg_L, maxgap = 2),
         rainwater_m3 = (ppt_mm_bret/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+3, # Convert  concentration from mg/L to kg/m³
         deposition_kg = rainwater_m3 * deposition_value_kg_m3, #kg across the time period (a week in this case)
         deposition_kg_per_ha = deposition_kg/660) %>%
  as.data.frame() %>%
  mutate(month = case_when(weekofyear %in% c(23,24,25,26) ~ "June",
                           weekofyear %in% c(27,28,29,30) ~ "July",
                           weekofyear %in% c(31,32,33,34) ~ "August")) %>%
  filter(month %in% c("June","July","August")) %>%
  group_by(waterYear, month, deposition_name) %>%
  summarize(deposition_kg_per_ha_impute = sum(deposition_kg_per_ha, na.rm=TRUE))


#Summer dep (June-Aug)
summer_dep <- NADP %>%
  select(waterYear, weekofyear, ppt_mm_bret, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL_bret, NO3_mgL_bret, IN_mgL_bret, SO4_mgL, Na_mgL) %>%
  mutate(cations_mgL = Ca_mgL + Mg_mgL + Na_mgL + K_mgL) %>%
  select(-c(Ca_mgL, Mg_mgL, Na_mgL, K_mgL)) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  # filter(deposition_name=="NO3_mgL") %>%
  distinct(deposition_name, waterYear, weekofyear, .keep_all = TRUE) %>%
  arrange(deposition_name,waterYear, weekofyear) %>%
  as_tsibble(., key = c(deposition_name,waterYear), index = weekofyear) %>% #time series tibble
  fill_gaps() %>%
  #impute missing data
  mutate(deposition_name = case_match(
    deposition_name,
    "cations_mgL" ~ "cations",
    # "inorganicN_mgL" ~ "inorganic N",
    "NH4_mgL_bret" ~ "NH4-N",
    "NO3_mgL_bret" ~ "NO3-N",
    "SO4_mgL" ~ "SO4-S",
    "IN_mgL_bret" ~ "Inorganic N",
    .default = deposition_name
  )) %>%
  mutate(deposition_value_mg_L = case_when(deposition_name == "NO3-N" ~ deposition_value_mg_L * 0.226,
                                           deposition_name == "NH4-N" ~ deposition_value_mg_L * 0.777,
                                           deposition_name == "SO4-S" ~ deposition_value_mg_L * 0.333,
                                           TRUE ~ deposition_value_mg_L)) %>%
  mutate(ppt_mm_bret = imputeTS::na_interpolation(ppt_mm_bret, maxgap = 2),
         deposition_value_mg_L = imputeTS::na_interpolation(deposition_value_mg_L, maxgap = 2),
         rainwater_m3 = (ppt_mm_bret/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+3, # Convert  concentration from mg/L to kg/m³
         deposition_kg = rainwater_m3 * deposition_value_kg_m3, #kg across the time period (a week in this case)
         deposition_kg_per_ha = deposition_kg/660) %>%
  as.data.frame() %>%
  mutate(month = case_when(weekofyear %in% c(23,24,25,26) ~ "June",
                           weekofyear %in% c(27,28,29,30) ~ "July",
                           weekofyear %in% c(31,32,33,34) ~ "August")) %>%
  filter(month %in% c("June","July","August")) %>%
  mutate(season="summer") %>%
  group_by(waterYear, season, deposition_name) %>%
  summarize(deposition_kg_per_ha_impute = sum(deposition_kg_per_ha, na.rm=TRUE))

