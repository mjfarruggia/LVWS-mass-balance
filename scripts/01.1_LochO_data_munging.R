source("scripts/00_functions.R")
source("scripts/00_libraries.R")
#Pull in the disparate .csv files with loch outlet Q data
#(temp, cond, and other exist at different timescales but we'll just pull Q for now)


# Flow! -------------------------------------------------------------------


## This is old code from stored .csv files. I don't think this is needd
## anymore since we can just get it all from dataRetrieval?

# Q_1983to2010 <- read_csv(here("data/LochO_DailyData_19830930-20231208.csv"), skip=29, col_names = TRUE) %>%
#   slice(-1) %>% #Get rid of the first row, erroneous subheader
#   mutate(date = mdy(datetime),
#          Q_cfs = `250142_00060_00003`,
#          Q_cfs = as.numeric(Q_cfs)) %>%
#   select(-c(datetime, site_no, `250142_00060_00003_cd`, `250142_00060_00003`)) %>%
#   mutate(Q_m3s = Q_cfs * 0.02831) %>% #conver to cubic meters per second
#   filter(date <= "2011-09-30") %>%
#   ungroup() %>%
#   select(-agency_cd)


# Q_2010to2019 <- read_csv(here("data/LochO_DailyData_20101001-20190930.csv"), col_names = TRUE) %>%
#   select(Date, Discharge_CFS) %>%
#   rename(date=Date,
#          Q_cfs=Discharge_CFS) %>%
#   mutate(date = mdy(date),
#          Q_m3s = Q_cfs * 0.02831) #conver to cubic meters per second

## More recent data from dataRetrieal

# Get data for LV
lv_no <- '401733105392404'

# define parameters for discharge
params <- c('00060')

# get daily values from NWIS
LochQ <- readNWISdv(siteNumbers = lv_no, parameterCd = params,
                     startDate = '1983-10-01', endDate = '2024-09-30')

# rename columns using renameNWISColumns from package dataRetrieval
LochQ <- renameNWISColumns(LochQ)

LochQ <- LochQ %>%
  filter(Flow_cd=="A") %>%
  rename(Q_cfs = Flow,
         date= Date) %>%
  mutate(Q_m3s = Q_cfs * 0.02831) %>%
  select(date, Q_cfs, Q_m3s) %>%
  mutate(waterYear = calcWaterYear(date))

# LochQ <- bind_rows(Q_1983to2010,
#                    Q_2010to2019,
#                    Q_2019to2024) %>%
#   arrange(date) %>%
#   mutate(waterYear = calcWaterYear(date))

# Look at the gaps in 2019, 2020
LochQ %>%
  filter(waterYear %in% c("1985","1986")) %>%
  ggplot(aes(x=date, y=Q_m3s))+
  geom_point()+
  facet_wrap(~waterYear, scales="free_x")

# Need to look at how many NAs, and make a decision about gap filling for next step
LochQ_tsbl <- as_tsibble(LochQ, key = waterYear) %>%
  fill_gaps() %>% #adds ~400 missing days
  mutate(Q_m3s = imputeTS::na_interpolation(Q_m3s, maxgap = 60))

# Look at gaps in 2019, 2020 again
LochQ_tsbl %>%
  filter(waterYear %in% c("2019","2020")) %>%
  ggplot(aes(x=date, y=Q_m3s))+
  geom_point()+
  facet_wrap(~waterYear, scales="free_x")

# Anything weird in the rest of the data?
LochQ_tsbl %>%
  # filter(waterYear %in% c("2019","2020")) %>%
  ggplot(aes(x=date, y=Q_m3s))+
  geom_point()+
  facet_wrap(~waterYear, scales="free_x")


# calculate cumulative discharge for each year by first grouping by water year,
# and then using the "cumsum" function. Add day of water year for plotting purposes.
# These steps will build a new dataframe, with the existing information in yahara_dat
# but with two additional columns.
LochQ <- group_by(LochQ_tsbl, waterYear) %>%
  mutate(cumulative_dis = cumsum(Q_m3s), 
         wy_doy = hydro.day(date)) %>%
  filter(!waterYear %in% c("1983","2024"))


#Defining seasons by the hydrograph?
# percentile_days <- LochQ %>%
#   group_by(waterYear) %>%
#   arrange(date) %>%
#   mutate(cumulative_dis = cumsum(Q_m3s),
#          total_flow = sum(Q_m3s)) %>%
#   group_by(waterYear) %>%
#   summarise(
#     day_20th = cur_data()$date[which(cumulative_dis >= 0.2 * total_flow)[1]],
#     day_50th = cur_data()$date[which(cumulative_dis >= 0.5 * total_flow)[1]],
#     day_80th = cur_data()$date[which(cumulative_dis >= 0.8 * total_flow)[1]]
#   ) %>%
#   mutate(day_20th_wydoy = hydro.day(day_20th),
#          day_50th_wydoy = hydro.day(day_50th),
#          day_80th_wydoy = hydro.day(day_80th))

percentile_days <- data.frame(LochQ) %>%
  arrange(waterYear, date) %>%
  group_by(waterYear) %>%
  mutate(
    cumulative_dis = cumsum(Q_m3s),
    total_flow = sum(Q_m3s),
    frac_flow = cumulative_dis / total_flow
  ) %>%
  # For each threshold, find the first date
  summarise(
    day_20th = date[which.min(abs(frac_flow - 0.2))],
    day_50th = date[which.min(abs(frac_flow - 0.5))],
    day_80th = date[which.min(abs(frac_flow - 0.8))]
  ) %>%
  mutate(
    day_20th_wydoy = hydro.day(day_20th),
    day_50th_wydoy = hydro.day(day_50th),
    day_80th_wydoy = hydro.day(day_80th)
  )

# Check this works
percentile_days %>%
  ggplot(aes(x=waterYear, y=day_50th_wydoy))+
  geom_point()

# Changes in days from snowmelt to peak? 
percentile_days %>%
  ggplot(aes(x=waterYear, y=day_50th_wydoy-day_20th_wydoy))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="# days from start of snowmelt to peak")
lm1<- lm(day_50th_wydoy-day_20th_wydoy~waterYear, data=percentile_days)
summary(lm1) #modest, 1.6 days per decade shorter duration of snowmelt start to peak

# or peak to baseflow?
percentile_days %>%
  ggplot(aes(x=waterYear, y=day_80th_wydoy-day_50th_wydoy))+
  geom_point()+
  geom_smooth(method="lm")
# No

# Or start to baseflow? 
percentile_days %>%
  ggplot(aes(x=waterYear, y=day_80th_wydoy-day_20th_wydoy))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="# days from start of snowmelt to baseflow")
lm2<- lm(day_80th_wydoy-day_20th_wydoy~waterYear, data=percentile_days)
summary(lm2) #modest, 2.4 days per decade shorter duration of snowmelt start to baseflow



# Plot dates over raw. Look reasonable?
# Cumulative flow
LochQ %>%
  ggplot(aes(x=date, y=cumulative_dis))+
  geom_point()+
  geom_vline(xintercept=percentile_days$day_20th, color="blue")+
  geom_vline(xintercept=percentile_days$day_50th, color="green")+
  geom_vline(xintercept=percentile_days$day_80th, color="red")+
  facet_wrap(.~waterYear, scales="free_x")

# Raw discharge
LochQ %>%
  ggplot(aes(x=date, y=Q_m3s))+
  geom_line()+
  geom_vline(xintercept=percentile_days$day_20th, color="blue")+
  geom_vline(xintercept=percentile_days$day_50th, color="green")+
  geom_vline(xintercept=percentile_days$day_80th, color="red")+
  facet_wrap(.~waterYear, scales="free_x")


# Chemistry! --------------------------------------------------------------

Loch_chem <- read.csv("data/LV_chemistry_4Jun2025_MJF.csv") %>%
  filter(site == "loch" & sampleType == "norm") %>%
  group_by(site, sampleLocation, date, parameter) %>%
  #For now take mean of everything for multiple day entries, but ask Jill about this
  summarize(value = mean(value, na.rm=TRUE)) %>%
  mutate(waterYear = calcWaterYear(date),
         date = ymd(date))
head(Loch_chem)

LochO_chem <- Loch_chem %>%
  filter(sampleLocation == "out")

## Old code below
# LochO_chem <-
#   read.csv(
#     "data/LVWS_waterchem_master.csv",
#     sep = ",",
#     header = TRUE,
#     skip = 1,
#     na.strings = c("", " ", "NA")
#   ) %>%
#   select(-X) %>%
#   rename(
#     site_id = SITE.ID,
#     SODIUM = NA.,
#     FLUORINE = `F`,
#     NH4_calc = NH4.calc,
#     NO3_calc = NO3.calc,
#     TDN_calc = TDN.calc,
#     PO4_NREL_calc = PO4_NREL.calc,
#     TP_NREL_calc = TP_NREL.calc
#   ) %>%
#   mutate(
#     DATE = mdy(`DATE`),
#     NO3_calc = case_when( #override old column
#       NO3 == "<0.01" ~ 0.005,
#       NO3 == "<0.02" ~ 0.01,
#       NO3 == "<0.03" ~ 0.015,
#       TRUE ~ as.numeric(NO3)
#     ),
#     NH4_calc = case_when(
#       NH4 == "<0.01" ~ 0.005,
#       NH4 == "0" ~ 0.005, #check w Jill if this is actually how we wanna treat zeros
#       TRUE ~ as.numeric(NH4)
#     )) %>%
#   filter(SITE=="LOCH.O" | SITE=="LOCH.O ") %>%
#   filter(TYPE == "NORMAL") %>%
#   mutate(waterYear = calcWaterYear(DATE)) %>%
#   mutate(across(TEMP:ncol(.), as.numeric))
# length(unique(LochO_chem$DATE)) # There are some duplicate dates-- why?

# For now take mean of everything for multiple day entries, but ask Jill about this
# LochO_chem <- LochO_chem %>%
#   group_by(DATE) %>%
#   summarise(across(TEMP:waterYear, ~ mean(.x, na.rm = TRUE))) %>%
#   mutate(across(TEMP:waterYear, ~ ifelse(is.nan(.), NA, .)))

#Add in missing data from 2019 and 2020
# LochO_chem2 <- 
#   read.csv(
#     "data/LVWS_2019_2020_master.csv",
#     sep = ",",
#     skip = 1,
#     header = TRUE,
#     na.strings = c("", " ", "NA"),
#     strip.white = TRUE
#   ) %>%
#   rename(
#     site_id = SITE.ID,
#     SODIUM = NA.,
#     FLUORINE = `F`,
#     NH4_calc = NH4.calc,
#     NO3_calc = NO3.calc,
#     TDN_calc = TDN.calc,
#     PO4_NREL_calc = PO4_NREL.calc,
#     TP_NREL_calc = TP_NREL.calc
#   ) %>%
#   mutate(
#     DATE = mdy(`DATE`),
#     NO3_calc = case_when( #override old column
#       NO3 == "<0.01" ~ 0.005,
#       NO3 == "<0.02" ~ 0.01,
#       NO3 == "<0.03" ~ 0.015,
#       TRUE ~ as.numeric(NO3)
#     ),
#     NH4_calc = case_when(
#       NH4 == "<0.01" ~ 0.005,
#       NH4 == "0" ~ 0.005, #check w Jill if this is actually how we wanna treat zeros
#       TRUE ~ as.numeric(NH4)
#     )) %>%
#   filter(SITE=="LOCH.O" | SITE=="LOCH.O ") %>%
#   filter(TYPE == "NORMAL") %>%
#   mutate(waterYear = calcWaterYear(DATE)) %>%
#   mutate(across(TEMP:ncol(.), as.numeric))

# LochO_chem <- bind_rows(LochO_chem, LochO_chem2)

#Find dupes
# DUPES <- LochO_chem[duplicated(LochO_chem$DATE)|duplicated(LochO_chem$DATE, fromLast=TRUE),]
#There's some redundancy. Take the mean of the 2 (they are identical)

# LochO_chem <- LochO_chem %>%
#   group_by(DATE) %>%
#   summarise(across(TEMP:waterYear, ~ mean(.x, na.rm = TRUE))) %>%
#   mutate(across(TEMP:waterYear, ~ ifelse(is.nan(.), NA, .)))

# length(unique(LochO_chem$DATE))

# #Check for missing data
# vis_miss(Loch_chem)
# 
# #Check for missing data
# vis_miss(LochO_chem %>% filter(waterYear == 1989))


outlet_raw <- data.frame(LochQ) %>%
  select(date, waterYear, Q_m3s) %>%
  left_join(., LochO_chem, by=c("date","waterYear")) %>%
  left_join(., percentile_days %>% select(waterYear, day_20th_wydoy:day_80th_wydoy), by="waterYear") %>%
  mutate(wy_doy = hydro.day(date))

unique(outlet_raw$parameter)
LochO_chem %>%
  filter(parameter=="no3_mgl") %>%
  filter(waterYear=="2022") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  select(wy_doy, waterYear,parameter, value) %>%
  ggplot(aes(x=wy_doy, y=value))+
  geom_point()+
  facet_wrap(. ~ waterYear)
# Confirmeed that year-round Q and NO3 start in 1991. 1984-1991 will only use summer values
# Gap in 2022 - will let MJ know about it. I think I tracked down the missing data

#For now, work only with WY 1991 to present

# outlet_post91 <- outlet_raw %>%
#   filter(waterYear >= 1991 & waterYear <= 2021) %>%
#   mutate(SO4_impute = imputeTS::na_interpolation(SO4, maxgap = 14),
#          CA_impute = imputeTS::na_interpolation(CA, maxgap = 14),
#          # FLUORINE_impute = imputeTS::na_interpolation(FLUORINE, maxgap = 14),
#          K_impute = imputeTS::na_interpolation(K, maxgap = 14),
#          MG_impute = imputeTS::na_interpolation(MG, maxgap = 14),
#          SODIUM_impute = imputeTS::na_interpolation(SODIUM, maxgap = 14),
#          NH4_impute = imputeTS::na_interpolation(NH4_calc, maxgap = 14),
#          NO3_impute = imputeTS::na_interpolation(NO3_calc, maxgap = 14),
#          SiO2_impute = imputeTS::na_interpolation(SiO2, maxgap = 14))
# 
# #Check if these look ok
# outlet_post91 %>%
#   select(wy_doy, waterYear,NO3_impute, Q_m3s) %>%
#   pivot_longer(-c(wy_doy, waterYear)) %>%
#   ggplot(aes(x=wy_doy, y=value, color=name))+
#   geom_point()+
#   facet_wrap(name ~ waterYear) #Note still missing quite a bit of 2020 nitrate data :(
# 
# outlet_post91 <- outlet_post91 %>%
#   # mutate(cations = CA+FLUORINE+K+MG+SODIUM+NH4_calc) %>% #A lot NAs... leave this out for now
#   pivot_longer(c(contains("impute"))) %>% #units for all are mg/L
#   mutate(flux_mg_s = value * Q_m3s * 1000, #1000 L per m3
#          flux_kg_day = flux_mg_s * 86400 * 10e-6) 
# 
# annual_flux <- outlet_post91 %>%
#   select(waterYear, flux_kg_day, name) %>%
#   group_by(waterYear, name) %>%
#   mutate(cum_daily_flux_kg = cumsum(flux_kg_day),
#          wy_doy = seq(1:n())) %>%
#          # annual_flux_kg =sum(flux_kg_day)) %>%
#   arrange(waterYear, name)
# 
# annual_flux %>%
#   filter(name=="NO3_impute") %>%
#   ggplot(aes(x=wy_doy, y=cum_daily_flux_kg, color=name))+
#   geom_point()+
#   facet_wrap(name ~ waterYear)


#There are some gaps at the daily scale
#May need to just toss out the interpolation to the daily scale and just grab flow on the day
# of each water sample collection and estimated weekly fluxes? (daily C * daily Q * 7)?


## WEEKLY flux esimates instead
# 
outlet_raw_weekly <- LochO_chem %>%
  left_join(., LochQ %>% select(date, Q_m3s), by=c("date","waterYear")) %>%
  left_join(., percentile_days %>% select(waterYear, day_20th_wydoy:day_80th_wydoy), by="waterYear") %>%
  mutate(wy_doy = hydro.day(date))
# 
# outlet_weekly_flux <- outlet_raw_weekly %>%
#   filter(waterYear <= 2021) %>%
#   mutate(weekofyear = week(DATE)) %>%
#   mutate(cations_mgl = CA + MG + SODIUM + K,
#          inorganicN_mgL = NO3_calc + NH4_calc) %>%
#   # select(-c(CA, MG, SODIUM, K)) %>%
#   # If there are multiple samples in a week, just get the mean
#   select(DATE, waterYear, weekofyear, Q_m3s, SO4, cations_mgl, NH4_calc, NO3_calc, inorganicN_mgL, SiO2) %>%
#   pivot_longer(c(SO4, cations_mgl, NH4_calc, NO3_calc, inorganicN_mgL, SiO2),
#                names_to = "chem_name",
#                values_to = "chem_value") %>% #units for all are mg/L
#   group_by(chem_name, waterYear, weekofyear) %>% 
#   summarize(chem_value = median(chem_value, na.rm=TRUE),
#             Q_m3s = median(Q_m3s, na.rm=TRUE)) %>% #take the median if >1 sample per week
#   arrange(chem_name, waterYear, weekofyear) %>%
#   as_tsibble(., key = c(chem_name,waterYear), index = weekofyear) %>% #time series tibble
#   fill_gaps() %>%
#   as.data.frame() %>%
#   arrange(chem_name, waterYear, weekofyear) %>%
#   group_by(chem_name) %>%
#   mutate(chem_value = imputeTS::na_interpolation(chem_value, maxgap = 3),
#          Q_m3s = imputeTS::na_interpolation(Q_m3s, maxgap = 3)) %>%
#   group_by(chem_name, waterYear, weekofyear) %>%
#   mutate(flux_mg_s = chem_value * Q_m3s * 1000, #1000 L per m3
#          flux_kg_week = flux_mg_s * 86400 * 10e-6 * 7, #add * 7 potentially
#          flux_kg_per_ha = flux_kg_week / 660) %>%#kg per week 
#   mutate(chem_name = case_match(
#     chem_name,
#     "cations_mgl" ~ "cations",
#     "inorganicN_mgL" ~ "inorganic N",
#     "NH4_calc" ~ "NH4",
#     "NO3_calc" ~ "NO3",
#     .default = chem_name
#   ))
  

# vis_miss(outlet_weekly_flux %>% pivot_wider(names_from = chem_name,
#                                             values_from = chem_value))

  
######################
######################
######################
## THIS IS THE WAY####
## NEED TO CLEAN #####

#Start with daily interpolated chem values, join with daily flow
#then do the flux calculation
#why the eff are they so high?
unique(LochO_chem$parameter)
outlet_weekly_flux <- LochO_chem %>%
  filter(waterYear >= 1984 & waterYear <= 2023) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(cations_mgl = ca_mgl + mg_mgl + na_mgl + k_mgl) %>% 
  select(date, waterYear, so4_mgl, cations_mgl, nh4_mgl, no3_mgl, sio2_mgl) %>%
  pivot_longer(c(so4_mgl, cations_mgl, nh4_mgl, no3_mgl, sio2_mgl),
               names_to = "chem_name",
               values_to = "chem_value") %>% #units for all are mg/L
  # group_by(chem_name, waterYear, weekofyear) %>% 
  # summarize(chem_value = median(chem_value, na.rm=TRUE)) %>% #take the median if >1 sample per week
  arrange(chem_name, waterYear, date) %>%
  as_tsibble(., key = c(chem_name, waterYear), index = date) %>% #time series tibble
  fill_gaps() %>%
  as.data.frame() %>%
  arrange(chem_name, waterYear, date) %>%
  mutate(weekofyear = week(date)) %>%
  group_by(chem_name) %>%
  mutate(chem_value = imputeTS::na_interpolation(chem_value, maxgap = 30)) %>%
  left_join(., LochQ %>% select(waterYear, date, Q_m3s), by=c("date","waterYear"),
            relationship = "many-to-many") %>%
  mutate(chem_name = case_match(
    chem_name,
    "cations_mgl" ~ "cations",
    # "inorganicN_mgL" ~ "inorganic N",
    "nh4_mgl" ~ "NH4-N",
    "no3_mgl" ~ "NO3-N",
    "SO4" ~ "SO4-S",
    .default = chem_name
  )) %>%
  distinct(date, chem_name, .keep_all = TRUE) %>%
  mutate(chem_value = case_when(chem_name == "NO3-N" ~ chem_value * 0.226,
                                #The conversion factor is the ratio of the molar mass of N, 14 to the molar mass of NO3, 62
                                chem_name == "NH4-N" ~ chem_value * 0.777,
                                # conv. fact. is molar mass of N, 14 divided by molar mass of NH4 18
                                chem_name == "SO4-S" ~ chem_value * 0.333,
                                # conv. fact. s molar mass of S, 32 divided by molar mass of SO4, 96
                                TRUE ~ chem_value)) %>%
  group_by(waterYear) %>%
  mutate(daily_flux_kg = chem_value * Q_m3s * 86.4)  #kg per week 




#Calculate the fluxes at various timescales
annual_flux <- outlet_weekly_flux %>%
  mutate(
    month = month(date)
  ) %>%
  group_by(waterYear, chem_name) %>%
  summarise(
    # Calculate annual flux
    annual_flux_kg = sum(daily_flux_kg, na.rm = TRUE)) %>%
  mutate(
    # Calculate annual flux per hectare
    annual_flux_kg_per_ha = annual_flux_kg / 660)
    
summer_flux <- outlet_weekly_flux %>%
  mutate(
    month = month(date)
  ) %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(waterYear, chem_name) %>%
  summarise(
    # Calculate annual flux
    summer_flux_kg = sum(daily_flux_kg, na.rm = TRUE)) %>%
  mutate(
    # Calculate annual flux per hectare
    summer_flux_kg_per_ha = summer_flux_kg / 660)

june_flux <- outlet_weekly_flux %>%
  mutate(
    month = month(date)
  ) %>%
  filter(month %in% c(6)) %>%
  group_by(waterYear, chem_name) %>%
  summarise(
    # Calculate annual flux
    june_flux_kg = sum(daily_flux_kg, na.rm = TRUE)) %>%
  mutate(
    # Calculate annual flux per hectare
    june_flux_kg_per_ha = june_flux_kg / 660)

july_flux <- outlet_weekly_flux %>%
  mutate(
    month = month(date)
  ) %>%
  filter(month %in% c(7)) %>%
  group_by(waterYear, chem_name) %>%
  summarise(
    # Calculate annual flux
    july_flux_kg = sum(daily_flux_kg, na.rm = TRUE)) %>%
  mutate(
    # Calculate annual flux per hectare
    july_flux_kg_per_ha = july_flux_kg / 660)

august_flux <- outlet_weekly_flux %>%
  mutate(
    month = month(date)
  ) %>%
  filter(month %in% c(8)) %>%
  group_by(waterYear, chem_name) %>%
  summarise(
    # Calculate annual flux
    august_flux_kg = sum(daily_flux_kg, na.rm = TRUE)) %>%
  mutate(
    # Calculate annual flux per hectare
    august_flux_kg_per_ha = august_flux_kg / 660)


# Runoff:precipitation ----------------------------------------------------


source("scripts/01.3_NADP_CO98.R")
head(LochQ)
head(NADP)

Q_weekly <- data.frame(LochQ) %>%
  mutate(weekofyear = week(date),
         Q_m3 = Q_m3s * 86400) %>%
  group_by(waterYear, weekofyear) %>%
  summarize(Q_m3 = sum(Q_m3, na.rm=TRUE))

P_weekly <- NADP %>%
  mutate(ppt_m3 = (subppt_mm/1000)*loch_ws_size_m2) %>%
  select(waterYear, weekofyear, ppt_m3)

bind_RP <- full_join(Q_weekly, P_weekly, by=c("waterYear", "weekofyear"))

#How much missing data?
ggplot(bind_RP, 
       aes(x = weekofyear, 
           y = ppt_m3)) + 
  geom_miss_point()+
  facet_wrap(~waterYear) +
  labs(title="Missing data for PPT over the year")

ggplot(bind_RP, 
       aes(x = weekofyear, 
           y = Q_m3)) + 
  geom_miss_point()+
  facet_wrap(~waterYear) +
  labs(title="Missing data for Q over the year")


annual_RP <- bind_RP %>%
  group_by(waterYear) %>%
  summarize(ppt_m3 = sum(ppt_m3, na.rm=TRUE),
            Q_m3 = sum(Q_m3, na.rm=TRUE),
            RP = Q_m3/ppt_m3) %>%
  filter(waterYear >= 1984 & waterYear <= 2023)

