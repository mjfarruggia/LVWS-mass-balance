#Pull in the disparate .csv files with loch outlet Q data
#(temp, cond, and other exist at different timescales but we'll just pull Q for now)

Q_1983to2010 <- read_csv(here("data/LochO_DailyData_19830930-20231208.csv"), skip=29, col_names = TRUE) %>%
  slice(-1) %>% #Get rid of the first row, erroneous subheader
  mutate(date = mdy(datetime),
         Q_cfs = `250142_00060_00003`,
         Q_cfs = as.numeric(Q_cfs)) %>%
  select(-c(datetime, site_no, `250142_00060_00003_cd`, `250142_00060_00003`)) %>%
  mutate(Q_m3s = Q_cfs * 0.02831) %>% #conver to cubic meters per second
  filter(date <= "2010-09-30") %>%
  ungroup() %>%
  select(-agency_cd)


Q_2010to2019 <- read_csv(here("data/LochO_DailyData_20101001-20190930.csv"), col_names = TRUE) %>%
  select(Date, Discharge_CFS) %>%
  rename(date=Date,
         Q_cfs=Discharge_CFS) %>%
  mutate(date = mdy(date),
         Q_m3s = Q_cfs * 0.02831) #conver to cubic meters per second

#More recent data from dataRetrieal

# Get data for LV
lv_no <- '401733105392404'

# define parameters for discharge
params <- c('00060')

# get daily values from NWIS
Q_2019to2024 <- readNWISdv(siteNumbers = lv_no, parameterCd = params,
                     startDate = '2019-10-01', endDate = '2024-09-30')

# rename columns using renameNWISColumns from package dataRetrieval
Q_2019to2024 <- renameNWISColumns(Q_2019to2024)

Q_2019to2024 <- Q_2019to2024 %>%
  filter(Flow_cd=="A") %>%
  rename(Q_cfs = Flow,
         date= Date) %>%
  mutate(Q_m3s = Q_cfs * 0.02831) %>%
  select(date, Q_cfs, Q_m3s)

LochQ <- bind_rows(Q_1983to2010,
                   Q_2010to2019,
                   Q_2019to2024) %>%
  arrange(date) %>%
  mutate(waterYear = calcWaterYear(date))

# calculate cumulative discharge for each year by first grouping by water year,
# and then using the "cumsum" function. Add day of water year for plotting purposes.
# These steps will build a new dataframe, with the existing information in yahara_dat
# but with two additional columns.
LochQ <- group_by(LochQ, waterYear) %>%
  mutate(cumulative_dis = cumsum(Q_m3s), 
         wy_doy = hydro.day(date)) %>%
  filter(!waterYear == "1983")


#Defining seasons by the hydrograph?
percentile_days <- LochQ %>%
  group_by(waterYear) %>%
  arrange(date) %>%
  mutate(cumulative_dis = cumsum(Q_m3s),
         total_flow = sum(Q_m3s)) %>%
  group_by(waterYear) %>%
  summarise(
    day_20th = cur_data()$date[which(cumulative_dis >= 0.2 * total_flow)[1]],
    day_50th = cur_data()$date[which(cumulative_dis >= 0.5 * total_flow)[1]],
    day_80th = cur_data()$date[which(cumulative_dis >= 0.8 * total_flow)[1]]
  ) %>%
  mutate(day_20th_wydoy = hydro.day(day_20th),
         day_50th_wydoy = hydro.day(day_50th),
         day_80th_wydoy = hydro.day(day_80th))

tree -L 2
