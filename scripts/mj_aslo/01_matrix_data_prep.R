source("functions/00_functions.R")
source("functions/00_libraries.R")
library(tidyverse)

lv <- read.csv("data/mj_aslo/LV_chemistry.csv")

#----data prep------------------------------------------------------------------------------

unique(lv$sampleLocation)

#keep: sample_type=norm,  sampleLocation = "ls" or "shr" or "out" or "in" or "in_s" or "in_n", sample rep =1
# this just excludes the hypo and meta samples and only keeps surface/stream samples
lv_filtered <- lv %>%
  filter(
    sampleType == "norm",
    sampleLocation %in% c("ls", "shr", "out", "in", "in_s", "in_n"), 
    sampleReplicate == 1) 


# ggplot(lv_filtered, aes(x = month(date, label = TRUE), fill = factor(year))) +
#   geom_bar(position = "dodge") +
#   labs(title = "Monthly Sampling by Year", x = "Month", y = "Count", fill = "Year")+
#   facet_wrap(site~sampleLocation, scales = "free_y") +
#   theme_bw()


lv_wide <- lv_filtered %>%
  pivot_wider(id_cols = c(site, datetimeDenver, sampleLocation, lab),      
              names_from = parameter,      
              values_from = value )

lv_wide <- lv_wide %>%
  rename(
    lake_ID = site,
    ANC = anc_ueql,
    Ca_mgL = ca_mgl,
    Mg_mgL = mg_mgl,
    Na_mgL = na_mgl,
    K_mgL = k_mgl,
    NH4_mgL = nh4_mgl,
    SO4_mgL = so4_mgl,
    NO3_mgL = no3_mgl,
    PO4_mgL = po4_mgl,
    Cl_mgL = cl_mgl,
    F_mgL = f_mgl,
    SiO2_mgL = sio2_mgl,
    DOC_mgL = doc_mgl,
    TDN_mgL = tdn_mgl,
    TP_ugL = tp_ugl,
    chla_ugL = chla_ugl,
    cond_uScm = fldcond_uscm,   
    labcond_uScm = labcond_uscm,
    pH = fldph, 
    labpH = labph, 
    chla_ugL= chla_ugl, 
    Al_ugL=al_ugl,
    Mn_ugL=mn_ugl, 
    Fe_ugL=fe_ugl
  )



#add z scores by lake and analyte instead of just by lake
lv_wide <- lv_wide %>%
  group_by(lake_ID) %>%
  mutate(
    z_ANC = scale(ANC),
    z_Ca_mgL = scale(Ca_mgL),
    z_Cl_mgL = scale(Cl_mgL),
    z_DOC_mgL = scale(DOC_mgL),
    z_F_mgL = scale(F_mgL),
    z_K_mgL = scale(K_mgL),
    z_Mg_mgL = scale(Mg_mgL),
    z_NH4_mgL = scale(NH4_mgL),
    z_NO3_mgL = scale(NO3_mgL),
    z_Na_mgL = scale(Na_mgL),
    z_PO4_mgL = scale(PO4_mgL),
    z_SO4_mgL = scale(SO4_mgL),
    z_TDN_mgL = scale(TDN_mgL),
    z_TP_ugL = scale(TP_ugL),
    z_cond_uScm = scale(cond_uScm),
    z_labcond_uScm=scale(labcond_uScm),
    z_labpH = scale(labpH),
    z_pH = scale(pH), 
    z_chla_ugL= scale(chla_ugL), 
    z_Al_ugL=scale(Al_ugL),
    z_Mn_ugL=scale(Mn_ugL), 
    z_Fe_ugL=scale(Fe_ugL)
  )

lv_wide$lab <- NULL
lv_wide$year <- year(lv_wide$datetimeDenver)

unique(lv_wide$lake_ID)


#no3 marss matrix---------------------------------------------------------------------

lv_no3 <- lv_wide %>%
  ungroup() %>%
  filter(lake_ID %in% c("sky", "loch", "andrewscreek")) %>%
  filter(!is.na(NO3_mgL)) %>%
  distinct(lake_ID, sampleLocation, datetimeDenver, .keep_all = TRUE)%>%
  select(lake_ID, sampleLocation, datetimeDenver, year, NO3_mgL)

lv_no3 <- lv_no3 %>%
  mutate(datetimeDenver = as.POSIXct(datetimeDenver)) %>%
  arrange(datetimeDenver)

lv_no3 <- lv_no3 %>%
  mutate(series = paste(lake_ID, sampleLocation, sep = "_"))

#monthly avg
lv_no3_monthly <- lv_no3 %>%
  mutate( month = month(datetimeDenver)) %>%
  group_by(lake_ID, sampleLocation, year, month) %>%
  summarise(NO3_mgL = mean(NO3_mgL, na.rm = TRUE),.groups = "drop") %>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"),
    date = as.Date(paste(year, month, "01", sep = "-")) )%>%
  arrange(date)

lv_no3_monthly$date <- as.Date(lv_no3_monthly$date)

lv_no3_monthly_wide <- lv_no3_monthly %>%
  select(date, site, NO3_mgL) %>%
  pivot_wider(names_from = date,
    values_from = NO3_mgL)

#daily
lv_no3_daily <- lv_no3 %>%
  arrange(datetimeDenver)%>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"))%>%
  mutate(date = as.Date(datetimeDenver))

lv_no3_daily$date <- as.Date(lv_no3_daily$date)


lv_no3_daily_wide <- lv_no3_daily %>%
  select(date, site, NO3_mgL) %>%
  pivot_wider(names_from = date,
              values_from = NO3_mgL)

#rearrange rows so it makes sense spatially
site_order <- c(
  "sky_in_n",
  "sky_in_s",
  "sky_ls",
  "sky_out",
  "andrewscreek_shr",
  "loch_in",
  "loch_ls",
  "loch_out")

lv_no3_monthly_wide<- lv_no3_monthly_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

lv_no3_daily_wide<- lv_no3_daily_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

#marss format
monthly_no3_matrix <- lv_no3_monthly_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()

daily_no3_matrix <- lv_no3_daily_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()


# ggplot(lv_no3_daily, aes(datetimeDenver, NO3_mgL, color = site)) +
#   geom_point(alpha = 0.7) +
#   facet_wrap(~site) +
#   scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

#use 1984 to 2023
#exclude sky_in_s



# also prep SO4, silica, N:P ---------------------------------------
lv_so4 <- lv_wide %>%
  ungroup() %>%
  filter(lake_ID %in% c("sky", "loch", "andrewscreek")) %>%
  filter(!is.na(SO4_mgL)) %>%
  distinct(lake_ID, sampleLocation, datetimeDenver, .keep_all = TRUE)%>%
  select(lake_ID, sampleLocation, datetimeDenver, year, SO4_mgL)

lv_so4 <- lv_so4 %>%
  mutate(datetimeDenver = as.POSIXct(datetimeDenver)) %>%
  arrange(datetimeDenver)

lv_so4 <- lv_so4 %>%
  mutate(series = paste(lake_ID, sampleLocation, sep = "_"))

#monthly avg
lv_so4_monthly <- lv_so4 %>%
  mutate( month = month(datetimeDenver)) %>%
  group_by(lake_ID, sampleLocation, year, month) %>%
  summarise(SO4_mgL = mean(SO4_mgL, na.rm = TRUE),.groups = "drop") %>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"),
         date = as.Date(paste(year, month, "01", sep = "-")) )%>%
  arrange(date)

lv_so4_monthly$date <- as.Date(lv_so4_monthly$date)

lv_so4_monthly_wide <- lv_so4_monthly %>%
  select(date, site, SO4_mgL) %>%
  pivot_wider(names_from = date,
              values_from = SO4_mgL)

#daily
lv_so4_daily <- lv_so4 %>%
  arrange(datetimeDenver)%>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"))%>%
  mutate(date = as.Date(datetimeDenver))

lv_so4_daily$date <- as.Date(lv_so4_daily$date)


lv_so4_daily_wide <- lv_so4_daily %>%
  select(date, site, SO4_mgL) %>%
  pivot_wider(names_from = date,
              values_from = SO4_mgL)

#rearrange rows so it makes sense spatially
site_order <- c(
  "sky_in_n",
  "sky_in_s",
  "sky_ls",
  "sky_out",
  "andrewscreek_shr",
  "loch_in",
  "loch_ls",
  "loch_out")

lv_so4_monthly_wide<- lv_so4_monthly_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

lv_so4_daily_wide<- lv_so4_daily_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

#marss format
monthly_so4_matrix <- lv_so4_monthly_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()

daily_so4_matrix <- lv_so4_daily_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()




#sio2
lv_sio2 <- lv_wide %>%
  ungroup() %>%
  filter(lake_ID %in% c("sky", "loch", "andrewscreek")) %>%
  filter(!is.na(SiO2_mgL)) %>%
  distinct(lake_ID, sampleLocation, datetimeDenver, .keep_all = TRUE)%>%
  select(lake_ID, sampleLocation, datetimeDenver, year, SiO2_mgL)

lv_sio2 <- lv_sio2 %>%
  mutate(datetimeDenver = as.POSIXct(datetimeDenver)) %>%
  arrange(datetimeDenver)

lv_sio2 <- lv_sio2 %>%
  mutate(series = paste(lake_ID, sampleLocation, sep = "_"))

#monthly avg
lv_sio2_monthly <- lv_sio2 %>%
  mutate( month = month(datetimeDenver)) %>%
  group_by(lake_ID, sampleLocation, year, month) %>%
  summarise(SiO2_mgL = mean(SiO2_mgL, na.rm = TRUE),.groups = "drop") %>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"),
         date = as.Date(paste(year, month, "01", sep = "-")) )%>%
  arrange(date)

lv_sio2_monthly$date <- as.Date(lv_sio2_monthly$date)

lv_sio2_monthly_wide <- lv_sio2_monthly %>%
  select(date, site, SiO2_mgL) %>%
  pivot_wider(names_from = date,
              values_from = SiO2_mgL)

#daily
lv_sio2_daily <- lv_sio2 %>%
  arrange(datetimeDenver)%>%
  mutate(site = paste(lake_ID, sampleLocation, sep = "_"))%>%
  mutate(date = as.Date(datetimeDenver))

lv_sio2_daily$date <- as.Date(lv_sio2_daily$date)


lv_sio2_daily_wide <- lv_sio2_daily %>%
  select(date, site, SiO2_mgL) %>%
  pivot_wider(names_from = date,
              values_from = SiO2_mgL)

#rearrange rows so it makes sense spatially
site_order <- c(
  "sky_in_n",
  "sky_in_s",
  "sky_ls",
  "sky_out",
  "andrewscreek_shr",
  "loch_in",
  "loch_ls",
  "loch_out")

lv_sio2_monthly_wide<- lv_sio2_monthly_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

lv_sio2_daily_wide<- lv_sio2_daily_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)

#marss format
monthly_sio2_matrix <- lv_sio2_monthly_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()

daily_sio2_matrix <- lv_sio2_daily_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()


#N:P ratio?

#get dissolved N from NO3 and NH4, calc. N:P
lv_wide <- lv_wide %>%
  mutate(
    DIN_mgL = NO3_mgL + NH4_mgL,
    NP_molar = (DIN_mgL/14) / (PO4_mgL/31))

lv_wide %>%
  filter(lake_ID %in% c("loch", "sky", "andrewscreek")) %>%
  mutate(date = as.Date(datetimeDenver)) %>%
  group_by(lake_ID, date) %>%
  summarise(NP_molar = mean(NP_molar, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = NP_molar, color = lake_ID)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Mean N:P (molar)", x = "Date")

#low orthoP is blowing up the N:P ratio... maybe dont use this





#deposition marss matrix (this data is monthly) ---------------------------------------------------------------------
monthly_nadp_bret <- read.csv("data/mj_aslo/monthly_dep_lvws.csv")

bret_inorg_N <- monthly_nadp_bret %>%
  filter(deposition_name == "Inorganic N") %>%
  mutate( month_num = match(month, month.name),
    year = ifelse(month_num >= 10, waterYear - 1, waterYear),
    date = as.Date(paste(year, month_num, "01", sep = "-"))) %>%
  select(date, deposition_kg_per_ha_impute)


#add sites manually (repeat data) since this is just one nadp value for all of lvws
sites <- c( "sky_in_n", "sky_in_s", "sky_ls", "sky_out", "andrewscreek","loch_in", "loch_ls", "loch_out")
bret_inorg_N <- bret_inorg_N %>%
  tidyr::crossing(site = sites)  %>%
  group_by(site, date) %>%
  summarise(deposition = mean(deposition_kg_per_ha_impute), .groups = "drop")

bret_inorg_N$date <- as.Date(bret_inorg_N$date)

bret_inorg_n_matrix <- bret_inorg_N %>%
  pivot_wider(names_from = date,values_from = deposition) %>%
  column_to_rownames("site") %>%
  as.matrix()







#NADP deposition as another option
# base::load("data/mj_aslo/nadp.RData") #this is my gridmet derived variable from concentrations; seems to underestimate when compared to CO98 station deposition
# 
# nadp_plot <- nadp_wetdep %>%
#   mutate(date = as.Date(paste(year, month, "01", sep = "-")))%>%
#   filter(lake_ID %in% c("LOCH", "SKY"))
# ggplot(nadp_plot, aes(x = date, y = TIN_N_kg_ha, color = lake_ID)) +
#   geom_line() +
#   geom_point(size = 0.6) +
#   labs(
#     x = "Date",
#     y = "TIN (kg N/ha)",
#     color = "Lake ID"
#   ) +
#   theme_minimal()
# 
# sites <- tibble(
#   site = c("sky_in_n", "sky_in_s", "sky_ls", "sky_out","andrewscreek_shr", "loch_in", "loch_ls", "loch_out"),
#   lake_site = c("sky", "sky", "sky", "sky", "andrewscreek", "loch", "loch", "loch"))
# 
# nadp_tin_n_matrix <- sites %>%
#   left_join(nadp_wetdep %>%
#       mutate(lake_site = tolower(lake_ID),
#              date = as.Date(paste(year, month, "01", sep = "-"))) %>%
#       select(lake_site, date, TIN_N_kg_ha),by = "lake_site") %>%
#   select(site, date, TIN_N_kg_ha) %>%
#   pivot_wider(names_from = date, values_from = TIN_N_kg_ha) %>%
#   column_to_rownames("site") %>%
#   as.matrix()

#try using just the loch vale deposition estimates from CO98
 # co98 <- read.csv("data/mj_aslo/NTN-co98-m-s-kg_no_incompletes.csv")#downloaded without option "show incomplete data"
  co98 <- read.csv("data/mj_aslo/NTN-co98-m-i-kg.csv") #downloaded with option "show incomplete data"

#-9 is an error code, remove rows with -9's. 
co98 <- co98[ rowSums(co98[, c("Ca", "Mg", "K", "Na", "NH4", "NO3", "totalN", "Cl", "SO4")] < 0) == 0, ]

co98 <- co98 %>%
  mutate(date = ymd(paste(yr, seas, "01", sep = "-")))

#interpolate missing months
co98_interpolated <- co98 %>%
  complete(date = seq(min(date), max(date), by = "month")) %>%
  ungroup()

co98_interpolated <- co98_interpolated %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, date, na.rm = FALSE))) %>% #na.approx is linear interpolation
  ungroup()

co98_interpolated %>%
  ggplot(aes(x = date, y = totalN)) +
  geom_point() +
  theme_bw()


nadp_totalN_matrix <- co98_interpolated %>%
     select(date, totalN) %>%
    pivot_wider(names_from = date, values_from = totalN) %>%
    as.matrix()

nadp_sulfate_matrix <- co98_interpolated %>%
  select(date, SO4) %>%
  pivot_wider(names_from = date, values_from = SO4) %>%
  as.matrix()


#plot annual totals
annual_total_dep <- co98_interpolated %>%
  group_by(yr) %>%
  summarise(totalN_annual = sum(totalN, na.rm = TRUE),
            sulfate_annual = sum(SO4, na.rm = TRUE),
    .groups = "drop")
ggplot(annual_total_dep, aes(x = yr, y = totalN_annual)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Annual Total N") +
  theme_minimal()

#climate marss matrices---------------------------------------------------------------------

#gridmet first, until met station data is available
#for gridmet, sky/andrews creek share values and all the loch sites share values (2 groups)
base::load("data/mj_aslo/climate.RData") 


sites <- tibble(
  site = c("sky_in_n", "sky_in_s", "sky_ls", "sky_out","andrewscreek_shr", "loch_in", "loch_ls", "loch_out"),
  lake_site = c("sky", "sky", "sky", "sky", "andrewscreek", "loch", "loch", "loch"))

monthly_temp_matrix <- sites %>%
  left_join(monthly_climate %>%
      mutate(lake_site = tolower(lake_ID),
             date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      select(lake_site, date, monthly_mean_temp), by = "lake_site") %>%
  select(site, date, monthly_mean_temp) %>%
  pivot_wider(names_from = date, values_from = monthly_mean_temp) %>%
  column_to_rownames("site") %>%
  as.matrix()

daily_temp_matrix <- sites %>%
  left_join(daily_climate %>%
              mutate(lake_site = tolower(lake_ID),
                     mean_temp = (tmmn+tmmx)/2) %>%
              select(lake_site, date, mean_temp), by = "lake_site") %>%
  select(site, date, mean_temp) %>%
  pivot_wider(names_from = date, values_from = mean_temp) %>%
  column_to_rownames("site") %>%
  as.matrix()


pdsi_matrix <- sites %>%
  left_join(monthly_climate %>%
              mutate(lake_site = tolower(lake_ID),
                     date = as.Date(paste(year, month, "01", sep = "-"))) %>%
              select(lake_site, date, monthly_mean_pdsi), by = "lake_site") %>%
  select(site, date, monthly_mean_pdsi) %>%
  pivot_wider(names_from = date, values_from = monthly_mean_pdsi) %>%
  column_to_rownames("site") %>%
  as.matrix()

totalprecip_matrix <- sites %>%
  left_join(monthly_climate %>%
              mutate(lake_site = tolower(lake_ID),
                     date = as.Date(paste(year, month, "01", sep = "-"))) %>%
              select(lake_site, date, monthly_total_precip), by = "lake_site") %>%
  select(site, date, monthly_total_precip) %>%
  pivot_wider(names_from = date, values_from = monthly_total_precip) %>%
  column_to_rownames("site") %>%
  as.matrix()





#edits based on 4/9 meeting -------------------------------------------------------------------

#try a de-seasonalized and de-trended temperature anomaly -----------
  #do we have to de-trend, or will marss take care of this?
#check out forecast R package
#iao: look into STL (seasonal trend decomposition using loess), should help detect anomalies in the timeseries after deseasonalizing/detrending

library(forecast)
#use stl: Decompose a time series into seasonal, trend and irregular components using loess, acronym STL.

#shared temp across all sites -----

temp_ts_mean <- colMeans(monthly_temp_matrix, na.rm = TRUE)
temp_ts_mean <- ts(temp_ts_mean, frequency = 12, start = c(1981, 1))

# STL decomposition
stl_mean <- stl(temp_ts_mean, s.window = "periodic")

# anomaly = remainder
anomaly_shared_stl <- stl_mean$time.series[, "remainder"]

plot(stl_mean)

temp_anomaly_shared <- t(matrix(as.numeric(anomaly_shared_stl), ncol = 1))


#site-specific temp -----
n_time <- ncol(monthly_temp_matrix)

temp_anomaly_bysite <- matrix(NA, nrow = nrow(monthly_temp_matrix), ncol = n_time)
fitted_bysite  <- matrix(NA, nrow = nrow(monthly_temp_matrix), ncol = n_time)

for (i in seq_len(nrow(monthly_temp_matrix))) {
    y <- ts(monthly_temp_matrix[i, ], frequency = 12, start = c(1981, 1))
    stl_fit <- stl(y, s.window = "periodic")
  #save trend+ seasonal, and also remainder. use remainder as marss input
      fitted_bysite[i, ]  <- stl_fit$time.series[, "trend"] +
    stl_fit$time.series[, "seasonal"]
      temp_anomaly_bysite[i, ] <- stl_fit$time.series[, "remainder"]
}

#plot
stl_list <- lapply(seq_len(nrow(monthly_temp_matrix)), function(i) {
  ts_i <- ts(monthly_temp_matrix[i, ], frequency = 12, start = c(1981, 1))
  stl(ts_i, s.window = "periodic")})

plot(stl_list[[1]])  #there's really only 2 versions (not 8) since there's just 2 gridmet cells
plot(stl_list[[8]])



str(temp_anomaly_shared)
str(temp_anomaly_bysite)

colnames(temp_anomaly_shared) <- colnames(monthly_temp_matrix) 

colnames(temp_anomaly_bysite) <- colnames(monthly_temp_matrix) 
rownames(temp_anomaly_bysite) <- rownames(monthly_temp_matrix)
ncol(monthly_temp_matrix)
ncol(temp_anomaly_shared)
ncol(temp_anomaly_bysite)

#make a daily anomaly version

# shared 
temp_ts_mean_daily <- colMeans(daily_temp_matrix, na.rm = TRUE)

temp_ts_mean_daily <- ts(
  temp_ts_mean_daily,
  frequency = 365,
  start = c(1981, 1))


stl_mean_daily <- stl(temp_ts_mean_daily, s.window = "periodic")

anomaly_shared_stl_daily <- stl_mean_daily$time.series[, "remainder"]

temp_anomaly_shared_daily <- t(matrix(
  as.numeric(anomaly_shared_stl_daily),
  ncol = 1))

plot(stl_mean_daily)
str(temp_anomaly_shared_daily)

#site specific
n_time <- ncol(daily_temp_matrix)

temp_anomaly_bysite_daily <- matrix(NA, nrow = nrow(daily_temp_matrix), ncol = n_time)
fitted_bysite_daily <- matrix(NA, nrow = nrow(daily_temp_matrix), ncol = n_time)

for (i in seq_len(nrow(daily_temp_matrix))) {
  
  y <- ts(daily_temp_matrix[i, ],
          frequency = 365,
          start = c(1981, 1))
  
  stl_fit <- stl(y, s.window = "periodic")
  
  fitted_bysite_daily[i, ] <-
    stl_fit$time.series[, "trend"] +
    stl_fit$time.series[, "seasonal"]
  
  temp_anomaly_bysite_daily[i, ] <-
    stl_fit$time.series[, "remainder"]
}

stl_list_daily <- lapply(seq_len(nrow(daily_temp_matrix)), function(i) {
  ts(daily_temp_matrix[i, ], frequency = 365, start = c(1981, 1)) %>%
    stl(s.window = "periodic")
})

plot(stl_list_daily[[1]])
plot(stl_list_daily[[8]])
str(temp_anomaly_bysite_daily)


colnames(temp_anomaly_shared_daily) <- colnames(daily_temp_matrix) 

colnames(temp_anomaly_bysite_daily) <- colnames(daily_temp_matrix) 
rownames(temp_anomaly_bysite_daily) <- rownames(daily_temp_matrix)
ncol(daily_temp_matrix)
ncol(temp_anomaly_shared_daily)
ncol(temp_anomaly_bysite_daily)

# cumulative flow as a covariate--------------------------------------------------------
# cumulative flow on the DOY of a given sample, DOY starts oct 1
# see script 01.1 lochO 
#copied from 01.1 script:
library(dataRetrieval)
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
      
      
      # Look at the gaps in 2019, 2020
      # LochQ %>%
      #   filter(waterYear %in% c("2019","2020")) %>%
      #   ggplot(aes(x=date, y=Q_m3s))+
      #   geom_point()+
      #   facet_wrap(~waterYear, scales="free_x")
      # 
      # Need to look at how many NAs, and make a decision about gap filling for next step
      LochQ_tsbl <- as_tsibble(LochQ, key = waterYear) %>%
        fill_gaps() %>% #adds ~400 missing days
        mutate(Q_m3s = imputeTS::na_interpolation(Q_m3s, maxgap = 60))
      
      # Look at gaps in 2019, 2020 again
      # LochQ_tsbl %>%
      #   filter(waterYear %in% c("2019","2020")) %>%
      #   ggplot(aes(x=date, y=Q_m3s))+
      #   geom_point()+
      #   facet_wrap(~waterYear, scales="free_x")
      
      # Anything weird in the rest of the data?
      # LochQ_tsbl %>%
      #   # filter(waterYear %in% c("2019","2020")) %>%
      #   ggplot(aes(x=date, y=Q_m3s))+
      #   geom_point()+
      #   facet_wrap(~waterYear, scales="free_x")
      # Nope, looks good
      
      # calculate cumulative discharge for each year by first grouping by water year,
      # and then using the "cumsum" function. Add day of water year for plotting purposes.
      
      LochQ <- group_by(LochQ_tsbl, waterYear) %>%
        mutate(cumulative_dis = cumsum(Q_m3s), 
               wy_doy = hydro.day(date)) %>%
        filter(!waterYear %in% c("1983","2024"))
      
      
      percentile_days <- data.frame(LochQ_tsbl) %>%
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
      
      # # Check this works
      # percentile_days %>%
      #   ggplot(aes(x=waterYear, y=day_20th_wydoy))+
      #   geom_point()
      # 
      # percentile_days %>%
      #   ggplot(aes(x=waterYear, y=day_50th_wydoy))+
      #   geom_point()
      # 
      # percentile_days %>%
      #   ggplot(aes(x=waterYear, y=day_80th_wydoy))+
      #   geom_point()
      # 
      # ## Plot all 
      # percentile_days %>%
      #   pivot_longer(day_20th_wydoy:day_80th_wydoy) %>%
      #   ggplot(aes(x=waterYear, y=value, color=name))+
      #   geom_point()+theme_bw()
      
      
#turn it into a covariate matrix for marss
all_dates <- data.frame(date = seq(as.Date("1984-01-01"), as.Date("2023-12-31"), by = "day"))
all_dates <- all_dates %>%
  mutate(waterYear = ifelse(format(date, "%m-%d") >= "10-01",
                       as.numeric(format(date, "%Y")) + 1,
                       as.numeric(format(date, "%Y"))))
q50 <- all_dates %>%
  left_join(percentile_days %>%
              select(waterYear, day_50th), by = "waterYear")      
q50 <- q50 %>%
  mutate(day50_doy = as.numeric(date - day_50th))

q50_matrix <- t(q50$day50_doy)
colnames(q50_matrix) <- format(q50$date, "%Y-%m-%d")
ncol(q50_matrix)
ncol(daily_temp_matrix)

colnames(q50_matrix)[1]
colnames(q50_matrix)[ncol(q50_matrix)]
colnames(daily_temp_matrix)[1]
colnames(daily_temp_matrix)[ncol(daily_temp_matrix)]
q50_matrix <- as.matrix(q50_matrix)


#clip all to chem dataframe
#what are all my matrices called
ls()[sapply(ls(), function(x) is.matrix(get(x)))]

#clipping params
start_date <- as.Date("1984-01-01")
end_date   <- as.Date("2023-12-31")

monthly_no3_matrix <- monthly_no3_matrix[, as.Date(colnames(monthly_no3_matrix)) >= start_date & as.Date(colnames(monthly_no3_matrix)) <= end_date]
daily_no3_matrix <- daily_no3_matrix[, as.Date(colnames(daily_no3_matrix)) >= start_date & as.Date(colnames(daily_no3_matrix)) <= end_date]

monthly_so4_matrix <- monthly_so4_matrix[, as.Date(colnames(monthly_so4_matrix)) >= start_date & as.Date(colnames(monthly_so4_matrix)) <= end_date]
daily_so4_matrix <- daily_so4_matrix[, as.Date(colnames(daily_so4_matrix)) >= start_date & as.Date(colnames(daily_so4_matrix)) <= end_date]

monthly_sio2_matrix <- monthly_sio2_matrix[, as.Date(colnames(monthly_sio2_matrix)) >= start_date & as.Date(colnames(monthly_sio2_matrix)) <= end_date]
daily_sio2_matrix <- daily_sio2_matrix[, as.Date(colnames(daily_sio2_matrix)) >= start_date & as.Date(colnames(daily_sio2_matrix)) <= end_date]

nadp_totalN_matrix <- nadp_totalN_matrix[, as.Date(colnames(nadp_totalN_matrix)) >= start_date & as.Date(colnames(nadp_totalN_matrix)) <= end_date, drop=F]
bret_inorg_n_matrix <- bret_inorg_n_matrix[, as.Date(colnames(bret_inorg_n_matrix)) >= start_date & as.Date(colnames(bret_inorg_n_matrix)) <= end_date, drop=F]
nadp_sulfate_matrix <- nadp_sulfate_matrix[, as.Date(colnames(nadp_sulfate_matrix)) >= start_date & as.Date(colnames(nadp_sulfate_matrix)) <= end_date, drop=F]

monthly_temp_matrix <- monthly_temp_matrix[, as.Date(colnames(monthly_temp_matrix)) >= start_date & as.Date(colnames(monthly_temp_matrix)) <= end_date, drop=F]
daily_temp_matrix <- daily_temp_matrix[, as.Date(colnames(daily_temp_matrix)) >= start_date & as.Date(colnames(daily_temp_matrix)) <= end_date, drop=F]
temp_anomaly_bysite <- temp_anomaly_bysite[, as.Date(colnames(temp_anomaly_bysite)) >= start_date & as.Date(colnames(temp_anomaly_bysite)) <= end_date, drop=F]
temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily[, as.Date(colnames(temp_anomaly_bysite_daily)) >= start_date & as.Date(colnames(temp_anomaly_bysite_daily)) <= end_date, drop=F]

totalprecip_matrix <- totalprecip_matrix[, as.Date(colnames(totalprecip_matrix)) >= start_date & as.Date(colnames(totalprecip_matrix)) <= end_date, drop=F ] 

pdsi_matrix <- pdsi_matrix[, as.Date(colnames(pdsi_matrix)) >= start_date & as.Date(colnames(pdsi_matrix)) <= end_date, drop=F]

q50_matrix <- q50_matrix[, as.Date(colnames(q50_matrix)) >= start_date & as.Date(colnames(q50_matrix)) <= end_date, drop=F ]


#fill in date gaps
all_days   <- seq(as.Date("1984-01-01"), as.Date("2023-12-31"), by = "day")
all_months <- seq(as.Date("1984-01-01"), as.Date("2023-12-01"), by = "month")

#monthly data
monthly_no3_matrix <- monthly_no3_matrix[, match(all_months, as.Date(colnames(monthly_no3_matrix))), drop=F]
monthly_so4_matrix <- monthly_so4_matrix[, match(all_months, as.Date(colnames(monthly_so4_matrix))), drop=F]
monthly_sio2_matrix <- monthly_sio2_matrix[, match(all_months, as.Date(colnames(monthly_sio2_matrix))), drop=F]
nadp_totalN_matrix <- nadp_totalN_matrix[, match(all_months, as.Date(colnames(nadp_totalN_matrix))), drop=F]
nadp_sulfate_matrix <- nadp_sulfate_matrix[, match(all_months, as.Date(colnames(nadp_sulfate_matrix))), drop=F]
bret_inorg_n_matrix <- bret_inorg_n_matrix[, match(all_months, as.Date(colnames(bret_inorg_n_matrix))), drop=F]
monthly_temp_matrix <- monthly_temp_matrix[, match(all_months, as.Date(colnames(monthly_temp_matrix))), drop=F]
totalprecip_matrix <- totalprecip_matrix[, match(all_months, as.Date(colnames(totalprecip_matrix))), drop=F]
pdsi_matrix <- pdsi_matrix[, match(all_months, as.Date(colnames(pdsi_matrix))), drop=F]
temp_anomaly_bysite <- temp_anomaly_bysite[, match(all_months, as.Date(colnames(temp_anomaly_bysite))), drop=F]

#daily data
daily_no3_matrix <- daily_no3_matrix[, match(all_days, as.Date(colnames(daily_no3_matrix))), drop=F]
daily_so4_matrix <- daily_so4_matrix[, match(all_days, as.Date(colnames(daily_so4_matrix))), drop=F]
daily_sio2_matrix <- daily_sio2_matrix[, match(all_days, as.Date(colnames(daily_sio2_matrix))), drop=F]
daily_temp_matrix <- daily_temp_matrix[, match(all_days, as.Date(colnames(daily_temp_matrix))), drop=F]
temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily[, match(all_days, as.Date(colnames(temp_anomaly_bysite_daily))), drop=F]
q50_matrix <- q50_matrix[, match(all_days, as.Date(colnames(q50_matrix))), drop=F]

#gap fill NA dates
matrix_date_fill <- function(mat, full_dates) {
  idx <- match(full_dates, as.Date(colnames(mat)))
  mat <- mat[, idx, drop = FALSE]
  colnames(mat) <- as.character(full_dates)
  return(mat)
}

#monthly
monthly_no3_matrix      <- matrix_date_fill(monthly_no3_matrix, all_months)
monthly_so4_matrix      <- matrix_date_fill(monthly_so4_matrix, all_months)
monthly_sio2_matrix      <- matrix_date_fill(monthly_sio2_matrix, all_months)
nadp_totalN_matrix      <- matrix_date_fill(nadp_totalN_matrix, all_months)
nadp_sulfate_matrix      <- matrix_date_fill(nadp_sulfate_matrix, all_months)
bret_inorg_n_matrix     <- matrix_date_fill(bret_inorg_n_matrix, all_months)
monthly_temp_matrix     <- matrix_date_fill(monthly_temp_matrix, all_months)
totalprecip_matrix     <- matrix_date_fill(totalprecip_matrix, all_months)
pdsi_matrix     <- matrix_date_fill(pdsi_matrix, all_months)
temp_anomaly_bysite     <- matrix_date_fill(temp_anomaly_bysite, all_months)

#daily
daily_no3_matrix        <- matrix_date_fill(daily_no3_matrix, all_days)
daily_so4_matrix        <- matrix_date_fill(daily_so4_matrix, all_days)
daily_sio2_matrix        <- matrix_date_fill(daily_sio2_matrix, all_days)
daily_temp_matrix       <- matrix_date_fill(daily_temp_matrix, all_days)
temp_anomaly_bysite_daily       <- matrix_date_fill(temp_anomaly_bysite_daily, all_days)
q50_matrix       <- matrix_date_fill(q50_matrix, all_days)

# check
ncol(monthly_no3_matrix); ncol(monthly_so4_matrix);ncol(monthly_sio2_matrix);ncol(nadp_totalN_matrix);  ncol(nadp_sulfate_matrix);ncol(bret_inorg_n_matrix); ncol(monthly_temp_matrix); ncol(temp_anomaly_bysite); ncol(totalprecip_matrix); ncol(pdsi_matrix)
ncol(daily_no3_matrix); ncol(daily_temp_matrix); ncol(q50_matrix); ncol(temp_anomaly_bysite_daily)





# are there NAs in each covariate? should be Inf -Inf if no NA's (clunky but couldnt think of something slicker atm...)
range(which(is.na(nadp_totalN_matrix)))
range(which(is.na(nadp_sulfate_matrix)))
range(which(is.na(bret_inorg_n_matrix))) #this has NAs
range(which(is.na(monthly_temp_matrix)))
range(which(is.na(totalprecip_matrix)))
range(which(is.na(pdsi_matrix)))
range(which(is.na(daily_temp_matrix)))
range(which(is.na(temp_anomaly_bysite)))
range(which(is.na(temp_anomaly_bysite_daily)))
range(which(is.na(q50_matrix)))



colnames(monthly_no3_matrix)[1]; colnames(monthly_no3_matrix)[ncol(monthly_no3_matrix)]; ncol(monthly_no3_matrix)
colnames(monthly_so4_matrix)[1]; colnames(monthly_so4_matrix)[ncol(monthly_so4_matrix)]; ncol(monthly_so4_matrix)
colnames(monthly_sio2_matrix)[1]; colnames(monthly_sio2_matrix)[ncol(monthly_sio2_matrix)]; ncol(monthly_sio2_matrix)
colnames(nadp_totalN_matrix)[1]; colnames(nadp_totalN_matrix)[ncol(nadp_totalN_matrix)]; ncol(nadp_totalN_matrix)
colnames(nadp_sulfate_matrix)[1]; colnames(nadp_sulfate_matrix)[ncol(nadp_sulfate_matrix)]; ncol(nadp_sulfate_matrix)
colnames(bret_inorg_n_matrix)[1]; colnames(bret_inorg_n_matrix)[ncol(bret_inorg_n_matrix)]; ncol(bret_inorg_n_matrix)
colnames(monthly_temp_matrix)[1]; colnames(monthly_temp_matrix)[ncol(monthly_temp_matrix)]; ncol(monthly_temp_matrix)
colnames(totalprecip_matrix)[1]; colnames(totalprecip_matrix)[ncol(totalprecip_matrix)]; ncol(totalprecip_matrix)
colnames(pdsi_matrix)[1]; colnames(pdsi_matrix)[ncol(pdsi_matrix)]; ncol(pdsi_matrix)
colnames(temp_anomaly_bysite)[1]; colnames(temp_anomaly_bysite)[ncol(temp_anomaly_bysite)]; ncol(temp_anomaly_bysite)

colnames(daily_no3_matrix)[1]; colnames(daily_no3_matrix)[ncol(daily_no3_matrix)]; ncol(daily_no3_matrix)
colnames(temp_anomaly_bysite_daily)[1]; colnames(temp_anomaly_bysite_daily)[ncol(temp_anomaly_bysite_daily)]; ncol(temp_anomaly_bysite_daily)
colnames(q50_matrix)[1]; colnames(q50_matrix)[ncol(q50_matrix)]; ncol(q50_matrix)
colnames(daily_temp_matrix)[1]; colnames(daily_temp_matrix)[ncol(daily_temp_matrix)]; ncol(daily_temp_matrix)


# clip all the matrices to just the open water season ----------------------------------------
#standardize the colnames
colnames(bret_inorg_n_matrix)<-as.character(as.Date(colnames(bret_inorg_n_matrix)))
colnames(daily_no3_matrix)<-as.character(as.Date(colnames(daily_no3_matrix)))
colnames(daily_temp_matrix)<-as.character(as.Date(colnames(daily_temp_matrix)))
colnames(monthly_no3_matrix)<-as.character(as.Date(colnames(monthly_no3_matrix)))
colnames(monthly_so4_matrix)<-as.character(as.Date(colnames(monthly_so4_matrix)))
colnames(monthly_sio2_matrix)<-as.character(as.Date(colnames(monthly_sio2_matrix)))
colnames(monthly_temp_matrix)<-as.character(as.Date(colnames(monthly_temp_matrix)))
colnames(nadp_totalN_matrix)<-as.character(as.Date(colnames(nadp_totalN_matrix)))
colnames(nadp_sulfate_matrix)<-as.character(as.Date(colnames(nadp_sulfate_matrix)))
colnames(pdsi_matrix)<-as.character(as.Date(colnames(pdsi_matrix)))
colnames(q50_matrix)<-as.character(as.Date(colnames(q50_matrix)))
colnames(temp_anomaly_bysite)<-as.character(as.Date(colnames(temp_anomaly_bysite)))
colnames(temp_anomaly_bysite_daily)<-as.character(as.Date(colnames(temp_anomaly_bysite_daily)))
colnames(totalprecip_matrix)<-as.character(as.Date(colnames(totalprecip_matrix)))

start_openwater <- "06-01"
end_openwater   <- "10-01"

monthly_no3_matrix <- monthly_no3_matrix[, format(as.Date(colnames(monthly_no3_matrix)), "%m-%d") >= start_openwater &
                                           format(as.Date(colnames(monthly_no3_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

daily_no3_matrix <- daily_no3_matrix[, format(as.Date(colnames(daily_no3_matrix)), "%m-%d") >= start_openwater &
                                       format(as.Date(colnames(daily_no3_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

monthly_so4_matrix <- monthly_so4_matrix[, format(as.Date(colnames(monthly_so4_matrix)), "%m-%d") >= start_openwater &
                                           format(as.Date(colnames(monthly_so4_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

monthly_sio2_matrix <- monthly_sio2_matrix[, format(as.Date(colnames(monthly_sio2_matrix)), "%m-%d") >= start_openwater &
                                           format(as.Date(colnames(monthly_sio2_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

nadp_totalN_matrix <- nadp_totalN_matrix[, format(as.Date(colnames(nadp_totalN_matrix)), "%m-%d") >= start_openwater &
                                         format(as.Date(colnames(nadp_totalN_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

nadp_sulfate_matrix <- nadp_sulfate_matrix[, format(as.Date(colnames(nadp_sulfate_matrix)), "%m-%d") >= start_openwater &
                                         format(as.Date(colnames(nadp_sulfate_matrix)), "%m-%d") <= end_openwater, drop = FALSE]


bret_inorg_n_matrix <- bret_inorg_n_matrix[, format(as.Date(colnames(bret_inorg_n_matrix)), "%m-%d") >= start_openwater &
                                             format(as.Date(colnames(bret_inorg_n_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

monthly_temp_matrix <- monthly_temp_matrix[, format(as.Date(colnames(monthly_temp_matrix)), "%m-%d") >= start_openwater &
                                             format(as.Date(colnames(monthly_temp_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

daily_temp_matrix <- daily_temp_matrix[, format(as.Date(colnames(daily_temp_matrix)), "%m-%d") >= start_openwater &
                                         format(as.Date(colnames(daily_temp_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

temp_anomaly_bysite <- temp_anomaly_bysite[, format(as.Date(colnames(temp_anomaly_bysite)), "%m-%d") >= start_openwater &
                                             format(as.Date(colnames(temp_anomaly_bysite)), "%m-%d") <= end_openwater, drop = FALSE]

temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily[, format(as.Date(colnames(temp_anomaly_bysite_daily)), "%m-%d") >= start_openwater &
                                                         format(as.Date(colnames(temp_anomaly_bysite_daily)), "%m-%d") <= end_openwater, drop = FALSE]

totalprecip_matrix <- totalprecip_matrix[, format(as.Date(colnames(totalprecip_matrix)), "%m-%d") >= start_openwater &
                                           format(as.Date(colnames(totalprecip_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

pdsi_matrix <- pdsi_matrix[, format(as.Date(colnames(pdsi_matrix)), "%m-%d") >= start_openwater &
                             format(as.Date(colnames(pdsi_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

q50_matrix <- q50_matrix[, format(as.Date(colnames(q50_matrix)), "%m-%d") >= start_openwater &
                           format(as.Date(colnames(q50_matrix)), "%m-%d") <= end_openwater, drop = FALSE]

#take out sky in_n ------------------------------------------------------------------------
bret_inorg_n_matrix <- bret_inorg_n_matrix[rownames(bret_inorg_n_matrix) != "sky_in_n", , drop = FALSE]
daily_no3_matrix <- daily_no3_matrix[rownames(daily_no3_matrix) != "sky_in_n", , drop = FALSE]
daily_temp_matrix <- daily_temp_matrix[rownames(daily_temp_matrix) != "sky_in_n", , drop = FALSE]
monthly_no3_matrix <- monthly_no3_matrix[rownames(monthly_no3_matrix) != "sky_in_n", , drop = FALSE]
monthly_so4_matrix <- monthly_so4_matrix[rownames(monthly_so4_matrix) != "sky_in_n", , drop = FALSE]
monthly_sio2_matrix <- monthly_sio2_matrix[rownames(monthly_sio2_matrix) != "sky_in_n", , drop = FALSE]
monthly_temp_matrix <- monthly_temp_matrix[rownames(monthly_temp_matrix) != "sky_in_n", , drop = FALSE]
nadp_totalN_matrix <- nadp_totalN_matrix[rownames(nadp_totalN_matrix) != "sky_in_n", , drop = FALSE]
nadp_sulfate_matrix <- nadp_sulfate_matrix[rownames(nadp_sulfate_matrix) != "sky_in_n", , drop = FALSE]
pdsi_matrix <- pdsi_matrix[rownames(pdsi_matrix) != "sky_in_n", , drop = FALSE]
temp_anomaly_bysite <- temp_anomaly_bysite[rownames(temp_anomaly_bysite) != "sky_in_n", , drop = FALSE]
totalprecip_matrix <- totalprecip_matrix[rownames(totalprecip_matrix) != "sky_in_n", , drop = FALSE]



# are there NAs in each covariate? should be Inf -Inf if no NA's (clunky but couldnt think of something slicker atm...)
range(which(is.na(nadp_totalN_matrix)))
range(which(is.na(bret_inorg_n_matrix))) #this has NAs
range(which(is.na(monthly_temp_matrix)))
range(which(is.na(totalprecip_matrix)))
range(which(is.na(pdsi_matrix)))
range(which(is.na(daily_temp_matrix)))
range(which(is.na(temp_anomaly_bysite)))
range(which(is.na(temp_anomaly_bysite_daily)))
range(which(is.na(q50_matrix)))











#save all the matrices as ts_matrices.rdata file for easy loading into the marss script
# save all matrices into one .RData file
save(
  bret_inorg_n_matrix,
  nadp_totalN_matrix,
  nadp_sulfate_matrix,
  monthly_no3_matrix,
  monthly_so4_matrix,
  monthly_sio2_matrix,
  daily_no3_matrix,
  pdsi_matrix,
  daily_temp_matrix,
  monthly_temp_matrix,
  temp_anomaly_bysite,
  temp_anomaly_bysite_daily,
  totalprecip_matrix,
  q50_matrix,
  file = "data/mj_aslo/ts_matrices.RData")



#save data frames for gams 
save(
  lv_wide,
  bret_inorg_N,
  nadp_wetdep,
  monthly_climate,
  daily_climate,
  temp_anomaly_shared,
  temp_anomaly_bysite,
  temp_anomaly_bysite_daily,
  temp_anomaly_shared_daily,
  q50,
  file = "data/mj_aslo/ts_dfs.RData")

