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


ggplot(lv_filtered, aes(x = month(date, label = TRUE), fill = factor(year))) +
  geom_bar(position = "dodge") +
  labs(title = "Monthly Sampling by Year", x = "Month", y = "Count", fill = "Year")+
  facet_wrap(site~sampleLocation, scales = "free_y") +
  theme_bw()


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
    date = as.Date(paste(year, month, "01", sep = "-")) )

lv_no3_wide <- lv_no3_monthly %>%
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

lv_no3_wide<- lv_no3_wide %>%
  mutate(site = factor(site, levels = site_order)) %>%
  arrange(site)


#marss format
no3_matrix <- lv_no3_wide %>%
  column_to_rownames("site") %>%  
  as.matrix()



#deposition marss matrix---------------------------------------------------------------------
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


bret_inorg_n_matrix <- bret_inorg_N %>%
  pivot_wider(names_from = date,values_from = deposition) %>%
  column_to_rownames("site") %>%
  as.matrix()







#NADP deposition as another option
base::load("data/mj_aslo/nadp.RData") 


sites <- tibble(
  site = c("sky_in_n", "sky_in_s", "sky_ls", "sky_out","andrewscreek_shr", "loch_in", "loch_ls", "loch_out"),
  lake_site = c("sky", "sky", "sky", "sky", "andrewscreek", "loch", "loch", "loch"))

nadp_tin_n_matrix <- sites %>%
  left_join(nadp_wetdep %>%
      mutate(lake_site = tolower(lake_ID),
             date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      select(lake_site, date, TIN_N_kg_ha),by = "lake_site") %>%
  select(site, date, TIN_N_kg_ha) %>%
  pivot_wider(names_from = date, values_from = TIN_N_kg_ha) %>%
  column_to_rownames("site") %>%
  as.matrix()


#climate marss matrices---------------------------------------------------------------------

#gridmet first, until met station data is available
#for gridmet, sky/andrews creek share values and all the loch sites share values (2 groups)
base::load("data/mj_aslo/climate.RData") 


sites <- tibble(
  site = c("sky_in_n", "sky_in_s", "sky_ls", "sky_out","andrewscreek_shr", "loch_in", "loch_ls", "loch_out"),
  lake_site = c("sky", "sky", "sky", "sky", "andrewscreek", "loch", "loch", "loch"))

temp_matrix <- sites %>%
  left_join(monthly_climate %>%
      mutate(lake_site = tolower(lake_ID),
             date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      select(lake_site, date, monthly_mean_temp), by = "lake_site") %>%
  select(site, date, monthly_mean_temp) %>%
  pivot_wider(names_from = date, values_from = monthly_mean_temp) %>%
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








#try a de-seasonalized and de-trended temperature anomaly 
  #do we have to de-trend, or will marss take care of this?
#check out forecast R package
#iao: look into STL (seasonal trend decomposition using loess), should help detect anomalies in the timeseries after deseasonalizing/detrending

library(forecast)
#use stl: Decompose a time series into seasonal, trend and irregular components using loess, acronym STL.

#shared temp across all sites -----

temp_ts_mean <- colMeans(temp_matrix, na.rm = TRUE)
temp_ts_mean <- ts(temp_ts_mean, frequency = 12, start = c(1981, 1))

# STL decomposition
stl_mean <- stl(temp_ts_mean, s.window = "periodic")

# anomaly = remainder
anomaly_shared_stl <- stl_mean$time.series[, "remainder"]

plot(stl_mean)

temp_anomaly_shared <- t(matrix(as.numeric(anomaly_shared_stl), ncol = 1))


#site-specific temp -----
n_time <- ncol(temp_matrix)

temp_anomaly_bysite <- matrix(NA, nrow = nrow(temp_matrix), ncol = n_time)
fitted_bysite  <- matrix(NA, nrow = nrow(temp_matrix), ncol = n_time)

for (i in seq_len(nrow(temp_matrix))) {
    y <- ts(temp_matrix[i, ], frequency = 12, start = c(1981, 1))
    stl_fit <- stl(y, s.window = "periodic")
  #save trend+ seasonal, and also remainder. use remainder as marss input
      fitted_bysite[i, ]  <- stl_fit$time.series[, "trend"] +
    stl_fit$time.series[, "seasonal"]
      temp_anomaly_bysite[i, ] <- stl_fit$time.series[, "remainder"]
}

#plot
stl_list <- lapply(seq_len(nrow(temp_matrix)), function(i) {
  ts_i <- ts(temp_matrix[i, ], frequency = 12, start = c(1981, 1))
  stl(ts_i, s.window = "periodic")})

plot(stl_list[[1]])  #there's really only 2 versions (not 8) since there's just 2 gridmet cells
plot(stl_list[[8]])



str(temp_anomaly_shared)
str(temp_anomaly_bysite)

colnames(temp_anomaly_shared) <- colnames(temp_matrix) 

colnames(temp_anomaly_bysite) <- colnames(temp_matrix) 
rownames(temp_anomaly_bysite) <- rownames(temp_matrix)
ncol(temp_matrix)
ncol(temp_anomaly_shared)
ncol(temp_anomaly_bysite)


#save all the matrices as ts_matrices.rdata file for easy loading into the marss script
# save all matrices into one .RData file
save(
  bret_inorg_n_matrix,
  nadp_tin_n_matrix,
  no3_matrix,
  pdsi_matrix,
  temp_matrix,
  temp_anomaly_bysite,
  temp_anomaly_shared,
  totalprecip_matrix,
  file = "data/mj_aslo/ts_matrices.RData")
