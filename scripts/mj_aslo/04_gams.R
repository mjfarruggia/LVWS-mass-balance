#gams to complement marss model

library(tidyverse)
library(mgcv)
load('data/mj_aslo/ts_dfs.Rdata')


temp_anomaly_bysite <- as.data.frame(temp_anomaly_bysite)
temp_anomaly_shared <- as.data.frame(temp_anomaly_shared)
temp_anomaly_shared_daily <- as.data.frame(temp_anomaly_shared_daily)


temp_anomaly_bysite$lake_ID <- rownames(temp_anomaly_bysite)
temp_anomaly_bysite <- temp_anomaly_bysite %>%
  pivot_longer(cols = -lake_ID,
    names_to = "date",
    values_to = "temp_anomaly" )

temp_anomaly_shared$lake_ID <- rownames(temp_anomaly_shared)
temp_anomaly_shared <- temp_anomaly_shared %>%
  pivot_longer(cols = -lake_ID,
               names_to = "date",
               values_to = "temp_anomaly" )
temp_anomaly_shared$lake_ID <- NULL

temp_anomaly_shared_daily$lake_ID <- rownames(temp_anomaly_shared_daily)
temp_anomaly_shared_daily <- temp_anomaly_shared_daily %>%
  pivot_longer(cols = -lake_ID,
               names_to = "date",
               values_to = "temp_anomaly" )
temp_anomaly_shared_daily$lake_ID <- NULL



#clip all dfs to the same time frame
monthly_climate$date <- as.Date(paste(monthly_climate$year, monthly_climate$month, "01", sep = "-"))

nadp_wetdep$date <- as.Date(paste(nadp_wetdep$year, nadp_wetdep$month, "01", sep = "-"))
lv_wide$date <- as.Date(lv_wide$datetimeDenver)

lv_monthly <- lv_wide %>%
  mutate(date = as.Date(datetimeDenver),
    month = floor_date(date, "month")) %>%
  group_by(lake_ID, month) %>%
  summarise(across(.cols = temp_c:last_col(),.fns = ~ mean(.x, na.rm = TRUE)),
    .groups = "drop")
lv_monthly$date <- NULL
names(lv_monthly)[names(lv_monthly) == "month"] <- "date"

start_date <- as.Date("1984-01-01")
end_date   <- as.Date("2023-12-31")

clip_dates <- function(df) {
  if (!"date" %in% names(df)) return(df)
  df %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start_date, date <= end_date)
}

ls_names <- c(
  "bret_inorg_N",
  "daily_climate",
  "lv_wide",
  "lv_monthly",
  "monthly_climate",
  "nadp_wetdep",
  "q50",
  "temp_anomaly_bysite",
  "temp_anomaly_shared",
  "temp_anomaly_shared_daily"
)

# apply clipping
for (nm in ls_names) {
  obj <- get(nm, envir = .GlobalEnv)
  assign(nm, clip_dates(obj), envir = .GlobalEnv)
}


range(as.Date(daily_climate$date), na.rm = TRUE)
range(as.Date(q50$date), na.rm = TRUE)
range(as.Date(temp_anomaly_shared_daily$date), na.rm = TRUE)
range(as.Date(lv_wide$date), na.rm=TRUE)

range(as.Date(temp_anomaly_bysite$date), na.rm = TRUE)
range(as.Date(temp_anomaly_shared$date), na.rm = TRUE)
range(as.Date(monthly_climate$date), na.rm = TRUE)
range(as.Date(nadp_wetdep$date), na.rm = TRUE)
range(as.Date(lv_monthly$date), na.rm=TRUE)

#monthly df for gams
lv_monthly$lake_ID <- toupper(lv_monthly$lake_ID)
monthly_climate$lake_ID <- toupper(monthly_climate$lake_ID)
nadp_wetdep$lake_ID <- toupper(nadp_wetdep$lake_ID)

monthly_gam_df <- lv_monthly %>%
  left_join(monthly_climate, by = c("lake_ID", "date")) %>%
  left_join(nadp_wetdep, by = c("lake_ID", "date")) %>%
  left_join(temp_anomaly_shared, by = "date")

#daily df for gams
lv_wide$lake_ID <- toupper(lv_wide$lake_ID)
daily_climate$lake_ID <- toupper(daily_climate$lake_ID)


daily_gam_df <- lv_wide %>%
  left_join(daily_climate, by = c("lake_ID", "date")) %>%
  left_join(q50, by =  "date") %>%
  left_join(temp_anomaly_shared, by = "date") %>% ungroup()


#monthly gams
str(monthly_gam_df)
#drop the sites we don't need
monthly_gam_df <- monthly_gam_df %>%
  filter(!lake_ID %in% c("EMERALD", "FERN","GLASS","HAIYAHA", "HUSTED","LITTLELOCHCREEK","LOUISE","ODESSA","ROWEGLACIERTARN"))


summer_df <- monthly_gam_df %>%
  mutate(month = as.numeric(format(date, "%m"))) %>%
  filter(month %in% c(6, 7, 8, 9))
summer_summary <- summer_df %>%
  group_by(lake_ID, year) %>%
  summarise(
    NO3_mgL = mean(NO3_mgL, na.rm = TRUE),
    temp_anomaly = mean(temp_anomaly, na.rm = TRUE),
    summer_total_precip = sum(monthly_total_precip, na.rm = TRUE),
    TIN_N_kg_ha = mean(TIN_N_kg_ha, na.rm = TRUE),
    pdsi = mean(monthly_mean_pdsi),
    .groups = "drop"
  ) %>%
  filter(complete.cases(.))
summer_summary$lake_ID <- factor(summer_summary$lake_ID)

#the kitchen sink
gam1 <- gam(NO3_mgL ~ 
      s(temp_anomaly) +
      # s(summer_total_precip) + #should prob only do precip OR pdsi, not both
         s(pdsi)+ #pdsi gives model higher dev. than precip
      s(TIN_N_kg_ha) + 
      s(lake_ID, bs = "re"),
    data = summer_summary,
    family = Gamma(link = "log"),
    method = "REML")
plot(gam1, pages = 1, shade = TRUE)
library(gratia)
draw(gam1)
summary(gam1)

#ok one at a time
gam2 <- gam(NO3_mgL ~ 
              s(temp_anomaly) +
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")

draw(gam2)
summary(gam2)

gam3 <- gam(NO3_mgL ~ 
              s(summer_total_precip) +
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")

draw(gam3)
summary(gam3)

gam4 <- gam(NO3_mgL ~ 
              s(TIN_N_kg_ha) +
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")

draw(gam4)
summary(gam4)

gam5 <- gam(NO3_mgL ~ 
              s(pdsi) +
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")

draw(gam5)
summary(gam5)

gam6<- gam(NO3_mgL ~ 
              s(TIN_N_kg_ha) + s(pdsi)+
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")

draw(gam6)
summary(gam6)

gam7<- gam(NO3_mgL ~ 
             s(TIN_N_kg_ha) + s(summer_total_precip)+
             s(lake_ID, bs = "re"),
           data = summer_summary,
           family = Gamma(link = "log"),
           method = "REML")
draw(gam7)
summary(gam7)


gam8 <- gam(NO3_mgL ~ 
              s(TIN_N_kg_ha) + s(temp_anomaly)+ s(pdsi) +
              s(lake_ID, bs = "re"),
            data = summer_summary,
            family = Gamma(link = "log"),
            method = "REML")
draw(gam8)
summary(gam8)



#try some daily gams
#the kitchen sink
daily_gam_df2 <- daily_gam_df %>%
  filter(!is.na(NO3_mgL), !is.na(temp_anomaly))

gam_A <- gam(NO3_mgL ~ 
            s(day50_doy)+
              s(lake_ID, bs = "re"),
            data = daily_gam_df2,
            family = Gamma(link = "log"),
            method = "REML")
draw(gam_A)
summary(gam_A)





























## ---------------------------
##
## Project: FS Wilderness Lakes
## Repository: https://github.com/mtnlimnolab/FS-wilderness-lakes
## Script name: 09_gam_testing_individual_lakes.R
## Purpose of script: do model testing for each lake individually, then group lakes by similar models
## Authors: MJ Farruggia 
## Date Created: Mar 2025
##
## notes:
#individual lake GAM model testing
#first run model testing on each lake
#then group lakes by similar models (x lakes are all explained by y model which includes climate + dep parameters, or something like this)

# source("scripts/00_libraries.R")
base::load("data/mj_aslo/summer_lake_surface_chem.RData") 
base::load("data/mj_aslo/climate.RData")
base::load("data/mj_aslo/nadp.RData")

#chem data prep
#LVWS
lv <- read.csv("data/mj_aslo/LV_chemistry.csv")

#----data prep------------------------------------------------------------------------------
#since most samples are taken in the summer, can i check for some kind of seasonal sampling bias?

ggplot(lv, aes(x = month(date, label = TRUE), fill = factor(year))) +
  geom_bar(position = "dodge") +
  labs(title = "Monthly Sampling by Year", x = "Month", y = "Count", fill = "Year")+
  theme_bw()

unique(lv$sampleLocation)

#keep: sample_type=norm,  sampleLocation = "ls" or "shr" or "out" or "in" or "in_s" or "in_n", sample rep =1
#also keep summer only here
lv_filtered <- lv %>%
  filter(
    sampleType == "norm",
    month %in% 6:9, #summer only
    sampleLocation %in% c("ls", "shr", "out", "in",  "in_n"), 
    sampleReplicate == 1) 

lv_filtered$site_ID <- paste(lv_filtered$siteId, lv_filtered$sampleLocation, sep = "_")

lv_wide <- lv_filtered %>%
  pivot_wider(id_cols = c(site_ID, site, datetimeDenver, sampleLocation, lab),      
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
lv_wide$datetimeDenver <- NULL

summer_lake_surface_chem<- lv_wide %>%
  group_by(lake_ID, site_ID, year) %>%
  summarise(across(labcond_uScm:z_Fe_ugL, ~mean(. , na.rm = TRUE)), .groups = 'drop')%>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

lv_summer_lake_surface_chem <- summer_lake_surface_chem

#covaruate data prep------------------------------------------------------------------------------

#summarize nadp wet dep from monthly to water year totals
nadp_wetdep_wy_totals <- nadp_wetdep %>%
  mutate(water_year = ifelse(month >= 10, year + 1, year)) %>%
  group_by(lake_ID, nadp_siteId, water_year) %>%
  summarise(across(ends_with("kg_ha"), ~sum(.x, na.rm = TRUE), .names = "wy_{.col}"), .groups = "drop")%>%
  distinct()

#summarize nadp wet dep from monthly to summer totals
nadp_wetdep_summer_totals <- nadp_wetdep %>%
  filter(month %in% 6:9) %>%
  group_by(lake_ID, nadp_siteId, year) %>%
  summarise(across(ends_with("kg_ha"), ~sum(.x, na.rm = TRUE), .names = "smr_{.col}"), .groups = "drop")%>%
  distinct()

# LV nadp
lv_summer_lake_surface_chem$lake_ID <- toupper(lv_summer_lake_surface_chem$lake_ID)
lv_nadp_wetdep_wy_totals <- nadp_wetdep_wy_totals %>%
  filter(lake_ID%in% lv_summer_lake_surface_chem$lake_ID) 

lv_nadp_wetdep_summer_totals <- nadp_wetdep_summer_totals %>%
  filter(lake_ID%in% lv_summer_lake_surface_chem$lake_ID) 

lv_nadp_wetdep_summer_totals$nadp_siteId <- NULL
lv_nadp_wetdep_wy_totals$nadp_siteId <- NULL


summary(lv_summer_lake_surface_chem$SO4_mgL)
hist(lv_summer_lake_surface_chem$SO4_mgL)

#Bret Schichtel's loch vale dep 
bret_so4s <- annual_nadp_bret %>%
  filter(deposition_name == "SO4-S") %>%
  select(waterYear, SO4S_kg_ha = deposition_kg_per_ha_impute)

lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(bret_so4s, by = c("year" = "waterYear"))

bret_tin <- annual_nadp_bret %>%
  filter(deposition_name %in% c("NH4-N", "NO3-N")) %>%
  select(waterYear, deposition_name, deposition_kg_per_ha_impute) %>%
  pivot_wider(
    names_from = deposition_name,
    values_from = deposition_kg_per_ha_impute) %>%
  mutate(TIN_kg_ha_schichtel = `NH4-N` + `NO3-N`) %>%
  select(waterYear, TIN_kg_ha_schichtel)

lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(bret_tin, by = c("year" = "waterYear"))


#get summer climate means from daily climate data
daily_climate <- daily_climate %>%
  mutate(mean_temp = (tmmn + tmmx) / 2)

summer_climate <- daily_climate %>%
  filter(month(date) %in% 6:9) %>%  
  group_by(lake_ID, year(date)) %>%
  summarise(
    mean_tmmn = mean(tmmn, na.rm = TRUE),
    mean_tmmx = mean(tmmx, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    mean_pdsi = mean(pdsi, na.rm = TRUE),
    mean_pdsi_zindex = mean(pdsi_zindex, na.rm = TRUE),
    total_summer_precip = sum(precip, na.rm = TRUE),
    .groups = 'drop'
  )

names(summer_climate)[names(summer_climate) == "year(date)"] <- "year"


# LV climate
lv_summer_climate <- summer_climate %>%
  filter(lake_ID%in% lv_summer_lake_surface_chem$lake_ID) 


#add summer climate data to summer lake data
lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(lv_summer_climate, by = c("lake_ID", "year"))



#add deposition data to summer lake data
lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(lv_nadp_wetdep_wy_totals, by = c("lake_ID", "year" = "water_year"))



lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(lv_nadp_wetdep_summer_totals, by = c("lake_ID", "year"))



#add in water year total precip and test summer vs winter precip in the models       #new addition week of 7/7
lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  left_join(wateryear_climate %>% select(lake_ID, water_year, WY_total_precip, lag1_WY_total_precip, WY_totalprecip_2yr_mean), #only the water year total precip col for now...but this df has lagged precip, temp, and pdsi, could consider adding later (can also lag within gam)
            by = c("lake_ID", "year" = "water_year"))


ggplot(lv_summer_lake_surface_chem, aes(x = year, y = SO4_mgL, color = lake_ID)) +
  geom_line() +
  geom_point() +
  theme_bw() 

ggplot(lv_summer_lake_surface_chem, aes(x = lake_ID, y = SO4_mgL)) +
  geom_boxplot() +
  theme_bw() 

lake_year_counts <- lv_summer_lake_surface_chem %>%
  group_by(lake_ID) %>%
  summarise(n_years = n_distinct(year)) %>%
  arrange(desc(n_years))

#only keep marss sites
lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  filter(!lake_ID %in% c("EMERALD", "FERN","GLASS","HAIYAHA", "HUSTED","LITTLELOCHCREEK","LOUISE","ODESSA","ANDREWSTARN", "ROWEGLACIERTARN"))

#split by lake ID
unique(lv_summer_lake_surface_chem$site_ID)
lake_list <- split(lv_summer_lake_surface_chem, lv_summer_lake_surface_chem$site_ID)


# nitrate -----------------------------------------------------------------------------------------------------
unique(lv_summer_lake_surface_chem$lake_ID)
test_model <- gam(NO3_mgL ~  s(smr_TIN_N_kg_ha),
                  data = lv_summer_lake_surface_chem, 
                  subset = lake_ID == "LOCH", 
                  family = Gamma(link = "log"),
                  method = "REML")
summary(test_model)
plot.gam(test_model)
plot(test_model, trans = exp, shade = TRUE, seWithMean = TRUE)


#write out all the model combos for nitrate

#climate x dep
#testing out both WY precip totals vs. summer precip totals - indented for easy commenting out if desired



nitrate_models <- list(
  temp               = NO3_mgL ~ s(mean_temp),
  precip             = NO3_mgL ~ s(total_summer_precip),
  WYprecip           = NO3_mgL ~ s(WY_total_precip),
  temp_precip        = NO3_mgL ~ s(mean_temp) + s(total_summer_precip),
  temp_WYprecip        = NO3_mgL ~ s(mean_temp) + s(WY_total_precip),
  wetdep_TIN     = NO3_mgL ~ s(smr_TIN_N_kg_ha),
  temp_wetdep        = NO3_mgL ~ s(mean_temp) + s(smr_TIN_N_kg_ha),
  precip_wetdep      = NO3_mgL ~ s(total_summer_precip) + s(smr_TIN_N_kg_ha),
  WYprecip_wetdep      = NO3_mgL ~ s(WY_total_precip) + s(smr_TIN_N_kg_ha),
  temp_precip_wetdep = NO3_mgL ~ s(mean_temp) + s(total_summer_precip) + s(smr_TIN_N_kg_ha),
  temp_WYprecip_wetdep = NO3_mgL ~ s(mean_temp) + s(WY_total_precip) + s(smr_TIN_N_kg_ha),
  pdsi              = NO3_mgL ~ s(mean_pdsi),
  pdsi_wetdep              = NO3_mgL ~ s(mean_pdsi) + s(smr_TIN_N_kg_ha),
  
  laggedprecip1_lastyearonly        = NO3_mgL ~ s(lag1_WY_total_precip),
  laggedprecip1_thisyearprecip_WY         = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip),
  laggedprecip1_temp          = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip)+ s(mean_temp),
  laggedprecip1_wetdep        = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip)+ s(smr_TIN_N_kg_ha),
  laggedprecip1_temp_wetdep   = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip)+ s(mean_temp) + s(smr_TIN_N_kg_ha),
  
  twoyearmeanprecip           = NO3_mgL ~ s(WY_totalprecip_2yr_mean),
  twoyearmeanprecip_temp      = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(mean_temp),
  twoyearmeanprecip_wetdep    = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(smr_TIN_N_kg_ha),
  twoyearmeanprecip_temp_wetdep = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(mean_temp) + s(smr_TIN_N_kg_ha)
)



# list out all the outputs i want from each model
all_models <- list()
stats <- data.frame(
  lake_ID = character(),
  model = character(),
  term = character(),
  EDF = numeric(),
  F_statistic = numeric(),
  p_value = numeric(),
  Deviance_Explained = numeric(),
  R_squared = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)



#loop to run all models for all lakes and store the model outputs
for (site in names(lake_list)) {
  df <- lake_list[[site]]
  
  for (model_name in names(nitrate_models)) {
    modelformula <- nitrate_models[[model_name]]
    
    model <- try(gam(modelformula, 
                     family = Gamma(link = "log"), 
                     method = "REML", 
                     data = df, 
                     na.action = na.omit), silent = TRUE)
    if (inherits(model, "try-error")) {
      message("Model failed for ", site, " with ", model_name)
      next
    }    
    sm <- summary(model)
    dev_expl <- sm$dev.expl * 100
    r2 <- sm$r.sq 
    aic <- AIC(model)
    
    if (!is.null(sm$s.table) && nrow(sm$s.table) > 0) {
      smooth_terms <- as.data.frame(sm$s.table)
      smooth_terms$term <- rownames(smooth_terms)
      
      for (i in 1:nrow(smooth_terms)) {
        stats <- rbind(stats, data.frame(
          lake_ID = site,
          model = model_name,
          term = smooth_terms$term[i],
          EDF = smooth_terms$edf[i],
          F_statistic = smooth_terms$F[i],
          p_value = smooth_terms$`p-value`[i],
          Deviance_Explained = dev_expl,
          R_squared = r2,
          AIC = aic,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    all_models[[paste(site, model_name, sep = "_")]] <- model
  }
}


stats <- stats[order(stats$lake_ID, stats$AIC), ]

#write.csv(stats, "data/lv_nitrate_models_bysite.csv")

# #plot and DL smooths for when model = "year" 
# year_models <- all_models[grepl("_year$", names(all_models))]
# par(mfrow = c(1,1))
# for (nm in names(year_models)) {
#   m <- year_models[[nm]]
#     plot(m,select = 1, shade = TRUE, main = nm)
# }
# 
# ## Set the directory of where to put the plots
# out_dir <- here::here("plots", "time gams FS")
# 
# if (!dir.exists(out_dir)) {
#   dir.create(out_dir, recursive = TRUE)
# }
# # loop and save each plot
# for (nm in names(year_models)) {
#   
#   m <- year_models[[nm]]
#   
#   # clean filename (important)
#   file_name <- paste0(gsub("[^a-zA-Z0-9_]", "_", nm), ".png")
#   file_path <- file.path(out_dir, file_name)
#   
#   png(filename = file_path, width = 1200, height = 900, res = 150)
#   
#   par(mfrow = c(1,1))
#   
#   plot(m,
#        select = 1,
#        shade = TRUE,
#        main = nm)
#   
#   dev.off()
# }

# Filter stats to only include models with deviance explained < 90%
filtered_stats <- subset(stats, Deviance_Explained < 90)


#add linear trends to exlude lakes with opposite trends from deposition----------------
lake_list <- split(lv_summer_lake_surface_chem, lv_summer_lake_surface_chem$lake_ID)

lm_results <- lapply(names(lake_list), function(lake_name) {
  
  df <- lake_list[[lake_name]]
  
  model <- lm(SO4_mgL ~ year, data = df)
  sm <- summary(model)
  coefs <- sm$coefficients
  
  slope <- NA
  pval <- NA
  
  # only extract if year exists
  if ("year" %in% rownames(coefs)) {
    slope <- coefs["year", "Estimate"]
    pval  <- coefs["year", "Pr(>|t|)"]
  }
  
  data.frame(
    lake_ID = lake_name,
    slope = slope,
    p_value = pval,
    r_squared = sm$r.squared
  )
})

lm_results_df <- do.call(rbind, lm_results)

positive_lakes_nitrate <- lm_results_df %>%
  filter(p_value < 0.05 & slope > 0) %>%
  pull(lake_ID)

negative_lakes_nitrate <- lm_results_df %>%
  filter(p_value < 0.05 & slope < 0) %>%
  pull(lake_ID)

no_linear_trend_lakes_nitrate <- setdiff(lv_summer_lake_surface_chem$lake_ID, 
                                         c(positive_lakes_nitrate, negative_lakes_nitrate))

all_lakes <- lv_summer_lake_surface_chem %>%
  distinct(lake_ID) %>%
  as.data.frame()

colnames(all_lakes) <- "lake_ID"

all_lakes$nitrate <- ifelse(all_lakes$lake_ID %in% positive_lakes_nitrate, "positive",
                            ifelse(all_lakes$lake_ID %in% negative_lakes_nitrate, "negative",
                                   ifelse(all_lakes$lake_ID %in% no_linear_trend_lakes_nitrate, "no trend", NA)))

all_lakes_lineartrends <- all_lakes





filtered_stats <- filtered_stats %>%
  left_join(all_lakes_lineartrends %>% select(lake_ID, nitrate), by = "lake_ID")


filtered_stats2 <- filtered_stats %>%
  filter(
    !(grepl("wetdep", model) & nitrate != "negative")
  )



best_models_nitrate <- do.call(rbind, lapply(split(filtered_stats2, filtered_stats2$lake_ID), function(df) {
  #df[which.min(df$AIC), ]
  df[which.max(df$Deviance_Explained), ] #select highest dev exp instead of AIC just to see...
}))

#add a column to best models that categorizes it by climate or climate+dep
best_models_nitrate <- best_models_nitrate %>%
  mutate(
    model_type = case_when(
      str_detect(model, "dep") & str_detect(model, "pdsi|temp|precip|WYprecip") ~ "climate + dep",
      str_detect(model, "dep") ~ "dep only",
      str_detect(model, "pdsi|temp|precip|WYprecip") ~ "climate only"
    )
  )

best_models_nitrate <- best_models_nitrate %>%
  mutate(DE_above_30 = ifelse(Deviance_Explained > 30, TRUE, FALSE)) %>%
  mutate(DE_above_50 = ifelse(Deviance_Explained > 50, TRUE, FALSE))
































#split by lake ID
unique(monthly_gam_df$lake_ID)
lake_list <- split(monthly_gam_df, monthly_gam_df$lake_ID)


#write out all the model combos for nitrate

#climate x dep
nitrate_models <- list(
  temp_anomaly       = NO3_mgL ~ s(temp_anomaly),
  precip             = NO3_mgL ~ s(monthly_total_precip),
  temp_precip        = NO3_mgL ~ s(temp_anomaly) + s(monthly_total_precip),
  wetdep_TIN         = NO3_mgL ~ s(TIN_N_kg_ha),
  temp_wetdep        = NO3_mgL ~ s(temp_anomaly) + s(TIN_N_kg_ha),
  precip_wetdep      = NO3_mgL ~ s(monthly_total_precip) + s(TIN_N_kg_ha),
  temp_precip_wetdep = NO3_mgL ~ s(temp_anomaly) + s(monthly_total_precip) + s(TIN_N_kg_ha),
  pdsi              = NO3_mgL ~ s(monthly_mean_pdsi),
  pdsi_wetdep              = NO3_mgL ~ s(monthly_mean_pdsi) + s(TIN_N_kg_ha)
  # year                 = NO3_mgL ~ s(year)
)



# list out all the outputs i want from each model
all_models <- list()
stats <- data.frame(
  lake_ID = character(),
  model = character(),
  term = character(),
  EDF = numeric(),
  F_statistic = numeric(),
  p_value = numeric(),
  Deviance_Explained = numeric(),
  R_squared = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)



#loop to run all models for all lakes and store the model outputs
for (site in names(lake_list)) {
  df <- lake_list[[site]]
  
  for (model_name in names(nitrate_models)) {
    modelformula <- nitrate_models[[model_name]]
    
    model <- try(gam(modelformula, 
                     family = Gamma(link = "log"), 
                     method = "REML", 
                     data = df, 
                     na.action = na.omit), silent = TRUE)
    if (inherits(model, "try-error")) {
      message("Model failed for ", site, " with ", model_name)
      next
    }    
    sm <- summary(model)
    dev_expl <- sm$dev.expl * 100
    r2 <- sm$r.sq 
    aic <- AIC(model)
    
    if (!is.null(sm$s.table) && nrow(sm$s.table) > 0) {
      smooth_terms <- as.data.frame(sm$s.table)
      smooth_terms$term <- rownames(smooth_terms)
      
      for (i in 1:nrow(smooth_terms)) {
        stats <- rbind(stats, data.frame(
          lake_ID = site,
          model = model_name,
          term = smooth_terms$term[i],
          EDF = smooth_terms$edf[i],
          F_statistic = smooth_terms$F[i],
          p_value = smooth_terms$`p-value`[i],
          Deviance_Explained = dev_expl,
          R_squared = r2,
          AIC = aic,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    all_models[[paste(site, model_name, sep = "_")]] <- model
  }
}


stats <- stats[order(stats$lake_ID, stats$AIC), ]



