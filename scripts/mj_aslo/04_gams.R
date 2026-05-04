#gams to complement marss model

library(tidyverse)
library(mgcv)
load('data/mj_aslo/ts_dfs.Rdata')


temp_anomaly_bysite <- as.data.frame(temp_anomaly_bysite)
temp_anomaly_bysite_daily <- as.data.frame(temp_anomaly_bysite_daily)
temp_anomaly_shared <- as.data.frame(temp_anomaly_shared)
temp_anomaly_shared_daily <- as.data.frame(temp_anomaly_shared_daily)


temp_anomaly_bysite$lake_ID <- rownames(temp_anomaly_bysite)
temp_anomaly_bysite <- temp_anomaly_bysite %>%
  pivot_longer(cols = -lake_ID,
    names_to = "date",
    values_to = "temp_anomaly" )

temp_anomaly_bysite_daily$lake_ID <- rownames(temp_anomaly_bysite_daily)
temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily %>%
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


#load chem data

# source("scripts/00_libraries.R")
base::load("data/mj_aslo/summer_lake_surface_chem.RData") 
base::load("data/mj_aslo/climate.RData")
base::load("data/mj_aslo/nadp.RData")

#chem data prep
#LVWS
lv <- read.csv("data/mj_aslo/LV_chemistry.csv")
unique(lv$sampleLocation)

#keep: sample_type=norm,  sampleLocation = "ls" or "shr" or "out" or "in" or "in_n", sample rep =1
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

summer_lake_surface_chem<- lv_wide %>%
  group_by(datetimeDenver, lake_ID, site_ID, year) %>%
  summarise(across(labcond_uScm:z_Fe_ugL, ~mean(. , na.rm = TRUE)), .groups = 'drop')%>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

lv_summer_lake_surface_chem <- summer_lake_surface_chem
lv_summer_lake_surface_chem <- lv_summer_lake_surface_chem %>%
  mutate(date = as.Date(datetimeDenver)) %>%   
  select(-datetimeDenver)

#match all covariate dfs to the nitrate data
str(daily_climate)
str(nadp_wetdep)
str(q50)
str(temp_anomaly_bysite_daily)


lv_nitrate <- lv_summer_lake_surface_chem %>%
  select(lake_ID, site_ID, date, NO3_mgL) %>%
  mutate(lake_ID = tolower(lake_ID))

#drop the sites we don't need
lv_nitrate <- lv_nitrate %>%
  filter(!lake_ID %in% c("emerald", "fern","glass","haiyaha", "husted","littlelochcreek","louise","odessa","roweglaciertarn", "andrewstarn"))


daily_climate <- daily_climate %>%
  mutate(lake_ID = tolower(lake_ID),
    date = as.Date(date))

temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily %>%
  mutate(date = as.Date(date))
temp_anomaly_bysite_daily <- temp_anomaly_bysite_daily %>%
  mutate(lake_ID = recode(lake_ID,
                          "andrewscreek_shr" = "acr_shr",
                          "loch_in"          = "loc_in",
                          "loch_ls"          = "loc_ls",
                          "loch_out"         = "loc_out",
                          "sky_in_s"         = "sky_in_s",
                          "sky_ls"           = "sky_ls",
                          "sky_out"          = "sky_out"
  ))

nadp_wetdep <- nadp_wetdep %>%
  mutate(lake_ID = tolower(lake_ID))


lv_nitrate_joined <- lv_nitrate %>%
  mutate(year = year(date), month = month(date)) %>%
  left_join(daily_climate, by = c("lake_ID", "date")) %>%
  left_join(temp_anomaly_bysite_daily, by = c("site_ID" = "lake_ID", "date")) %>%
  left_join(nadp_wetdep %>% select(lake_ID, year, month, TIN_kg_ha, TIN_N_kg_ha),
            by = c("lake_ID", "year", "month")) %>%
  left_join(q50 %>% select(date, day50_doy), by = "date") %>%
  select(-year, -month)

lv_nitrate_joined <- lv_nitrate_joined %>%
  filter(date >= as.Date("1984-01-01"))

# #fill in pdsi
# lv_nitrate_joined <- lv_nitrate_joined %>%
#   group_by(lake_ID, year = year(date), month = month(date)) %>%
#   fill(pdsi, pdsi_zindex, .direction = "downup") %>%
#   ungroup() %>%
#   select(-year, -month)

# source("scripts/00_libraries.R")
base::load("data/mj_aslo/summer_lake_surface_chem.RData") 
base::load("data/mj_aslo/climate.RData")
base::load("data/mj_aslo/nadp.RData")

wateryear_climate<-wateryear_climate%>%
  mutate(lake_ID = tolower(lake_ID))
#add in water year total precip and test summer vs winter precip in the models       
lv_nitrate_joined <- lv_nitrate_joined %>%
  mutate(year = year(date))%>%
  left_join(wateryear_climate %>% select(lake_ID, water_year, WY_total_precip, lag1_WY_total_precip, WY_totalprecip_2yr_mean), #only the water year total precip col for now...but this df has lagged precip, temp, and pdsi, could consider adding later (can also lag within gam)
            by = c("lake_ID", "year" = "water_year"))

#drop LS samples for gams
lv_nitrate_joined <- lv_nitrate_joined %>%
  filter(!site_ID %in% c("loc_ls", "sky_ls"))


#split by lake ID
unique(lv_nitrate_joined$site_ID)
lake_list <- split(lv_nitrate_joined, lv_nitrate_joined$site_ID)


# nitrate -----------------------------------------------------------------------------------------------------
unique(lv_nitrate_joined$lake_ID)
test_model <- gam(NO3_mgL ~  s(TIN_N_kg_ha),
                  data = lv_nitrate_joined, 
                  subset = site_ID == "loc_in", 
                  family = Gamma(link = "log"),
                  method = "REML")
summary(test_model)
plot.gam(test_model)
plot(test_model, trans = exp, shade = TRUE, seWithMean = TRUE)


#write out all the model combos for nitrate

#climate x dep
#testing out both WY precip totals vs. summer precip totals - indented for easy commenting out if desired




nitrate_models <- list(
  temp                          = NO3_mgL ~ s(temp_anomaly),
  precip                        = NO3_mgL ~ s(precip),
  WYprecip                      = NO3_mgL ~ s(WY_total_precip),
  temp_precip                   = NO3_mgL ~ s(temp_anomaly) + s(precip),       
  temp_WYprecip                 = NO3_mgL ~ s(temp_anomaly) + s(WY_total_precip),
  wetdep_TIN                    = NO3_mgL ~ s(TIN_N_kg_ha),
  temp_wetdep                   = NO3_mgL ~ s(temp_anomaly) + s(TIN_N_kg_ha),
  precip_wetdep                 = NO3_mgL ~ s(precip) + s(TIN_N_kg_ha),        
  WYprecip_wetdep               = NO3_mgL ~ s(WY_total_precip) + s(TIN_N_kg_ha),
  temp_precip_wetdep            = NO3_mgL ~ s(temp_anomaly) + s(precip) + s(TIN_N_kg_ha), 
  temp_WYprecip_wetdep          = NO3_mgL ~ s(temp_anomaly) + s(WY_total_precip) + s(TIN_N_kg_ha),
  pdsi                          = NO3_mgL ~ s(pdsi),
  pdsi_wetdep                   = NO3_mgL ~ s(pdsi) + s(TIN_N_kg_ha),
  
  q50                           = NO3_mgL ~ s(day50_doy),
  q50_temp                      = NO3_mgL ~ s(day50_doy) + s(temp_anomaly),
  q50_wetdep                    = NO3_mgL ~ s(day50_doy) + s(TIN_N_kg_ha),
  q50_temp_wetdep               = NO3_mgL ~ s(day50_doy) + s(temp_anomaly) + s(TIN_N_kg_ha),
  q50_WYprecip                  = NO3_mgL ~ s(day50_doy) + s(WY_total_precip),
  q50_WYprecip_wetdep           = NO3_mgL ~ s(day50_doy) + s(WY_total_precip) + s(TIN_N_kg_ha),
  
  laggedprecip1_lastyearonly    = NO3_mgL ~ s(lag1_WY_total_precip),
  laggedprecip1_thisyear        = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip),
  laggedprecip1_temp            = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip) + s(temp_anomaly),  
  laggedprecip1_wetdep          = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip) + s(TIN_N_kg_ha),
  laggedprecip1_temp_wetdep     = NO3_mgL ~ s(lag1_WY_total_precip) + s(WY_total_precip) + s(temp_anomaly) + s(TIN_N_kg_ha),
  
  twoyearmeanprecip             = NO3_mgL ~ s(WY_totalprecip_2yr_mean),
  twoyearmeanprecip_temp        = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(temp_anomaly),
  twoyearmeanprecip_wetdep      = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(TIN_N_kg_ha), 
  twoyearmeanprecip_temp_wetdep = NO3_mgL ~ s(WY_totalprecip_2yr_mean) + s(temp_anomaly) + s(TIN_N_kg_ha)

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


# #add linear trends to exlude lakes with opposite trends from deposition----------------
# lake_list <- split(lv_nitrate_joined, lv_nitrate_joined$site_ID)
# 
# lm_results <- lapply(names(lake_list), function(lake_name) {
#   
#   df <- lake_list[[lake_name]]
#   
#   model <- lm(NO3_mgL ~ year, data = df)
#   sm <- summary(model)
#   coefs <- sm$coefficients
#   
#   slope <- NA
#   pval <- NA
#   
#   # only extract if year exists
#   if ("year" %in% rownames(coefs)) {
#     slope <- coefs["year", "Estimate"]
#     pval  <- coefs["year", "Pr(>|t|)"]
#   }
#   
#   data.frame(
#     lake_ID = lake_name,
#     slope = slope,
#     p_value = pval,
#     r_squared = sm$r.squared
#   )
# })
# 
# lm_results_df <- do.call(rbind, lm_results)
# 
# positive_lakes_nitrate <- lm_results_df %>%
#   filter(p_value < 0.05 & slope > 0) %>%
#   pull(lake_ID)
# 
# negative_lakes_nitrate <- lm_results_df %>%
#   filter(p_value < 0.05 & slope < 0) %>%
#   pull(lake_ID)
# 
# no_linear_trend_lakes_nitrate <- setdiff(lv_nitrate_joined$lake_ID, 
#                                          c(positive_lakes_nitrate, negative_lakes_nitrate))
# 
# all_lakes <- lv_nitrate_joined %>%
#   distinct(site_ID) %>%
#   as.data.frame()
# 
# colnames(all_lakes) <- "lake_ID"
# 
# all_lakes$nitrate <- ifelse(all_lakes$lake_ID %in% positive_lakes_nitrate, "positive",
#                             ifelse(all_lakes$lake_ID %in% negative_lakes_nitrate, "negative",
#                                    ifelse(all_lakes$lake_ID %in% no_linear_trend_lakes_nitrate, "no trend", NA)))
# 
# all_lakes_lineartrends <- all_lakes
# 
# 
# 
# 
# 
# filtered_stats <- filtered_stats %>%
#   left_join(all_lakes_lineartrends %>% select(lake_ID, nitrate), by = "lake_ID")
# 
# 
# filtered_stats2 <- filtered_stats %>%
#   filter(
#     !(grepl("wetdep", model) & nitrate != "negative")
#   )



best_models_nitrate <- do.call(rbind, lapply(split(stats, stats$lake_ID), function(df) {
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





library(patchwork)
## IAO addition:
## Pulling out the best models and then exporting the smooths
selected_keys <- best_models_nitrate %>% 
  mutate(lake_model = paste(lake_ID, model, sep = "_")) %>% 
  pull(lake_model)

## Set the directory of where two put the plots
out_dir <- here::here("plots", "nitrate GAMs")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}


## Auto batch loop with 1 model per page
n_per_page <- 1
n_batches  <- ceiling(length(selected_keys) / n_per_page)

for (batch_id in seq_len(n_batches)) {
  
  keys_subset <- selected_keys[
    ((batch_id - 1) * n_per_page + 1) :
      min(batch_id * n_per_page, length(selected_keys))
  ]
  
  key <- keys_subset[1]
  safe_name <- gsub("[^A-Za-z0-9_]", "_", key)
  
  plots <- lapply(keys_subset, function(key) {
    mod <- all_models[[key]]
    if (inherits(mod, "gam")) {
      gratia::draw(mod)
    } else {
      NULL
    }
  })
  
  plots <- Filter(Negate(is.null), plots)
  
  p <- wrap_plots(plots, ncol = 1) +
    patchwork::plot_annotation(title = key)
  
  ggsave(
    filename = here::here("plots", "nitrate GAMs",
                          paste0("GAM_", safe_name, ".png")),
    plot     = p,
    width    = 8,
    height   = 6,
    units    = "in",
    dpi = 300
  )
}





























