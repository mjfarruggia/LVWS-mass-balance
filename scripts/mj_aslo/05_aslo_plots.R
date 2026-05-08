# covariate plots for aslo talk


#ggdark package seems to be broken. set plot theme here:
dark_theme <- function(base_size = 12, base_family = "") {
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "gray20"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white", size = base_size),
    axis.title = element_text(color = "white", size = base_size + 2),
    legend.text = element_text(color = "white", size = base_size),
    legend.title = element_text(color = "white", size = base_size + 2),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    axis.title.y = element_text(color = "white", size = base_size),  
    axis.title.x = element_text(color = "white", size = base_size ),  
    plot.title = element_text(color = "white", size = base_size + 4), 
    strip.background = element_rect(fill = "gray20"),      
    strip.text = element_text(color = "white", size = base_size -6 ),
    text = element_text(family = base_family))
}


library(tidyverse)
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
  left_join(nadp_wetdep %>% select(lake_ID, year, month, TIN_N_kg_ha),
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




#daily climate
#nadp_tin_n_matrix


#mean summer temp
lv_summer_climate_subset <- lv_summer_climate %>%
  filter(lake_ID %in% c("SKY", "LOCH")) %>%
  mutate(lake_ID = case_when(
    lake_ID == "SKY" ~ "Upper",
    lake_ID == "LOCH" ~ "Lower",
    TRUE ~ lake_ID),
    lake_ID = factor(lake_ID, levels = c("Upper", "Lower")))


mean_temp_plot <- ggplot(lv_summer_climate_subset,
       aes(x = year, y = mean_temp, color = lake_ID)) +
    geom_line(aes(group = lake_ID), alpha = 0.7) +
    geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(y = "Summer Mean Air Temp °C", x = "Year", color = "Watershed Position") +
    scale_color_manual(values = c(
    "Upper" = "purple",
    "Lower" = "dodgerblue" )) +
    dark_theme(base_size = 18)+
  theme(legend.position = "bottom")
mean_temp_plot


#total WY precip

annual_subset <- annual_climate %>%
  filter(lake_ID %in% c("SKY", "LOCH")) %>%
  mutate(
    lake_ID = case_when(
      lake_ID == "SKY" ~ "Upper",
      lake_ID == "LOCH" ~ "Lower",
      TRUE ~ lake_ID),
    lake_ID = factor(lake_ID, levels = c("Upper", "Lower")))

wyprecip_plot <- ggplot(annual_subset, aes(year, annual_total_precip, color = lake_ID)) +
  geom_line(aes(group = lake_ID), alpha = 0.7) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  scale_color_manual(values = c("Upper" = "purple",
                                "Lower" = "dodgerblue")) +
  labs(x = "Year", y = "Total Water Year Precip (mm)", color = "Watershed Position") +
  dark_theme(base_size = 18) +
  theme(legend.position = "bottom")
wyprecip_plot

library(cowplot)
cowplot::plot_grid(mean_temp_plot, wyprecip_plot, ncol=2)


#deposition
str(nadp_totalN_matrix)

nadp_long <- as.data.frame(nadp_totalN_matrix) %>%
  mutate(lake_ID = rownames(nadp_totalN_matrix)) %>%
  filter(lake_ID %in% c("sky_out", "loch_out")) %>%
  pivot_longer(-lake_ID, names_to = "date", values_to = "totalN") %>%
    mutate(
    date = as.Date(date),
    lake_ID = case_when(
      lake_ID == "sky_out" ~ "Upper",
      lake_ID == "loch_out" ~ "Lower"),
    lake_ID = factor(lake_ID, levels = c("Upper", "Lower")))

nadp_annual <- nadp_long %>%
  mutate(yr = year(date)) %>%
  group_by(lake_ID, yr) %>%
  summarise(totalN_annual = sum(totalN, na.rm = TRUE), .groups = "drop")

nadp_annual_plot <- ggplot(nadp_annual, aes(yr, totalN_annual, color = lake_ID)) +
  geom_line(alpha = 0.8) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(x = "Year", y = "Annual Total N deposition", color = "Watershed Position") +
  scale_color_manual(values = c(
    "Upper" = "purple",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18) +
  theme(legend.position = "none",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
nadp_annual_plot



nadp_annual_sulfate <- nadp_wetdep %>%
  group_by(lake_ID, year) %>%
  summarise(sulfate_annual = sum(SO4_kg_ha, na.rm = TRUE), .groups = "drop")

nadp_annual_sulfate <- nadp_annual_sulfate %>%
  filter(lake_ID %in% c( "LOCH")) %>%
  mutate(
    lake_ID = case_when(
      lake_ID == "LOCH" ~ "Lower",
      TRUE ~ lake_ID))

sulfate_annual_plot<- ggplot(nadp_annual_sulfate, aes(year, sulfate_annual, color = lake_ID)) +
  geom_line(alpha = 0.8) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(x = "Year", y = "Annual Total Sulfate deposition", color = "Watershed Position") +
  scale_color_manual(values = c(
    "Upper" = "purple",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18) +
  theme(legend.position = "none",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
sulfate_annual_plot

cowplot::plot_grid(nadp_annual_plot, sulfate_annual_plot, ncol=2)

nadp_lower <- as.data.frame(nadp_tin_n_matrix) %>%
  mutate(lake_ID = rownames(nadp_tin_n_matrix)) %>%
  filter(lake_ID == "loch_out") %>%
  pivot_longer(-lake_ID, names_to = "date", values_to = "tin_n") %>%
  mutate(date = as.Date(date))

ggplot(nadp_lower, aes(date, tin_n)) +
  geom_point(color="dodgerblue", alpha = 0.5) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(x = "Date", y = "TIN deposition") +
 dark_theme(base_size = 18)


#chem
loch_chem <- summer_lake_surface_chem %>%
  filter(lake_ID %in% c("sky", "loch", 'andrewscreek')) %>%
  mutate(lake_ID = case_when(
    grepl("sky", lake_ID, ignore.case = TRUE) ~ "Upper S.",
    grepl("loch", lake_ID, ignore.case = TRUE) ~ "Lower",
    grepl("andrewscreek", lake_ID, ignore.case = TRUE) ~ "Upper N.")) %>%
  mutate(lake_ID = factor(lake_ID, levels = c("Upper S.", "Upper N.","Lower")))

loch_chem_summermean <- loch_chem %>%
  mutate(year = year(datetimeDenver)) %>%   
  group_by(year, lake_ID, site_ID) %>%      
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)),
    .groups = "drop")


nitrate <- ggplot(loch_chem_summermean, aes(year, NO3_mgL, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(x = "Year",y = expression(NO[3]~"(mg/L)"), color = "Watershed\nPosition") +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N."   = "forestgreen",
    "Lower" = "dodgerblue")) +
  dark_theme(base_size = 18) +
  theme(legend.position = "bottom",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
nitrate

nitratefacet <- ggplot(loch_chem_summermean, aes(year, NO3_mgL, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "loess", linewidth = 1.2, se = TRUE) +
  facet_wrap(~lake_ID, ncol=1) +
  labs(
    x = "Year",
    y = expression(NO[3]~"(mg/L)"),
    color = "Watershed\nPosition"
  ) +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N." = "forestgreen",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18) 
nitratefacet

lake_colors <- c(
  "Upper S." = "purple",
  "Upper N." = "forestgreen",
  "Lower" = "dodgerblue"
)

nitrate_sky <- loch_chem_summermean %>%
  filter(lake_ID == "Upper S.") %>%
  ggplot(aes(year, NO3_mgL)) +
  geom_point(color = "purple", alpha = 0.8, size = 1) +
  geom_smooth(color = "purple", method = "loess", linewidth = 1.2, se = TRUE) +
  labs( x = "Year", y = expression(NO[3]~"(mg/L)")) +
  dark_theme(base_size = 18)
nitrate_sky

nitrate_andrews <- loch_chem_summermean %>%
  filter(lake_ID == "Upper N.") %>%
  ggplot(aes(year, NO3_mgL)) +
  geom_point(color = "forestgreen", alpha = 0.8, size = 1) +
  geom_smooth(color = "forestgreen", method = "loess", linewidth = 1.2, se = TRUE) +
  labs(x = "Year", y = expression(NO[3]~"(mg/L)")) +
  dark_theme(base_size = 18)
nitrate_andrews

nitrate_loch <- loch_chem_summermean %>%
  filter(lake_ID == "Lower") %>%
  ggplot(aes(year, NO3_mgL)) +
  geom_point(color = "dodgerblue", alpha = 0.8, size = 1) +
  geom_smooth(color = "dodgerblue", method = "loess", linewidth = 1.2, se = TRUE) +
  labs( x = "Year", y = expression(NO[3]~"(mg/L)")) +
  dark_theme(base_size = 18)

sulfate <- ggplot(loch_chem_summermean, aes(year, SO4_mgL, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "loess", linewidth = 1.2, se = T) +
  labs(x = "Year",y = expression(SO[4]~"(mg/L)"), color = "Watershed\nPosition") +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N."   = "forestgreen",
    "Lower" = "dodgerblue")) +
  dark_theme(base_size = 18) +
  theme(legend.position = "bottom",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
sulfate


so4_sky <- loch_chem_summermean %>%
  filter(lake_ID == "Upper S.") %>%
  ggplot(aes(year, SO4_mgL)) +
  geom_point(color = "purple", alpha = 0.8, size = 1) +
  geom_smooth(color = "purple", method = "loess", linewidth = 1.2, se = TRUE) +
  labs(x = "Year", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)

so4_andrews <- loch_chem_summermean %>%
  filter(lake_ID == "Upper N.") %>%
  ggplot(aes(year, SO4_mgL)) +
  geom_point(color = "forestgreen", alpha = 0.8, size = 1) +
  geom_smooth(color = "forestgreen", method = "loess", linewidth = 1.2, se = TRUE) +
  labs(x = "Year", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)

so4_loch <- loch_chem_summermean %>%
  filter(lake_ID == "Lower") %>%
  ggplot(aes(year, SO4_mgL)) +
  geom_point(color = "dodgerblue", alpha = 0.8, size = 1) +
  geom_smooth(color = "dodgerblue", method = "loess", linewidth = 1.2, se = TRUE) +
  labs(x = "Year", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)



silica<- ggplot(loch_chem, aes(year, SiO2_mgL, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "gam", linewidth = 1.2, se = T) +
  labs(x = "Year",y = expression(SiO[2]~"(mg/L)"), color = "Watershed\nPosition") +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N."   = "forestgreen",
    "Lower" = "dodgerblue")) +
  dark_theme(base_size = 18) +
  theme(legend.position = "bottom",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
silica



loch_chem <- loch_chem %>%
  mutate(
    N_mol = NO3_mgL / 14.007,
    P_mol = PO4_mgL / 30.974,
    NP_ratio = N_mol / P_mol)

loch_chem <- loch_chem %>%
  mutate(NP_ratio = ifelse(P_mol > 0 & !is.na(P_mol), N_mol / P_mol, NA))

np_ratio <- ggplot(loch_chem, aes(year, NP_ratio, color = lake_ID)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "gam", linewidth = 1.2, se = TRUE) +
  labs(x = "Year", y = "N:P", color = "Watershed\nPosition") +
  scale_color_manual(values = c(Upper="purple", Mid="forestgreen", Lower="dodgerblue")) +
  dark_theme(base_size = 18) +
  theme(legend.position = "bottom",legend.title = element_text(size = 10),legend.text  = element_text(size = 9))
np_ratio

cowplot::plot_grid(nitrate, sulfate,  ncol=1)





#weathering

#ca mg
loch_chem %>%
  mutate(Ca_Mg_mgL = Ca_mgL + Mg_mgL) %>%
  ggplot(aes(year, Ca_Mg_mgL, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "gam", linewidth = 1.2, se = TRUE) +
  labs(
    x = "Year",
    y = "Ca + Mg (mg/L)",
    color = "Watershed\nPosition"
  ) +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N." = "forestgreen",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


#base cations
loch_chem %>%
  mutate(base_cations = Mg_mgL + Na_mgL + K_mgL) %>%
  ggplot(aes(year, base_cations, color = lake_ID)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "gam", linewidth = 1.2, se = TRUE) +
  labs(
    x = "Year",
    y = "Base cations ( Mg + Na + K, mg/L)",
    color = "Watershed\nPosition"
  ) +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N." = "forestgreen",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )



loch_chem <- loch_chem %>%
  mutate(BC_noCa = Mg_mgL + Na_mgL + K_mgL)

so4_bc_plot <- ggplot(loch_chem, aes(BC_noCa, SO4_mgL, color = lake_ID)) +
  geom_point(alpha = 0.7, size = 1) +
  geom_smooth(method = "gam", se = TRUE, linewidth = 1.1) +
   facet_wrap(~lake_ID, ncol = 1) +
  coord_cartesian(xlim = c(NA, 1.5)) +
  labs(
    x = "Base cations (Mg + Na + K)",
    y = expression(SO[4]~"(mg/L)")
  ) +
  scale_color_manual(values = c(
    "Upper S." = "purple",
    "Upper N." = "forestgreen",
    "Lower" = "dodgerblue"
  )) +
  dark_theme(base_size = 18)
so4_bc_plot

so4_sky <- loch_chem %>%
  filter(lake_ID == "Upper S.") %>%
  ggplot(aes(BC_noCa, SO4_mgL)) +
  geom_point(color = "purple", alpha = 0.7, size = 1) +
  geom_smooth(color = "purple", method = "gam", se = TRUE, linewidth = 1.1) +
  coord_cartesian(xlim = c(NA, 1.4)) +
  labs(x = "Base cations (Mg + Na + K)", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)
so4_sky

so4_andrews <- loch_chem %>%
  filter(lake_ID == "Upper N.") %>%
  ggplot(aes(BC_noCa, SO4_mgL)) +
  geom_point(color = "forestgreen", alpha = 0.7, size = 1) +
  geom_smooth(color = "forestgreen", method = "gam", se = TRUE, linewidth = 1.1) +
  coord_cartesian(xlim = c(NA, 1.4)) +
  labs(x = "Base cations (Mg + Na + K)", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)
so4_andrews

so4_loch <- loch_chem %>%
  filter(lake_ID == "Lower") %>%
  ggplot(aes(BC_noCa, SO4_mgL)) +
  geom_point(color = "dodgerblue", alpha = 0.7, size = 1) +
  geom_smooth(color = "dodgerblue", method = "gam", se = TRUE, linewidth = 1.1) +
  coord_cartesian(xlim = c(NA, 1.4)) +
  labs(x = "Base cations (Mg + Na + K)", y = expression(SO[4]~"(mg/L)")) +
  dark_theme(base_size = 18)
so4_loch
