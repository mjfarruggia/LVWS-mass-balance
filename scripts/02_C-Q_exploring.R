source("scripts/01.1_LochO_data_munging.R")

head(outlet_daily_flux)

outlet_daily_flux %>%
  mutate(doy = yday(date)) %>%
  ggplot(aes(y=chem_value,x=Q_m3s, color=doy))+
  geom_point(alpha=0.2)+
  facet_wrap(chem_name~., scales="free_y")+
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)

# Do something like define seasons by hydrograph, look at slopes? IDK

head(percentile_days)
CQ_seasons <- outlet_daily_flux %>%
  left_join(., percentile_days, by="waterYear") %>%
  mutate(wy_doy = hydro.day(date))

# Pick DOC for now
CQ_seasons_DOC <- CQ_seasons %>%
  # filter(chem_name=="DOC") %>%
  group_by(waterYear) %>%
  # filter(waterYear=="2010") %>%
  mutate(hydro_season = case_when(date > day_80th  ~ "fall baseflow",
                                  date < day_20th ~ "winter baseflow",
                                  date >= day_20th & date < day_50th ~ "rising limb",
                                  date >= day_50th & date <= day_80th ~ "falling limb"))

CQ_seasons_DOC %>%
  filter(chem_name=="sio2_mgl") %>%
  group_by(waterYear, hydro_season) %>%
  summarize(chem_value = median(chem_value, na.rm=TRUE)) %>%
  ggplot(aes(x=waterYear, y=chem_value, color=hydro_season))+
  geom_point()+
  geom_smooth(method="gam")

CQ_seasons_DOC %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hydro_season, scales="free")


# Calculate snowmelt ONSET ------------------


# Define function to detect snowmelt onset per year
detect_snowmelt_onset <- function(df, window = 7, slope_threshold = 0.01) {
  df %>%
    filter(month(date) >= 1 & month(date) <= 5) %>%  # Focus Jan-May
    arrange(date) %>%
    mutate(
      rollmean = zoo::rollmean(Q_m3s, k = window, fill = NA, align = "right"),
      diff = c(NA, diff(rollmean)),
      onset_flag = diff > slope_threshold
    ) %>%
    filter(onset_flag) %>%
    slice(1) %>%
    pull(date)
}
#rollmean smooths out high-frequency noise over a 7-day window.
# diff computes the daily change in smoothed streamflow.
# slope_threshold (e.g., 0.01 m³/s/day) filters out small fluctuations and captures meaningful rise.
# The first day with a sustained positive slope is interpreted as snowmelt onset.
# Adjust the window and slope_threshold based on local hydrograph characteristics.

# Apply to each year
snowmelt_onsets <- CQ_seasons_NO3 %>%
  group_by(waterYear) %>%
  group_modify(~ {
    onset_date <- detect_snowmelt_onset(.x)
    tibble(snowmelt_onset_date = onset_date)
  })


CQ_seasons_NO3 %>%
  # filter(waterYear %in% c(1995, 2000, 2005)) %>%
  ggplot(aes(x = date, y = Q_m3s)) +
  geom_line() +
  geom_vline(data = snowmelt_onsets,
             aes(xintercept = snowmelt_onset_date),
             color = "blue", linetype = "dashed") +
  facet_wrap(~waterYear, scales = "free_x") +
  labs(title = "Snowmelt Onset Detection", y = "Streamflow (m³/s)", x = "")


CQ_seasons_NO3 %>%
  # filter(waterYear %in% c(1995, 2000, 2005)) %>%
  ggplot(aes(x = date, y = Q_m3s)) +
  geom_line() +
  geom_vline(data = snowmelt_onsets,
             aes(xintercept = snowmelt_onset_date),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept=percentile_days$day_20th, color="purple")+
  geom_vline(xintercept=percentile_days$day_50th, color="green")+
  geom_vline(xintercept=percentile_days$day_80th, color="red")+
  facet_wrap(~waterYear, scales = "free_x") +
  labs(title = "Snowmelt Onset Detection", y = "Streamflow (m³/s)", x = "")


#Is snowmelt onset getting earlier?
snowmelt_onsets %>%
  mutate(doy=yday(snowmelt_onset_date)) %>%
  ggplot(aes(x=waterYear,y=doy))+
  geom_point()

# Just one year for demonstration

hydro_seasons_rawQ <- CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear==2011) %>%
  # filter(chem_name=="DOC") %>%
  ggplot(aes(x=date, y=Q_m3s, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")+
  scale_color_manual(values = hydroCols) +
  theme(strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  labs(y = "Streamflow (m³/s)", x = "") 

# hydro_seasons_rawQ <- CQ_seasons_NO3 %>%
#   filter(waterYear %in% c(2009)) %>%
#   ggplot(aes(x = date, y = Q_m3s)) +
#   geom_line() +
#   geom_vline(data = snowmelt_onsets %>% filter(waterYear %in% c(2009)),
#              aes(xintercept = snowmelt_onset_date),
#              color = "#6493c8", linetype = "dashed",
#              linewidth=2, alpha=0.8) +
#   geom_vline(xintercept=percentile_days$day_20th, color="#aad440", linewidth=2, alpha=0.8)+
#   geom_vline(xintercept=percentile_days$day_50th, color="#eed440", linewidth=2, alpha=0.8)+
#   geom_vline(xintercept=percentile_days$day_80th, color="#bb5b48", linewidth=2, alpha=0.8)+
#   facet_wrap(~waterYear, scales = "free_x") +
#   labs(y = "Streamflow (m³/s)", x = "")+
#   theme(strip.text.x = element_blank())+
#   scale_x_date(date_labels="%b`%y")

# How does this map onto cumulative discharge? 

hydro_seasons_cumulQ <- CQ_seasons_NO3 %>%
  group_by(waterYear)%>%
  mutate(cumulative_dis = cumsum(Q_m3s)) %>%
  filter(waterYear %in% c(2011)) %>%
  ggplot(aes(x = date, y = cumulative_dis)) +
  geom_line() +
  geom_vline(data = snowmelt_onsets %>% filter(waterYear %in% c(2011)),
             aes(xintercept = snowmelt_onset_date),
             color = "#6493c8", linetype = "dashed",
             linewidth=2, alpha=0.8) +
  geom_vline(xintercept=percentile_days$day_20th, color="#aad440", linewidth=2, alpha=0.8)+
  geom_vline(xintercept=percentile_days$day_50th, color="#eed440", linewidth=2, alpha=0.8)+
  geom_vline(xintercept=percentile_days$day_80th, color="#bb5b48", linewidth=2, alpha=0.8)+
  facet_wrap(~waterYear, scales = "free_x") +
  labs(y = "Cumulative discharge", x = "") +
  theme(strip.text.x = element_blank())+
  scale_x_date(date_labels="%b`%y")


hydro_seasons_rawQ / hydro_seasons_cumulQ + plot_layout(guides = "collect")
ggsave("figures/hydro_season_definitions.png", dpi=600, width=4, height=6, units="in")


# One graph showing ridgelines for each parameter for distributions

snowmelt_phenology <-left_join(percentile_days, snowmelt_onsets) %>%
  mutate(snowmelt_onset_doy = yday(snowmelt_onset_date),
         day_20th_doy = yday(day_20th),
         day_50th_doy = yday(day_50th),
         day_80th_doy = yday(day_80th)) %>%
  select(waterYear, day_20th_doy, day_50th_doy, day_80th_doy, snowmelt_onset_doy) %>%
  pivot_longer(-waterYear) 


# Distributions  
ggplot(snowmelt_phenology, aes(x = value, y = name)) + ggridges::geom_density_ridges()

# Trends
snowmelt_phenology %>%
  mutate(name = factor(name,
                       levels = c("snowmelt_onset_doy",
                                  "day_20th_doy",
                                  "day_50th_doy",
                                  "day_80th_doy"),
                       labels = c("snowmelt onset",
                                  "Q 20th",
                                  "Q 50th (COM)",
                                  "Q 80th"))) %>%
  mutate(value = as.Date(value - 1, origin = "2000-01-01")) %>%
  ggplot(aes(x=waterYear, y=value, color=name))+
  geom_point()+
  geom_line()+
  # geom_smooth(method="gam")+
  scale_y_date(
    date_labels = "%b %d",           # Show month and day
    date_breaks = "2 weeks"          # Adjust as needed
  ) +
  scale_color_manual(values=hydroCols2,
                     "Legend")+
  scale_fill_manual(values=hydroCols2,
                    name="Legend")+
  theme(legend.position="bottom",
        axis.title.y=element_blank())+
  labs(x="Year")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(alpha = 1)))  

ggsave("figures/changes_in_snowmelt_timing.png", dpi=600, width=5, height=5, units="in")


## TRENDS to report on poster
snowmelt_phenology %>%
  pivot_wider(names_from = name, 
              values_from = value) %>%
  ggplot(aes(x=waterYear, y=day_50th_doy-day_20th_doy))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="# days from start of snowmelt to peak")
lm1<- lm(day_50th_wydoy-day_20th_wydoy~waterYear, data=percentile_days)
lm1_summary <-summary(lm1) #modest, 1.6 days per decade shorter duration of snowmelt start to peak
lm1_summary
confint(lm1)


# This seems to work pretty well!

# Is there a trend? 
percentile_days %>%
  left_join(., snowmelt_onsets, by="waterYear") %>% 
  ggplot(aes(x=waterYear, y=hydro.day(snowmelt_onset_date)))+
  geom_point()

percentile_days %>%
  left_join(., snowmelt_onsets, by="waterYear") %>% 
  ggplot(aes(x=waterYear, y=day_50th_wydoy-hydro.day(snowmelt_onset_date)))+
  geom_point()

# Add it to percentile_days
head(snowmelt_onsets)

## TRY AGAIN!
CQ_seasons <- outlet_daily_flux %>%
  left_join(., percentile_days, by="waterYear") %>%
  left_join(., snowmelt_onsets, by="waterYear") %>%
  mutate(wy_doy = hydro.day(date))

# Pick DOC for now
CQ_seasons <- CQ_seasons %>%
  # filter(chem_name=="DOC") %>%
  group_by(waterYear) %>%
  # filter(waterYear=="2010") %>%
  mutate(hydro_season = case_when(date > day_80th  ~ "fall baseflow",
                                  date < snowmelt_onset_date ~ "winter baseflow",
                                  date >= snowmelt_onset_date & date < day_50th ~ "rising limb",
                                  date >= day_50th & date <= day_80th ~ "falling limb"))


# Plot all Q on the same graph? ----------------------------




# Plot chem by waterYear, color by hydroseason ----------------------------


CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear==2010) %>%
  # filter(chem_name=="DOC") %>%
  ggplot(aes(x=date, y=Q_m3s, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="DOC") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") +
  scale_x_date(date_labels="%m") +
  labs(y="DOC (mg/L)")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="NO3-N") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")+
  scale_x_date(date_labels="%m", date_breaks = "6 months") +
  labs(y="NO3-N (mg/L)")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="cations") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")+
  scale_x_date(date_labels="%m", date_breaks = "6 months") +
  labs(y="Sum of base cations (mg/L)")


CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="SO4-S") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") 


# Look at C-Q by hydroseason, waterYear ----------------------------
CQ_seasons %>%
  # filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear=="2009") %>%
  filter(chem_name=="SO4-S") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, color=wy_doy, shape = hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA) +
  labs(y="SO4-S (mg/L)")

CQ_seasons %>%
  # filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear=="2009") %>%
  filter(chem_name=="NO3-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, color=wy_doy, shape = hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA) +
  labs(y="NO3-N (mg/L)")


CQ_seasons %>%
  # filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear=="2022") %>%
  filter(chem_name=="cations") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, color=wy_doy, shape = hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA) 
# labs(y="NO3-N (mg/L)")


# Slopes of C-Q by season and chem value ----------------------------------

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="cations") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="Cations")
### OHHH THERE IS SOMETHING HERE MAYBE??

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="SO4-S") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="SO4-S")+
  scale_x_log10()+
  scale_y_log10()


CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2013") %>%
  filter(chem_name=="NO3-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="Nitrate")+
  scale_x_log10()+
  scale_y_log10()

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="NH4-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="Ammonium")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="DOC") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="DOC")



# Extract the slopes of the C-Q curves!!! ---------------------------------

## get the trends and add that as a category for graphing
map_lm <- function(df){
  mod <- lm(df$value ~ df$Q_m3s)
}


lm_slope <- function(mod) {
  mod$coefficients[[2]] # pull out slope
}


CQ_NO3_nested <- CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  filter(chem_name=="NO3-N") %>%
  rename(value = chem_value) %>%
  select(waterYear, hydro_season, Q_m3s, chem_name, value) %>%
  mutate(Q_m3s = log10(Q_m3s+0.001),
         value = log10(value+0.001)) %>%
  group_by(waterYear, hydro_season) %>%
  nest() %>%
  mutate(
    lm = map(data, map_lm),
    lm_sum = map(lm, broom::glance),
    slope = map(lm, lm_slope))

## Un-nest 
CQ_NO3_unnested = unnest(CQ_NO3_nested, c(lm_sum, slope)) %>%
  mutate(
    trend = case_when(
      p.value <= 0.05 & slope >= 0 ~ 'mobilizing',
      p.value <= 0.05 & slope <= 0 ~ 'dilution',
      p.value > 0.05 ~ 'chemostatic'
    ),
    trend = factor(trend,
                   levels = c('chemostatic',
                              'mobilizing',
                              'dilution'))
  )

## PLOT ME!
head(CQ_NO3_unnested)

CQ_NO3_unnested %>%
  mutate(hydro_season = factor(hydro_season,
                               levels = c("winter baseflow",
                                          "rising limb",
                                          "falling limb",
                                          "fall baseflow"
                               ))) %>%
  ggplot(aes(x=waterYear, y=slope,fill=hydro_season))+
  geom_point(shape=21, size=2.5, color="black")+
  facet_wrap(~hydro_season)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position="none") +
  labs(y="C-Q slope for NO3",
       x="Year") +
  scale_x_continuous()+
  scale_fill_manual(values = hydroCols, name = "Hydrologic season") +
  theme_few(base_size = 22)+
  theme(legend.position = "none")

ggsave(
  "figures/CQ_NO3_over_time.png",
  dpi = 600,
  width = 8,
  height = 7,
  units = "in"
)


CQ_NO3_unnested %>%
  mutate(hydro_season = factor(hydro_season,
                               levels = c("winter baseflow",
                                          "rising limb",
                                          "falling limb",
                                          "fall baseflow"
                               ))) %>%
  ggplot(aes(x=waterYear, y=slope, fill=hydro_season, group=NA))+
  geom_point(shape=21, size=2, color="black")+
  # facet_wrap(~hydro_season)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position="none") +
  labs(y="C-Q slope for NO3") +
  scale_x_continuous()+
  scale_fill_manual(values = hydroCols, name = "Hydrologic season") 

CQ_NO3_unnested %>%
  select(hydro_season, waterYear, trend, slope) %>%
  left_join(., hydro_season_flux %>%
              filter(chem_name=="NO3-N") %>%
              select(waterYear, hydro_season, annual_flux_kg_per_ha)) %>%
  mutate(hydro_season = factor(hydro_season,
                               levels = c("rising limb",
                                          "falling limb",
                                          "fall baseflow",
                                          "winter baseflow"))) %>%
  ggplot(aes(x=waterYear, y=slope, fill=annual_flux_kg_per_ha))+
  geom_point(shape=21, size=2, color="black")+
  facet_wrap(~hydro_season)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position="none") +
  labs(y="C-Q slope for NO3") +
  scale_x_continuous()


# Mean, min, max concentrations by hydro season  --------------------------

chem_seasons_concentration_summary <- CQ_seasons %>%
  filter(waterYear >= 1991) %>%
  drop_na(chem_name) %>%
  group_by(waterYear, chem_name, hydro_season) %>%
  summarize(min = min(chem_value, na.rm = TRUE),
            mean = mean(chem_value, na.rm = TRUE),
            median = median(chem_value, na.rm = TRUE),
            max = max(chem_value, na.rm = TRUE)) 

head(chem_seasons_concentration_summary)

chem_seasons_concentration_summary %>%
  pivot_longer(min:max) %>%
  filter(!hydro_season=="NA") %>%
  ggplot(aes(x=waterYear, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_wrap(chem_name~hydro_season, scales="free_y", ncol=4)


chem_seasons_concentration_summary <- CQ_seasons %>%
  filter(waterYear >= 1991) %>%
  drop_na(chem_name) %>%
  group_by(waterYear, chem_name, hydro_season) %>%
  summarize(min = min(daily_flux_kg, na.rm = TRUE),
            mean = mean(daily_flux_kg, na.rm = TRUE),
            median = median(daily_flux_kg, na.rm = TRUE),
            max = max(daily_flux_kg, na.rm = TRUE),
            sum = sum(daily_flux_kg, na.rm = TRUE)) 

head(chem_seasons_concentration_summary)

chem_seasons_concentration_summary %>%
  pivot_longer(min:sum) %>%
  filter(!name=="sum") %>%
  filter(!hydro_season=="NA") %>%
  ggplot(aes(x=waterYear, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_wrap(chem_name~hydro_season, scales="free_y", ncol=4)

chem_seasons_concentration_summary %>%
  pivot_longer(min:sum) %>%
  filter(name=="sum") %>%
  filter(!hydro_season=="NA") %>%
  ggplot(aes(x=waterYear, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_wrap(chem_name~hydro_season, scales="free_y", ncol=4)

# NEXT STEPS: -------------------------------------------------------------


# (1) Write a loop that extracts the slope (and intercept) of the C-Q relationships, plot over time for each water season
# (2) are there relationships between the C-Q relationship and the cumulative flux? Maybe that is a "DUH" question??

