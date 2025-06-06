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
CQ_seasons_NO3 <- CQ_seasons %>%
  filter(chem_name=="DOC") %>%
  group_by(waterYear) %>%
  # filter(waterYear=="2010") %>%
  mutate(hydro_season = case_when(date > day_80th  ~ "fall baseflow",
                                  date < day_20th ~ "winter baseflow",
                                  date >= day_20th & date < day_50th ~ "rising limb",
                                  date >= day_50th & date <= day_80th ~ "falling limb"))

CQ_seasons_NO3 %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()


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


# Plot chem by waterYear, color by hydroseason ----------------------------


CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  # filter(chem_name=="DOC") %>%
  ggplot(aes(x=date, y=Q_m3s, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="DOC") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="NO3-N") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(chem_name=="cations") %>%
  ggplot(aes(x=date, y=chem_value, color=hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x")


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
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)

CQ_seasons %>%
  # filter(waterYear>=1991 & !waterYear == "2022") %>% 
  filter(waterYear=="2020") %>%
  filter(chem_name=="NO3-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, color=wy_doy, shape = hydro_season))+
  geom_point()+
  facet_wrap(.~waterYear, scales="free_x") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)


# Slopes of C-Q by season and chem value ----------------------------------

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="cations") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="Cations")
### OHHH THERE IS SOMETHING HERE MAYBE??

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="NO3-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="Nitrate")

CQ_seasons %>%
  filter(waterYear>=1991 & !waterYear == "2022") %>%
  # filter(waterYear=="2020") %>%
  filter(chem_name=="NH4-N") %>%
  mutate(wy_doy = hydro.day(date)) %>%
  ggplot(aes(x=Q_m3s, y=chem_value, group=waterYear, color=waterYear, shape = hydro_season))+
  geom_point()+
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
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(hydro_season~., scales="free") +
  scale_colour_gradient(low = "yellow", high = "blue", na.value = NA)+
  labs(title="DOC")


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

