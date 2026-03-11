#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# defining season based on streamflow #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(dataRetrieval)
library(scales)

GL4discharge <- read.csv('scripts/gl_network_discharge_raw.csv') |> select(-X) |> 
  filter(local_site=='GL4') |>
  mutate(Date=as.Date(date)) |> 
  drop_na(discharge_vol_cm)  |>
  addWaterYear() |>
  filter(waterYear >= 2009) %>%
  mutate(Q_m3s = discharge_vol_cm / 1000)|>#I believe the units at L/s on EDI?
  arrange(Date) |>  # make sure dates are ordered
  mutate(Q_m3s_interp = zoo::na.approx(Q_m3s, 
                                  x = Date, 
                                  na.rm = FALSE,
                                  maxgap = 30))  #no gaps longer than 7 days 


ggplot(GL4discharge, aes(Date, Q_m3s)) +
  geom_line() +
  geom_point(alpha=0.25,size=1)+
  facet_wrap(.~waterYear, scales="free_x")+
  scale_x_date(labels = label_date("%m-%d"),
               breaks = "4 months")

# some years with low data that might need to be removed but overall looks decent

percentile_days<- GL4discharge |>
  group_by(waterYear) |>
  arrange(Date) |>
  mutate(cumulative_dis = cumsum(discharge_vol_cm),
         total_flow = sum(discharge_vol_cm)) |>
  summarise(
    day_20th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.25 * pick(total_flow)[[1]])[1]],
    day_50th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.5 * pick(total_flow)[[1]])[1]],
    day_80th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.8 * pick(total_flow)[[1]])[1]]
  ) |> 
  mutate(day_20th_doy = yday(day_20th),
         day_50th_doy = yday(day_50th),
         day_80th_doy = yday(day_80th)) |>
  ungroup()



# look at water year hydrograph, cumulative discharge graph
checks <- GL4discharge |>
  mutate(
    wy_doy = yday(Date) - yday(as.Date(paste0(year(Date), "-10-01"))) + 1,
    wy_doy = ifelse(wy_doy <= 0, wy_doy + 365, wy_doy) 
  ) |>
  group_by(waterYear) |>
  arrange(Date) |>
  mutate(cumulative_dis = cumsum(discharge_vol_cm),
         total_flow = sum(discharge_vol_cm))


# hydrograph
ggplot(checks %>% filter(waterYear=="2017"), aes(wy_doy, discharge_vol_cm, color = as.factor(waterYear))) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, 92, 183, 274, 366),
                     labels = c("Oct", "Jan", "Apr", "Jul", "Oct"))


# cumulative sum
ggplot(checks %>% filter(waterYear=="2017"), aes(wy_doy, cumulative_dis, color = as.factor(waterYear))) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, 92, 183, 274, 366),
                     labels = c("Oct", "Jan", "Apr", "Jul", "Oct"))


# Plot dates over raw. Look reasonable?
# Cumulative flow
GL4discharge %>%
  filter(!waterYear=="2018") %>%
  ggplot(aes(x=Date, y=Q_m3s))+
  geom_point()+
  geom_vline(xintercept=percentile_days$day_20th, color="blue")+
  geom_vline(xintercept=percentile_days$day_50th, color="green")+
  geom_vline(xintercept=percentile_days$day_80th, color="red")+
  facet_wrap(.~waterYear, scales="free_x")+
  scale_x_date(labels = label_date("%m-%d"),
               breaks = "4 months")



# Snowmelt onset? ---------------------------------------------------------

# Define function to detect snowmelt onset per year
detectSnowmeltOnset <- function(df, window = 7, slope_threshold = 0.01) {
  df %>%
    filter(month(Date) >= 1 & month(Date) <= 5) %>%  # Focus Jan-May
    arrange(Date) %>%
    mutate(
      rollmean = zoo::rollmean(Q_m3s, k = window, fill = NA, align = "right"),
      diff = c(NA, diff(rollmean)),
      onset_flag = diff > slope_threshold
    ) %>%
    filter(onset_flag) %>%
    slice(1) %>%
    pull(Date)
}
#rollmean smooths out high-frequency noise over a 7-day window.
# diff computes the daily change in smoothed streamflow.
# slope_threshold (e.g., 0.01 m³/s/day) filters out small fluctuations and captures meaningful rise.
# The first day with a sustained positive slope is interpreted as snowmelt onset.
# Adjust the window and slope_threshold based on local hydrograph characteristics.

# Apply to each year
snowmelt_onsets <- GL4discharge %>%
  group_by(waterYear) %>%
  group_modify(~ {
    onset_date <- detect_snowmelt_onset(.x)
    tibble(snowmelt_onset_date = onset_date)
  })


GL4discharge %>%
  # filter(waterYear %in% c(1995, 2000, 2005)) %>%
  ggplot(aes(x = Date, y = Q_m3s, group=waterYear)) +
  geom_line() +
  geom_vline(data = snowmelt_onsets,
             aes(xintercept = ymd(snowmelt_onset_date)),
             color = "blue", linetype = "dashed") +
  facet_wrap(~waterYear, scales = "free_x") +
  labs(title = "Snowmelt Onset Detection", y = "Streamflow (m³/s)", x = "")+
  scale_x_date(labels = label_date("%m-%d"),
               breaks = "4 months")

