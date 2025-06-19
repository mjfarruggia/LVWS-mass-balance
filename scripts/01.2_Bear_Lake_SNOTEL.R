source("scripts/00_functions.R")
source("scripts/00_libraries.R")
##Pull Bear Lake SNOTEL data from the snotelr package
#https://bluegreen-labs.github.io/snotelr/articles/snotelr-vignette.html

# download and list site information
site_meta_data <- snotel_info()
head(site_meta_data) #we want site_name == "bear lake", site_id == 322

# downloading data for a random site
snow_data <- snotel_download(
  site_id = 322,
  internal = TRUE
) 

snow_data <- snow_data %>%
  mutate(waterYear = calcWaterYear(date),
         date=ymd(date))

# A plot of snow accummulation through the years
plot(as.Date(snow_data$date),
     snow_data$snow_water_equivalent,
     type = "l",
     xlab = "Date",
     ylab = "SWE (mm)"
)

# calculate snow phenology
SWE_stats <- snotel_phenology(snow_data)

SWE_stats <- SWE_stats %>%
  mutate(waterYear = calcWaterYear(max_swe_date)) #DONT use year column in analyses-- use waterYear

#Check that the stats are doing what we think they're doing
head(snow_data)
str(snow_data)

head(SWE_stats)
str(SWE_stats)

plot(SWE_stats$year, SWE_stats$waterYear)

snow_data %>%
  ggplot(aes(x=date, y=snow_water_equivalent))+
  geom_line()+
  geom_vline(xintercept=SWE_stats$max_swe_date,
             color="red") +
  geom_vline(xintercept=SWE_stats$last_snow_melt,
             color="blue") +
  facet_wrap(.~waterYear, scales="free_x")

#Create water-year DOY columns
str(SWE_stats)
SWE_stats <- SWE_stats %>%
  mutate(first_snow_acc_wydoy = hydro.day(first_snow_acc),
         cont_snow_acc_wydoy = hydro.day(cont_snow_acc),
         first_snow_melt_wydoy = hydro.day(first_snow_melt),
         last_snow_melt_wydoy = hydro.day(last_snow_melt),
         max_swe_wydoy = hydro.day(max_swe_date),
         snow_melt_duration = first_snow_melt_wydoy - max_swe_wydoy)

# Plot date of max SWE of time, last snow melt, and the distance between the two dates

maxSWEdate <- SWE_stats %>%
  select(year, max_swe_date) %>%
  mutate(max_swe_doy = yday(max_swe_date)) %>%
  mutate(max_swe_date = as.Date(max_swe_doy - 1, origin = "2000-01-01")) %>%
  ggplot(aes(x=year, y=max_swe_date))+
  geom_point(shape=21, size=2, fill="#E69F00")+
  theme_few(base_size=12)+
  theme(legend.position="none")+
  labs(x="Year", y = "Date of\nmax. SWE") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0,0,0,0), "lines"))

maxSWE <- SWE_stats %>%
  select(year, max_swe) %>%
  ggplot(aes(x=year, y=max_swe))+
  geom_point(shape=21, size=2, fill="#56B4E9")+
  theme_few(base_size=12)+
  theme(legend.position="none")+
  labs(x="Year", y = "Maximum\nSWE (cm)")+
  theme(plot.margin=unit(c(0,0,0,0), "lines"))

maxSWEdate / maxSWE 

ggsave("figures/bear_lake_SWE.png", dpi=600, units="in", height=2.6, width=3.2)

# April 1 SWE? 
snow_data %>%
  mutate(doy=yday(date)) %>%
  filter(doy=="91") %>%
  ggplot(aes(x=waterYear, y=snow_water_equivalent))+
  geom_point()+
  geom_smooth()+
  labs(y="April 1 SWE (cm)")


# More May snow?
snow_data %>%
  mutate(month=month(date)) %>%
  filter(month=="5") %>%
  group_by(waterYear, month) %>%
  summarize(total_new_snow = sum(precipitation)) %>%
  ggplot(aes(x=waterYear, y=total_new_snow))+
  geom_point()+
  labs(y="Total new snow, May (cm)",
       title="SNOTEL at Bear Lake")
  # facet_wrap(~month)+
  # geom_smooth(method="gam")
