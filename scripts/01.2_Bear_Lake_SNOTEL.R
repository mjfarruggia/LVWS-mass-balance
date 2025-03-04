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
SWE_stats %>%
  select(year, max_swe_wydoy, max_swe) %>%
  pivot_longer(-year) %>%
  mutate(name = recode(name, max_swe_wydoy = "Date of max. SWE", max_swe = "Max SWE (cm)")) %>%
  ggplot(aes(x=year, y=value, fill=name))+
  geom_point(shape=21, size=2)+
  # geom_smooth(method="lm")+
  facet_wrap(.~name, scales="free_y", nrow=2)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme_few(base_size=16)+
  theme(legend.position="none")+
  labs(x="Year", y = "Value")
ggsave("figures/bear_lake_SWE.png", dpi=600, units="in", height=5, width=3)

