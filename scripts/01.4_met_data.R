
met1 <- read_csv(here("data/WY1992to2019_LochVale_Climate_SexstoneDataRelease.csv"),  col_names = TRUE) %>%
  filter(station_name == "Main weather station") %>%
  mutate(timestamp = mdy_hm(timestamp),
         waterYear = calcWaterYear(timestamp)) %>%
  rename(date_time=timestamp) %>%
  # filter(measurement_height==6) %>%
  mutate(dataset="met1")
  # distinct(date_time,measurement_height, .keep_all = TRUE)

head(met1$date_time)
tail(met1$date_time)

met2 <- read_csv(here("data/WY2016to2022_LochVale_Climate.csv"),  col_names = TRUE) %>%
  filter(station_name == "Main weather station") %>%
  mutate(date_time = mdy_hm(date_time),
         waterYear = calcWaterYear(date_time))%>%
  # filter(measurement_height==6) %>%
  mutate(dataset="met2") 
  # distinct(date_time,measurement_height, .keep_all = TRUE)

head(met2$date_time)
tail(met2$date_time)

met3 <- met1 %>%
  filter(!date_time %in% met2$date_time) %>%
  bind_rows(., met2)

met3_daily <- met3 %>%
  mutate(date = date(date_time)) %>%
  select(date_time, date, T_air, waterYear, measurement_height) %>%
  filter(measurement_height==2) %>%
  group_by(date) %>%
  summarize(Tave2M = mean(T_air, na.rm=TRUE),
            Tmax2M = max(T_air, na.rm=TRUE),
            Tmin2M = min(T_air, na.rm=TRUE))

subdaily_met_1991to2022 <- bind_rows(met1, met2, met3)


# Visualize missing data with the nanier package: -------------------------

#(warning, these can take ~30 sec to run on Bella's machine)
ggplot(subdaily_met_1991to2022 %>%
         filter(measurement_height==2), 
       aes(x = date_time, 
           y = T_air)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Temp data contain few gaps

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==2), 
       aes(x = date_time, 
           y = RH)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Sizable gap in late 2021 but otherwise not much missing. 

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==2), 
       aes(x = date_time, 
           y = WSpd)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Sporadic gaps with two larger gaps in 2001 and 2021:
ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==2) %>% filter(waterYear %in% c(2001, 2021)), 
       aes(x = date_time, 
           y = WSpd)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Both years probably don't overlap with the period around ice off, which is good
ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==2), 
       aes(x = date_time, 
           y = SWin)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#VERY little SWin data at 2m, except 2020, 2021, 2022

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==6), 
       aes(x = date_time, 
           y = SWin)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#The 6m sensor seems fine more of the time but still big gaps in 2000-2001

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==6), 
       aes(x = date_time, 
           y = SWout)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Same as above

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==6), 
       aes(x = date_time, 
           y = LWin)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#At 6m: No LWin data late1994-early1995, 1998-2002, late 2006-2009

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==6), 
       aes(x = date_time, 
           y = LWout)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#At 6m: Lots of gaps throughout, biggest ones at 1998-2000, 2006-2009

ggplot(subdaily_met_1991to2022%>%
         filter(measurement_height==2), 
       aes(x = date_time, 
           y = LWout)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")

#Given that radiation values at 2m or 6m should not differ much, I'm going to create
#A joint column that combines the two. All others (temp, wind, RH) will use the 2m sensors.

subdaily_met_1991to2022_tempRHWind <- subdaily_met_1991to2022 %>%
  select(date_time, T_air, RH, WDir, WSpd, WGust, measurement_height) %>%
  pivot_longer(T_air:WGust,
               names_to = "name",
               values_to = "value") %>%
  #Pivot wider but add the measurement height to the column name
  pivot_wider(names_from = c(name, measurement_height),
              values_from = value,
              names_glue = "{name}_{measurement_height}_m",
              values_fn = mean) #If duplicate observations take the mean

subdaily_met_1991to2022_radiation <- subdaily_met_1991to2022 %>%
  select(date_time, SWin, SWout, LWin, LWout, measurement_height) %>%
  pivot_longer(SWin:LWout,
               names_to = "name",
               values_to = "value") %>%
  #Create one value for both 2m and 6m sensor, take mean.
  group_by(date_time,name) %>%
  summarize(value = mean(value, na.rm=TRUE)) %>% 
  #Pivot wider but add the measurement height to the column name
  pivot_wider(names_from = name,
              values_from = value,
              names_glue = "{name}_2m6m_mean") %>% #If duplicate observations take the mean
  mutate(across(LWin_2m6m_mean:SWout_2m6m_mean, ~ ifelse(is.nan(.), NA, .)))

subdaily_met_1991to2022_final <- left_join(subdaily_met_1991to2022_tempRHWind, 
                                           subdaily_met_1991to2022_radiation,
                                           by="date_time") %>%
  mutate(waterYear=calcWaterYear(date_time))

ggplot(subdaily_met_1991to2022_final, 
       aes(x = date_time, 
           y = SWin_2m6m_mean)) + 
  geom_miss_point() + 
  facet_wrap(~waterYear, scale="free")
#Now minimal gaps in SWin. LWin has some bigger gaps-- oh well!

write_csv(subdaily_met_1991to2022_final, "data/export/subdaily_met_1991to2022.csv")


#Prior to 1992 the station was called RAWS
#Found this in the LVWS Google drive > LVWS_data > meterological data
#Have not found data beyond temperature
#These are daily values

met4_daily <- read_excel("data/LVWS_masterdata_temp_and_precip_180220.xlsx",
                                                     sheet = "temp", skip = 2) %>%
  mutate(date = ymd(Date)) %>%
  rename(Tave2M=`Tave2M (°C)`,
        Tmax2M=`Tmax2M (°C)`,
        Tmin2M= `Tmin2M (°C)`) %>%
  select(date, Tave2M,Tmax2M,Tmin2M) %>%
  filter(date < "1991-12-17")

met_all <- bind_rows(met4_daily, met3_daily) %>%
  mutate(waterYear = calcWaterYear(date))

met_all %>%
  ggplot(aes(x=date, y=Tave2M))+
  geom_point()+
  facet_wrap(~waterYear, scales="free_x")


met_monthly <- met_all %>%
  mutate(month = month(date, label=TRUE)) %>%
  group_by(waterYear,month) %>%
  summarize(Tmax2M=mean(Tmax2M, na.rm=TRUE),
            Tave2M=mean(Tave2M, na.rm=TRUE),
            Tmin2M=mean(Tmin2M, na.rm=TRUE))

met_monthly[sapply(met_monthly, is.infinite)] <- NA


met_monthly_wide <- met_monthly %>%
  select(waterYear, month, contains("Tmax2M")) %>%
  pivot_wider(names_from = month, values_from = Tmax2M, names_prefix = "Tmax2M_")
