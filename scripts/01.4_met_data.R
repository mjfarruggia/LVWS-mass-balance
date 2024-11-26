
met1 <- read_csv(here("data/WY1992to2019_LochVale_Climate_SexstoneDataRelease.csv"),  col_names = TRUE) %>%
  filter(station_name == "Main weather station") %>%
  mutate(timestamp = mdy_hm(timestamp),
         waterYear = calcWaterYear(timestamp)) %>%
  rename(date_time=timestamp) %>%
  filter(measurement_height==2) %>%
  mutate(dataset="met1")%>%
  distinct(date_time, .keep_all = TRUE)

head(met1$date_time)
tail(met1$date_time)

met2 <- read_csv(here("data/WY2016to2022_LochVale_Climate.csv"),  col_names = TRUE) %>%
  filter(station_name == "Main weather station") %>%
  mutate(date_time = mdy_hm(date_time),
         waterYear = calcWaterYear(date_time))%>%
  filter(measurement_height==2) %>%
  mutate(dataset="met2") %>%
  distinct(date_time, .keep_all = TRUE)

head(met2$date_time)
tail(met2$date_time)

met3 <- met1 %>%
  filter(!date_time %in% met2$date_time) %>%
  bind_rows(., met2)

met3_daily <- met3 %>%
  mutate(date = date(date_time)) %>%
  select(date_time, date, T_air, waterYear) %>%
  group_by(date) %>%
  summarize(Tave2M = mean(T_air, na.rm=TRUE),
            Tmax2M = max(T_air, na.rm=TRUE),
            Tmin2M = min(T_air, na.rm=TRUE))

# met3 %>%
#   ggplot(aes(x=date_time, y=T_air, color=dataset))+
#   geom_point()+
#   facet_wrap(~waterYear, scales="free_x")

#Prior to 1992 the station was called RAWS
#Found this in the LVWS Google drive > LVWS_data > meterological data
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
