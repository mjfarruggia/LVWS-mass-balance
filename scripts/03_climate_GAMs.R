source("scripts/00_functions.R")
source("scripts/00_libraries.R")

#Pull in the subdaily met data
met_data <- read_csv("data/export/subdaily_met_1991to2022.csv")

# Aggregate the temperature data to daily timesteps
# Do daily min, mean, max first, then look at average across months
met_data_daily_summary <- met_data %>%
  mutate(date = date(date_time), 
         waterYear = calcWaterYear(date)) %>%
  group_by(waterYear, date) %>%
  summarize(Tave2M = mean(T_air_2_m, na.rm=TRUE),
            Tmax2M = max(T_air_2_m, na.rm=TRUE),
            Tmin2M = min(T_air_2_m, na.rm=TRUE)) %>%
  arrange(date)
head(met_data_daily_summary)

#Prior to 1992 the station was called RAWS
#Found this in the LVWS Google drive > LVWS_data > meterological data
#Have not found data beyond temperature
#These are daily values
met4_daily <- read_excel("data/LVWS_masterdata_temp_and_precip_180220.xlsx",
                         sheet = "temp", skip = 2) %>%
  mutate(date = ymd(Date),
         waterYear=calcWaterYear(date)) %>%
  rename(Tave2M=`Tave2M (°C)`,
         Tmax2M=`Tmax2M (°C)`,
         Tmin2M= `Tmin2M (°C)`) %>%
  select(date, waterYear, Tave2M,Tmax2M,Tmin2M) %>%
  filter(date < "1991-12-17") %>%
  arrange(date)

head(met4_daily)
tail(met4_daily)

met_data_daily_summary <- bind_rows(met_data_daily_summary,
                                    met4_daily) %>%
  mutate(month = month(date)) 
  # group_by(waterYear, month) %>%
  # summarize(Tave2M = mean(Tave2M, na.rm=TRUE),
  #           Tmax2M = max(Tmax2M, na.rm=TRUE),
  #           Tmin2M = min(Tmin2M, na.rm=TRUE))

head(met_data_daily_summary)

# Get rid of month-year combos where over 30% of the observations are missing
met_data_daily_summary <- met_data_daily_summary %>%
  group_by(waterYear, month) %>%
  mutate(n_days = n(),
         n_NAs=sum(is.na(Tave2M)),
         perc_missing_days=(n_NAs/n_days)*100) %>%
  filter(!perc_missing_days>30)



# ~~ FIGURE S2.  MEAN Monthly temp.  trends  -----------------------------------



# Fitting GAMs for mean Jan temperature -------------------------------------------
JanWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="1") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Jan")


### Model
modJanTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = JanWx,
                       method = "REML")
summary(modJanTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JanTempMeanPred <- with(JanWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
JanTempMeanPred <- cbind(JanTempMeanPred, data.frame(predict(modJanTempMean$gam, JanTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JanTempMeanPred <- transform(JanTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Jan")


m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = JanTempMeanPred$deriv,
                   JanTempMeanPred$upper,
                   JanTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modJanTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Jan_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=JanTempMeanPred$waterYear) %>%
  left_join(.,JanTempMeanPred) %>%
  mutate(month="Jan")

JanWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=JanTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=JanTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=JanTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=Jan_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean Feb temperature -------------------------------------------
FebWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="2") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Feb")


### Model
modFebTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = FebWx,
                       method = "REML")
summary(modFebTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
FebTempMeanPred <- with(FebWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
FebTempMeanPred <- cbind(FebTempMeanPred, data.frame(predict(modFebTempMean$gam, FebTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
FebTempMeanPred <- transform(FebTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Feb")


m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = FebTempMeanPred$deriv,
                   FebTempMeanPred$upper,
                   FebTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modFebTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Feb_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=FebTempMeanPred$waterYear) %>%
  # left_join(.,FebTempMeanPred) %>%
  mutate(month="Feb")

FebWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()+
  geom_line(data=FebTempMeanPred, aes(x=waterYear, y=fit)) +
  geom_line(data=FebTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  geom_line(data=FebTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  geom_line(data=Feb_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean March temperature -------------------------------------------
MarchWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="3") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="March")


### Model
modMarchTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                         data = MarchWx,
                         method = "REML")
summary(modMarchTempMean$gam)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MarchTempMeanPred <- with(MarchWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                               max(waterYear, na.rm=TRUE),
                                                               length.out = 200)))
MarchTempMeanPred <- cbind(MarchTempMeanPred, data.frame(predict(modMarchTempMean$gam, MarchTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MarchTempMeanPred <- transform(MarchTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(month="March")


m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = MarchTempMeanPred$deriv,
                   MarchTempMeanPred$upper,
                   MarchTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modMarchTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

March_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=MarchTempMeanPred$waterYear) %>%
  # left_join(.,MarchTempMeanPred) %>%
  mutate(month="March")

MarchWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=MarchTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=MarchTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=MarchTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=March_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean April temperature -------------------------------------------
AprilWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="4") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="April")


### Model
modAprilTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                         data = AprilWx,
                         method = "REML")
summary(modAprilTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AprilTempMeanPred <- with(AprilWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                               max(waterYear, na.rm=TRUE),
                                                               length.out = 200)))
AprilTempMeanPred <- cbind(AprilTempMeanPred, data.frame(predict(modAprilTempMean$gam, AprilTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AprilTempMeanPred <- transform(AprilTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(month="April")


m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = AprilTempMeanPred$deriv,
                   AprilTempMeanPred$upper,
                   AprilTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modAprilTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

April_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=AprilTempMeanPred$waterYear) %>%
  # left_join(.,AprilTempMeanPred) %>%
  mutate(month="April")

AprilWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=AprilTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=AprilTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=AprilTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=April_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean May temperature -------------------------------------------
MayWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="5") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="May")


### Model
modMayTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = MayWx,
                       method = "REML")
summary(modMayTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MayTempMeanPred <- with(MayWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
MayTempMeanPred <- cbind(MayTempMeanPred, data.frame(predict(modMayTempMean$gam, MayTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MayTempMeanPred <- transform(MayTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="May")


m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = MayTempMeanPred$deriv,
                   MayTempMeanPred$upper,
                   MayTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modMayTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

May_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=MayTempMeanPred$waterYear) %>%
  # left_join(.,MayTempMeanPred) %>%
  mutate(month="May")

MayWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=MayTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=MayTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=MayTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=May_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)


# Fitting GAMs for mean June temperature -------------------------------------------
JuneWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="6") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="June")


### Model
modJuneTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                        data = JuneWx,
                        method = "REML")
summary(modJuneTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JuneTempMeanPred <- with(JuneWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                             max(waterYear, na.rm=TRUE),
                                                             length.out = 200)))
JuneTempMeanPred <- cbind(JuneTempMeanPred, data.frame(predict(modJuneTempMean$gam, JuneTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JuneTempMeanPred <- transform(JuneTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(month="June")


m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = JuneTempMeanPred$deriv,
                   JuneTempMeanPred$upper,
                   JuneTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modJuneTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

June_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=JuneTempMeanPred$waterYear) %>%
  # left_join(.,JuneTempMeanPred) %>%
  mutate(month="June")

JuneWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=JuneTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=JuneTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=JuneTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=June_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean July temperature -------------------------------------------
JulyWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="7") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="July")


### Model
modJulyTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                        data = JulyWx,
                        method = "REML")
summary(modJulyTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JulyTempMeanPred <- with(JulyWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                             max(waterYear, na.rm=TRUE),
                                                             length.out = 200)))
JulyTempMeanPred <- cbind(JulyTempMeanPred, data.frame(predict(modJulyTempMean$gam, JulyTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JulyTempMeanPred <- transform(JulyTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(month="July")


m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = JulyTempMeanPred$deriv,
                   JulyTempMeanPred$upper,
                   JulyTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modJulyTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

July_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=JulyTempMeanPred$waterYear) %>%
  # left_join(.,JulyTempMeanPred) %>%
  mutate(month="July")

JulyWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()+
  geom_line(data=JulyTempMeanPred, aes(x=waterYear, y=fit)) +
  geom_line(data=JulyTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  geom_line(data=JulyTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  geom_line(data=July_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Aug temperature -------------------------------------------
AugWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="8") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Aug")


### Model
modAugTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = AugWx,
                       method = "REML")
summary(modAugTempMean$gam)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AugTempMeanPred <- with(AugWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
AugTempMeanPred <- cbind(AugTempMeanPred, data.frame(predict(modAugTempMean$gam, AugTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AugTempMeanPred <- transform(AugTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Aug")


m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = AugTempMeanPred$deriv,
                   AugTempMeanPred$upper,
                   AugTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modAugTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Aug_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=AugTempMeanPred$waterYear) %>%
  # left_join(.,AugTempMeanPred) %>%
  mutate(month="Aug")

AugWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=AugTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=AugTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=AugTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=Aug_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Sept temperature -------------------------------------------
SeptWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="9") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Sept")


### Model
modSeptTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                        data = SeptWx,
                        method = "REML")
summary(modSeptTempMean$gam)

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
SeptTempMeanPred <- with(SeptWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                             max(waterYear, na.rm=TRUE),
                                                             length.out = 200)))
SeptTempMeanPred <- cbind(SeptTempMeanPred, data.frame(predict(modSeptTempMean$gam, SeptTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
SeptTempMeanPred <- transform(SeptTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(month="Sept")


m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = SeptTempMeanPred$deriv,
                   SeptTempMeanPred$upper,
                   SeptTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modSeptTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Sept_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=SeptTempMeanPred$waterYear) %>%
  # left_join(.,SeptTempMeanPred) %>%
  mutate(month="Sept")

SeptWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()+
  geom_line(data=SeptTempMeanPred, aes(x=waterYear, y=fit)) +
  geom_line(data=SeptTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  geom_line(data=SeptTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  geom_line(data=Sept_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Oct temperature -------------------------------------------
OctWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="10") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Oct")


### Model
modOctTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = OctWx,
                       method = "REML")
summary(modOctTempMean$gam)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
OctTempMeanPred <- with(OctWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
OctTempMeanPred <- cbind(OctTempMeanPred, data.frame(predict(modOctTempMean$gam, OctTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
OctTempMeanPred <- transform(OctTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Oct")


m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = OctTempMeanPred$deriv,
                   OctTempMeanPred$upper,
                   OctTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modOctTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Oct_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=OctTempMeanPred$waterYear) %>%
  # left_join(.,OctTempMeanPred) %>%
  mutate(month="Oct")

OctWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=OctTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=OctTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=OctTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=Oct_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Nov temperature -------------------------------------------
NovWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="11") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Nov")


### Model
modNovTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = NovWx,
                       method = "REML")
summary(modNovTempMean$gam)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
NovTempMeanPred <- with(NovWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
NovTempMeanPred <- cbind(NovTempMeanPred, data.frame(predict(modNovTempMean$gam, NovTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
NovTempMeanPred <- transform(NovTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Nov")


m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = NovTempMeanPred$deriv,
                   NovTempMeanPred$upper,
                   NovTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modNovTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Nov_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=NovTempMeanPred$waterYear) %>%
  # left_join(.,NovTempMeanPred) %>%
  mutate(month="Nov")

NovWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()+
  geom_line(data=NovTempMeanPred, aes(x=waterYear, y=fit)) +
  geom_line(data=NovTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  geom_line(data=NovTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  geom_line(data=Nov_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Dec temperature -------------------------------------------
DecWx<-met_data_daily_summary %>%
  filter(!waterYear == "1984" &month=="12") %>%
  group_by(waterYear) %>%
  dplyr::summarize(Tave2M=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(month="Dec")


### Model
modDecTempMean <- gamm(Tave2M ~ s(waterYear, k=20),
                       data = DecWx,
                       method = "REML")
summary(modDecTempMean$gam)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(DecWx, data.frame(waterYear = seq(min(waterYear, na.rm=TRUE),
                                                           max(waterYear, na.rm=TRUE),
                                                           length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(month="Dec")


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "waterYear"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "waterYear")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$incr), waterYear=DecTempMeanPred$waterYear) %>%
  # left_join(.,DecTempMeanPred) %>%
  mutate(month="Dec")

DecWx %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point()
  # geom_line(data=DecTempMeanPred, aes(x=waterYear, y=fit)) +
  # geom_line(data=DecTempMeanPred, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=DecTempMeanPred, aes(x=waterYear, y=lower), linetype="dashed") +
  # geom_line(data=Dec_incr, aes(x=waterYear, y=value_pred), color="red", linewidth=1.5)



# >> Combine -----------------------------------------------------------------
theme_MS <- function () {
  theme_base(base_size=18) %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=18, angle=270),
      # strip.text.x = element_text(size=6, angle=45),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}


theme_set(theme_MS())

# Combine...
emp_df<-bind_rows(JanWx, FebWx, MarchWx, AprilWx,
                  MayWx, JuneWx, JulyWx, AugWx, 
                  SeptWx, OctWx, NovWx, DecWx)%>%
  mutate(month=factor(month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
sim_df<-bind_rows(JanTempMeanPred,FebTempMeanPred,MarchTempMeanPred,
                  AprilTempMeanPred,MayTempMeanPred,JuneTempMeanPred,
                  JulyTempMeanPred,AugTempMeanPred,SeptTempMeanPred,
                  OctTempMeanPred,NovTempMeanPred,DecTempMeanPred)%>%
  mutate(month=factor(month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
incr_df<-bind_rows(Jan_incr, Feb_incr, March_incr, April_incr,
                   May_incr, June_incr, July_incr, Aug_incr,
                   Sept_incr, Oct_incr, Nov_incr, Dec_incr)%>%
  mutate(month=factor(month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))


library(ggh4x) 

emp_df %>%
  ggplot(aes(x=waterYear, y=Tave2M))+
  geom_point(size=1, color="grey80", alpha=0.8)+
  geom_line(data=sim_df, aes(x=waterYear, y=fit), linewidth=0.2) +
  geom_ribbon(data=sim_df,aes(ymin = (lower), ymax = (upper), x = waterYear),
              alpha = 0.5, inherit.aes = FALSE, fill="black") +
  # geom_line(data=sim_df, aes(x=waterYear, y=upper), linetype="dashed") +
  # geom_line(data=sim_df, aes(x=waterYear, y=lower), linetype="dashed") +
  geom_line(data=incr_df, aes(x=waterYear, y=value_pred), color="maroon", linewidth=0.2) +
  facet_wrap(~month,ncol=12)+
  # scale_x_continuous(limit = c(1983, 2022),
  #                    breaks = seq(1983, 2022, by = 5),
  #                    minor_breaks = seq(1940, 2020, 20),
  #                    guide = "axis_minor" # add minor ticks
  # )+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        # strip.text.x = element_text(margin = margin(b = -0.9)),
        plot.margin=unit(c(0,0,0,0), "lines"))+
  labs(y="Mean monthly\nair temperature (°C)",
       x="Year")

ggsave(
  "figures/climate_trends/Mean_air_temperature_GAMS.pdf",
  width = 12,
  height = 4,
  units = "in",
  dpi = 600
)
# 

# Get the p-values for the trends
summary(modFebTempMean$gam)$s.pv