NADP <- read_csv(here("data/NTN-CO98-1984-2024.csv")) %>%
  mutate(dateTimeOn = mdy_hm(dateOn),
         dateTimeOff = mdy_hm(dateOff)) %>%
  select(-c(dateOn, dateOff)) %>%
  rename(subppt_mm = subppt,
         Ca_mgL = Ca,
         Mg_mgL = Mg,
         K_mgL = K,
         Na_mgL = na,
         NH4_mgL = NH4,
         NO3_mgL = NO3,
         Cl_mgL = Cl,
         SO4_mgL = SO4) %>%
  replace_with_na_all(condition = ~.x == -9) %>%
  replace_with_na_all(condition = ~.x == -9.990) %>%
  mutate(dateOn = date(dateTimeOn),
         dateOff = date(dateTimeOff),
         # Calculate the midpoint
         midpoint = dateOn + as.duration((dateOff - dateOn) / 2),
         start_date_plus_4 = dateOn + days(4),
         waterYear = calcWaterYear(start_date_plus_4),
         weekofyear = week(start_date_plus_4),
         year = year(start_date_plus_4))
         

as.duration((NADP$dateOff - NADP$dateOn) / 2)

#These came from Bret.Schichtel@colostate.edu
NADP_bret <- read_csv(here("data/WeeklyData_exp_subPrism22.csv")) %>%
  mutate(date=ymd(AvgDate),
         weekofyear=week(date),
         waterYear = calcWaterYear(date)) %>%
  rename(ppt_mm_bret=`weekly prec mm`,
         NO3_mgL_bret = `NO3 mg/l`,
         NH4_mgL_bret = `NH4 mg/l`,
         IN_mgL_bret = `IN mg/l`)

compare <- NADP_bret %>% 
  select(waterYear, date, weekofyear, ppt_mm_bret, NO3_mgL_bret) %>%
  left_join(NADP %>% select(midpoint,start_date_plus_4,  waterYear, dateOn, weekofyear, subppt_mm, NO3_mgL), by=c("waterYear","weekofyear"))


#How different?
compare %>%
  ggplot(aes(x=ppt_mm_bret, y=subppt_mm))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)

#How different?
compare %>%
  ggplot(aes(x=NO3_mgL_bret, y=NO3_mgL))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)
# Okay, so NO3 concentrations are largely, unchanged, it is ppt that is often underestimated, thus messing with deposition.
# I'll go ahead and use the ppt he used, and that'll also filled a few gaps in the N species

NADP <- NADP_bret %>%
  select(date, weekofyear, ppt_mm_bret, NO3_mgL_bret, NH4_mgL_bret, IN_mgL_bret) %>%
  left_join(., NADP, by=c("date"="start_date_plus_4","weekofyear"))
