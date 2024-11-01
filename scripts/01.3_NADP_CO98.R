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
  mutate(dateOn = date(dateTimeOn),
         dateOff = date(dateTimeOff),
         weekofyear = week(dateOn),
         waterYear = calcWaterYear(dateOn))


#Preview data
NADP %>%
  ggplot(aes(x=weekofyear, y= subppt_mm))+
  geom_point() +
  facet_wrap(~waterYear)
