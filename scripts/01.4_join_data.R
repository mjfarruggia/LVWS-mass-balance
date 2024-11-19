#source scripts 00 -> 01.3:

NADP_trim <- NADP %>%
  select(waterYear, weekofyear, subppt_mm, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL, NO3_mgL, SO4_mgL, Na_mgL) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value") %>%
  mutate(deposition_rate_)
