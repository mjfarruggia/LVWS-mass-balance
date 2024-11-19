#source scripts 00 -> 01.3:

NADP_trim <- NADP %>%
  select(waterYear, weekofyear, subppt_mm, Ca_mgL, Mg_mgL, K_mgL, NH4_mgL, NO3_mgL, SO4_mgL, Na_mgL) %>%
  pivot_longer(-c(1:3),
               names_to = "deposition_name",
               values_to = "deposition_value_mg_L") %>%
  mutate(rainwater_m3 = (subppt_mm/1000)*loch_ws_size_m2, # over the entire watershed that week
         deposition_value_kg_m3 = deposition_value_mg_L / 1e+6, # Convert  concentration from mg/L to kg/m³
         mass_per_ha = rainwater_m3 * deposition_value_kg_m3) #kg per week across the entire watershed
