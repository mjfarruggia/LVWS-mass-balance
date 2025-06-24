## Is annual deposition correlated with annual export?
## In a rush so not sure what scripts need to be run for this to work out

head(annual_dep)

head(annual_flux)

annual_dep_wide <- annual_dep %>%
  mutate(deposition_name = case_match(
    deposition_name,
    # "inorganicN_mgL" ~ "inorganic N",
    "NH4-N" ~ "nh4_n_dep" ,
    "NO3-N" ~ "no3_n_dep",
    "SO4-S" ~ "so4_s_dep",
    "Inorganic N" ~ "total_n_dep",
    "cations" ~ "cation_dep",
    .default = deposition_name
  ))  %>%
  pivot_wider(names_from = deposition_name,
              values_from = deposition_kg_per_ha_impute)

annual_flux_wide <- annual_flux %>%
  filter(!chem_name %in% c("temp_c","po4_mgl","tdn_mgl")) %>%
  select(-annual_flux_kg) %>%
  mutate(chem_name = case_match(
    chem_name,
    # "inorganicN_mgL" ~ "inorganic N",
    "NH4-N" ~ "nh4_n_export" ,
    "NO3-N" ~ "no3_n_export",
    "SO4-S" ~ "so4_s_export",
    "Inorganic N" ~ "total_n_export",
    "cations" ~ "cation_export",
    "DOC" ~ "DOC_export",
    .default = chem_name
  ))  %>%
  pivot_wider(names_from = chem_name,
              values_from = annual_flux_kg_per_ha)

mean_annual_T_wide <- met_data_daily_summary %>%
  filter(waterYear >= "1991" & waterYear < "2022") %>%
  group_by(waterYear) %>%
  dplyr::summarize(mean_annual_airT=mean(Tave2M, na.rm=TRUE)) %>%
  mutate(mean_annual_airT_lag1 = lag(mean_annual_airT, n=1))


mean_annual_T_wide %>%
  ggplot(aes(x=waterYear, y=mean_annual_airT))+
  geom_point()

annual_dep_wide_lagged <- annual_dep_wide %>%
  arrange(waterYear) %>%
  ungroup() %>%
  mutate(total_n_dep_lag1 = lag(total_n_dep, n=1),
         no3_n_dep_lag1 = lag(no3_n_dep, n=1)) %>%
  select(waterYear, total_n_dep_lag1, no3_n_dep_lag1 )

annual_correlations <-annual_flux_wide %>%
  full_join(., annual_dep_wide) %>%
  full_join(., annual_dep_wide_lagged) %>%
  full_join(., mean_annual_T_wide) %>%
  ungroup() %>%
  filter(waterYear >= "1991" & waterYear < "2022") 


corr <- round(cor(annual_correlations), 1)
corr

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(annual_correlations)
p.mat

# Visualize the correlation matrix
ggcorrplot(corr,
           type = "lower",
           lab = TRUE)

GGally::ggpairs(annual_correlations %>% select(contains("no3"),contains("total_n"),mean_annual_airT_lag1,mean_annual_airT)) +
  theme_few(base_size = 8)

# Net N export...


annual_correlations %>%
  mutate(net_n_export = (nh4_n_export+no3_n_export) - total_n_dep) %>%
  ggplot(aes(x=waterYear, y=net_n_export))+
  geom_point(shape=21, size=2, fill="grey50")+
  geom_hline(yintercept=0)+
  labs(y="Net N export (kg/ha)")

annual_correlations %>%
  mutate(net_n_export = (nh4_n_export+no3_n_export) - total_n_dep) %>%
  ggplot(aes(x=mean_annual_airT_lag1, y=net_n_export, fill=factor(waterYear)))+
  geom_point(shape=21, size=2)+
  labs(y="Net N export (kg/ha)")



annual_correlations %>%
  mutate(net_s_export = (so4_s_export) - so4_s_dep) %>%
  ggplot(aes(x=waterYear, y=net_s_export))+
  geom_point(shape=21, size=2, fill="grey50")+
  geom_hline(yintercept=0)+
  labs(y="Net S export (kg/ha)")

annual_correlations %>%
  mutate(net_cat_export = (cation_export) - cation_dep) %>%
  ggplot(aes(x=waterYear, y=net_cat_export))+
  geom_point(shape=21, size=2, fill="grey50")+
  geom_hline(yintercept=0)+
  labs(y="Net cation export (kg/ha)")
