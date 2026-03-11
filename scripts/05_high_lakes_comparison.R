FS_lakes <- read_csv("data/USFS_regional2_summer_lake_surface_chem_20250618MJF.csv")
head(FS_lakes)

FS_lakes %>%
  ggplot(aes(x= year, y=z_SO4_mgL, color=lake_ID))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position="none") +
  labs(title="original data")

#Re-calculate z-score
FS_lakes %>%
  group_by(lake_ID) %>%
  mutate(z_SO4_mgL=scale(SO4_mgL)) %>%
  ggplot(aes(x= year, y=SO4_mgL, color=lake_ID))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position="none")+
  labs(title="group by lake_ID for z-score")+
  facet_wrap(~lake_ID, scales="free_y")


## get the trends and add that as a category for graphing
map_lm <- function(df){
  mod <- lm(df$value ~ df$year)
}


lm_slope <- function(mod) {
  mod$coefficients[[2]] # pull out slope
}



FS_nested <- FS_lakes %>%
  group_by(lake_ID) %>%
  mutate(value=scale(SO4_mgL)) %>%
  select(lake_ID, year, value) %>%
  nest() %>%
  mutate(
    lm = map(data, map_lm),
    lm_sum = map(lm, broom::glance),
    slope = map(lm, lm_slope)
    # zyp_mod = map(data, map_zyp),
    # intercept = map(zyp_mod, sens_intercept)
  )


## Un-nest 
FS_unnested = unnest(FS_nested, c(lm_sum, slope)) %>%
  mutate(
    trend = case_when(
      p.value <= 0.05 & slope >= 0 ~ 'increasing',
      p.value <= 0.05 & slope <= 0 ~ 'decreasing',
      p.value > 0.05 ~ 'no trend'
    ),
    trend = factor(trend,
                   levels = c('no trend',
                              'increasing',
                              'decreasing'))
  )

#Extract all covariates for modeling...
FS_unnested_for_join <- FS_unnested %>%
  select(lake_ID, trend) %>%
  left_join(., FS_lakes %>%
              group_by(lake_ID)%>%
              mutate(value=scale(SO4_mgL)) %>%
              select(lake_ID, year, value))


FS_unnested_for_join %>%
  group_by(trend) %>%
  mutate(n = length(unique(lake_ID))) %>%
  ungroup() %>%
  mutate(facet_label = paste0(trend, " (n = ", n, ")")) %>%
  ggplot(aes(x = year, y = value, group = lake_ID)) +
  geom_point(shape=21, size=2, color="black", fill="grey50") +
  geom_smooth(method = "lm", color="black") +
  theme(legend.position = "none") +
  facet_wrap(~facet_label, ncol = 2) +
  labs(y = "z-score SO4 (mg/L)",
       x = "Year")+
  theme_few(base_size = 22)
  # scale_x_continuous(breaks=c(1980,1990,20))

ggsave(
  "figures/USFS_Region2_comparison.pdf",
  dpi = 600,
  width = 8,
  height = 7,
  units = "in"
)
