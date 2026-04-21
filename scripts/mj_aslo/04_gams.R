#gams to complement marss model

load('data/mj_aslo/LVWS_output_04172026.Rdata')

names(mod.output)

mod.to.analyze <- mod.output[[24]]

#extract marss state estimates------------------------------------------------------------------------------
xhat <- mod.to.analyze$states  

state_df <- data.frame(
  time = colnames(y),            
  t(xhat))                  

colnames(state_df)[-1] <- rownames(y)

# Long format for GAM

state_long <- pivot_longer(
  state_df,
  cols = -time,
  names_to = "site",
  values_to = "no3_state")

# Convert time to proper index if needed
state_long$time <- as.factor(state_long$time)


# merge with covariates ---------------------------------------------------------------------------------
# Example covariate frame (you should already have aligned columns)
cov_df <- data.frame(
  time = colnames(nadp_tin_n_z),
  nadp = as.numeric(nadp_tin_n_z[1, ]),
  temp_upper = as.numeric(temp_z[1, ]),
  temp_lower = as.numeric(temp_z[2, ]),
  precip_upper = as.numeric(precip_z[1, ]),
  precip_lower = as.numeric(precip_z[2, ]),
  pdsi_upper = as.numeric(pdsi_z[1, ]),
  pdsi_lower = as.numeric(pdsi_z[2, ]))

# Join GAM dataset
gam_df <- merge(state_long, cov_df, by = "time")

gam_df$precip <- ifelse(
  gam_df$site %in% c("sky_in_n", "sky_ls", "sky_out", "andrews"),
  gam_df$precip_upper,
  gam_df$precip_lower)

gam_df$temp <- ifelse(
  gam_df$site %in% c("sky_in_n", "sky_ls", "sky_out", "andrews"),
  gam_df$temp_upper,
  gam_df$temp_lower)

gam_df$pdsi <- ifelse(
  gam_df$site %in% c("sky_in_n", "sky_ls", "sky_out", "andrews"),
  gam_df$pdsi_upper,
  gam_df$pdsi_lower)

gam_df$time <- as.numeric(gam_df$time)

#model building ---------------------------------------------------------------------------------------
library(mgcv)

gam0.1 <- gam(no3_state ~   s(nadp) + s(temp) + s(precip), data = gam_df, method = "REML")

gam0.2 <- gam(no3_state ~ s(time), data = gam_df, method="REML")

gam0.1
gam0.2

# Hydrologic event model (Q50)
gam(no3_state ~ 
      s(time) +
      s(q50),
    data = gam_df,
    method = "REML")

gam(residual_no3 ~ 
      s(q50) +
      s(time),
    data = gam_resid_df,
    method = "REML")