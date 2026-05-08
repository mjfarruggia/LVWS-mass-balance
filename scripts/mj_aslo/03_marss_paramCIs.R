#######################################################################################################################################################################################################
library(MARSS)


load('data/mj_aslo/LVWS_nitrate_output_05072026.Rdata')


names(mod.output)

mod.to.analyze = mod.output[24]

marss_mle_obj = mod.to.analyze$mod

# output with error estimates around model parameters
paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric', nboot=100) #currently 100, change for final

print(paramCI)



# Extract information from paramCI 
paramCI$coef
coef_order <- names(paramCI$coef)
ML_estimate <- unname(paramCI$coef)

Std_Err <- paramCI$par.se[coef_order]
Lower_CI <- paramCI$par.lowCI[coef_order]
Upper_CI <- paramCI$par.upCI[coef_order]
Est_Bias <- paramCI$par.bias[coef_order]

Unbias_Est <- ML_estimate - Est_Bias
AIC_value <- paramCI$AIC
AICc_value <- paramCI$AICc

# Store in dataframe
paramCI_df_24 <- data.frame(
  Parameter = param_names,
  ML.Estimate = ML_estimate,
  Std.Err = Std_Err,
  Lower_CI = Lower_CI,
  Upper_CI = Upper_CI,
  Est.Bias = Est_Bias,
  Unbias.Est = Unbias_Est,
  AIC = AIC_value,
  AICc = AICc_value
)

paramCI_df_24
paramCI_df_24$Model= names(mod.output[24])

paramCI_df <- paramCI_df_24





#plot effects with ci's 
library(tidyverse)
precip_df <- subset(paramCI_df, grepl("precip", Parameter))

ggplot(precip_df, aes(x = Parameter, y = ML.Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(size = 3) +
  labs(y = "Covariate Effect", x = NULL) +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text())




# plot the sep one too
mod.to.analyze = mod.output[28]

marss_mle_obj = mod.to.analyze$mod

# output with error estimates around model parameters
paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric', nboot=100) #currently 100, change for final

print(paramCI)


coef_order <- names(paramCI$coef)

ML_estimate <- unname(paramCI$coef)



# ---- FIX: flatten block-structured outputs ----
se_list  <- paramCI$par.se
low_list <- paramCI$par.lowCI
up_list  <- paramCI$par.upCI
bias_list <- paramCI$par.bias


Std_Err <- c(
  R.diag = se_list$R,
  Q.var.sky = se_list$Q[1],
  Q.cov.sky = se_list$Q[2],
  Q.var.andrews = se_list$Q[3],
  Q.var.loch = se_list$Q[4],
  Q.cov.loch = se_list$Q[5],
  C.precip_upper.sky_in_n = se_list$U[1],
  C.precip_upper.sky_ls = se_list$U[2],
  C.precip_upper.sky_out = se_list$U[3],
  C.precip_upper.andrews = se_list$U[4],
  C.precip_lower.loch_in = se_list$U[5],
  C.precip_lower.loch_ls = se_list$U[6]
)

Lower_CI <- c(
  R.diag = low_list$R,
  Q.var.sky = low_list$Q[1],
  Q.cov.sky = low_list$Q[2],
  Q.var.andrews = low_list$Q[3],
  Q.var.loch = low_list$Q[4],
  Q.cov.loch = low_list$Q[5],
  C.precip_upper.sky_in_n = low_list$U[1],
  C.precip_upper.sky_ls = low_list$U[2],
  C.precip_upper.sky_out = low_list$U[3],
  C.precip_upper.andrews = low_list$U[4],
  C.precip_lower.loch_in = low_list$U[5],
  C.precip_lower.loch_ls = low_list$U[6]
)

Upper_CI <- c(
  R.diag = up_list$R,
  Q.var.sky = up_list$Q[1],
  Q.cov.sky = up_list$Q[2],
  Q.var.andrews = up_list$Q[3],
  Q.var.loch = up_list$Q[4],
  Q.cov.loch = up_list$Q[5],
  C.precip_upper.sky_in_n = up_list$U[1],
  C.precip_upper.sky_ls = up_list$U[2],
  C.precip_upper.sky_out = up_list$U[3],
  C.precip_upper.andrews = up_list$U[4],
  C.precip_lower.loch_in = up_list$U[5],
  C.precip_lower.loch_ls = up_list$U[6]
)

Est_Bias <- c(
  R.diag = bias_list$R,
  Q.var.sky = bias_list$Q[1],
  Q.cov.sky = bias_list$Q[2],
  Q.var.andrews = bias_list$Q[3],
  Q.var.loch = bias_list$Q[4],
  Q.cov.loch = bias_list$Q[5],
  C.precip_upper.sky_in_n = bias_list$U[1],
  C.precip_upper.sky_ls = bias_list$U[2],
  C.precip_upper.sky_out = bias_list$U[3],
  C.precip_upper.andrews = bias_list$U[4],
  C.precip_lower.loch_in = bias_list$U[5],
  C.precip_lower.loch_ls = bias_list$U[6]
)



# ---- align everything to coefficient order ----
Std_Err   <- Std_Err[coef_order]
Lower_CI  <- Lower_CI[coef_order]
Upper_CI  <- Upper_CI[coef_order]
Est_Bias  <- Est_Bias[coef_order]

Unbias_Est <- ML_estimate - Est_Bias

AIC_value <- paramCI$AIC
AICc_value <- paramCI$AICc



# ---- Store in dataframe ----
paramCI_df_28 <- data.frame(
  Parameter = coef_order,
  ML.Estimate = ML_estimate,
  Std.Err = Std_Err,
  Lower_CI = Lower_CI,
  Upper_CI = Upper_CI,
  Est.Bias = Est_Bias,
  Unbias.Est = Unbias_Est,
  AIC = AIC_value,
  AICc = AICc_value
)

paramCI_df_28$Model = names(mod.output[28])

# paramCI_df <- paramCI_df_28


# ---- Plot effects with CI's ----
library(tidyverse)

precip_df <- subset(paramCI_df_28, grepl("precip", Parameter))

ggplot(precip_df, aes(x = Parameter, y = ML.Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(size = 3) +
  labs(y = "Covariate Effect", x = NULL) +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))











#plot top 6 models, no CI's first

# Extract AIC, AICc, logLik, and convergence status from all models
summary_df <- data.frame(
  model     = mod.names,
  covariate = c(rep("none",4), rep("nadp",8), rep("temp",8), rep("precip",8), rep("pdsi",8)),
  Q_struct  = rep(c("de","bp","du","bp_cov"), nmods/4),
  effect    = c(rep("none",4), rep(c(rep("sh",4), rep("sep",4)), 4)),
  logLik    = sapply(mod.output, function(m) m$logLik),
  AIC       = sapply(mod.output, function(m) m$AIC),
  AICc      = sapply(mod.output, function(m) m$AICc),
  converged = sapply(mod.output, function(m) m$convergence == 0),
  stringsAsFactors = FALSE
)
summary_df <- summary_df[order(summary_df$AICc),]
summary_df$delta_AICc <- summary_df$AICc - min(summary_df$AICc)

top6 <- head(summary_df, 5)
top2 <- head(summary_df, 2)

# extract C estimates for each top model
library(tidyverse)

extract_C <- function(mod_name) {
  m <- mod.output[[mod_name]]
  C_est <- coef(m, type = "vector")
  C_est <- C_est[grepl("^C\\.", names(C_est))]
  if(length(C_est) == 0) return(NULL)
  
  df <- data.frame(param = names(C_est),estimate = as.numeric(C_est),model= mod_name, stringsAsFactors = FALSE)
  
  df %>%
    mutate(n_dots = stringr::str_count(param, "\\."),
      covariate_row = sub("^C\\.([^.]+)\\..*", "\\1", param),
      site = ifelse(n_dots >= 2,sub("^C\\.[^.]+\\.", "", param),"all sites")) %>%
    select(-n_dots)
}

C_all <- map_dfr(top2$model, extract_C) %>%
  left_join(top6[, c("model","covariate","effect","delta_AICc","converged")], by = "model") %>%
  mutate(
    model = factor(model, levels = top6$model),
    model_label = paste0(model, "\nΔAICc=", round(delta_AICc, 1)),
    model_label = factor(model_label, levels = unique(model_label)),
    site = factor(site, levels = c("sky_in_n","sky_in_s","sky_ls","sky_out",
                                   "andrews","loch_in","loch_ls","loch_out")))

# plot
ggplot(C_all, aes(y = site, x = estimate, color = covariate_row)) +
  geom_point(size = 3) +
  geom_segment(aes(y = site, yend = site, x = 0, xend = estimate, color = covariate_row)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~model_label, ncol = 2, scales = "free_x") +
  labs(y = "Site", x = "effect est") +
  theme_bw(base_size=16) +
  theme(strip.text = element_text(size = 8),legend.position = "bottom")
