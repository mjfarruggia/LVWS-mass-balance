#######################################################################################################################################################################################################
library(MARSS)


load('data/mj_aslo/LVWS_output_04172026.Rdata')


names(mod.output)

mod.to.analyze = mod.output[24]

marss_mle_obj = mod.to.analyze$mod

# output with error estimates around model parameters
paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric', nboot=100) #currently 100, change for final

print(paramCI)


# Extract information from paramCI 
param_names <- names(paramCI$coef)
ML_estimate <- unname(paramCI$coef)
order <- c("R", "Q", "U")

par.se.unlisted <- unlist(paramCI$par.se)
par.se_reordered <- par.se.unlisted[order]
Std_Err <- unname(par.se_reordered)

par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
par.lowCI_reordered <- par.lowCI.unlisted[order]
Lower_CI <- unname(par.lowCI_reordered)

par.upCI.unlisted <- unlist(paramCI$par.upCI)
par.upCI_reordered <- par.upCI.unlisted[order]
Upper_CI <- unname(par.upCI_reordered)

par.bias.unlisted <- unlist(paramCI$par.bias)
par.bias_reordered <- par.bias.unlisted[order]
Est_Bias <- unname(par.bias_reordered)

Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
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

C_all <- map_dfr(top6$model, extract_C) %>%
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
  theme_bw() +
  theme(strip.text = element_text(size = 8),legend.position = "bottom")
