
########################################################################################################################################################################################
#DAILY SCALE MODELS
#Description: This script runs multivariate state-space models on DAILY nitrate over x years
#in x streams and lakes within the Loch Vale Watershed. Goal is to quantify relationships with covariates (drivers),
#including climate and discharge, and determine the spatial structure of those relationships (if any).

#adapted from adrianne/mj smoke marss code
############################################################################################################################################################################################
library(MARSS)
load('data/mj_aslo/ts_matrices.Rdata')

#######################################################################################################################################################################################################
#response variable (observations)
y <- daily_no3_matrix
dim(y)
row.names(y) 
#number of different response time series
nsites <- nrow(y)


#Z-score covariate data so effect magnitudes can be compared directly


#temp anomaly (daily)
temp <- temp_anomaly_bysite_daily
#make matrix 2 rows since only 2 values for the watershed
temp <- temp[c(1, 7), , drop = FALSE]
rownames(temp) <- c('temp_upper','temp_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
the.mean <- apply(temp,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(temp,1,var, na.rm=TRUE))
temp_z <-(temp-the.mean)*(1/the.sigma)#z-score data


#days since Q50
q50 <- q50_matrix
rownames(q50)<- 'LVWS' #one value for the whole watershed
the.mean <- apply(q50,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(q50,1,var, na.rm=TRUE))
q50_z <-(q50-the.mean)*(1/the.sigma)#z-score data


#variance should be 1 for all the covariates
apply(temp_z,1,var, na.rm = TRUE)
apply(q50_z,1,var, na.rm = TRUE)

#check that they're all the same length/dates
colnames(y)[1]; colnames(y)[ncol(y)]; ncol(y)
colnames(temp_z)[1]; colnames(temp_z)[ncol(temp_z)]; ncol(temp_z)
colnames(q50_z)[1]; colnames(q50_z)[ncol(q50_z)]; ncol(q50_z)


######################################################################################################################################################################################################
##Create inputs to MARSS function (matrices):
#Create names of models to run: (list) ###UPDATE THIS WHEN TRYING NEW SETS OF MODELS!!!!!!
#Name Format: 'mod', first number, '.','second number','first letter','_','second letters'

#Note: First number indicates number of states. In this analysis we assume 7 independent states (one per site)
#     (Example: mod1.1 estimates one underlying state process, mod3.1 indicates three state processes)
#Note: Second number indicates which covariates are included in model

# We will test whether covariate effects are shared across sites, or if there are site-specific effects
#Covariate Codes:
#1 = no covariates
#2 = temp (daily anomaly)
#3 = q50 (days since Q50 in water year)

#Note: First Letter indicates the process error structure in the model (environmental variability):
#Process error codes (for Q matrix)
#de = diagonal and equal (all sites have the same process variance)
#bp = by proximity (three clusters: sky, andrews, loch)
#du = diagonal and unequal (each site has its own process variance)
#bp_cov = errors from sites near one another also covary

#Note: The second set of letters indicates whether to estimate a single effect of a covariate for all
#the state processes, or whether to estimate site-specific covariate effects
#Effect codes
#sh  = shared covariate effect among all sites
#sep = separate covariate effects estimated for each site

#UPDATE THIS WHEN ADDING/CHANGING MODEL STRUCTURES!

#Set of models testing different Q structures (process variance) and C structures (covariate effects)
mod.names <- c(
  "mod7.1de","mod7.1bp","mod7.1du","mod7.1bp_cov",                 #no covariates
  
  "mod7.2de_sh","mod7.2bp_sh","mod7.2du_sh","mod7.2bp_cov_sh",
  "mod7.2de_sep","mod7.2bp_sep","mod7.2du_sep","mod7.2bp_cov_sep", #temp
  
  "mod7.3de_sh","mod7.3bp_sh","mod7.3du_sh","mod7.3bp_cov_sh",
  "mod7.3de_sep","mod7.3bp_sep","mod7.3du_sep","mod7.3bp_cov_sep"  #q50
)

#Number of models
nmods <- length(mod.names)

#Names of matrix inputs to MARSS equation:
mat.names <- c('B', 'Q','Z', 'R','A','U','c','C')

#Empty list to put model outputs into:
mod.output <- list()
#####################################################################################################################################################################################################
##Create vectors, matrices, or lists of parameter matrices:

##B (degree of mean reversion) - set to identity (random walk)
b1 <- 'identity'
B.list <- list(b1)

##Q (process error matrix)
#Create a Q matrix where sites in close proximity share process variance
#3 clusters: sky (sky_in_n, sky_ls, sky_out), andrews, loch (loch_in, loch_ls, loch_out)
rownames(y)
bp <- matrix(list(0), nsites, nsites)
diag(bp) <- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch')
bp

#bp_cov: errors from sites in the same cluster also covary
bp_cov <- matrix(list(0), nsites, nsites)
diag(bp_cov) <- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch')

# --- sky cluster (sky in n = 1, sky ls = 2, sky out = 3) ---
bp_cov[1,2] <- bp_cov[2,1] <- "cov.sky"
bp_cov[1,3] <- bp_cov[3,1] <- "cov.sky"
bp_cov[2,3] <- bp_cov[3,2] <- "cov.sky"

#andrews by itself (andrews = 4), no off-diagonal covariance

# --- loch cluster (loch in = 5, loch ls = 6, loch out = 7) ---
bp_cov[5,6] <- bp_cov[6,5] <- "cov.loch"
bp_cov[5,7] <- bp_cov[7,5] <- "cov.loch"
bp_cov[6,7] <- bp_cov[7,6] <- "cov.loch"
bp_cov

Q.list <- list('diagonal and equal', bp, 'diagonal and unequal', bp_cov)

##Z (maps observation time series to state processes)
#7 states - each site is a separate state process
Z.7 <- 'identity'
Z.list <- list(Z.7)

##R (observation error matrix)
#observation error equal at all sites
R.list <- list('diagonal and equal')

##A
A.list <- 'zero'

##U
U.list <- 'zero'

#x0 (initial conditions)
x0.model <- 'zero'

#V0 (initial conditions)
V0.model <- 'zero'

##c: covariate data matrices
#no covariates
nocovar <- matrix(0)
#List of all covariate matrices
c.list <- list(nocovar, temp_z, q50_z)

##C (matrix that maps covariates to the state processes)
#7 states

#1: No covariates
C_7.1 <- matrix(0, nrow=nsites)

# 2: Temp anomaly (2 rows: temp_upper = sky+andrews [col1], temp_lower = loch [col2])
C_7.2_sh  <- matrix(list('temp_upper','temp_upper','temp_upper','temp_upper',0,0,0,
                         0,0,0,0,0,'temp_lower','temp_lower','temp_lower'), nsites, 2)
C_7.2_sep <- matrix(list('temp_upper.sky_in_n','temp_upper.sky_ls','temp_upper.sky_out','temp_upper.andrews',0,0,0,
                         0,0,0,0,0,'temp_lower.loch_in','temp_lower.loch_ls','temp_lower.loch_out'), nsites, 2)

# 3: Q50 (1 row: LVWS - single watershed value)
C_7.3_sh  <- matrix(list('q50','q50','q50','q50','q50','q50','q50'), nsites, 1)
C_7.3_sep <- matrix(list('q50.sky_in_n','q50.sky_ls','q50.sky_out',
                         'q50.andrews','q50.loch_in','q50.loch_ls','q50.loch_out'), nsites, 1)

C.list <- list(C_7.1, C_7.2_sh, C_7.2_sep, C_7.3_sh, C_7.3_sep)

########################################################################################################################################################################################################################################
#nrow = number of models tested,
#ncol = number of parameter matrices in MARSS equation

#Empty matrix
combos <- matrix(0, nmods, length(mat.names), dimnames = list(mod.names, mat.names))

#B: identity for all models
combos[,1] <- rep(1, nmods)

#Q: cycles through 4 Q structures (de, bp, du, bp_cov) across all groups of 4
combos[,2] <- rep(1:4, nmods/4)

#Z: identity for all models
combos[,3] <- rep(1, nmods)

#R: diagonal and equal for all models
combos[,4] <- rep(1, nmods)

#c: which covariate data matrix to use
# 1=none(4 mods), 2=temp(8), 3=q50(8)
combos[,7] <- c(rep(1,4), rep(2,8), rep(3,8))

#C: which covariate effect matrix to use
# no covar: C_7.1(1)
# temp:     C_7.2_sh(2), C_7.2_sep(3)
# q50:      C_7.3_sh(4), C_7.3_sep(5)
combos[,8] <- c(rep(1,4),           # no covariates: C_7.1
                rep(2,4), rep(3,4),  # temp sh, sep
                rep(4,4), rep(5,4))  # q50 sh, sep

#check to make sure it's correct!
combos

####################################################################################################################################################################################################################################################
# This loop runs the MARSS function for all combinations of model parameters in 'combos'
# and stores the output in the list 'mod.output'
for(i in 1:nrow(combos)){
  mod.list <- list(B=B.list[[combos[i,1]]], Q=Q.list[[combos[i,2]]], Z=Z.list[[combos[i,3]]], R=R.list[[combos[i,4]]],
                   A=A.list, U=U.list, c=c.list[[combos[i,7]]], C=C.list[[combos[i,8]]], x0=x0.model, V0=V0.model, tinitx=1)
  mod <- MARSS(y, model=mod.list, control=list(maxit=1000))
  mod.output[[i]] <- mod
  names(mod.output)[i] <- mod.names[i]
}

##############################################################################################################################################################################################################################################################
#Check what's in mod.output
mod.output[[4]]
names(mod.output)

# Extract AIC, AICc, logLik, and convergence status from all models
summary_df <- data.frame(
  model     = mod.names,
  covariate = c(rep("none",4), rep("temp",8), rep("q50",8)),
  Q_struct  = rep(c("de","bp","du","bp_cov"), nmods/4),
  effect    = c(rep("none",4),
                rep(c(rep("sh",4), rep("sep",4)), 2)),
  logLik    = sapply(mod.output, function(m) m$logLik),
  AIC       = sapply(mod.output, function(m) m$AIC),
  AICc      = sapply(mod.output, function(m) m$AICc),
  converged = sapply(mod.output, function(m) m$convergence == 0),
  stringsAsFactors = FALSE
)

# rank by AICc
summary_df <- summary_df[order(summary_df$AICc),]
summary_df$delta_AICc <- summary_df$AICc - min(summary_df$AICc)

# rerun non-converged models with higher maxit
no_conv <- which(sapply(mod.output, function(m) m$convergence != 0))
names(mod.output)[no_conv]  # check which ones

for(i in no_conv){
  mod.list <- list(B=B.list[[combos[i,1]]], Q=Q.list[[combos[i,2]]], Z=Z.list[[combos[i,3]]], R=R.list[[combos[i,4]]],
                   A=A.list, U=U.list, c=c.list[[combos[i,7]]], C=C.list[[combos[i,8]]], x0=x0.model, V0=V0.model, tinitx=1)
  mod.output[[i]] <- tryCatch(
    MARSS(y, model=mod.list, control=list(maxit=5000)),
    error = function(e) { message("Model ", mod.names[i], " failed: ", e$message); NULL }
  )
  names(mod.output)[i] <- mod.names[i]
}

#Save model output as Rdata file, to use in other scripts:
save(mod.output, file="Data/marss/LVWS_daily_output_04172026.Rdata")