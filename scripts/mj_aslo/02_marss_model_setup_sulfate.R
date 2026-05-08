
########################################################################################################################################################################################
#Description: This script runs multivariate state-space models on mean monthly nitrate over x years
#in x streams and lakes within the Loch Vale Watershed. Goal is to quantify relationships with covariates (drivers),
#including climate and deposition, and determine the spatial structure of those relationships (if any).

#adapted from adrianne/mj smoke marss code
############################################################################################################################################################################################
library(MARSS)
load('data/mj_aslo/ts_matrices.Rdata')

#######################################################################################################################################################################################################
#response variable (observations) - SULFATE
y <- monthly_so4_matrix
dim(y)
row.names(y) 
#number of different response time series
nsites <- nrow(y)


#Z-score covariate data so effect magnitudes can be compared directly
#nadp
nadp_sulfate <- nadp_sulfate_matrix
rownames(nadp_sulfate) #theres two values for the whole watershed, based on 1 nadp station but precip from 2 diff gridmet cells
#make matrix 1 row since only 1 value for the watershed
nadp_sulfate <- nadp_sulfate[1, , drop = FALSE]
rownames(nadp_sulfate)<- 'LVWS' #one value for the whole watershed
the.mean <- apply(nadp_sulfate,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(nadp_sulfate,1,var, na.rm=TRUE))
nadp_sulfate_z <-(nadp_sulfate-the.mean)*(1/the.sigma)#z-score data


#temp anomaly (monthly)
temp <- temp_anomaly_bysite
#make matrix 2 rows since only 2 values for the watershed
temp <- temp[c(1, 7), , drop = FALSE]
rownames(temp) <- c('temp_upper','temp_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
the.mean <- apply(temp,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(temp,1,var, na.rm=TRUE))
temp_z <-(temp-the.mean)*(1/the.sigma)#z-score data


#total monthly precip
precip <- totalprecip_matrix
#make matrix 2 rows since only 2 values for the watershed
precip <- precip[c(1, 7), , drop = FALSE]
rownames(precip) <- c('precip_upper','precip_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
the.mean <- apply(precip,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(precip,1,var, na.rm=TRUE))
precip_z <-(precip-the.mean)*(1/the.sigma)#z-score data


#mean pdsi
pdsi <- pdsi_matrix
#make matrix 2 rows since only 2 values for the watershed
pdsi <- pdsi[c(1, 7), , drop = FALSE]
rownames(pdsi) <- c('pdsi_upper','pdsi_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
the.mean <- apply(pdsi,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(pdsi,1,var, na.rm=TRUE))
pdsi_z <-(pdsi-the.mean)*(1/the.sigma)#z-score data


#days since Q50
q50 <- q50_matrix
rownames(q50)<- 'LVWS' #one value for the whole watershed
the.mean <- apply(q50,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(q50,1,var, na.rm=TRUE))
q50_z <-(q50-the.mean)*(1/the.sigma)#z-score data


#variance should be 1 for all the covariates
apply(nadp_sulfate,1,var, na.rm = TRUE)
apply(temp_z,1,var, na.rm = TRUE)
apply(precip_z,1,var, na.rm = TRUE)
apply(pdsi_z,1,var, na.rm = TRUE)

#check that they're all the same length/dates
colnames(y)[1]; colnames(y)[ncol(y)]; ncol(y)
colnames(nadp_sulfate)[1]; colnames(nadp_sulfate)[ncol(nadp_sulfate)]; ncol(nadp_sulfate)
colnames(temp_z)[1]; colnames(temp_z)[ncol(temp_z)]; ncol(temp_z)
colnames(precip_z)[1]; colnames(precip_z)[ncol(precip_z)]; ncol(precip_z)
colnames(pdsi_z)[1]; colnames(pdsi_z)[ncol(pdsi_z)]; ncol(pdsi_z)

######################################################################################################################################################################################################
##Create inputs to MARSS function (matrices):
#Create names of models to run: (list) ###UPDATE THIS WHEN TRYING NEW SETS OF MODELS!!!!!!
#Name Format: 'mod', first number, '.','second number','first letter','_','second letters'

#Note: First number indicates number of states. In this analysis we assume 7 independent states (one per site) 
#     (Example: mod1.1 estimates one underlying state process, mod3.1 indicates three state processes)
#Note: Second number indicates which covariates are included in model 

# We will test whether covariate effects are shared across sites, or if there are site-specific effects (can try more later)
#Covariate Codes:
#1 = no covariates
#2 = nadp 
#3 = temp (monthly anomaly)
#4 = precip (monthly avg)
#5 = pdsi (monthly avg)


#Note: First set of letters indicate the process error structure in the model (environmental variability):
#Process error codes (for Q matrix) - 9 structures total:
#de       = diagonal and equal     (equal var, no cov)
#du       = diagonal and unequal   (var by site, no cov)
#eq       = equal var and cov      (full synchrony)
#bp       = by proximity           (var by cluster, no cov)
#bp_cov   = by proximity + cov     (var by cluster, cov by cluster)
#cl_cov   = within-cluster cov     (var by site, pairwise cov within watershed)
#flow     = flow-connected         (var by site, cov all sky x loch + andrews x loch)
#flow_out = flow-connected outlet  (var by site, cov sky outlet x loch + andrews x loch)

#Note: The second set of letters indicates whether to estimate a single effect of a covariate for all the state processes, or whether to estimate site-specific covariate effects 
#Effect codes
#sh= shared covariate effect among all sites
#sep= separate covariate effects estimated for each site

#UPDATE THIS WHEN ADDING/CHANGING MODEL STRUCTURES!

#Set of models primarily testing different Q structures (e.g. process variance) and C structures (e.g. covariate effects)

mod.names <- c(
  "mod6.1de","mod6.1du","mod6.1eq","mod6.1bp","mod6.1bp_cov","mod6.1cl_cov",         # no covariates
  
  "mod6.2de_sh","mod6.2du_sh","mod6.2eq_sh","mod6.2bp_sh","mod6.2bp_cov_sh","mod6.2cl_cov_sh",
  "mod6.2de_sep","mod6.2du_sep","mod6.2eq_sep","mod6.2bp_sep","mod6.2bp_cov_sep","mod6.2cl_cov_sep", # nadp
  
  "mod6.3de_sh","mod6.3du_sh","mod6.3eq_sh","mod6.3bp_sh","mod6.3bp_cov_sh","mod6.3cl_cov_sh",
  "mod6.3de_sep","mod6.3du_sep","mod6.3eq_sep","mod6.3bp_sep","mod6.3bp_cov_sep","mod6.3cl_cov_sep", # temp
  
  "mod6.4de_sh","mod6.4du_sh","mod6.4eq_sh","mod6.4bp_sh","mod6.4bp_cov_sh","mod6.4cl_cov_sh",
  "mod6.4de_sep","mod6.4du_sep","mod6.4eq_sep","mod6.4bp_sep","mod6.4bp_cov_sep","mod6.4cl_cov_sep", # precip
  
  "mod6.5de_sh","mod6.5du_sh","mod6.5eq_sh","mod6.5bp_sh","mod6.5bp_cov_sh","mod6.5cl_cov_sh",
  "mod6.5de_sep","mod6.5du_sep","mod6.5eq_sep","mod6.5bp_sep","mod6.5bp_cov_sep","mod6.5cl_cov_sep"  # pdsi
)
nmods <- length(mod.names)  # should be 54
#Number of models: 6 Q structures x (1 no-covariate + 4 covariates x 2 effects) = 6 x 9 = 54
nmods <- length(mod.names)

#Names of matrix inputs to MARSS equation:
mat.names <- c('B', 'Q','Z', 'R','A','U','c','C')

#Empty list to put model outputs into:
mod.output <- list()


######################################################################################################################################################################################################
##Create vectors, matrices, or lists of parameter matrices:

##B (degree of mean reversion)- in this case set to 1
b1 <- 'identity'
B.list <- list(b1)

##Q (process error matrix)
#Create a Q matrix where sites in close proximity share process variance 
#(e.g. sky pond cluster and a loch cluster. or a sky, andrews, loch cluster.)
rownames(y)


#diagonal equal---------------------
# equal var, no cov; MARSS shorthand, no matrix needed
de <- 'diagonal and equal'

#diagonal unequal---------------------
# var by site, no cov; MARSS shorthand, no matrix needed
du <- 'diagonal and unequal'

#equal var and cov (full synchrony)---------------------
# equal var, equal cov; MARSS shorthand, no matrix needed
eq <- 'equalvarcov'

#by proximity (3 clusters - sky, andrews, loch)---------------------
bp <- matrix(list(0),nsites,nsites)
diag(bp) <- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') # 3 clusters; sky, andrews, loch
# diag(bp) <- c('var.sky','var.sky','var.sky','var.sky','var.sky','var.loch','var.loch','var.loch') #2 clusters; i lumped andrews with sky
bp

#bp_cov (by proxmity clusters, plus errors from sites near one another also covary) -------------------------
bp_cov <- matrix(list(0), nsites, nsites)
diag(bp_cov)<- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') 
#sky cluster (sky in n = 1, sky ls = 2, sky out=3) 
bp_cov[1,2] <- bp_cov[2,1] <- "cov.sky"
bp_cov[1,3] <- bp_cov[3,1] <- "cov.sky"
bp_cov[2,3] <- bp_cov[3,2] <- "cov.sky"
#andrews by itself (andrews = 4), no off diag covariance
# loch cluster (loch in = 5, loch ls = 6, loch out = 7) 
bp_cov[5,6] <- bp_cov[6,5] <- "cov.loch"
bp_cov[5,7] <- bp_cov[7,5] <- "cov.loch"
bp_cov[6,7] <- bp_cov[7,6] <- "cov.loch"
bp_cov



#within-cluster covariance---
cluster_cov <- matrix(list(0), nsites, nsites)
diag(cluster_cov)<- c('var.sky.in','var.sky.lake','var.sky.out','var.andrews','var.loch.in','var.loch.lake','var.loch.out') 
#sky cluster (sky in n = 1, sky ls = 2, sky out=3) 
cluster_cov[1,2] <- cluster_cov[2,1] <- "cov.sky.lake.in"
cluster_cov[1,3] <- cluster_cov[3,1] <- "cov.sky.in.out"
cluster_cov[2,3] <- cluster_cov[3,2] <- "cov.sky.lake.out"
#andrews by itself (andrews = 4), no off diag covariance
# loch cluster (loch in = 5, loch ls = 6, loch out = 7) 
cluster_cov[5,6] <- cluster_cov[6,5] <- "cov.loch.in.lake"
cluster_cov[5,7] <- cluster_cov[7,5] <- "cov.loch.in.out"
cluster_cov[6,7] <- cluster_cov[7,6] <- "cov.loch.lake.out"
cluster_cov

# 
#this one broke when running it...
# nest <- matrix("cov_global", nsites, nsites)
# diag(nest)<- c('var.sky.in','var.sky.lake','var.sky.out','var.andrews','var.loch.in','var.loch.lake','var.loch.out') 
# nest


# 
# # flow_cov -----
# flow_cov <- matrix(list(0), nsites, nsites)
# diag(flow_cov)<- c('var.sky.in','var.sky.lake','var.sky.out','var.andrews','var.loch.in','var.loch.lake','var.loch.out') 
# 
# # sky x loch (all pairs, cols 1-3 x rows 5-7)
# flow_cov[1,5] <- flow_cov[5,1] <- "cov.sky.in.loch.in"
# flow_cov[1,6] <- flow_cov[6,1] <- "cov.sky.in.loch.lake"
# flow_cov[1,7] <- flow_cov[7,1] <- "cov.sky.in.loch.out"
# flow_cov[2,5] <- flow_cov[5,2] <- "cov.sky.lake.loch.in"
# flow_cov[2,6] <- flow_cov[6,2] <- "cov.sky.lake.loch.lake"
# flow_cov[2,7] <- flow_cov[7,2] <- "cov.sky.lake.loch.out"
# flow_cov[3,5] <- flow_cov[5,3] <- "cov.sky.out.loch.in"
# flow_cov[3,6] <- flow_cov[6,3] <- "cov.sky.out.loch.lake"
# flow_cov[3,7] <- flow_cov[7,3] <- "cov.sky.out.loch.out"
# # andrews x loch (all pairs, row/col 4 x rows 5-7)
# flow_cov[4,5] <- flow_cov[5,4] <- "cov.andrews.loch.in"
# flow_cov[4,6] <- flow_cov[6,4] <- "cov.andrews.loch.lake"
# flow_cov[4,7] <- flow_cov[7,4] <- "cov.andrews.loch.out"
# flow_cov
# 
# 
# flow_cov_out <- matrix(list(0), nsites, nsites)
# diag(flow_cov_out) <- c('var.sky.in', 'var.sky.lake', 'var.sky.out',
#                         'var.andrews',
#                         'var.loch.in', 'var.loch.lake', 'var.loch.out')
# 
# # sky outlet x loch only (row/col 3 x rows 5-7)
# flow_cov_out[3,5] <- flow_cov_out[5,3] <- "cov.sky.out.loch.in"
# flow_cov_out[3,6] <- flow_cov_out[6,3] <- "cov.sky.out.loch.lake"
# flow_cov_out[3,7] <- flow_cov_out[7,3] <- "cov.sky.out.loch.out"
# 
# # andrews x loch (all pairs)
# flow_cov_out[4,5] <- flow_cov_out[5,4] <- "cov.andrews.loch.in"
# flow_cov_out[4,6] <- flow_cov_out[6,4] <- "cov.andrews.loch.lake"
# flow_cov_out[4,7] <- flow_cov_out[7,4] <- "cov.andrews.loch.out"




Q.list <- list(
  'diagonal and equal',   # equal var, no cov
  'diagonal and unequal', # var by site, no cov
  'equalvarcov',          # full synchrony - equal var, equal cov
  bp,                     # var by cluster, no cov
  bp_cov,                 # var by cluster, cov by cluster
  cluster_cov            # var by site, pairwise cov by site
  # nest
  # flow_cov,               # var by site, cov all sky x loch; andrews x loch
  # flow_cov_out            # var by site, cov sky outlet x loch; andrews x loch only
)

Q.list[[1]]

##Z (maps observation time series to state processes)
#7 states (each site is a separate state process):
Z.7 <- 'identity'
Z.list <- list(Z.7)

##R (observation error matrix)
##To start, set observation error to be equal at all sites
R.list <- list('diagonal and equal')
R.list[[1]]

##A 
A.list <- 'zero'

##U 
U.list <- 'zero'

#xO (initial conditions)
x0.model <- 'zero'

#VO (intitial conditions)
V0.model <- 'zero'

##c: 
#different sets of covariate matrices
#no covariates
nocovar <- matrix(0, nrow = 1, ncol = ncol(y))
#List of all covariate matrices....UPDATE THIS!!
c.list <- list(nocovar, nadp_sulfate_z,  temp_z, precip_z, pdsi_z)
c.list[[2]]

##C (matrix that maps covariates to the state processes)
#7 states
#No covariates
C_7.1 <- matrix(0, nrow = nsites, ncol = 1)


# 2: NADP dep (1 row: LVWS - single watershed value)
C_7.2_sh  <- matrix(list('dep','dep','dep','dep','dep','dep','dep'), nsites, 1)
C_7.2_sep <- matrix(list('dep.sky_in','dep.sky_ls','dep.sky_out',
                         'dep.andrews','dep.loch_in','dep.loch_ls','dep.loch_out'), nsites, 1)

# 3: Temp anomaly (2 rows: temp_upper = sky+andrews [col1], temp_lower = loch [col2])
C_7.3_sh  <- matrix(list('temp_upper','temp_upper','temp_upper','temp_upper',0,0,0,
                         0,0,0,0,'temp_lower','temp_lower','temp_lower'), nsites, 2)
C_7.3_sep <- matrix(list('temp_upper.sky_in','temp_upper.sky_ls','temp_upper.sky_out','temp_upper.andrews',0,0,0,
                         0,0,0,0,'temp_lower.loch_in','temp_lower.loch_ls','temp_lower.loch_out'), nsites, 2)

# 4: Precip (2 rows: precip_upper = sky+andrews [col1], precip_lower = loch [col2])
C_7.4_sh  <- matrix(list('precip_upper','precip_upper','precip_upper','precip_upper',0,0,0,
                         0,0,0,0,'precip_lower','precip_lower','precip_lower'), nsites, 2)
C_7.4_sep <- matrix(list('precip_upper.sky_in','precip_upper.sky_ls','precip_upper.sky_out','precip_upper.andrews',0,0,0,
                         0,0,0,0,'precip_lower.loch_in','precip_lower.loch_ls','precip_lower.loch_out'), nsites, 2)

# 5: PDSI (2 rows: pdsi_upper = sky+andrews [col1], pdsi_lower = loch [col2])
C_7.5_sh  <- matrix(list('pdsi_upper','pdsi_upper','pdsi_upper','pdsi_upper',0,0,0,
                         0,0,0,0,'pdsi_lower','pdsi_lower','pdsi_lower'), nsites, 2)
C_7.5_sep <- matrix(list('pdsi_upper.sky_in','pdsi_upper.sky_ls','pdsi_upper.sky_out','pdsi_upper.andrews',0,0,0,
                         0,0,0,0,'pdsi_lower.loch_in','pdsi_lower.loch_ls','pdsi_lower.loch_out'), nsites, 2)


C.list <- list(C_7.1, C_7.2_sh, C_7.2_sep, C_7.3_sh, C_7.3_sep,C_7.4_sh, C_7.4_sep,C_7.5_sh, C_7.5_sep)
C.list[[7]]

########################################################################################################################################################################################################################################
#nrow = number of models tested, 
#ncol= number of parameter matrices in MARSS equation
# Empty matrix
combos <- matrix(0, nmods, length(mat.names),
                 dimnames = list(mod.names, mat.names))

# B: identity for all models
combos[, 1] <- rep(1, nmods)

# Q: 6 Q structures
combos[, 2] <- rep(1:6, length.out = nmods)

# Z: identity for all models
combos[, 3] <- rep(1, nmods)

# R: diagonal and equal for all models
combos[, 4] <- rep(1, nmods)

# c: which covariate matrix to use
combos[, 7] <- c(
  rep(1, 6),          # no covariates
  rep(2, 12),         # nadp
  rep(3, 12),         # temp
  rep(4, 12),         # precip
  rep(5, 12)          # pdsi
)

# C: covariate effect matrices
combos[, 8] <- c(
  rep(1, 6),              # no covariates
  rep(2, 6), rep(3, 6),   # nadp: sh, sep
  rep(4, 6), rep(5, 6),   # temp: sh, sep
  rep(6, 6), rep(7, 6),   # precip: sh, sep
  rep(8, 6), rep(9, 6)    # pdsi: sh, sep
)

# check
combos
####################################################################################################################################################################################################################################################
# This loop runs the MARSS function for all the combinations of model parameters contained in the matrix 'combos'
#and stores the output in the list 'mod.output'
for(i in 1:nrow(combos)){
  #select model structure and parameters
  #change this so that it uses combo matrix to index correct parameters
  mod.list <- list(B=B.list[[combos[i,1]]], Q=Q.list[[combos[i,2]]], Z=Z.list[[combos[i,3]]], R=R.list[[combos[i,4]]] ,
                   A=A.list, U=U.list, c=c.list[[combos[i,7]]], C=C.list[[combos[i,8]]], x0=x0.model, V0=V0.model, tinitx=1)
  #MARSS function call, can change number of iterations if desired
  mod <- MARSS(y, model=mod.list, control=list(maxit=1000))
  #put MARSS output into a list
  mod.output[[i]] <- mod 
  #give the model output the right name
  names(mod.output)[i] <- mod.names[i]
}



##############################################################################################################################################################################################################################################################
#Check what's in mod.output
mod.output[[4]]
names(mod.output)




summary_df <- data.frame(
  model     = mod.names,
  covariate = c(rep("none", 6), rep("nadp", 12), rep("temp", 12),
                rep("precip", 12), rep("pdsi", 12)),
  Q_struct  = rep(c("de","du","eq","bp","bp_cov","cl_cov"), nmods / 6),
  effect    = c(rep("none", 6),
                rep(c(rep("sh", 6), rep("sep", 6)), 4)),
  logLik    = sapply(mod.output, function(m) m$logLik),
  AIC       = sapply(mod.output, function(m) m$AIC),
  AICc      = sapply(mod.output, function(m) m$AICc),
  converged = sapply(mod.output, function(m) m$convergence == 0),
  stringsAsFactors = FALSE
)

summary_df <- summary_df[order(summary_df$AICc), ]
summary_df$delta_AICc <- summary_df$AICc - min(summary_df$AICc)





# rerun non-converged models with higher maxit
# no_conv <- which(sapply(mod.output, function(m) m$convergence != 0))
# names(mod.output)[no_conv]  # check which ones

# for(i in no_conv){
#   mod.list <- list(B=B.list[[combos[i,1]]], Q=Q.list[[combos[i,2]]], Z=Z.list[[combos[i,3]]], R=R.list[[combos[i,4]]],
#                    A=A.list, U=U.list, c=c.list[[combos[i,7]]], C=C.list[[combos[i,8]]], x0=x0.model, V0=V0.model, tinitx=1)
#   mod.output[[i]] <- tryCatch(
#     MARSS(y, model=mod.list, control=list(maxit=5000)),
#     error = function(e) { message("Model ", mod.names[i], " failed: ", e$message); NULL }
#   )
#   names(mod.output)[i] <- mod.names[i] #replace in mod.output if they converged
# 
# }


#Save model output as Rdata file, to use in other scripts:
save(mod.output, file="data/mj_aslo/LVWS_sulfate_output_05072026.Rdata")