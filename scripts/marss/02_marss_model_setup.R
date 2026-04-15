
########################################################################################################################################################################################
#Description: This script runs multivariate state-space models on mean monthly nitrate over x years
#in x streams and lakes within the Loch Vale Watershed. Goal is to quantify relationships with covariates (drivers),
#including air temp, precip, and deposition, and determine the spatial structure of those relationships (if any).

#adapted from adrianne/mj smoke marss code
############################################################################################################################################################################################
library(MARSS)
load('Data/marss/ts_matrices.Rdata')

#######################################################################################################################################################################################################
#response variable (observations)
y <- no3_matrix
dim(y)
row.names(y) 
#number of different response time series
nsites <- nrow(y)


#Z-score covariate data so effect magnitudes can be compared directly
#nadp
nadp_tin_n <- nadp_tin_n_matrix
rownames(nadp_tin_n) #theres two values for the whole watershed, based on 1 nadp station but precip from 2 diff gridmet cells
  #make matrix 1 row since only 1 value for the watershed
nadp_tin_n <- nadp_tin_n[1, , drop = FALSE]
  rownames(nadp_tin_n)<- 'LVWS' #one value for the whole watershed
  the.mean <- apply(nadp_tin_n,1, mean, na.rm=TRUE)
  the.sigma <-sqrt( apply(nadp_tin_n,1,var, na.rm=TRUE))
  nadp_tin_n_z <-(nadp_tin_n-the.mean)*(1/the.sigma)#z-score data

#bret's deposition - stops in 2022 and only has summer
# tin_n_bret <- bret_inorg_n_matrix
#   #make matrix 1 row since only 1 value for the watershed
#   tin_n_bret <- tin_n_bret[1, , drop = FALSE]
#     rownames(tin_n_bret)<- 'LVWS' #one value for the whole watershed
# 
#   #z score
#   the.mean <- apply(tin_n_bret,1, mean, na.rm=TRUE)
#   the.sigma <-sqrt( apply(tin_n_bret,1,var, na.rm=TRUE))
#   tin_n_bret_z <-(tin_n_bret-the.mean)*(1/the.sigma)#z-score data
#   tin_n_bret_z <- tin_n_bret_z[, !is.na(colnames(tin_n_bret_z)) & colnames(tin_n_bret_z) != "NA", drop = FALSE]
  
#mean temp
temp <- temp_matrix
  #make matrix 2 rows since only 2 values for the watershed
  temp <- temp[c(1, 8), , drop = FALSE]
  rownames(temp) <- c('temp_upper','temp_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
  the.mean <- apply(temp,1, mean, na.rm=TRUE)
  the.sigma <-sqrt( apply(temp,1,var, na.rm=TRUE))
  temp_z <-(temp-the.mean)*(1/the.sigma)#z-score data


#total monthly precip
precip <- totalprecip_matrix
  #make matrix 2 rows since only 2 values for the watershed
  precip <- precip[c(1, 8), , drop = FALSE]
  rownames(precip) <- c('precip_upper','precip_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
  the.mean <- apply(precip,1, mean, na.rm=TRUE)
  the.sigma <-sqrt( apply(precip,1,var, na.rm=TRUE))
  precip_z <-(precip-the.mean)*(1/the.sigma)#z-score data


#mean pdsi
 pdsi <- pdsi_matrix
  #make matrix 2 rows since only 2 values for the watershed
 pdsi <- pdsi[c(1, 8), , drop = FALSE]
  rownames(pdsi) <- c('pdsi_upper','pdsi_lower') #theres two values for the whole watershed from 2 diff gridmet cells. If replacing with lvws met data, this will be one value only.
  the.mean <- apply(pdsi,1, mean, na.rm=TRUE)
  the.sigma <-sqrt( apply(pdsi,1,var, na.rm=TRUE))
  pdsi_z <-(pdsi-the.mean)*(1/the.sigma)#z-score data



#variance should be 1 for all the covariates
apply(nadp_tin_n_z,1,var, na.rm = TRUE)
apply(tin_n_bret_z,1,var, na.rm = TRUE)
apply(temp_z,1,var, na.rm = TRUE)
apply(precip_z,1,var, na.rm = TRUE)
apply(pdsi_z,1,var, na.rm = TRUE)

#check that they're all the same length/dates
colnames(y)[1]; colnames(y)[ncol(y)]; ncol(y)
colnames(nadp_tin_n_z)[1]; colnames(nadp_tin_n_z)[ncol(nadp_tin_n_z)]; ncol(nadp_tin_n_z)
colnames(tin_n_bret_z)[1]; colnames(tin_n_bret_z)[ncol(tin_n_bret_z)]; ncol(tin_n_bret_z)
colnames(temp_z)[1]; colnames(temp_z)[ncol(temp_z)]; ncol(temp_z)
colnames(precip_z)[1]; colnames(precip_z)[ncol(precip_z)]; ncol(precip_z)
colnames(pdsi_z)[1]; colnames(pdsi_z)[ncol(pdsi_z)]; ncol(pdsi_z)

######################################################################################################################################################################################################
##Create inputs to MARSS function (matrices):
#Create names of models to run: (list) ###UPDATE THIS WHEN TRYING NEW SETS OF MODELS!!!!!!
#Name Format: 'mod', first number, '.','second number','first letter','_','second letters'

#Note: First number indicates number of states. In this analysis we assume 8 independent states (one per site) 
#     (Example: mod1.1 estimates one underlying state process, mod3.1 indicates three state processes)
#Note: Second number indicates which covariates are included in model 

# We will test whether covariate effects are shared across sites, or if there are site-specific effects (can try more later)
#Covariate Codes:
#1 = no covariates
#2 = nadp TIN
#3 = Bret's calculated TIN
#4 = temp (monthly avg)
#5 = precip (monthly avg)
#6 = pdsi (monthly avg)

#Note: First Letter indicates the process error structure in the model (environmental variability):
#Process error codes (for Q matrix)
#de = diagonal and equal (all sites have the same process variance)
#bp= by proximity (Two clusters of sites that share process variance, upper and lower, upper = sky + andrews, lower = loch)
  #could potentially do 3 clusters of sites... sky, andrews, loch
#du= diagonal and unequal (each site has its own process variance)
#bp_cov = testing a version of Q where errors from sites near one another also covary        

#Note: The second set of letters indicates whether to estimate a single effect of a covariate for all the state processes, 
#or whether to estimate site-specific covariate effects 
#Effect codes
#sh= shared covariate effect among all sites
#sep= separate covariate effects estimated for each site

#UPDATE THIS WHEN ADDING/CHANGING MODEL STRUCTURES!

#Set of models primarily testing different Q structures (e.g. process variance) and C structures (e.g. covariate effects)

mod.names <- c(
  "mod8.1de","mod8.1bp","mod8.1du","mod8.1bp_cov",                 #no covariates
  
  "mod8.2de_sh","mod8.2bp_sh","mod8.2du_sh","mod8.2bp_cov_sh",
  "mod8.2de_sep","mod8.2bp_sep","mod8.2du_sep","mod8.2bp_cov_sep", #nadp
  
  # "mod8.3de_sh","mod8.3bp_sh","mod8.3du_sh","mod8.3bp_cov_sh",
  # "mod8.3de_sep","mod8.3bp_sep","mod8.3du_sep","mod8.3bp_cov_sep", #bret's deposition
  
  "mod8.4de_sh","mod8.4bp_sh","mod8.4du_sh","mod8.4bp_cov_sh",     #temp     
  "mod8.4de_sep","mod8.4bp_sep","mod8.4du_sep","mod8.4bp_cov_sep",
  
  "mod8.5de_sh","mod8.5bp_sh","mod8.5du_sh","mod8.5bp_cov_sh",     #precip
  "mod8.5de_sep","mod8.5bp_sep","mod8.5du_sep","mod8.5bp_cov_sep",
  
  "mod8.6de_sh","mod8.6bp_sh","mod8.6du_sh","mod8.6bp_cov_sh",    #pdsi
  "mod8.6de_sep","mod8.6bp_sep","mod8.6du_sep","mod8.6bp_cov_sep"
)
#Number of models
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
bp <- matrix(list(0),nsites,nsites)
diag(bp) <- c('var.sky','var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') # 3 clusters; sky, andrews, loch
  # diag(bp) <- c('var.sky','var.sky','var.sky','var.sky','var.sky','var.loch','var.loch','var.loch') #2 clusters; i lumped andrews with sky
bp

#adding bp_cov (errors from sites near one another also covary)
bp_cov <- matrix(list(0), nsites, nsites)
diag(bp_cov)<- c('var.sky','var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') 

# --- sky cluster (sky in n = 1, sky in s =2, sky ls = 3, sky out=4) ---
bp_cov[1,2] <- bp_cov[2,1] <- "cov.sky"
bp_cov[1,3] <- bp_cov[3,1] <- "cov.sky"
bp_cov[1,4] <- bp_cov[4,1] <- "cov.sky"
bp_cov[2,3] <- bp_cov[3,2] <- "cov.sky"
bp_cov[2,4] <- bp_cov[4,2] <- "cov.sky"
bp_cov[3,4] <- bp_cov[4,3] <- "cov.sky"

#andrews by itself (andrews = 5), no off diag covariance

# --- loch cluster (loch in = 6, loch ls = 7, loch out = 8) ---
bp_cov[6,7] <- bp_cov[7,6] <- "cov.loch"
bp_cov[6,8] <- bp_cov[8,6] <- "cov.loch"
bp_cov[7,8] <- bp_cov[8,7] <- "cov.loch"
bp_cov

Q.list <- list('diagonal and equal',bp,'diagonal and unequal', bp_cov)
Q.list[[2]]

##Z (maps observation time series to state processes)
#8 states (each site is a separate state process):
Z.8 <- 'identity'
Z.list <- list(Z.8)

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
nocovar <- matrix(0)
#List of all covariate matrices....UPDATE THIS!!
c.list <- list(nocovar, nadp_tin_n_z,  temp_z, precip_z, pdsi_z)
c.list[[2]]

##C (matrix that maps covariates to the state processes)
#8 states
#No covariates
C_8.1 <- matrix(0,nrow=nsites)


# 2: NADP TIN (1 row: LVWS - single watershed value)
C_8.2_sh  <- matrix(list('tin','tin','tin','tin','tin','tin','tin','tin'), nsites, 1)
C_8.2_sep <- matrix(list('tin.sky_in_n','tin.sky_in_s','tin.sky_ls','tin.sky_out',
                         'tin.andrews','tin.loch_in','tin.loch_ls','tin.loch_out'), nsites, 1)

# # 3: Bret's TIN (1 row: LVWS - single watershed value)
# C_8.3_sh  <- matrix(list('bret_tin','bret_tin','bret_tin','bret_tin','bret_tin','bret_tin','bret_tin','bret_tin'), nsites, 1)
# C_8.3_sep <- matrix(list('bret_tin.sky_in_n','bret_tin.sky_in_s','bret_tin.sky_ls','bret_tin.sky_out',
#                          'bret_tin.andrews','bret_tin.loch_in','bret_tin.loch_ls','bret_tin.loch_out'), nsites, 1)

# 4: Temp (2 rows: temp_upper = sky+andrews [col1], temp_lower = loch [col2])
C_8.4_sh  <- matrix(list('temp_upper','temp_upper','temp_upper','temp_upper','temp_upper',0,0,0,
                         0,0,0,0,0,'temp_lower','temp_lower','temp_lower'), nsites, 2)
C_8.4_sep <- matrix(list('temp_upper.sky_in_n','temp_upper.sky_in_s','temp_upper.sky_ls','temp_upper.sky_out','temp_upper.andrews',0,0,0,
                         0,0,0,0,0,'temp_lower.loch_in','temp_lower.loch_ls','temp_lower.loch_out'), nsites, 2)

# 5: Precip (2 rows: precip_upper = sky+andrews [col1], precip_lower = loch [col2])
C_8.5_sh  <- matrix(list('precip_upper','precip_upper','precip_upper','precip_upper','precip_upper',0,0,0,
                         0,0,0,0,0,'precip_lower','precip_lower','precip_lower'), nsites, 2)
C_8.5_sep <- matrix(list('precip_upper.sky_in_n','precip_upper.sky_in_s','precip_upper.sky_ls','precip_upper.sky_out','precip_upper.andrews',0,0,0,
                         0,0,0,0,0,'precip_lower.loch_in','precip_lower.loch_ls','precip_lower.loch_out'), nsites, 2)

# 6: PDSI (2 rows: pdsi_upper = sky+andrews [col1], pdsi_lower = loch [col2])
C_8.6_sh  <- matrix(list('pdsi_upper','pdsi_upper','pdsi_upper','pdsi_upper','pdsi_upper',0,0,0,
                         0,0,0,0,0,'pdsi_lower','pdsi_lower','pdsi_lower'), nsites, 2)
C_8.6_sep <- matrix(list('pdsi_upper.sky_in_n','pdsi_upper.sky_in_s','pdsi_upper.sky_ls','pdsi_upper.sky_out','pdsi_upper.andrews',0,0,0,
                         0,0,0,0,0,'pdsi_lower.loch_in','pdsi_lower.loch_ls','pdsi_lower.loch_out'), nsites, 2)


C.list <- list(C_8.1, C_8.2_sh, C_8.2_sep, C_8.4_sh, C_8.4_sep,C_8.5_sh, C_8.5_sep,C_8.6_sh, C_8.6_sep)
C.list[[7]]

########################################################################################################################################################################################################################################
#nrow = number of models tested, 
#ncol= number of parameter matrices in MARSS equation

#Empty matrix
combos <- matrix(0, nmods, length(mat.names), dimnames = list(mod.names, mat.names))

#B: identity for all models
combos[,1] <- rep(1, nmods)

#Q: cycles through 4 Q structures (de, bp, du, bp_cov) across all 10 groups of 4
combos[,2] <- rep(1:4, nmods/4)

#Z: identity for all models
combos[,3] <- rep(1, nmods)

#R: diagonal and equal for all models
combos[,4] <- rep(1, nmods)

#c: which covariate matrix to use
# 1=none(4 mods), 2=nadp(8), 3=bret(8), 4=temp(8), 5=precip(8), 6=pdsi(8)
combos[,7] <- c(rep(1,4), rep(2,8), rep(3,8), rep(4,8), rep(5,8))

#C: which C matrix to use
# no covar: C_8.1(1)
# nadp:     C_8.2_sh(2), C_8.2_sep(3)
# bret:     C_8.3_sh(4), C_8.3_sep(5)
# temp:     C_8.4_sh(6), C_8.4_sep(7)
# precip:   C_8.5_sh(8), C_8.5_sep(9)
# pdsi:     C_8.6_sh(10),C_8.6_sep(11)
combos[,8] <- c(rep(1,4),
                rep(2,4), rep(3,4),  # nadp sh, sep
                rep(4,4), rep(5,4),  # temp sh, sep
                rep(6,4), rep(7,4),  # precip sh, sep
                rep(8,4), rep(9,4))  # pdsi sh, sep

#check to make sure it's correct!
combos

####################################################################################################################################################################################################################################################
# This loop runs the MARSS function for all the combinations of model parameters contained in the matrix 'combos'
#and stores the output in the list 'mod.output'
for(i in 29:nrow(combos)){
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




# Extract AIC, AICc, logLik, and convergence status from all models
summary_df <- data.frame(
  model    = mod.names,
  covariate = c(rep("none",4), rep("nadp",8), rep("temp",8), rep("precip",8), rep("pdsi",8)),
  Q_struct  = rep(c("de","bp","du","bp_cov"), nmods/4),
  effect    = c(rep("none",4),
                rep(c(rep("sh",4), rep("sep",4)), 4)),
  logLik   = sapply(mod.output, function(m) m$logLik),
  AIC      = sapply(mod.output, function(m) m$AIC),
  AICc     = sapply(mod.output, function(m) m$AICc),
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
  names(mod.output)[i] <- mod.names[i] #replace in mod.output if they converged

}


#Save model output as Rdata file, to use in other scripts:
save(mod.output, file="Data/marss/LVWS_output_04082026.Rdata")


