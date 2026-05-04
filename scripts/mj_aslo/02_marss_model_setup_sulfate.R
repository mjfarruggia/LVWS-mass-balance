
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
#Process error codes (for Q matrix)
#de = diagonal and equal (all sites have the same process variance)
#bp= by proximity (Two clusters of sites that share process variance, upper and lower, upper = sky + andrews, lower = loch)
#could potentially do 3 clusters of sites... sky, andrews, loch
#du= diagonal and unequal (each site has its own process variance)
#bp_cov = testing a version of Q where errors from sites near one another also covary 
#type = by waterbody type (sites of similar waterbody types share process variance; lake vs. stream .lake = LS sites, stream = Inlet/outlet/andrews creek sites)
#bp_equal = equal variance and covariance

#Note: The second set of letters indicates whether to estimate a single effect of a covariate for all the state processes, or whether to estimate site-specific covariate effects 
#Effect codes
#sh= shared covariate effect among all sites
#sep= separate covariate effects estimated for each site

#UPDATE THIS WHEN ADDING/CHANGING MODEL STRUCTURES!

#Set of models primarily testing different Q structures (e.g. process variance) and C structures (e.g. covariate effects)

mod.names <- c(
  "mod7.1de","mod7.1bp","mod7.1du","mod7.1bp_cov","mod7.1type","mod7.1bp_type_cov","mod7.1bp_stream_sync",  #no covariates
  
  "mod7.2de_sh","mod7.2bp_sh","mod7.2du_sh","mod7.2bp_cov_sh","mod7.2type_sh","mod7.2bp_type_cov_sh","mod7.2bp_stream_sync_sh",
  "mod7.2de_sep","mod7.2bp_sep","mod7.2du_sep","mod7.2bp_cov_sep","mod7.2type_sep","mod7.2bp_type_cov_sep","mod7.2bp_stream_sync_sep", #nadp
  
  "mod7.3de_sh","mod7.3bp_sh","mod7.3du_sh","mod7.3bp_cov_sh","mod7.3type_sh","mod7.3bp_type_cov_sh","mod7.3bp_stream_sync_sh",
  "mod7.3de_sep","mod7.3bp_sep","mod7.3du_sep","mod7.3bp_cov_sep","mod7.3type_sep","mod7.3bp_type_cov_sep","mod7.3bp_stream_sync_sep", #temp
  
  "mod7.4de_sh","mod7.4bp_sh","mod7.4du_sh","mod7.4bp_cov_sh","mod7.4type_sh","mod7.4bp_type_cov_sh","mod7.4bp_stream_sync_sh",
  "mod7.4de_sep","mod7.4bp_sep","mod7.4du_sep","mod7.4bp_cov_sep","mod7.4type_sep","mod7.4bp_type_cov_sep","mod7.4bp_stream_sync_sep", #precip
  
  "mod7.5de_sh","mod7.5bp_sh","mod7.5du_sh","mod7.5bp_cov_sh","mod7.5type_sh","mod7.5bp_type_cov_sh","mod7.5bp_stream_sync_sh",
  "mod7.5de_sep","mod7.5bp_sep","mod7.5du_sep","mod7.5bp_cov_sep","mod7.5type_sep","mod7.5bp_type_cov_sep","mod7.5bp_stream_sync_sep"  #pdsi
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
diag(bp) <- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') # 3 clusters; sky, andrews, loch
diag(bp) <- c('sky_inlet','sky_lake','sky_outlet','andrews','loch_inlet','loch_lake','loch_outlet') # 3 clusters; sky, andrews, loch

# diag(bp) <- c('var.sky','var.sky','var.sky','var.sky','var.sky','var.loch','var.loch','var.loch') #2 clusters; i lumped andrews with sky
bp

#adding bp_cov (errors from sites near one another also covary)
bp_cov <- matrix(list(0), nsites, nsites)
diag(bp_cov)<- c('var.sky','var.sky','var.sky','var.andrews','var.loch','var.loch','var.loch') 

# --- sky cluster (sky in n = 1, sky ls = 2, sky out=3) ---
bp_cov[1,2] <- bp_cov[2,1] <- "cov.sky"
bp_cov[1,3] <- bp_cov[3,1] <- "cov.sky"
bp_cov[2,3] <- bp_cov[3,2] <- "cov.sky"


#andrews by itself (andrews = 4), no off diag covariance

# --- loch cluster (loch in = 5, loch ls = 6, loch out = 7) ---
bp_cov[5,6] <- bp_cov[6,5] <- "cov.loch"
bp_cov[5,7] <- bp_cov[7,5] <- "cov.loch"
bp_cov[6,7] <- bp_cov[7,6] <- "cov.loch"
bp_cov

#---equal var and cov
bp_equal <- matrix(list("cov.all"), nsites, nsites)
diag(bp_equal) <- "var.all"
bp_equal

#---stream vs lake
type <- matrix(list(0), nsites, nsites)
diag(type) <- c('var.stream', 'var.lake', 'var.stream',
                'var.stream',
                'var.stream', 'var.lake', 'var.stream')

# All streams covary equally regardless of position
type[1,3] <- type[3,1] <- "cov.stream"
type[1,4] <- type[4,1] <- "cov.stream"
type[1,5] <- type[5,1] <- "cov.stream"
type[1,7] <- type[7,1] <- "cov.stream"
type[3,4] <- type[4,3] <- "cov.stream"
type[3,5] <- type[5,3] <- "cov.stream"
type[3,7] <- type[7,3] <- "cov.stream"
type[4,5] <- type[5,4] <- "cov.stream"
type[4,7] <- type[7,4] <- "cov.stream"
type[5,7] <- type[7,5] <- "cov.stream"
type[2,6] <- type[6,2] <- "cov.lake"
type



#---spatial+type — same-cluster pairs get a cluster-specific covariance, cross-cluster pairs get a weaker type-only covariance
# proximity-first with type on top (within-cluster spatial baseline; cross-cluster same-type pairs share weaker covariance)
bp_type_cov <- matrix(list(0), nsites, nsites)
diag(bp_type_cov) <- c('var.stream', 'var.lake', 'var.stream',
                       'var.stream',
                       'var.stream', 'var.lake', 'var.stream')

# Spatial baseline — sky cluster (sites 1-3 only) and loch cluster (sites 5-7)
bp_type_cov[1,2] <- bp_type_cov[2,1] <- "cov.sky"
bp_type_cov[1,3] <- bp_type_cov[3,1] <- "cov.sky"
bp_type_cov[2,3] <- bp_type_cov[3,2] <- "cov.sky"
bp_type_cov[5,6] <- bp_type_cov[6,5] <- "cov.loch"
bp_type_cov[5,7] <- bp_type_cov[7,5] <- "cov.loch"
bp_type_cov[6,7] <- bp_type_cov[7,6] <- "cov.loch"

# Cross-cluster same-type covariance on top
# andrews floats — only appears here as a stream, no spatial cluster
bp_type_cov[1,4] <- bp_type_cov[4,1] <- "cov.stream"
bp_type_cov[1,5] <- bp_type_cov[5,1] <- "cov.stream"
bp_type_cov[1,7] <- bp_type_cov[7,1] <- "cov.stream"
bp_type_cov[3,4] <- bp_type_cov[4,3] <- "cov.stream"
bp_type_cov[3,5] <- bp_type_cov[5,3] <- "cov.stream"
bp_type_cov[3,7] <- bp_type_cov[7,3] <- "cov.stream"
bp_type_cov[4,5] <- bp_type_cov[5,4] <- "cov.stream"
bp_type_cov[4,7] <- bp_type_cov[7,4] <- "cov.stream"
bp_type_cov[2,6] <- bp_type_cov[6,2] <- "cov.lake"
bp_type_cov




bp_type_cov <- matrix(list(0), nsites, nsites)

# --- variances (diagonal) ---
diag(bp_type_cov) <- c('sky.stream',  # 1 sky_in_s
                       'sky.lake',    # 2 sky_ls
                       'sky.stream',  # 3 sky_out
                       'andrews.stream',# 4 andrews  (by itself)
                       'loch.stream', # 5 loch_in
                       'loch.lake',   # 6 loch_ls
                       'loch.stream') # 7 loch_out

bp_type_cov[1,3] <- bp_type_cov[3,1] <- "cov.sky.stream"
bp_type_cov[5,7] <- bp_type_cov[7,5] <- "cov.loch.stream"
bp_type_cov[2,6] <- bp_type_cov[6,2] <- "cov.lake"

# sky lake (site 2) and loch lake (site 6) by themselves? or cov with each other?
bp_type_cov









# ---streams are synchronous, lakes doing their own thing (independent)
bp_stream_sync <- matrix(list(0), nsites, nsites)
diag(bp_stream_sync) <- c('var.stream', 'var.lake.sky', 'var.stream',
                          'var.stream',
                          'var.stream', 'var.lake.loch', 'var.stream')

# All streams share a single covariance
bp_stream_sync[1,3] <- bp_stream_sync[3,1] <- "cov.stream"
bp_stream_sync[1,4] <- bp_stream_sync[4,1] <- "cov.stream"
bp_stream_sync[1,5] <- bp_stream_sync[5,1] <- "cov.stream"
bp_stream_sync[1,7] <- bp_stream_sync[7,1] <- "cov.stream"
bp_stream_sync[3,4] <- bp_stream_sync[4,3] <- "cov.stream"
bp_stream_sync[3,5] <- bp_stream_sync[5,3] <- "cov.stream"
bp_stream_sync[3,7] <- bp_stream_sync[7,3] <- "cov.stream"
bp_stream_sync[4,5] <- bp_stream_sync[5,4] <- "cov.stream"
bp_stream_sync[4,7] <- bp_stream_sync[7,4] <- "cov.stream"
bp_stream_sync[5,7] <- bp_stream_sync[7,5] <- "cov.stream"
bp_stream_sync



Q.list <- list('diagonal and equal',bp,'diagonal and unequal', bp_cov, type, bp_type_cov, bp_stream_sync)
Q.list[[2]]

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
nocovar <- matrix(0)
#List of all covariate matrices....UPDATE THIS!!
c.list <- list(nocovar, nadp_sulfate_z,  temp_z, precip_z, pdsi_z)
c.list[[2]]

##C (matrix that maps covariates to the state processes)
#7 states
#No covariates
C_7.1 <- matrix(0,nrow=nsites)


# 2: NADP (1 row: LVWS - single watershed value)
C_7.2_sh  <- matrix(list('dep','dep','dep','dep','dep','dep','dep'), nsites, 1)
C_7.2_sep <- matrix(list('dep.sky_in_s','dep.sky_ls','dep.sky_out',
                         'dep.andrews','dep.loch_in','dep.loch_ls','dep.loch_out'), nsites, 1)

# 3: Temp anomaly (2 rows: temp_upper = sky+andrews [col1], temp_lower = loch [col2])
C_7.3_sh  <- matrix(list('temp_upper','temp_upper','temp_upper','temp_upper',0,0,0,
                         0,0,0,0,'temp_lower','temp_lower','temp_lower'), nsites, 2)
C_7.3_sep <- matrix(list('temp_upper.sky_in_s','temp_upper.sky_ls','temp_upper.sky_out','temp_upper.andrews',0,0,0,
                         0,0,0,0,'temp_lower.loch_in','temp_lower.loch_ls','temp_lower.loch_out'), nsites, 2)

# 4: Precip (2 rows: precip_upper = sky+andrews [col1], precip_lower = loch [col2])
C_7.4_sh  <- matrix(list('precip_upper','precip_upper','precip_upper','precip_upper',0,0,0,
                         0,0,0,0,'precip_lower','precip_lower','precip_lower'), nsites, 2)
C_7.4_sep <- matrix(list('precip_upper.sky_in_s','precip_upper.sky_ls','precip_upper.sky_out','precip_upper.andrews',0,0,0,
                         0,0,0,0,'precip_lower.loch_in','precip_lower.loch_ls','precip_lower.loch_out'), nsites, 2)

# 5: PDSI (2 rows: pdsi_upper = sky+andrews [col1], pdsi_lower = loch [col2])
C_7.5_sh  <- matrix(list('pdsi_upper','pdsi_upper','pdsi_upper','pdsi_upper',0,0,0,
                         0,0,0,0,'pdsi_lower','pdsi_lower','pdsi_lower'), nsites, 2)
C_7.5_sep <- matrix(list('pdsi_upper.sky_in_s','pdsi_upper.sky_ls','pdsi_upper.sky_out','pdsi_upper.andrews',0,0,0,
                         0,0,0,0,'pdsi_lower.loch_in','pdsi_lower.loch_ls','pdsi_lower.loch_out'), nsites, 2)


C.list <- list(C_7.1, C_7.2_sh, C_7.2_sep, C_7.3_sh, C_7.3_sep,C_7.4_sh, C_7.4_sep,C_7.5_sh, C_7.5_sep)
C.list[[7]]

########################################################################################################################################################################################################################################
#nrow = number of models tested, 
#ncol= number of parameter matrices in MARSS equation

#Empty matrix
combos <- matrix(0, nmods, length(mat.names), dimnames = list(mod.names, mat.names))

#B: identity for all models
combos[,1] <- rep(1, nmods)

#Q: cycles through 4 Q structures (de, bp, du, bp_cov) across all 10 groups of 4
combos[, 2] <- rep(1:7, nmods / 7)

#Z: identity for all models
combos[,3] <- rep(1, nmods)

#R: diagonal and equal for all models
combos[,4] <- rep(1, nmods)

#c: which covariate matrix to use
# c: 1=none(7), 2=nadp(14), 3=temp(14), 4=precip(14), 5=pdsi(14)
combos[, 7] <- c(rep(1, 7), rep(2, 14), rep(3, 14), rep(4, 14), rep(5, 14))

# C: covariate effect matrices
combos[, 8] <- c(
  rep(1, 7),           # no covariates
  rep(2, 7), rep(3, 7), # nadp sh, sep
  rep(4, 7), rep(5, 7), # temp sh, sep
  rep(6, 7), rep(7, 7), # precip sh, sep
  rep(8, 7), rep(9, 7))  # pdsi sh, sep

#check to make sure it's correct!
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




# Extract AIC, AICc, logLik, and convergence status from all models
summary_df <- data.frame(
  model     = mod.names,
  covariate = c(rep("none", 7), rep("nadp", 14), rep("temp", 14),
                rep("precip", 14), rep("pdsi", 14)),
  Q_struct  = rep(c("de","bp","du","bp_cov","type","bp_type_cov","bp_stream_sync"), nmods / 7),
  effect    = c(rep("none", 7),
                rep(c(rep("sh", 7), rep("sep", 7)), 4)),
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
   mod.output[[i]] <- tryCatch(MARSS(y, model=mod.list, control=list(maxit=5000)),
    error = function(e) { message("Model ", mod.names[i], " failed: ", e$message); NULL }  )
  names(mod.output)[i] <- mod.names[i] #replace in mod.output if they converged
 }


#Save model output as Rdata file, to use in other scripts:
save(mod.output, file="data/mj_aslo/LVWS_sulfate_output_05042026.Rdata")


