
########################################################################################################################################################################################
#Description: This script runs multivariate state-space models on mean monthly nitrate over x years
#in x streams and lakes within the Loch Vale Watershed. Goal is to quantify relationships with covariates (drivers),
#including air temp, precip, and deposition, and determine the spatial structure of those relationships (if any).

#adapted from adrianne/mj smoke marss code
############################################################################################################################################################################################
library(MARSS)
load('Data/mj_aslo/ts_matrices.Rdata')

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

#bret's deposition
tin_n_bret <- bret_inorg_n_matrix 
  #make matrix 1 row since only 1 value for the watershed
  tin_n_bret <- tin_n_bret[1, , drop = FALSE]
    rownames(tin_n_bret)<- 'LVWS' #one value for the whole watershed
  
  #z score
  the.mean <- apply(tin_n_bret,1, mean, na.rm=TRUE)
  the.sigma <-sqrt( apply(tin_n_bret,1,var, na.rm=TRUE))
  tin_n_bret_z <-(tin_n_bret-the.mean)*(1/the.sigma)#z-score data

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
  "mod8.1de","mod8.1bp","mod8.1du","mod8.1bp_cov",
  
  "mod8.2de_sh","mod8.2bp_sh","mod8.2du_sh","mod8.2bp_cov_sh",
  "mod8.2de_sep","mod8.2bp_sep","mod8.2du_sep","mod8.2bp_cov_sep",
  
  "mod8.3de_sh","mod8.3bp_sh","mod8.3du_sh","mod8.3bp_cov_sh",
  "mod8.3de_sep","mod8.3bp_sep","mod8.3du_sep","mod8.3bp_cov_sep",
  
  "mod8.4de_sh","mod8.4bp_sh","mod8.4du_sh","mod8.4bp_cov_sh",
  "mod8.4de_sep","mod8.4bp_sep","mod8.4du_sep","mod8.4bp_cov_sep"
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
bp_cov[2,5] <- bp_cov[5,2] <- "cov.sky"
bp_cov[2,5] <- bp_cov[5,2] <- "cov.sky"

#andrews by itself (andrews = 5)---
bp_cov[1,2] <- bp_cov[2,1] <- "cov.andrews"

# --- loch cluster (loch in = 6, loch ls = 7, loch out = 8) ---
bp_cov[3,4] <- bp_cov[4,3] <- "cov.loch"
bp_cov[3,8] <- bp_cov[8,3] <- "cov.loch"
bp_cov[4,8] <- bp_cov[8,4] <- "cov.loch"
bp_cov[4,8] <- bp_cov[8,4] <- "cov.loch"
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
c.list <- list(nocovar, nadp_tin_n_z, tin_n_bret_z, temp_z, precip_z, pdsi_z)
c.list[[6]]

##C (matrix that maps covariates to the state processes)
#8 states
#No covariates
C_8.1 <- matrix(0,nrow=nsites)

#nadp tin n 
C_8.2_sh <-  matrix(list('sw','sw',0,0,'sw',0,0,0,'sw','sw',0,'sw'),nsites,2)
C_8.2_sep <- matrix(list('sw.EMLPond1','sw.TOK11',0,0,'sw.EmeraldLake',0,0,0,'sw.TopazPond','sw.TOK30',0,'sw.TopazLake'),
                    nsites,2)
#bret tin n
rownames(smoke_z)
C_8.3_sh <-  matrix(list('smoke','smoke',0,0,'smoke',0,0,0,'smoke','smoke',0,'smoke'),nsites,2)
C_8.3_sep <- matrix(list('smoke.EMLPond1','smoke.TOK11',0,0,'smoke.EmeraldLake',0,0,0,'smoke.TopazPond','smoke.TOK30',0,'smoke.TopazLake'),
                    nsites,2)
#temp
rownames(y)
rownames(pm2.5_z)
C_8.4_sh <-  matrix(list('pm','pm','pm','pm','pm','pm'),nsites,1)
C_8.4_sep <- matrix(list('pm.EMLPond1','pm.TOK11','pm.TopazPond','pm.TOK30','pm.EmeraldLake','pm.TopazLake'),
                    nsites,1)

#precip
rownames(y)
rownames(pm2.5_z)
C_8.4_sh <-  matrix(list('pm','pm','pm','pm','pm','pm'),nsites,1)
C_8.4_sep <- matrix(list('pm.EMLPond1','pm.TOK11','pm.TopazPond','pm.TOK30','pm.EmeraldLake','pm.TopazLake'),
                    nsites,1)

#pdsi
rownames(y)
rownames(pm2.5_z)
C_8.4_sh <-  matrix(list('pm','pm','pm','pm','pm','pm'),nsites,1)
C_8.4_sep <- matrix(list('pm.EMLPond1','pm.TOK11','pm.TopazPond','pm.TOK30','pm.EmeraldLake','pm.TopazLake'),
                    nsites,1)
C.list <- list(C_8.1, C_8.2_sh, C_8.2_sep, C_8.3_sh, C_8.3_sep,C_8.4_sh, C_8.4_sep)
C.list[[7]]

########################################################################################################################################################################################################################################
#nrow = number of models tested, 
#ncol= number of parameter matrices in MARSS equation

#Empty matrix
combos <- matrix(0,nmods, length(mat.names), dimnames=list(mod.names,mat.names))
#B
combos[,1] <- rep(1,nmods)#B=identity
#Q
#combos[,2] <-c(rep(1:3,(nmods/3))) 
combos[,2] <-c(rep(1:4,(nmods/4))) 

#Z
combos[,3] <- c(rep(1,nmods))#Z=identity
#R
combos[,4] <- rep(1,nmods)#start with diagonal and equal
#c:  which covariates to use
combos[,7] <- c(rep(1,4),rep(2,8),rep(3,8),rep(4,8))
#C
combos[,8] <- c(rep(1,4), rep(2,4), rep(3,4),rep(4,4), rep(5,4),rep(8,4),rep(7,4))
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