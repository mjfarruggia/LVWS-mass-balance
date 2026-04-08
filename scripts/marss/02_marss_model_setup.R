#mj note - still in process of updating, don't run

########################################################################################################################################################################################
#Description: This script runs multivariate state-space models on mean daily water temperature from a single summer
#in six lakes/ponds within the Tokopah Basin. Goal is to quantify relationships with covariates (drivers),
#including shortwave radiation, smoke density, and PM2.5, and determine the spatial structure of those relationships (if any).
############################################################################################################################################################################################
library(MARSS)
load('Data/marss/ts_matrices.Rdata')
#######################################################################################################################################################################################################
#response variable (observations)
y <- no3_matrix
dim(y)
row.names(y) <- c('EMLPond1','TOK11','TopazPond','TOK30','EmeraldLake','TopazLake')
#number of different response time series
nsites <- nrow(y)


#Z-score covariate data so effect magnitudes can be compared directly
#nadp
pm2.5 <- pm25_matrix_c
rownames(pm2.5) <- 'pm2.5_ThreeRivers'
the.mean <- apply(pm2.5,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(pm2.5,1,var, na.rm=TRUE))
pm2.5_z <-(pm2.5-the.mean)*(1/the.sigma)#z-score data

#bret's deposition
sw <- sw_matrix 
rownames(sw) <- c('sw_Emerald','sw_Topaz')
the.mean <- apply(sw,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(sw,1,var, na.rm=TRUE))
sw_z <-(sw-the.mean)*(1/the.sigma)#z-score data

#mean temp
smoke <- smokedensity_matrix
rownames(smoke) <- c('smoke_Emerald','smoke_Topaz')
the.mean <- apply(smoke,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(smoke,1,var, na.rm=TRUE))
smoke_z <-(smoke-the.mean)*(1/the.sigma)#z-score data


#total monthly precip
smoke <- smokedensity_matrix
rownames(smoke) <- c('smoke_Emerald','smoke_Topaz')
the.mean <- apply(smoke,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(smoke,1,var, na.rm=TRUE))
smoke_z <-(smoke-the.mean)*(1/the.sigma)#z-score data

#mean pdsi
smoke <- smokedensity_matrix
rownames(smoke) <- c('smoke_Emerald','smoke_Topaz')
the.mean <- apply(smoke,1, mean, na.rm=TRUE)
the.sigma <-sqrt( apply(smoke,1,var, na.rm=TRUE))
smoke_z <-(smoke-the.mean)*(1/the.sigma)#z-score data



#variance should be 1 for all the covariates
apply(smoke_z,1,var)

######################################################################################################################################################################################################
##Create inputs to MARSS function (matrices):
#Create names of models to run: (list) ###UPDATE THIS WHEN TRYING NEW SETS OF MODELS!!!!!!
#Name Format: 'mod', first number, '.','second number','first letter','_','second letters'

#Note: First number indicates number of states. In this analysis we assume six independent states (one per site) 
#     (Example: mod1.1 estimates one underyling state process, mod3.1 indicates three state processes)
#Note: Second number indicates which covariates are included in model 
# We will test whether covariate effects are shared across sites, or if there are site-specific effects (can try more later)
#Covariate Codes:
#1 = no covariates
#2 = SW radiation (daily mean; W m^-2)
#3 = Smoke density (Four levels: 0,5,17,27)
#4 = PM2.5 

#Note: First Letter indicates the process error structure in the model (environmental variability):
#Process error codes (for Q matrix)
#de = diagonal and equal (all lakes have the same process variance)
#bp= by proximity (Two clusters of sites that share process variance, Emerald and Topaz)
#du= diagonal and unequal (each lake/pond has its own process variance)
#bp_cov = testing a version of Q where errors from sites near one another also covary        

#Note: The second set of letters indicates whether to estimate a single effect of a covariate for all the state processes, 
#or whether to estimate site-specific covariate effects 
#Effect codes
#sh= shared covariate effect among all sites
#sep= separate covariate effects estimated for each site

#UPDATE THIS WHEN ADDING/CHANGING MODEL STRUCTURES!

#Set of models primarily testing different Q structures (e.g. process variance) and C structures (e.g. covariate effects)

mod.names <- c(
  "mod6.1de","mod6.1bp","mod6.1du","mod6.1bp_cov",
  
  "mod6.2de_sh","mod6.2bp_sh","mod6.2du_sh","mod6.2bp_cov_sh",
  "mod6.2de_sep","mod6.2bp_sep","mod6.2du_sep","mod6.2bp_cov_sep",
  
  "mod6.3de_sh","mod6.3bp_sh","mod6.3du_sh","mod6.3bp_cov_sh",
  "mod6.3de_sep","mod6.3bp_sep","mod6.3du_sep","mod6.3bp_cov_sep",
  
  "mod6.4de_sh","mod6.4bp_sh","mod6.4du_sh","mod6.4bp_cov_sh",
  "mod6.4de_sep","mod6.4bp_sep","mod6.4du_sep","mod6.4bp_cov_sep"
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
#(e.g. an Emerald Lake cluster and a Topaz lake cluster)
rownames(y)
bp <- matrix(list(0),nsites,nsites)
diag(bp) <- c('var.EML','var.EML','var.Topaz','var.Topaz','var.EML','var.Topaz')
bp

#adding bp_cov (errors from sites near one another also covary)
bp_cov <- matrix(list(0), nsites, nsites)
diag(bp_cov)<- c('var.EML','var.EML','var.Topaz','var.Topaz','var.EML','var.Topaz')

# --- Emerald cluster (EMLPond1=1, TOK11=2, Emerald Lake=5) ---
bp_cov[1,2] <- bp_cov[2,1] <- "cov.EML"
bp_cov[1,5] <- bp_cov[5,1] <- "cov.EML"
bp_cov[2,5] <- bp_cov[5,2] <- "cov.EML"

# --- Topaz cluster (TopazPond=3, TOK30=4, Topaz Lake=6) ---
bp_cov[3,4] <- bp_cov[4,3] <- "cov.Topaz"
bp_cov[3,6] <- bp_cov[6,3] <- "cov.Topaz"
bp_cov[4,6] <- bp_cov[6,4] <- "cov.Topaz"
bp_cov

Q.list <- list('diagonal and equal',bp,'diagonal and unequal', bp_cov)
Q.list[[2]]

##Z (maps observation time series to state processes)
#6 states (each lake/pond is a separate state process):
Z.6 <- 'identity'
Z.list <- list(Z.6)

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
c.list <- list(nocovar, sw_z, smoke_z, pm2.5_z)
c.list[[4]]

##C (matrix that maps covariates to the state processes)
#6 states
#No covariates
C_6.1 <- matrix(0,nrow=nsites)
#SW radiation
C_6.2_sh <-  matrix(list('sw','sw',0,0,'sw',0,0,0,'sw','sw',0,'sw'),nsites,2)
C_6.2_sep <- matrix(list('sw.EMLPond1','sw.TOK11',0,0,'sw.EmeraldLake',0,0,0,'sw.TopazPond','sw.TOK30',0,'sw.TopazLake'),
                    nsites,2)
#Smoke density
rownames(smoke_z)
C_6.3_sh <-  matrix(list('smoke','smoke',0,0,'smoke',0,0,0,'smoke','smoke',0,'smoke'),nsites,2)
C_6.3_sep <- matrix(list('smoke.EMLPond1','smoke.TOK11',0,0,'smoke.EmeraldLake',0,0,0,'smoke.TopazPond','smoke.TOK30',0,'smoke.TopazLake'),
                    nsites,2)
#PM2.5
rownames(y)
rownames(pm2.5_z)
C_6.4_sh <-  matrix(list('pm','pm','pm','pm','pm','pm'),nsites,1)
C_6.4_sep <- matrix(list('pm.EMLPond1','pm.TOK11','pm.TopazPond','pm.TOK30','pm.EmeraldLake','pm.TopazLake'),
                    nsites,1)
C.list <- list(C_6.1, C_6.2_sh, C_6.2_sep, C_6.3_sh, C_6.3_sep,C_6.4_sh, C_6.4_sep)
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
combos[,8] <- c(rep(1,4), rep(2,4), rep(3,4),rep(4,4), rep(5,4),rep(6,4),rep(7,4))
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