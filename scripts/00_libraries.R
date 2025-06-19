

if (!require('pacman')) install.packages('pacman'); library('pacman')
# remotes::install_github("MilesMcBain/breakerofchains")

# p_update(update = FALSE)  #Tells you which packages are out of date
# p_update()  #actually updates the out of date packages


##Load all the libraries your heart desires
pacman::p_load("lubridate",
               "tidyverse",
               "plyr",
               "dtplyr",
               "Rmisc",
               "ggpubr",
               "rstatix",
               "ggQC",
               "renv",
               "here",
               "wql",
               "patchwork",
               "plotly",
               "ggrepel", 
               "huxtable",
               "ggridges",
               "viridis",
               "ggthemes",
               "huxtable",
               "fs",
               "dataRetrieval",
               "snotelr",
               "naniar",
               "tsibble",
               "visdat",
               "trend",
               "zyp",
               "huxtable",
               "officer",
               "flextable",
               "zoo",
               "Hmisc",
               "readxl",
               "ggh4x", #add minor breaks to x-axis
               #GAMS stuff
               "mgcv",
               "magrittr",
               "gratia")

#Use renv for version control.  Beginner guide here:
# https://rstudio.github.io/renv/articles/renv.html

# if (!require('renv')) install.packages('renv'); library('renv')
# renv::restore()

rename <- dplyr::rename
select <- dplyr::select
summarize <- dplyr::summarize



monthCols <- c(
  'July' = '#eed440',
  'June' = '#3258a8',
  'August' = '#a83632')

chemCols <- c(
  "cations" = "#0C090D",
  "NH4-N" = "#bb5b48",
  "NO3-N" = "#d58b48",
  "SiO2" = "#eed440",
  "SO4-S" = "#6493c8",
  "Inorganic N" ="#1978E4"
)

hydroCols <- c("fall baseflow" = "#bb5b48",
               "winter baseflow" = "#6493c8",
               "falling limb" = "#eed440",
               "rising limb" = "#aad440")

hydroCols2 <- c("Q 80th" = "#bb5b48",
               "snowmelt onset" = "#6493c8",
               "Q 50th (COM)" = "#eed440",
               "Q 20th" = "#aad440")

