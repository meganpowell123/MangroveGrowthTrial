#from tutorial at https://www.flutterbys.com.au/stats/tut/tut7.6.html#h3_35
install.packages("tidybayes")
install.packages("brms")
install.packages ("rstanarm")
install.packages ("tidyverse")
library(brms)
library (broom)
library (coda)
library (rstanarm)
library (tidybayes)
library (tidyverse)
#set working directory
setwd ("C:/Data/AegicerasTrial/BRM_mangrove")
#load data from file
mangrove.data = read.csv ("mangrove_mp.csv")
head (mangrove.data)