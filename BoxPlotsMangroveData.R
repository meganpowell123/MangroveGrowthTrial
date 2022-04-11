#boxplots to have a look at data - normal distibution and variances
#install.packages ("brms")
#install.packages ("tidyverse")
library (brms)
library (tidyverse)
library (ggplot2)
library (broom)
library (coda)
#check working directory
getwd()
#load data
mangrove.data = read.csv ("mangrove_mp.csv")
head (mangrove.data)

#make a boxplot showing total dry biomass vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= t_biomass, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("total biomass (g)") +
  xlab("CO2")
  
#make a boxplot showing root dry biomass vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= root_b, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("root biomass (g)") +
  xlab("CO2")
  
#make a boxplot showing above ground dry biomass vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= above_b, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("above ground biomass (g)") +
  xlab("CO2")

#make a boxplot showing above ground dry biomass vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= leaf_b, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("leaf biomass (g)") +
  xlab("CO2")

#make a boxplot showing above leaf area total for each seedling vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= leaf_a, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("seedling leaf area (cm2)") +
  xlab("CO2")

#make a boxplot showing above stem height for each seedling vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= stem_h, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("seedling stem height (cm)") +
  xlab("CO2")

#make a boxplot showing seedling root length for each seedling vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= root_l, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("seedling root length (cm)") +
  xlab("CO2")

##make a boxplot showing seedling specific leaf area (aka leaf ratio) vs CO2 and Light treatments
ggplot(mangrove.data, aes(y= leaf_ratio, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("specific leaf area (cm/g)") +
  xlab("CO2")




###########leaf count - boxplot doesn't for whole number or another  
#problem with the data distribution??? Too many missing values? 
#Not a normal distribution.
#plot of leaf count showing spread of values for each grouping

##this is what the boxplot looks like: boxplot showing leaf count 
#by CO2 and Light treatment
ggplot(mangrove.data, aes(y= leaf_count, x = CO2, fill = Light)) + 
  geom_boxplot(notch=TRUE) +
  ylab("number of leaves") +
  xlab("CO2")



