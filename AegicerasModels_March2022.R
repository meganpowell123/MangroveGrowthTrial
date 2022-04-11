#from tutorial at https://www.flutterbys.com.au/stats/tut/tut7.6b.html

install.packages("tidybayes")
install.packages("brms")
install.packages ("rstanarm")
install.packages ("tidyverse")
install.packages("rtools")
library(brms)
library (broom)
library (coda)
library (rstanarm)
library (tidybayes)
library (tidyverse)
#get working directory
getwd()
#change word directory
setwd("I:/AegicerasTrial/BRM_mangrove")
#load data from file
mangrove.data = read.csv ("mangrove_mp.csv")
head (mangrove.data)
#model  fitting TOTAL BIOMASS
#use default values for priors???)
mangrovetotalbiomass.brms = brm(t_biomass ~ CO2*Light, data = mangrove.data,)
print(summary (mangrovetotalbiomass.brms), digits = 4)


#MCMC diagnostics (total biomass)
mcmc_totalbiomass = as.mcmc(mangrovetotalbiomass.brms)
plot(mcmc_totalbiomass)

#Graph summary conditional effects with credible interval (total biomass)
plot(conditional_effects(mangrovetotalbiomass.brms))


#model  fitting ROOT BIOMASS
mangroverootbiomass.brms = brm(root_b ~ CO2*Light, data = mangrove.data,)
print(summary (mangroverootbiomass.brms), digits = 4)


#MCMC diagnostics (root biomass)
mcmc_rootbiomass = as.mcmc(mangroverootbiomass.brms)
plot(mcmc_rootbiomass)

#Graph summary conditional effects with credible interval (root biomass)
plot(conditional_effects(mangroverootbiomass.brms))

#model  fitting LEAF BIOMASS
mangroveleafbiomass.brms = brm(leaf_b ~ CO2*Light, data = mangrove.data,)
print(summary (mangroveleafbiomass.brms), digits = 4)


#MCMC diagnostics (leaf biomass)
mcmc_leafbiomass = as.mcmc(mangroveleafbiomass.brms)
plot(mcmc_leafbiomass)

#Graph summary conditional effects with credible interval (leaf biomass)
plot(conditional_effects(mangroveleafbiomass.brms))

#model  fitting ABOVE GROUND BIOMASS (biomass of stem plus leaves)

mangroveabovegroundbiomass.brms = brm(above_b ~ CO2*Light, data = mangrove.data,)
print(summary (mangroveabovegroundbiomass.brms), digits = 4)


#MCMC diagnostics (above ground biomass)
mcmc_abovegroundbiomass = as.mcmc(mangroveabovegroundbiomass.brms)
plot(mcmc_abovegroundbiomass)

#Graph summary conditional effects with credible interval (above ground biomass)
plot(conditional_effects(mangroveabovegroundbiomass.brms))

##STEM HEIGHT

mangrovestemheight.brms = brm(stem_h ~ CO2*Light, data = mangrove.data,)
print(summary(mangrovestemheight.brms), digits = 4)

#MCMC diagnostics (stem height)
mcmc_stemheight = as.mcmc(mangrovestemheight.brms)
plot(mcmc_stemheight)

#Graph summary conditional effects with credible interval (stem height)
plot(conditional_effects(mangrovestemheight.brms))

#ROOT LENGTH
mangroverootlength.brms = brm(root_l ~ CO2*Light, data = mangrove.data,)
print(summary(mangroverootlength.brms), digits = 4)


#MCMC diagnostics root length
mcmc_rootlength = as.mcmc(mangroverootlength.brms)
plot(mcmc_rootlength)

#Graph summary conditional effects with credible interval (root length)
plot(conditional_effects(mangroverootlength.brms))

#leaf ratio
mangroveleafratio.brms = brm(leaf_ratio ~ CO2*Light, data = mangrove.data,)
print(mangroveleafratio.brms)


#MCMC diagnostics (leaf ratio)
mcmc_leafratio = as.mcmc(mangroveleafratio.brms)
plot(mcmc_leafratio)

#Graph summary conditional effects with credible interval (leaf ratio)
plot(conditional_effects(mangroveleafratio.brms))


#leaf area
#model  fitting LEAF area

mangroveleafarea.brms = brm(leaf_a ~ CO2*Light, data = mangrove.data,)

print(summary(mangroveleafarea.brms), digits =4)


#MCMC diagnostics (leaf area)
mcmc_leafarea = as.mcmc(mangroveleafarea.brms)
plot(mcmc_leafarea)

#Graph summary conditional effects with credible interval (leaf area)
plot(conditional_effects(mangroveleafarea.brms))

#above ground to below ground biomass ratio

mangroveabovebelow.brms = brm(above_below ~ CO2*Light, data = mangrove.data,)
print(summary(mangroveabovebelow.brms), digits = 4)


#MCMC diagnostics (above to below ground biomass ratio)
mcmc_abovebelow = as.mcmc(mangroveabovebelow.brms)
plot(mcmc_abovebelow)

#Graph summary conditional effects with credible interval (leaf ratio)
plot(conditional_effects(mangroveabovebelow.brms))

###investigate effects by splitting into single factor models (for where 2 factor
###model had a significant interaction)

########
####subset data based on variable values (light condition = light, light condition = shade)
mangrove_light.data <- mangrove.data [which(mangrove.data$Light=='light'),]
head (mangrove_light.data)
tail(mangrove_light.data, n=50)

mangrove_shade.data <- mangrove.data[which(mangrove.data$Light=='shade'),]
head(mangrove_shade.data)
tail(mangrove_shade.data)

#Single factor Model for totalbiomass_light condition

mangrovetotalbiomass_light.brms = brm(t_biomass ~ CO2, data = mangrove_light.data,)
print(summary (mangrovetotalbiomass_light.brms), digits = 4)

#MCMC diagnostics (totalbiomass_light)
mcmc_totalbiomass_light = as.mcmc(mangrovetotalbiomass_light.brms)
plot(mcmc_totalbiomass_light)

#Graph summary conditional effects with credible interval (totalbiomass_light)
plot(conditional_effects(mangrovetotalbiomass_light.brms)) 

#Single factor Model for totalbiomass_shade condition

mangrovetotalbiomass_shade.brms = brm(t_biomass ~ CO2, data = mangrove_shade.data,)
print(summary (mangrovetotalbiomass_shade.brms), digits = 4)

#MCMC diagnostics (totalbiomass_shade)
mcmc_totalbiomass_shade = as.mcmc(mangrovetotalbiomass_shade.brms)
plot(mcmc_totalbiomass_shade)

#Graph summary conditional effects with credible interval (shade)
plot(conditional_effects(mangrovetotalbiomass_shade.brms))


##
#Single factor model for leafbiomass_light

mangroveleafbiomass_light.brms = brm(leaf_b ~ CO2, data = mangrove_light.data,)
print(summary (mangroveleafbiomass_light.brms), digits = 4)

#MCMC diagnostics (leafbiomass_light)
mcmc_leafbiomass_light = as.mcmc(mangroveleafbiomass_light.brms)
plot(mcmc_leafbiomass_light)

#Graph summary conditional effects with credible interval (leaf biomass light)

plot(conditional_effects(mangroveleafbiomass_light.brms))


#Single factor Models for leafbiomass_shade

mangroveleafbiomass_shade.brms = brm(leaf_b ~ CO2, data = mangrove_shade.data,)
print(summary (mangroveleafbiomass_shade.brms), digits = 4)

#MCMC diagnostics (leafbiomass_shade)
mcmc_leafbiomass_light = as.mcmc(mangroveleafbiomass_shade.brms)
plot(mcmc_leafbiomass_shade)

#Graph summary conditional effects with credible interval (leaf biomass shade)
plot(conditional_effects(mangroveleafbiomass_shade.brms))


#Single factor model for aboveground biomass_light

mangroveabovegroundbiomass_light.brms = brm(above_b ~ CO2, data = mangrove_light.data)
print(summary(mangroveabovegroundbiomass_light.brms), digits = 4)

#MCMC diagnostics (mangroveabovegroundbiomass_light)
mcmc_mangroveabovegroundbiomass_light = as.mcmc(mangroveabovegroundbiomass_light.brms)
plot(mcmc_mangroveabovegroundbiomass_light)

#Graph summary conditional effects with credible interval (above ground biomass light)
plot (conditional_effects(mangroveabovegroundbiomass_light.brms))

#Single factor model for above ground biomass_shade
mangroveabovegroundbiomass_shade.brms = brm(above_b ~ CO2, data = mangrove_shade.data)
print(summary(mangroveabovegroundbiomass_shade.brms), digits = 4)

#MCMC diagnostics (mangroveabovegroundbiomass_shade)
mcmc_mangroveabovegroundbiomass_shade = as.mcmc(mangroveabovegroundbiomass_shade.brms)
plot(mcmc_mangroveabovegroundbiomass_shade)

#Graph summary conditional effects with credible interval (above ground biomass shade)
plot (conditional_effects(mangroveabovegroundbiomass_shade.brms))

#Single factor Model for leafratio_light condition

mangroveleafratio_light.brms = brm(leaf_ratio ~ CO2, data = mangrove_light.data,)
print(summary (mangroveleafratio_light.brms), digits = 4)


#MCMC diagnostics (leafratio_light)
mcmc_leafratio_light = as.mcmc(mangroveleafratio_light.brms)
plot(mcmc_leafratio_light)

#Graph summary conditional effects with credible interval (leaf ratio light)
plot(conditional_effects(mangroveleafratio_light.brms))

#Single factor model for leafratio_shade 
mangroveleafratio_shade.brms = brm(leaf_ratio ~ CO2, data = mangrove_shade.data,)
print(summary (mangroveleafratio_shade.brms), digits = 4)

#MCMC diagnostics (leafratio_shade)
mcmc_leafratio_shade = as.mcmc(mangroveleafratio_shade.brms)
plot(mcmc_leafratio_shade)

#Graph summary conditional effects with credible interval (leaf ratio shade)
plot(conditional_effects(mangroveleafratio_shade.brms))

########
####subset data based on variable values (CO2 = elevated, CO2 = ambient)
mangrove_ambient.data <- mangrove.data [which(mangrove.data$CO2=='ambient'),]
head (mangrove_ambient.data)
tail(mangrove_ambient.data, n=50)

mangrove_elevated.data <- mangrove.data[which(mangrove.data$CO2=='elevated'),]
head(mangrove_elevated.data)
tail(mangrove_elevated.data)

#More Single factor models for above ground biomass 
mangroveabovegroundbiomass_ambient.brms = brm(above_b ~ Light, data = mangrove_ambient.data,)
print(summary (mangroveabovegroundbiomass_ambient.brms), digits = 4)

mangroveabovegroundbiomass_elevated.brms = brm(above_b ~ Light, data = mangrove_elevated.data,)
print(summary (mangroveabovegroundbiomass_elevated.brms), digits = 4)