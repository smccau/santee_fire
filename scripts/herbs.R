library(vegan)
library(dummies)
library(car)

#Initialize data dataframes
dat = read.csv('./data/PC-ORD new(1)_Species.csv', row.names=1)
sites = read.csv('./data/pc-ord sfp_Plot_Data.csv')
#Set NAs to 0
dat[is.na(dat)] = 0
sites[is.na(sites)] = 0

#Declare an herb data.frame to use for analysis.
dat_herb = dat[ , grepl('...1', names(dat), fixed=TRUE)]
#Clean up species names to Genus.species
names(dat_herb) = sub('...1', '', names(dat_herb))

#Check if plot IDs and row names are equal
all(sites$plot_ID == row.names(dat_herb))

## RDA Analysis
#Variance partition to see different kinds of variation interactions among
#different kinds of variables.
#tree variables
tree = sites[ ,c("total_pine", "total_hw", "canopy_cover", "subcan_cover", "trees_acre")]
#burn season and frequency dummy matrix, because burn_freq and burn_season
#are factors
freq = dummy(sites$burn_freq)
burn = dummy(sites$burn_season)
#Show the variation partitioning explaining cover in dat_herb
varpart(dat_herb, tree, cbind(freq, burn))

#Declare each RDA, by various explanatory variables, and then perform
#anova() to observe possible significance
#Herb RDA by burn season and frequency
rda_herb_burn = rda(dat_herb ~ burn_freq + burn_season, data = sites)
anova(rda_herb_burn, by='margin', permutations = 1000)

#Herb RDA by community factors
rda_herb_env = rda(dat_herb ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                  trees_acre, data = sites)
anova(rda_herb_env, by='margin', permutations = 1000)

#Herb RDA by community factors, and burn_freq and burn_season
#as covariables
rda_herb_all = rda(dat_herb ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                  trees_acre + Condition(burn_freq + burn_season),
                 data = sites)
anova(rda_herb_all, by='margin', permutations = 1000)

#Plot rda_herb_all, to observe 
plot(rda_herb_all, display=c('bp'), xlim=c(-1, 1), ylim=c(-5, 5))
text(rda_herb_all, display='sp', col='maroon', cex=.5)
color_vect = c('black', 'dark red', 'tan', 'forest green')[-1]
points(rda_herb_all, 'sites', pch=19, 
       col=color_vect[sites$burn_season])
legend('topright', paste("Burn Season =", 1:3, sep=''), 
       col=color_vect, pch=19)



##Species Richness Analysis
herb_sr = apply(dat_herb, 1, function(x) sum(x > 0))
head(herb_sr)

#Species richness linear model for herbs
herb_mod = lm(herb_sr ~ burn_freq + burn_season + total_pine + 
               total_hw + canopy_cover + subcan_cover + trees_acre, 
            data = sites)
summary(herb_mod)
Anova(herb_mod)
plot(herb_mod)

#Species richness glm for herbs
herb_glm = glm(herb_sr ~ burn_freq + burn_season + total_pine + 
               total_hw + canopy_cover + subcan_cover + trees_acre, 
             data = sites, family = 'poisson')
summary(herb_glm)
Anova(herb_glm)
plot(herb_glm)

#Compare the two models
anova(herb_mod, herb_glm)

#herb_mod as a linear model with a better AIC score
AIC(herb_mod)
AIC(herb_glm)