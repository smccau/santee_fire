library(vegan)
library(dummies)
library(car)

#Initialize data dataframes
dat = read.csv('./data/PC-ORD new(1)_Species.csv', row.names=1)
sites = read.csv('./data/pc-ord sfp_Plot_Data.csv')
#Set NAs to 0
dat[is.na(dat)] = 0
sites[is.na(sites)] = 0

#Declare a subcanopy data.frame to use for analysis.
dat_sub = dat[ , grepl('...3', names(dat), fixed=TRUE)]
#Clean up species names to Genus.species
names(dat_sub) = sub('...3', '', names(dat_sub))

#Check if plot IDs and row names are equal
all(sites$plot_ID == row.names(dat_sub))

## RDA Analysis
#Variance partition to see different kinds of variation interactions among
#different kinds of variables.
#tree variables
tree = sites[ ,c("total_pine", "total_hw", "canopy_cover", "subcan_cover", "trees_acre")]
#burn season and frequency dummy matrix, because burn_freq and burn_season
#are factors
freq = dummy(sites$burn_freq)
burn = dummy(sites$burn_season)
#Show the variation partitioning explaining cover in dat_sub
varpart(dat_sub, tree, cbind(freq, burn))

#Declare each RDA, by various explanatory variables, and then perform
#anova() to observe possible significance
#Subcanopy RDA by burn season and frequency
rda_sub_burn = rda(dat_sub ~ burn_freq + burn_season, data = sites)
anova(rda_sub_burn, by='margin', permutations = 1000)

#Subcanopy RDA by community factors
rda_sub_env = rda(dat_sub ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                      trees_acre, data = sites)
anova(rda_sub_env, by='margin', permutations = 1000)

#Subcanopy RDA by community factors, and burn_freq and burn_season
#as covariables
rda_sub_all = rda(dat_sub ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                      trees_acre + Condition(burn_freq + burn_season),
                    data = sites)
anova(rda_sub_all, by='margin', permutations = 1000)

#Plot rda_shrub_all
plot(rda_sub_all, display=c('bp'), xlim=c(-1, 1), ylim=c(-5, 5))
text(rda_sub_all, display='sp', col='maroon', cex=.5)
color_vect = c('black', 'dark red', 'tan', 'forest green')[-1]
points(rda_sub_all, 'sites', pch=19, 
       col=color_vect[sites$burn_season])
legend('topright', paste("Burn Season =", 1:3, sep=''), 
       col=color_vect, pch=19)
#Note Burn Season 3 loading along RDA2, and BS 2 loading on RDA1


##Species Richness Analysis
sub_sr = apply(dat_sub, 1, function(x) sum(x > 0))
head(sub_sr)

#Species richness linear model for subcanopy
sub_mod = lm(sub_sr ~ burn_freq + burn_season + total_pine + 
                 total_hw + canopy_cover + subcan_cover + trees_acre, 
               data = sites)
summary(sub_mod)
Anova(sub_mod)
plot(sub_mod)

#Species richness glm for subcanopy
sub_glm = glm(sub_sr ~ burn_freq + burn_season + total_pine + 
                  total_hw + canopy_cover + subcan_cover + trees_acre, 
                data = sites, family = 'poisson')
summary(sub_glm)
Anova(sub_glm)
plot(sub_glm)

#Compare the two models
anova(sub_mod, sub_glm)

#sub_mod as a linear model with better AIC score
AIC(sub_mod)
AIC(sub_glm)