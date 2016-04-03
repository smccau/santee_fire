library(vegan)
library(dummies)
library(car)

#Initialize data dataframes
dat = read.csv('./data/PC-ORD new(1)_Species.csv', row.names=1)
sites = read.csv('./data/pc-ord sfp_Plot_Data.csv')
#Set NAs to 0
dat[is.na(dat)] = 0
sites[is.na(sites)] = 0

#Check if plot IDs and row names are equal
all(sites$plot_ID == row.names(dat_shrub))

#Histograms of variation of cover between sites and species
uni_sp2 = unique(row.names(dat_shrub))
sp_sum2 = apply(dat_shrub, 2, sum)
site_sum2 = apply(dat_shrub, 1, sum)
par(mfrow=c(2,2))
hist(sp_sum2)
col = colorRamp(c('red', 'orange', 'blue'))
sp_cols = col(length(uni_sp2))
plot(sp_sum2[order(sp_sum2, decreasing=T)], type='o', col='red', lwd=2,
     xlab='Sp Rank', ylab='Sum Cover')
hist(site_sum2)
plot(site_sum2[order(site_sum2, decreasing=T)], type='o', col='red', lwd=2,
     xlab='Site Rank', ylab='Sum Cover')

## RDA Analysis
#Variance partition to see different kinds of variation interactions among
#different kinds of variables.
#tree variables
tree = sites[ ,c("total_pine", "total_hw", "canopy_cover", "subcan_cover", "trees_acre")]
#burn season and frequency dummy matrix, because burn_freq and burn_season
#are factors
freq = dummy(sites$burn_freq)
burn = dummy(sites$burn_season)
#Show the variation partitioning explaining cover in dat_shrub
varpart(dat_shrub, tree, cbind(freq, burn))

#Shrub RDA by burn season and frequency
rda_shrub_burn = rda(dat_shrub ~ burn_freq + burn_season, data = sites)
anova(rda_shrub_burn, by='margin', permutations = 1000)

#Shrub RDA by community factors
rda_shrub_env = rda(dat_shrub ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                     trees_acre, data = sites)
anova(rda_shrub_env, by='margin', permutations = 1000)

#Shrub RDA by community factors, and burn_freq and burn_season
#as covariables
rda_shrub_all = rda(dat_shrub ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                     trees_acre + Condition(burn_freq + burn_season),
                   data = sites)
anova(rda_shrub_all, by='margin', permutations = 1000)

#Plot rda_shrub_all
plot(rda_shrub_all, display=c('bp'), xlim=c(-1, 1), ylim=c(-5, 5))
text(rda_shrub_all, display='sp', col='maroon', cex=.5)
color_vect = c('black', 'dark red', 'tan', 'forest green')[-1]
points(rda_shrub_all, 'sites', pch=19, 
       col=color_vect[sites$burn_season])
legend('topright', paste("Burn Season =", 1:4, sep=''), 
       col=color_vect, pch=19)



##Species Richness Analysis
shrub_sr = apply(dat_shrub, 1, function(x) sum(x > 0))
head(shrub_sr)

#Species richness linear model for shrubs
shrub_mod = lm(shrub_sr ~ burn_freq + burn_season + total_pine + 
              total_hw + canopy_cover + subcan_cover + trees_acre, 
            data = sites)
summary(shrub_mod)
Anova(shrub_mod)
plot(shrub_mod)

#Species richness glm for shrubs
shrub_glm = glm(shrub_sr ~ burn_freq + burn_season + total_pine + 
               total_hw + canopy_cover + subcan_cover + trees_acre, 
             data = sites, family = 'poisson')
summary(shrub_glm)
Anova(shrub_glm)
plot(shrub_glm)

#Compare the two models
anova(shrub_mod, shrub_glm)

AIC(shrub_mod)
AIC(shrub_glm)