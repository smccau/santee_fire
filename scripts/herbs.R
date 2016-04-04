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
all(sites$plot_ID == row.names(dat_herb))

#Histograms of variation of cover between sites and species
uni_sp = unique(row.names(dat_herb))
sp_sum = apply(dat_herb, 2, sum)
site_sum = apply(dat_herb, 1, sum)
par(mfrow=c(2,2))
hist(sp_sum)
col = colorRamp(c('red', 'orange', 'blue'))
sp_cols = col(length(uni_sp))
plot(sp_sum[order(sp_sum, decreasing=T)], type='o', col='red', lwd=2,
     xlab='Sp Rank', ylab='Sum Cover')
hist(site_sum)
plot(site_sum[order(site_sum, decreasing=T)], type='o', col='red', lwd=2,
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
#Show the variation partitioning explaining cover in dat_herb
varpart(dat_herb, tree, cbind(freq, burn))

#Herb RDA by burn season and frequency
rda_herb_burn = rda(dat_herb ~ burn_freq + burn_season, data = sites)
anova(rda_herb_burn, by='margin', permutations = 1000)
#Herb RDA by community factors
rda_herb_env = rda(dat_herb ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                  trees_acre, data = sites)
#Herb RDA by community factors, and burn_freq and burn_season
#as covariables
rda_herb_all = rda(dat_herb ~ total_pine + total_hw + canopy_cover + subcan_cover + 
                  trees_acre + Condition(burn_freq + burn_season),
                 data = sites)
#Analyze variance within rda_herb_all
anova(rda_herb_all, by='margin', permutations = 1000)

#Plot rda_herb_all
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

AIC(herb_mod)
AIC(herb_glm)