dat = read.csv('./data/PC-ORD new(1)_Species.csv', row.names=1)
#Set NAs to 0
dat[is.na(dat)] = 0
head(dat)
names(dat)

#Declare variables
#Note: dat_tree and dat_total only Pinus taeda
dat_herb = dat[ , grepl('...1', names(dat), fixed=TRUE)]
dat_shrub = dat[ , grepl('...2', names(dat), fixed=TRUE)]
dat_sub = dat[ , grepl('...3', names(dat), fixed=TRUE)]
dat_tree= dat[ , grepl('...4', names(dat), fixed=TRUE)]
dat_total = dat[ , grepl('...5', names(dat), fixed=TRUE)]

#Clean up names
names(dat_herb) = sub('...1', '', names(dat_herb))
names(dat_shrub) = sub('...2', '', names(dat_shrub))
names(dat_sub) = sub('...3', '', names(dat_sub))
names(dat_tree) = sub('...4', '', names(dat_tree))
names(dat_total) = sub('...5', '', names(dat_total))