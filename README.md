# santee_fire

Welcome to the Santee Long-Term Fire Experiment!

The objective of this project is to determine what drives plant cover and species richness in the Santee Fire Plots,
the location of a long-term study on the effects of different prescribed fire regimes within the plots.  

The codebase for this project is entirely in R.

The scripts are divided into three types of plants:  herbs, shrubs, and subcanopy.  After reading in data via .CSV files, 
the scripts allow the user to create histograms of variation of cover.  Following that is three separate RDA analyses, 
in which one uses only burn data, one uses only environmental data, and the last one using all selected environmental data 
and burn data as covariables.  The scripts also allow the user to examine species richness by site, and model the predictors
of species richness within the Santee Fire Plots.  

I'd like to thank Daniel McGlinn for the inspiration for the project and all of the much-needed advice along the way, as well
as Anne Cubetta for providing data for the project. 
