# Load library
library(sf)
library (rgdal)
library(raster)
library(spdep)
library(ggplot2)
library(INLA)


path = 'C:/Users/maggi/Documents/Research/West Nile/Data/'
data = read.csv(paste0(path,'data.csv'))

#Load shapefile
WN <- read_sf(paste0(path,'shapefile/shapefile.shp'))

#Binary "B" and row-standardized "W" adjacency matrices will be computed
WN$NewID <- seq(1:dim(WN)[1])
nb.sc = poly2nb(WN,row.names = WN$NewID)
w.mat = nb2mat(nb.sc,style = "B")

            ##########################
            ###########2022###########
            ##########################

form = data$count ~ AgeProp + elevft + Open.Water + Developed..Medium.Intensity + Developed..High.Intensity +  Barren.Land + Deciduous.Forest + Evergreen.Forest + Mixed.Forest + Herbaceous + Hay.Pasture + sparrow + NINO + jay + crow + culex_quinquefasciatus + culex_pipiens + culex_tarsalis +  cycle1 + cycle2 + YEAR2 +f(NewID, model = 'besagproper', graph = w.mat,constr = TRUE) + f(YEAR22, model = "ar1") 
fit = inla(form,  data = data,family = "n binomial",control.predictor = list(link = 1, compute=TRUE), control.family = list(link = 'log'), verbose = TRUE, control.compute = list(return.marginals.predictor=TRUE))

data$fitted.values <- exp(fit$summary.linear.predictor$mean)

