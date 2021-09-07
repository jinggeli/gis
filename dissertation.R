#-----------SDE------------
#Load packages
library(splancs)
library(sp)
library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(shapefiles)
library(foreign)

# calculating SDE
calc_sde(id=1, filename="SDE.txt", #a string indicating the ASCII textfile where shape coordinates will be written
         centre.xy=NULL, calccentre=TRUE, #get mean center
         weighted=TURE, weights=residential_area, # use residential area as weight
         points=housing,#housing points
         verbose=FALSE)

#plot SDE shp
shp <- convert.to.shapefile(sdeloc,sdeatt,"id",5)
write.shapefile(shp, "SDE_Shape", arcgis=T)
#or
plot_sde(plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE, 
         plotpoints=TRUE, plotcentre=TRUE, titletxt="Title", 
         xaxis="Easting (m)", yaxis="Northing (m)")

#calculating and plotting SDE for each year
##or use SDE tool in ArcMap

#-----------KDE------------

#or use KDE tool in ArcMap

#-----------NKDE------------

#Load packages
suppressPackageStartupMessages(library(ggplot2))
library(ggplot2)
library(reshape2)
library(kableExtra)
library(spNetwork)
library(sp)
library(maptools)
library(rgeos)
library(raster)
library(Rcpp)
library(sf) 
library(dplyr)
library(RColorBrewer)
library(classInt)

#NKDE for Westminster

#Use the 'near tool' in ArcMap to get the network point of the housing on the ITN road network
#read housing network point data and ITN road network

networkgpkg <- system.file("extdata", "westminroad.gpkg",
                           package = "spNetwork", mustWork = TRUE)
eventsgpkg <- system.file("extdata", "westminpoint.gpkg",
                          package = "spNetwork", mustWork = TRUE)
network <- rgdal::readOGR(networkgpkg,verbose = FALSE)
housing <- rgdal::readOGR(eventsgpkg, verbose = FALSE)

#plotting network points on road network
plot(network)
plot(housing,add=T,col='red',pch = 19)


#calculating lixels to use as sampling points(lixels length: 100m)
lixels <- lixelize_lines(network,100,mindist = 50)
samples <- lines_center(lixels)
#or: samples <- lines_points_along(network,50)

#If the memory is not enough to run the file, please increase the memory
memory.limit(size=56000)

#applying the NKDE
densities <- nkde(network, #network file
                  events = housing ,#events network point file
                  w=1,#weight of points
                  samples = samples,#sampling points for calculating density
                  kernel_name = "quartic",#kernel function
                  bw = 600, div= "bw", #bandwidth
                  adaptive = TRUE, # we use here an adaptive bandwidth
                  trim_bw = 1000, #the maximum local values of bandwidth will be 1000m
                  method = "continuous", digits = 3, tol = 1,# use continuous method for NKDE
                  grid_shape = c(2,2),#split the study area in four rectangles to reduce calculation time
                  max_depth = 10,#When the recursive function runs to 15, it can stop, and when it runs to 10, the result is basically the same as 15
                  agg = 5, #aggregate events within a 5m radius (faster calculation)
                  sparse = TRUE,
                  verbose = FALSE)

samples$density <- densities$k


#map the density values estimated for each lixel centre

#rescaling to help the mapping
samples$density <- samples$density*1000

# using a discretization method
breaks <- classIntervals(samples$density, n = 7, style = "fisher", intervalClosure = "right")

colorRamp <- brewer.pal(n = 7, name = "Blues")

samples$class <- as.character(cut(samples$density,breaks$brks,colorRamp,include.lowest =TRUE))

xy <- coordinates(samples)
samples$mapx <- xy[,1]
samples$mapy <- xy[,2]

#finally map with ggplot
labels <- names(print(breaks))

network$line_id <- 1:nrow(network)
Mapnetwork <- fortify(network,id="line_id")

df <- samples@data[order(samples@data$density),]

ggplot() + 
  geom_path(data = Mapnetwork, mapping = aes(x=long,y=lat,group=group), color="black")+
  geom_point(data = df, mapping = aes(x=mapx,y=mapy,color=class))+
  scale_color_manual("density",
                     breaks = colorRamp, values = colorRamp, 
                     label = labels)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_fixed()+
  labs(title = "residential redevelopment density in Westminster",
       subtitle = "within a radius of 600-1000 metres",
       caption = "using the quartic kernel")

#NKDE for Hammersmith and Fulham

#read housing network point data and ITN road network

networkgpkg <- system.file("extdata", "hammersmithroad.gpkg",
                           package = "spNetwork", mustWork = TRUE)
eventsgpkg <- system.file("extdata", "hammersmithpoint.gpkg",
                          package = "spNetwork", mustWork = TRUE)
network <- rgdal::readOGR(networkgpkg,verbose = FALSE)
housing <- rgdal::readOGR(eventsgpkg, verbose = FALSE)

#plotting network points on road network
plot(network)
plot(housing,add=T,col='red',pch = 19)


#calculating lixels to use as sampling points(lixels length: 100m)
lixels <- lixelize_lines(network,100,mindist = 50)
samples <- lines_center(lixels)
#or: samples <- lines_points_along(network,50)

#If the memory is not enough to run the file, please increase the memory
memory.limit(size=56000)

#applying the NKDE
densities <- nkde(network, #network file
                  events = housing ,#events network point file
                  w=1,#weight of points
                  samples = samples,#sampling points for calculating density
                  kernel_name = "quartic",#kernel function
                  bw = 600, div= "bw", #bandwidth
                  adaptive = TRUE, # we use here an adaptive bandwidth
                  trim_bw = 1000, #the maximum local values of bandwidth will be 1000m
                  method = "continuous", digits = 3, tol = 1,# use continuous method for NKDE
                  grid_shape = c(2,2),#split the study area in four rectangles to reduce calculation time
                  max_depth = 10,#When the recursive function runs to 15, it can stop, and when it runs to 10, the result is basically the same as 15
                  agg = 5, #aggregate events within a 5m radius (faster calculation)
                  sparse = TRUE,
                  verbose = FALSE)

samples$density <- densities$k


#map the density values estimated for each lixel centre

#rescaling to help the mapping
samples$density <- samples$density*1000

# using a discretization method
breaks <- classIntervals(samples$density, n = 7, style = "fisher", intervalClosure = "right")

colorRamp <- brewer.pal(n = 7, name = "Blues")

samples$class <- as.character(cut(samples$density,breaks$brks,colorRamp,include.lowest =TRUE))

xy <- coordinates(samples)
samples$mapx <- xy[,1]
samples$mapy <- xy[,2]

#finally map with ggplot
labels <- names(print(breaks))

network$line_id <- 1:nrow(network)
Mapnetwork <- fortify(network,id="line_id")

df <- samples@data[order(samples@data$density),]

ggplot() + 
  geom_path(data = Mapnetwork, mapping = aes(x=long,y=lat,group=group), color="black")+
  geom_point(data = df, mapping = aes(x=mapx,y=mapy,color=class))+
  scale_color_manual("density",
                     breaks = colorRamp, values = colorRamp, 
                     label = labels)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_fixed()+
  labs(title = "residential redevelopment density in Hammersmith and Fulham",
       subtitle = "within a radius of 600-1000 metres",
       caption = "using the quartic kernel")


#-----------regression------------

#Load packages
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(sp)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(spdep)
library(car)
library(fs)
library(janitor)
library(dplyr)
library(spatialreg)
library(Matrix)
library(spdep)

#STAGE 1

#read geographic shape file:London LSOA
lsoamap<-st_read("LSOA_2011_London_gen_MHW.shp")
qtm(lsoamap)

#read CSV file:Y and X variables of each LSOA
xy3<-read.csv("loghousecount.csv",sep = ",",header = TRUE)
xy3n<-read.csv("loghousecountnolsoa.csv",sep = ",",header = TRUE)

#Add data to geographic files
lsoaxy <- left_join(lsoamap, xy3, by = c("LSOA11CD"="LSOA11CD"))

#run OLS model
olsmodel3<-lm(loghousecount~between2km+close2km,data=xy3n)
summary(olsmodel3)

#Obtain the residuals of OLS model and test whether it satisfies the normal distribution
lsoaxy$olsmodel3resids <- residuals(olsmodel3)
ggplot(lsoaxy, aes(x=olsmodel3resids)) + 
  geom_histogram()

#View residuals and heteroscedasticity of OLS model
par(mfrow=c(2,2)) 
plot(olsmodel3)

#Test spatial autocorrelation

#Extract lsoa centroid
coordsLSOA <- lsoaxy%>%
  st_centroid()%>%
  st_geometry()

#Create matrix
knn_LSOA <-coordsLSOA %>%
  knearneigh(., k=4)

LSOA_knn <- knn_LSOA %>%
  knn2nb()

LSOA.knn_4_weight <- LSOA_knn %>%
  nb2listw(., style="C")

# Moran' I test on residuals of OLS model
Nearest_neighbour <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(olsmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour

#Run spatial lag regression
lagmodel3 <- lagsarlm(loghousecount~between2km+close2km,
                      data=lsoaxy, 
                      nb2listw(LSOA_knn, style="C"), 
                      method = "eigen")
summary(lagmodel3)

# Moran' I test on residuals of SLM model
lsoaxy <- lsoaxy %>%
  mutate(lagmodel3resids = residuals(lagmodel3))

Nearest_neighbour_lag <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(lagmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()
Nearest_neighbour_lag

#STAGE 2

#read geographic shape file:London LSOA
lsoamap<-st_read("LSOA_2011_London_gen_MHW.shp")
qtm(lsoamap)

#read CSV file:Y and X variables of each LSOA
xy3<-read.csv("loghousecount.csv",sep = ",",header = TRUE)
xy3n<-read.csv("loghousecountnolsoa.csv",sep = ",",header = TRUE)

#Add data to geographic files
lsoaxy <- left_join(lsoamap, xy3, by = c("LSOA11CD"="LSOA11CD"))

#run OLS model
olsmodel3<-lm(loghousecount~Attractions+CommercialServices+EducationandHealth+ManufacturingandProduction+Retail+SportandEntertainment+Transport+simpson+between2km+close2km,data=xy3n)
summary(olsmodel3)

#Obtain the residuals of OLS model and test whether it satisfies the normal distribution
lsoaxy$olsmodel3resids <- residuals(olsmodel3)
ggplot(lsoaxy, aes(x=olsmodel3resids)) + 
  geom_histogram()

#View residuals and heteroscedasticity of OLS model
par(mfrow=c(2,2)) 
plot(olsmodel3)

#Test spatial autocorrelation

#Extract lsoa centroid
coordsLSOA <- lsoaxy%>%
  st_centroid()%>%
  st_geometry()

#Create matrix
knn_LSOA <-coordsLSOA %>%
  knearneigh(., k=4)

LSOA_knn <- knn_LSOA %>%
  knn2nb()

LSOA.knn_4_weight <- LSOA_knn %>%
  nb2listw(., style="C")

# Moran' I test on residuals of OLS model
Nearest_neighbour <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(olsmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour

#Run spatial lag regression
lagmodel3 <- lagsarlm(loghousecount~Attractions+CommercialServices+EducationandHealth+ManufacturingandProduction+Retail+SportandEntertainment+Transport+simpson+between2km+close2km,
                      data=lsoaxy, 
                      nb2listw(LSOA_knn, style="C"), 
                      method = "eigen")
summary(lagmodel3)

# Moran' I test on residuals of SLM model
lsoaxy <- lsoaxy %>%
  mutate(lagmodel3resids = residuals(lagmodel3))

Nearest_neighbour_lag <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(lagmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()
Nearest_neighbour_lag

#STAGE 3

#read geographic shape file:London LSOA
lsoamap<-st_read("LSOA_2011_London_gen_MHW.shp")
qtm(lsoamap)

#read CSV file:Y and X variables of each LSOA
xy3<-read.csv("loghousecount.csv",sep = ",",header = TRUE)
xy3n<-read.csv("loghousecountnolsoa.csv",sep = ",",header = TRUE)

#Add data to geographic files
lsoaxy <- left_join(lsoamap, xy3, by = c("LSOA11CD"="LSOA11CD"))

#run OLS model
olsmodel3<-lm(loghousecount~Attractions+CommercialServices+EducationandHealth+ManufacturingandProduction+Retail+SportandEntertainment+Transport+simpson+AccommodationType+AgeStructure+EthniGroup+HouseholdComposition+Socioeconomic+Tenure+between2km+close2km,data=xy3n)
summary(olsmodel3)

#Obtain the residuals of OLS model and test whether it satisfies the normal distribution
lsoaxy$olsmodel3resids <- residuals(olsmodel3)
ggplot(lsoaxy, aes(x=olsmodel3resids)) + 
  geom_histogram()

#View residuals and heteroscedasticity of OLS model
par(mfrow=c(2,2)) 
plot(olsmodel3)

#Test spatial autocorrelation

#Extract lsoa centroid
coordsLSOA <- lsoaxy%>%
  st_centroid()%>%
  st_geometry()

#Create matrix
knn_LSOA <-coordsLSOA %>%
  knearneigh(., k=4)

LSOA_knn <- knn_LSOA %>%
  knn2nb()

LSOA.knn_4_weight <- LSOA_knn %>%
  nb2listw(., style="C")

# Moran' I test on residuals of OLS model
Nearest_neighbour <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(olsmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour

#Run spatial lag regression
lagmodel3 <- lagsarlm(loghousecount~Attractions+CommercialServices+EducationandHealth+ManufacturingandProduction+Retail+SportandEntertainment+Transport+simpson+AccommodationType+AgeStructure+EthniGroup+HouseholdComposition+Socioeconomic+Tenure+between2km+close2km,
                      data=lsoaxy, 
                      nb2listw(LSOA_knn, style="C"), 
                      method = "eigen")
summary(lagmodel3)

# Moran' I test on residuals of SLM model
lsoaxy <- lsoaxy %>%
  mutate(lagmodel3resids = residuals(lagmodel3))

Nearest_neighbour_lag <- lsoaxy %>%
  st_drop_geometry()%>%
  dplyr::select(lagmodel3resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()
Nearest_neighbour_lag


