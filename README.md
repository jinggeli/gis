# gis
gisassignment
#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
getwd()
setwd(dir = "G:/zhuom/CASA/course practical/03 GIS/R/")

#Read spatial map data of London

st_read("London_Borough_Excluding_MHW.shp")
rm(list = ls())
londborough<-st_read("London_Borough_Excluding_MHW.shp")
qtm(londborough)

#Read the data of variables

covid<-read.table("covid.csv",sep = ",",header = TRUE)
education<-read.table("education.csv",sep = ",",header = TRUE)
ethnic<-read.table("ethnic.csv",sep = ",",header = TRUE)
weeklypay<-read.table("weeklypay.csv",sep = ",",header = TRUE)
accommodation<-read.table("accommodation.csv",sep = ",",header = TRUE)
library(dplyr)

# Map data onto space and show the distribution

londborough <- left_join(londborough, covid, by = c("GSS_CODE"="ONS.Code"))
londborough <- left_join(londborough, ethnic, by = c("GSS_CODE"="ONS.Code"))
londborough <- left_join(londborough, education, by = c("GSS_CODE"="ONS.Code"))
londborough <- left_join(londborough, weeklypay, by = c("GSS_CODE"="ONS.Code"))
londborough <- left_join(londborough, accommodation, by = c("GSS_CODE"="ONS.Code"))
plot(londborough[9])
plot(londborough[11])
plot(londborough[13])
plot(londborough[15])
plot(londborough[17])

#Verify that the data meets the normal distribution, and if not, perform the logarithmic transformation of the variable

ggplot(londborough, aes(x=Prevalence)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.0075)

londborough$Prevalence_log <- log(londborough$Prevalence)

ggplot(londborough, aes(x=Prevalence_log)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.155)

ggplot(londborough, aes(x=Declies_Ethnic.Diversity)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1.25)

ggplot(londborough, aes(x=Percentage_no.qualification)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1.8)

londborough$Percentage_no.qualification_log <- log(londborough$Percentage_no.qualification)

ggplot(londborough, aes(x=Percentage_no.qualification_log)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.14)

ggplot(londborough, aes(x=Weekly.Pay)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 40)

londborough$Weekly.Pay_log <- log(londborough$Weekly.Pay)

ggplot(londborough, aes(x=Weekly.Pay_log)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.07)

ggplot(londborough, aes(x=Declies_accommodation.Diversity)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.675)

londborough$Declies_accommodation.Diversity_log <- log(londborough$Declies_accommodation.Diversity)

ggplot(londborough, aes(x=Declies_accommodation.Diversity_log)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.17)

#build OLS model
  
model1 <-lm(Prevalence_log~Declies_Ethnic.Diversity+Percentage_no.qualification_log+Weekly.Pay_log+Declies_accommodation.Diversity_log, data = londborough)
summary(model1)

#Check whether the residuals are independent

londborough$res_fit1 <- residuals(model1)
londborough$sd_breaks <- scale(londborough$res_fit1)[,1]
summary(londborough$sd_breaks)

my_breaks <- c(-14,-3,-2,-1,1,2,3,14)

tm_shape(londborough) + 
  tm_fill("sd_breaks", title = "Residuals", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

#Setting up a space matrix

londonborough_sp <- as(londborough, "Spatial")

w <- poly2nb(londonborough_sp, row.names=londonborough_sp$FIPSNO)
summary(w)
wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')

#Moran I test

lm.morantest(model1, rwm, alternative="two.sided")

#LM test

lm.LMtests(model1, rwm, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

#build the SLM model

model1_lag <- lagsarlm(Prevalence_log~Declies_Ethnic.Diversity+Percentage_no.qualification_log+Weekly.Pay_log+Declies_accommodation.Diversity_log, data=londborough, rwm)
summary(model1_lag)

#Check whether the residuals are independent by Moran I test (Queen)


coordsW<-londborough%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

LWard_nb<-londborough%>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")

plot(londborough)

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")


londborough$res_fit2 <- residuals(model1_lag)

Queen2<-londborough%>%
  st_drop_geometry()%>%
  dplyr::select(res_fit2)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

Queen2
