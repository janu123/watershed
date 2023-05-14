library(sf)
library(sp)
library(raster)
library(dplyr)
library(tidyverse)

NDVI<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/2017")
Rainfall<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/Rainfall")
LST<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/LST_Celsius.tif")

plot(NDVI)
NDVI$fid<-1:nrow(NDVI)
Rainfall$fid<-1:nrow(Rainfall)
LST$fid<-1:nrow(LST)
df_list <- list(NDVI,Rainfall,LST)      

df<-df_list %>% reduce(st_join)
class(df)
mod<-lm(NDVI~precipitat+LST_Day_1k,data=df)
summary(mod)
df$pred<-mod$fitted.values
df$res<-mod$residuals
st_write(df, "result.shp")
library(gstat)
df<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/result.shp")
wd <- ("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles")
setwd(wd)
NDVI_2017 <- raster("NDVI.tif")
spdat <- sp::spTransform(df, CRS(proj4string(DEM)))
plot(NDVI_2017)

fitmax <- gstat::gstat(formula = pred ~ 1, data = df, nmax = 4, set = list(idp = .5))
maxint <- raster::interpolate(DEM, model=fitmax)
plot(maxint, col=rev(heat.colors(255)))
plot(vt, add=TRUE)


