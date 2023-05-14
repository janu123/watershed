library(sf)
library(sp)
library(raster)
library(dplyr)
library(tidyverse)

NDVI<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/NDVI_2018.shp")
Rainfall<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/Rainfall_2018.shp")
LST<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/LST_2018.shp")

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
st_write(df, "result_2018.shp")
library(gstat)
df<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/result.shp")
wd <- ("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles")
setwd(wd)
NDVI_2018 <- raster("2018.tif")
spdat <- sp::spTransform(df, CRS(proj4string(DEM)))
plot(NDVI_2018)

fitmax <- gstat::gstat(formula = pred ~ 1, data = df, nmax = 4, set = list(idp = .5))
maxint <- raster::interpolate(NDVI_2018, model=fitmax)
plot(maxint, col=rev(heat.colors(255)))
plot(vt, add=TRUE)


