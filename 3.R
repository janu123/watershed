library(sf)
library(sp)
library(raster)
library(dplyr)
library(tidyverse)

NDVI<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/NDVI_2020.shp")
Rainfall<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/Rainfall_2020.shp")
LST<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/LST_2020.shp")

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
wd <- ("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles")
setwd(wd)
st_write(df, "result_2020.shp")
library(gstat)
df<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/result_2020.shp")

NDVI_2020 <- raster("2020.tif")
plot(NDVI_2020)

fitmax <- gstat::gstat(formula = pred ~ 1, data = df, nmax = 4, set = list(idp = .5))
maxint <- raster::interpolate(NDVI_2020, model=fitmax)
plot(maxint, col=rev(heat.colors(255)))
plot(vt, add=TRUE)


