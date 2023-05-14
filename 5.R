library(sf)
library(sp)
library(raster)
library(dplyr)
library(tidyverse)

NDVI<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/NDVI_2021.shp")
Rainfall<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/Rainfall_2021.shp")
LST<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/LST_2021.shp")

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
st_write(df, "result_2021.shp")
library(gstat)
df<-sf::read_sf("C:/Users/janan/OneDrive/Documents/ArcGIS/Projects/MyProject/shapefiles/result_2021.shp")

NDVI_2021 <- raster("2021.tif")
plot(NDVI_2021)

fitmax <- gstat::gstat(formula = pred ~ 1, data = df, nmax = 4, set = list(idp = .5))
maxint <- raster::interpolate(NDVI_2021, model=fitmax)
plot(maxint, col=rev(heat.colors(255)))
plot(vt, add=TRUE)


