library(imputeTS)
library(readr)
library(xts)
library(ggplot2)

model_data <- read_csv("C:/Users/janan/OneDrive/Desktop/model_data.csv")

model_data$LST<-na_interpolation(model_data$LST)
model_data$NDVI<-na_interpolation(model_data$NDVI)
model_data$Rainfall<-na_interpolation(model_data$Rainfall)
df_NDVI <- xts(model_data$NDVI, order.by = model_data$Date)
df_LST<- xts(model_data$LST, order.by = model_data$Date)
df_Rainfall<-xts(model_data$Rainfall, order.by = model_data$Date)
plot.xts(df_NDVI,col="blue", main="NDVI")
plot.xts(df_LST,col="blue", main="LST")
plot.xts(df_Rainfall,col="blue",main = "Rainfall")
df_NDVI<-ts(df_NDVI)
df_LST<-ts(df_LST)
install.packages("dLagM")
library(dLagM)
data(seaLevelTempSOI)
rolCorPlot(y = df_NDVI, x = df_LST, width = c(3, 5, 7, 11),
           level = 0.95,
           SDtest = TRUE)

model.ardl = ardlDlm(x = model_data$LST,
                     y = model_data$NDVI, p = 1 , q = 1)
summary(model.ardl)
residuals(model.ardl)
coef(model.ardl)
fitted(model.ardl)

model.ardl = ardlDlm(x = model_data$Rainfall,
                     y = model_data$NDVI, p = 1 , q = 1)
summary(model.ardl)

rem.p = c(0,1) # 0 removes the main effect of X.t
rem.q = c(1,3)
remove = list(p = rem.p , q = rem.q)
model.ardl = ardlDlm(x = model_data$LST,
                     y = model_data$NDVI, p = 2 , q = 3 , remove = remove)
summary(model.ardl)


model.ardlDlm = ardlDlm(formula = NDVI ~ LST + Rainfall,
                        data = model_data , p = 2 , q = 1 )

summary(model.ardlDlm)
model.dlm = dlm(x = model_data$LST,
                y = model_data$NDVI , q = 5)
summary(model.dlm)
residuals(model.dlm)
coef(model.dlm)
fitted(model.dlm)

model.dlm = dlm(x = model_data$Rainfall,
                y = model_data$NDVI , q = 5)
summary(model.dlm)

model.dlm = dlm(formula =  NDVI ~ LST + Rainfall,
                data = model_data , q = 5)
summary(model.dlm)
