#Universidad del Valle de Guatemala
#23/07/2020
#Luis Esturban 17256
#Luis Fernandez 16344
#Juan Menchu 16150
#Lab2

########## Se importan las librerias ##########

library(tabulizer)
library(dplyr)
library(stringr)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

########## Lectura de PDF ##########

pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

########## Limpieza de los datos ##########

#Limpieza de titulos
data<-datosImp[datosImp$GLP!="GLP",]
#Limpieza de gas superior
data$GasSuperior<-as.character(levels(data$GasSuperior))[data$GasSuperior]
data$GasSuperior<-gsub(",","",data$GasSuperior)
data$GasSuperior<-as.numeric(data$GasSuperior)
#Limpieza de gas regular
data$GasRegular<-as.character(levels(data$GasRegular))[data$GasRegular]
data$GasRegular<-gsub(",","",data$GasRegular)
data$GasRegular<-as.numeric(data$GasRegular)
#Limpieza y union de diesel
data$Diesel<-as.character(levels(data$Diesel))[data$Diesel]
data$Diesel[data$Diesel =="-"] <- 0
data$Diesel<-gsub(",","",data$Diesel)
data$Diesel<-as.numeric(data$Diesel)
data$DieselLS<-as.character(levels(data$DieselLS))[data$DieselLS]
data$DieselLS[data$DieselLS =="-"] <- 0
data2 = data

#COnvertir
options(digits=9)
data2$Anio = as.numeric(data2$Anio)
data2$Mes = as.numeric(data2$Mes)
data2$Diesel = as.numeric(gsub(",", "", data2$Diesel))
data2$DieselLS = as.numeric(gsub(",", "", data2$DieselLS))
data$Diesel[data$Diesel =="-"] <- 0
data$DieselLS[data$DieselLS =="-"] <- 0
data2 = data
#Unir Diesel
data2$Diesel = data2$Diesel + data2$DieselLS



#Se crea la serie de tiempo
myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

# plot series
plot(myts)




# SECCION GAS
# new dataframe para gas regular
gasRegImp <- data.frame(datosImp$GasRegular) 
names(gasRegImp)[1] <- "GasRegular"
gasRegImp
gasRegImp<-gasRegImp[!(grepl("Gasolina",gasRegImp$GasRegular)),]

# Convertion to numerics
gasRegImp <- str_replace_all(gasRegImp, ",","")
gasRegImp <- as.numeric(gasRegImp)
gasRegImp[is.na(gasRegImp)] <- 0

# Convertion to time series
gasRegImp <- ts(gasRegImp, frequency = 12, start = 2001)
gasRegImp

start(gasRegImp)
end(gasRegImp)
frequency(gasRegImp)

# plot
plot(gasRegImp)
abline(reg=lm(gasRegImp~time(gasRegImp)), col=c("red"))

# decompose
plot(aggregate(gasRegImp,FUN=mean))
dec.gasRegImp <- decompose(gasRegImp)
plot(dec.gasRegImp)

# transformar a logaritmica
logGasRegImp <- log(gasRegImp)
plot(decompose(logGasRegImp))

# Augmented Dickey-Fuller Test
adfTest(logGasRegImp)
adfTest(diff(logGasRegImp))

# autocorrelación
acf(logGasRegImp)

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logGasRegImp),12)
pacf(diff(logGasRegImp))

# Modelo
auto.arima(logGasRegImp, D=1) #auto arima con estacionalidad forzada

fit <- arima(logGasRegImp, c(2, 0, 1), seasonal = list(order=c(1,1,0), perdiod = 12))
forecastAP <- forecast(fit, level = c(95), h = 3*12)
autoplot(forecastAP)

