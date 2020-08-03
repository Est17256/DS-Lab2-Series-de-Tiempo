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
library(prophet)

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
data$DieselLS<-gsub(",","",data$DieselLS)
data$DieselLS<-as.numeric(data$DieselLS)
DataLimpia = data
DataLimpia$Diesel = DataLimpia$Diesel + DataLimpia$DieselLS

#Seccion Diesel
sDiesel <- ts(DataLimpia$Diesel, start=c(2001, 1), end=c(2020, 3), frequency=12)
sDiesel

start(sDiesel)
end(sDiesel)
frequency(sDiesel)

# plot series
plot(sDiesel)
abline(reg=lm(sDiesel~time(sDiesel)), col=c("red"))

# decompose
plot(aggregate(sDiesel,FUN=mean))
dec.sDiesel <- decompose(sDiesel)
plot(dec.sDiesel)

# transformar a logaritmica
logsDiesel <- log(sDiesel)
plot(decompose(logsDiesel))

# Augmented Dickey-Fuller Test =1
adfTest(logsDiesel)
adfTest(diff(logsDiesel))

# autocorrelación
acf(logsDiesel,12)

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logsDiesel),12)
pacf(diff(logsDiesel))

# Modelo
auto.arima(logsDiesel, D=1)

fit <- arima(logsDiesel, c(2, 0, 0), seasonal = list(order=c(2,1,0), perdiod = 12))
forecastAP <- forecast(fit, level = c(95), h = 5*12)
autoplot(forecastAP)

#Prediccion de anio 2018,2019,2020

DataLimpia2<-DataLimpia[DataLimpia$Anio!="2018",]
DataLimpia2<-DataLimpia2[DataLimpia2$Anio!="2019",]
DataLimpia2<-DataLimpia2[DataLimpia2$Anio!="2020",]
sDieselPred <- ts(DataLimpia2$Diesel, start=c(2001, 1), end=c(2017, 12), frequency=12)
sDieselPred
logsDieselPred <- log(sDieselPred)
auto.arima(logsDieselPred, D=1)
fit <- arima(logsDieselPred, c(2, 0, 2), seasonal = list(order=c(1,1,0), perdiod = 12))
forecastAP <- forecast(fit, level = c(95), h = 2*12)
autoplot(forecastAP)

#Predicion 2020
DataLimpia3<-DataLimpia[DataLimpia$Anio!="2020",]

sDieselPred2 <- ts(DataLimpia3$Diesel, start=c(2001, 1), end=c(2019, 12), frequency=12)
sDieselPred2
logsDieselPred2 <- log(sDieselPred2)
auto.arima(logsDieselPred2, D=1)
fit <- arima(logsDieselPred2, c(2, 0, 0), seasonal = list(order=c(2,1,0), perdiod = 12))
forecastAP <- forecast(fit, level = c(95), h = 1*12)
autoplot(forecastAP)

#Preparacion para modelo Prophet
PruebaData<-DataLimpia
PruebaData$Anio<-as.character(levels(PruebaData$Anio))[PruebaData$Anio]
PruebaData$Mes<-as.character(levels(PruebaData$Mes))[PruebaData$Mes]
PruebaData$Mes[PruebaData$Mes =="1"] <- "01-01"
PruebaData$Mes[PruebaData$Mes =="2"] <- "02-01"
PruebaData$Mes[PruebaData$Mes =="3"] <- "03-01"
PruebaData$Mes[PruebaData$Mes =="4"] <- "04-01"
PruebaData$Mes[PruebaData$Mes =="5"] <- "05-01"
PruebaData$Mes[PruebaData$Mes =="6"] <- "06-01"
PruebaData$Mes[PruebaData$Mes =="7"] <- "07-01"
PruebaData$Mes[PruebaData$Mes =="8"] <- "08-01"
PruebaData$Mes[PruebaData$Mes =="9"] <- "09-01"
PruebaData$Mes[PruebaData$Mes =="10"] <- "10-01"
PruebaData$Mes[PruebaData$Mes =="11"] <- "11-01"
PruebaData$Mes[PruebaData$Mes =="12"] <- "12-01"
PruebaData$Anio<-paste(PruebaData$Anio, PruebaData$Mes, sep="-")
PruebaData2<-PruebaData[,c("Anio","Diesel")]
names(PruebaData2)=c("ds","y")

PruebaData2

#Modelo Prophet
library(prophet)
fitProphet<-prophet(PruebaData2,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods =1,freq = "month", include_history = T)
p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)

# SECCION GAS REGULAR
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

# Modelo ARIMA
auto.arima(logGasRegImp, D=1) #auto arima con estacionalidad forzada

fit <- arima(logGasRegImp, c(2, 0, 1), seasonal = list(order=c(1,1,0), perdiod = 12))
forecastAP <- forecast(fit, level = c(95), h = 3*12)
autoplot(forecastAP)

# Modelo Prophet
# data
gasRegImp4proph <- data.frame(paste(datosImp$Mes, '- 1 -',datosImp$Anio), datosImp$GasRegular) 
names(gasRegImp4proph)[1] <- "ds"
names(gasRegImp4proph)[2] <- "y"
gasRegImp4proph<-gasRegImp4proph[!(grepl("Gasolina",gasRegImp4proph$y)),]
gasRegImp4proph$ds <- as.Date(gasRegImp4proph$ds , format = "%m - %d - %y")
gasRegImp4proph

#modelo
fitProphet<-prophet(gasRegImp_4prophet,yearly.seasonality = T, weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods =1,freq = "month", include_history = T)
p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)

