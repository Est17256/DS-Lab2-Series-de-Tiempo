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
data$DieselLS<-gsub(",","",data$DieselLS)
data$DieselLS<-as.numeric(data$DieselLS)
DataLimpia = data
DataLimpia$Diesel = DataLimpia$Diesel + DataLimpia$DieselLS

#Se crea la serie de tiempo


sDiesel <- ts(DataLimpia$Diesel, start=c(2001, 1), end=c(2020, 3), frequency=12)

# plot series
plot(sDiesel)
abline(reg=lm(sDiesel~time(sDiesel)), col=c("red"))


plot(aggregate(sDiesel,FUN=mean))
dec.AirPass<-decompose(sDiesel)
plot(dec.AirPass)
plot(dec.AirPass$seasonal)
