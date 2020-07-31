#Universidad del Valle de Guatemala
#23/07/2020
#Luis Esturban 17256
#Luis Fernandez 16344
#Juan Menchu 16150
#Lab2
# Leer de un PDF
# install.packages("tabulizer")
library(tabulizer)
library(dplyr)
library(stringr)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar
data<-datosImp[datosImp$GLP!="GLP",]

#Limpiar

data$GasRegular<-as.character(levels(data$GasRegular))[data$GasRegular]
data$GasRegular<-gsub(",","",data$GasRegular)
data$GasRegular<-as.numeric(data$GasRegular)

data$Diesel[data$Diesel =="-"] <- 0
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