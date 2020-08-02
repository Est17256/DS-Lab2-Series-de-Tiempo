install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")
install.packages("tabulizer")

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(tabulizer)
library(dplyr)
library(stringr)

# Leer de un PDF
pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

# Normalidad
gasSupImp <- datosImp$GasSuperior
gasSupImp <- str_replace_all(gasSupImp, ",","")
gasSupImp <- as.numeric(gasSupImp)
gasSupImp[is.na(gasSupImp)] <- 0
hist(gasSupImp)

gasRegImp <- datosImp$GasRegular
gasRegImp <- str_replace_all(gasRegImp, ",","")
gasRegImp <- as.numeric(gasRegImp)
gasRegImp
hist(gasRegImp)

dieselImp <- datosImp$Diesel
dieselImp <- str_replace_all(dieselImp, ",","")
dieselImp <- as.numeric(dieselImp)
dieselImp[is.na(dieselImp)] <- 0
dieselImp
hist(dieselImp)

dieselLSImp <- datosImp$DieselLS
dieselLSImp <- str_replace_all(dieselLSImp, ",","")
dieselLSImp <- as.numeric(dieselLSImp)
dieselLSImp
hist(dieselLSImp)

dieselULSImp <- datosImp$DieselULS
dieselULSImp <- str_replace_all(dieselULSImp, ",","")
dieselULSImp <- as.numeric(dieselULSImp)
dieselULSImp
hist(dieselULSImp)

# Barplots promedios por mes
meses <- datosImp$Mes
meses

meses_diesel <- data.frame(meses,dieselImp, header=TRUE)
mean_MD <- aggregate(meses_diesel$dieselImp, list(meses_diesel$meses), mean)
mean_MD <- mean_MD[-c(13),]
mean_MD$Group.1 <- as.numeric(mean_MD$Group.1)
mean_MD <- mean_MD[order(mean_MD$Group.1),]
barplot(mean_MD$x, main="Diesel por mes",
        names.arg=mean_MD$Group.1)

meses_gasReg <- data.frame(meses,gasRegImp, header=TRUE)
mean_GR <- aggregate(meses_gasReg$gasRegImp, list(meses_gasReg$meses), mean)
mean_GR <- mean_GR[-c(13),]
mean_GR$Group.1 <- as.numeric(mean_GR$Group.1)
mean_GR <- mean_GR[order(mean_GR$Group.1),]
barplot(mean_GR$x, main="Gas regular por mes",
        names.arg=mean_GR$Group.1)

meses_gasSup <- data.frame(meses,gasSupImp, header=TRUE)
mean_GS <- aggregate(meses_gasSup$gasSupImp, list(meses_gasSup$meses), mean)
mean_GS <- mean_GS[-c(13),]
mean_GS$Group.1 <- as.numeric(mean_GS$Group.1)
mean_GS <- mean_GS[order(mean_GS$Group.1),]
barplot(mean_GS$x, main="Gas superior por mes",
        names.arg=mean_GS$Group.1)


# Barplots totales
barplot(dieselImp, main="Diesel en los ultimos años")
barplot(dieselLSImp, main="Diesel LSI en los ultimos años")
