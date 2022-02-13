

ruta_Excel_CalidadAire <- "Practica2\\CalidadAire14_19_zonaProv.csv"

#Importar csv datos Calidad del Aire con c?digo R
data_CalidadAire <- read.csv(ruta_Excel_CalidadAire, header=TRUE, sep=';', dec = ',')

#Para ver los datos contenidos dentro del dataset de calidad del aire, se utiliza el siguiente comando
View(data_CalidadAire)

#Seleccionamos las columnas que nos interesan y las guardamos en un nuevo dataset 

library (dplyr)
CalidadAire <- select(data_CalidadAire, Year, CodProv, Population, PM10.population.weighted.average..ug.m3., PM2.5.population.weighted.average..ug.m3., NO2.population.weighted.average..ug.m3., O3.SOMO35.population.weighted.average..ug.days.m3.)

names (CalidadAire) = c("Year", "CodProv", "Population", "Factor_PM10", "Factor_PM2.5", "Factor_NO2", "Factor_O3")

View(CalidadAire)

# Sumarizar todas las columnas excepto las columnas group_by usando "sum"


CalidadAire_Final <- CalidadAire %>% 
  group_by(CodProv, Year) %>% 
  summarise_all(sum)

#Para importar el archivo csv de datos CP

#Buscar la ruta del nuevo archivo csv
file.choose()

#Copiar ruta de la consola y guardarla en una variable tipo caracter
ruta_Excel_datosCP <- "Practica2\\Datos_CP_14_19_prov.csv"

#Importar csv datos CP con c?digo R
data_datosCP <- read.csv(ruta_Excel_datosCP, header=TRUE, sep=";")

#Para ver los datos contenidos dentro del dataset de datos de CP (Cancer pulmon), se utiliza el siguiente comando
View(data_datosCP)

#Realizar la uni?n de los dos datasets por los campos Year y CodProv

#Primero de todo, se precisa cambiar nombres a ciertas columnas para que, a la hora de hacer el join, el nombre de estas coincidan en los dos datasets. 
# Para cambiar los nombres de las variables del DF data_datosCP, hacemos lo siguiente.
names (data_datosCP) = c("Year", "CodProv", "CIE10", "Provincia", "SumaTotal", "value_f", "value_m")

#Ahora, se procede a realizar la uni?n de los datasets 
data_final <- merge(data_datosCP, CalidadAire_Final, by = c("Year", "CodProv"))

data_final$Prevalencia = round( (data_final$SumaTotal * 100000) / data_final$Population , 2 )

n <- length(data_final$Prevalencia)    # El tamaño válido de la muestra
media <- mean(data_final$Prevalencia) # la media 
desv <- sd(data_final$Prevalencia)  # La desviación estándar. Datos históricos
nivelconfianza = 0.95

#error.est <- desv/sqrt(n) # Calculamos el error estándar
#margen.error <- 1.644854 * error.est # nivel de confianza de 90% 


lim.inf <- media - margen.error # Límite inferior del intervalo
lim.inf

lim.sup <- media + margen.error # Límite superior del intervalo
lim.sup

install.packages("BSDA")
library(BSDA)

zsum.test(mean.x=media,sigma.x=desv, n.x=n,conf.level=nivelconfianza)


par(mfrow=c(1, 2))
require(car)  # Debe instalar antes el paquete car
qqPlot(data_final$Prevalencia, pch=19,
       main='QQplot para la prevalencia',
       xlab='Cuantiles teóricos',
       ylab='Cuantiles muestrales')

hist(data_final$Prevalencia , freq=TRUE,
     main='Histograma para la prevalencia',
     xlab='Prevalencia',
     ylab='Frecuencia')


