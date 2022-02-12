
#Instalación de paquetes necesarios para la ejecución de dicho script
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("Ggally")

#cargar paquete readr
library(readr)

#buscar la ruta del archivo csv
file.choose()

#Para importar el archivo csv de calidad del aire

#copiar ruta de la consola y guardarla en una variable tipo caracter
ruta_Excel_CalidadAire <- "C:\\Users\\Usuari\\Downloads\\CalidadAire14_19_zonaProv.csv"

#Importar csv datos Calidad del Aire con código R
data_CalidadAire <- read.csv(ruta_Excel_CalidadAire, header=TRUE, sep=';')

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
ruta_Excel_datosCP <- "C:\\Users\\Usuari\\Downloads\\Datos_CP_14_19_prov.csv"

#Importar csv datos CP con código R
data_datosCP <- read.csv(ruta_Excel_datosCP, header=TRUE, sep=";")

#Para ver los datos contenidos dentro del dataset de datos de CP (Cancer pulmon), se utiliza el siguiente comando
View(data_datosCP)

#Realizar la unión de los dos datasets por los campos Year y CodProv

#Primero de todo, se precisa cambiar nombres a ciertas columnas para que, a la hora de hacer el join, el nombre de estas coincidan en los dos datasets. 
# Para cambiar los nombres de las variables del DF data_datosCP, hacemos lo siguiente.
names (data_datosCP) = c("Year", "CodProv", "CIE10", "Provincia", "SumaTotal", "value_f", "value_m")

#Ahora, se procede a realizar la unión de los datasets 
data_final <- merge(data_datosCP, CalidadAire_Final, by = c("Year", "CodProv"))

View(data_final)

#Seleccionar las columnas que nos interesan para realizar la correlación
Data_Correlacion_SumaTotal <- select(data_final, Factor_PM10, Factor_PM2.5, Factor_NO2, Factor_O3, SumaTotal)
View(Data_Correlacion_SumaTotal)

#Análisis de correlación
round(cor(Data_Correlacion_SumaTotal),4)

library(ggplot2)
library(GGally)

ggpairs(Data_Correlacion_SumaTotal, lower = list(continuos="smooth"), diag = list(continuos="barDiag"), axislabels="none")

#Generación del modelo
modelo <- lm(SumaTotal~Factor_PM10 + Factor_PM2.5 + Factor_NO2 + Factor_O3, data = Data_Correlacion_SumaTotal)
summary(modelo)