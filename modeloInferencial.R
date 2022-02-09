
#Instalación de paquetes necesarios para la ejecución de dicho script
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("GGally")

#cargar paquete readr
library(readr)

#buscar la ruta del archivo csv
file.choose()

#Para importar el archivo csv de calidad del aire

#copiar ruta de la consola y guardarla en una variable tipo caracter
ruta_Excel_CalidadAire <- "Practica2\\CalidadAire14_19_zonaProv.csv"

#Importar csv datos Calidad del Aire con código R
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

data_final$Incidencia = round( (data_final$SumaTotal * 100000) / data_final$Population , 2 )
View(data_final)
df_2014 = subset(data_final, Year == 2014 )

boxplot(data_final$SumaTotal)
boxplot(data_final$Incidencia)

#Seleccionar las columnas que nos interesan para realizar la correlación
Data_Correlacion_SumaTotal <- select(data_final, Factor_PM10, Factor_PM2.5, Factor_NO2, Factor_O3, Incidencia)
View(Data_Correlacion_SumaTotal)


scaled = data.frame(scale(Data_Correlacion_SumaTotal,center = TRUE, scale = TRUE))
#Análisis de correlación
round(cor(scaled),4)

library(ggplot2)
library(GGally)

ggpairs(scaled, lower = list(continuos="smooth"), diag = list(continuos="barDiag"), axislabels="none")

#Generación del modelo
modelo <- lm(data_final~Factor_PM10 + Factor_PM2.5 + Factor_NO2 + Factor_O3, data = scaled)
summary(modelo)

#Backguard elminitation
modelo2 = lm(Incidencia~Factor_PM10 + Factor_PM2.5  + Factor_O3, data = scaled)
summary(modelo2)

# Evaluando el modelo
#install.packages("caTools")

library(caTools)
set.seed(123)
split = sample.split(Data_Correlacion_SumaTotal$Incidencia, SplitRatio = 0.8)
training_set = subset(Data_Correlacion_SumaTotal, split == TRUE)
testing_set = subset(Data_Correlacion_SumaTotal, split == FALSE)

# Ajustar el modelo de RegresiÃ³n Lineal MÃºltiple con el Conjunto de Entrenamiento
regression =  lm(Incidencia~Factor_PM10 + Factor_PM2.5  + Factor_O3, data = training_set)

# Predecir los resultados con el conjunto de testing
#install.packages("caret")
library(caret)

y_pred = predict(regression, newdata = testing_set)

RMSE(y_pred, testing_set$Incidencia)
R2(y_pred, testing_set$Incidencia)
y_pred
testing_set$Incidencia
y_compared = data.frame(y_pred,testing_set$Incidencia )

plot( testing_set$Incidencia  - y_pred)

# Support Vector Regression

library(e1071)
regression = svm(formula = Incidencia~Factor_PM10 + Factor_PM2.5  + Factor_O3,
                 data = training_set, 
                 type = "eps-regression", 
                 kernel = "radial")
y_pred = predict(regression, newdata =testing_set )
RMSE(y_pred, testing_set$Incidencia)
R2(y_pred, testing_set$Incidencia)
y_pred
testing_set$Incidencia



#Decission Tree Regressor
library(randomForest)
set.seed(1234)
regression = randomForest(x = training_set[, 1:4],
                          y = training_set$Incidencia,
                          ntree = 100)
y_pred = predict(regression, newdata =testing_set )
RMSE(y_pred, testing_set$Incidencia)
R2(y_pred, testing_set$Incidencia)
y_pred
testing_set$Incidencia
y_compared = data.frame(y_pred,testing_set$Incidencia )

plot( testing_set$Incidencia  - y_pred)


