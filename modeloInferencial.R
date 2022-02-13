
#Instalaci?n de paquetes necesarios para la ejecuci?n de dicho script
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
View(data_final)

df_2014 = subset(data_final, Year == 2014 )

boxplot(data_final$SumaTotal)
boxplot(data_final$Prevalencia)

#Seleccionar las columnas que nos interesan para realizar la correlaci?n
Data_Correlacion_SumaTotal <- select(data_final, Year ,Factor_PM10, Factor_PM2.5, Factor_NO2, Factor_O3, Prevalencia)
View(Data_Correlacion_SumaTotal)


scaled = data.frame(scale(Data_Correlacion_SumaTotal,center = TRUE, scale = TRUE))
#An?lisis de correlaci?n
#method="spearman"
round(cor(Data_Correlacion_SumaTotal),4)

library(ggplot2)
library(GGally)

ggpairs(Data_Correlacion_SumaTotal, lower = list(continuos="smooth"), diag = list(continuos="barDiag"), axislabels="none")

#Generaci?n del modelo
modelo <- lm(Prevalencia~Factor_PM10 + Factor_PM2.5 + Factor_NO2 + Factor_O3, data = Data_Correlacion_SumaTotal)
summary(modelo)

#Backguard elminitation
modelo2 = lm(Prevalencia~Factor_PM10 + Factor_PM2.5  + Factor_O3, data = Data_Correlacion_SumaTotal)
summary(modelo2)

# Evaluando el modelo
#install.packages("caTools")

library(caTools)
set.seed(123)
split = sample.split(Data_Correlacion_SumaTotal$Prevalencia, SplitRatio = 0.8)
training_set = subset(Data_Correlacion_SumaTotal, split == TRUE)
testing_set = subset(Data_Correlacion_SumaTotal, split == FALSE)

# Ajustar el modelo de Regresión Lineal Múltiple con el Conjunto de Entrenamiento
regression =  lm(Prevalencia~Factor_PM10 + Factor_PM2.5  + Factor_O3, data = training_set)

# Predecir los resultados con el conjunto de testing
#install.packages("caret")
library(caret)

y_pred = predict(regression, newdata = testing_set)

RMSE(y_pred, testing_set$Prevalencia)
R2(y_pred, testing_set$Prevalencia)
y_pred
testing_set$Prevalencia
y_compared = data.frame(y_pred,testing_set$Prevalencia )

plot( testing_set$Prevalencia  - y_pred)

# Support Vector Regression

library(e1071)
regression = svm(formula = Prevalencia~Factor_PM10 + Factor_PM2.5  + Factor_O3,
                 data = training_set, 
                 type = "eps-regression", 
                 kernel = "radial")
y_pred = predict(regression, newdata =testing_set )
RMSE(y_pred, testing_set$Prevalencia)
R2(y_pred, testing_set$Prevalencia)
y_pred
testing_set$Prevalencia



#Decission Tree Regressor
#install.packages("randomForest")
library(randomForest)
set.seed(1234)
regression = randomForest(x = training_set[, 1:4],
                          y = training_set$Prevalencia,
                          ntree = 100)
y_pred = predict(regression, newdata =testing_set )
RMSE(y_pred, testing_set$Prevalencia)
R2(y_pred, testing_set$Prevalencia)
y_pred
testing_set$Prevalencia
y_compared = data.frame(y_pred,testing_set$Prevalencia )

plot( testing_set$Prevalencia  - y_pred)

plot(data_final$Prevalencia~ data_final$Year)



###  Intervalo de confianza bilateral para la diferencia de medias 

n <- length(data_final$Prevalencia)    # El tamaño válido de la muestra
media <- mean(data_final$Prevalencia) # la media 
desv <- sd(data_final$Prevalencia)  # La desviación estándar. Datos históricos
nivelconfianza = 0.80

#error.est <- desv/sqrt(n) # Calculamos el error estándar
#margen.error <- 1.644854 * error.est # nivel de confianza de 90% 


#lim.inf <- media - margen.error # Límite inferior del intervalo
#lim.inf

#lim.sup <- media + margen.error # Límite superior del intervalo
#lim.sup

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







