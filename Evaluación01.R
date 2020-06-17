####Evaluación 1 - Analisis en R ####
#Abrir BD 
getwd()
setwd("C:/Users/JGALLO/Desktop/Clase4R4DS")
rm(list = ls())
dir()

data<-read.table(file="201804_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                 col.names = c("Cod_Empresa","Cod_Usuario", "Cod_Suministro","Fecha","Ener_Act","Ener_Reac","Periodo"),
                 colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))
head(data)

#Se tiene 7 variables y 4 449 559 observaciones
dim(data)

#Preparación de los datos para el analisis descriptivo

#Tratamiento para la variable fecha 
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("doBy")

library("lubridate")
library("ggplot2")
library("dplyr")
library("doBy")

#Con ayuda de la libreria lubridate formateamos la variable Fecha, para convertir el año, mes, dia, hora, minuto en una variable cada una de ellas.
#Formato fecha
data$Fecha <- ymd_hm(data$Fecha)
class(data)

#Variable año
data$Año <- format(data$Fecha, "%Y")

#variable mes
data$Mes<- format(data$Fecha, "%m")

#Variable día 
data$Dia <- format(data$Fecha, "%d")

#Variable hora
data$Hora <- format(data$Fecha, "%H")

#Variable minuto
data$Minuto <- format(data$Fecha, "%M")
head(data)

year(data$Fecha)
day(data$Fecha)
month(data$Fecha)

#Recodificando los dias 
data$Dias <- wday(data$Fecha,label = TRUE)
levels(data$Dias)<-c("Domingo", "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado")

#Recodificando los meses
data$Mes_Nom <- month(data$Fecha,label = TRUE, abbr=TRUE)

#Tratamiento para los NA encontrados en la BD 
#NA en la DB #46080
sum(is.na(data))

#Na en la variable cuantitativa Energia Activa #0
sum(is.na(data$Ener_Act))

#Na en la variable cuantitativa Energia Reactiva total=46080
sum(is.na(data$Ener_Reac))

#Encontramos que los NA representa el 1% del total de las observaciones, viendo que no afectará nuestro analisis procedemos a eliminar los NA encontrados en la BD 
#Habiendo realizado la omision de NA ahora tenemos un total de 4 452579 registros.
#Eliminando los NA
data<-na.omit(data)
dim(data)

#Asumiendo que el consumo de energia activa y reactiva son positivos y los consumos con el valor de 0 no nos aporta información procedemos a eliminarlos. 
#Habiendo eliminado los negativos y los ceros ahora se tiene un total de 3 942 172 observaciones. 
#Eliminar negativos, dado que el consumo es positivo
data1<-subset(x=data, subset = Ener_Act>0 & Ener_Reac>0)
dim(data1)

#El analisis que realizaremos es del perido del mes de abril 2018, por lo que procedemos a eliminar los meses que no correspondan a este periodo
data1<-subset(x=data1, subset = Mes=="04")
dim(data1)
#Ahora tenemos un total de 3 940 992 observaciones

#Total de observaciones a analizar es 3 940 992

### Analisis ###

#Analisis descriptivo de los datos
resumen <- summaryBy(Ener_Act ~ Cod_Empresa , data = data1,FUN = fun1 <- function(x)
{c(N=length(x),Total=sum(x), minimo=min(x), maximo=max(x), Desv=sd(x))},order=TRUE)

#Tabla de estadisticos descriptivos por empresa ordenados de mayor a menor por el total de consumo
# y calculamos el consumo acumulado para ver que empresas tienen en conjunto el mayor porcentaje de consumo
resumen <- arrange(resumen, -Ener_Act.Total)

#Se agrega la variable Propocion de consumo a la tabla resumen 
resumen$PorcCOnsumo<-resumen$Ener_Act.Total/sum(resumen$Ener_Act.Total)*100

#Se agrega la variable de proporcion de consumo acumulado
resumen$PorcCOnsumoAcum<-round(cumsum(prop.table(resumen$PorcCOnsumo)*100),2)
#Comentarios: 
#En la siguiente data frame RESUMEN encontramos los principales estadisticos descriptivos por codigo de empresa, ordenado de mayor a menor,
#donde encontramos el promedio del consumo, la mediana, la desviasión estandar, el máximo y mínimo consumo de energia activa, el porcentage que representa cada empresa
#y el procentaje acumulado que nos ayudará a encontrar el top 5 de empresas que consumen más energia activa. 
#Esta tabla nos permitirá tomar decisiones frente a cada empresa según su consumo de energia activa registrado en el mes de abril 2018. 

#Grafico de barras de las empresas segun consmumo de energia activa
win.graph()
ggplot(data = data1, aes(x = Cod_Empresa,y = Ener_Act),fill=Cod_Empresa ) +
  geom_bar(stat = "sum", position = "dodge")+
  coord_flip()
#Comentarios: 
#El siguiente grafico de barras nos muestra la frecuencia de consumo de energia activa por empresa, encontrando que el mayor consumo es por la empresa
#ENGI, seguido por ELP, EGPE, KLPA y EDPE, estas empresan representan el 75% del consumo total de energia activa en el periodo de abril 2018.

#Grafico de barras de las empresas segun consumo de energia activa ordenado de mayor a menor
ggplot(data = resumen, aes(x = reorder(Cod_Empresa,Ener_Act.Total), y=Ener_Act.Total)) +
  geom_bar(stat = "sum", position = "dodge")+
  scale_x_discrete("Empresas")+
  scale_y_continuous("Consumo Total")+
  coord_flip()
#Comentarios: 
#En este grafico de barras se aprecia mejor el orden de las empresas y la ubicación que ocupada cada una de ella segun el consumo de energia activa registrado en el mes de abril 2018

#Histograma de las empresas segun consumo de energia activa
p<-ggplot(resumen, aes(x=Ener_Act.Total))+
  geom_histogram(color="blue", fill="blue")
p

#Comentarios: 
#El siguiente histograma muestra el comportamiento de la variable
#consumo de energia activa en el mes de abril 2018, con 3 940 993 registros, se observa que el consumo de energia activa tiene una asimetria 
#hacia la derecha, un sesgo positivo, el promedio 
#del consumo de energia activa es 512 kwh, con baja densidad por la derecha, la mediana de 83 khw, el consumo máximo fue de 69937.6 kwh y el minimo de 0 kwh

################## ANALISIS INDIVIDUAL POR LAS EMPRESAS QUE ACUMULAN EL 75% DE CONSUMO DE ENERGIA ACTIVA############
#Luego de haber realizado el analisis estadístico por empresa encontramo el top 5 de consumo de energia activa
#siendo estas empresas: ENGI, ELP, EGPE, KLPA, EDPE. Por lo que ahora realizaremos un analisis para cada una de ellas
#por haber sido las 5 empresas que consumieron más energia activa en el mes de abril 2018. 

#Analisis de la empresa ENGI
empresa_ENGI<-subset(x=data1$Ener_Act, subset = data1$Cod_Empresa=="ENGI")
hist(empresa_ENGI, ylab= "Consumo Energia Activa", xlab= "ENGI", main="Histograma del consumo de energia activa de la empresa ENGI")
boxplot(empresa_ENGI, xlab="ENGI", ylab="Consumo de energia activa", main= "Grafico de cajas del consumo de energia activa en la empresa ENGI")
summary(empresa_ENGI)
#Comentarios
#Analizando graficamente el comportamiento de la empresa ENGI, encontramos que 
# el promedio de consumo de energia activa fue de 1700 whh, la 50% del consumo esta al rededor de 222.03 kwh, el consumo máximo fue de 44312 kwh y el minimo cercano a 0 kwh.
#registrado en el mes de abril 2018.

#Analisis de la empresa ELP
empresa_ELP<-subset(x=data1$Ener_Act, subset = data1$Cod_Empresa=="ELP")
hist(empresa_ELP, ylab= "Consumo Energia Activa", xlab= "ELP", main="Histograma del consumo de energia activa de la empresa ELP")
boxplot(empresa_ELP, xlab="EDPE", ylab="Consumo de energia activa", main= "Grafico de cajas del consumo de energia activa en la empresa ELP")
summary(empresa_ELP)
#Comentarios
#Las siguientes graficas nos muestran el comportamiento de la empresa ELP, encontramos que 
# el promedio de consumo de energia activa fue de 12042 whh, la 50% del consumo esta al rededor de 4944.76 kwh, el consumo máximo fue de 69937 kwh y el minimo de 0.31 kwh.
#registrado en el mes de abril 2018.

#Analisis de la empresa EGPE
empresa_EGPE<-subset(x=data1$Ener_Act, subset = data1$Cod_Empresa=="EGPE")
hist(empresa_EGPE, ylab= "Consumo Energia Activa", xlab= "EGPE", main="Histograma del consumo de energia activa de la empresa EGPE")
boxplot(empresa_EGPE, xlab="EGPE", ylab="Consumo de energia activa", main= "Grafico de cajas del consumo de energia activa en la empresa EGPE")
summary(empresa_EGPE)
#Comentarios
#Se aprecia en las siguientes graficas el comportamiento de la empresa EGPE, encontramos que 
# el promedio de consumo de energia activa fue de 451.184 whh, la 50% del consumo esta al rededor de 78.500 kwh, el consumo máximo fue de 29137 kwh y el minimo de 0.068 kwh.
#registrado en el mes de abril 2018.

#Analisis de la empresa KLPA
empresa_KLPA<-subset(x=data1$Ener_Act, subset = data1$Cod_Empresa=="KLPA")
hist(empresa_KLPA, ylab= "Consumo Energia Activa", xlab= "KLPA", main="Histograma del consumo de energia activa de la empresa KLPA")
boxplot(empresa_KLPA, xlab="KLPA", ylab="Consumo de energia activa", main= "Grafico de cajas del consumo de energia activa en la empresa KLPA")
summary(empresa_KLPA)
#Comentarios
#Las siguientes graficas el comportamiento de la empresa KLPA, encontramos que 
# el promedio de consumo de energia activa fue de 864.24 whh, la 50% del consumo esta al rededor de 67.71 kwh, el consumo máximo fue de 29493.87 kwh y el minimo de 0.10 kwh.
#registrado en el mes de abril 2018.

#Analisis de la empresa EDPE
empresa_EDPE<-subset(x=data1$Ener_Act, subset = data1$Cod_Empresa=="EDPE")
hist(empresa_EDPE, ylab= "Consumo Energia Activa", xlab= "EDPE", main="Histograma del consumo de energia activa de la empresa EDPE")
boxplot(empresa_EDPE, xlab="EDPE", ylab="Consumo de energia activa", main= "Grafico de cajas del consumo de energia activa en la empresa EDPE")
summary(empresa_EDPE)
#Comentarios: 
#Las siguientes graficas el comportamiento de la empresa KLPA, encontramos que 
# el promedio de consumo de energia activa fue de 156.6 whh, la 50% del consumo esta al rededor de 72.2 kwh, el consumo máximo fue de 1836.6 kwh y el minimo de 0.1 kwh.
#registrado en el mes de abril 2018.
